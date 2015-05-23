{
 Unit zum Zugriff auf die Büchereiseiten
 Diese Unit enthält die einzigsten beide Funktionen (TUpdateLibThread.execute und extendAccountBookData),
 die die Daten der Konten verändert und speichert
}

unit libraryAccess;

{$mode objfpc}{$H+}


interface
uses
  Classes, SysUtils,libraryParser,booklistreader, LCLType, strutils, bbutils;

type TBookListOperation = procedure (list: TBookList) of object;

//--Aktualisierungen--
type
  TThreadConfig=record
    //oneThreadSuccessful: boolean;//write only true
    libraryAccessSection: TRTLCriticalSection;
    threadManagementSection: TRTLCriticalSection;
    updateThreadsRunning:integer; //all threads
    listUpdateThreadsRunning: integer; //count of threads which are updating the list of books (and have not started updating singely)
    successfulListUpdateDate: longint;
    OnPartialUpdated: TNotifyEvent;
  end;
var updateThreadConfig: TThreadConfig;
procedure ThreadDone(pself: TObject; sender:TObject);

//Aktualisiert eine (oder bei lib=nil alle) Konten in einem extra-thread
function updateAccountBookData(account: TCustomAccountAccess;ignoreConnErrors, checkDate,extendAlways: boolean): boolean;
procedure defaultAccountsRefresh;

//--Verlängerungen--
//Verlängert die Bücher des Accounts indem extendAccountBookData aufgerufen wird
//procedure extendAccountBookData(account: TCustomAccountAccess;books: TBookArray);
procedure extendBooks(books: TBookList);
procedure extendBooks(lastLimit:longint; account: TCustomAccountAccess=nil);

//procedure orderBooks(books: TBookList);
procedure cancelBooks(books: TBookList);

//--Userkommunikation--

procedure findBooksThatMustBeReturned(out booksOverdue, booksSoonNotExtendable, booksSoon: TList; out minDateOverdue, minDateSoonNotExtendable, minDateSoon: integer);
//benachrichtigt den Benutzer über fällige Medien und fragt nach dem Öffnen des
//Hauptformulars deswegen (->true wenn es geöffnet werden soll)
function alertAboutBooksThatMustBeReturned:boolean;


type PThreadConfig=^TThreadConfig;
//procedure updateBooksDirectBlocking(const lib: TCustomAccountAccess; const pconfig: PThreadConfig; const ignoreConnectionErrors, checkDate, forceExtend: boolean);

implementation
uses applicationconfig,internetaccess,bookwatchmain,bbdebugtools,androidutils,{$ifdef android}bbjniutils,{$endif}
     forms; //for messages
const TRY_BOOK_UPDATE='Versuche Mediendaten zu aktualisieren...';
{$IFDEF WIN32}
const MB_SYSTEMMODAL = $1000;
{$ELSE}
const MB_SYSTEMMODAL=0;
{$ENDIF}

type TBookProcessingRequest = class
  ignoreConnectionErrors, checkDate, useExtendOverride: boolean;
  ExtendOverride: TExtendType;
  partialList: TBookList;
  partialListOperation: TBookListOperation
end;



//==============================================================================
//============================Aktualisierungs Thread============================
//==============================================================================
type

  { TUpdateLibThread }

  TUpdateLibThread = class(TThread)
  protected
    pconfig: PThreadConfig;
    lib: TCustomAccountAccess;
    errorstr,errordetails:string;
    listUpdateComplete: boolean;
    messageShown: boolean;

    requests: TFPList;

    procedure processRequest(request: TBookProcessingRequest);
    procedure execute;override;
    //procedure exceptionRaised(raisedException:Exception);
    //procedure showError;
  public
    constructor Create(alib: TCustomAccountAccess;var config:TThreadConfig; someRequests: TFPList);
    constructor Create(alib: TCustomAccountAccess;var config:TThreadConfig; request: TBookProcessingRequest);
  end;

procedure TUpdateLibThread.processRequest(request: TBookProcessingRequest);
var booksToExtendCount,booksExtendableCount: longint;
    realBooksToExtend: TBookList;
    i:longint;
    wasConnected: Boolean;
    repBook: TBook;
    newPartialBookList: TBookList;
begin
  if logging then begin
    log('TUpdateLibThread.processRequest(@lib='+inttostr(IntPtr(Pointer(lib)))+') started');
    log('Library is: '+lib.prettyName);
  end;

  try
  try
    if lib=nil then
      raise ELibraryException.create('Interner Fehler'#13#10'Aufruf von TUpdateLibThread.execute für einen nicht existierenden Account');
    with request do begin
      if checkDate then
        if (lib.lastCheckDate>currentDate-refreshInterval) and (not lib.existsCertainBookToExtend) then begin
          if logging then log('TUpdateLibThread.processRequest() ended without checking');
          exit;
        end;

      if logging then log('TUpdateLibThread.execute ended marker 1');
      wasConnected := lib.connected;
      if not wasConnected then lib.connect(defaultInternetAccessClass.create);
      if logging then log('TUpdateLibThread.execute ended marker 2');
      if (not wasConnected) or (not lib.updated) or (partialListOperation = nil) then begin //default behaviour is "update", disabled when a special partial list operation is given, but if we were not connected, it still needs to update
        lib.updateAll();
        if logging then log('TUpdateLibThread.execute ended marker 3');
        if lib.needSingleBookCheck() then begin
          if logging then log('TUpdateLibThread.execute marker 3.1');
          EnterCriticalSection(pconfig^.libraryAccessSection);
          lib.books.merge(false);
          LeaveCriticalSection(pconfig^.libraryAccessSection);
          if logging then log('TUpdateLibThread.execute marker 3.4');


          if logging then log('TUpdateLibThread.execute marker 3.5');
          EnterCriticalSection(pconfig^.threadManagementSection);
          pconfig^.listUpdateThreadsRunning-=1;
          pconfig^.successfulListUpdateDate:=currentDate;
          LeaveCriticalSection(pconfig^.threadManagementSection);
          if logging then log('TUpdateLibThread.execute marker 3.6');
          if assigned(pconfig^.OnPartialUpdated) then pconfig^.OnPartialUpdated(lib);

          listUpdateComplete:=true;
          lib.updateAllSingly();
        end;
      end;
      if logging then log('TUpdateLibThread.execute marker 4');

      if not useExtendOverride then ExtendOverride := lib.extendType;
      //Automatisches Verlängern
      if ExtendOverride <> etNever then begin
        booksExtendableCount:=0;
        for i:=0 to lib.books.currentUpdate.count-1 do
          if lib.books.currentUpdate[i].status in BOOK_EXTENDABLE then
            booksExtendableCount+=1;

        case ExtendOverride of
          etNever: booksToExtendCount:=0;
          etAlways: //renew always (when there are books which can be extended)
            booksToExtendCount:=booksExtendableCount;
          etAllDepends,etSingleDepends: begin
            booksToExtendCount:=0;
            for i:=0 to lib.books.currentUpdate.Count-1 do //check for books to renew
              if lib.shouldExtendBook(lib.books.currentUpdate[i]) then
                booksToExtendCount+=1;
            if (lib.extendType=etAllDepends) and (booksToExtendCount>0) then
              booksToExtendCount:=booksExtendableCount;
          end;
          else raise Exception.Create('Internal error: unknown lib.extendType');
        end;


        if booksToExtendCount>0 then
          if booksToExtendCount=booksExtendableCount then
            lib.extendAll
           else begin
            realBooksToExtend:=TBookList.Create;
            for i:=0 to lib.books.currentUpdate.Count-1 do
              if lib.shouldExtendBook(lib.books.currentUpdate[i]) then
                realBooksToExtend.add(lib.books.currentUpdate[i]);
            lib.extendList(realBooksToExtend);
            realBooksToExtend.free
           end;
      end;

      //other, special operation
      if (partialList <> nil) and (Assigned(partialListOperation)) then begin
        newPartialBookList := TBookList.create();
        for i := 0 to partialList.Count - 1 do begin
          repBook := lib.books.currentUpdate.findBook(partialList[i]);
          if not (repBook.status in BOOK_NOT_LEND) then
            if repBook.dueDate > partialList[i].dueDate then continue; //was already renewed
          //if repBook = nil then repBook := lib.books.current.findBook(partialList[i]); ??
          newPartialBookList.add(repBook);
        end;

        partialList.free;
        partialList := newPartialBookList;
        partialListOperation(partialList);
      end;


      if logging then log('TUpdateLibThread.execute ended marker 5');
      EnterCriticalSection(pconfig^.libraryAccessSection);
      lib.books.merge(true);
      LeaveCriticalSection(pconfig^.libraryAccessSection);
      if logging then log('TUpdateLibThread.execute ended marker 8');

      lib.save();

      if logging then log('TUpdateLibThread.execute ended marker 9');
      pconfig^.successfulListUpdateDate:=currentDate;
    end;
  except
    on e: EInternetException do {$ifndef activeDebug}if not request.ignoreConnectionErrors then{$endif}
      storeException(e,lib,lib.getLibrary().id,'');
    on e: exception do begin
      lib.broken:=currentDate;
      storeException(e,lib,lib.getLibrary().id,'');
    end
    else if logging then log('Unverständliche Fehlermeldung');
  end;
  finally
    if assigned(request.partialList) then request.partialList.Free;
    request.Free;
  end;
  if logging then log('TUpdateLibThread.processRequest ended');
end;

procedure TUpdateLibThread.execute;
var
  request: TBookProcessingRequest;
begin
  if logging then log('TUpdateLibThread.execute started');
  while true do begin
    EnterCriticalsection(pconfig^.threadManagementSection);
    if requests.Count > 0 then begin
      request := TObject(requests.First) as TBookProcessingRequest;
      requests.Delete(0);
    end else begin
      request := nil;
      FreeAndNil(requests);
      break;
    end;
    LeaveCriticalsection(pconfig^.threadManagementSection);

    processRequest(request);
  end;

  if not listUpdateComplete then
    pconfig^.listUpdateThreadsRunning-=1;
  pconfig^.updateThreadsRunning-=1;
  lib.thread:=Nil;
  LeaveCriticalSection(pconfig^.threadManagementSection);


  {$ifdef android}
  if Assigned(OnTerminate) then begin //normal onterminate does not work without event loop as it is called by synchronize
   OnTerminate(self);
   OnTerminate:=nil;
  end;
  {$endif}
  if logging then log('TUpdateLibThread.execute ended');
end;

                                           {
procedure TUpdateLibThread.showError;
//var i:integer;
begin
  if messageShown then exit;
  messageShown:=true;
  addErrorMessage(errorStr,errordetails,lib);
  messageShown:=false;
end;
procedure TUpdateLibThread.exceptionRaised(raisedException:Exception);
begin
  if raisedException=nil then exit;
  createErrorMessageStr(raisedException,errorstr,errordetails,lib);
 // end;
  messageShown:=false;
  Synchronize(@showError);
end;                                      }

constructor TUpdateLibThread.Create(alib: TCustomAccountAccess; var config: TThreadConfig; someRequests: TFPList);
begin
  lib:=alib;
  pconfig:=@config;
  requests := someRequests;

  OnTerminate:=TNotifyEvent(procedureToMethod(TProcedure(@ThreadDone)));
  FreeOnTerminate:=true;

  if logging then log('TUpdateLibThread.Create');
  inherited create(false);
end;

constructor TUpdateLibThread.Create(alib: TCustomAccountAccess; var config: TThreadConfig; request: TBookProcessingRequest);
var
  somerequests: TFPList;
begin
  somerequests := TFPList.Create;
  somerequests.Add(request);
  Create(alib, config, somerequests);
end;

procedure performAccountAction(account: TCustomAccountAccess;var config:TThreadConfig;
                            aIgnoreConnectionErrors, ACheckDate: boolean;
                            AUseExtendOverride: Boolean; AExtendOverride: TExtendType;
                            apartialList: TBookList; apartialListOperation: TBookListOperation);
var
  request: TBookProcessingRequest;
  oldThread: TUpdateLibThread;
  useExisting: Boolean;
begin
  request := TBookProcessingRequest.Create;
  with request do begin
    ignoreConnectionErrors:=aIgnoreConnectionErrors;
    checkDate := acheckDate;
    useExtendOverride:=AUseExtendOverride;
    ExtendOverride := aExtendOverride;
    partialList := apartialList;
    partialListOperation := apartialListOperation;
  end;

  EnterCriticalSection(config.threadManagementSection);
  useExisting := false;
  if (account.thread <> nil) and  ((account.thread as TUpdateLibThread).requests <> nil) then
    (account.thread as TUpdateLibThread).requests.Add(request)
  else begin
    config.updateThreadsRunning+=1;
    config.listUpdateThreadsRunning+=1;
    account.thread := TUpdateLibThread.Create(account,config,request);
  end;
  LeaveCriticalSection(config.threadManagementSection);
end;

procedure ThreadDone(pself: TObject; sender:TObject);
//called in the main thread on normal OS
//called in update thread on Android!
begin
  if logging then log('ThreadDone started'#13#10'Without this one, '+IntToStr(updateThreadConfig.updateThreadsRunning)+' threads are currently running');
//  Assert(mainForm<>nil);
//log(booltostr(sender is TUpdateLibThread) );
  if not (sender is TUpdateLibThread) then
    raise exception.Create('Interner Fehler:'#13#10'Die Funktion, die für gerade beendete Aktualisierungthread zuständig ist, wurde auf einen anderen Thread angewendet'#13#10'(kann eigentlich nicht auftreten)');

  {$ifdef android}EnterCriticalSection(updateThreadConfig.threadManagementSection);{$endif}

  if (updateThreadConfig.updateThreadsRunning<=0) then begin
    updateGlobalAccountDates();
    accountsRefreshedDate := updateThreadConfig.successfulListUpdateDate;
    if accountsRefreshedDate = currentDate then begin
      if (mainForm <> nil) and (mainform.visible) then
         mainform.RefreshListView;
      applicationUpdate(true);
      sendMailReports();
    end;
    if mainForm <> nil then mainform.delayedCall.Enabled:=true; //show error messages
    {$ifdef android}androidAllThreadsDone();{$endif}
  end;

  {$ifdef android}
  LeaveCriticalsection(updateThreadConfig.threadManagementSection);
  jvmref^^.DetachCurrentThread(jvmref);
  {$endif}

  if logging then log('ThreadDone ended');
end;

//Aufruf des Aktualisierungsthread
function updateAccountBookData(account: TCustomAccountAccess;ignoreConnErrors,checkDate,extendAlways: boolean): boolean;
var i: longint;
  threadsToStart: Integer;
  request: TBookProcessingRequest;
begin
  if logging then log('updateAccountBookData started');

  result := false;

  if (account=nil) and (updateThreadConfig.updateThreadsRunning>0) then exit;
  if (account<>nil) and (account.thread <> nil) then exit;

  if (account=nil)and(accounts.count=1) then
    account:=(accounts[0]);
    
  if refreshAllAndIgnoreDate then begin
    ignoreConnErrors:=false;
    checkDate:=false;
  end;

//  threadConfig.baseWindow:=handle;
  if account<>nil then begin
    if ignoreConnErrors and (account.broken = currentDate) then
      exit;
    if (mainform<>nil) and (mainForm.Visible) then
      mainform.StatusBar1.Panels[0].text:=TRY_BOOK_UPDATE;
    performAccountAction(account,updateThreadConfig,ignoreConnErrors,checkDate,extendAlways,etAlways,nil,nil);
  end else begin
    //(synchronized) set count of threads
    threadsToStart := accounts.count;
    for i:=0 to accounts.count-1 do begin
      account := (accounts[i]);
      if (not account.enabled) or (ignoreConnErrors and (account.broken = currentDate)) then
        threadsToStart-=1;
    end;

    if threadsToStart = 0 then exit;

    EnterCriticalSection(updateThreadConfig.threadManagementSection);
    updateThreadConfig.updateThreadsRunning:=threadsToStart;
    updateThreadConfig.listUpdateThreadsRunning:=threadstostart;
    LeaveCriticalSection(updateThreadConfig.threadManagementSection);

    updateThreadConfig.successfulListUpdateDate:=0; //we know that no threads are running atm
    if (mainform<>nil) and (mainForm.Visible) then
      mainform.StatusBar1.Panels[0].text:=TRY_BOOK_UPDATE;

    //actually start threads
    for i:=0 to accounts.count-1 do begin
      account := (accounts[i]);
      if (not account.enabled) or (ignoreConnErrors and (account.broken = currentDate)) then
        continue;
      request := TBookProcessingRequest.Create;
      request.ignoreConnectionErrors := ignoreConnErrors;
      request.checkDate := checkDate;
      request.UseExtendOverride := extendAlways;
      request.ExtendOverride:= etAlways;
      account.thread:=TUpdateLibThread.create(account,updateThreadConfig, request);
    end;
  end;
  result := true;
end;
//Bücher aktualisieren
procedure defaultAccountsRefresh;
begin
  if accountsRefreshedDate = currentDate then exit;
  if logging then log('defaultAccountsRefresh started');
  case userConfig.ReadInteger('base','startup-connection',1) of
    1: updateAccountBookData(nil,true,true,false);
    2: updateAccountBookData(nil,true,false,false);
  end;
  if logging then log('defaultAccountsRefresh ended');
end;


//type TProcessBooks procedure extendAccountBookData(account: TCustomAccountAccess;
//  books: TBookList);

                 (*
procedure extendAccountBookData(account: TCustomAccountAccess; books: TBookList);
var internet:TInternetAccess;
begin
  try
    if not assigned(account) then
      raise EXCEPTION.Create('Kein Konto zum Verlängern ausgewählt');
    if account.isThreadRunning then
      exit; //TODO!!!: mehrere Threads beim verlängern erlauben
    if GetCurrentThreadId <> MainThreadID then begin
      if logging then log('extendAccountBookData canceled due to thread mismatch: '+IntToStr(GetThreadID)+' '+IntToStr(MainThreadID));
      exit; //Nur vom Haupthread aus aufrufen
    end;

    if (account<>nil) and (books.Count>0) then begin
      if not account.connected then begin
        account.connect(defaultInternetAccessClass.create());
        account.updateAll();
        if account.needSingleBookCheck() then
          account.updateAllSingly;
      end;
      books.mergeMissingInformation(account.books.currentUpdate);
      books.mergeMissingInformation(account.books.current);

      account.extendList(books);
      account.books.merge(true);
      account.save();
    end;
  except
    on e: exception do
      createAndAddException(e,account);
  end;
end;
                        *)

procedure applyAccountMethodForBooks(books: TBookList; method: TBookListOperation);
var current:TBookList;
    i,j:integer;
begin
  //TODO: optimize
  //SetLength(accBoo,accounts.Count);
  for i:=0 to accounts.Count-1 do begin
    current:=nil;
    for j:=0 to books.count-1 do
      if books[j].owner=accounts.Objects[i] then begin
        if current = nil then current := TBookList.Create;
        current.add(books[j]);
      end;
    if current <> nil then begin
      TMethod(method).Data:=accounts.Objects[i];
      performAccountAction( accounts[i], updateThreadConfig, false, false, true, etNever, current, method);
    end;
  end;
  {showErrorMessages();
  if (mainform<>nil) and (mainform.visible) then
    mainform.RefreshListView;     }
end;

procedure extendBooks(lastLimit: longint; account: TCustomAccountAccess);
var i:integer;
    books: TBookList;
begin
  if account=nil then begin
     for i:=0 to accounts.count-1 do
       extendBooks(lastLimit,(accounts[i]));
     exit;
  end;
  books:=TBookList.Create;
  //TODO: Optimize
  for i:=0 to account.books.current.Count-1 do
    if (account.books.current[i].dueDate<=lastLimit) and
       (account.books.current[i].status in BOOK_EXTENDABLE) then
       books.add(account.books.current[i]);
  extendBooks(books);

  showErrorMessages();
  if (mainform<>nil) and (mainform.visible) then
    mainform.RefreshListView;
end;

procedure extendBooks(books: TBookList);
begin
  applyAccountMethodForBooks(books, TBookListOperation(procedureToMethod(TProcedure(@TTemplateAccountAccess.extendList))));
end;

{procedure orderBooks(books: TBookList);
begin
  applyAccountMethodForBooks(books, TBookListOperation(procedureToMethod(TProcedure(@TTemplateAccountAccess.orderList))));
end;
 }
procedure cancelBooks(books: TBookList);
begin
  applyAccountMethodForBooks(books, TBookListOperation(procedureToMethod(TProcedure(@TTemplateAccountAccess.cancelList))));
end;



procedure findBooksThatMustBeReturned(out booksOverdue, booksSoonNotExtendable, booksSoon: TList; out minDateOverdue, minDateSoonNotExtendable,
  minDateSoon: integer);
var
  i,j: Integer;
begin
  currentDate:=longint(trunc(now));

  booksOverdue:=TList.Create;
  booksSoonNotExtendable:=TList.Create;
  booksSoon:=TList.Create;

  minDateOverdue := currentDate+1000;
  minDateSoonNotExtendable := currentDate+1000;
  minDateSoon := currentDate+1000;

  //redTime := currentDate+14; //debug!

  for i:=0 to accounts.count-1 do
    with (accounts[i]) do
      for j:=0  to books.current.count-1 do begin
        if books.current[j].status in BOOK_NOT_LEND then continue;
        if books.current[j].dueDate<currentDate then begin
          booksOverdue.Add(books.current[j]);
          if books.current[j].dueDate < minDateOverdue then
            minDateOverdue:=books.current[j].dueDate;
        end else if (books.current[j].dueDate<=redTime) then begin
          if (books.current[j].status in BOOK_NOT_EXTENDABLE) then begin
            booksSoonNotExtendable.Add(books.current[j]);
            if books.current[j].dueDate < minDateSoonNotExtendable then
              minDateSoonNotExtendable:=books.current[j].dueDate;
           end else begin
            booksSoon.Add(books.current[j]);
            if books.current[j].dueDate < minDateSoon then
              minDateSoon:=books.current[j].dueDate;
           end;
        end
      end;
end;

function alertAboutBooksThatMustBeReturned: boolean;
  function strJoin(l: tlist): string;
  var sl: TStringList;
    i: Integer;
  begin
    sl := TStringList.Create;
    for i := 0 to l.Count - 1 do sl.Add(tbook(l[i]).toSimpleString());
    result := #9 + bbutils.strJoin(sl, LineEnding + #9, -10)+LineEnding;
    sl.free;
  end;
var alert:string;
    booksOverdue, booksSoonNotExtendable, booksSoon: TList;
    minDateSoon: Integer;
    minDateSoonNotExtendable: Integer;
    minDateOverdue: Integer;
begin
  if logging then log('alertAboutBooksThatMustBeReturned started');
  result:=false;

  findBooksThatMustBeReturned(booksOverdue, booksSoonNotExtendable, booksSoon,
                              minDateOverdue, minDateSoonNotExtendable, minDateSoon);

  if ((lastWarnDate + WarnInterval <= currentDate) and (booksOverdue.Count + booksSoonNotExtendable.Count + booksSoon.Count > 0))
     or (booksOverdue.Count > 0) then begin
    alert:='';
    if booksOverdue.Count > 0 then begin
      alert+=Format('Die folgenden Medien (%d) sind überfällig und sollten schon bis %s abgegeben worden sein:'#13,
                    [booksOverdue.Count, DateToPrettyGrammarStr('zum ','',minDateOverdue)]);
      alert+=strJoin(booksOverdue) + LineEnding;
    end;
    if booksSoonNotExtendable.Count > 0 then begin
      alert+=Format('Die folgenden Medien (%d) sind nicht verlängerbar und sollten bis %s abgegeben werden:'#13,
                   [booksSoonNotExtendable.Count, DateToPrettyGrammarStr('zum ','',minDateSoonNotExtendable)]);
      alert+=strJoin(booksSoonNotExtendable) + LineEnding;
    end;
    if booksSoon.Count > 0 then begin
      alert+=Format('Die folgenden Medien (%d) sind bald bis %s fällig:'#13, [booksSoon.Count, DateToPrettyGrammarStr('zum ','',minDateSoon)]);
      alert+=strJoin(booksSoon) + LineEnding;
      alert+='VideLibri wird versuchen, diese Bücher automatisch zu verlängern'#13#13;
    end;
    alert+='Soll die Gesamt-Übersicht geöffnet werden?';
    if Application.MessageBox(pchar(alert),'Videlibri',MB_YESNO or MB_ICONWARNING or MB_SYSTEMMODAL)=IDYES then
      result:=true;
  end;

  booksSoon.Free;
  booksSoonNotExtendable.free;
  booksOverdue.Free;

  lastWarnDate:=currentDate;
  userConfig.WriteInteger('base','last-warn-date',currentDate);

  if logging then log('alertAboutBooksThatMustBeReturned ended');
end;




end.

