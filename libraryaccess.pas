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

//--Aktualisierungen--
type
  TThreadConfig=record
    //oneThreadSuccessful: boolean;//write only true
    libraryAccessSection: TRTLCriticalSection;
    threadManagementSection: TRTLCriticalSection;
    updateThreadsRunning:integer; //all threads
    listUpdateThreadsRunning: integer; //count of threads which are updating the list of books (and have not started updating singely)
    atLeastOneListUpdateSuccessful: boolean;
    OnPartialUpdated: TNotifyEvent;
  end;
var updateThreadConfig: TThreadConfig;
procedure ThreadDone(sender:TObject);

//Aktualisiert eine (oder bei lib=nil alle) Konten in einem extra-thread
procedure updateAccountBookData(account: TCustomAccountAccess;ignoreConnErrors, checkDate,extendAlways: boolean);
procedure defaultAccountsRefresh;

//--Verlängerungen--
//Verlängert die Bücher des Accounts indem extendAccountBookData aufgerufen wird
//procedure extendAccountBookData(account: TCustomAccountAccess;books: TBookArray);
procedure extendBooks(books: TBookList);
procedure extendBooks(lastLimit:longint; account: TCustomAccountAccess=nil);


//--Userkommunikation--
//benachrichtigt den Benutzer über fällige Medien und fragt nach dem Öffnen des
//Hauptformulars deswegen (->true wenn es geöffnet werden soll)
function alertAboutBooksThatMustBeReturned:boolean;


type PThreadConfig=^TThreadConfig;
procedure updateBooksDirectBlocking(const lib: TCustomAccountAccess; const pconfig: PThreadConfig; const ignoreConnectionErrors, checkDate, extend: boolean);

implementation
uses applicationconfig,internetaccess,bookwatchmain,bbdebugtools,
     forms; //for messages
const TRY_BOOK_UPDATE='Versuche Mediendaten zu aktualisieren...';
{$IFDEF WIN32}
const MB_SYSTEMMODAL = $1000;
{$ELSE}
const MB_SYSTEMMODAL=0;
{$ENDIF}

procedure updateBooksDirectBlocking(const lib: TCustomAccountAccess; const pconfig: PThreadConfig; const ignoreConnectionErrors,
  checkDate, extend: boolean);
var internet: TInternetAccess;
    listUpdateComplete: boolean;

    booksToExtendCount,booksExtendableCount: longint;
    realBooksToExtend: TBookList;
    i:longint;
begin
//Die Funktionsweise ist folgende:
{
--AKTUALISIEREN und langsames verlängern--
#connect
#update-all
(merge and display)
if exists #update-single then for all: #update-single
if exists books-to-renew then begin
  if exists #renew-list then renew-list
  else if exists #renew-single then for all: #renew-single
  else renew-all
end
(disconnect)
save
}
  if logging then begin
    log('TUpdateLibThread.execute(@lib='+inttostr(int64(Pointer(lib)))+') started');
    log('Library is: '+lib.prettyName);
  end;

  try
    listUpdateComplete:=false;
    if lib=nil then
      raise ELibraryException.create('Interner Fehler'#13#10'Aufruf von TUpdateLibThread.execute für einen nicht existierenden Account');
    if checkDate then
      if (lib.lastCheckDate>currentDate-refreshInterval) and (not lib.existsCertainBookToExtend) then begin
        EnterCriticalSection(pconfig^.threadManagementSection);
        pconfig^.updateThreadsRunning-=1;
        pconfig^.listUpdateThreadsRunning-=1;
        LeaveCriticalSection(pconfig^.threadManagementSection);
        lib.isThreadRunning:=false;
        if logging then log('TUpdateLibThread.execute ended without checking');
        exit;
      end;

    if logging then log('TUpdateLibThread.execute ended marker 1');
    if not lib.connected then
      lib.connect(defaultInternetAccessClass.create);
    if logging then log('TUpdateLibThread.execute ended marker 2');
    lib.updateAll();
    if logging then log('TUpdateLibThread.execute ended marker 3');
    if lib.needSingleBookCheck() then begin
      //InterlockedIncrement(@pconfig^.totalBookThreadDone);
      //Synchronize(@mainForm.RefreshListView);
      if logging then log('TUpdateLibThread.execute marker 3.1');
      EnterCriticalSection(pconfig^.libraryAccessSection);
      lib.books.merge(false);
      LeaveCriticalSection(pconfig^.libraryAccessSection);
      if logging then log('TUpdateLibThread.execute marker 3.4');


      if logging then log('TUpdateLibThread.execute marker 3.5');
      EnterCriticalSection(pconfig^.threadManagementSection);
      pconfig^.listUpdateThreadsRunning-=1;
      pconfig^.atLeastOneListUpdateSuccessful:=true;
      LeaveCriticalSection(pconfig^.threadManagementSection);
      if logging then log('TUpdateLibThread.execute marker 3.6');
      if assigned(pconfig^.OnPartialUpdated) then pconfig^.OnPartialUpdated(lib);

      listUpdateComplete:=true;
      lib.updateAllSingly();
    end;
    if logging then log('TUpdateLibThread.execute marker 4');

    //Automatisches Verlängern
    booksExtendableCount:=0;
    for i:=0 to lib.books.currentUpdate.count-1 do
      if lib.books.currentUpdate[i].status in BOOK_EXTENDABLE then
        booksExtendableCount+=1;

    case lib.extendType of
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




    if logging then log('TUpdateLibThread.execute ended marker 5');
    EnterCriticalSection(pconfig^.libraryAccessSection);
    lib.books.merge(true);
    LeaveCriticalSection(pconfig^.libraryAccessSection);
    if logging then log('TUpdateLibThread.execute ended marker 8');

    lib.save();

    if logging then log('TUpdateLibThread.execute ended marker 9');
    pconfig^.atLeastOneListUpdateSuccessful:=true;
  except
    on e: EInternetException do {$ifndef activeDebug}if not ignoreConnectionErrors then{$endif}
      storeException(e,lib);
    on e: exception do begin
      lib.broken:=currentDate;
      storeException(e,lib);
    end
    else if logging then log('Unverständliche Fehlermeldung');
  end;
  EnterCriticalSection(pconfig^.threadManagementSection);
  if not listUpdateComplete then
    pconfig^.listUpdateThreadsRunning-=1;
  pconfig^.updateThreadsRunning-=1;
  LeaveCriticalSection(pconfig^.threadManagementSection);
  lib.isThreadRunning:=false;
  if logging then log('TUpdateLibThread.execute ended');

end;


//==============================================================================
//============================Aktualisierungs Thread============================
//==============================================================================
type

  TUpdateLibThread = class(TThread)
  protected
    pconfig: PThreadConfig;
    lib: TCustomAccountAccess;
    errorstr,errordetails:string;
    messageShown: boolean;
    ignoreConnectionErrors: boolean;   //read only
    checkDate: boolean;                //read only

    extend: boolean;
    procedure execute;override;
    //procedure exceptionRaised(raisedException:Exception);
    //procedure showError;
  public
    constructor Create(alib: TCustomAccountAccess;var config:TThreadConfig;
                       aIgnoreConnectionErrors, ACheckDate,AExtend: boolean);
  end;

procedure TUpdateLibThread.execute;
begin
  updateBooksDirectBlocking(lib, pconfig, ignoreConnectionErrors, checkDate, extend);
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

constructor TUpdateLibThread.Create(alib: TCustomAccountAccess;var config:TThreadConfig;
                                    aIgnoreConnectionErrors, ACheckDate,AExtend: boolean);
begin
  lib:=alib;
  ignoreConnectionErrors:=aignoreConnectionErrors;
  checkDate:=ACheckDate;
  pconfig:=@config;
  extend:=AExtend;
  OnTerminate:=@mainForm.ThreadDone;
  FreeOnTerminate:=true;

  inherited create(false);
end;

procedure ThreadDone(sender:TObject);
//called in the main thread
begin
  if logging then log('ThreadDone started'#13#10'Without this one, '+IntToStr(updateThreadConfig.updateThreadsRunning)+' threads are currently running');
  Assert(mainForm<>nil);
  if not (sender is TUpdateLibThread) then
    raise exception.Create('Interner Fehler:'#13#10'Die Funktion, die für gerade beendete Aktualisierungthread zuständig ist, wurde auf einen anderen Thread angewendet'#13#10'(kann eigentlich nicht auftreten)');

  if (updateThreadConfig.updateThreadsRunning<=0) then
    updateGlobalAccountDates();
  if (updateThreadConfig.updateThreadsRunning<=0) or (updateThreadConfig.listUpdateThreadsRunning<=0) then begin
    if (updateThreadConfig.atLeastOneListUpdateSuccessful) then begin
      updateThreadConfig.atLeastOneListUpdateSuccessful:=updateThreadConfig.updateThreadsRunning>0;
      accountsRefreshedToday:=true;
      if (mainform.visible) then
        mainform.RefreshListView;
      applicationUpdate(true);
      sendMailReports();
    end;
    mainform.delayedCall.Enabled:=true //show error messages
  end;

  if logging then log('ThreadDone ended');
end;

//Aufruf des Aktualisierungsthread
procedure updateAccountBookData(account: TCustomAccountAccess;ignoreConnErrors,checkDate,extendAlways: boolean);
var i: longint;
  threadsToStart: Integer;
begin
  if (account=nil) and (updateThreadConfig.updateThreadsRunning>0) then exit;
  if (account<>nil) and (account.isThreadRunning) then exit;

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
    EnterCriticalSection(updateThreadConfig.threadManagementSection);
    updateThreadConfig.updateThreadsRunning+=1;
    updateThreadConfig.listUpdateThreadsRunning+=1;
    LeaveCriticalSection(updateThreadConfig.threadManagementSection);

    account.isThreadRunning:=true;
    TUpdateLibThread.Create(account,updateThreadConfig,ignoreConnErrors,checkDate,extendAlways);
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

    updateThreadConfig.atLeastOneListUpdateSuccessful:=false;
    if (mainform<>nil) and (mainForm.Visible) then
      mainform.StatusBar1.Panels[0].text:=TRY_BOOK_UPDATE;

    //actually start threads
    for i:=0 to accounts.count-1 do begin
      account := (accounts[i]);
      if (not account.enabled) or (ignoreConnErrors and (account.broken = currentDate)) then
        continue;
      account.isThreadRunning:=true;
      TUpdateLibThread.create(account,updateThreadConfig, ignoreConnErrors,checkDate,extendAlways);
    end;
  end;
end;
//Bücher aktualisieren
procedure defaultAccountsRefresh;
begin
  if accountsRefreshedToday then exit;
  if logging then log('defaultAccountsRefresh started');
  case userConfig.ReadInteger('base','startup-connection',1) of
    1: updateAccountBookData(nil,true,true,false);
    2: updateAccountBookData(nil,true,false,false);
  end;
  if logging then log('defaultAccountsRefresh ended');
end;


procedure extendAccountBookData(account: TCustomAccountAccess;
  books: TBookList);
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

procedure extendBooks(books: TBookList);
var current:TBookList;
    i,j:integer;
begin
  //TODO: optimize
  //SetLength(accBoo,accounts.Count);
  current:=TBookList.Create;
  for i:=0 to accounts.Count-1 do begin
    current.clear;
    for j:=0 to books.count-1 do
      if books[j].owner=accounts.Objects[i] then
        current.add(books[j]);
    if current.Count > 0 then
      extendAccountBookData((accounts[i]),current);
  end;
  current.free;
  showErrorMessages();
  if (mainform<>nil) and (mainform.visible) then
    mainform.RefreshListView;
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
  extendAccountBookData(account,books);
  books.free;

  showErrorMessages();
  if (mainform<>nil) and (mainform.visible) then
    mainform.RefreshListView;
end;



function alertAboutBooksThatMustBeReturned: boolean;
var alert:string;
    tempInternet: TInternetAccess;
    i,j,count:longint;
    booksOverdue, booksSoonNotExtendable, booksSoon: TStringList;
    minDateSoon: Integer;
    minDateSoonNotExtendable: Integer;
    minDateOverdue: Integer;
begin
  if logging then log('alertAboutBooksThatMustBeReturned started');
  result:=false;
  currentDate:=longint(trunc(now));
  count:=0;

  booksOverdue:=TStringList.Create;
  booksSoonNotExtendable:=TStringList.Create;
  booksSoon:=TStringList.Create;

  minDateOverdue := currentDate+1000;
  minDateSoonNotExtendable := currentDate+1000;
  minDateSoon := currentDate+1000;

  for i:=0 to accounts.count-1 do
    with (accounts[i]) do
      for j:=0  to books.current.count-1 do begin
        if books.current[j].dueDate<currentDate then begin
          booksOverdue.Add(books.current[j].toSimpleString());
          if books.current[j].dueDate < minDateOverdue then
            minDateOverdue:=books.current[j].dueDate;
        end else if (books.current[j].dueDate<=redTime) then begin
          if (books.current[j].status in BOOK_NOT_EXTENDABLE) then begin
            booksSoonNotExtendable.Add(books.current[j].toSimpleString());
            if books.current[j].dueDate < minDateSoonNotExtendable then
              minDateSoonNotExtendable:=books.current[j].dueDate;
           end else begin
            booksSoon.Add(books.current[j].toSimpleString());
            if books.current[j].dueDate < minDateSoon then
              minDateSoon:=books.current[j].dueDate;
           end;
        end
      end;
  if (lastWarnDate + WarnInterval <= currentDate) and (booksOverdue.Count + booksSoonNotExtendable.Count + booksSoon.Count > 0) then begin
    alert:='';
    if booksOverdue.Count > 0 then begin
      alert+=Format('Die folgenden Medien (%d) sind überfällig und sollten schon bis %s abgegeben worden sein:'#13, [booksOverdue.Count, DateToPrettyGrammarStr('zum ','',minDateOverdue)]);
      alert+=#9+strJoin(booksOverdue, #13#9, -10)+#13#13;
    end;
    if booksSoonNotExtendable.Count > 0 then begin
      alert+=Format('Die folgenden Medien (%d) sind nicht verlängerbar und sollten bis %s abgegeben werden:'#13, [booksSoonNotExtendable.Count, DateToPrettyGrammarStr('zum ','',minDateSoonNotExtendable)]);
      alert+=#9+strJoin(booksSoonNotExtendable, #13#9, -10)+#13#13;
    end;
    if booksSoon.Count > 0 then begin
      alert+=Format('Die folgenden Medien (%d) sind bald bis %s fällig:'#13, [booksSoon.Count, DateToPrettyGrammarStr('zum ','',minDateSoon)]);
      alert+=#9+strJoin(booksSoon, #13#9, -10)+#13;
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
