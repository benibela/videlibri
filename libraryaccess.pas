{
 Unit zum Zugriff auf die Büchereiseiten
 Diese Unit enthält die einzigsten beide Funktionen (TUpdateLibThread.execute und extendAccountBookData),
 die die Daten der Konten verändert und speichert
}

unit libraryAccess;

{$mode objfpc}{$H+}


interface
uses
  Classes, SysUtils,libraryParser,booklistreader, LCLType, bbutils,xquery;

type TBookListOperation = procedure (list: TBookList) of object;

//--Aktualisierungen--
type
  TThreadConfig=record
    //oneThreadSuccessful: boolean;//write only true
    libraryAccessSection: TRTLCriticalSection; //this protects reads/writes to VideLibri's book (lists). It does NOT protect read/writes to the variable log of the reader (because standalone queries in the multipage template are unprotected. todo: either remove protection on patterns or add to all queries)
    threadManagementSection: TRTLCriticalSection;
    libraryFileAccess: TRTLCriticalSection; //access to the book list files (update vs. export). todo: do not block threads of multiple accounts
    updateThreadsRunning:integer; //all threads
    listUpdateThreadsRunning: integer; //count of threads which are updating the list of books (and have not started updating singely)
    successfulListUpdateDate: longint;
    OnPartialUpdated: TNotifyEvent;
  end;
var updateThreadConfig: TThreadConfig;
type TThreadDoneHolder = object
  procedure ThreadDone(sender:TObject);
end;
var ThreadDone: TThreadDoneHolder;

//Aktualisiert eine (oder bei lib=nil alle) Konten in einem extra-thread
function updateAccountBookData(account: TCustomAccountAccess;ignoreConnErrors, checkDate,extendAlways: boolean): boolean;
function defaultAccountsRefresh: boolean;

//--Verlängerungen--
//Verlängert die Bücher des Accounts indem extendAccountBookData aufgerufen wird
//procedure extendAccountBookData(account: TCustomAccountAccess;books: TBookArray);
procedure extendBooks(books: TBookList);
procedure extendBooks(lastLimit:longint; account: TCustomAccountAccess=nil);

//procedure orderBooks(books: TBookList);
procedure cancelBooks(books: TBookList);

function queryHistory(reader:TBookListReader;q:string):ixqvalue;
//--Userkommunikation--

procedure findBooksThatMustBeReturned(out booksOverdue, booksSoonNotExtendable, booksSoon: TList; out minDateOverdue, minDateSoonNotExtendable, minDateSoon: integer);
//benachrichtigt den Benutzer über fällige Medien und fragt nach dem Öffnen des
//Hauptformulars deswegen (->true wenn es geöffnet werden soll)


type PThreadConfig=^TThreadConfig;
//procedure updateBooksDirectBlocking(const lib: TCustomAccountAccess; const pconfig: PThreadConfig; const ignoreConnectionErrors, checkDate, forceExtend: boolean);

implementation
uses applicationconfig,internetaccess,bbdebugtools,androidutils{$ifdef android},bbjniutils{$endif};
const TRY_BOOK_UPDATE='Versuche Mediendaten zu aktualisieren...';


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

    function queueRequest(request: TBookProcessingRequest): boolean;
    procedure processRequest(request: TBookProcessingRequest);
    procedure execute;override;
    //procedure exceptionRaised(raisedException:Exception);
    //procedure showError;
  public
    constructor Create(alib: TCustomAccountAccess;var config:TThreadConfig; someRequests: TFPList);
    constructor Create(alib: TCustomAccountAccess;var config:TThreadConfig; request: TBookProcessingRequest);
  end;

function TUpdateLibThread.queueRequest(request: TBookProcessingRequest): boolean;
//must synchronized with threadManagementSection
var
  lastRequest: TBookProcessingRequest;
  i: Integer;
const alreadyProcessedRequests = 1; //the first one is the currently processed
begin
  result := true;
  if requests.Count > alreadyProcessedRequests then begin
    lastRequest := TObject(requests.Last) as TBookProcessingRequest;
    if (lastRequest.ignoreConnectionErrors = request.ignoreConnectionErrors)
       and (lastRequest.checkDate = request.checkDate)
       and (lastRequest.useExtendOverride = request.useExtendOverride)
       and (lastRequest.ExtendOverride = request.ExtendOverride) then begin
      if (lastRequest.partialList = nil) and (lastRequest.partialListOperation = nil) then begin
        request.free; //no point in updating twice, it will do the same
        result := false;
        exit;
      end;
      if (lastRequest.partialListOperation = request.partialListOperation) then begin
        for i := 0 to request.partialList.Count - 1 do begin
          if lastRequest.partialList.IndexOf(request.partialList[i]) >= 0 then begin
            if logging then log('TUpdateLibThread.queueRequest: Skipping ' + request.partialList[i].toSimpleString());
            continue;
          end;
          lastRequest.partialList.add(request.partialList[i]);
        end;
        request.partialList.free;
        request.free;
        exit;
      end;
    end;
  end;
  requests.Add(request);
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
    log('TUpdateLibThread.processRequest(@lib='+strFromPtr(pointer(lib))+') started');
    log('Library is: '+lib.prettyName);
  end;

  try
  try
    if lib=nil then
      raise ELibraryException.create('Interner Fehler'#13#10'Aufruf von TUpdateLibThread.execute für einen nicht existierenden Account');
    with request do begin
      if checkDate then
        if not lib.needChecking then begin
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
        if lib.needSingleBookUpdate then begin
          {if logging then log('TUpdateLibThread.execute marker 3.1');
          EnterCriticalSection(pconfig^.libraryAccessSection);
          lib.books.mergePersistentToCurrentUpdate;
          LeaveCriticalSection(pconfig^.libraryAccessSection);
          if logging then log('TUpdateLibThread.execute marker 3.4');}


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

        if lib.needSingleBookUpdate then
          lib.updateAllSingly;
      end;

      //other, special operation
      if (partialList <> nil) and (Assigned(partialListOperation)) then begin
        if logging then log('Updating book list: '+IntToStr(partialList.Count));
        newPartialBookList := TBookList.create();
        for i := 0 to partialList.Count - 1 do begin
          repBook := lib.books.currentUpdate.findBook(partialList[i]);
          if repBook = nil then begin
            if logging then log('Not found: '+partialList[i].toSimpleString());
            Continue;
          end;
          if not (repBook.status in BOOK_NOT_LEND) then
            if repBook.dueDate > partialList[i].dueDate then begin
              if logging then log('Skipping: '+partialList[i].toLimitString() + ' because ' + repBook.toLimitString()) ;
              continue; //was already renewed
            end;
          //if repBook = nil then repBook := lib.books.current.findBook(partialList[i]); ??
          newPartialBookList.add(repBook);
        end;

        partialList.free;
        partialList := newPartialBookList;
        partialListOperation(partialList);
        if lib.needSingleBookUpdate then
          lib.updateAllSingly;
      end;


      if logging then log('TUpdateLibThread.execute ended marker 5');
      EnterCriticalSection(pconfig^.libraryAccessSection);
      try
        lib.books.mergePersistentToCurrentUpdate;
        lib.books.completeUpdate();
      finally
        LeaveCriticalSection(pconfig^.libraryAccessSection);
      end;
      if logging then log('TUpdateLibThread.execute ended marker 8');

      EnterCriticalSection(pconfig^.libraryFileAccess);
      try
        lib.save();
      finally
        LeaveCriticalsection(pconfig^.libraryFileAccess);
      end;

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
  EnterCriticalsection(pconfig^.threadManagementSection);
  while requests.Count > 0 do begin
    request := TObject(requests.First) as TBookProcessingRequest;
    LeaveCriticalsection(pconfig^.threadManagementSection);

    processRequest(request);

    EnterCriticalsection(pconfig^.threadManagementSection);
    requests.Delete(0);
  end;
  FreeAndNil(requests);
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
  xquery.freeThreadVars    ;
  if logging then log('TUpdateLibThread.execute ended');
end;

constructor TUpdateLibThread.Create(alib: TCustomAccountAccess; var config: TThreadConfig; someRequests: TFPList);
begin
  lib:=alib;
  pconfig:=@config;
  requests := someRequests;

  OnTerminate:= @ThreadDone.ThreadDone;
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

function performAccountAction(account: TCustomAccountAccess;var config:TThreadConfig;
                            aIgnoreConnectionErrors, ACheckDate: boolean;
                            AUseExtendOverride: Boolean; AExtendOverride: TExtendType;
                            apartialList: TBookList; apartialListOperation: TBookListOperation): boolean;
var
  request: TBookProcessingRequest;
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
  if (account.thread <> nil) and  ((account.thread as TUpdateLibThread).requests <> nil) then
    result := (account.thread as TUpdateLibThread).queueRequest(request)
  else begin
    config.updateThreadsRunning+=1;
    config.listUpdateThreadsRunning+=1;
    account.thread := TUpdateLibThread.Create(account,config,request);
    result := true;
  end;
  LeaveCriticalSection(config.threadManagementSection);
end;

procedure TThreadDoneHolder.ThreadDone(sender:TObject);
//called in the main thread on normal OS
//called in update thread on Android!
begin
  if logging then log('ThreadDone started'#13#10'Without this one, '+IntToStr(updateThreadConfig.updateThreadsRunning)+' threads are currently running');
//  Assert(mainForm<>nil);
//log(booltostr(sender is TUpdateLibThread) );
  if not assigned(sender) or not sender.InheritsFrom(TUpdateLibThread) then
    raise exception.Create('Interner Fehler:'#13#10'Die Funktion, die für gerade beendete Aktualisierungthread zuständig ist, wurde auf einen anderen Thread angewendet'#13#10'(kann eigentlich nicht auftreten)');

  {$ifdef android}EnterCriticalSection(updateThreadConfig.threadManagementSection);{$endif}

  if (updateThreadConfig.updateThreadsRunning<=0) then begin
    updateGlobalAccountDates();
    libraryManager.reloadPendingTemplates();
    accountsRefreshedDate := updateThreadConfig.successfulListUpdateDate;
    callbacks.allThreadsDone();
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
begin
  if logging then log('updateAccountBookData started');

  result := false;

  if account=nil then begin
    if updateThreadConfig.updateThreadsRunning = 0 then
      updateThreadConfig.successfulListUpdateDate:=0;

    for i:=0 to accounts.count-1 do
      if accounts[i].enabled then
        if updateAccountBookData(accounts[i], ignoreConnErrors, checkDate, extendAlways) then
          result := true;
    exit;
  end;

  if refreshAllAndIgnoreDate then begin
    ignoreConnErrors:=false;
    checkDate:=false;
  end;
  if checkDate and not account.needChecking then exit
  else checkDate := false;
  if ignoreConnErrors and (account.broken = currentDate) then exit;
  callbacks.statusChange(TRY_BOOK_UPDATE);
  result := performAccountAction(account,updateThreadConfig,ignoreConnErrors,checkDate,extendAlways,etAlways,nil,nil);
end;
//Bücher aktualisieren
function defaultAccountsRefresh: boolean;
begin
  result := false;
  if accountsRefreshedDate = currentDate then exit;
  if logging then log('defaultAccountsRefresh started');
  case userConfig.ReadInteger('base','startup-connection',1) of
    1: result := updateAccountBookData(nil,true,true,false);
    2: result := updateAccountBookData(nil,true,false,false);
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
      if books[j].owningAccount=accounts.Objects[i] then begin
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


function queryHistory(reader: TBookListReader; q: string): ixqvalue;
var
  list: TXQVList;

  procedure addBook(b: TBook);
  var
    obj: TXQValueObject;
  begin
    obj := reader.bookToPXP(b);
    obj.setMutable('_accountPtr', xqvalue(PtrInt(b.owningAccount)));
    list.add(obj);
  end;

var
  i, j: Integer;
begin
  list:=TXQVList.create();
  for i := 0 to accounts.Count-1 do
  begin
    for j := 0 to accounts[i].books.old.count-1 do
      addBook(accounts[i].books.old[j]);
    for j := 0 to accounts[i].books.current.count-1 do
      addBook(accounts[i].books.current[j]);
  end;
  reader.parser.variableChangeLog.add('books', TXQValueSequence.create(list));
  result:=reader.parser.QueryEngine.evaluate(q, xqpmXQuery3_1);
end;

procedure findBooksThatMustBeReturned(out booksOverdue, booksSoonNotExtendable, booksSoon: TList; out minDateOverdue, minDateSoonNotExtendable,
  minDateSoon: integer);
var
  i,j: Integer;
begin
  updateGlobalTimeCache;

  booksOverdue:=TList.Create;
  booksSoonNotExtendable:=TList.Create;
  booksSoon:=TList.Create;

  minDateOverdue := currentDate+1000;
  minDateSoonNotExtendable := currentDate+1000;
  minDateSoon := currentDate+1000;

  system.EnterCriticalSection(updateThreadConfig.libraryAccessSection);
  try
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
    finally
      system.LeaveCriticalSection(updateThreadConfig.libraryAccessSection);
    end;
end;






end.

