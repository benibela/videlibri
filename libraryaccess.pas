{
 Unit zum Zugriff auf die Büchereiseiten
 Diese Unit enthält die einzigsten beide Funktionen (TUpdateLibThread.execute und extendAccountBookData),
 die die Daten der Konten verändert und speichert
}

unit libraryAccess;

{$mode objfpc}{$H+}


interface
uses
  Classes, SysUtils,libraryParser,booklistreader;

//--Aktualisierungen--
type
  TThreadConfig=record
    //oneThreadSuccessful: boolean;//write only true
    libraryAccessSection: TRTLCriticalSection;
    threadManagementSection: TRTLCriticalSection;
    updateThreadsRunning:integer; //all threads
    listUpdateThreadsRunning: integer; //count of threads which are updating the list of books (and have not started updating singely)
    atLeastOneListUpdateSuccessful: boolean;
  end;
var updateThreadConfig: TThreadConfig;
procedure ThreadDone(sender:TObject);

//Aktualisiert eine (oder bei lib=nil alle) Konten in einem extra-thread
procedure updateAccountBookData(account: TCustomAccountAccess;ignoreConnErrors, checkDate,extendAlways: boolean);
procedure defaultAccountsRefresh;

procedure startCheckThread;

//--Verlängerungen--
//Verlängert die Bücher des Accounts indem extendAccountBookData aufgerufen wird
//procedure extendAccountBookData(account: TCustomAccountAccess;books: TBookArray);
procedure extendBooks(books: TBookList);
procedure extendBooks(lastLimit:longint; account: TCustomAccountAccess=nil);


//--Userkommunikation--
//Überprüft jeden *Tag* ob nun Medien fällig sind
procedure startDailyCheckDate;
//benachrichtigt den Benutzer über fällige Medien und fragt nach dem Öffnen des
//Hauptformulars deswegen (->true wenn es geöffnet werden soll)
function alertAboutBooksThatMustBeReturned:boolean;
implementation
uses applicationconfig,internetaccess,w32internetAccess,bookwatchmain,windows,bbdebugtools;
const TRY_BOOK_UPDATE='Versuche Mediendaten zu aktualisieren...';

//==============================================================================
//============================Aktualisierungs Thread============================
//==============================================================================
type
  PThreadConfig=^TThreadConfig;
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

var checkingThreadRunning: boolean=false;

procedure TUpdateLibThread.execute;
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
if exists books-to-extend then begin
  if exists #extend-list then extend-list
  else if exists #extend-single then for all: #extend-single
  else extend-all
end
(disconnect)
save
}
  if logging then begin
    log('TUpdateLibThread.execute(@lib='+inttostr(longint(lib))+') started');
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

    internet:=TW32InternetAccess.create;
    try
      if logging then log('TUpdateLibThread.execute ended marker 1');
      lib.connect(internet);
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
        if (pconfig^.listUpdateThreadsRunning<=0) and (mainform<>nil) and (mainForm.visible) then
          Synchronize(@mainForm.RefreshListView);

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
        etAlways: //extend always (when there are books which can be extended)
          booksToExtendCount:=booksExtendableCount;
        etAllDepends,etSingleDepends: begin
          for i:=0 to lib.books.currentUpdate.Count-1 do //check for books to extend
            if lib.shouldExtendBook(lib.books.currentUpdate[i]) then
              booksToExtendCount+=1;
          if (lib.extendType=etAllDepends) and (booksToExtendCount>0) then
            booksToExtendCount:=booksExtendableCount;
        end;
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

    //lib.disconnect();
    finally
      internet.free;
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
    on e: exception do
      storeException(e,lib);
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
//var i,j:integer;
begin
  if logging then log('ThreadDone started'#13#10'Without this one, '+IntToStr(updateThreadConfig.updateThreadsRunning)+' threads are currently running');
  if not (sender is TUpdateLibThread) then
    raise exception.Create('Interner Fehler:'#13#10'Die Funktion, die für gerade beendete Aktualisierungthread zuständig ist, wurde auf einen anderen Thread angewendet'#13#10'(kann eigentlich nicht auftreten)');


  if (updateThreadConfig.updateThreadsRunning<=0) or (updateThreadConfig.listUpdateThreadsRunning<=0) then begin
    if (updateThreadConfig.atLeastOneListUpdateSuccessful) then begin
      updateThreadConfig.atLeastOneListUpdateSuccessful:=updateThreadConfig.updateThreadsRunning>0;
      accountsRefreshed:=true;
      if (mainform<>nil) and (mainform.visible) then
        mainform.RefreshListView;
      applicationUpdate(true);
    end;
    if mainform<>nil then
      mainform.delayedCall.Enabled:=true //show error messages
     else if not lclstarted then
      showErrorMessages;
  end;
  if (updateThreadConfig.updateThreadsRunning<=0) then
    updateGlobalAccountDates();

  if logging then log('ThreadDone ended');
end;

//Aufruf des Aktualisierungsthread
procedure updateAccountBookData(account: TCustomAccountAccess;ignoreConnErrors,checkDate,extendAlways: boolean);
var i: longint;
begin
  if (account=nil) and (updateThreadConfig.updateThreadsRunning>0) then exit;
  if (account<>nil) and (account.isThreadRunning) then exit;

  if (account=nil)and(accountIDs.count=1) then
    account:=TCustomAccountAccess(accountIDs.Objects[0]);
    
  if refreshAllAndIgnoreDate then begin
    ignoreConnErrors:=false;
    checkDate:=false;
  end;
      
//  threadConfig.baseWindow:=handle;
  if account=nil then
    updateThreadConfig.atLeastOneListUpdateSuccessful:=false;
  if (mainform<>nil) and (mainForm.Visible) then
    mainform.StatusBar1.Panels[0].text:=TRY_BOOK_UPDATE;
  if account<>nil then begin
    EnterCriticalSection(updateThreadConfig.threadManagementSection);
    updateThreadConfig.updateThreadsRunning+=1;
    updateThreadConfig.listUpdateThreadsRunning+=1;
    LeaveCriticalSection(updateThreadConfig.threadManagementSection);

    account.isThreadRunning:=true;
    TUpdateLibThread.Create(account,updateThreadConfig,ignoreConnErrors,checkDate,extendAlways);
  end else begin
    EnterCriticalSection(updateThreadConfig.threadManagementSection);
    updateThreadConfig.updateThreadsRunning:=accountIDs.count;
    updateThreadConfig.listUpdateThreadsRunning:=accountIDs.count;
    LeaveCriticalSection(updateThreadConfig.threadManagementSection);
    for i:=0 to accountIDs.count-1 do begin

      TCustomAccountAccess(accountIDs.Objects[i]).isThreadRunning:=true;
      TUpdateLibThread.create(TCustomAccountAccess(accountIDs.Objects[i]),updateThreadConfig,
                              ignoreConnErrors,checkDate,extendAlways);
    end;
  end;
end;
//Bücher aktualisieren
procedure defaultAccountsRefresh;
begin
  if accountsRefreshed then exit;
  if logging then log('defaultAccountsRefresh started');
  case userConfig.ReadInteger('base','startup-connection',1) of
    1: updateAccountBookData(nil,true,true,false);
    2: updateAccountBookData(nil,true,false,false);
  end;
  if logging then log('defaultAccountsRefresh ended');
end;

type
  { TPersistentCheckThread }

  TPersistentCheckThread=class(TThread)
  protected
    procedure execute;override;
  public
    constructor Create();
  end;

{ TPersistentCheckThread }

procedure TPersistentCheckThread.execute();
//procedure defaultCheckThread(lpParameter: pointer);stdcall;
var internet:TInternetAccess;
begin
  while (not accountsRefreshed) and (tna<>nil) do begin
    try
      internet:=TW32InternetAccess.create;
      try
        if internet.existsConnection then begin
          //defaultAccountsRefresh;
          if logging then log('defaultCheckThread internet connection exists');
          postMessage(tna.messageWindow,wm_command,MENU_ID_AUTO_UPDATE,0);
          if logging then log('message posted');
          internet.free;
          if logging then log('internet freed');
          checkingThreadRunning:=false;
          exit;
          //messagebeep(0);
        end;
      finally
        internet.free;
      end;
    except
    end;
    sleep(5*60*1000);
  end;
  checkingThreadRunning:=false;
end;

constructor TPersistentCheckThread.Create();
begin
  FreeOnTerminate:=true;
  inherited create(false);
end;

procedure startCheckThread;
begin
  checkingThreadRunning:=true;
  TPersistentCheckThread.Create();
end;


procedure extendAccountBookData(account: TCustomAccountAccess;
  books: TBookList);
var internet:TW32InternetAccess;
begin
  try
    if not assigned(account) then
      raise EXCEPTION.Create('Kein Konto zum Verlängern ausgewählt');
    if account.isThreadRunning then
      exit; //TODO: mehrere Threads beim verlängern erlauben
    if GetThreadID <> MainThreadID then
      exit; //Nur vom Haupthread aus aufrufen

    if (account<>nil) and (books.Count>0) then begin
        internet:=TW32InternetAccess.create();
      try
        if not account.connected then begin
          account.connect(internet);
          account.updateAll();
          if account.needSingleBookCheck() then
            account.updateAllSingly;
        end;
        account.extendList(books);
        account.books.merge(true);
        account.save();
      finally
        internet.free;
      end;
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
  //SetLength(accBoo,accountIDs.Count);
  current:=TBookList.Create;
  for i:=0 to accountIDs.Count-1 do begin
    current.clear;
    for j:=0 to books.count-1 do
      if books[j].owner=accountIDs.Objects[i] then
        current.add(books[j]);
    extendAccountBookData(TCustomAccountAccess(accountIDs.Objects[i]),current);
  end;
  current.free;
  showErrorMessages();
  if lclStarted and (mainform<>nil) and (mainform.visible) then
    mainform.RefreshListView;
end;

procedure extendBooks(lastLimit: longint; account: TCustomAccountAccess);
var i:integer;
    books: TBookList;
begin
  if account=nil then begin
     for i:=0 to accountIDs.count-1 do
       extendBooks(lastLimit,TCustomAccountAccess(accountIDs.Objects[i]));
     exit;
  end;
  books:=TBookList.Create;
  //TODO: Optimize
  for i:=0 to account.books.current.Count-1 do
    if (account.books.current[i].limitDate<=lastLimit) and
       (account.books.current[i].status in BOOK_EXTENDABLE) then
       books.add(account.books.current[i]);
  extendAccountBookData(account,books);
  books.free;

  showErrorMessages();
  if lclStarted and (mainform<>nil) and (mainform.visible) then
    mainform.RefreshListView;
end;


{ TDailyCheckThread }

type
  { TDailyCheckThread }

  TDailyCheckThread=class(TThread)
  protected
    procedure execute;override;
  public
    constructor Create();
  end;

procedure TDailyCheckThread.execute;
begin
  while tna<>nil do begin
    sleep(1000*60*60*24);
    if not checkingThreadRunning then begin
      accountsRefreshed:=false;
      if alertAboutBooksThatMustBeReturned then
        PostMessage(tna.messageWindow,WM_COMMAND,MENU_ID_START_LCL,0);
    end;
  end;
end;

constructor TDailyCheckThread.Create();
begin
  FreeOnTerminate:=true;
  inherited create(false);
end;


procedure startDailyCheckDate;
begin
  TDailyCheckThread.Create();
end;

function alertAboutBooksThatMustBeReturned: boolean;
var alert:string;
    tempInternet: TInternetAccess;
    i,j,count:longint;
begin
  result:=false;
  currentDate:=longint(trunc(now));
  count:=0;

  if nextLimit<currentDate then begin
    for i:=0 to accountIDs.count-1 do
      with TCustomAccountAccess(accountIDs.Objects[i]) do
        for j:=0  to books.current.count-1 do
          if books.current[j].limitDate<currentDate then
            count+=1;
    alert:='Einige Medien ('+inttostr(count)+') sind überfällig und sollten schon bis '+DateToPrettyGrammarStr('zum ','',nextLimit)+' abgegeben worden sein.'#13#10'Wollen Sie eine Liste dieser Medien angezeigt bekommen? (Wenn Sie die Medien schon abgegeben haben, müssen Sie die Medienliste aktualisieren)';
    if MessageBox(0,pchar(alert),'VideLibri',MB_YESNO or MB_ICONWARNING or MB_SYSTEMMODAL)=IDYES then
      result:=true;
  end else if nextNotExtendableLimit<=redTime then begin
    for i:=0 to accountIDs.count-1 do
      with TCustomAccountAccess(accountIDs.Objects[i]) do
        for j:=0  to books.current.count-1 do
          with books.current[j] do
            if (limitDate<=redTime) and (status in BOOK_NOT_EXTENDABLE) then
              count+=1;
    alert:='Bald (bis '+DateToPrettyGrammarStr('zum ','',nextNotExtendableLimit)+') müssen einige nicht verlängerbare Medien ('+IntToStr(count)+') abgegeben werden.'#13#10'Wollen Sie eine Liste dieser Medien angezeigt bekommen?';
    if MessageBox(0,pchar(alert),'VideLibri',MB_YESNO or MB_ICONWARNING or MB_SYSTEMMODAL)=IDYES then
      result:=true;
  end else if nextLimit<=redTime then begin
    for i:=0 to accountIDs.count-1 do
      with TCustomAccountAccess(accountIDs.Objects[i]) do
        for j:=0  to books.current.count-1 do
          if books.current[i].limitDate<=redTime then
            count+=1;
    alert:='Bald (bis '+DateToPrettyGrammarStr('zum ','',nextLimit)+') müssen einige Medien ('+IntToStr(count)+') abgegeben werden.'#13#10'Die Medien können allerdings verlängert werden, soll dies jetzt versucht werden?';
    if MessageBox(0,pchar(alert),'VideLibri',MB_YESNO or MB_ICONWARNING or MB_SYSTEMMODAL)=IDYES then
      result:=true;
    tempInternet:=TW32InternetAccess.create;
    if not tempInternet.needConnection() then begin
      alert:='Der Aufbau einer Internetverbindung zum Verlängern ist fehlgeschlagen'#13#10'Wollen sie dafür eine Liste der abzugebenden Medien angezeigt bekommen?';
      if MessageBox(0,pchar(alert),'VideLibri',MB_YESNO or MB_ICONWARNING or MB_SYSTEMMODAL)=IDYES then
        result:=true;
    end;
    tempInternet.free;
  end;

  if (not result) and not checkingThreadRunning then startCheckThread;
end;



end.

