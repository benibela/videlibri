{
 Unit zum Zugriff auf die Büchereiseiten
 Diese Unit enthält die einzigsten beide Funktionen (TUpdateLibThread.execute und extendAccountBookData),
 die die Daten der Konten verändert und speichert
}

unit libraryAccess;

{$mode objfpc}{$H+}


interface
uses
  Classes, SysUtils,libraryParser;

//--Aktualisierungen--
type
  TThreadConfig=record
    oneThreadSuccessful: boolean;//write only true
    libraryAccessSection: TRTLCriticalSection;
    groupThreadsRunning:integer;
    singleBookThreadStarted:integer;
    singleBookThreadStopped:integer;
  end;
var updateThreadsRunning: longint = 0;
    updateThreadConfig: TThreadConfig;
procedure ThreadDone(sender:TObject);

//Aktualisiert eine (oder bei lib=nil alle) Konten in einem extra-thread
procedure updateAccountBookData(account: TCustomAccountAccess;ignoreConnErrors, checkDate,extendAlways: boolean);
procedure defaultAccountsRefresh;

procedure startCheckThread;

//--Verlängerungen--
//Verlängert die Bücher des Accounts indem extendAccountBookData aufgerufen wird
//procedure extendAccountBookData(account: TCustomAccountAccess;books: TBookArray);
procedure extendBooks(books: TBookArray);
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
    noLongerGroupThread: boolean;
    extend: boolean;
    procedure execute;override;
    procedure exceptionRaised(raisedException:Exception);
    procedure showError;
  public
    constructor Create(alib: TCustomAccountAccess;var config:TThreadConfig;
                       aIgnoreConnectionErrors, ACheckDate,AExtend: boolean);
  end;

var checkingThreadRunning: boolean=false;

procedure TUpdateLibThread.execute;
var internet: TInternetAccess;
begin
//Die Funktionsweise ist folgende:
//   connect         verbindet zur Bücherei
//
//   parseBooks      liest die Mediendaten und schreibt sie thread-save in
//                   bltInCurrentDataUpdate
//   (merge)**       wenn needSingleBookCheck gesetzt ist, werden die Mediendaten übertragen,
//                   damit die Anzahl und die neuesten Informationen angezeigt werden können
//   (parse...ByOne) wenn needSingleBookCheck gesetzt ist, werden die Mediendaten einzeln gelesen
//   merge**         aktualisiert die anzeigbaren Mediendaten
//   save            speichert die Mediendaten auf der Festplatte
//   disconnect      beendet die Verbindung

//  **: Aufruf verändert die gelesenen Daten, die Zugriffe müssen also synchronisiert erfolgen.
  if logging then begin
    log('TUpdateLibThread.execute(@lib='+inttostr(longint(lib))+') started');
    log('Library is: '+lib.prettyName);
  end;
  try
    noLongerGroupThread:=false;
    if lib=nil then
      raise ELibraryException.create('Interner Fehler'#13#10'Aufruf von TUpdateLibThread.execute für einen nicht existierenden Account');
    if checkDate then
      if (lib.lastCheckDate>currentDate-refreshInterval) and (not lib.checkMaximalBookTimeLimit) then begin
        InterlockedDecrement(pconfig^.groupThreadsRunning);
        lib.isThreadRunning:=false;
        if logging then log('TUpdateLibThread.execute ended without checking');
        exit;
      end;

    internet:=TW32InternetAccess.create;
    try
      if logging then log('TUpdateLibThread.execute ended marker 1');
      lib.connect(internet);
      if logging then log('TUpdateLibThread.execute ended marker 2');
      lib.parseBooks(extend);
      if logging then log('TUpdateLibThread.execute ended marker 3');
      if lib.needSingleBookCheck() then begin
        //InterlockedIncrement(@pconfig^.totalBookThreadDone);
        //Synchronize(@mainForm.RefreshListView);
        if logging then log('TUpdateLibThread.execute ended marker 3.1');
        EnterCriticalSection(pconfig^.libraryAccessSection);
        if logging then log('TUpdateLibThread.execute ended marker 3.2');
        lib.getBooks().merge(false);
        if logging then log('TUpdateLibThread.execute ended marker 3.3');
        LeaveCriticalSection(pconfig^.libraryAccessSection);
        if logging then log('TUpdateLibThread.execute ended marker 3.4');
        pconfig^.oneThreadSuccessful:=true;

        if logging then log('TUpdateLibThread.execute ended marker 3.5');
        InterlockedDecrement(pconfig^.groupThreadsRunning);
        InterlockedIncrement(pconfig^.singleBookThreadStarted);
        if logging then log('TUpdateLibThread.execute ended marker 3.6');
        if (pconfig^.groupThreadsRunning<=0) and (mainform<>nil) and (mainForm.visible) then
          Synchronize(@mainForm.RefreshListView);

        noLongerGroupThread:=true;
        if logging then log('TUpdateLibThread.execute ended marker 3.7');
        lib.parseBooksOneByOne(extend);
      end;
      if logging then log('TUpdateLibThread.execute ended marker 4');
    //lib.disconnect();
    finally
      internet.free;
    end;

    if logging then log('TUpdateLibThread.execute ended marker 5');
    EnterCriticalSection(pconfig^.libraryAccessSection);
    if logging then log('TUpdateLibThread.execute ended marker 6');
    lib.getBooks().merge(true);
    if logging then log('TUpdateLibThread.execute ended marker 7');
    LeaveCriticalSection(pconfig^.libraryAccessSection);
    if logging then log('TUpdateLibThread.execute ended marker 8');

    lib.save();

    if logging then log('TUpdateLibThread.execute ended marker 9');
    pconfig^.oneThreadSuccessful:=true;
  except
    on e: EInternetException do {$ifndef activeDebug}if not ignoreConnectionErrors then{$endif}
      exceptionRaised(e);
    on e: exception do
      exceptionRaised(e);
    else if logging then log('Unverständliche Fehlermeldung');
  end;
  if not noLongerGroupThread then
    InterlockedDecrement(pconfig^.groupThreadsRunning)
   else
    InterlockedIncrement(pconfig^.singleBookThreadStopped);
  lib.isThreadRunning:=false;
  if logging then log('TUpdateLibThread.execute ended');
end;


procedure TUpdateLibThread.showError;
//var i:integer;
begin
  if messageShown then exit;
  messageShown:=true;
  addErrorMessage(errorStr,errordetails,lib);
  messageShown:=false;
end;
procedure TUpdateLibThread.exceptionRaised(raisedException:Exception);
var i:integer;
begin
  if raisedException=nil then exit;
  createErrorMessageStr(raisedException,errorstr,errordetails,lib);
 // end;
  messageShown:=false;
  Synchronize(@showError);
end;

constructor TUpdateLibThread.Create(alib: TCustomAccountAccess;var config:TThreadConfig;
                                    aIgnoreConnectionErrors, ACheckDate,AExtend: boolean);
begin
  lib:=alib;
  ignoreConnectionErrors:=aignoreConnectionErrors;
  checkDate:=ACheckDate;
  pconfig:=@config;
  extend:=AExtend;
  FreeOnTerminate:=true;
  inherited create(false);
end;

procedure ThreadDone(sender:TObject);
//called in the main thread
//var i,j:integer;
var thread: TUpdateLibThread;
begin
  if logging then log('ThreadDone started'#13#10'Without this one, '+IntToStr(updateThreadsRunning)+' threads are currently running');
  if not (sender is TUpdateLibThread) then
    raise exception.Create('Interner Fehler:'#13#10'Die Funktion, die für gerade beendete Aktualisierungthread zuständig ist, wurde auf einen anderen Thread angewendet'#13#10'(kann eigentlich nicht auftreten)');
  dec(updateThreadsRunning);
  if (updateThreadsRunning<=0) or (((updateThreadConfig.groupThreadsRunning<=0))and not ((sender as TUpdateLibThread).noLongerGroupThread))
     or ((updateThreadConfig.singleBookThreadStopped>=updateThreadConfig.singleBookThreadStarted)and ((sender as TUpdateLibThread).noLongerGroupThread)) then begin
    if (updateThreadConfig.oneThreadSuccessful) then begin
      updateThreadConfig.oneThreadSuccessful:=false;
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
  if (updateThreadsRunning<=0) then
    updateGlobalAccountDates();
  if (updateThreadsRunning<=0) and (mainform<>nil) then
    {if mainform.StatusBar1.Panels[0].text=TRY_BOOK_UPDATE then }begin
      if abs(lastCheck-currentDate)>2 then
        mainform.StatusBar1.Panels[0].text:='Älteste angezeigte Daten sind vom '+DateToStr(lastCheck)
       else
        mainform.StatusBar1.Panels[0].text:='Älteste angezeigte Daten sind von '+DateToPrettyStr(lastCheck)
    end;
  if logging then log('ThreadDone ended');
end;

//Aufruf des Aktualisierungsthread
procedure updateAccountBookData(account: TCustomAccountAccess;ignoreConnErrors,checkDate,extendAlways: boolean);
var i: longint;
begin
  if (account=nil) and (updateThreadsRunning>0) then exit;
  if (account<>nil) and (account.isThreadRunning) then exit;

  if (account=nil)and(accountIDs.count=1) then
    account:=TCustomAccountAccess(accountIDs.Objects[0]);
    
  if refreshAllAndIgnoreDate then begin
    ignoreConnErrors:=false;
    checkDate:=false;
  end;
      
//  threadConfig.baseWindow:=handle;
  updateThreadConfig.oneThreadSuccessful:=false;
  if mainform<>nil then
    mainform.StatusBar1.Panels[0].text:=TRY_BOOK_UPDATE;
  if account<>nil then begin
    inc(updateThreadsRunning);
    InterlockedIncrement(updateThreadConfig.groupThreadsRunning);
    account.isThreadRunning:=true;
    TUpdateLibThread.Create(account,updateThreadConfig,ignoreConnErrors,checkDate,extendAlways).
         OnTerminate:=@mainform.ThreadDone;
  end else begin
    updateThreadsRunning:=accountIDs.count;
    for i:=0 to accountIDs.count-1 do begin
      InterlockedIncrement(updateThreadConfig.groupThreadsRunning);
      TCustomAccountAccess(accountIDs.Objects[i]).isThreadRunning:=true;
      TUpdateLibThread.create(TCustomAccountAccess(accountIDs.Objects[i]),updateThreadConfig,
                              ignoreConnErrors,checkDate,extendAlways).
         OnTerminate:=@mainform.ThreadDone;
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
  books: TBookArray);
var internet:TW32InternetAccess;
begin
  try
    if not assigned(account) then
      raise EXCEPTION.Create('Kein Konto zum Verlängern ausgewählt');
    if account.isThreadRunning then
      exit; //TODO: mehrere Threads beim verlängern erlauben
    if GetThreadID <> MainThreadID then
      exit; //Nur vom Haupthread aus aufrufen

    if (account<>nil) and (length(books)>0) then begin
        internet:=TW32InternetAccess.create();
      try
        account.connect(internet);
        account.parseSingleBooks(books,true);
        account.disconnect();
        account.getBooks().merge(true);
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

procedure extendBooks(books: TBookArray);
var current:TBookArray;
    i,j:integer;
begin
  //TODO: optimize
  //SetLength(accBoo,accountIDs.Count);
  for i:=0 to accountIDs.Count-1 do begin
    setlength(current,0);
    for j:=0 to high(books) do begin
      if books[j]^.lib=accountIDs.Objects[i] then begin
        SetLength(current,length(current)+1);
        current[high(current)]:=books[j];
      end;
    end;
    extendAccountBookData(TCustomAccountAccess(accountIDs.Objects[i]),current);
  end;
  showErrorMessages();
  if lclStarted and (mainform<>nil) and (mainform.visible) then
    mainform.RefreshListView;
end;

procedure extendBooks(lastLimit: longint; account: TCustomAccountAccess);
var i:integer;
    books: TBookArray;
begin
  if account=nil then begin
     for i:=0 to accountIDs.count-1 do
       extendBooks(lastLimit,TCustomAccountAccess(accountIDs.Objects[i]));
     exit;
  end;
  setlength(books,0);
  //TODO: Optimize
  for i:=0 to account.getBooks().getBookCount(botCurrent)-1 do
    if (account.getBooks().getBook(botCurrent,i)^.limitDate<=lastLimit) and
       (account.getBooks().getBook(botCurrent,i)^.status in BOOK_EXTENDABLE)then begin
      setlength(books,length(books)+1);
      books[high(books)]:=account.getBooks().getBook(botCurrent,i);
    end;
  extendAccountBookData(account,books);

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
      with TCustomAccountAccess(accountIDs.Objects[i]).getBooks() do
        for j:=0  to getBookCount(botCurrent)-1 do
          if getBook(botCurrent,j)^.limitDate<currentDate then
            count+=1;
    alert:='Einige Medien ('+inttostr(count)+') sind überfällig und sollten schon bis '+DateToPrettyGrammarStr('zum ','',nextLimit)+' abgegeben worden sein.'#13#10'Wollen Sie eine Liste dieser Medien angezeigt bekommen? (Wenn Sie die Medien schon abgegeben haben, müssen Sie die Medienliste aktualisieren)';
    if MessageBox(0,pchar(alert),'VideLibri',MB_YESNO or MB_ICONWARNING or MB_SYSTEMMODAL)=IDYES then
      result:=true;
  end else if nextNotExtendableLimit<=redTime then begin
    for i:=0 to accountIDs.count-1 do
      with TCustomAccountAccess(accountIDs.Objects[i]).getBooks() do
        for j:=0  to getBookCount(botCurrent)-1 do
          with getBook(botCurrent,j)^ do
            if (limitDate<=redTime) and (status in BOOK_NOT_EXTENDABLE) then
              count+=1;
    alert:='Bald (bis '+DateToPrettyGrammarStr('zum ','',nextNotExtendableLimit)+') müssen einige nicht verlängerbare Medien ('+IntToStr(count)+') abgegeben werden.'#13#10'Wollen Sie eine Liste dieser Medien angezeigt bekommen?';
    if MessageBox(0,pchar(alert),'VideLibri',MB_YESNO or MB_ICONWARNING or MB_SYSTEMMODAL)=IDYES then
      result:=true;
  end else if nextLimit<=redTime then begin
    for i:=0 to accountIDs.count-1 do
      with TCustomAccountAccess(accountIDs.Objects[i]).getBooks() do
        for j:=0  to getBookCount(botCurrent)-1 do
          if getBook(botCurrent,j)^.limitDate<=redTime then
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

