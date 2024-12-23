unit applicationconfig;

{$I videlibrilanguageconfig.inc}

interface

uses
  Classes, SysUtils, libraryparser,inifiles, inifilessafe, rcmdline, bbutils, internetaccess, {$IFNDEF ANDROID}autoupdate,{$ENDIF}extendedhtmlparser,
accountlist{$IFNDEF ANDROID}, LMessages{$endif}, commoninterface;

{$IFNDEF ANDROID}
const
   LM_SHOW_VIDELIBRI = LM_USER + $4224;
{$ENDIF}

type EBookListReader=class(Exception)
  details:string;
  constructor create;
  constructor create(s:string;more_details:string='');
end;
ELoginException=class(EBookListReader)
end;
EBookListReaderFromWebpage = class(EBookListReader)
end;
EVideLibriHTMLMatchingException = class(EHTMLParseMatchingException)
  partialMatches, anonymousPartialMatches: string;
  constructor create(amessage: string; asender: THtmlTemplateParser);
end;

type
  TVideLibriExceptionDetails = record
    account: TCustomAccountAccess;
    details, anonymouseDetails, libraryId, searchQuery: string;
  end;
  TVideLibriException = record
    error: string;
    kind: TPendingExceptionKind;
    details: array of TVideLibriExceptionDetails;
  end;
  TErrorArray = array of TVideLibriException;

const VIDELIBRI_MUTEX_NAME='VideLibriStarted';

type TCallbackHolder = class
  class procedure updateAutostart(enabled, askBeforeChange: boolean); virtual;
  class procedure applicationUpdate(auto:boolean); virtual;
  class procedure statusChange(const message: string); virtual;
  class procedure allThreadsDone(); virtual;
  class procedure postInitApplication(); virtual;
end;
TCallbackHolderClass = class of TCallbackHolder;


type TStringAsMemoryStream = class(TMemoryStream)
  constructor create(const s: RawByteString);
end;

var programPath,userPath:string;
    machineConfig: TIniFile;
    userConfig: TSafeIniFile;

    accounts: TAccountList;
    libraryManager: TLibraryManager=nil;

    cancelStarting,startToTNA:boolean;
    accountsRefreshedDate: longint=0; //set to currentDate

    currentDate:longint;
    lastCheck: integer;
    nextLimit:longint=MaxInt-1;
    nextNotExtendableLimit:longint=MaxInt;
    nextLimitStr: string;

    appFullTitle:string='VideLibri';
    versionNumber:integer=2930;
    versionBuildId: string = 'unknown';
    //=>versionNumber/1000
    newVersionInstalled: boolean=false;

    {$IFDEF WIN32}startedMutex:THandle=0;{$ENDIF}

    exceptionStoring: TRTLCriticalSection;
    
    needApplicationRestart: boolean; //Soll das Programm nach Beenden neugestartet werden

    //TODO: customize colors in search panel colorSearchTextNotFound: tcolor=$6060FF;    //colorSearchTextFound: tcolor=clWindow;
    redTime: integer;
    RefreshInterval, WarnInterval: integer;
    lastWarnDate: integer;
    HistoryBackupInterval: longint;
    refreshAllAndIgnoreDate:boolean; //gibt an, dass alle Medien aktualisiert werden
                                     //sollen unabhängig vom letzten Aktualisierungdatum
    debugMode: boolean;

    errorMessageList:TErrorArray = nil;

    callbacks: TCallbackHolderClass = TCallbackHolder;

const localeLanguage: string = 'de'; //for http
const localeCountry: string = 'DE'; //for http and sorting libraries

  procedure initApplicationConfig;
  procedure finalizeApplicationConfig;

  procedure addErrorMessage(kind: TPendingExceptionKind; errorStr,errordetails, anonymouseDetails, libraryId, searchQuery:string;lib:TCustomAccountAccess=nil);
  procedure createErrorMessageStr(exception:exception; out kind: TPendingExceptionKind; out errorStr,errordetails, anonymousDetails:string;account:TCustomAccountAccess; additionalStacktrace: bbutils.TStringArray);

  procedure storeException(ex: exception; account:TCustomAccountAccess; libraryId, searchQuery: string; additionalStacktrace: bbutils.TStringArray = nil); //thread safe

  function sendFeedback(data: string): boolean;

  //get the values the tna should have not the one it actually has
  //function getTNAHint():string;
  function getTNAIconBaseFileName():string;

  procedure updateGlobalTimeCache;
  procedure updateGlobalAccountDates;
  procedure updateActiveInternetConfig;

  function DateToSimpleStr(const date: tdatetime):string;
  function DateToPrettyStr(const date: tdatetime):string;
  function DateToPrettyStrGrammarPast(const date: tdatetime): string;
  function DateToPrettyStrGrammarDateItself(const date: tdatetime): string;
  function DateToPrettyStrGrammarFuture(const date: tdatetime): string;

  function createVideLibriInternetAccess: TInternetAccess;

resourcestring
  rsRenewable = 'verlängerbar';
  rsLastRenewDate = 'Älteste angezeigte Daten sind %s';
  rsBookStatusNotLend = 'abgegeben';
  rsBookStatusNormalRenewable = 'normal/verlängerbar';
  rsBookStatusUnknown = 'unbekannt';
  rsBookStatusNonRenewable = 'nicht verlängerbar';
  rsBookStatusOrdered = 'vorgemerkt';
  rsBookStatusProvided = 'bereitgestellt'; // 'zur Abholung bereit'
  rsBookStatusReserved = 'vorgemerkt';
  rsNeverLend = 'nie';
  rsLendHistory = 'erledigt';
  rsUnknown = 'unbekannt';

  rsPatternMatchingFailedS = 'Die Webseite der Bibliothek hat sich auf unerwartete Weise geändert. Interner Name der Seite: %s';
  rsPatternMatchingFailedDebugAtS = 'Auf der Seite ist "%s" nicht aufzufinden.';
  rsPatternMatchingFailedDebugPreviousElementS = 'Direkt hinter "%s".';
  rsPatternMatchingFailedDebugLastMatchSS = 'Gefunden wurde zuletzt: "%s" bei "%s".';
  rsPatternMatchingFailedDebugAllMatched = '';
  rsErrorNoPassword = 'Es wurde kein Passwort für das Konto eingegeben';
implementation
uses libraryaccess,math,{$ifndef android}FileUtil,{$endif}bbdebugtools,androidutils ,
  {$IFDEF WIN32}
  windows,synapseinternetaccess,w32internetaccess
  {$ELSE}
  {$IFDEF ANDROID}
  okhttpinternetaccess
  {$ELSE}
  synapseinternetaccess
  {$ENDIF}
  {$ENDIF}
  ;

resourcestring
  rsParamAutostart = 'Gibt an, ob das Programm automatisch gestartet wurde.';
  rsParamAlwaysStart = 'Startet das Program auch, wenn es schon läuft.';
  rsParamMinimize = 'Gibt an, ob das Programm minimiert gestartet werden soll.';
  rsParamUpdateComplete = 'Das Programm wurde auf Version ($1) aktualisiert (ACHTUNG: veraltet)';
  rsParamDebugAddress = 'Wandelt in der Debugversion eine Adresse in eine Funktionszeile um';
  rsParamLog = 'Zeichnet alle Aktionen auf';
  rsParamHttpLogPath = 'Pfad wo alle heruntergeladenen Dateien gespeichert werden sollen';
  rsParamRefresh = 'Aktualisiert alle Medien';
  rsParamTemplate = 'Führt ein Template aus (benötigt Datei)';
  rsParamPattern = 'Datei für das Template von debug-single-template';
  rsParamConfig = 'Pfad für Benutzereinstellungen';
  rsParamDebug = 'Aktiviert einige debug-Funktionen';
  rsProgramPathNotFound = 'Programmpfad "%s" wurde nicht gefunden';
  rsConfigPathNotFound = 'Datenpfad "%s" wurde nicht gefunden';
  rsProgramPathNotFoundAndInvalid = 'Benutzerpfad "%s" wurde nicht gefunden und konnte nicht erzeugt werden';
  rsNeverDate = 'nie';
  rsDateGrammarRelativeDateFuture = '%s';
  rsDateGrammarAbsoluteDateFuture = 'zum %s';
  rsDateGrammarRelativeDateItself = '%s';
  rsDateGrammarAbsoluteDateItself = 'dem %s';
  rsDateGrammarRelativeDatePast = 'von %s';
  rsDateGrammarAbsoluteDatePast = 'vom %s';
  rsBeforeYesterday = 'vorgestern';
  rsYesterday = 'gestern';
  rsToday = 'heute';
  rsTomorrow = 'morgen';
  rsAfterTomorrow = 'übermorgen';
  rsErrorCheckInternet = 'Bitte überprüfen Sie Ihre Internetverbindung.';
  rsErrorHTTP5xxServerDisturbed = 'Eventuell hat der Webserver der Bibliothek eine Störung.';
  rsErrorHTTP404NotFound = 'Die Webseite der Bibliothek wurde nicht gefunden.';
  rsErrorHTTP403AccessDenied = 'Die Webseite der Bibliothek hat den Zugriff nicht erlaubt (eventuell falsches Passwort? oder eine abgeschaltete Webseite)';
  rsErrorBacktrace = 'Detaillierte Informationen über die entsprechende Quellcodestelle:';
  rsErrorBookListReaderFromWebpage = 'Die Bibliothek zeigt diese Nachricht auf der Katalogwebseite an: '+LineEnding;
  rsErrorLoginException = 'Die Kontonummer bzw. das Passwort wurden vom Bibliothekskatalog nicht akzeptiert. Der Bibliothekskatalog erläutert dazu: '+LineEnding;
  rsErrorBookListReaderInternal = 'Interner Fehler: '+LineEnding;


  procedure addErrorMessage(kind: TPendingExceptionKind; errorStr,errordetails, anonymouseDetails, libraryId, searchQuery:string;lib:TCustomAccountAccess=nil);
  var i:integer;
    currentDT: String;
  begin
    if lib <> nil then
      errordetails := StringReplace(errordetails, lib.passWord, '******', [rfReplaceAll]);
    currentDT := lineending + DateTimeToStr(now);
    errordetails += currentDT;
    anonymouseDetails += currentDT;
    for i:=0 to high(errorMessageList) do
      if errorMessageList[i].error=errorstr then begin
        SetLength(errorMessageList[i].details,length(errorMessageList[i].details)+1);
        errorMessageList[i].details[high(errorMessageList[i].details)].account:=lib;
        errorMessageList[i].details[high(errorMessageList[i].details)].details:=errordetails;
        errorMessageList[i].details[high(errorMessageList[i].details)].anonymouseDetails:=anonymouseDetails;
        errorMessageList[i].details[high(errorMessageList[i].details)].libraryId:=libraryId;
        errorMessageList[i].details[high(errorMessageList[i].details)].searchQuery:=searchQuery;
        exit;
      end;
    SetLength(errorMessageList,length(errorMessageList)+1);
    errorMessageList[high(errorMessageList)].error:=errorstr;
    setlength(errorMessageList[high(errorMessageList)].details,1);
    errorMessageList[high(errorMessageList)].kind:=kind;
    errorMessageList[high(errorMessageList)].details[0].account:=lib;
    errorMessageList[high(errorMessageList)].details[0].details:=errordetails;
    errorMessageList[high(errorMessageList)].details[0].anonymouseDetails:=anonymouseDetails;
    errorMessageList[high(errorMessageList)].details[0].libraryId:=libraryId;
    errorMessageList[high(errorMessageList)].details[0].searchQuery:=searchQuery;
  end;

  procedure createErrorMessageStr(exception: exception; out kind: TPendingExceptionKind; out errorStr, errordetails, anonymousDetails: string; account: TCustomAccountAccess; additionalStacktrace: TStringArray);
  var i:integer;
    moreLineBreak, commondetails: string;
  begin
    kind := ekUnknown;
    errordetails:='';
    anonymousDetails:='';
    commondetails := '';
    moreLineBreak := {$ifdef android}''{$else}LineEnding{$endif};
    if exception.InheritsFrom(EInternetException) then begin
      kind := ekInternet;
      errorstr:=exception.message+LineEnding;
      if EInternetException(exception).errorCode <= 0 then errorStr += moreLineBreak+rsErrorCheckInternet
      else case EInternetException(exception).errorCode of
        403: errorStr += moreLineBreak+rsErrorHTTP403AccessDenied;
        404: errorStr += moreLineBreak+rsErrorHTTP404NotFound;
        500..599: errorStr += moreLineBreak+rsErrorHTTP5xxServerDisturbed;
      end;
      commondetails:=EInternetException(exception).details;
    end else if exception.InheritsFrom(ELoginException) then begin
      kind := ekLogin;
      errorstr:=rsErrorLoginException + moreLineBreak+ trim(exception.message);
    end else if exception.InheritsFrom(EBookListReaderFromWebpage) then begin
      errorstr:=rsErrorBookListReaderFromWebpage + moreLineBreak+ trim(exception.message);
    end else if exception.InheritsFrom(EBookListReader) then begin
      errorstr:=rsErrorBookListReaderInternal + moreLineBreak+ trim(exception.message);
    end else if exception.InheritsFrom(ELibraryException) then begin
      errorstr:=#13#10+exception.message;
      commondetails:=ELibraryException(exception).details;
    end else if exception.InheritsFrom(EHTMLParseMatchingException) then begin
       errorstr:=exception.message;
       if exception.InheritsFrom(EVideLibriHTMLMatchingException) then begin
         errordetails := EVideLibriHTMLMatchingException(exception).partialMatches;
         anonymousDetails := EVideLibriHTMLMatchingException(exception).anonymousPartialMatches;
       end else errorStr += '|'+exception.className();
    end else begin
      errorstr:=//'Es ist folgender Fehler aufgetreten:      '#13#10+
           exception.className()+': '+ exception.message+'     ';
    end;

    commondetails:=commondetails+#13#10  +rsErrorBacktrace+ #13#10+ BackTraceStrFunc(ExceptAddr);
    for i:=0 to ExceptFrameCount-1 do
      commondetails:=commondetails+#13#10+BackTraceStrFunc(ExceptFrames[i]);

    commondetails:=commondetails+#13#10+'Referenceoffset: '+strFromPtr(@initApplicationConfig);

    for i := 0 to high(additionalStacktrace) do commondetails:=commondetails+#13#10 + additionalStacktrace[i];

    errordetails += #13#10 + commondetails;
    anonymousDetails += #13#10 + commondetails;
    if logging then log('createErrorMessageStr: Exception: '+errorstr+#13#10'      Details: '+errordetails);
  end;

(*  function getTNAHint(): string;
  begin
    {if nextLimit>=MaxInt then
      result:='VideLibri'#13#10'Keine bekannte Abgabefrist'
     else if nextNotExtendableLimit=nextLimit then
      result:='VideLibri'#13#10'Nächste bekannte Abgabefrist:'#13#10+DateToPrettyStr(nextNotExtendableLimit)
     else
      result:='VideLibri'#13#10'Nächste bekannte Abgabefrist:'#13#10+DateToPrettyStr(nextLimit)+'  (verlängerbar)';}
     result:='
  end;        *)


  procedure storeException(ex: exception; account:TCustomAccountAccess; libraryId, searchQuery: string; additionalStacktrace: bbutils.TStringArray);
  var  errorstr, errordetails, anonymouseDetails: string;
       kind: TPendingExceptionKind;
  begin
    createErrorMessageStr(ex,kind,errorstr,errordetails,anonymouseDetails, account, additionalStacktrace);
    system.EnterCriticalSection(exceptionStoring);
    try
      addErrorMessage(kind, errorstr,errordetails,anonymouseDetails, libraryId, searchQuery, account);
    finally
      system.LeaveCriticalSection(exceptionStoring);
    end;

  end;


  constructor EVideLibriHTMLMatchingException.create(amessage: string; asender: THtmlTemplateParser);
  var
    anonymizer: TPatternMatchExceptionAnonymizer;
  begin
    inherited create(amessage);
    if asender <> nil then begin
      partialMatches := asender.debugMatchings(80);
      anonymizer := TPatternMatchExceptionAnonymizer.Create;
      anonymousPartialMatches := asender.debugMatchings(80, @anonymizer.nodeToString);
      anonymizer.free;
    end;
  end;

  constructor EBookListReader.create;
  begin

  end;

  constructor EBookListReader.create(s: string; more_details: string);
  begin
    Message:=s;
    details:=more_details;
  end;

  type TInternetAccessReact = object
    procedure internetReact(sender: TInternetAccess; var {%H-}transfer: TTransfer; var reaction: TInternetAccessReaction);
   end;
  procedure TInternetAccessReact.internetReact(sender: TInternetAccess; var transfer: TTransfer; var reaction: TInternetAccessReaction);
  begin
    ignore(sender);
    if reaction = iarFollowRedirectGET then reaction := iarFollowRedirectKeepMethod;
  end;

  function sendFeedback(data: string): boolean;
  var internet: TInternetAccess;
      page:string;
      internetReact: TInternetAccessReact;
  begin
    internetReact := default(TInternetAccessReact);
    internet:=createVideLibriInternetAccess;
    internet.OnTransferReact:=@internetReact.internetReact;
    page:=internet.post('https','www.benibela.de','/autoFeedback.php',
                  'app='+internet.urlEncodeData('VideLibri')+
                  '&ver='+inttostr(versionNumber)+
                  '&data='+internet.urlEncodeData(data)+internet.urlEncodeData(#10'buildid: '+versionBuildId));
    result := trim(page) = 'PHPOK';
    internet.free;
  end;

  function getTNAIconBaseFileName(): string;
  begin
    if nextLimit<=redTime then
      result:='smallRed.ico'
     else if nextNotExtendableLimit=nextLimit then
      result:='smallYellow.ico'
     else
      result:='smallGreen.ico';
  end;

  procedure updateGlobalTimeCache;
  begin
    currentDate:=trunc(now);
    redTime:=currentDate + userConfig.ReadInteger('base','near-time',2);
  end;

  procedure updateGlobalAccountDates;
  var i,j:integer;
  begin
    //set global nextLmiit and nextNotExtandable
    //(search next one)
    nextLimit:=MaxInt-1;
    nextNotExtendableLimit:=MaxInt;

    lastcheck:=currentDate;
    for i:=0 to accounts.count-1 do begin
      for j:=0 to accounts[i].books.current.count-1 do
        with (accounts[i]).books.current[j] do
          lastcheck:=min(lastcheck,lastExistsDate);
      if ((accounts[i]).books.nextLimit>0) then
         nextLimit:=min(nextLimit,(accounts[i]).books.nextLimit);
      if ((accounts[i]).books.nextNotExtendableLimit>0) then
        nextNotExtendableLimit:=min(nextNotExtendableLimit,(accounts[i]).books.nextNotExtendableLimit);
    end;
    nextLimitStr:=DateToPrettyStr(nextLimit);
    if nextLimit<>nextNotExtendableLimit then
      nextLimitStr:=nextLimitStr+ ' (' + rsRenewable + ')';
    callbacks.statusChange(Format(rsLastRenewDate, [DateToPrettyStrGrammarPast(lastCheck)]));
  end;
  procedure updateActiveInternetConfig;
  var
    CAStore: String;
  begin
    {$IFDEF WIN32}
    defaultInternetAccessClass:=TW32InternetAccess;
    {$ELSE}
    {$IFDEF ANDROID}
    defaultInternetAccessClass:=TOKHTTPInternetAccess;
    {$ELSE}
    defaultInternetAccessClass:=TSynapseInternetAccess;
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF ANDROID}
    case userConfig.readInteger('access','internet-backend',0) of
      1: {$IFDEF WIN32} defaultInternetAccessClass:=TW32InternetAccess{$ENDIF};
      2: defaultInternetAccessClass:=TSynapseInternetAccess;
    end;
    {$ENDIF}



    //    defaultInternetConfiguration.userAgent:='Mozilla 3.0 (compatible; VideLibri ';//2:13/20
    //    defaultInternetConfiguration.userAgent:='Mozilla 3.0 (compatible; VideLibri '+IntToStr(versionNumber);//+' '+machineConfig.ReadString('debug','userAgentAdd','')+')';
    defaultInternetConfiguration.userAgent:='Mozilla 3.0 (compatible; VideLibri '+IntToStr(versionNumber)+' '+machineConfig.ReadString('debug','userAgentAdd','')+')';
    if machineConfig.ReadString('debug','userAgentOverride','') <> '' then
      defaultInternetConfiguration.userAgent:=machineConfig.ReadString('debug','userAgentOverride','');
    if userConfig.ReadString('debug','userAgentOverride','') <> '' then
      defaultInternetConfiguration.userAgent:=userConfig.ReadString('debug','userAgentOverride','');
    //defaultInternetConfiguration.connectionCheckPage:='www.duesseldorf.de';
    case userConfig.readInteger('access','internet-type',0) of
      0: begin
        defaultInternetConfiguration.tryDefaultConfig:=true;
        defaultInternetConfiguration.useProxy:=false;
      end;
      1: begin
           defaultInternetConfiguration.tryDefaultConfig:=false;
           defaultInternetConfiguration.useProxy:=false;
         end;
      2: begin
           defaultInternetConfiguration.tryDefaultConfig:=false;
           defaultInternetConfiguration.useProxy:=true;
         end;
    end;
    {$IFNDEF ANDROID}
    defaultInternetConfiguration.proxyHTTPName:=userConfig.ReadString('access','httpProxyName','');
    defaultInternetConfiguration.proxyHTTPPort:=userConfig.ReadString('access','httpProxyPort','8080');
    defaultInternetConfiguration.proxyHTTPSName:=userConfig.ReadString('access','httpsProxyName','');
    defaultInternetConfiguration.proxyHTTPSPort:=userConfig.ReadString('access','httpsProxyPort','8080');
    defaultInternetConfiguration.proxySOCKSName:=userConfig.ReadString('access','socksProxyName','');
    defaultInternetConfiguration.proxySOCKSPort:=userConfig.ReadString('access','socksProxyPort','1080');
    defaultInternetConfiguration.checkSSLCertificates:=userConfig.ReadBool('access', 'checkCertificates', true);
    if defaultInternetAccessClass = TSynapseInternetAccess then begin
      CAStore := userConfig.ReadString('access', 'CAPath', '');
      defaultInternetConfiguration.CAFile := '';
      defaultInternetConfiguration.CAPath := '';
      if CAstore <> '' then
        if DirectoryExists(CAstore) then defaultInternetConfiguration.CAPath := CAStore
        else if FileExists(CAStore) then defaultInternetConfiguration.CAFile := CAStore;
      defaultInternetConfiguration.searchCertificates;
    end;
    {$endif}
  end;
  type EInitializationError = class(Exception);

  procedure initApplicationConfig;
    procedure setVersionBuildId();
    begin

    end;
    procedure setVersionBuildId(const s: string);
    begin
      versionBuildId := s;
    end;

  var i:integer;
      {$IFDEF WIN32}window,proc:THANDLE;{$endif}

      commandLine:TCommandLineReader;
      //checkOne: boolean;
  begin
    {$IFDEF FPC_HAS_CPSTRING}
    //from lazarus
    SetMultiByteConversionCodePage(CP_UTF8);
    SetMultiByteRTLFileSystemCodePage(CP_UTF8);
    {$ENDIF}

    currentDate:=trunc(now);

//    if currentDate>39264 then
  //     raiseInitializationError('Dises Betaversion ist abgelaufen (seit 1. Juli 2007).  Die neueste Version sollte unter www.benibela.de zu bekommen sein.');

    appFullTitle:='VideLibri '+FloatToStr(versionNumber / 1000);

    //Kommandozeile lesen
    commandLine:=TCommandLineReader.create;
    commandLine.declareFlag('autostart', rsParamAutostart);
    commandLine.declareFlag('start-always', rsParamAlwaysStart);
    commandLine.declareFlag('minimize', rsParamMinimize);
    commandLine.declareInt('updated-to', rsParamUpdateComplete, 0);
    commandLine.declareInt('debug-addr-info', rsParamDebugAddress, 0);
    commandLine.declareFlag('log', rsParamLog, false);
    commandLine.declareString('http-log-path', rsParamHttpLogPath, '');
    commandLine.declareFlag('refreshAll', rsParamRefresh, false);
    commandLine.declareString('debug-html-template', rsParamTemplate, '');
    commandLine.declareString('on', rsParamPattern, '');
    commandLine.declareString('user-path', rsParamConfig, '');
    commandLine.declareFlag('debug', rsParamDebug);
    {$ifdef android}commandLine.parse('');{$endif}

    //Überprüft, ob das Programm schon gestart ist, und wenn ja, öffnet dieses
    {$IFDEF WIN32}
    SetLastError(0);
    startedMutex:=CreateMutex(nil,true,VIDELIBRI_MUTEX_NAME);
    if (not commandLine.readFlag('start-always')) and (GetLastError=ERROR_ALREADY_EXISTS) then begin
      window:=FindWindow(nil,pchar(appFullTitle));//FindWindow(nil,'VideLibri');
      if window<>0 then begin
        SetForegroundWindow(window); //important to allow the other instance to raise itself
        sendMessage(window, LM_SHOW_VIDELIBRI, 0,0);
        cancelStarting:=true;
        commandLine.free;
        exit;
      end;
    end;
    {$ENDIF}
  
    //Aktiviert das Logging
    logging:=commandLine.readFlag('log');
    if logging then log('Started with logging enabled, command line:'+ParamStr(0));

    defaultInternetConfiguration.logToPath:=commandLine.readString('http-log-path');
    if defaultInternetConfiguration.logToPath <>'' then begin
      ForceDirectories(defaultInternetConfiguration.logToPath);
      defaultInternetConfiguration.logToPath:=IncludeTrailingPathDelimiter(defaultInternetConfiguration.logToPath);
    end;
    if logging then log('Started with internet logging enabled');

    //Pfade auslesen und überprüfen
    {$ifdef android}
    programPath := '';
    assetPath := '';
    {$else}
    programPath:=ExtractFilePath(ParamStr(0));
    if not (programPath[length(programPath)] in ['/','\']) then programPath:=programPath+DirectorySeparator;
    assetPath:=programPath+'data'+DirectorySeparator;
    {$endif}

    if logging then log('programPath is '+programPath);
    if logging then log('dataPath is '+assetPath);

    {$ifndef android}
    if not DirectoryExists(programPath) then
      raise EInitializationError.create(Format(rsProgramPathNotFound, [programPath]));
    if not DirectoryExists(assetPath) then begin
      {$ifdef UNIX}
      if DirectoryExists('/usr/share/videlibri/data/') then
        assetPath:='/usr/share/videlibri/data/'
       else
      {$endif}
      raise EInitializationError.Create(Format(rsConfigPathNotFound, [assetPath]));
    end;
    if logging and (not FileExists(assetPath+'machine.config')) then
      log('machine.config will be created');
    {$endif}

    //Globale Einstellungen lesen
    machineConfig:=iniFileFromString(assetFileAsString('machine.config'));


    //Userpfad auslesen und überprüfen
    if commandLine.existsProperty('user-path') then
      userPath:=commandLine.readString('user-path')
     else
      userPath:=machineConfig.ReadString('paths','user',programPath+'config'+DirectorySeparator);
    if logging then log('plain user path: '+userPath);
    userPath:=StringReplace(userPath,'{$appdata}',getUserConfigPath,[rfReplaceAll,rfIgnoreCase]);
    if logging then log('replaced user path: '+userPath);
    if (copy(userpath,2,2)<>':\') and (copy(userpath,1,2)<>'\\') and (copy(userpath,1,1) <> '/') then
      userPath:=programPath+userpath;
    userPath:=IncludeTrailingPathDelimiter(userPath);
    if logging then log('finally user path: '+userPath);
    
    if not DirectoryExists(userPath) then begin
      try
        if logging then log('user path: '+userPath+' doesn''t exists');
        ForceDirectories(userPath);
        if logging then log('user path: '+userPath+' should be created');
        if not DirectoryExists(userPath) then
          raise EInitializationError.Create(Format(rsProgramPathNotFoundAndInvalid, [userPath]));
       except
         raise EInitializationError.Create(Format(rsProgramPathNotFoundAndInvalid, [userPath]));
       end;
    end;

    if logging and (not FileExists(userPath+'user.config')) then
      log('user.config will be created');

    //Userdaten lesen
    userConfig:=TSafeIniFile.Create(userPath + 'user.config');

    if userConfig.ReadInteger('updates','lastVersion',999999)<versionNumber then begin
      userConfig.writeInteger('updates','lastVersion',versionNumber);
      newVersionInstalled:=true;
    end;
    versionNumber:=userConfig.ReadInteger('updates','lastVersion',versionNumber);
    if logging then log('DATA-Version ist nun bekannt: '+inttostr(versionNumber));
    setVersionBuildId({$I videlibribuildid.inc});

    RefreshInterval:=userConfig.ReadInteger('access','refresh-interval',1);
    WarnInterval:=userConfig.ReadInteger('base','warn-interval',0);
    lastWarnDate:=userConfig.ReadInteger('base','last-warn-date',0);
    HistoryBackupInterval:=userConfig.ReadInteger('base','history-backup-interval',90);

    {$ifdef android}logging:=logging or userConfig.ReadBool('base','logging',false);{$endif};

    libraryManager:=TLibraryManager.create();
    libraryManager.init(userPath);
    if libraryManager.count=0 then
      raise EXCEPTION.Create('No templates in '+assetPath);

    accounts:=TAccountList.create(userPath+'account.list', libraryManager);
    accounts.load;

    nextLimitStr:=DateToPrettyStr(nextLimit);
    if nextLimit<>nextNotExtendableLimit then
      nextLimitStr:=nextLimitStr+' ('+rsRenewable+')';


    if commandLine.readInt('debug-addr-info')<>0 then begin
      cancelStarting:=true;
      raise EInitializationError.create(BackTraceStrFunc(UIntToPtr(commandLine.readInt('debug-addr-info'))));
    end;

    if commandLine.readInt('updated-to')<>0 then
      userConfig.WriteInteger('version','number',commandLine.readInt('updated-to'));

    updateGlobalTimeCache;

    if commandLine.readFlag('autostart') then begin
      startToTNA:=userConfig.ReadBool('autostart','minimized',true);
      if (userConfig.ReadInteger('autostart','type',1)=1) then begin
        cancelStarting:=true;
        for i:=0 to accounts.count-1 do
          if ((accounts[i]).enabled) and (((accounts[i]).lastCheckDate<=currentDate-refreshInterval) or
             ((accounts[i]).existsCertainBookToExtend) or
             (((accounts[i]).books.nextLimit<>0)and((accounts[i]).books.nextLimit<=redTime))) then begin
            cancelStarting:=false;
            break;
          end;
      end else cancelStarting:=false;
    end else begin
      cancelStarting:=false;
      //TODO: Check autostart registry value (for later starts)
      if (not userConfig.SectionExists('autostart')) or
         (userConfig.ReadInteger('autostart','type',1)<>2) then
           callbacks.updateAutostart(true,true);
    end;
    if not cancelStarting then begin
      debugMode := commandLine.readFlag('debug');
      refreshAllAndIgnoreDate:=commandline.readFlag('refreshAll');

      extendedhtmlparser.rsPatternMatchingFailedS := rsPatternMatchingFailedS;
      extendedhtmlparser.rsPatternMatchingFailedDebugAtS := rsPatternMatchingFailedDebugAtS;
      extendedhtmlparser.rsPatternMatchingFailedDebugPreviousElementS := rsPatternMatchingFailedDebugPreviousElementS;
      extendedhtmlparser.rsPatternMatchingFailedDebugLastMatchSS := rsPatternMatchingFailedDebugLastMatchSS;
      extendedhtmlparser.rsPatternMatchingFailedDebugAllMatched := rsPatternMatchingFailedDebugAllMatched;
      extendedhtmlparser.rsPatternMatchingFailedS := rsPatternMatchingFailedS;


      updateActiveInternetConfig;

      fillchar(updateThreadConfig,sizeof(updateThreadConfig),0);
      InitCriticalSection(updateThreadConfig.libraryAccessSection);
      InitCriticalSection(updateThreadConfig.threadManagementSection);
      InitCriticalSection(exceptionStoring);

      callbacks.postInitApplication();
    end;
    commandLine.free;
  end;
  
  procedure finalizeApplicationConfig;
  begin
    if logging then log('finalizeApplicationConfig started');
    if accounts<>nil then begin
      accounts.free;
      libraryManager.free;
      userConfig.free;
      machineConfig.free;
    end;

    if not cancelStarting then begin
      system.DoneCriticalsection(updateThreadConfig.libraryAccessSection);
      system.DoneCriticalsection(updateThreadConfig.threadManagementSection);
      system.DoneCriticalsection(exceptionStoring);
    end;

    {$IFDEF WIN32}
    if startedMutex<>0 then ReleaseMutex(startedMutex);
    if needApplicationRestart then
      WinExec(pchar(ParamStr(0)+' --start-always') ,SW_SHOWNORMAL);
    {$ENDIF}
    if logging then begin
      log('finalizeApplicationConfig ended'#13#10' => program will exit normally, after closing log');
      bbdebugtools.stoplogging();
    end;
    androidutils.uninit;
  end;

  function DateToSimpleStr(const date: tdatetime): string;
  begin
    result := sysutils.DateToStr(date);
  end;

  function DateToPrettyStr(const date: tdatetime):string;
  begin
    if date=-2 then result:=rsNeverDate
    else if date=0 then result:=rsUnknown
    else case trunc(date-currentDate) of
      -2: result:=rsBeforeYesterday;
      -1: result:=rsYesterday;
      0: result:=rsToday;
      1: result:=rsTomorrow;
      2: result:=rsAfterTomorrow;
      else result:=DateToSimpleStr(date);
    end;
  end;



  function DateToPrettyStrGrammarPast(const date: tdatetime): string;
  begin
    if date=-2 then result:=rsNeverDate
    else if date=0 then result:=rsUnknown
    else case trunc(date-currentDate) of
      -2..2: result := Format(rsDateGrammarRelativeDatePast, [DateToPrettyStr(date)]); //von
      else result:=Format(rsDateGrammarAbsoluteDatePast, [DateToSimpleStr(date)]);     //vom
    end;
  end;
  function DateToPrettyStrGrammarDateItself(const date: tdatetime): string;
  begin
    if date=-2 then result:=rsNeverDate
    else if date=0 then result:=rsUnknown
    else case trunc(date-currentDate) of
      -2..2: result:=Format(rsDateGrammarRelativeDateItself, [DateToPrettyStr(date)]); //""
      else   result:=Format(rsDateGrammarAbsoluteDateItself, [DateToSimpleStr(date)]); //dem
    end;
  end;
  function DateToPrettyStrGrammarFuture(const date: tdatetime): string;
  begin
    if date=-2 then result:=rsNeverDate
    else if date=0 then result:=rsUnknown
    else case trunc(date-currentDate) of
      -2..2: result:=Format(rsDateGrammarRelativeDateFuture, [DateToPrettyStr(date)]); //""
      else   result:=Format(rsDateGrammarAbsoluteDateFuture, [DateToSimpleStr(date)]); //zum
    end;
  end;

  function createVideLibriInternetAccess: TInternetAccess;
  begin
    result := defaultInternetAccessClass.create();
    result.additionalHeaders.add('Accept-Language: ' +localeLanguage+'-'+localeCountry+','+localeLanguage+';q=0.9,en;q=0.4,de;q=0.3'); //this is a race condition if the language changes elsewhere. however, currently there is no option to change the language
  end;



class procedure TCallbackHolder.updateAutostart(enabled, askBeforeChange: boolean); begin ignore(enabled); ignore(askBeforeChange); end;
class procedure TCallbackHolder.applicationUpdate(auto: boolean); begin ignore(auto); end;
class procedure TCallbackHolder.statusChange(const message: string); begin ignore(message); end;
class procedure TCallbackHolder.allThreadsDone; begin end;
class procedure TCallbackHolder.postInitApplication; begin end;


constructor TStringAsMemoryStream.create(const s: RawByteString);
begin
  inherited Create;
  SetSize(length(s));
  if Size > 0 then begin
    write(s[1], length(s));
    Position := 0;
  end;
end;


end.

