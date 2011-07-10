unit applicationconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics,forms,libraryparser,{$ifdef win32}registry,{$endif}inifiles,rcmdline,errordialog,autoupdate,progressDialog,extendedhtmlparser,
ExtCtrls ,Dialogs,LMessages
;

type TErrorArray=array of record
                     error: string;
                     details: array of record
                       account: TCustomAccountAccess;
                       details: string;
                     end;
                   end;
  
const VIDELIBRI_MUTEX_NAME='VideLibriStarted';
      LM_SHOW_VIDELIBRI = LM_USER + $4224;
  
var programPath,userPath,dataPath:string;
    machineConfig,userConfig: TIniFile;

    accountIDs: TStringList;
    libraryManager: TLibraryManager=nil;

    cancelStarting,startToTNA:boolean;
    accountsRefreshedToday: boolean=false; //set to true if the account information has been updated in the last 24h

    currentDate:longint;
    lastCheck: integer;
    nextLimit:longint=MaxInt-1;
    nextNotExtendableLimit:longint=MaxInt;
    nextLimitStr: string;

    appFullTitle:string='VideLibri';
    versionNumber:integer=1100;
    //=>versionNumber/1000
    newVersionInstalled: boolean=false;

    {$IFDEF WIN32}startedMutex:THandle=0;{$ENDIF}

    exceptionStoring: TRTLCriticalSection;
    
    needApplicationRestart: boolean; //Soll das Programm nach Beenden neugestartet werden

    //cached
    colorLimited:tcolor;
    colorTimeNear:tcolor;
    colorOK:tcolor;
    colorOld:tcolor;
    //TODO: customize colors in search panel colorSearchTextNotFound: tcolor=$6060FF;    //colorSearchTextFound: tcolor=clWindow;
    redTime: integer;
    RefreshInterval: integer;
    HistoryBackupInterval: longint;
    refreshAllAndIgnoreDate:boolean; //gibt an, dass alle Medien aktualisiert werden
                                     //sollen unabhängig vom letzten Aktualisierungdatum

    sharewareUser, sharewareCode: string;

  errorMessageList:TErrorArray = nil;
  //oldErrorMessageList:TErrorArray = nil;
  oldErrorMessageString:string;

  procedure applicationUpdate(auto:boolean);
  procedure updateAutostart(enabled, askBeforeChange:boolean);



  procedure initApplicationConfig;
  procedure finalizeApplicationConfig;
  procedure saveLibIDs;

  procedure showErrorMessages();
  procedure addErrorMessage(errorStr,errordetails:string;lib:TCustomAccountAccess=nil);
  procedure createErrorMessageStr(exception:exception; out errorStr,errordetails:string;account:TCustomAccountAccess=nil);
  procedure createAndAddException(exception:exception; account:TCustomAccountAccess=nil);

  procedure storeException(ex: exception; account:TCustomAccountAccess); //thread safe

  //get the values the tna should have not the one it actually has
  //function getTNAHint():string;
  function getTNAIconFileName():string;

  procedure updateGlobalAccountDates;
  procedure updateActiveInternetConfig;

  function DateToPrettyStr(const date: tdatetime):string;
  function DateToPrettyGrammarStr(preDate,preName:string;const date: tdatetime):string;

  procedure openInternetPage(url: string; myOptions: string='');
implementation
uses bookwatchmain,internetaccess,controls,libraryaccess,math,FileUtil,bbutils,bbdebugtools,LCLType,lclintf,LCLProc,
  {$IFDEF WIN32}
  windows,w32internetaccess
  {$ELSE}
  synapseinternetaccess
  {$ENDIF}
  ;
  procedure errorCallback(sender:TObject; var Done: Boolean);
  begin
    //messagebeep(0);
    Application.OnIdle:=nil;
    showErrorMessages();
    done:=true;
  end;
  procedure showErrorMessages();
  var i,j:integer;
      title,mes,mesDetails: string;
      accountException: boolean;
      //met: TMethod;
  begin
    if logging then log('showErrorMessages called: '+IntToStr(length(errorMessageList))) ;
    system.EnterCriticalSection(exceptionStoring);
    try
      for i:=0 to high(errorMessageList) do begin
        if oldErrorMessageString='' then
          oldErrorMessageString:=#13#10#13#10#13#10'====Fehlerinformationen über alle vorhin aufgetretenden Fehler===='#13#10;
        with errorMessageList[i] do begin
          accountException:=true;
          for j:=0 to high(details) do
            if details[j].account=nil then accountException:=false;
        
          if accountException then begin
            mes:=' ist leider folgender Fehler aufgetreten: '#13#10+error;
            case length(details) of
              0: mes:=' kein Konto (dieser Fehler macht keinen Sinn) '+mes;
              1: mes:=' das Konto '+details[0].account.prettyName+mes;
              else begin
                mes:=details[high(details)-1].account.prettyName+' und '+details[high(details)].account.prettyName+mes;
                for j:=0 to high(details)-2 do
                  mes:=details[j].account.prettyName+', '+mes;
                mes:=' die Konten '+mes;
              end;
            end;
            mes:='Beim Zugriff auf'+mes;
          end else begin
            mes:='Es ist folgender Fehler aufgetreten: '#13#10+error;
          end;

          mesDetails:='';
          for j:=0 to high(details) do begin
            if details[j].account<>nil then
              mesDetails:=mesDetails+'Details für den Zugriff auf Konto '+details[j].account.prettyname+':'#13#10;
            mesDetails+=details[j].details+#13#10#13#10;
          end;
          oldErrorMessageString:=oldErrorMessageString+'---Fehler---'#13#10+mes+#13#10'Details:'#13#10+mesdetails;
          if accountException then  title:='Fehler beim Aktualisieren der Bücherdaten'
          else title:='Fehler';
          if mainForm.Visible then
            TshowErrorForm.showError(title,mes,mesdetails,@mainForm.MenuItem16Click)
           else
            TshowErrorForm.showError(title,mes,mesdetails);
          //Application.MessageBox(pchar(error),pchar('Fehler bei Zugriff auf '+lib.prettyName),MB_APPLMODAL or MB_ICONERROR or MB_OK);
        end;
      end;
      setlength(errorMessageList,0);
    finally
      system.LeaveCriticalSection(exceptionStoring);
    end;
  end;

  procedure addErrorMessage(errorStr,errordetails:string;lib:TCustomAccountAccess=nil);
  var i:integer;
  begin
    for i:=0 to high(errorMessageList) do
      if errorMessageList[i].error=errorstr then begin
        SetLength(errorMessageList[i].details,length(errorMessageList[i].details)+1);
        errorMessageList[i].details[high(errorMessageList[i].details)].account:=lib;
        errorMessageList[i].details[high(errorMessageList[i].details)].details:=errordetails;
        exit;
      end;
    SetLength(errorMessageList,length(errorMessageList)+1);
    errorMessageList[high(errorMessageList)].error:=errorstr;
    setlength(errorMessageList[high(errorMessageList)].details,1);
    errorMessageList[high(errorMessageList)].details[0].account:=lib;
    errorMessageList[high(errorMessageList)].details[0].details:=errordetails;
  end;

  procedure createErrorMessageStr(exception: exception; out errorStr,
    errordetails: string; account: TCustomAccountAccess);
  var i:integer;
  begin
    if exception is EInternetException then begin
      errorstr:=exception.message+#13#10#13#10+'Bitte überprüfen Sie Ihre Internetverbindung.';
      errordetails:=EInternetException(exception).details;
     end else if exception is ELoginException then begin
      errorstr:=#13#10+exception.message;
      errordetails:='';
     end else if exception is ELibraryException then begin
      errorstr:=#13#10+exception.message;
      errordetails:=ELibraryException(exception).details;
     end else begin
      errorstr:=//'Es ist folgender Fehler aufgetreten:      '#13#10+
           exception.className()+': '+ exception.message+'     ';
      errordetails:='';
     end;
    errordetails:=errordetails+#13#10'Detaillierte Informationen über die entsprechende Quellcodestelle: (zur Angabe bei Supportanfragen) '#13#10+ BackTraceStrFunc(ExceptAddr);
    for i:=0 to ExceptFrameCount-1 do
      errordetails:=errordetails+#13#10+BackTraceStrFunc(ExceptFrames[i]);
    if logging then log('createErrorMessageStr: Exception: '+errorstr+#13#10'      Details: '+errordetails);
  end;

  procedure createAndAddException(exception: exception;
    account: TCustomAccountAccess);
  var errorStr,errordetails:string;
  begin
    createErrorMessageStr(exception,errorStr,errordetails,account);
    addErrorMessage(errorStr,errorDetails,account);
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

  procedure storeException(ex: exception; account:TCustomAccountAccess);
  var  errorstr, errordetails: string;
  begin
    createErrorMessageStr(ex,errorstr,errordetails,account);
    system.EnterCriticalSection(exceptionStoring);
    try
      addErrorMessage(errorstr,errordetails,account);
    finally
      system.LeaveCriticalSection(exceptionStoring);
    end;

  end;

  function getTNAIconFileName(): string;
  begin
    if nextLimit<=redTime then
      result:=dataPath+'smallRed.ico'
     else if nextNotExtendableLimit=nextLimit then
      result:=dataPath+'smallYellow.ico'
     else
      result:=dataPath+'smallGreen.ico';
  end;

  procedure updateGlobalAccountDates;
  var i,j:integer;
  begin
    //set global nextLmiit and nextNotExtandable
    //(search next one)
    nextLimit:=MaxInt-1;
    nextNotExtendableLimit:=MaxInt;

    lastcheck:=currentDate;
    for i:=0 to accountIDs.count-1 do begin
      for j:=0 to TCustomAccountAccess(accountIDs.objects[i]).books.current.count-1 do
        with TCustomAccountAccess(accountIDs.objects[i]).books.current[j] do
          lastcheck:=min(lastcheck,lastExistsDate);
      if (TCustomAccountAccess(accountIDs.objects[i]).books.nextLimit>0) then
         nextLimit:=min(nextLimit,TCustomAccountAccess(accountIDs.objects[i]).books.nextLimit);
      if (TCustomAccountAccess(accountIDs.objects[i]).books.nextNotExtendableLimit>0) then
        nextNotExtendableLimit:=min(nextNotExtendableLimit,TCustomAccountAccess(accountIDs.objects[i]).books.nextNotExtendableLimit);
    end;
    nextLimitStr:=DateToPrettyStr(nextLimit);
    if nextLimit<>nextNotExtendableLimit then
      nextLimitStr:=nextLimitStr+' (verlängerbar)';
    if (mainform<>nil) and mainForm.Visible then
      mainform.StatusBar1.Panels[0].text:='Älteste angezeigte Daten sind '+dateToPrettyGrammarStr('vom ','von ',lastCheck)
  end;
  procedure updateActiveInternetConfig;
  begin
    //    defaultInternetConfiguration.userAgent:='Mozilla 3.0 (compatible; VideLibri ';//2:13/20
    //    defaultInternetConfiguration.userAgent:='Mozilla 3.0 (compatible; VideLibri '+IntToStr(versionNumber);//+' '+machineConfig.ReadString('debug','userAgentAdd','')+')';
    defaultInternetConfiguration.userAgent:='Mozilla 3.0 (compatible; VideLibri '+IntToStr(versionNumber)+' '+machineConfig.ReadString('debug','userAgentAdd','')+')';
    if machineConfig.ReadString('debug','userAgentOverride','') <> '' then
      defaultInternetConfiguration.userAgent:=machineConfig.ReadString('debug','userAgentOverride','');
    defaultInternetConfiguration.connectionCheckPage:='www.duesseldorf.de';
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
    defaultInternetConfiguration.proxyHTTPName:=userConfig.ReadString('access','httpProxyName','');
    defaultInternetConfiguration.proxyHTTPPort:=userConfig.ReadString('access','httpProxyPort','');
    defaultInternetConfiguration.proxyHTTPSName:=userConfig.ReadString('access','httpsProxyName','');
    defaultInternetConfiguration.proxyHTTPSPort:=userConfig.ReadString('access','httpsProxyPort','');
  end;

  procedure applicationUpdate(auto:boolean);
  var  updater: TAutoUpdater;
       temp:string;

  begin
    if auto and ((userConfig.ReadInteger('updates','auto-check',1) = 0)
                 or (currentDate-userConfig.ReadInteger('updates','lastcheck',0)<userConfig.ReadInteger('updates','interval',3))) then
      exit;
    if logging then log('applicationUpdate really started');
    //updater:=TAutoUpdater.create(versionNumber,programpath,'http://www.benibela.de/updates/videlibri/version.xml'
    //                                                      ,'http://www.benibela.de/updates/videlibri/changelog.xml');
    updater:=TAutoUpdater.create(versionNumber,programpath,'http://videlibri.hg.sourceforge.net/hgweb/videlibri/videlibri/raw-file/tip/programs/internet/VideLibri/_meta/version/version.xml'
                                                          ,'http://videlibri.hg.sourceforge.net/hgweb/videlibri/videlibri/raw-file/tip/programs/internet/VideLibri/_meta/version/changelog.xml');

    if updater.existsUpdate then begin
      if { (not auto) or} (Application.MessageBox(pchar('Es gibt ein Update auf die Version '+floattostr(updater.newestVersion/1000)+':'#13#10#13#10+
                                              updater.listChanges+#13#10+
                                              'Soll es jetzt heruntergeladen (und wenn möglich installiert) werden?'),'Videlibri Update', mb_yesno)=idyes) then begin

        assert(mainForm<>nil);
                                                //TODO:   update
                                                //Videlibri install win32 command: /SP- /silent /noicons "/dir=$OLDPATH;"
        Screen.cursor:=crHourglass;
        temp:=mainForm.StatusBar1.Panels[0].Text;

        mainForm.StatusBar1.Panels[0].Text:='Bitte warten, Update wird heruntergeladen...';
        updater.downloadUpdate();

        if (updater.installerCmd<>'') and updater.hasDirectoryWriteAccess then begin
          mainForm.StatusBar1.Panels[0].Text:='Bitte warten, Update wird installiert...';
          updater.installUpdate();
        end else ShowMessage('Update kann nicht automatisch installiert werden.'#13#10'Das Update wurde in der Datei '+updater.downloadedFileName+' gespeichert');

        Screen.cursor:=crDefault;
        mainForm.StatusBar1.Panels[0].Text:=temp;

        if updater.needRestart then begin
          mainForm.close;
          //TODO: update else PostMessage(tna.messageWindow,WM_CLOSE,0,0);
        end else if not auto then
          Application.MessageBox('Update abgeschlossen','Videlibri Update', mb_ok);
      end;
    end else if not auto then
      Application.MessageBox(pchar('Kein Update gefunden'#13#10'Die Version '+floattostr(updater.newestVersion/1000)+' ist die aktuelle.'),'Videlibri Update', mb_ok);
    updater.free;
    userConfig.WriteInteger('updates','lastcheck',currentDate);
    if logging then log('applicationUpdate ended');
  end;

  procedure updateAutostart(enabled, askBeforeChange: boolean);
  var {$IFDEF WIN32}reg:TRegistry;{$ENDIF}
      autostartInvalid, correctAutostart: boolean;
      autostartCommand:string;
  begin
    if enabled then begin
      autostartInvalid:=false;
      correctAutostart:=false;
      {$IFDEF WIN32}
      reg:=TRegistry.create;
      reg.RootKey:=HKEY_CURRENT_USER;
      reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run',true);
      //MessageBox(0,pchar(ParamStr(0)),'',0);
      autostartCommand:=lowercase('"'+ParamStr(0)+'" /autostart');
      autostartInvalid:=lowercase(reg.ReadString('VideLibriAutostart')) <> autostartCommand;
      {$ENDIF}
      {$IFDEF UNIX}
      autostartCommand:='[Desktop Entry]'+LineEnding+
        'Type=Application'+LineEnding+
        'Exec='+ParamStrUTF8(0)+' --autostart'+LineEnding+
        'Hidden=false'+LineEnding+
        'X-GNOME-Autostart-enabled=true'+LineEnding+
        'Name=videlibri'+LineEnding+
        'Comment=Bücherverwaltungsprogramm'+ LineEnding+
        'Icon='+dataPath+'yellow48.png';
      if not FileExistsUTF8(GetUserDir+'.config/autostart/videlibri.desktop') then
        autostartInvalid:=true
       else
        autostartInvalid:=strLoadFromFileUTF8(GetUserDir+'.config/autostart/videlibri.desktop')<>autostartCommand;
      {$ENDIF}

      if autostartInvalid then
        if not askBeforeChange then correctAutostart:=true
        else correctAutostart:=Application.MessageBox('Der Autostarteintrag ist ungültig'#13#10+
                        'Wenn er nicht geändert wird, können die Medien wahrscheinlich nicht automatisch verlängert werden.'#13#10+
                        'Soll er nun geändert werden?','VideLibri',MB_YESNO) = IDYES;


      {$IFDEF WIN32}
      if correctAutostart then reg.WriteString('VideLibriAutostart','"'+ParamStr(0)+'" /autostart');
      reg.free;
      {$ENDIF}
      {$IFDEF UNIX}
      if correctAutostart then begin
        strSaveToFileUTF8(GetUserDir+'.config/autostart/videlibri.desktop',autostartCommand);
      end;
      {$ENDIF}
    end else begin
      //ignore ask here
      {$IFDEF WIN32}
      reg:=TRegistry.create;
      reg.RootKey:=HKEY_CURRENT_USER;
      reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run',true);
      reg.DeleteValue('VideLibriAutostart');
      reg.free;
      {$ENDIF}
      {$IFDEF UNIX}
      DeleteFileUTF8(GetUserDir+'.config/autostart/videlibri.desktop');
      {$ENDIF}
    end;
  end;

  //normal exception handling doesn't seem to work properly when lcl is not loaded
  //(update: since dec 2009/linux transition, lcl is always loaded)
  procedure raiseInitializationError(s: string);
  begin
    Application.MessageBox(pchar(s), 'Videlibri Fehler', MB_ICONERROR);
    cancelStarting:=true;
    if logging then log('raiseInitializationError: '+s);
    raise exception.Create(s);
  end;

  procedure initApplicationConfig;
  var i:integer;
      window,proc:THANDLE;

      commandLine:TCommandLineReader;
      //checkOne: boolean;
  begin
    currentDate:=trunc(now);

//    if currentDate>39264 then
  //     raiseInitializationError('Dises Betaversion ist abgelaufen (seit 1. Juli 2007).  Die neueste Version sollte unter www.benibela.de zu bekommen sein.');

    appFullTitle:='VideLibri '+FloatToStr(versionNumber / 1000);

    //Kommandozeile lesen
    commandLine:=TCommandLineReader.create;
    commandLine.declareFlag('autostart','Gibt an, ob das Programm automatisch gestartet wurde.');
    commandLine.declareFlag('start-always','Startet das Program auch, wenn es schon läuft.');
    commandLine.declareFlag('minimize','Gibt an, ob das Programm minimiert gestartet werden soll.');
    commandLine.declareInt('updated-to','Das Programm wurde auf Version ($1) aktualisiert (ACHTUNG: veraltet)',0);
    commandLine.declareInt('debug-addr-info','Wandelt in der Debugversion eine Adresse in eine Funktionszeile um',0);
    commandLine.declareFlag('log','Zeichnet alle Aktionen auf',false);
    commandLine.declareString               ('http-log-path','Pfad wo alle heruntergeladenen Dateien gespeichert werden sollen','');
    commandLine.declareFlag('refreshAll','Aktualisiert alle Medien',false);
    commandLine.declareString('debug-html-template','Führt ein Template aus (benötigt Datei)','');
    commandLine.declareString('on','Datei für das Template von debug-single-template','');

    {if commandLine.readString('debug-html-template')<>'' then begin
      checkHTMLTemplate(commandLine.readString('debug-html-template'),commandLine.readString('on'));
      cancelStarting:=true;
      exit;
    end; ??}

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
    if defaultInternetConfiguration.logToPath <>'' then
      defaultInternetConfiguration.logToPath:=IncludeTrailingPathDelimiter(defaultInternetConfiguration.logToPath);
    if logging then log('Started with internet logging enabled');

    //Überprüft die Farbeinstellung des Monitors
    if ScreenInfo.ColorDepth=8 then
      Application.MessageBox(pchar('VideLibri funktioniert im 256-Farbenmodus nur unvollständig.'#13#10+
                   'Am besten ändern Sie Ihre Monitoreinstellungen.'),'Videlibri',MB_ICONWARNING);

    //Pfade auslesen und überprüfen
    programPath:=ExtractFilePath(ParamStr(0));
    if not (programPath[length(programPath)] in ['/','\']) then programPath:=programPath+DirectorySeparator;
    dataPath:=programPath+'data'+DirectorySeparator;

    if logging then log('programPath is '+programPath);
    if logging then log('dataPath is '+dataPath);

    if not DirectoryExists(programPath) then
      raiseInitializationError('Programmpfad "'+programPath+'" wurde nicht gefunden');
    if not DirectoryExists(dataPath) then begin
      {$ifdef UNIX}
      if DirectoryExists('/usr/share/videlibri/data/') then
        dataPath:='/usr/share/videlibri/data/'
       else
      {$endif}
      raiseInitializationError('Datenpfad "'+dataPath+'" wurde nicht gefunden');
    end;
    if logging and (not FileExists(datapath+'machine.config')) then
      log('machine.config will be created');

    //Globale Einstellungen lesen
    machineConfig:=TIniFile.Create(datapath+'machine.config');
    if machineConfig.ReadInteger('version','number',versionNumber)<versionNumber then begin
      machineConfig.writeInteger('version','number',versionNumber);
      newVersionInstalled:=true;
    end;
    versionNumber:=machineConfig.ReadInteger('version','number',versionNumber);
    
    if logging then log('DATA-Version ist nun bekannt: '+inttostr(versionNumber));

    //Userpfad auslesen und überprüfen
    userPath:=machineConfig.ReadString('paths','user',programPath+'config'+DirectorySeparator);
    if logging then log('plain user path: '+userPath);
    userPath:=StringReplace(userPath,'{$appdata}',GetAppConfigDir(false),[rfReplaceAll,rfIgnoreCase]);
    if logging then log('replaced user path: '+userPath);
    if (copy(userpath,2,2)<>':\') and (copy(userpath,1,2)<>'\\') and (copy(userpath,1,1) <> '/') then
      userPath:=programPath+userpath;
    userPath:=IncludeTrailingPathDelimiter(userPath);
    if logging then log('finally user path: '+userPath);
    
    if not DirectoryExists(userPath) then begin
      try
        if logging then log('user path: '+userPath+' doesn''t exists');
        ForceDirectory(userPath);
        if logging then log('user path: '+userPath+' should be created');
        if not DirectoryExists(userPath) then
          raiseInitializationError('Benutzerpfad "'+userPath+'" wurde nicht gefunden und konnte nicht erzeugt werden');
       except
         raiseInitializationError('Benutzerpfad "'+userPath+'" wurde nicht gefunden und konnte nicht erzeugt werden');
       end;
    end;

    if logging and (not FileExists(userPath+'user.config')) then
      log('user.config will be created');

    //Userdaten lesen
    userConfig:=TIniFile.Create(userPath+'user.config');
    RefreshInterval:=userConfig.ReadInteger('access','refresh-interval',2);
    HistoryBackupInterval:=userConfig.ReadInteger('base','history-backup-interval',30);

    libraryManager:=TLibraryManager.create();
    libraryManager.init(userPath);
    if libraryManager.enumeratePrettyLongNames()='' then
      raise EXCEPTION.Create('Keine Büchereitemplates im Verzeichnis '+dataPath+' vorhanden');

    accountIDs:=TStringList.create;
    if FileExists(userPath+'account.list') then
      accountIDs.LoadFromFile(userPath+'account.list');
    for i:=0 to accountIDs.count-1 do
      accountIDs.Objects[i]:=libraryManager.getAccount(accountIDs[i]);

    nextLimitStr:=DateToPrettyStr(nextLimit);
    if nextLimit<>nextNotExtendableLimit then
      nextLimitStr:=nextLimitStr+' (verlängerbar)';


    if commandLine.readInt('debug-addr-info')<>0 then begin
      cancelStarting:=true;
      Application.MessageBox(pchar(string(BackTraceStrFunc(pointer(commandLine.readInt('debug-addr-info'))))),'Point',0);
      raise exception.create(BackTraceStrFunc(pointer(commandLine.readInt('debug-addr-info'))));
    end;

    if commandLine.readInt('updated-to')<>0 then
      userConfig.WriteInteger('version','number',commandLine.readInt('updated-to'));

//    sharewareUser:=userConfig.ReadString('registration','user','');
//    sharewareCode:=userConfig.ReadString('registration','code','');
    sharewareUser:='SourceForge';
    sharewareCode:='A8I8D DF3D4 EJ2N9 76IJK';



    redTime:=trunc(now)+userConfig.ReadInteger('base','near-time',2);

    if commandLine.readFlag('autostart') then begin
      startToTNA:=userConfig.ReadBool('autostart','minimized',true);
      if (userConfig.ReadInteger('autostart','type',1)=1) then begin
        cancelStarting:=true;
        for i:=0 to accountIDs.count-1 do
          if (TCustomAccountAccess(accountIDs.Objects[i]).enabled) and ((TCustomAccountAccess(accountIDs.Objects[i]).lastCheckDate<=currentDate-refreshInterval) or
             (TCustomAccountAccess(accountIDs.Objects[i]).existsCertainBookToExtend) or
             ((TCustomAccountAccess(accountIDs.Objects[i]).books.nextLimit<>0)and(TCustomAccountAccess(accountIDs.Objects[i]).books.nextLimit<=redTime))) then begin
            cancelStarting:=false;
            break;
          end;
      end else cancelStarting:=false;
    end else begin
      cancelStarting:=false;
      //TODO: Check autostart registry value (for later starts)
      if (not userConfig.SectionExists('autostart')) or
         (userConfig.ReadInteger('autostart','type',1)<>2) then
           updateAutostart(true,true);
    end;
    if not cancelStarting then begin
      refreshAllAndIgnoreDate:=commandline.readFlag('refreshAll');
    
      colorLimited:=userConfig.ReadInteger('appearance','limited',integer(clYellow));
      colorTimeNear:=userConfig.ReadInteger('appearance','timeNear',integer(clRed));
      colorOK:=userConfig.ReadInteger('appearance','default',integer((clGreen+clLime) div 2));
      colorOld:=userConfig.ReadInteger('appearance','history',integer(clSilver));

      updateActiveInternetConfig;
      {$IFDEF WIN32}
      defaultInternetAccessClass:=TW32InternetAccess;
      {$ELSE}
      defaultInternetAccessClass:=TSynapseInternetAccess;
      {$ENDIF}

      fillchar(updateThreadConfig,sizeof(updateThreadConfig),0);
      InitCriticalSection(updateThreadConfig.libraryAccessSection);
      InitCriticalSection(updateThreadConfig.threadManagementSection);
      InitCriticalSection(exceptionStoring);
    end;
    commandLine.free;
  end;
  
  procedure finalizeApplicationConfig;
  var i:integer;
  begin
    if logging then log('finalizeApplicationConfig started');
    if accountIDs<>nil then begin
      for i:=0 to accountIDs.count-1 do
        try
          accountIDs.Objects[i].free;
        except
          ;
        end;
      accountIDs.free;
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
      if logFileCreated then begin
        close(logFile);
        system.DoneCriticalsection(logFileSection);
      end;
    end;
  end;
  
  procedure saveLibIDs;
  begin
    accountIDs.SaveToFile(userPath+'account.list');
  end;

  function DateToPrettyStr(const date: tdatetime):string;
  begin
    if date=0 then result:='unbekannt'
    else case trunc(date-currentDate) of
      -2: result:='vorgestern';
      -1: result:='gestern';
      0: result:='heute';
      1: result:='morgen';
      2: result:='übermorgen';
      else result:=DateToStr(date);
    end;
  end;

  function DateToPrettyGrammarStr(preDate,preName: string; const date: tdatetime
    ): string;
  begin
    if date=0 then result:='unbekannt'
    else case trunc(date-currentDate) of
      -2: result:=preName+'vorgestern';
      -1: result:=preName+'gestern';
      0: result:=preName+'heute';
      1: result:=preName+'morgen';
      2: result:=preName+'übermorgen';
      else result:=preDate+DateToStr(date);
    end;
  end;

  procedure openInternetPage(url: string; myOptions: string);
  begin
    (*if (userConfig.ReadInteger('access','homepage-type',1)=0) and (FileExists(programPath+'simpleBrowser.exe')) then
       //TODO: WinExec(pchar(programPath+'simpleBrowser /site="'+url+'" '+myOptions),SW_SHOWNORMAL)
     else if Application.MainForm<>nil then
       //TODO: shellexecute(Application.MainForm.Handle,'open',pchar('"'+url+'"'),'','',SW_SHOWNORMAL);*)
    OpenURL(url)
   end;

end.
