unit applicationdesktopconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, applicationconfig,LazFileUtils, forms {$ifdef win32},registry{$endif};


var
  //cached
  colorLimited:tcolor;
  colorTimeNear:tcolor;
  colorOrdered, colorProvided:tcolor;
  colorOK:tcolor;
  colorOld:tcolor;

  function confirm(s: string): boolean;
  function alertAboutBooksThatMustBeReturned:boolean;

type TCallbackHolderDesktop = class (TCallbackHolder)
  class procedure updateAutostart(enabled, askBeforeChange: boolean); override;
  class procedure applicationUpdate(auto: boolean); override;
  class procedure statusChange(const message: string); override;
  class procedure allThreadsDone(); override;
  class procedure postInitApplication(); override;
end;

  var errorMessages: record
    common, details, anonymousDetails: string;
  end;
  procedure showErrorMessages();

resourcestring
  rsRenewOptions = 'immer, wenn möglich%salle, wenn nötig%seinzeln, wenn nötig%sniemals';
  rsRenewOptionsNoSingle = 'immer, wenn möglich%simmer, wenn nötig%sniemals';
  rsYes = 'ja';
  rsNo = 'nein';
  rsAlways = 'immer';
  rsNever = 'niemals';
  rsError = 'Fehler';
  rsBy = 'von';
  rsAnd = 'und';
  rsDetails = 'Details';


implementation

uses bookWatchMain,booklistreader,libraryAccess, bbutils,bbdebugtools,LCLType,Dialogs, Controls, errorDialog, autoupdate, FileUtil, androidutils;

resourcestring
  rsUpdateBeingInstalled = 'Bitte warten, Update wird installiert...';
  rsUpdateSaved = 'Update kann nicht automatisch installiert werden.%sDas Update wurde in der Datei %s gespeichert, die gleich im Explorer'
    +' geöffnet wird.';
  rsUpdateComplete = 'Update abgeschlossen';
  rsUpdateNone = 'Kein Update gefunden%sDie Version %s ist die aktuelle.';
  rsUpdateDownloading = 'Bitte warten, Update wird heruntergeladen...';
  rsUpdateFound = 'Es gibt ein Update auf die Version %s:%s%s%sSoll es jetzt heruntergeladen (und wenn möglich installiert) werden?';
  rsAutostartInvalidCorrect = 'Der Autostarteintrag ist ungültig%sWenn er nicht geändert wird, können die Medien wahrscheinlich nicht '
    +'automatisch verlängert werden.%sSoll er nun geändert werden?';
  rsAutostartCommentTitle = 'Bücherverwaltungsprogramm';
  rsAlertOverdue = 'Die folgenden Medien (%d) sind überfällig und sollten schon bis %s abgegeben worden sein:';
  rsAlertNonRenewable = 'Die folgenden Medien (%d) sind nicht verlängerbar und sollten bis %s abgegeben werden:';
  rsAlertRenewable = 'Die folgenden Medien (%d) sind bald bis %s fällig:';
  rsAlertWillRenew = 'VideLibri wird versuchen, diese Bücher automatisch zu verlängern';
  rsAlertOpenFull = 'Soll die Gesamt-Übersicht geöffnet werden?';
  rsErrorGeneral = 'Es ist folgender Fehler aufgetreten:';
  rsErrorOnAccounts = 'Beim Zugriff auf%s ist leider folgender Fehler aufgetreten: ';
  rsErrorDetailsAccount = '%sDetails für den Zugriff auf Konto %s bei %s:';
  rsErrorDetailsSearch = '%sDetails für die Suche nach %s bei %s:';
  rsErrorAccountTitle = 'Fehler beim Aktualisieren der Bücherdaten';
  rsErrorHeader = 'Fehlerinformationen über alle vorhin aufgetretenden Fehler';
  rsErrorNoAccountError = ' kein Konto (dieser Fehler macht keinen Sinn)';
  rsErrorSingleAccount = ' das Konto %s';
  rsErrorAccounts = ' die Konten %s und %s';


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
{$IFDEF WIN32}
const MB_SYSTEMMODAL = $1000;
{$ELSE}
const MB_SYSTEMMODAL=0;
{$ENDIF}
begin
  if logging then log('alertAboutBooksThatMustBeReturned started');
  result:=false;

  findBooksThatMustBeReturned(booksOverdue, booksSoonNotExtendable, booksSoon,
                              minDateOverdue, minDateSoonNotExtendable, minDateSoon);

  if ((lastWarnDate + WarnInterval <= currentDate) and (booksOverdue.Count + booksSoonNotExtendable.Count + booksSoon.Count > 0))
     or (booksOverdue.Count > 0) then begin
    alert:='';
    if booksOverdue.Count > 0 then begin
      alert+=Format(rsAlertOverdue,
                    [booksOverdue.Count, DateToPrettyStrGrammarFuture(minDateOverdue)]);
      alert+=LineEnding +strJoin(booksOverdue) + LineEnding;
    end;
    if booksSoonNotExtendable.Count > 0 then begin
      alert+=Format(rsAlertNonRenewable,
                   [booksSoonNotExtendable.Count, DateToPrettyStrGrammarFuture(minDateSoonNotExtendable)]);
      alert+=LineEnding +strJoin(booksSoonNotExtendable) + LineEnding;
    end;
    if booksSoon.Count > 0 then begin
      alert+=Format(rsAlertRenewable, [booksSoon.Count, DateToPrettyStrGrammarFuture(minDateSoon)]);
      alert+=LineEnding + strJoin(booksSoon) + LineEnding;
      alert+=rsAlertWillRenew + LineEnding+ LineEnding;
    end;
    alert+=rsAlertOpenFull;
    if Application.MessageBox(pchar(alert),'VideLibri',MB_YESNO or MB_ICONWARNING or MB_SYSTEMMODAL)=IDYES then
      result:=true;
  end;

  booksSoon.Free;
  booksSoonNotExtendable.free;
  booksOverdue.Free;

  lastWarnDate:=currentDate;
  userConfig.WriteInteger('base','last-warn-date',currentDate);

  if logging then log('alertAboutBooksThatMustBeReturned ended');
end;

procedure showErrorMessages();
var i,j:integer;
    title,mes,mesDetails,mesDetailsAnon: string;
    accountException: boolean;
    sl_title, sl_message, sl_messagedetails: tstringlist;
    //met: TMethod;
begin
  if logging then log('showErrorMessages called: '+IntToStr(length(errorMessageList))) ;
  system.EnterCriticalSection(exceptionStoring);
  sl_title := TStringList.Create; sl_message := TStringList.Create; sl_messagedetails := TStringList.Create;
  sl_messagedetails.OwnsObjects := false;
  try
    for i:=0 to high(errorMessageList) do begin
      if errorMessages.common='' then
        errorMessages.common := '==' + rsErrorHeader + '=='#13#10;
      with errorMessageList[i] do begin
        accountException:=true;
        for j:=0 to high(details) do
          if details[j].account=nil then accountException:=false;

        if accountException then begin
          mes:='';
          case length(details) of
            0: mes:=rsErrorNoAccountError;
            1: mes:=Format(rsErrorSingleAccount, [details[0].account.prettyName]);
            else begin
              for j:=0 to high(details)-2 do
                mes:=details[j].account.prettyName+', '+mes;
              mes:=Format(rsErrorAccounts, [mes + details[high(details)-1].account.prettyName, details[high(details)].account.prettyName]);
            end;
          end;
          mes:=Format(rsErrorOnAccounts, [mes]);
        end else mes := rsErrorGeneral;
        mes += LineEnding + LineEnding + error;

        mesDetails:='';
        for j:=0 to high(details) do begin
          if details[j].account<>nil then
            mesDetails:=Format(rsErrorDetailsAccount, [mesDetails, details[j].account.prettyname, details[j].libraryId]) + LineEnding
           else
            mesDetails:=Format(rsErrorDetailsSearch, [mesDetails, details[j].searchQuery, details[j].libraryId]) + LineEnding;
          mesDetailsAnon:=mesDetails;
          mesDetails += details[j].details+#13#10#13#10;
          mesDetailsAnon += details[j].anonymouseDetails;
        end;
        if accountException then  title:=rsErrorAccountTitle
        else title:=rsError;
        errorMessages.common += '---'+title+'---'#13#10+mes+#13#10+rsDetails+':'#13#10;
        errorMessages.details += mesDetails;
        errorMessages.anonymousDetails += mesDetailsAnon;
        sl_title.Add(title);
        sl_message.AddObject(mes, UIntToObj(ord(kind)));
        sl_messagedetails.Add(mesDetails);
        //Application.MessageBox(pchar(error),pchar('Fehler bei Zugriff auf '+lib.prettyName),MB_APPLMODAL or MB_ICONERROR or MB_OK);
      end;
    end;
    setlength(errorMessageList,0);
  finally
    system.LeaveCriticalSection(exceptionStoring);
  end;
  for i:=0 to sl_title.Count-1 do begin
    if mainForm <> nil then
      TshowErrorForm.showError(TExceptionKind(ObjToUInt(sl_message.Objects[i])), sl_title[i],sl_message[i],sl_messagedetails[i],@mainForm.MenuItem16Click,@mainform.ShowOptionsClick)
     else
      TshowErrorForm.showError(TExceptionKind(ObjToUInt(sl_message.Objects[i])),sl_title[i],sl_message[i],sl_messagedetails[i],nil,nil);
  end;
  sl_title.free; sl_message.free; sl_messagedetails.free
end;


function confirm(s: string): boolean;
begin
  result := MessageDlg(s, mtConfirmation ,[mbYes,mbNo],0) = mrYes;
end;


procedure raiseInitializationError(s: string);
begin
  Application.MessageBox(pchar(s), pchar('VideLibri ' + rsError), MB_ICONERROR);
  cancelStarting:=true;
  if logging then log('raiseInitializationError: '+s);
  raise exception.Create(s);
end;

class procedure TCallbackHolderDesktop.updateAutostart(enabled, askBeforeChange: boolean);
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
    {$IFNDEF ANDROID}
    autostartCommand:='[Desktop Entry]'+LineEnding+
      'Type=Application'+LineEnding+
      'Exec='+ParamStr(0)+' --autostart'+LineEnding+
      'Hidden=false'+LineEnding+
      'X-GNOME-Autostart-enabled=true'+LineEnding+
      'Name=videlibri'+LineEnding+
      'Comment='+ rsAutostartCommentTitle + LineEnding+
      'Icon='+assetPath+'yellow48.png';
    if not FileExistsUTF8(GetUserDir+'.config/autostart/videlibri.desktop') then
      autostartInvalid:=true
     else
      autostartInvalid:=strLoadFromFileUTF8(GetUserDir+'.config/autostart/videlibri.desktop')<>autostartCommand;
    {$ELSE}
    {$ENDIF}
    {$ENDIF}

    if autostartInvalid then
      if not askBeforeChange then correctAutostart:=true
      else correctAutostart:=Application.MessageBox(pchar(Format(rsAutostartInvalidCorrect, [#13#10, #13#10])), 'VideLibri', MB_YESNO) = IDYES;


    {$IFDEF WIN32}
    if correctAutostart then reg.WriteString('VideLibriAutostart','"'+ParamStr(0)+'" /autostart');
    reg.free;
    {$ENDIF}
    {$IFDEF UNIX}
    if correctAutostart then begin
      ForceDirectoriesUTF8(GetUserDir+'.config/autostart');
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

class procedure TCallbackHolderDesktop.applicationUpdate(auto: boolean);
var  updater: TAutoUpdater;
     temp:string;

begin
  {$ifdef android}exit;{$endif}
  if auto and ((userConfig.ReadInteger('updates','auto-check',1) = 0)
               or (currentDate-userConfig.ReadInteger('updates','lastcheck',0)<userConfig.ReadInteger('updates','interval',3))) then
    exit;
  if logging then log('applicationUpdate really started');
  //updater:=TAutoUpdater.create(versionNumber,programpath,'http://www.benibela.de/updates/videlibri/version.xml'
  //                                                      ,'http://www.benibela.de/updates/videlibri/changelog.xml');
  //updater:=TAutoUpdater.create(versionNumber,programpath,'http://videlibri.hg.sourceforge.net/hgweb/videlibri/videlibri/raw-file/tip/programs/internet/VideLibri/_meta/version/version.xml'
  //                                                      ,'http://videlibri.hg.sourceforge.net/hgweb/videlibri/videlibri/raw-file/tip/programs/internet/VideLibri/_meta/version/changelog.xml');
  try
    updater:=TAutoUpdater.create(versionNumber,programpath,'http://www.videlibri.de/updates/version.xml'
                                                          ,'http://www.videlibri.de/updates/changelog.xml');
    updater.language := 'de';

    if updater.existsUpdate then begin
      if { (not auto) or} (Application.MessageBox(pchar(Format(rsUpdateFound, [floattostr(updater.newestVersion/1000), #13#10#13#10,
        updater.listChanges, #13#10])), 'Videlibri Update', mb_yesno)=idyes) then begin

        assert(mainForm<>nil);
                                                //TODO:   update
                                                //Videlibri install win32 command: /SP- /silent /noicons "/dir=$OLDPATH;"
        Screen.cursor:=crHourglass;
        temp:=mainForm.StatusBar1.Panels[0].Text;

        mainForm.StatusBar1.Panels[0].Text:=rsUpdateDownloading;
        updater.downloadUpdate();

        if (updater.installerCmd<>'') and (updater.canRunInstaller) then begin
          mainForm.StatusBar1.Panels[0].Text:=rsUpdateBeingInstalled;
          updater.installUpdate();
        end else begin
          ShowMessage(Format(rsUpdateSaved, [LineEnding, updater.downloadedFileName]));
          updater.openFileBrowser;
        end;

        Screen.cursor:=crDefault;
        mainForm.StatusBar1.Panels[0].Text:=temp;

        if updater.needRestart then begin
          mainForm.close;
          //TODO: update else PostMessage(tna.messageWindow,WM_CLOSE,0,0);
        end else if not auto then
          Application.MessageBox(pchar(rsUpdateComplete), 'Videlibri Update', mb_ok);
      end;
    end else if not auto then
      Application.MessageBox(pchar(Format(rsUpdateNone, [LineEnding, floattostr(updater.newestVersion/1000)])), 'Videlibri Update', mb_ok);
    updater.free;
    userConfig.WriteInteger('updates','lastcheck',currentDate);
  except
    on e: exception do
      ShowMessage(rsError +': '+ e.Message);
  end;
  if logging then log('applicationUpdate ended');
end;

class procedure TCallbackHolderDesktop.statusChange(const message: string);
begin
  if (mainform<>nil) and (mainForm.Visible) then
    mainform.StatusBar1.Panels[0].text:=message;
end;

class procedure TCallbackHolderDesktop.allThreadsDone;
begin
  if accountsRefreshedDate = currentDate then begin
    if (mainForm <> nil) and (mainform.visible) then
       mainform.RefreshListView;
    applicationUpdate(true);
    sendMailReports();
  end;
  if mainForm <> nil then mainform.delayedCall.Enabled:=true; //show error messages
end;

class procedure TCallbackHolderDesktop.postInitApplication;
begin
  colorLimited:=userConfig.ReadInteger('appearance','limited',integer(clYellow));
  colorTimeNear:=userConfig.ReadInteger('appearance','timeNear',integer(clRed));
  colorOK:=userConfig.ReadInteger('appearance','default',integer((clGreen+clLime) div 2));
  colorOld:=userConfig.ReadInteger('appearance','history',integer(clSilver));
  colorProvided:=userConfig.ReadInteger('appearance','provided',integer(clFuchsia));
  colorOrdered:=userConfig.ReadInteger('appearance','ordered',integer(clAqua));
end;

initialization

callbacks := TCallbackHolderDesktop;


end.

