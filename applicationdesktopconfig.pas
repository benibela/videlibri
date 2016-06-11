unit applicationdesktopconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LMessages, applicationconfig,LazFileUtils {$ifdef win32},registry{$endif};


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
  class procedure updateAutostart(enabled, askBeforeChange: boolean); override; static;
  class procedure applicationUpdate(auto: boolean); override; static;
  class procedure statusChange(const message: string); override; static;
  class procedure allThreadsDone(); override; static;
  class procedure postInitApplication(); override; static;
end;

  procedure showErrorMessages();

implementation

uses bookWatchMain,booklistreader,libraryAccess, bbutils,bbdebugtools,forms,LCLType,Dialogs, Controls, errorDialog, autoupdate, FileUtil, androidutils;

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
    sl_title, sl_message, sl_messagedetails: tstringlist;
    //met: TMethod;
begin
  if logging then log('showErrorMessages called: '+IntToStr(length(errorMessageList))) ;
  system.EnterCriticalSection(exceptionStoring);
  sl_title := TStringList.Create; sl_message := TStringList.Create; sl_messagedetails := TStringList.Create;
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
            mesDetails:=mesDetails+'Details für den Zugriff auf Konto '+details[j].account.prettyname+' bei '+details[j].libraryId+':'#13#10
           else
            mesDetails:=mesDetails+'Details für die Suche nach '+details[j].searchQuery+' bei '+details[j].libraryId+':'#13#10;
          mesDetails+=details[j].details+#13#10#13#10;
        end;
        oldErrorMessageString:=oldErrorMessageString+'---Fehler---'#13#10+mes+#13#10'Details:'#13#10+mesdetails;
        if accountException then  title:='Fehler beim Aktualisieren der Bücherdaten'
        else title:='Fehler';
        sl_title.Add(title);
        sl_message.add(mes);
        sl_messagedetails.add(mesDetails);
        //Application.MessageBox(pchar(error),pchar('Fehler bei Zugriff auf '+lib.prettyName),MB_APPLMODAL or MB_ICONERROR or MB_OK);
      end;
    end;
    setlength(errorMessageList,0);
  finally
    system.LeaveCriticalSection(exceptionStoring);
  end;
  for i:=0 to sl_title.Count-1 do begin
    if mainForm.Visible then
      TshowErrorForm.showError(sl_title[i],sl_message[i],sl_messagedetails[i],@mainForm.MenuItem16Click)
     else
      TshowErrorForm.showError(sl_title[i],sl_message[i],sl_messagedetails[i]);
  end;
  sl_title.free; sl_message.free; sl_messagedetails.free
end;

function confirm(s: string): boolean;
begin
  result := MessageDlg(s, mtConfirmation ,[mbYes,mbNo],0) = mrYes;
end;


procedure raiseInitializationError(s: string);
begin
  Application.MessageBox(pchar(s), 'Videlibri Fehler', MB_ICONERROR);
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
      'Comment=Bücherverwaltungsprogramm'+ LineEnding+
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
      else correctAutostart:=Application.MessageBox('Der Autostarteintrag ist ungültig'#13#10+
                      'Wenn er nicht geändert wird, können die Medien wahrscheinlich nicht automatisch verlängert werden.'#13#10+
                      'Soll er nun geändert werden?','VideLibri',MB_YESNO) = IDYES;


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

        if (updater.installerCmd<>'') and (updater.canRunInstaller) then begin
          mainForm.StatusBar1.Panels[0].Text:='Bitte warten, Update wird installiert...';
          updater.installUpdate();
        end else begin
          ShowMessage('Update kann nicht automatisch installiert werden.'#13#10'Das Update wurde in der Datei '+updater.downloadedFileName+' gespeichert, die gleich im Explorer geöffnet wird.');
          updater.openFileBrowser;
        end;

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
  except
    on e: exception do
      ShowMessage('Fehler: '+ e.Message);
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

