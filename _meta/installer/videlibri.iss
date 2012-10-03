[Setup]
;Change this in every version
AppVerName=VideLibri 1.25
AppVersion=1.25

;Don't change
AppName=VideLibri
AppMutex=VideLibriStartedMutex
AppPublisher=Benito van der Zander
AppPublisherURL=http://www.benibela.de

OutputBaseFilename=videlibri-setup

DefaultDirName={pf}\VideLibri
DefaultGroupName=VideLibri
UninstallDisplayIcon={app}\VideLibri.exe
SourceDir=..\..\

[Types]
Name: "full"; Description: "Normale Installation"
Name: "custom"; Description: "Benutzerdefinierte Installation"; Flags: iscustom

[Components]
Name: "main"; Description: "Programm"; Types: full custom; Flags: fixed
Name: "browser"; Description: "Integrierter Browser"; Types: full
Name: "help"; Description: "Hilfedatei"; Types: full
Name: "usbstick"; Description: "Zum Start von USB-Stick konfigurieren (beta)";

[Tasks]
Name: autostart; Description: "Das Programm soll automatisch gestartet werden (empfohlen)"; GroupDescription: "Autostart"; Components: main; Flags: checkedonce
Name: menuicon; Description: "Erstelle ein Icon im &Startmenü"; GroupDescription: "Icons"; Components: main;
Name: desktopicon; Description: "Erstelle ein &Desktopicon"; GroupDescription: "Icons"; Components: main
Name: desktopicon\common; Description: "Für alle Benutzer"; GroupDescription: "Icons"; Components: main; Flags: exclusive
Name: desktopicon\user; Description: "Nur für den aktuellen"; GroupDescription: "Icons"; Components: main; Flags: exclusive unchecked
;Name: quicklaunchicon; Description: "Erstelle ein Icon in der Schnellstart&leiste"; GroupDescription: "Icons"; Components: main; Flags: unchecked

[Files]
Source: "data\*"; DestDir: "{app}\data"; Excludes: "machine.config"; Components: main; Flags: recursesubdirs
Source: "_meta\installer\add\data\machine.config"; DestDir: "{app}\data"; Components: main and not usbstick; Flags: onlyifdoesntexist
Source: "_meta\installer\add_usbstick\data\machine.config"; DestDir: "{app}\data"; Components: usbstick; Flags: onlyifdoesntexist
Source: "videlibri.exe"; DestDir: "{app}"; Components: main
Source: "bbabout.dll"; DestDir: "{app}"; Components: main
Source: "data\videlibri.chm"; DestDir: "{app}"; Components: help
Source: "simplebrowser.exe"; DestDir: "{app}"; Components: browser

[Icons]
Name: "{group}\VideLibri"; Filename: "{app}\VideLibri.exe"; Tasks: menuicon
Name: "{group}\VideLibri Hilfe"; Filename: "{app}\VideLibri.chm"; Tasks: menuicon; Components: help
Name: "{userdesktop}\VideLibri"; Filename: "{app}\VideLibri.exe"; Tasks: desktopicon\common
Name: "{commondesktop}\VideLibri"; Filename: "{app}\VideLibri.exe"; Tasks: desktopicon\user

[Languages]
Name: "de"; MessagesFile: "compiler:Languages\German.isl"

[Registry]
Root: HKCU;Subkey: "Software\Microsoft\Windows\CurrentVersion\Run"; ValueType: string; ValueName: "VideLibriAutostart";ValueData: """{app}\VideLibri.exe"" /autostart";Flags:uninsdeletevalue; Tasks: autostart

[Run]
Filename: "{app}\VideLibri.exe"; Parameters: "/refreshAll"; Description: "VideLibri jetzt starten"; Flags: nowait postinstall



[Code]
var
  MyProgChecked: Boolean;
  MyProgCheckResult: Boolean;

function UninstallAccounts():boolean;
begin
//if not MyProgChecked then begin
//    MyProgCheckResult := MsgBox('Sollen auch alle eventuell vorhandenen Informationen über ausgeliehene Medien gelöscht werden?', mbConfirmation, MB_YESNO) = idYes;
//    MyProgChecked := True;
//  end;
  Result := MyProgCheckResult;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  case CurUninstallStep of
    usUninstall:
      begin
        if MsgBox('Sollen alle eventuell vorhandenen Informationen über ausgeliehene Medien gelöscht werden?', mbConfirmation, MB_YESNO) = idYes then begin
          deltree(ExpandConstant('{localappdata}')+'\VideLibri\config',true,true,true);
          deltree(ExpandConstant('{localappdata}')+'\VideLibri',true,true,true);
          removeDir(ExpandConstant('{localappdata}')+'\VideLibri');
        end;
        
        //MsgBox('CurUninstallStepChanged:' #13#13 'Uninstall is about to start.', mbInformation, MB_OK)
        // ...insert code to perform pre-uninstall tasks here...
      end;
  end;
end;
