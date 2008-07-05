program bookWatch;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,menus,windows,Graphics
  { add your units here }, sysutils,bookWatchMain, libraryParser,
w32InternetAccess, wininet, options, newAccountWizard_u, errorDialog,
applicationConfig, statistik_u, diagram, tnaAccess, libraryAccess,
sendBackError, internetAccess, autoupdate, progressDialog, registrierung,
nagform, bbdebugtools,
bibtexexport, simplexmlparser, booklistreader, librarySearcher, bookListView,
bookSearchForm, librarySearcherAccess, extendedhtmlparser, TreeListView;

type

{ ttempclass }


ttempclass=class //used as simple event handler class
  extendWhenUpdate, updateOnlyWhenNecessary,ignoreConnectionErrors: boolean;
  procedure onMenuClick(sender: TObject; menuID: longint);
  procedure onDblClick(sender: TObject);
  procedure callUpdateLib(sender:TObject; var Done: Boolean);
end;

procedure ttempclass.onMenuClick(sender: TObject; menuID: longint);
var temp:boolean;
begin
  temp:=false;
  case menuID of
    MENU_ID_START_LCL: onDblClick(sender);
    MENU_ID_AUTO_UPDATE,MENU_ID_UPDATE,MENU_ID_EXTEND: begin
      extendWhenUpdate:=menuID=MENU_ID_EXTEND;
      updateOnlyWhenNecessary:=menuID=MENU_ID_AUTO_UPDATE;
      ignoreConnectionErrors:=updateOnlyWhenNecessary;
      if logging then log('ttempclass.onMenuClick update started');
      if lclstarted then callUpdateLib(nil,temp)
      else begin
        if logging then log('ttempclass.onMenuClick start lcl');
//        Application.Initialize;
  //      if logging then log('ttempclass.onMenuClick init ok');
        Application.ShowMainForm:=false;
        if logging then log('ttempclass.onMenuClick 2, now create form');
        Application.CreateForm(TmainForm, mainForm);
        if logging then log('ttempclass.onMenuClick created ok');
        Application.OnIdle:=@callUpdateLib;
        if logging then log('ttempclass.onMenuClick idle call ok');
        lclStartedTime:=GetTickCount;
        lclStarted:=true;
        Application.Run;
        tna.stopStandalone;
        exit;
      end;
    end;
    MENU_ID_CLOSE: begin
      if logging then log('ttempclass.onMenuClick stop started');
      postmessage(tna.messageWindow,WM_CLOSE,0,0);
      if (mainform<>nil)and lclStarted{ and mainForm.Visible }then begin
        if logging then log('lcl active: close');
        mainForm.Close;
        if logging then log('lcl active: closed, processmessages');
        Application.ProcessMessages;
      end;
      if logging then log('ttempclass.onMenuClick stop ended');
    end;
  end;
end;
procedure ttempclass.onDblClick(sender: TObject);
  function ForceForegroundWindow(hwnd: THandle): WordBool;
  const
    SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
    SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
  var
    ForegroundThreadID: DWORD;
    ThisThreadID: DWORD;
    timeout: DWORD;
  begin
    if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);

    if GetForegroundWindow = hwnd then Result := True
    else
    begin
      // Windows 98/2000 doesn't want to foreground a window when some other
      // window has keyboard focus

      if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
        ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
        ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
        (Win32MinorVersion > 0)))) then
      begin
        // Code from Karl E. Peterson, www.mvps.org/vb/sample.htm
        // Converted to Delphi by Ray Lischner
        // Published in The Delphi Magazine 55, page 16

        Result := False;
        ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
        ThisThreadID := GetWindowThreadPRocessId(hwnd, nil);
        if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
        begin
          BringWindowToTop(hwnd); // IE 5.5 related hack
          SetForegroundWindow(hwnd);
          AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
          Result := (GetForegroundWindow = hwnd);
        end;
        if not Result then
        begin
          // Code by Daniel P. Stasinski
          SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
          SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
            SPIF_SENDCHANGE);
          BringWindowToTop(hwnd); // IE 5.5 related hack
          SetForegroundWindow(hWnd);
          SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
        end;
      end
      else
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
      end;

      Result := (GetForegroundWindow = hwnd);
    end;
  end; { ForceForegroundWindow }
var //i:longint;
    wnd:THandle;
begin
  if mainform=nil then begin
    //Application.Initialize;
    Application.ShowMainForm:=false;
    Application.CreateForm(TmainForm, mainForm);
    mainForm.show;
    ForceForegroundWindow(mainform.Handle);

    lclStartedTime:=GetTickCount;
    lclStarted:=true;
    Application.Run;
    tna.stopStandalone;
  end else begin
    if not IsWindowEnabled(mainForm.Handle) then begin
      Application.BringToFront;
      if IsWindowEnabled(Screen.ActiveForm.Handle) then begin
        wnd:=Screen.ActiveForm.Handle;
        ForceForegroundWindow(mainForm.Handle);
        ForceForegroundWindow(wnd);
      end;
     end else begin
      mainform.WindowState:=wsNormal;
      mainform.hide;
      mainform.show;
      ForceForegroundWindow(mainform.Handle);
     end;
  end;
//
{    if not mainForm.Enabled then
    form
      for i:=0 to Application.ComponentCount;  }
//  end;
  {if (mainform<>nil) and (mainform.handle<>0) then
    DestroyWindow(mainform.handle);
  mainform:=nil;          }
end;

procedure ttempclass.callUpdateLib(sender: TObject; var Done: Boolean);
begin
  Application.onIdle:=nil;
  updateAccountBookData(nil,ignoreConnectionErrors,updateOnlyWhenNecessary,extendWhenUpdate);
  extendWhenUpdate:=false;
  done:=true;
end;

procedure showInTNA();
var //iconHandle:THandle;
    events:TTempclass;
    
    startApplication:boolean;
begin
  //item :=TMENUITEm.Create(popup); item.Caption:='123';
//  iconHandle:=LoadImage(0,'ico.ico',IMAGE_ICON,0,0,LR_LOADFROMFILE);//hinstance,MAKEINTRESOURCE(1031));
//  if iconhandle=0 then iconHandle:=loadicon(0,MAKEINTRESOURCE(32514));
  tna:=ttnaicon.create('VideLibri'{getTNAHint()},getTNAIconFileName());
  tna.addMenuItem (MENU_ID_START_LCL,'&Öffnen');
  tna.addMenuItem (MENU_ID_UPDATE,'Medien &aktualisieren');
  tna.addMenuItem (MENU_ID_EXTEND,'Medien &verlängern');
  tna.addMenuItem (MENU_ID_LIMIT_INFO,'   Nächste Abgabefrist: '+nextLimitStr);
  tna.addMenuItem (MENU_ID_CLOSE,'&Beenden');
  events:=ttempclass.Create;
  tna.onMenuClick:=@events.onMenuClick;
  tna.onDblClick:=@events.onDblClick;
  
  startApplication:=alertAboutBooksThatMustBeReturned;
  
  log('showInTNA 10');
  if startApplication then events.onDblClick(nil)
  else begin
    startDailyCheckDate;
    log('showInTNA 12a');
    //startCheckThread;
    log('showInTNA 12b');
    tna.runStandalone;
    log('showInTNA 12c');
  end;
  log('showInTNA 13');
  if tna<>nil then FreeAndNil(tna);
  log('showInTNA 14');
  events.free;
  log('showInTNA 15');
end;

{$R icons.res}

{$IFDEF WINDOWS}{$R manifest.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.Title:='VideLibri';
  application.Name:='VideLibri';
  initApplicationConfig;
  mainForm:=nil;
  if not cancelStarting then begin
    if startToTNA then begin
      showInTNA();
    end else begin
      Application.CreateForm(TmainForm, mainForm);
      lclStartedTime:=GetTickCount;
      lclStarted:=true;
     // Application.ShowMainForm:=true;
      //mainform.show;
      Application.Run;
    end;
  end;
  finalizeApplicationConfig;
{  if startToTNA then begin
    MessageBox(0,'finalappco ok ','',0);
  
    asm_exit(0);
  end;}
  end.

