unit tnaAccess;

{$mode objfpc}{$H+}
{$ifndef win32} {$error Need Windows} {$endif}
interface

uses
  Classes, SysUtils,windows,messages,menus;

type TMenuItemEvent=procedure (sender: TObject; menuID: longint)of object;

{ TTNAIcon }

TTNAIcon=class
protected
  window: THandle;
  iconID: integer;
  popupHandle: THandle;
  freePopup,freeIcon: boolean;
  stopStandaloneNow: boolean;
  
  currentIconHandle:THandle;
  currenttitle:string;
  procedure doClick();
  procedure doDblClick();
  procedure doShowPopup();
public
  constructor create(title: string; iconHandle: THandle; _popupHandle: THandle=0);
  constructor create(title: string; iconFileName: string);
  destructor destroy;override;
  
  procedure change(iconHandle: THandle;title:string);
  procedure changeIcon(iconHandle: THandle);
  procedure changeIcon(iconFileName: string);
  procedure changetitle(title:string);

  procedure addMenuItem(id:longint;caption: string);

  procedure runStandalone;
  procedure stopStandalone;

  onClick,onDblClick: TNotifyEvent;
  onMenuClick: TMenuItemEvent;

  class function getClassName():string;

  property messageWindow: thandle read window;
 // property id: thandle read iconID;
end;
const wm_taskbarmessage=wm_user+1;


implementation
{$IFDEF log}uses applicationconfig;{$ENDIF}
const NIF_MESSAGE=00000001;
const NIF_ICON   =00000002;
const NIF_TIP    =00000004;
  NIM_ADD         =00000000;
  NIM_MODIFY      =00000001;
  NIM_DELETE      =00000002 ;

var globIconID:integer=0;

function LocalWindowProc(HWND:hwnd;uMsg:UINT;wParam:WPARAM;lParam:LPARAM):lresult;stdcall;
var self: TTNAIcon;
begin
  result:=0;
  case umsg of
    wm_taskbarmessage,wm_command,wm_close:;
    else exit(DefWindowProc(hwnd,umsg,wparam,lparam));
  end;
  self:=TTNAIcon(GetProp(hwnd,'instance'));
  case umsg of
    wm_close:
      self.stopStandalone;
    wm_taskbarmessage:
      case lparam of
        WM_LBUTTONUP:
          self.doClick();
        WM_LBUTTONDBLCLK:
          self.doDblClick();
        WM_RBUTTONUP:
          self.doShowPopup();
      end;
    wm_command: begin
      if (HIWORD(wParam)=0) and (assigned(self.onMenuClick)) then
        self.onMenuClick(self,LOWORD(wparam));
      exit(DefWindowProc(hwnd,umsg,wparam,lparam));
    end;
  end;
end;

procedure TTNAIcon.doClick();
begin
  if assigned(onclick) then
    onclick(self);
end;
procedure TTNAIcon.doDblClick();
begin
  if assigned(ondblclick) then
    ondblclick(self);
end;
procedure TTNAIcon.doShowPopup();
var pos: TPOINT;
    rec:TRect;
begin
  if popupHandle=0 then exit;
  GetCursorPos(pos);
  SetForegroundWindow(window);
  TrackPopupMenu(popupHandle,tpm_leftalign or tpm_rightbutton,pos.x,pos.y,0,window,rec);
  pos.x:=getlasterror();
  PostMessage(window,wm_null,0,0);
  //popup.PopUp(pos.x,pos.y);
end;

constructor TTNAIcon.create(title: string; iconHandle: THandle; _popupHandle: THandle=0);
var
  tNid : TNotifyIconData ;
  wndclass: TWNDCLASS;
begin
  //====================Register Class=====================
  wndclass.style:=0;
  wndclass.lpfnWndProc:=@LocalWindowProc;//!!
  wndclass.cbClsExtra:=0;
  wndclass.cbWndExtra:=0;
  wndclass.hInstance:=HINSTANCE;
  wndclass.hIcon:=0;
  wndclass.hCursor:=0;
  wndclass.hbrBackground:=0;
  wndclass.lpszMenuName:=nil;
  wndclass.lpszClassName:=pchar(getClassName);

  RegisterClass(wndclass);
  
  //====================Create Window======================
  window:=CreateWindow(wndclass.lpszClassName,'',0,0,0,0,0,0,0,hinstance,nil);
  SetProp(window,'instance',THandle(self));

  //====================Create Popup=======================
  popupHandle:=_popupHandle;
  freePopup:=popupHandle=0;
  if freePopup then
    popupHandle:=CreatePopupMenu;


  //====================create icon========================
  inc(globIconID);
  iconID:=globIconID;
  tNid.cbSize           := SizeOf(TNotifyIconData);
  tNid.Wnd              := window;
  tNid.uID              := iconID;
  tNid.uFlags           := Nif_Message or Nif_Icon or Nif_Tip;
  tNid.uCallbackMessage := wm_taskbarmessage;
  tNid.hIcon            := iconhandle;
  StrCopy(tNid.szTip,@title[1]);        // Programm-Name - der ballon halt bei mouseover über dem icon

  Shell_NotifyIcon (Nim_Add, @tNid);
  
  currentIconHandle:=iconHandle;
  currenttitle:=title;
  freeIcon:=false;
end;

constructor TTNAIcon.create(title: string; iconFileName: string);
begin
  create(title,LoadImage(0,pchar(iconFileName),IMAGE_ICON,0,0,LR_LOADFROMFILE));
  freeIcon:=true;;
end;

destructor TTNAIcon.destroy;
var tNid : TNotifyIconData ;
begin
  {$IFDEF log}log('destroy 1');{$ENDIF}
  if iconId<>-1 then begin
    tNid.cbSize            := SizeOf(TNotifyIconData);
    tNid.Wnd               := window;
    tNid.uId               := iconID;
    Shell_NotifyIcon (Nim_Delete, @tNid);
    iconId:=-1;
  end;
  {$IFDEF log}log('destroy 2');{$ENDIF}

  if freePopup then begin
    DestroyMenu(popupHandle);
    freePopup:=false;
  end;

  {$IFDEF log}log('destroy 3');{$ENDIF}
  if freeIcon then begin
    DestroyIcon(currentIconHandle);
    freeIcon:=false;
  end;

  {$IFDEF log}log('destroy 4');{$ENDIF}
  if window<>0 then begin
    DestroyWindow(window);
    window:=0;

    UnregisterClass('BeniBela#MessageWindow#TNA',HINSTANCE);
  end;
  
  {$IFDEF log}log('destroy 5');{$ENDIF}
  inherited;
end;

procedure TTNAIcon.change(iconHandle: THandle; title: string);
var
  tNid : TNotifyIconData ;
begin
  tNid.cbSize           := SizeOf(TNotifyIconData);
  tNid.Wnd              := window;
  tNid.uID              := iconID;
  tNid.uFlags           := Nif_Icon or Nif_Tip;
  tNid.hIcon            := iconHandle;
  StrCopy(tNid.szTip,@title[1]);        // Programm-Name - der ballon halt bei mouseover über dem icon

  Shell_NotifyIcon (NIM_MODIFY, @tNid);

  if currentIconHandle<>iconHandle then
    freeIcon:=false;
  currentIconHandle:=iconHandle;
  currenttitle:=title;
end;

procedure TTNAIcon.changeIcon(iconHandle: THandle);
begin
  change(iconHandle,currenttitle);
end;

procedure TTNAIcon.changeIcon(iconFileName: string);
var oldIconHandle:THandle;
    oldFreeIcon:boolean;
begin
  oldIconHandle:=currentIconHandle;
  oldFreeIcon:=freeIcon;
  changeIcon(LoadImage(0,pchar(iconFileName),IMAGE_ICON,0,0,LR_LOADFROMFILE));
  if oldFreeIcon then
    DestroyIcon(oldIconHandle);
  freeIcon:=true;
end;

procedure TTNAIcon.changetitle(title:string);
begin
  change(currentIconHandle,title);
end;


procedure TTNAIcon.addMenuItem(id:longint;caption: string);
begin
  AppendMenu(popupHandle,MF_STRING,id,pchar(caption));
end;

procedure TTNAIcon.runStandalone;
var mes:windows.tMSG;
begin
  stopStandaloneNow:=false;
  while (not stopStandaloneNow) and windows.GetMessage( mes, 0, 0, 0 ) do begin
    windows.TranslateMessage(mes);
    windows.DispatchMessage(mes);
  end;
end;
procedure TTNAIcon.stopStandalone;
begin
  stopStandaloneNow:=true;
  postMessage(window,wm_null,0,0);
end;

class function TTNAIcon.getClassName(): string;
begin
  result:='BeniBela#'+ApplicationName+'#MessageWindow#TNA';
end;

end.

