unit applicationformconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms, Graphics, controls;

type TVideLibriForm = class(TForm)
  function GetRealBounds: TRect;
  procedure SetRealBounds(bounds: TRect);
  procedure MyAutoPlacement(var bounds: TRect; const oldBounds: TRect);
protected
  guiScaleFactor: Double;
  procedure videLibriScale(pcontrol: twincontrol);
  procedure DoCreate; override;
  procedure DoShow; override;
end;

implementation

uses bbdebugtools, LCLType,StdCtrls, ComCtrls, Dialogs, LCLIntf;

type TControlBreaker = class(TControl);

var BoundsRectOffset: TPoint;

function TVideLibriForm.GetRealBounds: TRect;
var realRect: TRect;
begin
  result := BoundsRect;
  if not BoundsRectOffset.IsZero then begin
    result.Right += BoundsRectOffset.x;
    result.Bottom += BoundsRectOffset.y;
  end else if HandleAllocated and (GetWindowRect(Handle, realRect{%H-}) <> 0) then begin
    result.Width := realRect.Width;
    result.Height := realRect.Height;
  end;
end;

procedure TVideLibriForm.SetRealBounds(bounds: TRect);
begin
  bounds.Right -= BoundsRectOffset.x;
  bounds.Bottom -= BoundsRectOffset.y;
  BoundsRect := bounds;
end;

procedure TVideLibriForm.MyAutoPlacement(var bounds: TRect; const oldBounds: TRect);
var workarea: TRect;
  mon: TMonitor;
begin
  //see MoveToDefaultPosition
  workarea := default(TRect);
  if Application.MainForm <> nil then mon := Application.MainForm.Monitor
  else mon := Monitor;
  if mon <> nil then
    workarea := mon.WorkareaRect;
  if (workarea.Width = 0) or (workarea.Height = 0) then //safety check. does this part have any use?
    workarea := screen.WorkAreaRect;

{  if (Application.MainForm <> nil) and (Application.MainForm <> self) then
  ShowMessage(
      'bounds: ' + IntToStr(bounds.Left) + ' ' + inttostr(bounds.Top) + ' x ' + IntToStr(bounds.Width) + ' ' + inttostr(bounds.Height)  + #13#10
      + 'workarea: ' + IntToStr(workarea.Left) + ' ' + inttostr(workarea.Top) + ' x ' + IntToStr(workarea.Width) + ' ' + inttostr(workarea.Height) + #13#10
      + 'screen: ' + IntToStr(Screen.Width) + ' ' + inttostr(Screen.Height) + #13#10
      + 'Screen workarea: ' + IntToStr(screen.WorkAreaLeft) + ' ' + inttostr(screen.WorkAreaTop) + ' x ' + IntToStr(screen.WorkAreaWidth) + ' ' + inttostr(screen.WorkAreaHeight) + #13#10
      + 'Screen desktop: ' + IntToStr(screen.DesktopLeft) + ' ' + inttostr(screen.DesktopTop) + ' x ' + IntToStr(screen.DesktopWidth) + ' ' + inttostr(screen.DesktopHeight)
//      + 'monitor: ' + IntToStr(Screen.Width) + ' ' + inttostr(Screen.Height)
  );
}

  if bounds.Width > workarea.Width then bounds.Width := workarea.Width;
  if bounds.Height > workarea.Height then bounds.Height := workarea.Height;
  bounds.SetLocation(workarea.Left + (workarea.Width - bounds.Width) div 2, workarea.Top + (workarea.Height - bounds.Height) div 2);

  if bounds <> oldbounds then
    SetRealBounds(bounds);
end;

procedure TVideLibriForm.videLibriScale(pcontrol: twincontrol);
var
  i, j: Integer;
  c: TWinControl;
  lv: TListView;
  newWidth: Int64;
begin
  for i := 0 to pcontrol.ControlCount - 1 do begin
    if pcontrol.Controls[i] is TWinControl then begin
      c := TWinControl(pcontrol.Controls[i]);
      if c.AutoSize and not TControlBreaker(c).WidthIsAnchored
         and (
           (c is TEdit) or (c is TComboBox)
        ) then begin
          newWidth := MathRound(c.Width * guiScaleFactor);
          if akRight in  c.Anchors then
            c.SetBoundsKeepBase( c.Left + c.Width - newWidth, c.top, newWidth, c.Height ) //keep right border
           else
            c.Width := newWidth;
       //   log(c.Name+ ' => ' + inttostr(c.Width));
      {else if c is tlistview then begin //https://bugs.freepascal.org/view.php?id=34044
        lv := TListView(c);
        for j := 0 to lv.ColumnCount - 1 do
          if lv.Column[j].Width <> 0 then
          begin
            log(lv.Column[j].Caption + ' '+IntToStr(lv.Column[j].Width));
//            lv.Column[j].Width := MathRound(lv.Column[j].Width * guiScaleFactor);
  //          log(lv.Column[j].Caption + ' -> '+IntToStr(lv.Column[j].Width));
          end;
      end; }
        end;

      videLibriScale(c);
    end;
  end;

end;

procedure TVideLibriForm.DoCreate;
const REFERENCE_FONT = 19; //19 on Linux, 15 on Windows for the same DPI
var
  fontHeight: Integer;
  bounds, oldbounds: TRect;
begin
  guiScaleFactor := 1;
  fontHeight := Canvas.GetTextHeight('Hm,.|');

  {Canvas.GetTextMetrics(metrics);
  log('METRI'+IntToStr(metrics.Height));
  log('PP::::' + inttostr(font.PixelsPerInch));
  log('GT::::' + inttostr(font.GetTextHeight('Hm,.|')));
  log('CGT::::' + inttostr(Canvas.GetTextHeight('Hm,.|')));}

  oldbounds := GetRealBounds;
  bounds := oldbounds;
  LockRealizeBounds;
  if fontHeight > REFERENCE_FONT then begin
    guiScaleFactor := fontHeight / REFERENCE_FONT;
    bounds.Width := MulDiv(bounds.Width, fontHeight, REFERENCE_FONT);
    bounds.Height := MulDiv(bounds.Height, fontHeight, REFERENCE_FONT);
  end;

  MyAutoPlacement(bounds, oldBounds);

  if fontHeight > REFERENCE_FONT then
    videLibriScale(self);

  inherited DoCreate;

  UnlockRealizeBounds;
{  if (Application.MainForm <> nil) and (Application.MainForm <> self) then
  ShowMessage(
    'bounds: ' + IntToStr(bounds.Left) + ' ' + inttostr(bounds.Top) + ' x ' + IntToStr(bounds.Width) + ' ' + inttostr(bounds.Height)  + #13#10  +
    'boundsRect: ' + IntToStr(boundsRect.Left) + ' ' + inttostr(boundsRect.Top) + ' x ' + IntToStr(boundsRect.Width) + ' ' + inttostr(boundsRect.Height)
    );}
end;

procedure TVideLibriForm.DoShow;
var
  bounds, oldbounds: TRect;
begin
  oldbounds := GetRealBounds;
  if BoundsRectOffset.IsZero then begin //this is here, because I am not sure the window has a valid rect in docreate
    BoundsRectOffset.x := oldbounds.Width - Width;
    BoundsRectOffset.y := oldbounds.Height - Height;
  end;
  bounds := oldbounds;

  MyAutoPlacement(bounds, oldBounds);
  inherited DoShow;
end;

end.

