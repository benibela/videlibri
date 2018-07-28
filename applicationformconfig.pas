unit applicationformconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms, Graphics, controls;

type TVideLibriForm = class(TForm)
protected
  guiScaleFactor: Double;
  procedure videLibriScale(pcontrol: twincontrol);
  procedure DoCreate; override;
end;

implementation

uses bbdebugtools, LCLType,StdCtrls, ComCtrls;

type TControlBreaker = class(TControl);

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
  fontHeight, i: Integer;
  bounds, oldbounds: TRect;
begin
  guiScaleFactor := 1;
  fontHeight := Canvas.GetTextHeight('Hm,.|');

  {Canvas.GetTextMetrics(metrics);
  log('METRI'+IntToStr(metrics.Height));
  log('PP::::' + inttostr(font.PixelsPerInch));
  log('GT::::' + inttostr(font.GetTextHeight('Hm,.|')));
  log('CGT::::' + inttostr(Canvas.GetTextHeight('Hm,.|')));}

  oldbounds := BoundsRect;
  bounds := oldbounds;
  LockRealizeBounds;
  if fontHeight > REFERENCE_FONT then begin
    guiScaleFactor := fontHeight / REFERENCE_FONT;
    bounds.Width := MulDiv(bounds.Width, fontHeight, REFERENCE_FONT);
    bounds.Height := MulDiv(bounds.Height, fontHeight, REFERENCE_FONT);
  end;

  if bounds.Width > screen.Width then bounds.Width := screen.Width;
  if bounds.Height > screen.Height then bounds.Height := screen.Height;
  if bounds.Left + bounds.Width > screen.Width then bounds.Left := screen.Width - bounds.Width;
  if bounds.Top + bounds.Height > screen.Height then bounds.Top := screen.Height - bounds.Height;

  if bounds <> oldbounds then
    BoundsRect := bounds;

  if fontHeight > REFERENCE_FONT then
    videLibriScale(self);

  inherited DoCreate;

  UnlockRealizeBounds;
end;

end.

