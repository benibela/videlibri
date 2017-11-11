unit errorDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, applicationconfig;

type

  { TshowErrorForm }

  TshowErrorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Details: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    sendErrorEvent, checkPasswordEvent:TNotifyEvent;
    procedure checkPassword(Sender: TObject);
    procedure arrangeLayout;
  public
    { public declarations }
    class procedure showError(kind: TExceptionKind; title,error: string;detailStr: string;sendError, acheckPasswordEvent: TNotifyEvent);
  end;

var
  showErrorForm: TshowErrorForm;

resourcestring
  rsCheckYourPassword = 'Passwort überprüfen...';
implementation

uses applicationdesktopconfig, math;

{ TshowErrorForm }

class procedure TshowErrorForm.showError(kind: TExceptionKind; title,error: string;detailStr: string;sendError, acheckPasswordEvent:TNotifyEvent);
var errorForm: TShowErrorForm;
begin
  errorForm:=TShowErrorForm.Create(nil);
  //errorForm:=showErrorForm;
  //TODO Move to create event
  with errorForm do begin
    caption:=title;
    Label1.Caption:=error;
    Details.Lines.Text:=detailStr;
    if (kind = ekLogin) and assigned(acheckPasswordEvent) then begin
      button2.Visible := true;
      button2.Caption := rsCheckYourPassword;
      button2.onClick := @checkPassword;
      button2.Font.Style := button2.Font.Style + [fsBold];
    end else Button2.Visible:=detailStr<>'';
    button3.visible:=sendError<>nil;
    sendErrorEvent:=sendError;
    checkPasswordEvent := acheckPasswordEvent;
  end;
  errorForm.showmodal;
  errorForm.free;
//  while errorForm.Visible do Application.ProcessMessages;
end;
procedure TshowErrorForm.Button1Click(Sender: TObject);
begin
  ModalResult:=mrOK;
  close;
end;

procedure TshowErrorForm.Button2Click(Sender: TObject);
begin
  if button2.caption= rsDetails + ' \/\/' then begin
    height:=height+150;
    details.height:=clientheight-5-details.top;
    button2.caption:=rsDetails + ' /\/\';
  end else begin
    height:=height-150;
    button2.caption:=rsDetails + ' \/\/';
  end;
end;

procedure TshowErrorForm.Button3Click(Sender: TObject);
begin
  sendErrorEvent(sender);
  close;
end;

procedure TshowErrorForm.FormCreate(Sender: TObject);
begin
  button2.caption:=rsDetails + ' \/\/';
end;

procedure TshowErrorForm.FormResize(Sender: TObject);
begin
  label1.Constraints.MaxWidth := max(ClientWidth - 2 * label1.Left, 30);
  label1.Constraints.MinWidth := max(ClientWidth - 2 * label1.Left, 0);
  arrangeLayout;
end;

procedure TshowErrorForm.FormShow(Sender: TObject);
begin
  arrangeLayout;
  OnShow:=nil;
 // details.height:=clientheight-5-details.top;
end;

procedure TshowErrorForm.checkPassword(Sender: TObject);
begin
  checkPasswordEvent(sender);
  close;
end;

procedure TshowErrorForm.arrangeLayout;
begin
  button1.Left := math.max(button3.Left + button3.Width, (ClientWidth - button1.Width) div 2);
  button2.Left := ClientWidth - 5 - button2.Width;
  if (OnShow <> nil) or (button1.top <> 16+Label1.top+Label1.Height) then begin
    button1.top:=16+Label1.top+Label1.Height;
    button2.top:=button1.top;
    button3.top:=button1.top;
    details.top:=button1.top+button1.Height+6;
    height:=button1.top+button1.height+5;
  end;
end;

initialization
  {$I errordialog.lrs}

end.

