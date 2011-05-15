unit errorDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

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
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    sendErrorEvent:TNotifyEvent;
  public
    { public declarations }
    class procedure showError(title,error: string;detailStr: string='';sendError:TNotifyEvent=nil);
  end;

var
  showErrorForm: TshowErrorForm;

implementation

{ TshowErrorForm }

class procedure TshowErrorForm.showError(title,error: string;detailStr: string='';sendError:TNotifyEvent=nil);
var errorForm: TShowErrorForm;
begin
  errorForm:=TShowErrorForm.Create(nil);
  //errorForm:=showErrorForm;
  //TODO Move to create event
  with errorForm do begin
    caption:=title;
    Label1.Caption:=error;
    Details.Lines.Text:=detailStr;
    Button2.Visible:=detailStr<>'';
    button3.visible:=sendError<>nil;
    sendErrorEvent:=sendError;
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
  if button2.caption='Details \/\/' then begin
    height:=height+150;
    details.height:=clientheight-5-details.top;
    button2.caption:='Details /\/\';
  end else begin
    height:=height-150;
    button2.caption:='Details \/\/';
  end;
end;

procedure TshowErrorForm.Button3Click(Sender: TObject);
begin
  sendErrorEvent(sender);
  close;
end;

procedure TshowErrorForm.FormCreate(Sender: TObject);
begin

end;

procedure TshowErrorForm.FormShow(Sender: TObject);
begin
  OnShow:=nil;
  button1.top:=16+Label1.top+Label1.Height;
  button2.top:=button1.top;
  button3.top:=button1.top;
  details.top:=button1.top+36;
  height:=button1.top+button1.height+5;
  details.height:=clientheight-5-details.top;
  button2.caption:='Details \/\/';
end;

initialization
  {$I errordialog.lrs}

end.
