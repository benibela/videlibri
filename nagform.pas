unit nagform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls;

type

  { TnagWindow }

  TnagWindow = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  nagWindow: TnagWindow;

implementation
uses applicationconfig, bookWatchMain,registrierung;
{ TnagWindow }

procedure TnagWindow.Timer1Timer(Sender: TObject);
begin
  Button1.Enabled:=true;
  button1.caption:=button1.hint;
end;

procedure TnagWindow.Button1Click(Sender: TObject);
begin
  ModalResult:=mrOK;
  close;
end;

procedure TnagWindow.Button2Click(Sender: TObject);
var regForm:TRegForm;
begin
  ModalResult:=mrOK;
  regForm:=TRegForm.Create(nil);
  regForm.ShowModal;
  regForm.free;
  if needApplicationRestart then close;
end;

procedure TnagWindow.FormShow(Sender: TObject);
begin
  tag:=6785;
  mainForm.Label2.Caption:='  eingeschränkte Shareware Version';
end;

initialization
  {$I nagform.lrs}

end.

