unit registrierung;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, LCLType;

type

  { TRegForm }

  TRegForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    username: TEdit;
    usercode: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure usercodeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    lastNameLength,lastCodeLength:longint;
    realCode,realName:String;
  end;

var
  RegForm: TRegForm;

implementation

uses applicationconfig,bookWatchMain;
{ TRegForm }

procedure TRegForm.Button1Click(Sender: TObject);
var i:longint;
    f,l:char;
begin
  if length(username.text)<4 then begin
    ShowMessage('Der eingegebene Name ist zu kurz');
    exit;
  end;

  while (lastNameLength<>length(username.text)) or (lastCodeLength<>length(usercode.text)) do
    Application.ProcessMessages;

  if length(usercode.text)<4 then begin
    ShowMessage('Der eingegebene Code ist zu kurz');
    exit;
  end;

  while (lastNameLength<>length(username.text)) or (lastCodeLength<>length(usercode.text)) do
    Application.ProcessMessages;
    
  if length(usercode.text)<14 then begin
    ShowMessage('Der eingegebene Code ist leider ungültig');
    exit;
  end;

  f:=upcase(username.text[1]);

  if (usercode.text[5]<>'-') or (usercode.text[10]<>'-') then begin
    ShowMessage('Der eingegebene Code ist leider ungültig');
    exit;
  end;
  
  l:=upcase(username.text[length(username.text)]);

  for i:=1 to 14 do
    if ((i<>5)and(i<>10) and (((i and 1=0) and (upcase(usercode.text[i])<>f)) or
       ((i and 1=1) and (upcase(usercode.text[i])<>l)))) then begin
    ShowMessage('Der eingegeben Code ist leider ungültig');
    ModalResult:=mrAbort;
    close;
  end;
  
  userConfig.WriteString('registration','user',realName);
  userConfig.WriteString('registration','code',realCode);
  
  if realName<>sharewareUser then
    if Application.MessageBox(pchar('Vielen Dank für Ihre Registrierung, '+realName+#13#10'Das Programm wird jetzt neugestartet, um die Registrierung abzuschließen'),'Registrierung',MB_OKCANCEL)=mrok then
      needApplicationRestart:=true;

  ModalResult:=mrOK;
  close;
end;

procedure TRegForm.Button2Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
  close;
end;

procedure TRegForm.FormCreate(Sender: TObject);
var s:string;
    i:longint;
begin
  username.Text:=sharewareUser;
  {$I obfuscate.inc}
  realName:=sharewareUser;
  if sharewareCode<>'' then begin
    Label3.Caption:=sharewareCode;
    usercode.Text:=sharewareCode;
    {$I obfuscate.inc}
    realCode:=sharewareCode;
  end else begin
    {$I obfuscate.inc}
    s:='Pbqr vfg tüygvt';
    for i:=1 to length(s) do
      if s[i] in ['a'..'z'] then s[i]:=chr((ord(s[i])-ord('a')+12) mod 26+1+ord('a'))
      else if s[i] in ['A'..'Z'] then s[i]:=chr((ord(s[i])-ord('A')+12) mod 26+1+ord('A'));
    Label3.Caption:=s;
  end;
end;

procedure TRegForm.FormResize(Sender: TObject);
begin
end;

procedure TRegForm.Timer1Timer(Sender: TObject);
var ok4,ok5,nok4,nok5:boolean;
    u,c,nc:string;
    i,temp:longint;
    f,l:char;
    nok1,ok1,ok2:longbool;
    ok3,nok2,nok3:longbool;
{$include _shareware.inc}
begin
  {$include obfuscate.inc}
  u:=username.Text;
  {$include obfuscate.inc}
  c:=usercode.text;
  if checkShareWare(ok1,nok1,u,c,ok2,nok2) then begin
    f:=u[1];
    l:=u[length(u)];
    
    for i:=1 to 14 do
      if (i and 1=0) then nc:=nc+f
      else nc:=nc+l;
    nc[5]:='-';
    nc[10]:='-';
    {$include obfuscate.inc}
    usercode.Text:=nc;
    {$include obfuscate.inc}
    usercode.Visible:=false;
  end;

  temp:=length(u);
  {$include obfuscate.inc}
  lastNameLength:=temp;
  temp:=length(c);
  {$include obfuscate.inc}
  lastCodeLength:=temp;
end;


procedure TRegForm.usercodeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  realCode:=usercode.text;
  realName:=username.text;
end;

initialization
  {$I registrierung.lrs}

end.

