unit debuglogviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TDebugLogForm = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DebugLogForm: TDebugLogForm;

implementation

uses bbdebugtools, applicationdesktopconfig;
procedure TDebugLogForm.FormShow(Sender: TObject);
begin
  if not logging then begin
    logging:=true;
    log('Debug logging started');
  end;
  Timer1Timer(sender);
end;

procedure TDebugLogForm.FormCreate(Sender: TObject);
begin
  globalOnFormCreate(self);
end;

var oldFileSize: integer;

procedure TDebugLogForm.Timer1Timer(Sender: TObject);
var
  temp: TStringList;
  tempstream: TFileStream;
begin
  if FileSize(logFileName) <> oldFileSize then begin
    oldFileSize := FileSize(logFilename);
    temp := TStringList.Create;
    tempstream:=TFileStream.Create(logFileName,fmOpenRead or fmShareDenyNone);
    temp.LoadFromStream(tempstream);
    tempstream.free;
    memo1.Lines.BeginUpdate;
    while temp.Count > memo1.lines.Count do
      memo1.Lines.add(temp[memo1.Lines.Count]);
    memo1.Lines.EndUpdate;
    temp.free;
  end;
end;

initialization
  {$I debuglogviewer.lrs}

end.

