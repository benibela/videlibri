unit debuglogviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,applicationformconfig;

type
  TDebugLogForm = class(TVideLibriForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure DebugLogFormLog(message: string);
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
  Timer1Timer(sender);
end;

var logModified: boolean;

procedure TDebugLogForm.FormCreate(Sender: TObject);
begin
  bbdebugtools.OnLog:=@DebugLogFormLog;
  logModified := true;
  if not logging then begin
    logging:=true;
    log('Debug logging started');
  end;
  memo1.Lines.Add('Debug Log File: ' + logFileName);

end;


procedure TDebugLogForm.Timer1Timer(Sender: TObject);
var
  temp: TStringList;
  tempstream: TFileStream;
begin
  if logModified then begin
    logModified := false;
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

procedure TDebugLogForm.DebugLogFormLog(message: string);
begin
  logModified := true;
end;

initialization
  {$I debuglogviewer.lrs}

end.

