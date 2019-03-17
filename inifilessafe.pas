unit inifilessafe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;


type TSafeIniFile = class(TIniFile)
  truefilename, activefilename, tmpfilename: string;
  constructor Create(const AFileName: string);
  procedure UpdateFile; override;
  destructor Destroy; override;
end;



implementation

uses bbutils,FileUtil;

constructor TSafeIniFile.Create(const AFileName: string);
begin
  truefilename := AFileName;
  activefilename := AFileName + '.sav';
  tmpfilename := AFileName + '~' + IntToStr(Random(10000)) + '.tmp';
  CopyFile(truefilename, activefilename);
  inherited Create(activefilename);
end;

procedure TSafeIniFile.UpdateFile;
const MAX_RETRIES = 5;
var
  ok: Boolean;
  retry: Integer;
begin
  ok := false;
  fileMoveReplace(activefilename, tmpfilename);
  try
    for retry := 1 to MAX_RETRIES do begin
      try
        inherited UpdateFile;
        fileMoveReplace(activefilename, truefilename);
        CopyFile(truefilename, activefilename); //this also retries
        DeleteFile(tmpfilename);
        ok := true;
        exit;
      except
        on e: EFCreateError do begin
          if retry >= MAX_RETRIES then raise;
          sleep(30);
        end;
      end;
    end;
  finally
    if not ok then
      fileMoveReplace(tmpfilename, activefilename);
  end;
end;

destructor TSafeIniFile.Destroy;
begin
  inherited Destroy;
end;


end.

