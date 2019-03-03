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
begin
  try
    inherited UpdateFile;
  except
    on e: EFCreateError do begin
      Sleep(20);
      try
        inherited UpdateFile;
      except
        on e: EFCreateError do begin
          Sleep(20);
          inherited UpdateFile;
        end;
      end;
    end;
  end;
  CopyFile(activefilename, tmpfilename);
  fileMoveReplace(tmpfilename, truefilename);
end;

destructor TSafeIniFile.Destroy;
begin
  inherited Destroy;
end;


end.

