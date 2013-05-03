unit accountlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libraryParser;

type

{ TAccountList }

 TAccountList = class(TStringList)
private
  fileName: string;
  libs: TLibraryManager;
  function get(i: integer): TCustomAccountAccess;
public
  constructor create(listFile: string; libraries: TLibraryManager);
  procedure load;
  procedure save;
  property Accounts[i: integer]: TCustomAccountAccess read get; default;
  destructor Destroy; override;
end;
implementation

uses bbdebugtools;
{ TAccountList }

function TAccountList.get(i: integer): TCustomAccountAccess;
begin
  result := TCustomAccountAccess(Objects[i]);
end;

constructor TAccountList.create(listFile: string; libraries: TLibraryManager);
begin
  fileName := listFile;
  libs := libraries;
end;

procedure TAccountList.load;
var
  i: Integer;
  deprecatedAccounts: Boolean;
begin
  if not FileExists(fileName) then begin
    if logging then log('Creating file: '+filename+ '  (written lines: '+inttostr(count)+')');
    SaveToFile(fileName);
  end;
  LoadFromFile(fileName);
  deprecatedAccounts := false;
  for i:=0 to count-1 do begin
    Objects[i]:=libs.getAccount(Strings[i]);
    if (Accounts[i].getID() <> Strings[i]) and (length(Strings[i]) > 4) and ((Strings[i])[4] = '#') then begin
      if logging then log('Renamed account: '+Strings[i]+' => '+Accounts[i].getID());
      deprecatedAccounts := true;
      Strings[i] := Accounts[i].getID();
    end;
  end;
  if deprecatedAccounts then save;
end;

procedure TAccountList.save;
begin
  if logging then log('Save account list');
  SaveToFile(fileName);
end;

destructor TAccountList.Destroy;
var
  i: Integer;
begin
  for i:=0 to count-1 do
    try
      Objects[i].free;
    except
      ;
    end;
  inherited Destroy;
end;

end.

