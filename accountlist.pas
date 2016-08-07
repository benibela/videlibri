unit accountlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libraryParser;

type

{ TAccountList }

 TAccountEvent = procedure (acc: TCustomAccountAccess) of object;
 TAccountList = class(TStringList)
private
  fileName: string;
  libs: TLibraryManager;
  function get(i: integer): TCustomAccountAccess;
public
  OnAccountAdd: TAccountEvent;

  constructor create(listFile: string; libraries: TLibraryManager);
  procedure load;
  procedure save;
  property Accounts[i: integer]: TCustomAccountAccess read get; default;
  destructor Destroy; override;

  function add(const libID: string; prettyName, aname, pass: string; extendType: TExtendType; extendDays:integer; history: boolean):TCustomAccountAccess;
  procedure add(account: TCustomAccountAccess);
end;
implementation

uses bbutils, bbdebugtools, applicationconfig;
{ TAccountList }

procedure stringsSaveSafeCallback(stream: TStream; data: pointer);
begin
  stream.Size := 0;
  tstrings(data).SaveToStream(stream);
end;

procedure stringsSaveSafe(sl: TStrings; fn: string);
begin
  fileSaveSafe(fn, @stringsSaveSafeCallback, sl  );
end;

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
    stringsSaveSafe(self, fileName);
  end;
  LoadFromFile(fileName);
  deprecatedAccounts := false;
  for i:=0 to count-1 do begin
    Objects[i]:=libs.getAccount(Strings[i]);
    if (Accounts[i].getPlusEncodedID() <> Strings[i]) and (length(Strings[i]) > 4) and ((Strings[i])[4] = '#') then begin
      if logging then log('Renamed account: '+Strings[i]+' => '+Accounts[i].getPlusEncodedID());
      deprecatedAccounts := true;
      Strings[i] := Accounts[i].getPlusEncodedID();
    end;
  end;
  if deprecatedAccounts then save;
end;

procedure TAccountList.save;
begin
  if logging then log('Save account list');
  stringsSaveSafe(self, fileName);
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

function TAccountList.add(const libID: string; prettyName, aname, pass: string; extendType: TExtendType; extendDays: integer;
  history: boolean): TCustomAccountAccess;
begin
  {if accountList.Selected <> nil then
      if (accountList.Selected.Caption=edtAccountPrettyName.Text) or
         (accountList.Selected.SubItems[0]=edtAccountUser.Text) then begin
        ShowMessage('Das Konto existiert bereits auf diesem Computer und kann deshalb nicht erstellt werden.   '#13#10+
                    'Falls Sie eine Eigenschaft von einem Konto ändern wollen, klicken Sie bitte auf den Button "Konto ändern"'#13#10+
                    'Falls Sie das Konto neu erstellen wollen, löschen Sie bitte das zuerst das alte, und erstellen es dann neu');
        exit;
     end;   }

  result:=libraryManager.getAccount(libID,aname);
  result.prettyName:=prettyName;
  result.password:=pass;
  result.keepHistory:=history;//ckbAccountHistory.Checked;
  result.extendType:=extendType;// TExtendType( cmbAccountExtend.ItemIndex));
  result.extendDays:=extendDays;// StrToInt(edtAccountExtendDays.Text));

  add(result);


{  if MessageDlg('Daten laden?',
                'Das Konto '+lib.getPrettyName()+' wurde erstellt.'#13#10'Sollen jetzt die Mediendaten heruntergeladen werden?',
                mtConfirmation ,[mbYes,mbNo],0)=mrYes then
    mainForm.updateLibrary(lib,false,false);}
end;

procedure TAccountList.add(account: TCustomAccountAccess);
begin
  AddObject(account.getPlusEncodedID(),account);
  account.save();
  save;
  if assigned(OnAccountAdd) then OnAccountAdd(account);
end;

end.


