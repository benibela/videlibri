unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, bookWatchMain, libraryParser, ExtCtrls,registry,
  ButtonPanel, EditBtn;
//TODO: Fix resizing bug (LCL)
//TODO2: Offenen Einstellungsfenster => Verschwinden aus Programmauswahl
type

  { ToptionForm }

  ToptionForm = class(TForm)
    accountList: TListView;
    Bevel1: TBevel;
    btnAccountChange: TButton;
    btnAccountCreate: TButton;
    btnAccountDelete: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    CheckBox1: TCheckBox;
    autoUpdate: TCheckBox;
    ckbAccountHistory: TCheckBox;
    cmbAccountExtend: TComboBox;
    ColorDialog1: TColorDialog;
    edtHistoryBackupInterval: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    symbols: TComboBox;
    Label19: TLabel;
    proxyHTTPName: TEdit;
    proxyHTTPSname: TEdit;
    proxyHTTPPort: TEdit;
    proxyHTTPSport: TEdit;
    timeNearMeaning: TEdit;
    edtAccountExtendDays: TEdit;
    edtAccountUser: TEdit;
    edtAccountPass: TEdit;
    edtAccountPrettyName: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblRefreshTIming: TLabel;
    Label9: TLabel;
    lblAccountPass: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblAccountLibrary: TLabel;
    lblAccountExtend1: TLabel;
    lblAccountExtend2: TLabel;
    Notebook1: TNotebook;
    pageInternet: TPage;
    pageColors: TPage;
    pageAccount: TPage;
    PageBehaviour: TPage;
    Page3: TPage;
    Panel1: TPanel;
    autostartAlways: TRadioButton;
    autostartDepends: TRadioButton;
    autostartNever: TRadioButton;
    internetWindows: TRadioButton;
    internetDirect: TRadioButton;
    homepageSimpleBrowser: TRadioButton;
    homepageDefaultBrowser: TRadioButton;
    internetProxy: TRadioButton;
    ShapeTimeNear: TShape;
    ShapeLimited: TShape;
    ShapeOK: TShape;
    ShapeOld: TShape;
    TrackBar1: TTrackBar;
    procedure accountListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure BitBtn1Click(Sender: TObject);
    procedure btnAccountChangeClick(Sender: TObject);
    procedure btnAccountCreateClick(Sender: TObject);
    procedure accountdeleteClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure cancelclick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure edtAccountUserChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageList1Change(Sender: TObject);
    procedure ListView2Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView2Click(Sender: TObject);
    procedure ListView2SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Notebook1ChangeBounds(Sender: TObject);
    procedure Notebook1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure Notebook1PageChanged(Sender: TObject);
    procedure Shape1MouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    currentSelectedAccount: TCustomAccountAccess;
    currentSelectedExtendType: TExtendType;
    function backSelect(const prompt: boolean):TCustomAccountAccess;
    procedure addAccount(const account: TCustomAccountAccess);
  end;

var
  optionForm: ToptionForm;

implementation

uses newAccountWizard_u, windows,applicationconfig;

{ ToptionForm }

procedure ToptionForm.addAccount(const account: TCustomAccountAccess);
begin
  with accountList.Items.Add do begin
    caption:=account.prettyName;
    SubItems.add(account.getUser());
    SubItems.add(account.passWord);
    if account.keepHistory then
      SubItems.add('ja')
     else
      SubItems.add('nein');
    case account.extendType of
      etAlways: SubItems.add('immer');
      etAllDepends,etSingleDepends: SubItems.add(IntToStr(account.extendDays)+' Tage vor Frist');
      etNever: SubItems.add('niemals');
    end;
    subItems.Add(account.getLibrary().prettyNameShort);
    data:=account;
  end;
end;

function ToptionForm.backSelect(const prompt: boolean):TCustomAccountAccess;
var i,newLibIndex:integer;
    currentLib: TCustomAccountAccess;
begin
  newLibIndex:=-1;
  for i:=0 to accountIDs.count-1 do begin
    currentLib:=TCustomAccountAccess(accountIDs.objects[i]);
    if edtAccountPrettyName.Text=currentLib.prettyName then begin
      if edtAccountUser.Text=currentLib.getUser() then begin
        newLibIndex:=i;
        break;
      end else if prompt and (MessageDlg('Nachfrage','Meinen sie dieses Konto: '#13#10'    Kontoname: '+currentLib.prettyName+#13#10'    Kontonummer: '+currentLib.getUser(),
                mtConfirmation ,[mbYes,mbNo],0)=mrYes) then begin
        newLibIndex:=i;
        break;
      end;
    end else if (edtAccountUser.Text=currentLib.getUser()) and prompt and
                (MessageDlg('Nachfrage','Meinen sie dieses Konto: '#13#10'    Kontoname: '+currentLib.prettyName+#13#10'    Kontonummer: '+currentLib.getUser(),
                mtConfirmation ,[mbYes,mbNo],0)=mrYes) then begin
      newLibIndex:=i;
      break;
    end;
  end;
  if newLibIndex<>-1 then begin
    result:=TCustomAccountAccess(accountIDs.objects[newLibIndex]);
    accountList.Selected:=accountList.items[newLibIndex];
  end else result:=nil;
end;

procedure ToptionForm.FormCreate(Sender: TObject);
var i:integer;
begin


  Notebook1.PageIndex:=0;

  //Accountpage
  for i:=0 to accountIDs.count-1 do
    addAccount(TCustomAccountAccess(accountIDs.objects[i]));

  //Colorpage
  ShapeLimited.brush.color:=colorLimited;
  ShapeOK.brush.color:=colorOK;
  ShapeTimeNear.brush.color:=colorTimeNear;
  ShapeOld.brush.color:=colorOld;
  timeNearMeaning.Text:=IntToStr(redTime-currentDate);
  symbols.ItemIndex:=userConfig.ReadInteger('appearance','symbols',0);

  //Internetpage
  case userConfig.readInteger('access','internet-type',0) of
    0: internetWindows.Checked:=true;
    1: internetDirect.Checked:=true;
    2: internetProxy.Checked:=true;
  end;
  proxyHTTPName.Text:=userConfig.ReadString('access','httpProxyName','');
  proxyHTTPPort.Text:=userConfig.ReadString('access','httpProxyPort','');
  proxyHTTPSname.Text:=userConfig.ReadString('access','httpsProxyName','');
  proxyHTTPSport.Text:=userConfig.ReadString('access','httpsProxyPort','');
  case userConfig.readInteger('access','homepage-type',1) of
    0: homepageSimpleBrowser.Checked:=true;
    1: homepageDefaultBrowser.Checked:=true;
  end;
  autoUpdate.checked:=userConfig.ReadInteger('updates','auto-check',1)=1;

  //Autostartpage
  TrackBar1.Position:=RefreshInterval;
  TrackBar1Change(nil);
  case userConfig.readInteger('autostart','type',1) of
    0:autostartAlways.Checked:=true;
    1:autostartDepends.Checked:=true;
    2:autostartNever.Checked:=true;
  end;
  CheckBox1.Checked:=userConfig.ReadBool('autostart','minimized',true);
  edtHistoryBackupInterval.text:=IntToStr(HistoryBackupInterval);


  Notebook1.ShowTabs:=false ;

  updateActiveInternetConfig;
end;

procedure ToptionForm.FormShow(Sender: TObject);
begin
  Notebook1.Height:=Notebook1.Height+1;
  Notebook1.Height:=Notebook1.Height-1;
end;

procedure ToptionForm.ImageList1Change(Sender: TObject);
begin

end;

procedure ToptionForm.Button3Click(Sender: TObject);
var reg:TRegistry;
begin
  colorLimited:=ShapeLimited.brush.color;
  colorOK:=ShapeOK.brush.color;
  colorTimeNear:=ShapeTimeNear.brush.color;
  colorOld:=ShapeOld.brush.color;
  redTime:=StrToInt(timeNearMeaning.Text)+currentDate;mainForm.RefreshListView;
  userConfig.WriteInteger('appearance','limited',colorLimited);
  userConfig.WriteInteger('appearance','timeNear',colorTimeNear);
  userConfig.WriteInteger('appearance','default',colorOK);
  userConfig.WriteInteger('appearance','history',colorOld);
  userConfig.WriteInteger('base','near-time',StrToInt(timeNearMeaning.Text));
  if mainForm.ViewOld.Checked then mainform.BookList.BackGroundColor:=ShapeOld.brush.color
  else mainform.BookList.BackGroundColor:=ShapeOK.brush.color;
  userConfig.WriteInteger('appearance','symbols',symbols.ItemIndex);
  mainForm.setSymbolAppearance(symbols.ItemIndex);

  //Internetpage
  if internetWindows.Checked then userConfig.writeInteger('access','internet-type',0)
  else if internetDirect.Checked then userConfig.writeInteger('access','internet-type',1)
  else if internetProxy.Checked then userConfig.writeInteger('access','internet-type',2);

  userConfig.WriteString('access','httpProxyName',proxyHTTPName.Text);
  userConfig.WriteString('access','httpProxyPort',proxyHTTPPort.Text);
  userConfig.WriteString('access','httpsProxyName',proxyHTTPSname.Text);
  userConfig.WriteString('access','httpsProxyPort',proxyHTTPSport.Text);
  if homepageSimpleBrowser.Checked then userConfig.writeInteger('access','homepage-type',0)
  else if homepageDefaultBrowser.Checked then userConfig.writeInteger('access','homepage-type',1);
  if autoUpdate.Checked then userConfig.WriteInteger('updates','auto-check',1)
  else userConfig.WriteInteger('updates','auto-check',0);

  //Autostart/Zeitenpage
  userConfig.WriteBool('autostart','minimized',CheckBox1.Checked);
  reg:=TRegistry.create;
  reg.RootKey:=HKEY_CURRENT_USER;
  reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run',true);
  if autostartAlways.Checked then begin
    userConfig.writeInteger('autostart','type',0);
    reg.WriteString('VideLibriAutostart','"'+ParamStr(0)+'" /autostart');
  end else if autostartDepends.Checked then begin
    userConfig.writeInteger('autostart','type',1);
    reg.WriteString('VideLibriAutostart','"'+ParamStr(0)+'" /autostart');
  end else if autostartNever.Checked then begin
    userConfig.writeInteger('autostart','type',2);
    reg.DeleteValue('VideLibriAutostart');
  end;
  reg.free;
  
  RefreshInterval:=TrackBar1.Position;
  userConfig.WriteInteger('access','refresh-interval',TrackBar1.Position);

  HistoryBackupInterval:=StrToInt(edtHistoryBackupInterval.Text);
  userConfig.WriteInteger('base','history-backup-interval',HistoryBackupInterval);

  mainForm.Refresh;
  ModalResult:=mrOK;
  Close;
end;

procedure ToptionForm.Button8Click(Sender: TObject);
begin
end;

procedure ToptionForm.ListView2Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
end;

procedure ToptionForm.ListView2Click(Sender: TObject);
begin

end;

procedure ToptionForm.ListView2SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
end;

procedure ToptionForm.Notebook1ChangeBounds(Sender: TObject);
begin

end;

procedure ToptionForm.Notebook1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin

end;

procedure ToptionForm.Notebook1PageChanged(Sender: TObject);
begin
end;

procedure ToptionForm.Shape1MouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog1.Execute then
    (sender as tshape).Brush.color:=ColorDialog1.Color;
end;

procedure ToptionForm.SpeedButton1Click(Sender: TObject);
begin
  Notebook1.PageIndex:=(sender as tcontrol).Tag;
  Notebook1.Height:=Notebook1.Height+1;
  Notebook1.Height:=Notebook1.Height-1;
end;

procedure ToptionForm.TrackBar1Change(Sender: TObject);
begin
  lblRefreshTIming.Left:=TrackBar1.left+15+(TrackBar1.width-15)*(TrackBar1.Position-TrackBar1.min) div (TrackBar1.Max-TrackBar1.min)-lblRefreshTIming.Width div 2;
  case TrackBar1.Position of
    0: lblRefreshTIming.Caption:='Automatische Aktualisierung: bei jedem Start';
    1: lblRefreshTIming.Caption:='Automatische Aktualisierung: maximal einmal pro Tag';
    else lblRefreshTIming.Caption:='Automatische Aktualisierung: maximal alle '+IntToStr(TrackBar1.Position)+' Tage';
  end;
end;

procedure ToptionForm.accountListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  currentSelectedAccount:=TCustomAccountAccess(item.data);
  lblAccountLibrary.Caption:=currentSelectedAccount.getLibrary().prettyNameLong;
  edtAccountPrettyName.Text:=item.Caption;
  edtAccountUser.Text:=item.SubItems[0];
  edtAccountPass.Text:=item.SubItems[1];
  ckbAccountHistory.Checked:=item.SubItems[2]='ja';
  if currentSelectedAccount.getLibrary().canModifySingleBooks then begin
    cmbAccountExtend.items.Text:='immer, wenn möglich'#13#10'alle, wenn nötig'#13#10'einzeln, wenn nötig'#13#10'niemals';
    cmbAccountExtend.ItemIndex:=longint(TCustomAccountAccess(item.data).extendType);
  end else begin
    cmbAccountExtend.items.Text:='immer, wenn möglich'#13#10'immer, wenn nötig'#13#10'niemals';
    case currentSelectedAccount.extendType of
      etAlways: cmbAccountExtend.ItemIndex:=0;
      etAllDepends,etSingleDepends: cmbAccountExtend.ItemIndex:=1;
      etNever: cmbAccountExtend.ItemIndex:=2;
    end;
  end;
  cmbAccountExtend.OnSelect(cmbAccountExtend);
  edtAccountExtendDays.Text:=inttostr(TCustomAccountAccess(item.data).extendDays);
{  case currentSelectedAccount.getLibrary().passwordType of
    ptCustom: lblAccountPass.caption:='Passwort:';
    else lblAccountPass.caption:='Geburtsdatum:';
  end;}
end;

procedure ToptionForm.BitBtn1Click(Sender: TObject);
begin
end;

procedure ToptionForm.btnAccountChangeClick(Sender: TObject);
var item:TListItem;
    lib: TCustomAccountAccess;
begin
  item:=accountList.Selected;
  lib:=TCustomAccountAccess(accountIDs.Objects[item.Index]);
  if edtAccountPass.text<>accountList.Selected.SubItems[1] then
    lib.password:=edtAccountPass.text;
  if edtAccountPrettyName.text<>accountList.Selected.Caption then
    lib.prettyName:=edtAccountPrettyName.text;
  if ckbAccountHistory.Checked<>(accountList.Selected.SubItems[2]='ja') then
    lib.keepHistory:=ckbAccountHistory.Checked;
  if currentSelectedExtendType <> lib.extendType then begin
    lib.extendType:=currentSelectedExtendType;
    case currentSelectedExtendType of
      etAlways: accountList.selected.SubItems[3]:='immer';
      etAllDepends,etSingleDepends: accountList.selected.SubItems[3]:=IntToStr(currentSelectedAccount.extendDays)+' Tage vor Frist';
      etNever: accountList.selected.SubItems[3]:='niemals';
    end;
   end else if (currentSelectedExtendType in [etAllDepends,etSingleDepends]) and (StrToInt(edtAccountExtendDays.Text) <> lib.extendDays) then begin
    lib.extendDays:=StrToInt(edtAccountExtendDays.Text);
    accountList.selected.SubItems[3]:=edtAccountExtendDays.Text+' Tage vor Frist';
   end;
  if edtAccountUser.text<>item.SubItems[0] then begin
    lib.changeUser(edtAccountUser.text);
    accountIDs[item.Index]:=lib.getID();
    saveLibIDs;
    mainForm.updateGUIItemsForAccount(lib);
  end else lib.save();
  item.Caption:=edtAccountPrettyName.text;
  item.SubItems[0]:=edtAccountUser.Text;
  item.SubItems[1]:=edtAccountPass.Text;
  if ckbAccountHistory.Checked then
    item.SubItems[2]:='ja'
   else
    item.SubItems[2]:='nein';
end;

procedure ToptionForm.btnAccountCreateClick(Sender: TObject);
var newAccount:TnewAccountWizard;
    i:integer;
begin
  newAccount:=TnewAccountWizard.Create(nil);
  if (accountList.Selected=nil) or (accountList.Selected.Caption<>edtAccountPrettyName.Text) then begin
    newAccount.accountName.Text:=edtAccountUser.text;
    newAccount.accountPass.Text:=edtAccountUser.text;
    if edtAccountPrettyName.Text<>'' then
      newAccount.accountPrettyName.Text:=edtAccountPrettyName.Text;
    newAccount.extendDaysEdit.text:=edtAccountExtendDays.text;
  end;
  newAccount.ShowModal;
  newAccount.free;

  for i:=accountList.Items.count to accountIDs.count -1 do
    addAccount(TCustomAccountAccess(accountIDs.Objects[i]));
  {if accountList.Selected <> nil then
    if (accountList.Selected.Caption=edtAccountPrettyName.Text) or
       (accountList.Selected.SubItems[0]=edtAccountUser.Text) then begin
      ShowMessage('Das Konto existiert bereits auf diesem Computer und kann deshalb nicht erstellt werden.   '#13#10+
                  'Falls Sie eine Eigenschaft von einem Konto ändern wollen, klicken Sie bitte auf den Button "Konto ändern"'#13#10+
                  'Falls Sie das Konto neu erstellen wollen, löschen Sie bitte das zuerst das alte, und erstellen es dann neu');
      exit;
    end;
  //TODO: Select library (in btnAccountCreateClick)
  lib:=TAccountAccessSTBDuesseldorf.create;
  lib.init(mainForm.basePath,edtAccountUser.Text);
  lib.setPrettyName(edtAccountPrettyName.Text);
  lib.setPassword(edtAccountPass.Text);
  lib.keepHistory:=ckbAccountHistory.Checked;
  lib.setExtendType(TExtendType( cmbAccountExtend.ItemIndex));
  lib.setExtendDays(StrToInt(edtAccountExtendDays.Text));

  addAccount(lib);

  mainForm.addGUIItemsForNewAccount(lib);

  accountIDs.AddObject(lib.getID(),lib);
  saveLibIDs;

  if MessageDlg('Daten laden?',
                'Das Konto '+lib.getPrettyName()+' wurde erstellt.'#13#10'Sollen jetzt die Mediendaten heruntergeladen werden?',
                mtConfirmation ,[mbYes,mbNo],0)=mrYes then
    mainForm.updateLibrary(lib,false,false);      }
end;

procedure ToptionForm.accountdeleteClick(Sender: TObject);
var selLib: TCustomAccountAccess;
begin
  selLib:=backSelect(true);
  if selLib=nil then exit;
  if MessageDlg('Konto löschung','Soll auf diesem Computer das Konto '+edtAccountPrettyName.text+' - '+edtAccountUser.text+' wirklich gelöscht werden?    '#13#10'Dadurch werden auch alle gespeicherten Bücherdaten dieses Kontos gelöscht   ',
                mtConfirmation ,[mbYes,mbNo],0)=mrYes then begin
    accountIDs.Delete(accountList.Selected.Index);
    saveLibIDs;
    accountList.Selected.Delete;
    selLib.remove();
    mainForm.removeGUIItemsForAccount(selLib);
    mainForm.RefreshListView;
  end;
end;

procedure ToptionForm.Button2Click(Sender: TObject);
begin
  if Notebook1.ActivePageComponent=pageAccount then begin
    if (accountList.Selected<>nil) and
       (edtAccountUser.Text=accountList.Selected.SubItems[0]) and (
         (edtAccountPrettyName.Text<>accountList.Selected.Caption) or
         (edtAccountPass.Text<>accountList.Selected.SubItems[1]) or
         (ckbAccountHistory.Checked<>(accountList.Selected.SubItems[2]='ja')) or
         (currentSelectedExtendType<>TCustomAccountAccess(accountList.Selected.Data).extendType) or (
           (currentSelectedExtendType in [etAllDepends,etSingleDepends]) and
           (StrToInt(edtAccountExtendDays.text)<>TCustomAccountAccess(accountList.Selected.Data).extendDays))) then
       if MessageDlg('Kontoänderung','Die Kontodaten für das momentan markierte Konto wurden geändert.    '#13#10'Sollen sie gespeichert werden?',
          mtConfirmation,mbYesNo,0)=mrYes then
        btnAccountChange.Click;
  end;
  ModalResult:=mrOK;
  close;
end;


procedure ToptionForm.cancelclick(Sender: TObject);
begin
  ModalResult:=mrCancel;
  Close;
end;

procedure ToptionForm.ComboBox1Change(Sender: TObject);
begin
  if currentSelectedAccount=nil then exit;
  if currentSelectedAccount.getLibrary().canModifySingleBooks then
    currentSelectedExtendType:=TExtendType(cmbAccountExtend.ItemIndex)
   else case cmbAccountExtend.ItemIndex of
     0: currentSelectedExtendType:=etAlways;
     1: currentSelectedExtendType:=etAllDepends;
     //2: currentSelectedExtendType:=etSingleDepends;
     2: currentSelectedExtendType:=etNever;
  end;
  
  if currentSelectedExtendType in [etAlways,etNever] then begin
    lblAccountExtend1.Visible:=false;
    lblAccountExtend2.Visible:=false;
    edtAccountExtendDays.Visible:=false;
    cmbAccountExtend.Width:=cmbAccountExtend.parent.ClientWidth-(407-315);
    include(cmbAccountExtend.anchors,akRight);
  end else begin
    lblAccountExtend1.Visible:=true;
    lblAccountExtend2.Visible:=true;
    edtAccountExtendDays.Visible:=true;
    cmbAccountExtend.Width:=lblAccountExtend1.Left-cmbAccountExtend.Left-5;
    exclude(cmbAccountExtend.anchors,akRight);
  end;
end;

procedure ToptionForm.edtAccountUserChange(Sender: TObject);
begin

end;

//TODO1: page icons
initialization
  {$I options.lrs}

end.

