unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, bookWatchMain, libraryParser, ExtCtrls,{$ifdef win32}registry,{$endif}
  ButtonPanel, EditBtn,Themes, xquery;
//TODO: Fix resizing bug (LCL)
//TODO2: Offenen Einstellungsfenster => Verschwinden aus Programmauswahl
type

  { ToptionForm }

  ToptionForm = class(TForm)
    accountList: TListView;
    groupingProperty: TComboBox;
    Label34: TLabel;
    Label35: TLabel;
    libNameEdit: TEdit;
    Label33: TLabel;
    Panel6: TPanel;
    internetAccessW32: TRadioButton;
    internetAccessSynapse: TRadioButton;
    templateDefine: TButton;
    Button11: TButton;
    templateName: TEdit;
    Label32: TLabel;
    libChange: TButton;
    libDelete: TButton;
    libAdd: TButton;
    checkCertificates: TCheckBox;
    Label30: TLabel;
    Label31: TLabel;
    libList: TListView;
    templateList: TListBox;
    libxml: TMemo;
    templatexml: TMemo;
    pageLibs: TPage;
    Panel4: TPanel;
    Panel5: TPanel;
    proxySocksName: TEdit;
    ShapeOrdered: TShape;
    ShapeProvided: TShape;
     SocksName: TEdit;
    proxySocksPort: TEdit;
    Label26: TLabel;
    lblShowWarning: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    mailList: TListView;
    autostartAlways: TRadioButton;
    autostartDepends: TRadioButton;
    autostartNever: TRadioButton;
    Bevel1: TBevel;
    btnAccountChange: TButton;
    btnAccountCreate: TButton;
    btnAccountDelete: TButton;
    Button1: TButton;
    mailadd: TButton;
    mailset: TButton;
    maildel: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    autoUpdate: TCheckBox;
    ckbAccountDisabled: TCheckBox;
    ckbAccountHistory: TCheckBox;
    cmbAccountExtend: TComboBox;
    ColorDialog1: TColorDialog;
    mailProgram: TEdit;
    mailreceiver: TEdit;
    mailaccounts: TEdit;
    mailinterval: TEdit;
    edtHistoryBackupInterval: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    pageMail: TPage;
    Panel2: TPanel;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Splitter1: TSplitter;
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
    timeNearMeaningLabelLeft: TLabel;
    timeNearMeaningLabelRight: TLabel;
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
    TrackBar2: TTrackBar;
    procedure accountListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure BitBtn1Click(Sender: TObject);
    procedure btnAccountChangeClick(Sender: TObject);
    procedure btnAccountCreateClick(Sender: TObject);
    procedure accountdeleteClick(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure internetAccessSynapseChange(Sender: TObject);
    procedure internetAccessSynapseChangeBounds(Sender: TObject);
    procedure internetAccessW32Change(Sender: TObject);
    procedure libAddClick(Sender: TObject);
    procedure libChangeClick(Sender: TObject);
    procedure libDeleteClick(Sender: TObject);
    procedure libListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure templateDefineClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure cancelclick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure edtAccountUserChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageList1Change(Sender: TObject);
    procedure lblShowWarningClick(Sender: TObject);
    procedure ListView2Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView2Click(Sender: TObject);
    procedure ListView2SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mailaddClick(Sender: TObject);
    procedure maildelClick(Sender: TObject);
    procedure mailListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure mailsetClick(Sender: TObject);
    procedure Notebook1ChangeBounds(Sender: TObject);
    procedure Notebook1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure Notebook1PageChanged(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
    procedure Shape1MouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure templateListSelectionChange(Sender: TObject; User: boolean);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
    currentSelectedAccount: TCustomAccountAccess;
    currentSelectedExtendType: TExtendType;
    function backSelect(const prompt: boolean):TCustomAccountAccess;
    procedure addAccount(const account: TCustomAccountAccess);
    procedure addUserLibrary(lib: TLibrary);
  end;

var
  optionForm: ToptionForm;

implementation

uses newAccountWizard_u, applicationconfig, simplehtmltreeparser, androidutils, multipagetemplate, bbutils, internetaccess, simpleinternet,math;



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
    if not account.enabled then
      SubItems.add('!DEAKTIVIERT!')
     else case account.extendType of
      etAlways: SubItems.add('immer');
      etAllDepends,etSingleDepends: SubItems.add(IntToStr(account.extendDays)+' Tage vor Frist');
      etNever: SubItems.add('niemals');
    end;
    subItems.Add(account.getLibrary().prettyNameShort);
    data:=account;
  end;
end;

procedure ToptionForm.addUserLibrary(lib: TLibrary);
var
  temp: String;
  vars: String;
  i: Integer;
  li: TListItem;
begin
  if lib = nil then exit;
  li := nil;
  for i:=0 to libList.Items.count-1 do
    if libList.Items[i].Data=pointer(lib) then li := libList.Items[i];
  if li = nil then li := libList.Items.Add;
  with li do begin
    SubItems.Clear;
    temp := lib.id;
    if strBeginsWith(temp, '-_-_-_') then system.delete(temp, 1, 6);
    Caption:=temp;
    data := lib;
    SubItems.Add(lib.prettyNameLong);
    if lib.template <> nil then SubItems.add(lib.template.name);
    vars := '';
    for i := 0 to lib.variables.count-1 do
      if vars = '' then vars := lib.variables[i]
      else vars += ', '+lib.variables[i];
    SubItems.add(vars);
  end;
end;

function ToptionForm.backSelect(const prompt: boolean):TCustomAccountAccess;
var i,newLibIndex:integer;
    currentLib: TCustomAccountAccess;
begin
  newLibIndex:=-1;
  for i:=0 to accounts.count-1 do begin
    currentLib:=accounts[i];
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
    result:=accounts[newLibIndex];
    accountList.Selected:=accountList.items[newLibIndex];
  end else result:=nil;
end;

procedure ToptionForm.FormCreate(Sender: TObject);
var i:integer;
 count: LongInt;
 searchResult: TSearchRec;
 list: TList;
 temp: String;
begin
  //TrackBar1.Color:=clWhite;
//  if ThemeServices.ThemesEnabled then TrackBar1.Color:=clWhite;
//  else TrackBar1.Color:=cl; //ThemeServices.ColorToRGB(clBtnFace);
   //TrackBar1.ControlStyle:=TrackBar1.ControlStyle-[csOpaque];

  Notebook1.PageIndex:=0;

  //Accountpage
  for i:=0 to accounts.count-1 do
    addAccount((accounts[i]));

  //Colorpage
  ShapeLimited.brush.color:=colorLimited;
  ShapeOK.brush.color:=colorOK;
  ShapeTimeNear.brush.color:=colorTimeNear;
  ShapeOld.brush.color:=colorOld;
  ShapeOrdered.brush.color:=colorOrdered;
  ShapeProvided.brush.color:=colorProvided;
  timeNearMeaning.Text:=IntToStr(redTime-currentDate);
  symbols.ItemIndex:=userConfig.ReadInteger('appearance','symbols',0);
  temp := userConfig.ReadString('appearance','groupingProperty', '');
  for i := 0 to high(groupingPropertyMap) do begin
    if i <> 0 then groupingProperty.Items.add(groupingPropertyNames[i]);
    if groupingPropertyMap[i] = temp then
      groupingProperty.ItemIndex := i;
  end;

  //Internetpage
  {$IFNDEF WINDOWS}internetAccessW32.Enabled := false;{$endif}
  case userConfig.readInteger('access','internet-backend',0) of
    0: {$IFDEF WINDOWS}internetAccessW32.Checked := True{$ELSE}internetAccessSynapse.Checked := True{$endif};
    1: internetAccessW32.Checked := True;
    2: internetAccessSynapse.Checked := True;
  end;

  case userConfig.readInteger('access','internet-type',0) of
    0: internetWindows.Checked:=true;
    1: internetDirect.Checked:=true;
    2: internetProxy.Checked:=true;
  end;
  proxyHTTPName.Text:=userConfig.ReadString('access','httpProxyName','');
  proxyHTTPPort.Text:=userConfig.ReadString('access','httpProxyPort','');
  proxyHTTPSname.Text:=userConfig.ReadString('access','httpsProxyName','');
  proxyHTTPSport.Text:=userConfig.ReadString('access','httpsProxyPort','');
  proxySocksName.Text:=userConfig.ReadString('access','socksProxyName','');
  proxySocksPort.Text:=userConfig.ReadString('access','socksProxyPort','');
  case userConfig.readInteger('access','homepage-type',1) of
    0: homepageSimpleBrowser.Checked:=true;
    1: homepageDefaultBrowser.Checked:=true;
  end;
  autoUpdate.checked:=userConfig.ReadInteger('updates','auto-check',1)=1;
  checkCertificates.Checked:=userConfig.ReadBool('access', 'checkCertificates', true);

  //Mail
  mailProgram.Text := userConfig.ReadString('Mail', 'Sendmail', 'sendmail -i -f "$from" $to');
  count := userConfig.ReadInteger('Mail', 'Reportcount', 0);
  for i:=0 to count-1 do begin
    with mailList.Items.Add do begin
      caption := userConfig.ReadString('Mailreport'+IntToStr(i), 'To', '');
      SubItems.add(userConfig.ReadString('Mailreport'+IntToStr(i), 'Accounts',  ''));
      SubItems.add(IntToStr(userConfig.ReadInteger('Mailreport'+IntToStr(i), 'Interval', 1)));
    end;
  end;


  //Autostartpage
  TrackBar1.Position:=RefreshInterval;
  trackbar2.Position:=WarnInterval;
  TrackBar1Change(nil);
  TrackBar2Change(nil);
  case userConfig.readInteger('autostart','type',1) of
    0:autostartAlways.Checked:=true;
    1:autostartDepends.Checked:=true;
    2:autostartNever.Checked:=true;
  end;
  CheckBox1.Checked:=userConfig.ReadBool('autostart','minimized',true);
  edtHistoryBackupInterval.text:=IntToStr(HistoryBackupInterval);

  //Libpage
  for i := 0 to libraryManager.templates.Count - 1 do
    if pos('|',libraryManager.templates[i]) = 0 then
      templateList.Items.add(libraryManager.templates[i]);
  if DirectoryExists(userPath+'libraries/templates') then begin
    if FindFirst(userPath+'libraries/templates/*', faDirectory, searchResult) = 0 then begin
      repeat
        if (searchResult.Name = '') or (searchResult.Name = '.') or (searchResult.Name = '..') then continue;
        if templateList.Items.IndexOf(searchResult.Name) < 0 then
          templateList.Items.Add(searchResult.Name);
      until FindNext(searchResult) <> 0;
    end;
  end;
  if DirectoryExists(assetPath+'libraries/templates') then begin
    if FindFirst(assetPath+'libraries/templates/*', faDirectory, searchResult) = 0 then begin
      repeat
        if (searchResult.Name = '') or (searchResult.Name = '.') or (searchResult.Name = '..') then continue;
        if templateList.Items.IndexOf(searchResult.Name) < 0 then
          templateList.Items.Add(searchResult.Name);
      until FindNext(searchResult) <> 0;
    end;
  end;
  count := 1;
  for i := 0 to templateList.Items.count-1 do begin
    temp := templateList.Items[i];
    if striContains(temp, 'user') then
      count := max(count, StrToIntDef(strAfter(temp, 'user'), 0) + 1);
  end;
  templateName.Text := 'user' + IntToStr(count);

  count := 1;
  list := libraryManager.getUserLibraries();
  for i := 0 to list.count - 1 do begin
    addUserLibrary(tlibrary(list[i]));
    temp := tlibrary(list[i]).id;
    if striContains(temp, 'user') then
      count := max(count, StrToIntDef(strAfter(temp, 'user'), 0) + 1);
  end;
  libNameEdit.Text := 'user' + IntToStr(count);

  updateActiveInternetConfig;
end;

procedure ToptionForm.FormResize(Sender: TObject);
begin
  timeNearMeaning.Left:=timeNearMeaningLabelLeft.Left+timeNearMeaningLabelLeft.Width+4;
  timeNearMeaningLabelRight.Left:=timeNearMeaning.Left+timeNearMeaning.Width+4;
end;

procedure ToptionForm.FormShow(Sender: TObject);
begin
  Notebook1.Height:=Notebook1.Height+1;
  Notebook1.Height:=Notebook1.Height-1;
end;

procedure ToptionForm.ImageList1Change(Sender: TObject);
begin

end;

procedure ToptionForm.lblShowWarningClick(Sender: TObject);
begin

end;

procedure ToptionForm.Button3Click(Sender: TObject);
var
 i: Integer;
 needRefreshListView: Boolean;
begin
  needRefreshListView := false;
  colorLimited:=ShapeLimited.brush.color;
  colorOK:=ShapeOK.brush.color;
  colorTimeNear:=ShapeTimeNear.brush.color;
  colorOld:=ShapeOld.brush.color;
  colorProvided := ShapeProvided.brush.Color;
  colorOrdered := ShapeOrdered.brush.Color;
  redTime:=StrToInt(timeNearMeaning.Text)+currentDate;mainForm.RefreshListView;
  userConfig.WriteInteger('appearance','limited',colorLimited);
  userConfig.WriteInteger('appearance','timeNear',colorTimeNear);
  userConfig.WriteInteger('appearance','default',colorOK);
  userConfig.WriteInteger('appearance','history',colorOld);
  userConfig.WriteInteger('appearance','ordered',colorOrdered);
  userConfig.WriteInteger('appearance','provided',colorProvided);
  userConfig.WriteInteger('base','near-time',StrToInt(timeNearMeaning.Text));
  if mainForm.ViewOld.Checked then mainform.BookList.BackGroundColor:=ShapeOld.brush.color
  else mainform.BookList.BackGroundColor:=ShapeOK.brush.color;
  userConfig.WriteInteger('appearance','symbols',symbols.ItemIndex);
  if (groupingProperty.ItemIndex >= 0)
     and (groupingProperty.ItemIndex < length(groupingPropertyMap))
     and (groupingPropertyMap[groupingProperty.ItemIndex] <> userConfig.ReadString('appearance', 'groupingProperty', '')) then begin
    userConfig.WriteString('appearance','groupingProperty', groupingPropertyMap[groupingProperty.ItemIndex]);
    if (groupingProperty.ItemIndex < mainForm.groupingItem.Count) then
       mainForm.groupingItem.Items[groupingProperty.ItemIndex].Checked := true;;
    needRefreshListView := true;
  end;
  mainForm.setSymbolAppearance(symbols.ItemIndex);

  //Internetpage
  if internetAccessW32.Checked then userConfig.writeInteger('access','internet-backend',1)
  else if internetAccessSynapse.Checked then userConfig.writeInteger('access','internet-backend', 2);

  if internetWindows.Checked then userConfig.writeInteger('access','internet-type',0)
  else if internetDirect.Checked then userConfig.writeInteger('access','internet-type',1)
  else if internetProxy.Checked then userConfig.writeInteger('access','internet-type',2);

  userConfig.WriteString('access','httpProxyName',proxyHTTPName.Text);
  userConfig.WriteString('access','httpProxyPort',proxyHTTPPort.Text);
  userConfig.WriteString('access','httpsProxyName',proxyHTTPSname.Text);
  userConfig.WriteString('access','httpsProxyPort',proxyHTTPSport.Text);
  userConfig.WriteString('access','socksProxyName',proxySocksName.Text);
  userConfig.WriteString('access','socksProxyPort',proxySocksPort.Text);
  if homepageSimpleBrowser.Checked then userConfig.writeInteger('access','homepage-type',0)
  else if homepageDefaultBrowser.Checked then userConfig.writeInteger('access','homepage-type',1);
  if autoUpdate.Checked then userConfig.WriteInteger('updates','auto-check',1)
  else userConfig.WriteInteger('updates','auto-check',0);
  if not internetAccessSynapse.Checked then
    userConfig.WriteBool('access', 'checkCertificates', checkCertificates.Checked);

  updateActiveInternetConfig;

  //Mail
  userConfig.WriteString('Mail', 'Sendmail', mailProgram.Text);
  userConfig.WriteInteger('Mail', 'Reportcount', mailList.Items.Count);
  for i:=0 to mailList.Items.Count-1 do begin
    userConfig.WriteString('Mailreport'+IntToStr(i), 'To', mailList.Items[i].Caption );
    userConfig.WriteString('Mailreport'+IntToStr(i), 'Accounts', mailList.Items[i].SubItems[0] );
    userConfig.WriteInteger('Mailreport'+IntToStr(i), 'Interval', StrToIntDef(mailList.Items[i].SubItems[1],userConfig.ReadInteger('Mailreport'+IntToStr(i), 'Interval', 1)));
  end;


  //Autostart/Zeitenpage
  userConfig.WriteBool('autostart','minimized',CheckBox1.Checked);
  updateAutostart(autostartAlways.Checked or autostartDepends.Checked,false);
  if autostartAlways.Checked then userConfig.writeInteger('autostart','type',0)
  else if autostartDepends.Checked then userConfig.writeInteger('autostart','type',1)
  else if autostartNever.Checked then userConfig.writeInteger('autostart','type',2)
  else ShowMessage('Autostart kaputt');

  RefreshInterval:=TrackBar1.Position;
  WarnInterval:=trackbar2.Position;
  userConfig.WriteInteger('access','refresh-interval',TrackBar1.Position);
  userConfig.WriteInteger('base','warn-interval',TrackBar2.Position);

  HistoryBackupInterval:=StrToInt(edtHistoryBackupInterval.Text);
  userConfig.WriteInteger('base','history-backup-interval',HistoryBackupInterval);

  mainForm.Refresh;
  if needRefreshListView then mainform.RefreshListView;
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

procedure ToptionForm.mailaddClick(Sender: TObject);
begin
  with mailList.Items.Add do begin
    Caption:=mailreceiver.Text;
    SubItems.Add(mailaccounts.Text);
    SubItems.Add(mailinterval.Text);
  end;
end;

procedure ToptionForm.maildelClick(Sender: TObject);
begin
  if (mailList.Items.Count = 0) or (mailList.SelCount = 0) or (mailList.Selected=nil) then exit;
  mailList.Items.Delete(mailList.Items.IndexOf(mailList.Selected));
end;

procedure ToptionForm.mailListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
 if (mailList.Items.Count = 0) or (mailList.SelCount = 0) or (mailList.Selected=nil) then
   exit;

 with mailList.Selected do begin
   mailreceiver.Text:=Caption;
   mailaccounts.Text:=SubItems[0];
   mailinterval.Text:=SubItems[1];
 end;
end;

procedure ToptionForm.mailsetClick(Sender: TObject);
begin
 if (mailList.Items.Count = 0) or (mailList.SelCount = 0) or (mailList.Selected=nil) then begin
   mailadd.Click;
   exit;
 end;
 with mailList.Selected do begin
   Caption:=mailreceiver.Text;
   SubItems[0]:=mailaccounts.Text;
   SubItems[1]:=mailinterval.Text;
 end;
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

procedure ToptionForm.Panel4Click(Sender: TObject);
begin

end;

procedure ToptionForm.Panel5Click(Sender: TObject);
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

procedure ToptionForm.templateListSelectionChange(Sender: TObject; User: boolean);
var
  temp: String;
begin
  temp := templateList.Items[templateList.ItemIndex];
  templateName.text:=temp;
  temp := 'libraries/templates/'+temp+'/template';
  templatexml.Lines.text := assetFileAsString(temp);
end;

procedure ToptionForm.TrackBar1Change(Sender: TObject);
begin
  lblRefreshTIming.Left:=TrackBar1.left+15+(TrackBar1.width-15)*(TrackBar1.Position-TrackBar1.min) div (TrackBar1.Max-TrackBar1.min)-lblRefreshTIming.Width div 2;
  case TrackBar1.Position of
    0: lblRefreshTIming.Caption:='Automatische Aktualisierung:'#13#10'bei jedem Start';
    1: lblRefreshTIming.Caption:='Automatische Aktualisierung:'#13#10'maximal einmal pro Tag';
    else lblRefreshTIming.Caption:='Automatische Aktualisierung:'#13#10'maximal alle '+IntToStr(TrackBar1.Position)+' Tage';
  end;
end;

procedure ToptionForm.TrackBar2Change(Sender: TObject);
begin
 lblShowWarning.Left:=TrackBar2.left+15+(TrackBar2.width-15)*(TrackBar2.Position-TrackBar2.min) div (TrackBar2.Max-TrackBar2.min)-lblShowWarning.Width div 2;
 case TrackBar2.Position of
   0: lblShowWarning.Caption:='Anzeige einer Warnung:'#13#10'bei jedem Start';
   1: lblShowWarning.Caption:='Anzeige einer Warnung:'#13#10'maximal einmal pro Tag';
   else lblShowWarning.Caption:='Anzeige einer Warnung:'#13#10'maximal alle '+IntToStr(TrackBar2.Position)+' Tage';
 end;

end;

procedure ToptionForm.accountListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if not Selected then exit;
  currentSelectedAccount:=TCustomAccountAccess(item.data);
  lblAccountLibrary.Caption:=currentSelectedAccount.getLibrary().prettyNameLong;
  edtAccountPrettyName.Text:=item.Caption;
  edtAccountUser.Text:=item.SubItems[0];
  edtAccountPass.Text:=item.SubItems[1];
  ckbAccountHistory.Checked:=item.SubItems[2]='ja';
  ckbAccountDisabled.Checked:=not currentSelectedAccount.enabled;
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
  edtAccountExtendDays.Text:=inttostr(currentSelectedAccount.extendDays);
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
  lib:=(accounts[item.Index]);
  if edtAccountPass.text<>accountList.Selected.SubItems[1] then
    lib.password:=edtAccountPass.text;
  if edtAccountPrettyName.text<>accountList.Selected.Caption then
    lib.prettyName:=edtAccountPrettyName.text;
  if ckbAccountHistory.Checked<>(accountList.Selected.SubItems[2]='ja') then
    lib.keepHistory:=ckbAccountHistory.Checked;

  lib.extendType:=currentSelectedExtendType;
  lib.extendDays:=StrToInt(edtAccountExtendDays.Text);
  lib.enabled:=not ckbAccountDisabled.Checked;
  if not lib.enabled then accountList.selected.SubItems[3]:='!DEAKTIVIERT!'
  else case currentSelectedExtendType of
    etAlways: accountList.selected.SubItems[3]:='immer';
    etAllDepends,etSingleDepends: accountList.selected.SubItems[3]:=IntToStr(currentSelectedAccount.extendDays)+' Tage vor Frist';
    etNever: accountList.selected.SubItems[3]:='niemals';
  end;
  if edtAccountUser.text<>item.SubItems[0] then begin
    lib.changeUser(edtAccountUser.text);
    accounts.Strings[item.Index]:=lib.getPlusEncodedID();
    accounts.save;
    mainForm.refreshAccountGUIElements();
    mainForm.RefreshListView;
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

  for i:=accountList.Items.count to accounts.count -1 do
    addAccount((accounts[i]));
  {if accountList.Selected <> nil then
    if (accountList.Selected.Caption=edtAccountPrettyName.Text) or
       (accountList.Selected.SubItems[0]=edtAccountUser.Text) then begin
      ShowMessage('Das Konto existiert bereits auf diesem Computer und kann deshalb nicht erstellt werden.   '#13#10+
                  'Falls Sie eine Eigenschaft von einem Konto ändern wollen, klicken Sie bitte auf den Button "Konto ändern"'#13#10+
                  'Falls Sie das Konto neu erstellen wollen, löschen Sie bitte das zuerst das alte, und erstellen es dann neu');
      exit;
    end;
  //TODO: Select library (in btnAccountCreateClick)
  libxml:=TAccountAccessSTBDuesseldorf.create;
  libxml.init(mainForm.basePath,edtAccountUser.Text);
  libxml.setPrettyName(edtAccountPrettyName.Text);
  libxml.setPassword(edtAccountPass.Text);
  libxml.keepHistory:=ckbAccountHistory.Checked;
  libxml.setExtendType(TExtendType( cmbAccountExtend.ItemIndex));
  libxml.setExtendDays(StrToInt(edtAccountExtendDays.Text));

  addAccount(libxml);

  mainForm.addGUIItemsForNewAccount(libxml);

  accountIDs.AddObject(libxml.getID(),libxml);
  saveLibIDs;

  if MessageDlg('Daten laden?',
                'Das Konto '+libxml.getPrettyName()+' wurde erstellt.'#13#10'Sollen jetzt die Mediendaten heruntergeladen werden?',
                mtConfirmation ,[mbYes,mbNo],0)=mrYes then
    mainForm.updateLibrary(libxml,false,false);      }
end;

procedure ToptionForm.accountdeleteClick(Sender: TObject);
var selLib: TCustomAccountAccess;
begin
  selLib:=backSelect(true);
  if selLib=nil then exit;
  if MessageDlg('Konto löschung','Soll auf diesem Computer das Konto '+edtAccountPrettyName.text+' - '+edtAccountUser.text+' wirklich gelöscht werden?    '#13#10'Dadurch werden auch alle gespeicherten Bücherdaten dieses Kontos gelöscht   ',
                mtConfirmation ,[mbYes,mbNo],0)=mrYes then begin
    accounts.Delete(accountList.Selected.Index);
    accounts.save;
    accountList.Selected.Delete;
    selLib.remove();
    mainForm.refreshAccountGUIElements();
    mainForm.RefreshListView;
  end;
end;

procedure ToptionForm.Button11Click(Sender: TObject);
begin
  if confirm(format('Soll das System "%s" wirklich gelöscht werden?', [templateName.Text])) then begin
    DeleteFile(userPath+'libraries/templates/'+templateName.Text+'/template');
    RemoveDir(userPath+'libraries/templates/'+templateName.Text);
    if (templateList.Items.IndexOf(templateName.text) >= 0) and not DirectoryExists(assetPath+'libraries/templates/'+templateName.text) then
      templateList.Items.Delete(templateList.Items.IndexOf(templateName.text));
  end;
end;

procedure ToptionForm.internetAccessSynapseChange(Sender: TObject);
begin
  if internetAccessSynapse.Checked then begin
    internetWindows.Enabled := false;
    checkCertificates.Enabled := false;
    checkCertificates.Checked:=false;
  end;
end;

procedure ToptionForm.internetAccessSynapseChangeBounds(Sender: TObject);
begin

end;

procedure ToptionForm.internetAccessW32Change(Sender: TObject);
begin
  if internetAccessW32.Checked then begin
    internetWindows.Enabled := true;
    checkCertificates.Enabled := true;
    checkCertificates.Checked:=userConfig.ReadBool('access', 'checkCertificates', true);
  end;
end;

procedure ToptionForm.libAddClick(Sender: TObject);
  procedure downloadAndInstallTemplate;
  var
    lib: TLibrary;
    url: String;
  begin
    url := '';
    if not InputQuery('VideLibri', 'Geben Sie die Adresse des von der Bibliothek zur Verfügung gestellten Templates ein:', url) then exit;
    try

      lib := libraryManager.downloadAndInstallUserLibrary(url);
      if lib <> nil then begin
        addUserLibrary(lib);
        if (lib.template <> nil) and (templateList.Items.IndexOf(lib.template.name) < 0) then
          templateList.Items.add(lib.template.name);;
      end;

      ShowMessage('Template installiert')
    except on e: EInternetException do
      ShowMessage('Template nicht gefunden: '+e.Message);
    on e: ELibraryException do
      ShowMessage('Template nicht gefunden: '+e.Message);
    end;
  end;

var
  libname: String;
  system: String;
  result: String;
  i: Integer;
  template: TMultiPageTemplate;
  meta: TTemplateActionMeta;
  vari: String;
  desc: String;
  systems: String;
  url: string;

label systemWrong;
begin
  if confirm('Stellt die Bibliothek ein eigenes VideLibri-Template zur Verfügung?') then begin
    downloadAndInstallTemplate;
    exit;
  end;

  libname := '';
  if not InputQuery('VideLibri', 'Geben Sie den Namen der Bibliothek ein', libname) then exit;
  system := '';
  systems := '';
  for i := 0 to templateList.Items.Count - 1 do begin
    if i <> 0 then systems += ', ';
    if i mod 10 = 9 then systems += LineEnding;
    systems += templateList.Items[i]
  end;

  systemWrong:
  if not InputQuery('VideLibri', 'Welches System verwendet die Bibliothek?'+LineEnding+'Die folgenden Systeme stehen zur Auswahl: '+LineEnding+systems, system) then
    exit;
  if templateList.Items.IndexOf(system) < 0 then goto systemWrong;

  result := '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding;
  result += '<library>' + LineEnding;
  result += '  <longName value="'+xmlStrEscape(libname)+'"/>'+LineEnding;
  result += '  <template value="'+xmlStrEscape(system)+'"/>'+LineEnding;

  template := libraryManager.getTemplate(system);
  meta := nil;
  for i := 0 to high(template.baseActions.children) do
    if template.baseActions.children[i] is TTemplateActionMeta then
      meta := template.baseActions.children[i] as TTemplateActionMeta ;

  if meta <> nil then begin
    for i := 0 to high(meta.variables) do begin
      vari := meta.variables[i].def;
      if meta.variables[i].hasDef then begin
        desc := format('Es kann ein Wert für die optionale Variable "%s" gesetzt werden. (%s)', [meta.variables[i].name, meta.variables[i].description]);
        if not InputQuery('VideLibri', desc, vari) then exit;
        if vari <> meta.variables[i].def then
          result += '  <variable name="'+xmlStrEscape(meta.variables[i].name)+'" value="'+xmlStrEscape(vari)+'"/>'+LineEnding;
      end
      else begin
        desc := format('Das Template benötigt einen Wert für die Variable "%s". (%s)', [meta.variables[i].name, meta.variables[i].description]);
        if not InputQuery('VideLibri', desc, vari) then exit;
        result += '  <variable name="'+xmlStrEscape(meta.variables[i].name)+'" value="'+xmlStrEscape(vari)+'"/>'+LineEnding;
      end;
    end;
  end;


  result += '</library>' + LineEnding;

  ShowMessage('Bibliotheksdaten erstellt. Um sie zu speichern, klicken Sie auf "Bibliothek speichern"');

  libxml.text := result;
end;

procedure ToptionForm.libChangeClick(Sender: TObject);
var
  trueId: String;
  userlibs: TStringArray;
begin
  if (libList.Selected <> nil) and (libList.Selected.Caption = libNameEdit.Text) then trueid := TLibrary(libList.Selected.Data).id
  else trueId := '-_-_-_'+trim(libNameEdit.text);

  libraryManager.setUserLibrary(trueId, libxml.Lines.Text);
  addUserLibrary(libraryManager.get(trueId));
end;

procedure ToptionForm.libDeleteClick(Sender: TObject);
var
  trueId: String;
  i: Integer;
begin
  if not confirm('Wollen Sie die Bücherei '+libNameEdit.Text+' löschen?') then exit;
  if (libList.Selected <> nil) and (libList.Selected.Caption = libNameEdit.Text) then trueid := TLibrary(libList.Selected.Data).id
  else trueId := '-_-_-_'+trim(libNameEdit.text);
  libraryManager.deleteUserLibrary(trueid);
  for i := 0 to libList.Items.Count - 1 do
    if libList.Items[i].Caption = trim(libNameEdit.text) then begin
      libList.Items.Delete(i);
      break;
  end;
end;

procedure ToptionForm.libListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  lib: TLibrary;
  temp: String;
begin
  if not Selected then exit;
  lib := TLibrary(item.Data);
  libxml.Lines.LoadFromFile(userPath+'libraries/'+lib.id+'.xml');
  temp := lib.id;
  if strBeginsWith(temp, '-_-_-_') then delete(temp,1,6);
  libNameEdit.Text:=temp;
end;


procedure ToptionForm.templateDefineClick(Sender: TObject);
var
  oldTemplateIdx: Integer;
  oldTemplate: TObject;
  i: Integer;
  j: Integer;
begin
  oldTemplateIdx := libraryManager.templates.IndexOf(templateName.Text);
  if oldTemplateIdx >= 0 then oldTemplate := libraryManager.templates.Objects[oldTemplateIdx];

  if not DirectoryExists(userPath+'libraries/templates/'+templateName.Text) then
    ForceDirectories(userPath+'libraries/templates/'+templateName.Text+'');
  templatexml.Lines.SaveToFile(userPath+'libraries/templates/'+templateName.Text+'/template');
  if templateList.Items.IndexOf(templateName.Text) < 0 then
    templateList.Items.add(templateName.Text);

  if oldTemplateIdx >= 0 then begin
    libraryManager.templates.Delete(oldTemplateIdx); //memory leak, but who cares (better than killing a template still used in a thread)
    for i := 0 to libraryManager.count - 1 do
      if libraryManager[i].template = oldTemplate then begin
        libraryManager[i].template := libraryManager.getTemplate(templateName.Text);
        for j := 0 to accounts.Count - 1 do
          if (accounts[j].getLibrary() = libraryManager[i]) and (accounts[j] is TTemplateAccountAccess) and (accounts[j].thread = nil) then
            TTemplateAccountAccess(accounts[j]).resetlib();
      end;
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
  button3.Click;
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
    cmbAccountExtend.Width:=cmbAccountExtend.parent.ClientWidth-cmbAccountExtend.Left-5;
    cmbAccountExtend.anchors:=cmbAccountExtend.anchors+[akRight];
  end else begin
    lblAccountExtend1.Visible:=true;
    lblAccountExtend2.Visible:=true;
    edtAccountExtendDays.Visible:=true;
    cmbAccountExtend.Width:=lblAccountExtend1.Left-cmbAccountExtend.Left-5;
    cmbAccountExtend.anchors:=cmbAccountExtend.anchors+[akRight];
  end;
end;

procedure ToptionForm.edtAccountUserChange(Sender: TObject);
begin

end;

//TODO1: page icons
initialization
  {$I options.lrs}

end.

