unit options;

{$I videlibrilanguageconfig.inc}
{$WARN 5024 on : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, bookWatchMain, libraryParser, ExtCtrls,{$ifdef win32}registry,{$endif}
   EditBtn,Themes, xquery,applicationformconfig;
//TODO: Fix resizing bug (LCL)
//TODO2: Offenen Einstellungsfenster => Verschwinden aus Programmauswahl
type

  { ToptionForm }

  ToptionForm = class(TVideLibriForm)
    accountList: TListView;
    Button10: TButton;
    Button12: TButton;
    Button13: TButton;
    cbCopyAccountLimits: TCheckBox;
    accountType: TComboBox;
    templateList: TComboBox;
    templateFile: TComboBox;
    Label36: TLabel;
    Label37: TLabel;
    openSSLCAStore: TEdit;
    groupingProperty: TComboBox;
    Label34: TLabel;
    Label35: TLabel;
    openSSLCAStoreLabel: TLabel;
    lblAccountType: TLabel;
    libNameEdit: TEdit;
    Label33: TLabel;
    PageControlLibs: TPageControl;
    Panel6: TPanel;
    internetAccessW32: TRadioButton;
    internetAccessSynapse: TRadioButton;
    Panel7: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
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
    SpeedButtonLibraryConfig: TSpeedButton;
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
    procedure accountListSelectItem({%H-}Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure BitBtn1Click({%H-}Sender: TObject);
    procedure btnAccountChangeClick({%H-}Sender: TObject);
    procedure btnAccountCreateClick({%H-}Sender: TObject);
    procedure accountdeleteClick({%H-}Sender: TObject);
    procedure Button10Click({%H-}Sender: TObject);
    procedure Button11Click({%H-}Sender: TObject);
    procedure Button12Click({%H-}Sender: TObject);
    procedure Button13Click({%H-}Sender: TObject);
    procedure templateFileChange(Sender: TObject);
    procedure internetAccessChange({%H-}Sender: TObject);
    procedure internetProxyChange({%H-}Sender: TObject);
    procedure libAddClick({%H-}Sender: TObject);
    procedure libChangeClick({%H-}Sender: TObject);
    procedure libDeleteClick({%H-}Sender: TObject);
    procedure libListSelectItem({%H-}Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure templateDefineClick({%H-}Sender: TObject);
    procedure Button2Click({%H-}Sender: TObject);
    procedure Button3Click({%H-}Sender: TObject);
    procedure Button8Click({%H-}Sender: TObject);
    procedure cancelclick({%H-}Sender: TObject);
    procedure ComboBox1Change({%H-}Sender: TObject);
    procedure edtAccountUserChange({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormResize({%H-}Sender: TObject);
    procedure FormShow({%H-}Sender: TObject);
    procedure ImageList1Change({%H-}Sender: TObject);
    procedure lblShowWarningClick({%H-}Sender: TObject);
    procedure mailaddClick({%H-}Sender: TObject);
    procedure maildelClick({%H-}Sender: TObject);
    procedure mailListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure mailsetClick({%H-}Sender: TObject);
    procedure Shape1MouseUp(Sender: TOBject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure templateListSelectionChange({%H-}Sender: TObject);
    procedure TrackBar1Change({%H-}Sender: TObject);
    procedure TrackBar2Change({%H-}Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
    currentSelectedItem: TListItem;
    currentSelectedAccount: TCustomAccountAccess;
    currentSelectedExtendType: TExtendType;
    function backSelect(const prompt: boolean):TCustomAccountAccess;
    procedure addAccount(const account: TCustomAccountAccess);
    procedure addUserLibrary(lib: TLibrary);
    procedure checkUncommitedAccountChange;
  end;

var
  optionForm: ToptionForm;

ResourceString
  rsDisabled = '!DEAKTIVIERT!';
  rsBeforeDueS = '%s Tage vor Frist';
  rsAccountExpiration = 'Die Ablaufdaten der Konten sind: ';




implementation

uses newAccountWizard_u, applicationconfig, applicationdesktopconfig, simplehtmltreeparser, androidutils, multipagetemplate, bbutils, internetaccess, simpleinternet,math, bookproperties, xquery.internals.common;

ResourceString
  rsAccountDeletion = 'Kontolöschung';
  rsAccountDeletionConfirm = 'Soll auf diesem Computer das Konto %s - %s wirklich gelöscht werden?    %sDadurch werden auch alle '
    +'gespeicherten Bücherdaten dieses Kontos gelöscht   ';
  rsTemplateDelete = 'Soll das System "%s" wirklich gelöscht werden?';
  rsInstallLibTemplate = 'Geben Sie die Internet-Adresse ein, von der ein Update heruntergeladen werden soll:';
  rsTemplateInstalled = 'Template installiert';
  rsTemplateNotFound = 'Template nicht gefunden: %s';
  rsLibNamePrompt = 'Geben Sie den Namen der Bibliothek ein';
  rsLibSystemPrompt = 'Welches Katalog-System verwendet die Bibliothek?';
  rsVariablePromptOptional = 'Es kann ein Wert für die optionale Variable "%s" gesetzt werden. (%s)';
  rsVariablePrompt = 'Das Template benötigt einen Wert für die Variable "%s". (%s)';
  rsLibConfirmSave = 'Bibliotheksdaten erstellt. Um sie zu speichern, klicken Sie auf "Bibliothek speichern"';
  rsLibDeleteConfirm = 'Wollen Sie die Bücherei %s löschen?';
  rsAccountChange = 'Kontoänderung';
  rsAccountSaveConfirm = 'Die Kontodaten für das momentan markierte Konto wurden geändert.    %sSollen sie gespeichert werden?';
  rsDidYouMeanThisAccount = 'Meinen sie dieses Konto: %s    Kontoname: %s%s    Kontonummer: %s';
  rsAutoRenewing = 'Automatische Aktualisierung:';
  rsAutoRenewAlways = '%sbei jedem Start';
  rsAutoRenewDaily = '%smaximal einmal pro Tag';
  rsAutoRenewInterval = '%smaximal alle %s Tage';
  rsWarning = 'Anzeige einer Warnung:';



{ ToptionForm }

procedure ToptionForm.addAccount(const account: TCustomAccountAccess);
begin
  with accountList.Items.Add do begin
    caption:=account.prettyName;
    SubItems.add(account.getUser());
    SubItems.add(account.passWord);
    if account.keepHistory then
      SubItems.add(rsYes)
     else
      SubItems.add(rsNo);
    if not account.enabled then
      SubItems.add(rsDisabled)
     else case account.extendType of
      etAlways: SubItems.add(rsAlways);
      etAllDepends, etSingleDepends: SubItems.add(Format(rsBeforeDueS, [IntToStr(account.extendDays)]));
      etNever: SubItems.add(rsNever);
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
      end else if prompt and (MessageDlg('VideLibri', Format(rsDidYouMeanThisAccount, [#13#10, currentLib.prettyName, #13#10,
        currentLib.getUser()]),
                mtConfirmation ,[mbYes,mbNo],0)=mrYes) then begin
        newLibIndex:=i;
        break;
      end;
    end else if (edtAccountUser.Text=currentLib.getUser()) and prompt and
                (MessageDlg('VideLibri', Format(rsDidYouMeanThisAccount, [#13#10, currentLib.prettyName, #13#10, currentLib.getUser()]),
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
 list: TList;
 temp: String;
 sl: TStringList;
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
  checkCertificates.Checked:=userConfig.ReadBool('access', 'checkCertificates', true);
  openSSLCAStore.text := userConfig.ReadString('access', 'CAPath', '');
  case userConfig.readInteger('access','internet-type',0) of
    0: internetWindows.Checked:=true;
    1: internetDirect.Checked:=true;
    2: internetProxy.Checked:=true;
  end;
  {$IFDEF WINDOWS}
  case userConfig.readInteger('access','internet-backend',0) of
    0, 1: internetAccessW32.Checked := True;
    2: internetAccessSynapse.Checked := True;
  end;
  {$else}
  internetAccessW32.Enabled := false;
  internetAccessSynapse.Checked := true;
  {$endif}
  internetAccessChange(self);
  internetProxyChange(internetProxy);

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


  //Autostartpage / Behavior
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
  cbCopyAccountLimits.Checked := userConfig.ReadBool('user','copy-limit',false);

  //Libpage
  sl := TStringList.Create;
  libraryManager.enumerateBuiltInTemplates(sl);
  libraryManager.enumerateUserTemplates(sl);
  sl.Sort();
  templateList.Items.Assign(sl);
  sl.Free;
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
  userConfig.WriteInteger('appearance','limited',colorLimited);
  userConfig.WriteInteger('appearance','timeNear',colorTimeNear);
  userConfig.WriteInteger('appearance','default',colorOK);
  userConfig.WriteInteger('appearance','history',colorOld);
  userConfig.WriteInteger('appearance','ordered',colorOrdered);
  userConfig.WriteInteger('appearance','provided',colorProvided);
  userConfig.WriteInteger('base','near-time',StrToInt(timeNearMeaning.Text));
  updateGlobalTimeCache;
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
  needRefreshListView := true; //todo
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
  userConfig.WriteBool('access', 'checkCertificates', checkCertificates.Checked);
  userConfig.WriteString('access', 'CAPath', openSSLCAStore.text);

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
  callbacks.updateAutostart(autostartAlways.Checked or autostartDepends.Checked,false);
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
  userConfig.WriteBool('user','copy-limit',cbCopyAccountLimits.Checked);

  mainForm.Refresh;
  if needRefreshListView then mainform.RefreshListView;
  ModalResult:=mrOK;
  Close;
end;

procedure ToptionForm.Button8Click(Sender: TObject);
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
 ignore(sender);
 ignore(item);
 ignore(selected);
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

procedure ToptionForm.templateListSelectionChange(Sender: TObject);
var
  temp: String;
  sl: TStringList;
begin
  temp := templateList.Items[templateList.ItemIndex];
  templateFile.Clear;
  templateFile.Text:='template';
  sl := TStringList.Create;
  libraryManager.enumerateFilesInDir(assetPath+'libraries/templates/'+temp, sl);
  libraryManager.enumerateFilesInDir(userPath+'libraries/templates/'+temp, sl);
  sl.Sort();
  templateFile.Items.Assign(sl);
  sl.free;

  templateName.text:=temp;
  templateFileChange(sender);
end;

procedure ToptionForm.TrackBar1Change(Sender: TObject);
var
  s: String;
begin
  s := rsAutoRenewing + LineEnding;
  case TrackBar1.Position of
    0: lblRefreshTIming.Caption:=Format(rsAutoRenewAlways, [s]);
    1: lblRefreshTIming.Caption:=Format(rsAutoRenewDaily, [s]);
    else lblRefreshTIming.Caption:=Format(rsAutoRenewInterval, [s, IntToStr(TrackBar1.Position)]);
  end;
  lblRefreshTIming.Left:=max(0,
                         min(notebook1.Width - lblRefreshTIming.Width,
                           TrackBar1.left+15+(TrackBar1.width-15)*(TrackBar1.Position-TrackBar1.min) div (TrackBar1.Max-TrackBar1.min)-lblRefreshTIming.Width div 2
                         ));
end;

procedure ToptionForm.TrackBar2Change(Sender: TObject);
var
  s: String;
begin
 lblShowWarning.Left:=TrackBar2.left+15+(TrackBar2.width-15)*(TrackBar2.Position-TrackBar2.min) div (TrackBar2.Max-TrackBar2.min)-lblShowWarning.Width div 2;
 s := rsWarning + LineEnding;
 case TrackBar2.Position of
   0: lblShowWarning.Caption:=Format(rsAutoRenewAlways, [s]);
   1: lblShowWarning.Caption:=Format(rsAutoRenewDaily, [s]);
   else lblShowWarning.Caption:=Format(rsAutoRenewInterval, [s, IntToStr(TrackBar2.Position)]);
 end;

end;

procedure ToptionForm.accountListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  lib: TLibrary;
begin
  if not Selected then exit;
  if TCustomAccountAccess(item.data) <> currentSelectedAccount then
     checkUncommitedAccountChange;
  currentSelectedItem := item;
  currentSelectedAccount:=TCustomAccountAccess(item.data);
  lib := currentSelectedAccount.getLibrary();
  lblAccountLibrary.Caption:=lib.prettyNameLong;
  edtAccountPrettyName.Text:=item.Caption;
  edtAccountUser.Text:=item.SubItems[0];
  edtAccountPass.Text:=item.SubItems[1];
  ckbAccountHistory.Checked:=item.SubItems[2]='ja';
  ckbAccountDisabled.Checked:=not currentSelectedAccount.enabled;
  if currentSelectedAccount.getLibrary().canModifySingleBooks then begin
    cmbAccountExtend.items.Text:=Format(rsRenewOptions, [#13#10, #13#10, #13#10]);
    cmbAccountExtend.ItemIndex:=longint(TCustomAccountAccess(item.data).extendType);
  end else begin
    cmbAccountExtend.items.Text:=Format(rsRenewOptionsNoSingle, [#13#10, #13#10]);
    case currentSelectedAccount.extendType of
      etAlways: cmbAccountExtend.ItemIndex:=0;
      etAllDepends,etSingleDepends: cmbAccountExtend.ItemIndex:=1;
      etNever: cmbAccountExtend.ItemIndex:=2;
    end;
  end;
  cmbAccountExtend.OnSelect(cmbAccountExtend);
  edtAccountExtendDays.Text:=inttostr(currentSelectedAccount.extendDays);

  accountType.ItemIndex := currentSelectedAccount.accountType - 1;
  accountType.Visible := lib.segregatedAccounts ;
  lblAccountType.Visible := lib.segregatedAccounts ;

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
  if not Assigned(currentSelectedItem) or not assigned(currentSelectedAccount) then exit;

  item:=currentSelectedItem;
  lib:=currentSelectedAccount;
  if edtAccountPass.text<>item.SubItems[1] then
    lib.password:=edtAccountPass.text;
  if edtAccountPrettyName.text<>item.Caption then
    lib.prettyName:=edtAccountPrettyName.text;
  if ckbAccountHistory.Checked<>(item.SubItems[2]=rsYes) then
    lib.keepHistory:=ckbAccountHistory.Checked;

  lib.extendType:=currentSelectedExtendType;
  lib.extendDays:=StrToInt(edtAccountExtendDays.Text);
  lib.enabled:=not ckbAccountDisabled.Checked;
  if not lib.enabled then item.SubItems[3]:=rsDisabled
  else case currentSelectedExtendType of
    etAlways: item.SubItems[3]:=rsAlways;
    etAllDepends, etSingleDepends: item.SubItems[3]:=Format(rsBeforeDueS, [IntToStr(currentSelectedAccount.extendDays)]);
    etNever: item.SubItems[3]:=rsNever;
  end;
  if accountType.Visible then
    lib.accountType := accountType.ItemIndex + 1;
  if edtAccountUser.text<>item.SubItems[0] then begin
    lib.changeUser(edtAccountUser.text);
    accounts.Strings[item.Index]:=lib.getPlusEncodedID();
    accounts.save;
    mainForm.refreshAccountGUIElements();
    mainForm.RefreshListView;
  end else lib.saveConfig();
  item.Caption:=edtAccountPrettyName.text;
  item.SubItems[0]:=edtAccountUser.Text;
  item.SubItems[1]:=edtAccountPass.Text;
  if ckbAccountHistory.Checked then
    item.SubItems[2]:=rsYes
   else
    item.SubItems[2]:=rsNo;
end;

procedure ToptionForm.btnAccountCreateClick(Sender: TObject);
var newAccount:TnewAccountWizard;
    i:integer;
begin
  checkUncommitedAccountChange;

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
  if MessageDlg(rsAccountDeletion, Format(rsAccountDeletionConfirm, [edtAccountPrettyName.text, edtAccountUser.text, #13#10]),
                mtConfirmation ,[mbYes,mbNo],0)=mrYes then begin
    accounts.Delete(accountList.Selected.Index);
    accounts.save;
    accountList.Selected.Delete;
    selLib.remove();
    mainForm.refreshAccountGUIElements();
    mainForm.RefreshListView;
  end;
end;

procedure ToptionForm.Button10Click(Sender: TObject);
begin
  PageControlLibs.ActivePageIndex := 1;
end;

procedure ToptionForm.Button11Click(Sender: TObject);
begin
  if confirm(format(rsTemplateDelete, [templateName.Text])) then begin
    DeleteFile(userPath+'libraries/templates/'+templateName.Text+'/template');
    RemoveDir(userPath+'libraries/templates/'+templateName.Text);
    if (templateList.Items.IndexOf(templateName.text) >= 0) and not DirectoryExists(assetPath+'libraries/templates/'+templateName.text) then
      templateList.Items.Delete(templateList.Items.IndexOf(templateName.text));
  end;
end;

procedure ToptionForm.Button12Click(Sender: TObject);
begin
  PageControlLibs.ActivePageIndex := 2;
end;

procedure ToptionForm.Button13Click(Sender: TObject);
  procedure downloadAndInstallTemplate;
  var
    lib: TLibrary;
    url: String;
  begin
    url := 'https://';
    if not InputQuery('VideLibri', rsInstallLibTemplate, url) then exit;
    try
      lib := libraryManager.downloadAndInstallUserLibrary(url);
      if lib <> nil then begin
        addUserLibrary(lib);
        if (lib.template <> nil) and (templateList.Items.IndexOf(lib.template.name) < 0) then
          templateList.Items.add(lib.template.name);;
      end;

      ShowMessage(rsTemplateInstalled)
    except on e: EInternetException do
      ShowMessage(Format(rsTemplateNotFound, [e.Message]));
    on e: ELibraryException do
      ShowMessage(Format(rsTemplateNotFound, [e.Message]));
    end;
  end;
begin
  downloadAndInstallTemplate;
end;

procedure ToptionForm.templateFileChange(Sender: TObject);
var
  temp: String;
begin
  temp := templateList.Items[templateList.ItemIndex];
  temp := 'libraries/templates/'+temp+'/'+templateFile.Text;
  try
    templatexml.Lines.text := assetFileAsString(temp);
  except
    on e: Exception do ;
  end;
end;

procedure ToptionForm.internetAccessChange(Sender: TObject);
begin
  internetWindows.Enabled := internetAccessW32.Checked;
  if internetWindows.Checked and not internetWindows.Enabled then internetDirect.Checked := true;
  checkCertificates.Checked := checkCertificates.Enabled; //and userConfig.ReadBool('access', 'checkCertificates', true)
  openSSLCAStore.Enabled := internetAccessSynapse.Checked;
  openSSLCAStoreLabel.Enabled := internetAccessSynapse.Checked;
end;

procedure ToptionForm.internetProxyChange(Sender: TObject);
begin
 proxyHTTPName.Enabled := internetProxy.Checked;
 proxyHTTPSname.Enabled := internetProxy.Checked;
 proxySocksName.Enabled := internetProxy.Checked;
 proxyHTTPPort.Enabled := internetProxy.Checked;
 proxyHTTPSport.Enabled := internetProxy.Checked;
 proxySocksPort.Enabled := internetProxy.Checked;
end;

procedure ToptionForm.libAddClick(Sender: TObject);

var
  libname: String;
  system: String;
  result: String;
  i, systemIdx: Integer;
  template: TMultiPageTemplate;
  meta: TTemplateActionMeta;
  vari: String;
  desc: String;
begin

  libname := '';
  if not InputQuery('VideLibri', rsLibNamePrompt, libname) then exit;
  systemIdx := InputList(rsLibSystemPrompt, templateList.Items);
  if systemIdx < 0 then exit;
  system := templateList.Items[systemIdx];

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
        desc := format(rsVariablePromptOptional, [meta.variables[i].name, meta.variables[i].description]);
        if not InputQuery('VideLibri', desc, vari) then exit;
        if vari <> meta.variables[i].def then
          result += '  <variable name="'+xmlStrEscape(meta.variables[i].name)+'" value="'+xmlStrEscape(vari)+'"/>'+LineEnding;
      end
      else begin
        desc := format(rsVariablePrompt, [meta.variables[i].name, meta.variables[i].description]);
        if not InputQuery('VideLibri', desc, vari) then exit;
        result += '  <variable name="'+xmlStrEscape(meta.variables[i].name)+'" value="'+xmlStrEscape(vari)+'"/>'+LineEnding;
      end;
    end;
  end;
  result += '  <testing-search value="yes"/>'+LineEnding;
  result += '  <testing-account value="yes"/>'+LineEnding;

  result += '</library>' + LineEnding;

  ShowMessage(rsLibConfirmSave);

  libxml.text := result;
end;

procedure ToptionForm.libChangeClick(Sender: TObject);
var
  trueId: String;
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
  if not confirm(Format(rsLibDeleteConfirm, [libNameEdit.Text])) then exit;
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
begin
  if not DirectoryExists(userPath+'libraries/templates/'+templateName.Text) then
    ForceDirectories(userPath+'libraries/templates/'+templateName.Text+'');
  templatexml.Lines.SaveToFile(userPath+'libraries/templates/'+templateName.Text+'/'+templateFile.Text);
  if templateList.Items.IndexOf(templateName.Text) < 0 then
    templateList.Items.add(templateName.Text);
  templateList.Text := templateName.Text;
  if templateFile.Items.IndexOf(templateFile.Text) < 0 then
    templateFile.Items.Add(templateFile.Text);


  libraryManager.reloadTemplate(templateName.Text);
end;

procedure ToptionForm.checkUncommitedAccountChange;
var
  account: TCustomAccountAccess;
begin
  if (accountList.Selected = nil) or (currentSelectedItem = nil) then exit;
  account := currentSelectedAccount;
  if account = nil then exit;

  if    (edtAccountPrettyName.Text<>account.prettyName)
     or (edtAccountUser.Text <> account.getUser())
     or (edtAccountPass.Text <> account.passWord)
     or (ckbAccountHistory.Checked <> account.keepHistory)
     or (currentSelectedExtendType <> account.extendType)
     or ( (currentSelectedExtendType in [etAllDepends,etSingleDepends])
          and (StrToInt(edtAccountExtendDays.text)<> account.extendDays) )
     or (accountType.Visible and ( accountType.ItemIndex <> currentSelectedAccount.accountType - 1) )
  then
    if MessageDlg(rsAccountChange, Format(rsAccountSaveConfirm, [#13#10]), mtConfirmation,mbYesNo,0) = mrYes then
      btnAccountChange.Click;
  currentSelectedItem := nil;
  currentSelectedAccount := nil;
end;

procedure ToptionForm.Button2Click(Sender: TObject);
begin
  if Notebook1.ActivePageComponent=pageAccount then
    checkUncommitedAccountChange;
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

