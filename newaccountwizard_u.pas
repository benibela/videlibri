unit newAccountWizard_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Grids,bookwatchmain,libraryParser, libraryListView, TreeListView;

type

  { TnewAccountWizard }

  TnewAccountWizard = class(TForm)
    newlibbtn: TButton;
    cancelBtn: TButton;
    accountType: TComboBox;
    LabelSelectedLib: TLabel;
    LabelAccountType: TLabel;
    label2Account: TLabel;
    label4autoextend: TLabel;
    Label8: TLabel;
    identificarionInvalid: TLabel;
    Label5: TLabel;
    Label1ChooseLib: TLabel;
    lblWarning: TLabel;
    nextbtn: TButton;
    back: TButton;
    accountName: TEdit;
    accountPass: TEdit;
    accountPrettyName: TEdit;
    extendDaysEdit: TEdit;
    Label4: TLabel;
    Panel2: TPanel;
    passLabel: TLabel;
    label3displayName: TLabel;
    extendDaysLbl: TLabel;
    extendDaysLbl2: TLabel;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    lastPage: TPage;
    Panel1: TPanel;
    extendTypeRG: TRadioGroup;
    RadioButton1: TRadioButton;
    saveHistory: TRadioButton;
    StringGrid1: TPanel;
    Timer1: TTimer;
    procedure accountNameChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure libsSelect(sender: TObject; item: TTreeListItem);
    procedure locationListChange(Sender: TObject);
    procedure newlibbtnClick(Sender: TObject);
    procedure Notebook1PageChanged(Sender: TObject);
    procedure Page2BeforeShow(ASender: TObject; ANewPage: TPage;
     ANewIndex: Integer);
    procedure Page2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Page4BeforeShow(ASender: TObject; ANewPage: TPage;
     ANewIndex: Integer);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    libs: TLibraryListView;
    selectedExtendTypeItemIndex: integer;
    finalPage: TTreeListView;
    procedure selectCurrentPage;
  end;

var
  newAccountWizard: TnewAccountWizard;

implementation
uses applicationconfig,applicationdesktopconfig, libraryaccess,internetAccess,LCLProc,bbdebugtools, options;
{ TnewAccountWizard }


resourcestring
  rsName = 'Name';
  rsValue = 'Value';
  rsLibrary = 'Bibliothek:';
  rsAccountnumber = 'Kartennummer:';
  rsPassword = 'Passwort:';
  rsPrettyName = 'angezeigter Name:';
  rsRenewing = 'Verlängerung:';
  rsHistory = 'History:';
  rsNoLibChosen = 'Keine Bücherei ausgewählt!';
  rsRenewAlways = 'immer, wenn möglich';
  rsRenewAllTogether = 'immer alle Medien %s Tage vor Ende der Leihfrist';
  rsRenewSingle = 'immer einzeln %s Tage vor Ende der Leihfrist';
  rsBtnNext = '&Weiter >';
  rsNoLibChosen2 = 'Es muss eine Bücherei ausgewählt werden.';
  rsFinishedRefreshConfirm = 'Das Konto %s wurde erstellt.%sSollen jetzt die Mediendaten heruntergeladen werden?';
  rsConnectionFailedTryAgain = 'Internetverbindung fehlgeschlagen, bitte versuchen Sie es später noch einmal.';
  rsAnotherAccountConfirm = 'Wollen Sie noch ein anderes Konto registrieren?';
  rsAccountGuessInvalid = 'Die eingegebe Kontonummer ist wahrscheinlich ungültig ';
  rsPasswordGuessInvalid = 'Das eingegeben %s ist wahrscheinlich ungültig';
  rsBtnCreate = '&Erstellen >';


procedure TnewAccountWizard.FormCreate(Sender: TObject);
begin
  identificarionInvalid.Caption:='';
  finalPage := TTreeListView.Create(StringGrid1);
  finalPage.Parent:=StringGrid1;
  finalPage.Align:=alClient;
  finalPage.Columns.Clear;
  with finalPage.Columns.Add() do begin Width:=175; Text:= rsName; end;
  with finalPage.Columns.Add() do begin Width:=300; Text:= rsValue; end;
//  with finalPage.Columns.Add() do begin Width:=300; Text:= 'Value';end;
  finalPage.Items.Add(rsLibrary);
  finalPage.Items.Add(rsAccountnumber);
  finalPage.Items.Add(rsPassword);
  finalPage.Items.Add(rsPrettyName);
  finalPage.Items.Add(rsRenewing);
  finalPage.Items.Add(rsHistory);
  finalPage.HeaderVisible:=false;
  finalPage.Scrollbars:=ssAutoHorizontal;
  finalPage.VerticalLineMode:=lmNone;
  finalPage.Visible:=true;
  finalPage.RowHeight:=finalPage.RowHeight+10;
  finalPage.Enabled:=false;


  libs := TLibraryListView.create(Panel2);
  libs.Align:=alClient;
  libs.OnSelect:=@libsSelect;
  libs.OnDblClick := @Button2Click;
  libs.Parent := panel2;
  //if accountIDs.Count =0 then FormStyle:=fsStayOnTop;

  {$ifdef android}
  Label1ChooseLib.WordWrap:=false;
  DebugLn('1');
  Label1ChooseLib.Caption := 'Wählen Sie Ihre Bücherei:';
  DebugLn('2');
  label2Account.Caption:='Geben Sie nun Ihre Kontodaten ein:'+LineEnding+'  (nicht nötig für Suche)';
  DebugLn('3');
  label2Account.WordWrap:=false;
  DebugLn('4:'+inttohex(ptruint(label3displayName),8 ));

  label3displayName.Caption:='Unter welchem Namen soll das Konto angezeigt werden?';
  DebugLn('5');
  label3displayName.WordWrap:=false;
  DebugLn('6');
  label4autoextend.WordWrap:=false;
  DebugLn('7');
  label4autoextend.Caption:='Sollen die Medien automatisch verlängert werden?';
  DebugLn('8');
  label5.WordWrap:=false;
  DebugLn('9');
  label5.Caption:='Sind alle Angaben korrekt?';
  {$endif}

  globalOnFormCreate(self);
end;

procedure TnewAccountWizard.FormShow(Sender: TObject);
begin

end;

procedure TnewAccountWizard.libsSelect(sender: TObject; item: TTreeListItem);
var
  selectedLibrary: TLibrary;
begin
  selectedLibrary:=libs.selectedLibrary;
  if selectedLibrary = nil then exit;

 { case selectedLibrary.passwordType of
    ptBirthday: passLabel.Caption:='Geburtstdatum: ';
    else passLabel.Caption:='Passwort: ';
  end;                         }
  finalPage.Items[2].RecordItemsText[0]:=passLabel.Caption;
  if selectedLibrary.maxRenewCount=0 then begin
    extendTypeRG.Items.Text:='niemals                       ';
    selectedExtendTypeItemIndex:=0;
  end else begin
    if selectedLibrary.canModifySingleBooks then
      extendTypeRG.Items.text:=format(rsRenewOptions,[LineEnding,LineEnding,LineEnding])
     else
      extendTypeRG.Items.Text:=format(rsRenewOptionsNoSingle,[LineEnding,LineEnding]) ;
    //we need to set a default itemindex, but we cannot change extendTypeRG.itemIndex
    //till all pending events (probably caused by changing items.text) have been processed.
    //We cannot call application.ProcessMessages or the listview starts flickering
    if selectedLibrary.canModifySingleBooks then selectedExtendTypeItemIndex:=2
    else selectedExtendTypeItemIndex:=1;
  end;
  extendTypeRG.OnClick(extendTypeRG);
  LabelSelectedLib.Caption := selectedLibrary.prettyNameLong;
end;

procedure TnewAccountWizard.locationListChange(Sender: TObject);
begin
end;

procedure TnewAccountWizard.newlibbtnClick(Sender: TObject);
var optionsForm:ToptionForm;
begin
  optionsForm:=ToptionForm.Create(nil);
  optionsForm.SpeedButtonLibraryConfig.Down := true;
  optionsForm.SpeedButtonLibraryConfig.Click;
  try
     optionsForm.ShowModal;
  finally
     optionsForm.free;
  end;
end;

procedure TnewAccountWizard.Notebook1PageChanged(Sender: TObject);
begin
  if Notebook1.ActivePageComponent=lastPage then begin
    if libs.selectedLibrary = nil then begin
      finalPage.Items[0].RecordItemsText[1]:=rsNoLibChosen;
      exit;
    end;
    finalPage.Items[0].RecordItemsText[1]:=libs.selectedLibrary.prettyNameLong ;//libraryList.Items[libraryList.ItemIndex];
    finalPage.Items[1].RecordItemsText[1]:=accountName.text;
    finalPage.Items[2].RecordItemsText[1]:=accountPass.text;
    finalPage.Items[3].RecordItemsText[1]:=accountPrettyName.text;
    case TExtendType(extendTypeRG.tag) of
      etAlways: finalPage.Items[4].RecordItemsText[1]:=rsRenewAlways;
      etAllDepends:
        {if libraryList.ItemIndex=0 then StringGrid1.Cells[1,4]:='immer '+extendDaysEdit.text+' Tage vor Ende der Leihfrist'
        else }finalPage.Items[4].RecordItemsText[1]:=Format(rsRenewAllTogether, [extendDaysEdit.text]);
      etSingleDepends: finalPage.Items[4].RecordItemsText[1]:=Format(rsRenewSingle, [extendDaysEdit.text]);
      etNever: finalPage.Items[4].RecordItemsText[1]:=rsNever;
    end;
    if saveHistory.Checked then
      finalPage.Items[5].RecordItemsText[1]:=rsYes
     else
      finalPage.Items[5].RecordItemsText[1]:=rsNo ;
    finalPage.ColumnsAutoSize;
    nextbtn.Caption:=rsBtnCreate;
    finalPage.Repaint;
  end else nextbtn.Caption:=rsBtnNext;
  newlibbtn.visible := Notebook1.PageIndex = 0;
  back.Enabled := Notebook1.PageIndex > 0;
  //fix lcl bug 14877
  Panel2.ReAlign;
  extendTypeRG.ReAlign;
end;

procedure TnewAccountWizard.Page2BeforeShow(ASender: TObject; ANewPage: TPage;
 ANewIndex: Integer);
begin

end;

procedure TnewAccountWizard.Page2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TnewAccountWizard.Page4BeforeShow(ASender: TObject; ANewPage: TPage;
 ANewIndex: Integer);
begin

end;

procedure TnewAccountWizard.RadioGroup1Click(Sender: TObject);
begin
end;

procedure TnewAccountWizard.RadioGroup2Click(Sender: TObject);
var extendType: TExtendType;
  selectedLibrary: TLibrary;
begin
  selectedLibrary:=libs.selectedLibrary;
  if selectedLibrary = nil then exit;
  extendType:=TExtendType(extendTypeRG.ItemIndex);
  if (not selectedLibrary.canModifySingleBooks) and (extendType=etSingleDepends) then
    extendType:=etNever;
  if selectedLibrary.maxRenewCount=0 then
    extendType:=etNever;
  extendTypeRG.tag:=integer(extendType);
  lblWarning.Visible:=(selectedLibrary.maxRenewCount>0) and (extendType=etAlways);
  //maxExtendCount.Visible:=lblWarning.Visible;
  //maxExtendCount.Caption:=IntToStr(selectedLibrary.maxExtendCount);

  extendDaysLbl.Visible:=extendType in [etSingleDepends,etAllDepends];
  extendDaysLbl2.Visible:=extendType in [etSingleDepends,etAllDepends];
  extendDaysEdit.Visible:=extendType in [etSingleDepends,etAllDepends];
  if extendTypeRG.ItemIndex >= 0 then
    selectedExtendTypeItemIndex := extendTypeRG.ItemIndex;
end;

procedure TnewAccountWizard.Timer1Timer(Sender: TObject);
{$ifdef android}
var
  panelBorder: Integer;
{$endif}
begin
Timer1.Enabled:=false;
Notebook1.PageIndex:=1;
Notebook1.PageIndex:=0;
Notebook1.Height:=Notebook1.Height+1;Notebook1.Height:=Notebook1.Height-1;
{$ifdef android}
//debugln(inttostr(Width)+' '+inttostr(height)+' / '+inttostr(screen.Height));
height := screen.Height;
panel1.height := libs.RowHeight; //layout corrections, screwed up by lcl customdrawn
panelBorder := panel2.Left;
panel2.Anchors:=[aktop, akLeft];
panel2.Width := panel2.Parent.Width - 2 * panelborder;
panel2.Height := Panel2.Parent.Height - panel2.Top - panelBorder;

log(IntToStr(accountPass.Width) + ' '+ IntToStr(accountPass.parent.Width)+ ' '+IntToStr(accountPass.parent.Width));
log(IntToStr(accountPass.clientWidth) + ' '+ IntToStr(accountPass.parent.clientWidth)+ ' '+IntToStr(accountPass.parent.clientWidth));
log(inttostr(accountPass.left));

accountName.Anchors:=[akLeft, akTop];
accountName.Width:=accountName.parent.Width - accountName.Left - 400;
accountPass.Anchors:=[akLeft, akTop];
accountPass.Width:=accountPass.parent.Width - accountPass.Left - 400 {?? <- tested on tablet ui};

{$endif}
log(IntToStr(height) + ' '+ IntToStr(Notebook1.Height)+ ' '+IntToStr(page1.Height)+' '+IntToStr(panel2.height)+  ' '+ IntToStr(libs.Height));
end;

procedure TnewAccountWizard.selectCurrentPage;
var temp:TFPList;
begin
  temp:=TFPList.create;
  Notebook1.Page[Notebook1.PageIndex].GetTabOrderList(temp);
  if temp.count>0 then
    TWinControl(temp[0]).setFocus;
  temp.free;
  Notebook1PageChanged(Notebook1);
end;
procedure TnewAccountWizard.Button2Click(Sender: TObject);
//const libraryIDs:array[0..1] of string=('001','200');
var newLib:TCustomAccountAccess;
  selectedLibrary: TLibrary;
  accType: Integer;
begin
  selectedLibrary:=libs.selectedLibrary;
  if selectedLibrary = nil then begin
    ShowMessage(rsNoLibChosen2);
    exit;
  end;

  if Notebook1.PageIndex = 1 then
    extendTypeRG.ItemIndex := selectedExtendTypeItemIndex;

  if Notebook1.PageIndex=Notebook1.PageCount-1 then begin
    if accountType.Visible then accType := accountType.ItemIndex + 1
    else accType := 0;
    newLib:=accounts.add(selectedLibrary.id,accountPrettyName.text,accountName.text,accountPass.text,
                        TExtendType(extendTypeRG.Tag),strtoint(extendDaysEdit.text),saveHistory.checked, accType);
    if MessageDlg('VideLibri',
                  Format(rsFinishedRefreshConfirm, [accountPrettyName.text, #13#10]),
                  mtConfirmation ,[mbYes,mbNo],0)=mrYes then
      try
        updateAccountBookData(newLib,false,false,false);
      except
        on EInternetException do
          if length(errorMessageList)=0 then
            showmessage(rsConnectionFailedTryAgain)
           else
            showErrorMessages;
        on Exception do
          showErrorMessages();
      end;

    if MessageDlg('VideLibri',
                  rsAnotherAccountConfirm,
                  mtConfirmation ,[mbYes,mbNo],0)=mrYes then begin
      Notebook1.PageIndex:=0;
      accountName.text:='';
      accountPass.text:='';
      accountPrettyName.text:='';
    end else cancelBtn.Click;
  end else begin
    Notebook1.PageIndex:=Notebook1.PageIndex+1;
    selectCurrentPage;
  end;
  Notebook1.Height:=Notebook1.Height+1; Notebook1.Height:=Notebook1.Height-1;
  {$ifdef android}
  Notebook1.ReAlign;
  for i := 0 to Notebook1.PageCount - 1 do
    Notebook1.Page[i].Visible:=false ;
  Notebook1.Page[Notebook1.PageIndex].Enabled:=false;
  Notebook1.Page[Notebook1.PageIndex].Visible:=true;
  Notebook1.Page[Notebook1.PageIndex].Enabled:=true;
//  debugln(BoolToStr(accountName.HandleObjectShouldBeVisible)+ ' '+accountName.ControlAtPos(point(x,y), false, true).Name);
  debugln(BoolToStr(accountName.HandleObjectShouldBeVisible)+ ' '+BoolToStr(accountName.Enabled)+ ' '+booltostr(Page2.Enabled));
  debugln(inttostr(accountName.Top)+' '+inttostr(page2.Top)+' '+inttostr(Notebook1.Top)+' '+inttostr(Top));
   {$endif}


  LabelAccountType.Visible := selectedLibrary.segregatedAccounts;
  accountType.Visible := selectedLibrary.segregatedAccounts;
end;

procedure TnewAccountWizard.Button1Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
  close;
end;

procedure TnewAccountWizard.accountNameChange(Sender: TObject);
var
  selectedLibrary: TLibrary;
begin
  selectedLibrary:=libs.selectedLibrary;
  if selectedLibrary = nil then exit;
  accountPrettyName.text:=accountName.text;
  identificarionInvalid.Caption:='';
  if (selectedLibrary<>nil) and (selectedLibrary.template<>nil) then
    if (accountPass.Text<>'') and (not selectedLibrary.usernameRegEx.Exec(accountName.Text)) then
      identificarionInvalid.Caption:=rsAccountGuessInvalid
    else if (accountPass.Text<>'') and (not selectedLibrary.passwordRegEx.Exec(accountPass.Text)) then
      identificarionInvalid.Caption:=Format(rsPasswordGuessInvalid, [copy(passLabel.Caption, 1, length(passLabel.Caption)-1)]);
end;

procedure TnewAccountWizard.Button3Click(Sender: TObject);
begin
  if Notebook1.PageIndex = 0 then exit;
  Notebook1.PageIndex:=Notebook1.PageIndex-1;
  selectCurrentPage;
end;

procedure TnewAccountWizard.FormActivate(Sender: TObject);
begin
end;

initialization
  {$I newaccountwizard_u.lrs}

end.


