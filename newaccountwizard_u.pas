unit newAccountWizard_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Grids,bookwatchmain,libraryParser;

type

  { TnewAccountWizard }

  TnewAccountWizard = class(TForm)
    cancelBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    identificarionInvalid: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    lblWarning: TLabel;
    nextbtn: TButton;
    back: TButton;
    accountName: TEdit;
    accountPass: TEdit;
    accountPrettyName: TEdit;
    extendDaysEdit: TEdit;
    Label4: TLabel;
    passLabel: TLabel;
    Label6: TLabel;
    extendDaysLbl: TLabel;
    extendDaysLbl2: TLabel;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    lastPage: TPage;
    Panel1: TPanel;
    libraryList: TRadioGroup;
    extendTypeRG: TRadioGroup;
    RadioButton1: TRadioButton;
    saveHistory: TRadioButton;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    procedure accountNameChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Notebook1PageChanged(Sender: TObject);
    procedure Page2BeforeShow(ASender: TObject; ANewPage: TPage;
     ANewIndex: Integer);
    procedure Page4BeforeShow(ASender: TObject; ANewPage: TPage;
     ANewIndex: Integer);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    selectedLibrary: TLibrary;
    procedure selectCurrentPage;
  end;

var
  newAccountWizard: TnewAccountWizard;

implementation
uses applicationconfig,libraryaccess,internetAccess;
{ TnewAccountWizard }


procedure TnewAccountWizard.FormCreate(Sender: TObject);
begin
  identificarionInvalid.Caption:='';
  StringGrid1.ScrollBars:=ssNone;
  StringGrid1.ColWidths[1]:=StringGrid1.ClientWidth-StringGrid1.ColWidths[0];
  libraryList.Items.text:=libraryManager.enumeratePrettyLongNames;
  libraryList.Items.Add('sollte nicht angezeigt werden');
  libraryList.Items.Delete(libraryList.Items.Count-1);
  selectedLibrary:=libraryManager.getLibraryFromEnumeration(0);
  //if accountIDs.Count =0 then FormStyle:=fsStayOnTop;
end;

procedure TnewAccountWizard.FormShow(Sender: TObject);
begin

end;

procedure TnewAccountWizard.Notebook1PageChanged(Sender: TObject);
begin
  if Notebook1.ActivePageComponent=lastPage then begin
    StringGrid1.Cells[1,0]:=libraryList.Items[libraryList.ItemIndex];
    StringGrid1.Cells[1,1]:=accountName.text;
    StringGrid1.Cells[1,2]:=accountPass.text;
    StringGrid1.Cells[1,3]:=accountPrettyName.text;
    case TExtendType(extendTypeRG.tag) of
      etAlways: StringGrid1.Cells[1,4]:='immer, wenn möglich';
      etAllDepends:
        if libraryList.ItemIndex=0 then StringGrid1.Cells[1,4]:='immer '+extendDaysEdit.text+' Tage vor Ende der Leihfrist'
        else StringGrid1.Cells[1,4]:='immer alle Medien '+extendDaysEdit.text+' Tage vor Ende der Leihfrist';
      etSingleDepends: StringGrid1.Cells[1,4]:='immer einzeln '+extendDaysEdit.text+' Tage vor Ende der Leihfrist';
      etNever: StringGrid1.Cells[1,4]:='niemals';
    end;
    if saveHistory.Checked then
      StringGrid1.Cells[1,5]:='ja'
     else
      StringGrid1.Cells[1,5]:='nein' ;
    nextbtn.Caption:='&Erstellen >';
  end else nextbtn.Caption:='&Weiter >';
  //fix lcl bug 14877
  libraryList.ReAlign;
  extendTypeRG.ReAlign;
end;

procedure TnewAccountWizard.Page2BeforeShow(ASender: TObject; ANewPage: TPage;
 ANewIndex: Integer);
begin

end;

procedure TnewAccountWizard.Page4BeforeShow(ASender: TObject; ANewPage: TPage;
 ANewIndex: Integer);
begin

end;

procedure TnewAccountWizard.RadioGroup1Click(Sender: TObject);
begin
  selectedLibrary:=libraryManager.getLibraryFromEnumeration(libraryList.ItemIndex);
 { case selectedLibrary.passwordType of
    ptBirthday: passLabel.Caption:='Geburtstdatum: ';
    else passLabel.Caption:='Passwort: ';
  end;                         }
  StringGrid1.Cells[0,2]:=passLabel.Caption;
  if selectedLibrary.maxRenewCount=0 then begin
    extendTypeRG.Items.Text:='niemals                       ';
    application.ProcessMessages;
    extendTypeRG.ItemIndex:=0;
  end else begin
    if selectedLibrary.canModifySingleBooks then
      extendTypeRG.Items.text:='immer, wenn möglich'#13#10'alle, wenn nötig     '#13#10'einzeln, wenn nötig    '#13#10'niemals'
     else
      extendTypeRG.Items.Text:='immer, wenn möglich'#13#10'immer, wenn nötig    '#13#10'niemals                       ';
    application.ProcessMessages;
    if selectedLibrary.maxRenewCount=-1 then extendTypeRG.ItemIndex:=0
    else if selectedLibrary.canModifySingleBooks then extendTypeRG.ItemIndex:=2
    else extendTypeRG.ItemIndex:=1;
  end;
  extendTypeRG.OnClick(extendTypeRG);
end;

procedure TnewAccountWizard.RadioGroup2Click(Sender: TObject);
var extendType: TExtendType;
begin
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
end;

procedure TnewAccountWizard.Timer1Timer(Sender: TObject);
begin
Timer1.Enabled:=false;
Notebook1.PageIndex:=1;
Notebook1.PageIndex:=0;
Notebook1.Height:=Notebook1.Height+1;Notebook1.Height:=Notebook1.Height-1;
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
begin
  if Notebook1.PageIndex=Notebook1.PageCount-1 then begin
    newLib:=mainform.addAccount(selectedLibrary.id,accountPrettyName.text,accountName.text,accountPass.text,
                        TExtendType(extendTypeRG.Tag),strtoint(extendDaysEdit.text),saveHistory.checked);
    if MessageDlg('Daten laden?',
                  'Das Konto '+accountPrettyName.text+' wurde erstellt.'#13#10'Sollen jetzt die Mediendaten heruntergeladen werden?',
                  mtConfirmation ,[mbYes,mbNo],0)=mrYes then
      try
        updateAccountBookData(newLib,false,false,false);
      except
        on EInternetException do
          if length(errorMessageList)=0 then
            showmessage('Internetverbindung fehlgeschlagen, bitte versuchen Sie es später noch einmal.')
           else
            showErrorMessages;
        on Exception do
          showErrorMessages();
      end;

    if MessageDlg('Noch ein Konto',
                  'Wollen Sie noch ein anderes Konto registrieren?',
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
end;

procedure TnewAccountWizard.Button1Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
  close;
end;

procedure TnewAccountWizard.accountNameChange(Sender: TObject);
begin
  accountPrettyName.text:=accountName.text;
  identificarionInvalid.Caption:='';
  if (selectedLibrary<>nil) and (selectedLibrary.template<>nil) then
    if (accountPass.Text<>'') and (not selectedLibrary.usernameRegEx.Exec(accountName.Text)) then
      identificarionInvalid.Caption:='Die eingegebe Kontonummer ist wahrscheinlich ungültig '
    else if (accountPass.Text<>'') and (not selectedLibrary.passwordRegEx.Exec(accountPass.Text)) then
      identificarionInvalid.Caption:='Das eingegeben '+copy(passLabel.Caption,1,length(passLabel.Caption)-1)+' ist wahrscheinlich ungültig';
end;

procedure TnewAccountWizard.Button3Click(Sender: TObject);
begin
  Notebook1.PageIndex:=Notebook1.PageIndex-1;
  selectCurrentPage;
end;

procedure TnewAccountWizard.FormActivate(Sender: TObject);
begin
end;

initialization
  {$I newaccountwizard_u.lrs}

end.

