unit exportxml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Themes, Buttons, TreeListView,
  simplehtmltreeparser, applicationconfig,libraryParser;

type
  TXMLExportFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    TreeListView1: TTreeListView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TreeListView1ClickAtRecordItem(sender: TObject; recorditem: TTreeListRecordItem);
    procedure TreeListView1CustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp; recordItem: TTreeListRecordItem;
      var defaultDraw: Boolean);
  private
    { private declarations }
  public
    { public declarations }
    currentMode: integer;
    importParser: TTreeParser;
    importAccounts: array of string;
    importFlags: array of TExportImportFlags;
    procedure updateMixedHeader(col: integer);
    procedure viewCurrentAccounts;
  end;

var
  XMLExportFrm: TXMLExportFrm;

implementation
uses strutils,bookWatchMain,libraryAccess;

const COLUMN_CURRENT = 1;
      COLUMN_HISTORY = 2;
      COLUMN_CONFIG = 3;
      COLUMN_PASSWORD = COLUMN_CONFIG + 1;

      CHECKBOX_CHECKED = 'X';
      CHECKBOX_DISABLED = '-';
      CHECKBOX_UNCHECKED = '';
      CHECKBOX_MIXED = 'M';
      CHECKBOX_HIDDEN = 'h';

      MODE_EXPORT = 0;
      MODE_IMPORT = 1;

procedure TXMLExportFrm.FormCreate(Sender: TObject);
begin
  currentMode := MODE_IMPORT;
end;

procedure TXMLExportFrm.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TXMLExportFrm.Button1Click(Sender: TObject);
begin
  case currentMode of
    MODE_EXPORT: begin
      SaveDialog1.FileName := edit1.Text;
      if SaveDialog1.Execute then
        Edit1.Text := OpenDialog1.FileName;
    end;
    MODE_IMPORT: begin
      OpenDialog1.FileName := edit1.Text;
      if OpenDialog1.Execute then begin
        Edit1.Text := OpenDialog1.FileName;
        FreeAndNil(importParser);
        importAccountsPrepare(edit1.Text, importParser, importAccounts, importFlags);
        viewCurrentAccounts;
      end;
    end;
  end;
end;

function rowToFlags(item: TTreeListItem): TExportImportFlags;
begin
  result := [];
  if item.RecordItemsText[COLUMN_CURRENT] = CHECKBOX_CHECKED then include(result, eifCurrent);;
  if item.RecordItemsText[COLUMN_HISTORY] = CHECKBOX_CHECKED then include(result, eifHistory);;
  if item.RecordItemsText[COLUMN_CONFIG] = CHECKBOX_CHECKED then  include(result, eifConfig);;
  if item.RecordItemsText[COLUMN_PASSWORD] = CHECKBOX_CHECKED then include(result, eifPassword);;
end;

procedure TXMLExportFrm.Button2Click(Sender: TObject);
var i: integer;
  choosenAccounts: array of TCustomAccountAccess;
  choosenAccountNames: array of string;
  flags: TExportImportFlagsArray;
  flag: TExportImportFlags;
begin
  choosenAccounts := nil;
  flags := nil;
  choosenAccountNames := nil;
  case currentMode of
    MODE_EXPORT: begin
      for i := 1 to TreeListView1.Items.Count - 1 do begin
        flag := rowToFlags(TreeListView1.Items[i]);
        if flag = [] then continue;
        SetLength(choosenAccounts, length(choosenAccounts) + 1);
        SetLength(flags, length(flags) + 1);
        choosenAccounts[high(choosenAccounts)] := accounts[i-1];
        flags[high(flags)] := flag;
      end;
      exportAccounts(edit1.Text, choosenAccounts, flags);
      ShowMessage('Export abgeschlossen');
      close;
    end;
    MODE_IMPORT: begin
      if importParser = nil then begin
        button1.Click;
        exit;
      end;
      if updateThreadConfig.updateThreadsRunning > 0 then begin
        ShowMessage('Ein Import kann nicht durchgeführt werden, während eine Bibliotheksaktion im Hintergrund läuft. (z.B.: AKtualisierung/Verlängerung)');
        exit
      end;
      for i := 1 to TreeListView1.Items.Count - 1 do begin
        flag := rowToFlags(TreeListView1.Items[i]);
        if flag = [] then continue;
        SetLength(choosenAccountNames, length(choosenAccountNames) + 1);
        SetLength(flags, length(flags) + 1);
        choosenAccountNames[high(choosenAccountNames)] := TreeListView1.Items[i].Text;
        flags[high(flags)] := flag;
      end;
      try
        libraryParser.importAccounts(importParser, choosenAccountNames, flags);
      finally
        importParser := nil;
      end;
      ShowMessage('Import abgeschlossen');
      if (mainForm <> nil) then mainForm.RefreshListView;
      close;
    end;
  end;
end;

procedure TXMLExportFrm.FormShow(Sender: TObject);
begin
  viewCurrentAccounts;
end;

procedure TXMLExportFrm.SpeedButton1Click(Sender: TObject);
begin
  currentMode := MODE_EXPORT;
  button2.Caption := 'Export durchführen';
  viewCurrentAccounts;
end;

procedure TXMLExportFrm.SpeedButton2Click(Sender: TObject);
begin
  currentMode := MODE_IMPORT;
  button2.Caption := 'Import durchführen';
  viewCurrentAccounts;
end;

procedure TXMLExportFrm.Timer1Timer(Sender: TObject);
begin
end;

function rowCheckBoxState(item: TTreeListItem): string;
var
  i: Integer;
  first: Boolean;
begin
  first := true;
  result := CHECKBOX_HIDDEN;
  for i := 1 to item.RecordItems.Count - 1 do begin
    if item.RecordItemsText[i] = CHECKBOX_HIDDEN then continue;
    if first then result := item.RecordItemsText[i];
    first := false;
    if item.RecordItemsText[i] <> result then
      exit(CHECKBOX_MIXED);
  end;
end;

procedure TXMLExportFrm.updateMixedHeader(col: integer);
var
  equal: Boolean;
  i: Integer;
  state: String;
begin
  equal := true;
  state := CHECKBOX_HIDDEN;
  for i := 1 to TreeListView1.Items.Count - 1 do
    if TreeListView1.Items[i].RecordItemsText[col] <> CHECKBOX_HIDDEN then
      if state = CHECKBOX_HIDDEN then state := TreeListView1.Items[i].RecordItemsText[col]
      else if state <> TreeListView1.Items[i].RecordItemsText[col] then equal := false;
  if equal then TreeListView1.Items[0].RecordItemsText[col] := state
  else TreeListView1.Items[0].RecordItemsText[col] := CHECKBOX_MIXED;
  if col = COLUMN_CONFIG then updateMixedHeader(COLUMN_PASSWORD);
end;

procedure TXMLExportFrm.TreeListView1ClickAtRecordItem(sender: TObject; recorditem: TTreeListRecordItem);
  function checkBoxClickable(state: string): boolean;
  begin
    result := (state  <> CHECKBOX_DISABLED) and (state  <> CHECKBOX_HIDDEN);
  end;

var
  row: Integer;
  rowto: Integer;
  col: LongInt;
  i: Integer;
  j: Integer;

begin
  TreeListView1.BeginUpdate;
  try
    col := recorditem.Index;
    row := TreeListView1.Items.IndexOf(recorditem.Parent);
    rowto := row;
    if row = 0 then rowto := TreeListView1.Items.Count-1;
    case recorditem.Text of
      CHECKBOX_CHECKED: begin
        for i := row to rowto do begin
          if checkBoxClickable(TreeListView1.Items[i].RecordItemsText[col]) then
            TreeListView1.Items[i].RecordItemsText[col] := CHECKBOX_UNCHECKED;
          if (col = COLUMN_CONFIG) and (TreeListView1.Items[i].RecordItemsText[COLUMN_PASSWORD] <> CHECKBOX_HIDDEN) then
            TreeListView1.Items[i].RecordItemsText[COLUMN_PASSWORD] := CHECKBOX_DISABLED;
        end;
        if row <> 0 then //this behaves strangely with the password column (overview column shows checked/unchecked when it shoudl be mixed, but easier to toggle)
          updateMixedHeader(col);
      end;
      CHECKBOX_UNCHECKED, CHECKBOX_MIXED: begin
        for i := row to rowto do begin
          if checkBoxClickable(TreeListView1.Items[i].RecordItemsText[col]) then
            TreeListView1.Items[i].RecordItemsText[col] := CHECKBOX_CHECKED;
          if (col = COLUMN_CONFIG) and (TreeListView1.Items[i].RecordItemsText[COLUMN_PASSWORD] <> CHECKBOX_HIDDEN) then
            TreeListView1.Items[i].RecordItemsText[COLUMN_PASSWORD] := CHECKBOX_UNCHECKED;
        end;
        if row <> 0 then updateMixedHeader(col); //dito
      end;
    end;
    if col = 0 then begin
      //Mixed -> Checked -> Unchecked -> Mixed
      case rowCheckBoxState(TreeListView1.Items[row]) of
        CHECKBOX_MIXED:
          for i := row to rowto do
            for j := 1 to TreeListView1.Items[i].RecordItems.Count - 1 do
              if TreeListView1.Items[i].RecordItemsText[j] <> CHECKBOX_HIDDEN then
                TreeListView1.Items[i].RecordItemsText[j] := CHECKBOX_CHECKED;
        CHECKBOX_CHECKED:
          for i := row to rowto do
            for j := 1 to TreeListView1.Items[i].RecordItems.Count - 1 do
              if TreeListView1.Items[i].RecordItemsText[j] <> CHECKBOX_HIDDEN then
                TreeListView1.Items[i].RecordItemsText[j] := CHECKBOX_UNCHECKED;
        CHECKBOX_UNCHECKED: begin
          for i := row to rowto do begin
            for j := 1 to TreeListView1.Items[i].RecordItems.Count - 1 do
              if TreeListView1.Items[i].RecordItemsText[j] <> CHECKBOX_HIDDEN then
                TreeListView1.Items[i].RecordItemsText[j] := CHECKBOX_CHECKED;
            if TreeListView1.Items[i].RecordItemsText[COLUMN_PASSWORD] <> CHECKBOX_HIDDEN then
              TreeListView1.Items[i].RecordItemsText[COLUMN_PASSWORD] := CHECKBOX_UNCHECKED;
          end;
        end;
      end;
      for j := 1 to TreeListView1.Items[row].RecordItems.Count - 1 do
        updateMixedHeader(j);
    end;
  finally
    TreeListView1.EndUpdate;
  end;
end;

procedure TXMLExportFrm.TreeListView1CustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp;
  recordItem: TTreeListRecordItem; var defaultDraw: Boolean);
var
  cb: TThemedElementDetails;
  drawCB: Boolean;
  contentrect: TRect;
  w: Integer;
  cbText: String;
begin
  drawCB := false;
  if recordItem.Index = 0 then begin
    if eventTyp_cdet = cdetPrePaint then begin
      if recordItem.Parent = TreeListView1.Items[0] then TreeListView1.Canvas.Font.Style := [fsBold]
      else TreeListView1.Canvas.Font.Style := [];
      drawCB := false;
    end else drawCB := true;
    cbText := rowCheckBoxState(recordItem.Parent);
  end else begin
    drawCB := true;
    defaultDraw := false;
    cbText := recordItem.Text;
  end;

  if not drawCB then exit;

  case cbText of
    CHECKBOX_CHECKED:  cb := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
    CHECKBOX_DISABLED:  cb := ThemeServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
    CHECKBOX_MIXED:  cb := ThemeServices.GetElementDetails(tbCheckBoxMixedNormal);
    CHECKBOX_HIDDEN: exit;
    else cb := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  end;
  contentrect := TreeListView1.DrawingRecordItemRect;//ThemeServices.ContentRect(TreeListView1.Canvas.Handle, cb, TreeListView1.DrawingRecordItemRect);
  if recordItem.Index <> 0 then begin
    w := contentrect.Bottom - contentrect.Top; //contentrect.Right - contentrect.left;
    contentrect.Right := (TreeListView1.DrawingRecordItemRect.Left + TreeListView1.DrawingRecordItemRect.Right + w) div 2;
  end;
  ThemeServices.DrawElement(TreeListView1.Canvas.Handle, cb, contentrect, @TreeListView1.DrawingRecordItemRect);
end;

procedure TXMLExportFrm.viewCurrentAccounts;
var
  i: Integer;
begin
  TreeListView1.BeginUpdate;
  TreeListView1.Items.Clear;
  case currentMode of
    MODE_EXPORT: begin
      TreeListView1.Items.Add(['Alle Konten',CHECKBOX_CHECKED,CHECKBOX_CHECKED,CHECKBOX_CHECKED,CHECKBOX_UNCHECKED]);
      for i := 0 to accounts.Count - 1 do
        TreeListView1.Items.Add([accounts[i].prettyName,CHECKBOX_CHECKED,CHECKBOX_CHECKED,CHECKBOX_CHECKED,CHECKBOX_UNCHECKED]);
    end;
    MODE_IMPORT: begin
      TreeListView1.Items.Add(['Alle Konten',CHECKBOX_CHECKED,CHECKBOX_CHECKED,CHECKBOX_CHECKED,CHECKBOX_CHECKED]);
      for i := 0 to high(importAccounts) do begin
        with TreeListView1.Items.Add(importAccounts[i]) do begin
          RecordItemsText[COLUMN_CURRENT]  := IfThen(eifCurrent in importFlags[i], CHECKBOX_CHECKED, CHECKBOX_HIDDEN);
          RecordItemsText[COLUMN_HISTORY]  := IfThen(eifHistory in importFlags[i], CHECKBOX_CHECKED, CHECKBOX_HIDDEN);
          RecordItemsText[COLUMN_CONFIG]   := IfThen(eifConfig in importFlags[i], CHECKBOX_CHECKED, CHECKBOX_HIDDEN);
          RecordItemsText[COLUMN_PASSWORD] := IfThen(eifPassword in importFlags[i], CHECKBOX_CHECKED, CHECKBOX_HIDDEN);
        end;
      end;
      for i := 1 to TreeListView1.Columns.Count - 1 do
        updateMixedHeader(i);
    end;
  end;
  TreeListView1.EndUpdate;
end;

initialization
  {$I exportxml.lrs}

end.

