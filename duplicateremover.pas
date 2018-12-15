unit duplicateremover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, TreeListView, bookListView, applicationformconfig;

type
  TduplicateForm = class(TVideLibriForm)
    Button1: TButton;
    Button2: TButton;
    cbAccounts: TComboBox;
    cbTimeConstraint: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    lbCount: TLabel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure cbAccountsSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    listview: TBookListView;
    totalcount,checkedcount: integer;
    procedure itemPositioning(sender: TObject; visualColumnIndex: integer; recordItem: TTreeListRecordItem; var aposition: TRect);
    procedure listviewCustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp; recordItem: TTreeListRecordItem;
      var defaultDraw: Boolean);
    procedure listviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure searchDuplicates;
  end;

var
  duplicateForm: TduplicateForm;

implementation

uses applicationconfig,applicationdesktopconfig ,booklistreader,bbutils,math,libraryAccess,libraryParser, themes;

resourcestring
  rsConfirmDelete = 'Sollen die ausgewählten Duplikate durch jeweils einen einzelnen Eintrag ersetzt werden?';



procedure TduplicateForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TduplicateForm.Button1Click(Sender: TObject);
var
  i, j: Integer;
begin
  if not confirm(rsConfirmDelete) then exit;

  system.EnterCriticalsection(updateThreadConfig.libraryAccessSection);

  for i := 0 to listview.Items.Count - 1 do begin
    if listview.getAdditionalBookData(listview.Items[i]).checked = csUnchecked then continue;

    (listview.books[i].owningAccount as TCustomAccountAccess).books.old.add(listview.books[i]);

    for j := 0 to listview.Items[i].SubItems.Count - 1 do
      if listview.getAdditionalBookData(listview.Items[i]).checked <> csUnchecked then
        (tbook(listview.Items[i].SubItems[j].data.obj).owningAccount as TCustomAccountAccess).books.old.remove(tbook(listview.Items[i].SubItems[j].data.obj));
  end;

  for i := 0 to accounts.Count - 1 do
    accounts[i].saveConfig();

  //it does not seem like a bad idea to block everything if an error occured
  system.LeaveCriticalsection(updateThreadConfig.libraryAccessSection);

  searchDuplicates;
end;

procedure TduplicateForm.CheckBox1Change(Sender: TObject);
begin
  searchDuplicates;
end;

procedure TduplicateForm.cbAccountsSelect(Sender: TObject);
begin
  searchDuplicates;
end;

procedure TduplicateForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  listview := TBookListView.create(self,true);
  listview.Align := alClient;
  listview.Options := listview.Options + [tlvoSorted];
  listview.Parent := self;
  listview.OnCustomRecordItemPositioning:=@itemPositioning;
  listview.OnCustomRecordItemDraw:=@listviewCustomRecordItemDraw;
  listview.OnMouseDown:=@listviewMouseDown;
  listview.addDefaultColumns;
  listview.Columns[high(listview.properties)].Width:=15;
  listview.addColumn('_firstexistsdate');
  listview.addColumn('_lastexistsdate');
  for i := 0 to accounts.count - 1 do cbAccounts.Items.add(accounts[i].prettyName);
  searchDuplicates;
end;

var sortUsers: boolean;

function bookcompare(Item1, Item2: Pointer): Integer;
begin
  result := striCompareClever(tbook(item1).title, tbook(item2).title);
  if result <> 0 then exit;
  result := striCompareClever(tbook(item1).author, tbook(item2).author);
  if result <> 0 then exit;
  result := striCompareClever(tbook(item1).year, tbook(item2).year);
  if result <> 0 then exit;
  result := striCompareClever(tbook(item1).id, tbook(item2).id);
  if result <> 0 then exit;
  if sortUsers then begin
    result := Sign(PtrInt(tbook(item1).owningAccount) - PtrInt(tbook(item2).owningAccount));
    if result <> 0 then exit;
  end;
  result := Sign(tbook(item1).issueDate - tbook(item2).issueDate);
  if result <> 0 then exit;
  result := Sign(tbook(item1).dueDate - tbook(item2).dueDate);
  if result <> 0 then exit;
end;

function minPos(const d, e: Integer): integer;
begin
  if d <= 0 then result := e
  else if e <= 0 then result := d
  else result := min(d,e);
end;


procedure TduplicateForm.searchDuplicates;
  procedure fillItem(item: TTreeListItem; book: TBook);
  begin
    listview.fillBookItem(item, book);
    listview.getAdditionalBookData(item).checked := csChecked;
    item.RecordItemsText[listview.getPropertyColumnIndex('dueDate')] := DateToPrettyStr(book.dueDate);
  end;
var alllists: TBookList;
  i, accountMode, timeMode: Integer;
  ingroup: Boolean;
  subitem, item: TTreeListItem;
  itemBook: TBook;
begin
  accountMode := cbAccounts.ItemIndex;
  timeMode := cbTimeConstraint.ItemIndex;
  listview.clear;
  alllists := TBookList.create();
  system.EnterCriticalSection(updateThreadConfig.libraryAccessSection);
  try
    if (accountMode < 2) then begin
      for i := 0 to accounts.Count - 1 do
        alllists.addList(accounts[i].books.old);
    end else alllists.addList(accounts[cbAccounts.ItemIndex-2].books.old);

    sortUsers := accountMode = 1;
    alllists.Sort(@bookcompare);

    totalcount := 0;
    item := nil; itembook := nil;
    ingroup := false;
    listview.BeginUpdate;
    for i := 1 to alllists.Count - 1 do begin
      if alllists[i].equalToKey(alllists[i-1])
         and ((accountMode <> 1) or (alllists[i].owningAccount = alllists[i-1].owningAccount))
         and ((timeMode = 0)
             or ((timeMode = 1) and ( max(alllists[i-1].dueDate, alllists[i-1].lastExistsDate) >= minPos(alllists[i].issueDate, alllists[i].firstExistsDate ) - 1))
             or ((timeMode = 2) and (alllists[i-1].issueDate = alllists[i].issueDate)))
         then begin
        if not ingroup then begin
          if item <> nil then fillItem(item, itembook);
          item := listview.Items.Add;
          itemBook := alllists[i-1].clone;
          itemBook.owningAccount := alllists[i-1].owningAccount;
          subitem := item.SubItems.add;
          fillItem(subitem, alllists[i-1]);
          ingroup := true;
          totalcount += 1;
        end;
        subitem := item.SubItems.add;
        fillItem(subitem, alllists[i]);
         if (alllists[i].firstExistsDate <> 0) and ( (alllists[i].firstExistsDate < itemBook.firstExistsDate) or (itemBook.firstExistsDate = 0)) then
           itemBook.firstExistsDate := alllists[i].firstExistsDate;
         if (alllists[i].issueDate <> 0) and ( (alllists[i].issueDate < itemBook.issueDate) or (itemBook.issueDate = 0)) then
           itemBook.issueDate := alllists[i].issueDate;
         if (alllists[i].lastExistsDate <> 0) and ( (alllists[i].lastExistsDate > itemBook.lastExistsDate) or (itemBook.lastExistsDate = 0)) then
           itemBook.lastExistsDate := alllists[i].lastExistsDate;
         if (alllists[i].dueDate <> 0) and ( (alllists[i].dueDate > itemBook.dueDate) or (itemBook.dueDate = 0)) then
           itemBook.dueDate := alllists[i].dueDate;
        totalcount += 1;
      end else ingroup := false;
    end;
    if item <> nil then fillItem(item, itembook);
    listview.EndUpdate;
    checkedcount := listview.Items.Count;
    lbCount.Caption := IntToStr(checkedcount) + ' / ' + IntToStr(listview.Items.Count) + ' / ' + IntToStr(totalcount);
  finally
    alllists.free;
    system.LeaveCriticalsection(updateThreadConfig.libraryAccessSection);
  end;
end;

procedure TduplicateForm.listviewCustomRecordItemDraw(sender: TObject; eventTyp_cdet: TCustomDrawEventTyp; recordItem: TTreeListRecordItem;
  var defaultDraw: Boolean);
var
  cb: TThemedElementDetails;
  contentrect: TRect;
  w: LongInt;
begin
  if eventTyp_cdet = cdetPrePaint then exit;
  if recordItem.Index <> 0 then exit;
  case listview.getAdditionalBookData(recordItem.Parent).checked of
    csChecked:  cb := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
    csDisabled:  cb := ThemeServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
    csMixed:  cb := ThemeServices.GetElementDetails(tbCheckBoxMixedNormal);
    //CHECKBOX_HIDDEN: exit;
    else cb := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  end;
  contentrect := listview.DrawingRecordItemRect;
  w := contentrect.Bottom - contentrect.Top; //contentrect.Right - contentrect.left;
  contentrect.Right := contentrect.left + w;
  ThemeServices.DrawElement(listview.Canvas.Handle, cb, contentrect, @listview.DrawingRecordItemRect);
end;

procedure TduplicateForm.listviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  recorditem: TTreeListRecordItem;
  item, counteditem: TTreeListItem;
  rec: TRect;
  hasChecked, hasUnchecked, old: Boolean;
  i: Integer;
begin
  recorditem := listview.GetRecordItemAtPos(x,y);
  if (recorditem = nil) or (recorditem.Index > 0) then exit;
  item := recorditem.Parent;
  rec := item.getBounds(0);
  if (x >= 15) and (x <= 15 + 5 + rec.Bottom - rec.Top) then begin
    old :=  listview.getAdditionalBookData(item).checked <> csUNCHECKED;
    case listview.getAdditionalBookData(item).checked of
      csUNCHECKED: listview.getAdditionalBookData(item).checked := csCHECKED;
      else listview.getAdditionalBookData(item).checked := csUNCHECKED;
    end;
    listview.invalidateItem(item);
    if item.Parent <> nil then begin
      hasChecked := false;
      hasUnchecked := false;
      counteditem := item.Parent;
      old := listview.getAdditionalBookData(item.Parent).checked  <> csUnchecked;
      for i := 0 to item.Parent.SubItems.Count - 1 do
        if listview.getAdditionalBookData(item.Parent.SubItems[i]).checked = csCHECKED then hasChecked := true
        else hasUnchecked := true;
      if hasChecked and hasUnchecked then listview.getAdditionalBookData(item.parent).checked := csMIXED
      else if hasChecked then listview.getAdditionalBookData(item.parent).checked := csCHECKED
      else if hasUnchecked then listview.getAdditionalBookData(item.parent).checked := csUNCHECKED;
      listview.invalidateItem(item.parent);
    end else counteditem := item;
    for i := 0 to item.SubItems.Count - 1 do
      listview.getAdditionalBookData(item.SubItems[i]).checked := listview.getAdditionalBookData(item).checked;

    if (old <> (listview.getAdditionalBookData(counteditem).checked <> csUNCHECKED)) then
      if listview.getAdditionalBookData(counteditem).checked = csUNCHECKED then dec(checkedcount)
      else inc(checkedcount);
    listview.sheduleInternRepaint();
    lbCount.Caption := IntToStr(checkedcount) + ' / ' + IntToStr(listview.Items.Count) + ' / ' + IntToStr(totalcount);
  end;
end;

procedure TduplicateForm.itemPositioning(sender: TObject; visualColumnIndex: integer; recordItem: TTreeListRecordItem; var aposition: TRect
  );
begin
  if recordItem.Index = 0 then aposition.Left += aposition.Bottom - aposition.top + 5;
end;

initialization
  {$I duplicateremover.lrs}

end.

