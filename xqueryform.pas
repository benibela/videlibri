unit xqueryform;

{$I videlibrilanguageconfig.inc}
interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Menus, TreeListView, booklistview,applicationformconfig,
  xquery, booklistreader, vlmaps;

type
  TOnBookResult = procedure (const b: IXQValue; book: TBook) of object;
  Txqueryfrm = class(TVideLibriForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemUserColumns: TMenuItem;
    querySave: TMenuItem;
    queryLoad: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItemUserColumnsClick(Sender: TObject);
    procedure querySaveClick(Sender: TObject);
  private
    { private declarations }
    lastResults: IXQValue;
    tempBookList: TBookList;
    procedure enumerateLastResults(callback: TOnBookResult);
    procedure callbackAddToListView(const b: IXQValue; book: TBook);
    procedure callbackAddToBookList(const b: IXQValue; book: TBook);
    procedure refreshListView;
  public
    { public declarations }
    listview: TBookListView;
    showingDefaultColumns: boolean;
    showingUserColumns: TSetOfString;
  end;

var
  xqueryfrm: Txqueryfrm;

implementation

uses libraryAccess,applicationdesktopconfig,bbutils,csvreadwrite,xquery.internals.common;

procedure Txqueryfrm.Button1Click(Sender: TObject);
begin
  close
end;

procedure Txqueryfrm.Button2Click(Sender: TObject);
var r: TBookListReader;
begin
  r:=TBookListReader.create(nil);
  try
    lastResults := queryHistory(r,memo1.Lines.text);
  finally
    r.free
  end;
  refreshListView;
end;

procedure Txqueryfrm.refreshListView;
var
  showDefaultColumns: Boolean;
  b: IXQValue;
  hasNoObjectItem: boolean;
  userColumns: TSetOfString;
  uc: string;
begin
  userColumns.init;
  showDefaultColumns := not MenuItemUserColumns.Checked;
  if MenuItemUserColumns.Checked then begin
    hasNoObjectItem := false;
    for b in lastResults do
      if (b.kind = pvkObject) and b.hasProperty('_accountPtr', nil) then
        showDefaultColumns := true
      else if b.kind = pvkObject then begin
        b.enumeratePropertyKeys(userColumns);
      end else hasNoObjectItem := true;
    if hasNoObjectItem and not showDefaultColumns then userColumns.include('title');
  end;
  if (showDefaultColumns <> showingDefaultColumns) or not (userColumns.isEqual(showingUserColumns)) then begin
    showingDefaultColumns := showDefaultColumns;
    showingUserColumns.assign(userColumns);
    listview.ColumnsClear;
    if showDefaultColumns then listview.addDefaultColumns;
    for uc in userColumns do listview.addColumn(uc);
  end;

  listview.clear;
  listview.SortColumn := 100; //disable auto sorting, but allow user to reenable it
  listview.BeginUpdate;
  EnterCriticalsection(updateThreadConfig.libraryAccessSection);
  try
    enumerateLastResults(@callbackAddToListView);
  finally
    LeaveCriticalsection(updateThreadConfig.libraryAccessSection);
    listview.EndUpdate;
    userColumns.done;
  end;
end;

procedure Txqueryfrm.Button3Click(Sender: TObject);
var
  p: TPoint;
begin
  p := button3.ControlToScreen(point(0,button3.Height));
  PopupMenu1.PopUp(p.x,p.y);
end;

procedure Txqueryfrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure Txqueryfrm.FormCreate(Sender: TObject);
begin
  listview := TBookListView.create(self,true);
  listview.addDefaultColumns;
  listview.Align := alClient;
  listview.Parent := self;
  listview.Options := listview.Options + [tlvoSorted];
  button3.Constraints.MaxHeight := button1.Height;
  showingUserColumns.init;
end;

procedure Txqueryfrm.FormShow(Sender: TObject);
begin
  if button2.Top < 5 then panel1.Height := panel1.Height - button2.top + 10;
end;

procedure Txqueryfrm.MenuItem1Click(Sender: TObject);
begin
end;

procedure Txqueryfrm.MenuItem2Click(Sender: TObject);
var
  fn: String;
begin
  SaveDialog1.Filter:='XML files|*.xml|All files|*.*';
  if not SaveDialog1.Execute then
    exit;
  tempBookList := TBookList.create();
  try
    enumerateLastResults(@callbackAddToBookList);
    fn := SaveDialog1.FileName;
    if fn.endsWithI('.xml') then delete(fn, length(fn) - 3, 4);
    tempBookList.save(fn);
  finally
    FreeAndNil(tempBookList);
  end;
end;

procedure Txqueryfrm.MenuItem3Click(Sender: TObject);
var
  fn: String;
begin
  SaveDialog1.Filter:='JSON files|*.json|All files|*.*';
  if not SaveDialog1.Execute then
    exit;
  fn := SaveDialog1.FileName;
  if not fn.endsWithI('.json') then fn += '.json';
  strSaveToFileUTF8(fn, lastResults.jsonSerialize(tnsText, true));
end;

function xqvbooksToCSV(const v: IXQValue): string;
var keys: TXQHashsetStr;
  w: IXQValue;
  k: string;
  csvw: TCSVBuilder;
begin
  if (v = nil) or (v.getSequenceCount = 0) then exit('');
  keys.init;
  for w in v do
    if w.kind = pvkObject then w.enumeratePropertyKeys(keys);
  if keys.Count = 0 then keys.include('');

  csvw := TCSVBuilder.Create;
  for k in keys do csvw.AppendCell(k);
  for w in v do begin
    csvw.AppendRow;
    if w.kind = pvkObject then begin
      for k in keys do
        csvw.AppendCell(w.getProperty(k).toString);
    end else csvw.AppendCell(w.toString);
  end;

  result := csvw.DefaultOutputAsString;

  csvw.Free;
  keys.done;
end;

procedure Txqueryfrm.MenuItem4Click(Sender: TObject);
var
  fn: String;
begin
  SaveDialog1.Filter:='CSV files|*.csv|All files|*.*';
  if not SaveDialog1.Execute then
    exit;
  fn := SaveDialog1.FileName;
  if not fn.endsWithI('.csv') then fn += '.csv';
  strSaveToFileUTF8(fn, xqvbooksToCSV(lastResults));
end;

procedure Txqueryfrm.MenuItemUserColumnsClick(Sender: TObject);
begin
  refreshListView;
end;

procedure Txqueryfrm.querySaveClick(Sender: TObject);
begin
  //todo: save and load queries

  //not possible with ini files. stupid TIniFile class cannot handle linebreaks
end;

procedure Txqueryfrm.enumerateLastResults(callback: TOnBookResult);
var
  b:ixqvalue;
  book: TBook;
  p: TXQProperty;
begin
  if lastResults = nil then exit;
  book := TBook.create;
  try
    for b in lastResults do begin
      case b.kind of
        pvkObject: begin
          book.clear;
          book.owningAccount:=nil;
          for p in b.getEnumeratorStringPropertiesUnsafe do
            case p.key  of
              '_accountPtr': book.owningAccount := TCustomBookOwner(PtrInt(p.Value.toInt64));
              'statusId': if p.Value.toString = 'history' then book.lend := false
              else begin
                book.lend := true;
                book.setProperty(p.key, p.Value.toString);
              end
              else book.setProperty(p.key, p.Value.toString);
            end;
          callback(b, book);
        end;
        else callback(b, nil);
      end;
    end;
  finally
    book.free;
  end;
end;

procedure Txqueryfrm.callbackAddToListView(const b: IXQValue; book: TBook);
var
  item: TTreeListItem;
begin
  if book <> nil then begin
    item := listview.Items.add;
    listview.fillBookItem(item, book);
    item.data.obj := nil;
  end else listview.Items.add.RecordItemsText[listview.getPropertyColumnIndex('title')] := b.toString;
end;

procedure Txqueryfrm.callbackAddToBookList(const b: IXQValue; book: TBook);
begin
  if book <> nil then begin
    tempBookList.add(book.clone);
  end else begin
    book := tbook.create;
    book.title := b.toString;
    tempBookList.add(book);
  end;
end;



initialization
  {$I xqueryform.lrs}

end.

