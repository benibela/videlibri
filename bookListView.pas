unit bookListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, booklistreader, TreeListView, forms, Controls,StdCtrls, FPCanvas,bbutils;

 type

 TEditableListView = class(TTreeListView)
   feditor: TEdit;
   feditedRecordItem: TTreeListRecordItem;

   //OnEditingDone: TNotifyEvent;
   property OnEditingDone; //lcl has it

   procedure startEditing(item: TTreeListRecordItem);

   procedure editorExit(Sender: TObject);
   procedure editorKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
 end;

 type TBookColorState = (bcsOld, bcsOrdered, bcsOK, bcsProvided, bcsLimited, bcsTimeNear);

 TAdditionalBookData = class(TTreeListRecordItem)
 public
   colorState: TBookColorState;
   weekSeparator: integer;
   checked: (csUnchecked, csChecked, csMixed, csDisabled);

   constructor Create(aparent: TTreeListItem); overload;
 end;

 TBookListView = class(TEditableListView)

 private
    fshowlendbooks: boolean;
    lastAddBook: tbook;
    ColumnIssueDateId, ColumnDueDateId: integer;

    procedure BookListCompareItems({%H-}sender: TObject; i1, i2: TTreeListItem;
      var compare: longint);
    procedure BookListCustomItemDraw(sender: TObject;
      eventTyp_cdet: TCustomDrawEventTyp; item: TTreeListItem; var defaultDraw: Boolean);
    procedure BookListViewItemsSortedEvent(Sender: TObject);

    procedure setBookWeekSeparator(b: TTreeListItem; v: integer);
    procedure setBookColorState(b: TTreeListItem; c: TBookColorState);
    function getBookWeekSeparator(b: TTreeListItem): integer;
    function getBookColorState(b: TTreeListItem): TBookColorState;

    function getBook(i: integer): TBook;
 public
   properties: TStringArray;
   groupingProperty: string;
   bookCount: integer;
   constructor create(aowner: TComponent; showLendBooks: boolean);
   destructor Destroy; override;
   procedure clear;
   procedure addBookList(list: TBookList; const accountName: string = '');
   procedure addBook(book: tbook; const accountName: string);
   procedure fillBookItem(item: TTreeListItem; book: TBook);
   property books[i:integer]: TBook read GetBook;

   procedure ColumnsClear;
   procedure addDefaultColumns;
   procedure addColumn(const prop: string);
   function getPropertyColumnIndex(p: string): integer;
   function getAdditionalBookData(item: TTreeListItem): TAdditionalBookData;

   function SelectedBook: TBooK;
   function SelectedBooks: TBookList;
 end;


function dateToWeek(date: longint):longint; //week: monday - sunday

const WEEK_SEPARATOR_UNDEFINED = MaxInt;

resourcestring
  rsWeekUnknown = 'Unbekannte Woche';
  rsWeekLast = 'Letzte Woche';
  rsWeekThis = 'Diese Woche';
  rsWeekNext = 'Nächste Woche';
  rsWeekDates = 'Woche vom %s zum %s';

implementation

uses applicationdesktopconfig, applicationconfig,  Graphics, bookproperties, StrUtils;
//  ,windows {for the search only};



{ TBookListView }
function dateToWeek(date: longint):longint; //week: monday - sunday
begin
  Result:=(date-2) div 7;
end;
function dateToWeekPretty(date: longint): string;
var
  week: LongInt;
begin
  if date <= 0 then exit(rsWeekUnknown);
  week := dateToWeek(date);
  case week - dateToWeek(currentDate) of
    -1: result := rsWeekLast;
    0: result := rsWeekThis;
    1: result := rsWeekNext;
    else result := Format(rsWeekDates, [DateToStr(week * 7 + 2), DateToStr(week * 7+2+6)]);
  end;
end;

constructor TAdditionalBookData.Create(aparent: TTreeListItem);
begin
  inherited;
  weekSeparator := WEEK_SEPARATOR_UNDEFINED;
end;

procedure TEditableListView.startEditing(item: TTreeListRecordItem);
var
  parentItem: TTreeListItem;
  column: LongInt;
begin
  feditedRecordItem := item;
  column := item.Index;
  parentItem := item.Parent;

  if feditor = nil then begin
    feditor := TEdit.Create(self);
    feditor.Height := RowHeight;
    feditor.Parent := self;
    feditor.BorderStyle := bsNone;
    feditor.OnExit:=@editorExit;
    feditor.AutoSize := false;
    feditor.OnKeyUp:=@editorKeyUp;
  end;
  feditor.BoundsRect := parentItem.getBounds(column);
  feditor.text := parentItem.RecordItemsText[column];
  if feditor.Height <> RowHeight then
    feditor.top := feditor.top - (feditor.Height - RowHeight) div 2;
  feditor.Visible := true;
  feditor.SetFocus;
end;

procedure TEditableListView.editorExit(Sender: TObject);
begin
  if (feditedRecordItem = nil) or (feditor = nil) then exit;
  //feditedRecordItem.Text := feditor.text; better to it in event handler
  if assigned(OnEditingDone) then OnEditingDone(self);
  feditedRecordItem := nil;
  feditor.Visible := false;
end;

procedure TEditableListView.editorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if feditedRecordItem = nil then exit;
  case key of
    13, 10: editorExit(feditor);
    $1B: begin
      feditor.Text := feditedRecordItem.Text;
      editorExit(feditor);
    end;
  end;
end;

procedure TBookListView.BookListViewItemsSortedEvent(Sender: TObject);
var i: longint;
    lastWeek: longint;
    book: TBook;
begin
  if SortColumn <> ColumnDueDateId then exit;
  if items.count=0 then exit;
  BeginMultipleUpdate;
  lastWeek:=dateToWeek(currentDate);
  for i:=0 to Items.Count-1 do begin
    book:=TBook(Items[i].data.obj);
    if (book<>nil) and (book.lend) and
       (dateToWeek(book.dueDate) <> lastWeek) then begin
         setBookWeekSeparator(items[i], abs(dateToWeek(book.dueDate) - lastWeek));
      lastWeek:=dateToWeek(book.dueDate);
     end else
      setBookWeekSeparator(items[i],WEEK_SEPARATOR_UNDEFINED);
  end;
  EndMultipleUpdate;
end;

procedure TBookListView.setBookWeekSeparator(b: TTreeListItem; v: integer);
begin
  getAdditionalBookData(b).weekSeparator := v;
end;

procedure TBookListView.setBookColorState(b: TTreeListItem; c: TBookColorState);
begin
  getAdditionalBookData(b).colorState := c;
end;

function TBookListView.getBookWeekSeparator(b: TTreeListItem): integer;
begin
  result := getAdditionalBookData(b).weekSeparator;
end;

function TBookListView.getBookColorState(b: TTreeListItem): TBookColorState;
begin
  result := getAdditionalBookData(b).colorState;
end;


procedure TBookListView.BookListCompareItems(sender: TObject; i1,
  i2: TTreeListItem; var compare: longint);
var book1,book2: TBook;
begin
  if i1.SubItems.Count > 0 then i1 := i1.SubItems[0];
  book1:=TBook(i1.data.obj);
  if i2.SubItems.Count > 0 then i2 := i2.SubItems[0];
  book2:=TBook(i2.data.obj);
  if (book1 = nil) and (book2 = nil) then begin
    compare := striCompareClever(i1.RecordItemsText[SortColumn], i2.RecordItemsText[SortColumn]);
    exit;
  end;
  if (book1=nil) then begin
    Compare:=1;
    exit;
  end;
  if (book2=nil) then begin
    Compare:=-1;
    exit;
  end;
  compare:=0;
  if SortColumn = ColumnIssueDateId then begin
    if book1.issueDate<book2.issueDate then compare:=-1
    else if book1.issueDate>book2.issueDate then compare:=1
    else compare := CompareText(book1.libraryBranch, book2.libraryBranch);
  end else if SortColumn = ColumnDueDateId then begin
    //see later
  end else compare:=CompareText(i1.RecordItemsText[SortColumn],i2.RecordItemsText[SortColumn]);
  if (compare=0) and (book1.status in BOOK_NOT_LEND) <> (book2.status in BOOK_NOT_LEND) then
    if book1.status in BOOK_NOT_LEND then
      compare := 1
     else
      compare := -1;
  if compare=0 then  //Sort LimitDate
    if book1.dueDate<book2.dueDate then
       compare:=-1
    else if book1.dueDate>book2.dueDate then
       compare:=1;
  if compare=0 then       //Sort Status
    if (book1.status in BOOK_NOT_EXTENDABLE) and (book2.status in BOOK_EXTENDABLE) then
      compare:=-1
     else if (book1.status in BOOK_EXTENDABLE) and (book2.status in BOOK_NOT_EXTENDABLE) then
      compare:=1;
//     else compare:=compareText(PBook(item1.data)^.statusStr,PBook(item2.data)^.statusStr);
  if compare=0 then       //Sort ID
    compare:=compareText(i1.Text,i2.Text);
end;


function getBookColor(book:TBook):TBookColorState;
begin
  if book = nil then exit(bcsOld);
  if book.lend=false then
    result:=bcsOld
  else if book.status = bsProvided then
    result:=bcsProvided
  else if book.status in BOOK_CANCELABLE then
    result:=bcsOrdered
  else if book.dueDate<=redTime then
    result:=bcsTimeNear
  else if book.status in BOOK_NOT_EXTENDABLE then
    result:=bcsLimited
  else result:=bcsOK;
end;


procedure TBookListView.BookListCustomItemDraw(sender: TObject;
  eventTyp_cdet: TCustomDrawEventTyp; item: TTreeListItem; var defaultDraw: Boolean);
var pa: array[0..2] of tpoint;
    i,x,y,ypos:longint;
    colorState: TBookColorState;
begin
  ypos:=TTreeListView(sender).DrawingYPos;
  case eventTyp_cdet of
    cdetPrePaint:
      if not item.SeemsSelected then begin
        canvas.Brush.Style:=bsSolid;
        colorState := getBookColorState(item);
        case colorState of
          bcsOld: Canvas.brush.color:=colorOld;
          bcsOrdered: Canvas.brush.color:=colorOrdered;
          bcsOK: Canvas.brush.color:=colorOK;
          bcsProvided: Canvas.brush.color:=colorProvided;
          bcsLimited: Canvas.brush.color:=colorLimited;
          else Canvas.brush.color:=colorTimeNear;
        end;
        if Canvas.brush.color=clnone then
          Canvas.brush.color:=self.Color;;
      end;
    cdetPostPaint: if SortColumn= ColumnDueDateId then
      if getBookWeekSeparator(item) <> WEEK_SEPARATOR_UNDEFINED then begin
        canvas.pen.Style:=psSolid;
        canvas.pen.Color:=$bB00BB;
        Canvas.pen.Width:=2;
        Canvas.Line(0,ypos,width,ypos);
        Canvas.brush.Color:=canvas.pen.Color;
        Canvas.pen.Color:=clblack;
        Canvas.pen.width:=1;
        pa[0].x:=0;pa[0].y:=ypos-4;
        pa[1].x:=4;pa[1].y:=ypos;
        pa[2].x:=0;pa[2].y:=ypos+4;
        Canvas.Polygon(pa);
        pa[0].x:=width-F_VScroll.width-1;pa[0].y:=ypos-4;
        pa[1].x:=width-F_VScroll.width-5;pa[1].y:=ypos;
        pa[2].x:=width-F_VScroll.width-1;pa[2].y:=ypos+4;
        Canvas.Polygon(pa);
        for i:=1 to getBookWeekSeparator(item) do begin
          x:=width-F_VScroll.width-8*i-2;
          y:=ypos;
          Canvas.Ellipse(x-3,y-3,x+3,y+3);
        end;
      end;
  end;

  defaultDraw:=true;
end;




procedure TBookListView.addBook(book: tbook; const accountName: string);
var
  bookItem: TTreeListItem;
  group: String;
  i: Integer;
begin
  group := '';
  bookItem := nil;
  bookCount += 1;
  case groupingProperty of
    '':  bookItem := items.Add;
    '_account':  group := accountName;
    '_dueWeek':  group := dateToWeekPretty(book.dueDate);
    '_status': begin
      if not book.lend then group := rsBookStatusNotLend
      else begin
        case book.status of
          bsNormal, bsCuriousInStr: group := rsBookStatusNormalRenewable;
          bsUnknown: group := rsBookStatusUnknown;
          bsProblematicInStr: group := rsBookStatusNonRenewable;
          bsOrdered: group := rsBookStatusOrdered;
          bsProvided: group := rsBookStatusProvided;
          else group := '???';
          //bsIsSearchedDONTUSETHIS,bsEarMarkedDONTUSETHIS, bsMaxLimitReachedDONTUSETHIS,,bsCuriousInStr,bsAccountExpiredDONTUSETHIS,);
        end;
        if book.statusStr <> '' then group := group + ': '+book.statusStr;
      end;
    end;
    '_issueWeek': group := dateToWeekPretty(book.issueDate);
    else group := book.getProperty(groupingProperty);
  end;
  if groupingProperty <> '' then begin
    for i := 0 to Items.Count - 1 do
      if Items[i].Text = group then begin
        bookItem := items[i].SubItems.Add();
        break;
      end;
    if bookItem = nil then
      with items.Add(group) do begin
        bookItem := SubItems.Add();
      end;
  end;

  fillBookItem(bookItem, book);
end;

function bestIssueDateGuessPretty(book: TBook): string;
begin
  if (book.issueDate > 0) or (book.firstExistsDate = 0) then
    result := DateToPrettyStr(book.issueDate)
  else
    result := '<= ' + DateToPrettyStr(book.firstExistsDate);
end;

function dueDatePretty(book: TBook): string;
var
  inLendingHistory: Boolean;
begin
  inLendingHistory := (book.lend = false) and (book.owningAccount <> nil);
  if inLendingHistory and (book.dueDate = -2) then result := rsNeverLend
  else begin
    result := DateToPrettyStr(book.dueDate);
    if inLendingHistory then result := '(' + result + ')';
  end;
end;

procedure TBookListView.fillBookItem(item: TTreeListItem; book: TBook);
var
  i: Integer;
  colorState: TBookColorState;
begin
  with item do begin
    book.incReference;

    item.RecordItems.Clear;
    RecordItems.Capacity := length(properties) + 1;
    for i := 0 to high(properties) do
      if i = ColumnIssueDateId then RecordItems.Add(bestIssueDateGuessPretty(book))
      else if i = ColumnDueDateId then RecordItems.Add(dueDatePretty(book))
      else
        case properties[i] of
          '?account': if book.owningAccount <> nil then RecordItems.Add(book.owningAccount.prettyName)
                      else RecordItems.Add(rsunknown);
          'status': RecordItems.Add(BookStatusToStr(book));
          'renewcount': RecordItems.Add(IfThen(book.renewCount < 0, '', book.renewCount.ToString));
          else RecordItems.Add(book.getProperty(properties[i]));
        end;

    //else
    data.obj:=book;
  end;
  colorState := getBookColor(book);
  setBookColorState(item, colorState);
  if (groupingProperty <> '') and (colorState > getBookColorState(item.Parent)) then
    setBookColorState(item.Parent, colorState);
end;

procedure TBookListView.ColumnsClear;
begin
  Columns.Clear;
  ColumnIssueDateId := -1;
  ColumnDueDateId := -1;
  properties := nil;
end;


procedure TBookListView.addDefaultColumns;
var i: Integer;
begin
  for i := 0 to high(defaultBookListViewLendColumns) do
    addColumn(defaultBookListViewLendColumns[i]);
end;

procedure TBookListView.addColumn(const prop: string);
var
  lprop, tprop: String;
begin
  lprop := lowercase(prop);
  with Columns.Add do begin
    text := getBookPropertyPretty(lprop);
    if text <> '' then begin
      width := getBookPropertyDefaultWidth(lprop);
      case lprop of
        'issuedate': begin
          Alignment:=taCenter;
          ColumnIssueDateId := columns.Count-1;
        end;
        'duedate': begin
          Alignment:=taCenter;
          ColumnDueDateId := columns.Count-1;
        end;
        'isbn': Visible:=false;
      end;
    end else begin
      tprop := prop;
      if strEndsWith(tprop, '!') then delete(tprop, length(tprop), 1);
      text := tprop;
      width := 50;
      visible := true;
    end;
  end;
  SetLength(properties, length(properties)+1);
  properties[high(properties)] := lprop;
end;

function TBookListView.getPropertyColumnIndex(p: string): integer;
begin
  p := lowercase(p);
  result := arrayIndexOf(properties, p);
end;

function TBookListView.getAdditionalBookData(item: TTreeListItem): TAdditionalBookData;
begin
  while item.RecordItems.Count < length(properties) do item.RecordItems.Add('');
  while item.RecordItems.Count >= length(properties) + 1 do
    if not (item.RecordItems[length(properties)] is TAdditionalBookData) then begin
      item.RecordItems[Length(properties)].Free;
      item.RecordItems[Length(properties)]:=nil;
      item.RecordItems.Delete(length(properties));
    end else break;
  if item.RecordItems.Count < length(properties) + 1 then item.RecordItems.Additem(TAdditionalBookData.Create(item));
  result := TAdditionalBookData(item.RecordItems[Length(properties)]);
end;

function TBookListView.getBook(i: integer): TBook;
begin
  result := tbook(items[i].data.obj);
end;

constructor TBookListView.create(aowner: TComponent;showLendBooks: boolean);
begin
  inherited create(aowner);
  OnCompareItems:=@BookListCompareItems;
  if showLendBooks then  begin
    OnCustomItemDraw:=@BookListCustomItemDraw;
    OnItemsSortedEvent:=@BookListViewItemsSortedEvent;
    RootLineMode:=lmNone;
  end;
  fshowlendbooks:=showLendBooks;
  Align:=alClient;

  Options:=Options+[tlvoColumnsDragable];
  ColumnsClear;
end;

destructor TBookListView.Destroy;
begin
  OnVScrollBarChange := nil;
  clear;
  inherited Destroy;
end;


procedure TBookListView.clear;
var
  i: Integer;
begin
  for i := 0 to items.count-1 do
    if TBook(Items[i].data.obj) <> nil then begin
      TBook(Items[i].data.obj).decReference;
      Items[i].data.obj := nil;
    end;
  Items.Clear;
  bookCount:=0;
  lastAddBook:=nil;
end;

procedure TBookListView.addBookList(list: TBookList; const accountName: string);
var i:longint;
begin
  for i:=0 to list.Count-1 do
    addBook(list[i], accountName);
end;

function TBookListView.SelectedBook: TBooK;
begin
  if Selected = nil then exit(nil);;
  Result := tbook(Selected.data.obj);
end;

function TBookListView.SelectedBooks: TBookList;
  procedure visit(list: TTreeListItems);
  var
    item: TTreeListItem;
    i: Integer;
  begin
    for i:=0 to list.Count-1 do begin
      item := list[i];
      if item.Selected and (item.data.obj<>nil) then
        result.add(item.data.obj as TBook);
      visit(item.SubItems);
    end;
  end;
begin
  result := TBookList.create();
  result.Capacity:=selCount;
  visit(items);
end;

end.
45076
8->killfocus
20->ERASEBKGND
66592=$10420->
