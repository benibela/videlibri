unit bookListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, booklistreader, TreeListView, forms, Controls,FPCanvas;

 type

 { TBookListView }

 TBookListView = class(TTreeListView)

 private
    fshowlendbooks: boolean;
    lastAddBook: tbook;

    procedure BookListCompareItems(sender: TObject; i1, i2: TTreeListItem;
      var compare: longint);
    procedure BookListCustomItemDraw(sender: TObject;
      eventTyp_cdet: TCustomDrawEventTyp; item: TTreeListItem; var defaultDraw: Boolean);
    procedure BookListViewItemsSortedEvent(Sender: TObject);

    procedure addBook(book: tbook; const accountName: string);
    function getBook(i: integer): TBook;
 public
   groupingProperty: string;
   bookCount: integer;
   constructor create(aowner: TComponent;showLendBooks: boolean);
   destructor Destroy; override;
   procedure clear;
   procedure addBookList(list: TBookList; const accountName: string = '');
   property books[i:integer]: TBook read GetBook;

   function SelectedBook: TBooK;
   function SelectedBooks: TBookList;
 end;

const BL_BOOK_COLUMNS_AUTHOR=2;
      BL_BOOK_COLUMNS_TITLE=3;
      BL_BOOK_COLUMNS_YEAR=4;
      BL_BOOK_COLUMNS_ISSUE_ID=5;
      BL_BOOK_COLUMNS_LIMIT_ID=6;
      BL_BOOK_COLUMNS_ACCOUNT=7;
      BL_BOOK_COLUMNS_STATUS=8;
      BL_BOOK_COLUMNS_ISBN=9;
      BL_BOOK_EXTCOLUMNS_COLOR=10;
      BL_BOOK_EXTCOLUMNS_WEEK_SEPARATOR=11;

function dateToWeek(date: longint):longint; //week: monday - sunday
implementation

uses applicationdesktopconfig, applicationconfig,  bbutils, libraryParser, Graphics;
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
  if date <= 0 then exit('Unbekannte Woche');
  week := dateToWeek(date);
  case week - dateToWeek(currentDate) of
    -1: result := 'Letzte Woche';
    0: result := 'Diese Woche';
    1: result := 'Nächste Woche';
    else result := 'Woche vom '+DateToStr(week * 7 + 2)+ ' zum '+DateToStr(week * 7+2+6);
  end;
end;

procedure TBookListView.BookListViewItemsSortedEvent(Sender: TObject);
var i: longint;
    lastWeek: longint;
    book: TBook;
begin
  if SortColumn<>BL_BOOK_COLUMNS_LIMIT_ID then exit;
  if items.count=0 then exit;
  BeginMultipleUpdate;
  lastWeek:=dateToWeek(currentDate);
  for i:=0 to Items.Count-1 do begin
    book:=TBook(Items[i].data.obj);
    if (book<>nil) and (book.lend) and
       (dateToWeek(book.dueDate) <> lastWeek) then begin
      Items[i].RecordItemsText[BL_BOOK_EXTCOLUMNS_WEEK_SEPARATOR]:=IntToStr(abs(dateToWeek(book.dueDate) - lastWeek));
      lastWeek:=dateToWeek(book.dueDate);
     end else
      Items[i].RecordItemsText[BL_BOOK_EXTCOLUMNS_WEEK_SEPARATOR]:='';
  end;
  EndMultipleUpdate;
end;

procedure TBookListView.BookListCompareItems(sender: TObject; i1,
  i2: TTreeListItem; var compare: longint);
var book1,book2: TBook;
begin
  if i1.SubItems.Count > 0 then i1 := i1.SubItems[0];
  book1:=TBook(i1.data.obj);
  if i2.SubItems.Count > 0 then i2 := i2.SubItems[0];
  book2:=TBook(i2.data.obj);
  if (book1=nil) then begin
    Compare:=1;
    exit;
  end;
  if (book2=nil) then begin
    Compare:=-1;
    exit;
  end;
  compare:=0;
  case SortColumn of
    BL_BOOK_COLUMNS_ISSUE_ID:
        if book1.issueDate<book2.issueDate then compare:=-1
        else if book1.issueDate>book2.issueDate then compare:=1
        else compare := CompareText(book1.libraryBranch, book2.libraryBranch);
    BL_BOOK_COLUMNS_LIMIT_ID:; //see later

    else compare:=CompareText(i1.RecordItemsText[SortColumn],i2.RecordItemsText[SortColumn]);
  end;
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

type TBookColorState = (bcsOld, bcsOrdered, bcsOK, bcsProvided, bcsLimited, bcsTimeNear);

function getBookColor(book:TBook):TBookColorState;
begin
  if book = nil then exit(bcsOld);
  if book.lend=false then
    result:=bcsOld
  else if book.status = bsProvided then
    result:=bcsProvided
  else if book.status = bsOrdered then
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
        colorState := TBookColorState(ord(item.RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR][1]) - ord('0'));
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
    cdetPostPaint: if SortColumn= BL_BOOK_COLUMNS_LIMIT_ID then
      if item.RecordItemsText[BL_BOOK_EXTCOLUMNS_WEEK_SEPARATOR] <> '' then begin
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
        for i:=1 to StrToInt(item.RecordItemsText[BL_BOOK_EXTCOLUMNS_WEEK_SEPARATOR]) do begin
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
      if not book.lend then group := 'abgegeben'
      else begin
        case book.status of
          bsNormal, bsCuriousInStr: group := 'normal/verlängerbar';
          bsUnknown: group := 'unbekannt';
          bsProblematicInStr: group := 'nicht verlängerbar';
          bsOrdered: group := 'vorgemerkt';
          bsProvided: group := 'bereitgestellt';
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
        RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR]:='0';
      end;
  end;

  with bookItem do begin
    book.incReference;

    text:=book.id;
    RecordItems.Add(book.category);
    RecordItemsText[BL_BOOK_COLUMNS_AUTHOR] := book.author;
    RecordItemsText[BL_BOOK_COLUMNS_TITLE] := book.title;
    RecordItemsText[BL_BOOK_COLUMNS_YEAR] := book.year;
    if book.libraryBranch = '' then RecordItemsText[BL_BOOK_COLUMNS_ISSUE_ID] := DateToPrettyStr(book.issueDate)
    else if book.issueDate = 0 then RecordItemsText[BL_BOOK_COLUMNS_ISSUE_ID] := book.libraryBranch
    else RecordItemsText[BL_BOOK_COLUMNS_ISSUE_ID] := DateToPrettyStr(book.issueDate) + ' in ' + book.libraryBranch;
    if book.lend = false then begin
      if book.dueDate = -2 then RecordItemsText[BL_BOOK_COLUMNS_LIMIT_ID] := 'nie'
      else RecordItemsText[BL_BOOK_COLUMNS_LIMIT_ID] := 'erledigt'
    end else
     RecordItemsText[BL_BOOK_COLUMNS_LIMIT_ID] := DateToPrettyStr(book.dueDate);
    if book.owner<>nil then RecordItemsText[BL_BOOK_COLUMNS_ACCOUNT] := (book.owner as TCustomAccountAccess).prettyName
    else RecordItemsText[BL_BOOK_COLUMNS_ACCOUNT] := 'unbekannt';
    RecordItemsText[BL_BOOK_COLUMNS_STATUS]:=BookStatusToStr(book);//Abgegeben nach '+DateToSimpleStr(book.lastExistsDate))
    RecordItemsText[BL_BOOK_COLUMNS_ISBN] := book.isbn;

//    RecordItems.Add(book.year); ;
   // SubItems.add(book.otherInfo);
    RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR]:=chr(ord(getBookColor(book)) + ord('0'));
    if (groupingProperty <> '') and (RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR] > Parent.RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR]) then
      parent.RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR] := RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR];

    //else
    data.obj:=book;
  end;
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
  Columns.Clear;
  with Columns.Add do begin
    Text:='ID';
    Width:=80;
  end;
  with Columns.Add do begin
    Text:='Kategorie';
    Width:=50;
  end;
  with Columns.Add do begin
    Text:='Verfasser';
    Width:=120;
  end;
  with Columns.Add do begin
    Text:='Titel';
    Width:=150;
  end;
  with Columns.Add do begin
    Text:='Jahr';
    Width:=30;
  end;
  with Columns.Add do begin
    Text:='Ausleihe';
    Width:=70;
    Alignment:=taCenter;
  end;
  with Columns.Add do begin
    Text:='Frist';
    Width:=70;
    Alignment:=taCenter;
  end;
  with Columns.Add do begin
    Text:='Konto';
    Width:=80;
  end;
  with Columns.Add do begin
    Text:='Bemerkung';
    Width:=250;
  end;
  with Columns.Add do begin
    Text:='ISBN';
    Width:=80;
    Visible:=false;
  end;

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
