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

    procedure addBook(book: tbook);
    function getBook(i: integer): TBook;
 public
   constructor create(aowner: TComponent;showLendBooks: boolean);
   procedure clear;
   procedure addBookList(list: TBookList);
   property books[i:integer]: TBook read GetBook;

   function SelectedBook: TBooK;
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

uses applicationconfig, bbutils, libraryParser, Graphics;
//  ,windows {for the search only};
{ TBookListView }
function dateToWeek(date: longint):longint; //week: monday - sunday
begin
  Result:=(date-2) div 7;
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
       (dateToWeek(book.limitDate) <> lastWeek) then begin
      Items[i].RecordItemsText[BL_BOOK_EXTCOLUMNS_WEEK_SEPARATOR]:=IntToStr(abs(dateToWeek(book.limitDate) - lastWeek));
      lastWeek:=dateToWeek(book.limitDate);
     end else
      Items[i].RecordItemsText[BL_BOOK_EXTCOLUMNS_WEEK_SEPARATOR]:='';
  end;
  EndMultipleUpdate;
end;

procedure TBookListView.BookListCompareItems(sender: TObject; i1,
  i2: TTreeListItem; var compare: longint);
var book1,book2: TBook;
begin
  book1:=TBook(i1.data.obj);
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
        else if book1.issueDate>book2.issueDate then compare:=1;
    BL_BOOK_COLUMNS_LIMIT_ID:; //see later

    else compare:=CompareText(i1.RecordItemsText[SortColumn],i2.RecordItemsText[SortColumn]);
  end;
  if compare=0 then  //Sort LimitDate
    if book1.limitDate<book2.limitDate then
       compare:=-1
    else if book1.limitDate>book2.limitDate then
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

function getBookColor(book:TBook):TColor;
begin
  if book = nil then exit(colorTimeNear);
  if book.lend=false then
    result:=colorOld
  else if book.limitDate<=redTime then
    result:=colorTimeNear
  else if book.status in BOOK_NOT_EXTENDABLE then
    result:=colorLimited
  else result:=colorOK;
end;


procedure TBookListView.BookListCustomItemDraw(sender: TObject;
  eventTyp_cdet: TCustomDrawEventTyp; item: TTreeListItem; var defaultDraw: Boolean);
var pa: array[0..2] of tpoint;
    i,x,y,ypos:longint;
begin
  ypos:=TTreeListView(sender).DrawingYPos;
  case eventTyp_cdet of
    cdetPrePaint:
      if not item.SeemsSelected then begin
        canvas.Brush.Style:=bsSolid;
        Canvas.brush.color:=StringToColor(item.RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR]);
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


procedure TBookListView.addBook(book: tbook);
begin
  with items.Add do begin
    text:=book.id;
    RecordItems.Add(book.category);
    RecordItemsText[BL_BOOK_COLUMNS_AUTHOR] := book.author;
    RecordItemsText[BL_BOOK_COLUMNS_TITLE] := book.title;
    RecordItemsText[BL_BOOK_COLUMNS_YEAR] := book.year;
    RecordItemsText[BL_BOOK_COLUMNS_ISSUE_ID] := DateToPrettyStr(book.issueDate);
    if book.lend = false then begin
      if book.limitDate = -2 then RecordItemsText[BL_BOOK_COLUMNS_LIMIT_ID] := 'nie'
      else RecordItemsText[BL_BOOK_COLUMNS_LIMIT_ID] := 'erledigt'
    end else
     RecordItemsText[BL_BOOK_COLUMNS_LIMIT_ID] := DateToPrettyStr(book.limitDate);
    if book.owner<>nil then RecordItemsText[BL_BOOK_COLUMNS_ACCOUNT] := (book.owner as TCustomAccountAccess).prettyName
    else RecordItemsText[BL_BOOK_COLUMNS_ACCOUNT] := 'unbekannt';
    RecordItemsText[BL_BOOK_COLUMNS_STATUS]:=BookStatusToStr(book);//Abgegeben nach '+DateToStr(book.lastExistsDate))
    RecordItemsText[BL_BOOK_COLUMNS_ISBN] := book.isbn;

//    RecordItems.Add(book.year); ;
   // SubItems.add(book.otherInfo);
    RecordItemsText[BL_BOOK_EXTCOLUMNS_COLOR]:=ColorToString(getBookColor(book));

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


procedure TBookListView.clear;
begin
  Items.Clear;
  lastAddBook:=nil;
end;

procedure TBookListView.addBookList(list: TBookList);
var i:longint;
begin
  for i:=0 to list.Count-1 do
    addBook(list[i]);
end;

function TBookListView.SelectedBook: TBooK;
begin
  if Selected = nil then exit(nil);;
  Result := tbook(Selected.data.obj);
end;

end.
45076
8->killfocus
20->ERASEBKGND
66592=$10420->