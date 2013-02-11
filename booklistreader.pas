unit booklistreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,bbutils,extendedhtmlparser,simplehtmlparser,simplehtmltreeparser,simplexmlparser, xquery, dRegExpr,internetaccess, multipagetemplate;
  
type
  TBookList = class;
  TBookStatus=(bsNormal,bsUnknown,bsIsSearchedDONTUSETHIS,bsEarMarkedDONTUSETHIS, bsMaxLimitReachedDONTUSETHIS,bsProblematicInStr,bsCuriousInStr,bsAccountExpiredDONTUSETHIS);

  { TBook }

  TBook=class
  protected
    _referenceCount: longint;
    procedure decReference;
    procedure incReference;
  public
  //protected
    //persistent
    id,author,title,year:string; //schlüssel
    isbn: string;
    category,statusStr,otherInfo: string;
    issueDate,dueDate:longint;
    status: TBookStatus;
    lend: boolean;

 // public
    lastExistsDate,firstExistsDate:longint;
    owner: TObject;
  //  list: TBookList;

    //temporary

    charges: currency;
    additional: TProperties;
    
    constructor create;
    function equalToKey(compareTo: TBook): boolean;overload;
    function equalToKey(aid,aauthor,atitle,ayear:string):boolean;overload;
    
    procedure clear;
    procedure assignNoReplace(book: TBook); //every value not set will be replaced with the one from book
    function toSimpleString():string;
    function toLimitString():string;
    //procedure assignOverride(book: TBook);  //every value set in book will be replace the one of self
  end;
  
  TSaveAction = (saReplace, saAdd);

  { TBookList }

  TBookList = class(TFPList)
  private
    flendList: boolean;
    function getBook(i:longint):TBook; inline;
    procedure setLendList(const AValue: boolean);
  public
    owner: TObject;
    constructor create(aowner: TObject=nil);
    destructor Destroy; override;
    procedure delete(i:longint);
    function remove(book: tbook):longint; //pointer comparison, not key comparison
    procedure clear;
    procedure add(book: TBook);
    function add(id,title,author,year: string):TBook;
    procedure assign(alist: TBookList);
    procedure addList(alist: TBookList);

    procedure mergeMissingInformation(const old: TBookList);
    //procedure overrideOldInformation(const old: TBookList);
    procedure removeAllFrom(booksToRemove: TBookList); //key comparison, not pointer
    procedure removeAllExcept(booksToKeep: TBookList); //key comparison, not pointer
    
    function removeBook(book: TBook):longint; //key comparison
    function findBook(book:TBook):TBook; //key comparison (use indexOf for pointer comparison)
    function findBookIndex(book:TBook):longint; //key comparison (use indexOf for pointer comparison)
    function findBook(id,author,title,year:string):TBook;
    
    procedure load(fileName: string);
    procedure save(fileName: string; saveAction: TSaveAction);
    
    function lastCheck: longint;
    function nextLimitDate(const extendable: boolean = true): longint;
    
    property books[i:longint]: TBook read getBook; default;
    property lendList: boolean read flendList write setLendList;
  end;

  EBookListReader=class(Exception)
    details:string;
    constructor create;
    constructor create(s:string;more_details:string='');
  end;

  { TBookListReader }

  TBookListReader = class(TMultipageTemplateReader)
  private
    currentBook,defaultBook: TBook;
    procedure setBookProperty(book:TBook;variable: string; value:IXQValue);
    procedure parserVariableRead(variable: string; value: IXQValue);
    procedure logall(sender: TMultipageTemplateReader; logged: string; debugLevel: integer=0);
  protected
    procedure processPage(page, cururl, contenttype: string); override;
  public
    bookAccessSection: ^TRTLCriticalSection;
    books: TBookList;
    constructor create(atemplate:TMultiPageTemplate);
    destructor destroy();override;

    function bookToPXP(book:TBook): TXQValueObject;
    procedure selectBook(book:TBook);
  end;
  
const BOOK_NOT_EXTENDABLE=[bsProblematicInStr,bsEarMarkedDONTUSETHIS,bsMaxLimitReachedDONTUSETHIS,bsAccountExpiredDONTUSETHIS];
      BOOK_EXTENDABLE=[bsNormal,bsCuriousInStr];

function BookStatusToStr(book: TBook;verbose:boolean=false): string; //returns utf8


implementation
uses bbdebugtools, applicationconfig;

const XMLNamespaceURL_VideLibri = 'http://www.benibela.de/2013/videlibri/';
var XMlNamespaceVideLibri, XMlNamespaceVideLibri_VL: INamespace;

function BookStatusToStr(book: TBook;verbose:boolean=false): string;
begin
  if book.lend =false then
    if verbose then exit('nicht ausgeliehen') else exit('');
  case book.Status of
    bsNormal: if verbose then exit('normal verlängerbar') else exit('');
    bsUnknown: exit('Ausleihstatus unbekannt');
    bsIsSearchedDONTUSETHIS: exit('Ausleihstatus wird ermittelt... (sollte nicht vorkommen, bitte melden!)');
//    bsEarMarked:exit('vorgemerkt');
//    bsMaxLimitReached: exit('maximale Ausleihfrist erreicht');
//    bsAccountExpired: exit('Büchereikarte ist abgelaufen');
    bsCuriousInStr: if verbose then exit('verlängerbar: '+book.statusStr) else exit(book.statusStr);
    bsProblematicInStr: if verbose then exit('nicht verlängerbar: '+book.statusStr) else exit(book.statusStr);
    else exit('Unbekannter Fehler, bei Ausleihstatusermittlung! Bitte melden!');
  end;

end;


{ TBook }

procedure TBook.decReference;
begin
  _referenceCount-=1;
  if _referenceCount<=0 then free;
end;

procedure TBook.incReference;
begin
  _referenceCount+=1;
end;

constructor TBook.create;
begin
  _referenceCount:=1;
  status:=bsUnknown;
end;

function TBook.equalToKey(compareTo: TBook): boolean;
begin
  result:=(id=compareTo.id) and (title=compareTo.title) and
          (author=compareTo.author) and (year=compareTo.year);
end;

function TBook.equalToKey(aid, aauthor, atitle, ayear: string): boolean;
begin
  result:=(id=aid) and (title=atitle) and
          (author=aauthor) and (year=ayear);
end;

procedure TBook.clear;
begin
  Id:='';
  category:='';
  Title:='';
  Author:='';
  year:='';
  StatusStr:='';
  Status:=bsUnknown;
  dueDate:=0;
  issueDate:=0;
  SetLength(Additional,0);
end;

procedure TBook.assignNoReplace(book: TBook);
var i:longint;
begin
  if book=nil then exit;
  if category='' then category:=book.category;
  if isbn='' then isbn:=book.isbn;
  if statusStr='' then statusStr:=book.statusStr;
  if otherInfo='' then otherInfo:=book.otherInfo;
  if issueDate=0 then issueDate:=book.issueDate;
  if dueDate=0 then dueDate:=book.dueDate;
  if status=bsUnknown then status:=book.status;
  if charges=0 then charges:=book.charges;
  if lastExistsDate=0 then lastExistsDate:=book.lastExistsDate;
  if (firstExistsDate=0) or ((book.firstExistsDate<>0) and (book.firstExistsDate<firstExistsDate)) then
    firstExistsDate:=book.firstExistsDate;
  for i:=0 to high(book.additional) do
    if getProperty(book.additional[i].name,additional)='' then
      addProperty(book.additional[i].name,book.additional[i].value,additional);
end;

function TBook.toSimpleString():string;
begin
  result:=id+' - '+author+' * '+ title;
end;

function TBook.toLimitString(): string;
begin
  result:=toSimpleString() + '  => '+DateToPrettyStr(dueDate);
end;

  {
procedure TBook.assignOverride(book: TBook);
var i:longint;
begin
  if book=nil then exit;
  if book.category<>'' then category:=book.category;
  if book.statusStr<>'' then statusStr:=book.statusStr;
  if book.otherInfo<>'' then otherInfo:=book.otherInfo;
  if book.issueDate<>0 then issueDate:=book.issueDate;
  if book.dueDate<>0 then dueDate:=book.dueDate;
  if book.status<>bsUnknown then status:=book.status;
  if book.charges<>0 then charges:=book.charges;
  if book.lastExistsDate<>0 then lastExistsDate:=book.lastExistsDate;
  if (firstExistsDate=0) or ((book.firstExistsDate<>0) and (book.firstExistsDate<firstExistsDate)) then
    firstExistsDate:=book.firstExistsDate;
  for i:=0 to high(book.additional) do
    setProperty(book.additional[i].name,book.additional[i].value,additional);
end;
   }

{ TBookList }


function TBookList.getBook(i: longint): TBook; inline;
begin
  result:=TBook(Items[i]);
end;

procedure TBookList.setLendList(const AValue: boolean);
var i:longint;
begin
  flendList:=AValue;
  for i:=0 to Count-1 do
    books[i].lend:=AValue;
end;

constructor TBookList.create(aowner: TObject);
begin
  owner:=aowner;
end;

destructor TBookList.Destroy;
begin
  clear;
  inherited Destroy;
end;


procedure TBookList.delete(i: longint);
begin
  books[i].decReference;
  inherited delete(i);
end;

function TBookList.remove(book: tbook):longint;
begin
  Result := IndexOf(book);
  If Result <> -1 then
    Self.Delete(Result);
end;

procedure TBookList.clear;
var i:longint;
begin
  for i:=0 to Count-1 do
    books[i].decReference;
  inherited clear;
end;

procedure TBookList.add(book: TBook);
begin
  if book.owner=nil then begin
    book.owner:=owner;
    book.lend:=lendList;
  end;
  book.incReference;
  inherited add(book);
end;

function TBookList.add(id, title, author, year: string): TBook;
begin
  result:=TBook.Create;
  result.id:=id;
  result.title:=title;
  Result.author:=author;
  Result.year:=year;
  Result.owner:=owner;
  result.lend:=lendList;
  inherited Add(Result);
end;

procedure TBookList.assign(alist: TBookList);
begin
  clear;
  AddList(alist);
end;

procedure TBookList.addList(alist: TBookList);
var i:longint;
begin
  inherited AddList(alist);
  for i:=0 to alist.count-1 do
    alist[i].incReference;
end;

procedure TBookList.mergeMissingInformation(const old: TBookList);
var i:longint;
begin
  //TODO: Optimize to O(n log n)
  for i:=0 to count-1 do
    books[i].assignNoReplace(old.findBook(books[i]));
end;

{procedure TBookList.overrideOldInformation(const old: TBookList);
var i:longint;
begin
  //TODO: Optimize to O(n log n)
  for i:=0 to count-1 do
    books[i].assignOverride(old.findBook(books[i]));
end;}

procedure TBookList.removeAllFrom(booksToRemove: TBookList);
var i:longint;
begin
  for i:=0 to booksToRemove.count-1 do
    removeBook(booksToRemove[i]);
end;

procedure TBookList.removeAllExcept(booksToKeep: TBookList);
var i:longint;
begin
  //TODO: Optimize to O(n log n)
  for i:=count-1 downto 0 do
    if booksToKeep.findBook(books[i]) = nil then
      delete(i);
end;

function TBookList.removeBook(book: TBook):longint;
begin
  Result:=findBookIndex(book);
  if Result<>-1 then delete(Result);
end;

function TBookList.findBook(book: TBook): TBook;
var i:longint;
begin
  for i:=0 to count-1 do
    if books[i].equalToKey(book) then
      exit(books[i]);
  Result:=nil;
end;

function TBookList.findBookIndex(book: TBook): longint;
var i:longint;
begin
  for i:=0 to count-1 do
    if books[i].equalToKey(book) then
      exit(i);
  Result:=-1;
end;

function TBookList.findBook(id, author, title, year: string): TBook;
var i:longint;
begin
  for i:=0 to count-1 do
    if books[i].equalToKey(id,author,title,year) then
      exit(books[i]);
  Result:=nil;
end;

procedure TBookList.load(fileName: string);
  function truncNull(var source: string):string;
  var p:integer;
  begin
    p:=pos(#0,source);
    result:=copy(source,1,p-1);
    system.delete(source,1,p);
  end;
  function truncNullDef(var source: string;def:string):string;
  var p:integer;
  begin
    p:=pos(#0,source);
    if p<=0 then exit(def);
    result:=copy(source,1,p-1);
    system.delete(source,1,p);
  end;
var sl:TStringList;
    line:string;
    i:integer;
    book:TBook;
begin
  if logging then
    log('TBookList.load('+fileName+') started');
  if not FileExists(fileName) then exit;
  clear;
  sl:=TStringList.create;
  sl.LoadFromFile(fileName);
  Capacity:=sl.count;
  for i:=0 to sl.count-1 do begin
    book:=TBook.Create;
    with book do begin
      line:=sl[i];
      id:=truncNull(line);
      category:=truncNull(line);
      author:=truncNull(line);
      title:=truncNull(line);
      statusStr:=truncNull(line);
      otherInfo:=truncNull(line);
      issueDate:=StrToInt(truncNull(line));
      dueDate:=StrToInt(truncNull(line));
      lastExistsDate:=StrToInt(truncNull(line));
      status:=TBookStatus(StrToInt(truncNull(line)));
      year:=truncNullDef(line,'');
      firstExistsDate:=StrToInt(truncNullDef(line,'0'));
      isbn:=truncNullDef(line,'');
      lend:=self.lendList;
      owner:=self.owner;
      //list:=self;
    end;
    inherited add(book);
  end;
  sl.free;
  if logging then
    log('TBookList.load('+fileName+') ended')
end;

procedure TBookList.save(fileName: string; saveAction: TSaveAction);
var text:TextFile;
    i:integer;
begin
  if logging then
    log('TBookList.save('+fileName+') started');
  if (saveAction<>saReplace) and not FileExists(fileName) then saveAction:=saReplace;
  AssignFile(text,fileName);
  if saveAction=saReplace then rewrite(text)
  else Append(text);
  for i:=0 to count-1 do
    with books[i] do begin
      if not (status in [bsProblematicInStr,bsCuriousInStr]) then statusStr:='';
      writeln(text,id+#0+category+#0+author+#0+title+#0+statusStr+#0+otherInfo+#0+
                   IntToStr(issueDate)+#0+IntToStr(dueDate)+#0+
                   IntToStr(lastExistsDate)+#0+inttostr(integer(status))+#0+year+#0+IntToStr(firstExistsDate)+#0+isbn+#0)
    end;
  close(text);
  if logging then
    log('TBookList.save('+fileName+') ended')
end;

function TBookList.lastCheck: longint;
var i:longint;
begin
  Result:=MaxInt;
  for i:=0 to count-1 do
    if books[i].lastExistsDate<result then result:=books[i].lastExistsDate;
end;

function TBookList.nextLimitDate(const extendable: boolean): longint;
var i:longint;
begin
  Result:=MaxInt;
  for i:=0 to count-1 do
    if (books[i].dueDate > 0) and (books[i].dueDate < Result) and
      (extendable or (books[i].status in BOOK_NOT_EXTENDABLE)) then
        result:=books[i].dueDate;
end;



{ TBookListTemplate }



    { TBookListReader }

procedure TBookListReader.logall(sender: TMultipageTemplateReader; logged: string; debugLevel: integer=0);
begin
  log(logged);
end;

procedure TBookListReader.processPage(page, cururl, contenttype: string);
var
  varlog: TXQVariableChangeLog;
  j: Integer;
begin
  if bookAccessSection<>nil then EnterCriticalsection(bookAccessSection^);
  try
    inherited;

    //simulate old parser interface
    varlog := parser.VariableChangeLogCondensed;
    for j:=0 to varlog.count-1 do
      parserVariableRead(varlog.getName(j), varlog.get(j));
  finally
    if bookAccessSection<>nil then LeaveCriticalsection(bookAccessSection^);
  end;
end;

procedure TBookListReader.setBookProperty(book: TBook; variable: string; value:IXQValue);
  function strconv():string;
  begin
    result:=StringReplace(value.toString,#13,'',[rfReplaceAll]);
    result:=StringReplace(result,#10,'',[rfReplaceAll]);
    result:=trim(result);
  end;
begin
  if variable='category' then book.Category:=strconv()
  else if variable='id' then book.Id:=strconv()
  else if variable='author' then book.Author:=strconv()
  else if variable='title' then book.Title:=strconv()
  else if variable='year' then book.Year:=strconv()
  else if variable='isbn' then book.isbn:=strconv()
  else if strlibeginswith(@variable[1],length(variable),'status') then begin
    book.StatusStr:=strconv();
    if variable='status:problematic' then book.Status:=bsProblematicInStr
    else if variable='status:curious' then book.Status:=bsCuriousInStr
    else if pos(':', variable) > 0 then book.statusStr:=book.statusStr + ' Achtung: Ungültige Statusvariable "' + variable + '" in Template'
    else book.status := bsNormal;
  end else if striEqual(variable, 'issuedate') then book.issueDate:=trunc(value.toDateTime)
  else if striEqual(variable, 'duedate') then
    book.dueDate:=trunc(value.toDateTime)
  else if strlibeginswith(@variable[1],length(variable),'issuedate') then
    book.IssueDate:=dateParse(strconv(),strcopyfrom(variable,pos(':',variable)+1))
  else if strlibeginswith(@variable[1],length(variable),'duedate') then
    book.dueDate:=dateParse(strconv(),strcopyfrom(variable,pos(':',variable)+1))
  else if striEqual(variable, 'limitdate') or strlibeginswith(@variable[1],length(variable),'limitdate') then
    raise EBookListReader.create('The template is using the limitdate property which is deprecated. It should now be called duedate')
  else
    setProperty(variable,strconv(),book.Additional);
end;

procedure TBookListReader.parserVariableRead(variable: string; value: IXQValue);
  function strconv(s:string):string;
  begin
    result:=StringReplace(s,#13,'',[rfReplaceAll]);
    result:=StringReplace(result,#10,'',[rfReplaceAll]);
    result:=trim(result);
  end;


var
 i: Integer;
 book: TXQValueObject;
 temp2: IXQValue;
 temp: TXQValue;
 s: string;
begin
  if logging then
    log('** Read variable: "'+variable+'" = "'+value.debugAsStringWithTypeAnnotation+'"');
  if variable='delete-current-books()' then begin
    books.clear();
  end else if variable='book-start()' then begin
  //reset
    //currentBook:=defaultBook;
    //currentBook.clear;
   raise EBookListReader.create('Deprecated book-start');
  end else if variable='book-end()' then begin
    raise EBookListReader.create('Deprecated book-end');
    {currentBook.firstExistsDate:=trunc(now);
    currentBook.lastExistsDate:=trunc(now);
    books
      .add(currentBook.Id,currentBook.Title,currentBook.Author,currentBook.Year)
         .assignNoReplace(currentBook);                                         }
  end else if variable='book-select(id)' then begin
   raise EBookListReader.create('Deprecated book-select(id)');
    {currentBook := nil;
    for i:=0 to books.Count-1 do
      if books[i].id = value then
        currentBook:=books[i];
    if currentBook=nil then begin
      if logging then for i:=0 to books.Count-1 do log('Book-Id: "'+books[i].id + '" <> "'+value+'"');
      raise EBookListReader.create('Template wants to select book '+value+', but it doesn''t exist');
    end;}
  end else if variable='book-select()' then begin
    //reset
    raise EBookListReader.create('not implemented yet');
  end else if (variable='raise()') or (variable = 'raise-login()') then begin
    raise EBookListReader.create(LineEnding + LineEnding + value.toString);
  end else if variable = 'book' then begin
    if not (value is TXQValueObject) then raise EBookListReader.Create('Buch ohne Eigenschaften');
    temp2 := value.clone;
    book := temp2 as TXQValueObject;
    if book.hasProperty('_existing', @temp) then begin
      if not temp.toBoolean then raise EBookListReader.create('Das Buch hat einen _existing Marker, aber er sagt, das Buch existiere nicht : '+temp.debugAsStringWithTypeAnnotation());
      if currentBook = nil then raise EBookListReader.Create('Das Template will ein existierendes Buch verändert, aber mir ist kein Buch bekannt.');
    end else if book.hasProperty('select(id)', @temp) then begin
      s := temp.toString;
      currentBook := nil;
      for i:=0 to books.Count-1 do
        if books[i].id = s then
          currentBook:=books[i];
      if currentBook=nil then begin
        if logging then for i:=0 to books.Count-1 do log('Book-Id: "'+books[i].id + '" <> "'+s+'"');
        raise EBookListReader.create('Template wants to select book '+s+', but it doesn''t exist');
      end;
    end else if book.hasProperty('select(new)', @temp) or book.hasProperty('select(current)', @temp) then
      raise EBookListReader.Create('Das Template hat die Bucheigenschaften select(new) oder select(current) gesetzt, aber in der neuesten Version, werden sie nicht länger benötigt)')
    else begin
      currentBook := defaultBook;
      currentBook.clear;
    end;
    for i:=0 to book.values.count-1 do begin
      s := book.values.getName(i);
      if (s = '_existing') or (s = 'select(id)') or (s = 'select(current)') or (s = 'select(new)')  then continue;
      setBookProperty(currentBook,s,book.values.get(i));
    end;
    currentBook.firstExistsDate:=trunc(now);
    currentBook.lastExistsDate:=trunc(now);
    if currentBook = defaultBook then  begin
      books
        .add(currentBook.Id,currentBook.Title,currentBook.Author,currentBook.Year)
           .assignNoReplace(currentBook);
    end;
    temp2:=nil;
  end;
end;

constructor TBookListReader.create(atemplate:TMultiPageTemplate);
begin
  inherited create(atemplate, nil);
  defaultBook:=TBook.create;
  if logging then onLog:=@logall;
  parser.QueryEngine.GlobalNamespaces.add(XMlNamespaceVideLibri);
  parser.QueryEngine.GlobalNamespaces.add(XMlNamespaceVideLibri_VL);
end;

destructor TBookListReader.destroy();
begin
  defaultBook.free;
  inherited destroy();
end;

function TBookListReader.bookToPXP(book: TBook): TXQValueObject;
var
  i: Integer;
begin
  result := TXQValueObject.create();
  result.setMutable('id', book.id);
  result.setMutable('author', book.author);
  result.setMutable('title', book.title);
  result.setMutable('year', book.year);
  result.setMutable('isbn', book.isbn);
  for i:=0 to high(book.additional) do
    result.setMutable(book.additional[i].name, book.additional[i].value);
  result.setMutable('_existing', xqvalueTrue);
end;

procedure TBookListReader.selectBook(book: TBook);
begin
  parser.variableChangeLog.add('book', bookToPXP(book));
  currentBook:=book;
end;

{ EBookListReader }

constructor EBookListReader.create;
begin

end;

constructor EBookListReader.create(s: string; more_details: string);
begin
  Message:=s;
  details:=more_details;
end;

function xqFunctionDelete_Current_Books(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0);
  context.staticContext.sender.OnDefineVariable(nil, 'delete-current-books()', xqvalueTrue);
  result := xqvalue();
end;

function xqFunctionRaise_Login(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then context.staticContext.sender.OnDefineVariable(nil, 'raise-login()', xqvalue())
  else context.staticContext.sender.OnDefineVariable(nil, 'raise-login()', args[0]);
  result := xqvalue();
end;

function xqFunctionRaise(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  requiredArgCount(args, 0, 1);
  if length(args) = 0 then context.staticContext.sender.OnDefineVariable(nil, 'raise()', xqvalue())
  else context.staticContext.sender.OnDefineVariable(nil, 'raise()', args[0]);
  result := xqvalue();
end;

var vl: TXQNativeModule;
initialization
  XMlNamespaceVideLibri := TNamespace.create(XMLNamespaceURL_VideLibri, 'videlibri');
  XMlNamespaceVideLibri_VL := TNamespace.create(XMLNamespaceURL_VideLibri, 'vl');
  vl := TXQNativeModule.Create(XMLNamespaceVideLibri);
  vl.registerFunction('delete-current-books', @xqFunctionDelete_Current_Books, []);
  vl.registerFunction('raise', @xqFunctionRaise, []);
  vl.registerFunction('raise-login', @xqFunctionRaise_Login, []);
  TXQueryEngine.registerNativeModule(vl);
end.

