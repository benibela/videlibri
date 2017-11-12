unit booklistreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,bbutils,extendedhtmlparser,simplehtmltreeparser,simplexmlparser, xquery, internetaccess, multipagetemplate, xquery__regex;
  
type
  TBookList = class;
  TBookStatus=(
    //self lend
    bsNormal,bsUnknown,bsIsSearchedDONTUSETHIS,bsEarMarkedDONTUSETHIS, bsMaxLimitReachedDONTUSETHIS,bsProblematicInStr,bsCuriousInStr,bsAccountExpiredDONTUSETHIS,bsOrdered,bsProvided,
    //availability
    bsAvailable, bsLend, bsVirtual, bsPresentation, bsInterLoan);
  trilean = (tUnknown, tFalse, tTrue);

  { TBook }

  TSerializeStringProperty = procedure (const name: string; const value: string) of object;
  TSerializeDateProperty = procedure (const name: string; date: integer) of object;
  TCustomBookOwner = class
    //todo: move TCustomAccountAccess in the same unit as TBook (probably move both in a new unit)
    //till moved, use this class as place holder type
  protected
    FPrettyName: string;
  public
    property prettyName: string read FPrettyName write FPrettyName;
  end;

  TBook=class
  private
    function GetOwningAccount: TCustomBookOwner;
    procedure SetOwningAccount(AValue: TCustomBookOwner);
  protected
    _referenceCount: longint;
    owner: TObject; //account
  public
    procedure decReference;
    procedure incReference;
  public
  //protected
    //persistent
    id,author,title,year:string; //schlüssel
    libraryBranch: string; //branch of the library ("Filiale")
    isbn: string;
    category,statusStr{,otherInfo}: string;
    issueDate,dueDate:longint;
    status: TBookStatus;
    cancelable: trilean;
    lend: boolean;
    renewCount: integer;

    //*how to add a new property: define it, update: assignNoReplace, clear, serialize, setProperty

 // public
    lastExistsDate,firstExistsDate:longint;
  //  list: TBookList;

    //temporary
    holdings: TBookList;
    charges: currency;
    additional: TProperties;
    
    function owningBook: TBook;
    property owningAccount: TCustomBookOwner read GetOwningAccount write SetOwningAccount;

    constructor create;
    function equalToKey(compareTo: TBook): boolean;overload;
    function equalToKey(aid,aauthor,atitle,ayear:string):boolean;overload;

    procedure serialize(str: TSerializeStringProperty; date: TSerializeDateProperty);

    procedure clear;
    destructor Destroy; override;
    procedure assign(book: TBook);        //assigns everything except key fields
    procedure assignAll(book: TBook);     //assigns everything
    procedure assignIfNewer(book: TBook); //assigns from the newer book, also take min of issue/exist date
    procedure mergePersistentFields(book: TBook);
    function clone: TBook;

    function getNormalizedISBN(const removeSeps, convertTo13: boolean): string;

    function toSimpleString():string;
    function toLimitString():string;
    //procedure assignOverride(book: TBook);  //every value set in book will be replace the one of self

    procedure setProperty(const name, value: string);
    function getProperty(const name: string; const def: string = ''): string;
    function getPropertyAdditional(const name: string; const def: string = ''): string; inline;
  end;
  
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

    procedure mergePersistentFields(const old: TBookList);
    //procedure overrideOldInformation(const old: TBookList);
    procedure removeAllFrom(booksToRemove: TBookList); //key comparison, not pointer
    procedure removeAllExcept(booksToKeep: TBookList); //key comparison, not pointer
    
    function removeBook(book: TBook):longint; //key comparison
    function findBook(book:TBook):TBook; //key comparison (use indexOf for pointer comparison)
    function findBookIndex(book:TBook):longint; //key comparison (use indexOf for pointer comparison)
    function findBook(id,author,title,year:string):TBook;
    
    procedure load(fileName: string);
    procedure save(fileName: string);
    function toXQuery: IXQValue;
    
    function lastCheck: longint;
    function nextLimitDate(const extendable: boolean = true): longint;
    
    property books[i:longint]: TBook read getBook; default;
    property lendList: boolean read flendList write setLendList;
  end;

  TPendingMessageKind = (pmkConfirm, pmkChoose);
  TPendingMessage = class
    kind: TPendingMessageKind;
    callback, caption: string;
    options, optionValues: TStringArray;
  end;

  { TBookListReader }

  TBookListReader = class(TMultipageTemplateReader)
  private
    currentBook,defaultBook: TBook;
    class procedure setAllBookProperties(book:TBook; const value:IXQValue); static;
    class procedure setBookProperty(book:TBook;variable: string; const value:IXQValue); static;
    procedure parserVariableRead(variable: string; value: IXQValue);
    procedure logall(sender: TMultipageTemplateReader; logged: string; debugLevel: integer=0);
  protected
    procedure applyPattern(pattern, name: string); override;
    function evaluateQuery(const query: IXQuery): IXQValue; override;
    procedure setVariable(name: string; value: IXQValue; namespace: string=''); override;
  public
    bookAccessSection: ^TRTLCriticalSection;
    books: TBookList;
    pendingMessage: TPendingMessage;
    constructor create(atemplate:TMultiPageTemplate);
    destructor destroy();override;

    class function bookToPXP(book:TBook): TXQValueObject; static;
    procedure selectBook(book:TBook);
  end;

  { TXQVideLibriStaticContext }

  TXQVideLibriStaticContext = class(TXQStaticContext)
  private
    bookListReader: TBookListReader;
  public
    constructor Create(abookListReader: TBookListReader);
    function clone: TXQStaticContext; override;
  end;

const BOOK_NOT_EXTENDABLE=[bsProblematicInStr,bsEarMarkedDONTUSETHIS,bsMaxLimitReachedDONTUSETHIS,bsAccountExpiredDONTUSETHIS];
      BOOK_EXTENDABLE=[bsNormal,bsCuriousInStr];
      BOOK_NOT_LEND=[bsOrdered, bsProvided];

function BookStatusToStr(book: TBook;verbose:boolean=false): string; //returns utf8


implementation
uses math, bbdebugtools, simplehtmlparser, applicationconfig, xquery_json//<- enable JSON
  ;

resourcestring
  rsBookStatusInvalid = 'Ungültiger Bücherstatus: %s';
  rsBookStatusAvailable = 'verfügbar';
  rsBookStatusLend = 'ausgeliehen';
  rsBookStatusVirtual = 'E-Book/sonstiges';
  rsBookStatusPresentation = 'Präsenzbestand';
  rsBookStatusInterloan = 'fernleihbar';


const XMLNamespaceURL_VideLibri = 'http://www.benibela.de/2013/videlibri/';
var XMlNamespaceVideLibri, XMlNamespaceVideLibri_VL: INamespace;

function BookStatusToStr(book: TBook;verbose:boolean=false): string;
begin
  if book.lend  then begin
    case book.Status of
      bsNormal: if verbose then exit(rsBookStatusNormalRenewable) else exit('');
  //    bsUnknown: exit('Ausleihstatus unbekannt');
  //    bsIsSearchedDONTUSETHIS: exit('Ausleihstatus wird ermittelt... (sollte nicht vorkommen, bitte melden!)');
  //    bsEarMarked:exit('vorgemerkt');
  //    bsMaxLimitReached: exit('maximale Ausleihfrist erreicht');
  //    bsAccountExpired: exit('Büchereikarte ist abgelaufen');
      bsCuriousInStr: if verbose then exit(rsRenewable + ': '+book.statusStr) else exit(book.statusStr);
      bsProblematicInStr: if verbose then exit(rsBookStatusNonRenewable + ': '+book.statusStr) else exit(book.statusStr);
      bsOrdered: if book.statusStr <> '' then exit(book.statusStr) else exit(rsBookStatusOrdered);
      bsProvided: if book.statusStr <> '' then exit(book.statusStr) else exit(rsBookStatusProvided);
      bsAvailable, bsLend, bsVirtual, bsPresentation, bsInterLoan: exit(rsBookStatusNotLend)
      else exit(format(rsBookStatusInvalid, [inttostr(ord(book.status))]));
    end;
    if verbose then exit(rsBookStatusNotLend) else exit('');
  end else
    case book.Status of
      bsNormal, bsUnknown, bsCuriousInStr: exit(book.statusStr);
      bsAvailable: exit(rsBookStatusAvailable);
      bsLend: exit(rsBookStatusLend);
      bsVirtual: exit(rsBookStatusVirtual);
      bsPresentation: exit(rsBookStatusPresentation);
      bsInterLoan: exit(rsBookStatusInterloan);
      else exit('???????');
    end;
end;

function BookStatusToSerializationStr(status: TBookStatus): string;
begin
  case status of
    bsNormal: exit('normal');
    bsUnknown: exit('unknown');
    bsProblematicInStr: exit('critical');
    bsCuriousInStr: exit('curious');
    bsOrdered: exit('ordered');
    bsProvided: exit('provided');
    bsAvailable: exit('available');
    bsLend: exit('lend');
    bsVirtual: exit('virtual');
    bsPresentation: exit('presentation');
    bsInterLoan: exit('interloan');
    else exit('--invalid--'+inttostr(integer(status)));
  end;
end;

{ TXQVideLibriStaticContext }

constructor TXQVideLibriStaticContext.Create(abookListReader: TBookListReader);
begin
  bookListReader := abookListReader;
end;

function TXQVideLibriStaticContext.clone: TXQStaticContext;
begin
  Result:=TXQVideLibriStaticContext.Create(bookListReader);
  result.assign(self);
end;

{ TBook }

function TBook.GetOwningAccount: TCustomBookOwner;
var
  b: TBook;
begin
  b := owningBook;
  if b.owner is TCustomBookOwner then result := TCustomBookOwner(b.owner)
  else result := nil;
end;

procedure TBook.SetOwningAccount(AValue: TCustomBookOwner);
var
  b: TBook;
begin
  b := owningBook;
  if b.owner=AValue then Exit;
  b.owner:=AValue;
end;

procedure TBook.decReference;
begin
  _referenceCount-=1;
  if _referenceCount<=0 then free;
end;

procedure TBook.incReference;
begin
  _referenceCount+=1;
end;

function TBook.owningBook: TBook;
begin
  result := self;
  while result.owner is TBook do result := tbook(result.owner);
end;

constructor TBook.create;
begin
  _referenceCount:=1;
  status:=bsUnknown;
  cancelable:=tUnknown;
  renewCount := -1;
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

procedure TBook.serialize(str: TSerializeStringProperty; date: TSerializeDateProperty);
begin
  if Assigned(str) then begin
    str('id', id);
    str('libraryBranch', libraryBranch);
    str('author', author);
    str('title', title);
    str('isbn', isbn);
    str('year', year);
    str('category', category);
    str('status', statusStr);
    //str('otherInfo', otherInfo);
    str('statusId', BookStatusToSerializationStr(status));
    if renewCount > 0 then str('renewCount', inttostr(renewCount));
    case cancelable of
      tUnknown: str('cancelable', '?');
      tTrue: str('cancelable', 'true');
      tFalse: str('cancelable', 'false');
    end;
  end;
  if assigned(date) then begin
    date('issueDate', issueDate);
    date('dueDate', dueDate);
    date('_lastExistsDate', lastExistsDate);
    date('_firstExistsDate', firstExistsDate);
  end;
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
  cancelable:=tUnknown;
  dueDate:=0;
  issueDate:=0;
  SetLength(Additional,0);
  FreeAndNil(holdings);
end;

destructor TBook.Destroy;
var
  i: Integer;
begin
  if holdings <> nil then begin
    //safety check
    for i := 0 to holdings.Count - 1 do
      if holdings[i].owner = self then holdings[i].owner := owner;
    holdings.Free;
  end;
  inherited Destroy;
end;

procedure TBook.assign(book: TBook);
var
  temp: TBook;
  i: Integer;
begin
  if (book=nil) or (book = self) then exit;
  category:=book.category;
  libraryBranch:=book.libraryBranch;
  isbn:=book.isbn;
  statusStr:=book.statusStr;
  issueDate:=book.issueDate;
  dueDate:=book.dueDate;
  status:=book.status;
  charges:=book.charges;
  lastExistsDate:=book.lastExistsDate;
  if (firstExistsDate=0) or ((book.firstExistsDate<>0) and (book.firstExistsDate<firstExistsDate)) then
    firstExistsDate:=book.firstExistsDate;
  cancelable:=book.cancelable;
  renewCount := book.renewCount;
  additional := book.additional;
  SetLength(additional, length(additional));
  if book.holdings <> nil then begin
    if holdings = nil then holdings := TBookList.create(self);
    holdings.clear;
    holdings.Capacity:=book.holdings.Count;
    for i := 0 to book.holdings.Count - 1 do begin
      temp := book.holdings[i].clone;
      holdings.add(temp);
      temp.decReference;
    end;
  end;
end;

procedure TBook.assignAll(book: TBook);
begin
  if (book=nil) or (book = self) or (self = nil) then exit;
  assign(book);
  author:=book.author;
  title:=book.title;
  year:=book.year;
  id:=book.id;
end;

procedure TBook.assignIfNewer(book: TBook);
begin
  if (book=nil) or (book = self) or (self = nil) then exit;
  if (issueDate <> 0) and (book.issueDate <> 0) then  issueDate:=min(issueDate, book.issueDate);
  if (firstExistsDate <> 0) and (book.firstExistsDate <> 0) then firstExistsDate:=min(firstExistsDate, book.firstExistsDate);
  if book.lastExistsDate > lastExistsDate then assign(book)
  else if book.lastExistsDate = lastExistsDate then begin
    dueDate:=max(dueDate, book.dueDate);
  end;
end;

procedure TBook.mergePersistentFields(book: TBook);
begin
  if (book=nil) or (book = self) or (self = nil) then exit;
  if (firstExistsDate <> 0) and (book.firstExistsDate <> 0) then firstExistsDate:=min(firstExistsDate, book.firstExistsDate);
end;

function TBook.clone: TBook;
begin
  result := TBook.create;
  result.assignAll(self);
end;

function TBook.getNormalizedISBN(const removeSeps, convertTo13: boolean): string;
var
  check: Integer;
  multiplier: Integer;
  i: Integer;
begin
  result := trim(isbn);
  if length(result) >= 5 then begin
    //see https://en.wikipedia.org/wiki/List_of_ISBN_identifier_groups
    //X can mean 0

    //e.g. isbn10: 3-680-08783-7
    if result[2] = '-' then begin
      result := copy(result, 1, 13);
      if convertTo13 then begin
        if strBeginsWith(result, '1') and ( strBeginsWith(result, '10-') or strBeginsWith(result, '11-') or strBeginsWith(result, '12-') ) then
          result := '979-' + result
         else
          result := '978-' + result;
        check := 0;
        multiplier := 1;
        for i := 1 to length(result) - 1 do
          if result[i] in ['0'..'9'] then begin
            check += multiplier * (ord(result[i]) - ord('0'));
            multiplier := (multiplier + 2) and 3;
          end;
        result[length(result)] := chr(ord('0') + (10 - check mod 10) mod 10);
      end;
    end;
    //isbn13: 978-3-7657-2781-8
    if result[4] in ['-', ' '] then result := copy(result, 1, 17);
  end;

  if removeSeps then
    Result := StringReplace(StringReplace(result, '-', '', [rfReplaceAll]), ' ', '', [rfReplaceAll]);
end;

function TBook.toSimpleString():string;
begin
  result:=id+' - '+author+' * '+ title;
end;

function TBook.toLimitString(): string;
begin
  result:=toSimpleString() + '  => '+DateToPrettyStr(dueDate);
end;

procedure TBook.setProperty(const name, value: string);
begin
  case lowercase(name) of
    'category': Category:=value;
    'librarybranch': libraryBranch := value;
    'id': Id:=value;
    'author': Author:=value;
    'title': Title:=value;
    'year': Year:=value;
    'isbn': isbn:=value;
    'statusid':
        case value of
          'curious': status:=bsCuriousInStr;
          'critical': status:=bsProblematicInStr;
          'ordered': status:=bsOrdered;
          'provided': status:=bsProvided;
          'normal': status:=bsNormal;
          'unknown': status:=bsUnknown;
          'available': status:=bsAvailable;
          'lend': status:=bsLend;
          'virtual': status:=bsVirtual;
          'presentation': status:=bsPresentation;
          'interloan': status:=bsInterLoan;

          'history', '': status := bsUnknown; //these are invalid statuses (not occuring during serialization, however history is used by xquery offline search )
          else raise EBookListReader.create(Format(rsBookStatusInvalid, [value]));
        end;
    'cancelable': if (value <> '') and (value <> '0') and not striEqual(value, 'false') and (value <> '?') then cancelable:=tTrue
                 else if value = '?' then cancelable:=tUnknown
                 else cancelable:=tFalse;
    'status': statusStr := value;
    'issuedate': issueDate:=bbutils.dateParse(value, 'yyyy-mm-dd');
    'duedate': dueDate:=bbutils.dateParse(value, 'yyyy-mm-dd');
    '_firstexistsdate': firstExistsDate:=bbutils.dateParse(value, 'yyyy-mm-dd');
    '_lastexistsdate': lastExistsDate:=bbutils.dateParse(value, 'yyyy-mm-dd');
    'renewcount': renewCount := StrToIntDef(value, -1);
    else simplexmlparser.setProperty(name,value,additional);
  end;
end;

function TBook.getProperty(const name: string; const def: string): string;
begin
  case lowercase(name) of
    'category': result:=Category;
    'librarybranch': result:=libraryBranch;
    'id': result:=Id;
    'author': result:=Author;
    'title': result:=Title;
    'year': result:=Year;
    'isbn': result:=isbn;
    'statusid': result := BookStatusToSerializationStr(status);
    'cancelable': case cancelable of
      tUnknown: result := '?';
      tFalse: result := 'false';
      tTrue: result := 'true';
    end;
    'status': result := statusStr;
    'issuedate': result := bbutils.dateTimeFormat('yyyy-mm-dd', issueDate);
    'duedate': result := bbutils.dateTimeFormat('yyyy-mm-dd', dueDate);
    '_firstexistsdate': result := bbutils.dateTimeFormat('yyyy-mm-dd', firstExistsDate);
    '_lastexistsdate': result := bbutils.dateTimeFormat('yyyy-mm-dd', lastExistsDate);
    'renewcount': result := inttostr(max(renewCount, 0));
    else result := getPropertyAdditional(name, def);
  end;
end;

function TBook.getPropertyAdditional(const name: string; const def: string): string;
begin
  result := simplexmlparser.getProperty(name, additional, def);
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

procedure TBookList.mergePersistentFields(const old: TBookList);
var i:longint;
begin
  //TODO: Optimize to O(n log n)
  for i:=0 to count-1 do
    books[i].mergePersistentFields(old.findBook(books[i]));
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

type

{ TBookListXMLReader }

 TBookListXMLReader = class
  list: TBookList;
  constructor Create(alist: TBookList);
  procedure parse(data: string);
private
  currentBook: TBook;
  currentPropertyName, currentPropertyValue: string;
  function enterTag(tagName: string; properties: TProperties): TParsingResult;
  function leaveTag(tagName: string): TParsingResult;
  function textRead(text: string): TParsingResult;
end;

constructor TBookListXMLReader.Create(alist: TBookList);
begin
  list := alist;
end;

function TBookListXMLReader.leaveTag(tagName: string): TParsingResult;
begin
  result := prContinue;
  case tagName of
    'books': result := prStop;
    'book': begin
      list.add(currentBook);
      currentBook.decReference; //HUH? That is not the same as the old loading (now ref count = 1 in list, previously was ref count = 2)
      currentBook := nil;
    end;
    'v': if (currentPropertyName = '') or (currentBook = nil) then raise EBookListReader.create('Korrupte Bücherdatei')
         else begin
           currentBook.setProperty(currentPropertyName, currentPropertyValue);
           currentPropertyName := '';
           currentPropertyValue := '';
         end;
    else raise EBookListReader.create('Korrupte Bücherdatei (ungültiger geschlossener Tag)');
  end;
end;

procedure TBookListXMLReader.parse(data: string);
begin
  simplexmlparser.parseXML(data, @enterTag, @leaveTag, @textRead, CP_UTF8);
end;

function TBookListXMLReader.enterTag(tagName: string; properties: TProperties): TParsingResult;
begin
  case LowerCase(tagName) of
    'books': ; //
    'book': currentBook := TBook.create;
    'v': begin
       currentPropertyName := simplexmlparser.getProperty('n', properties);
       currentPropertyValue := '';
    end;
    '?xml': ;
    else raise EBookListReader.create('Korrupte Bücherdatei (ungültiger geöffneter Tag)');
  end;
  result := prContinue;
end;

function TBookListXMLReader.textRead(text: string): TParsingResult;
begin
  if currentPropertyName <> '' then currentPropertyValue += text;
  result := prContinue;
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
    xmlReader: TBookListXMLReader;
begin
  if logging then
    log('TBookList.load('+fileName+') started');
  clear;
  if FileExists(fileName+'.xml') then begin
    xmlReader := TBookListXMLReader.Create(self);
    xmlReader.parse(strLoadFromFileUTF8(fileName+'.xml'));
    xmlReader.free;
  end else if FileExists(fileName) then begin
    log('Import old file format');
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
        {otherInfo:=}truncNull(line);
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
  end;
  if logging then
    log('TBookList.load('+fileName+') ended')
end;

type

{ TBookListSerializer }

TBookListSerializer = object
  stream: tstream;
  procedure writeProp(const n,v:string);
  procedure writeDateProp(const n: string; d: integer);
  procedure writeString(const s: string);
  procedure writeLn(const s: string);
end;

procedure TBookListSerializer.writeProp(const n, v: string);
begin
  writeLn(#9'<v n="'+xmlStrEscape(n,true)+'">'+xmlStrEscape(v)+'</v>');
end;

procedure TBookListSerializer.writeDateProp(const n: string; d: integer);
begin
  writeProp(n, dateTimeFormat('yyyy-mm-dd', d));
end;

procedure TBookListSerializer.writeString(const s: string);
begin
  if length(s) = 0 then exit;
  stream.WriteBuffer(s[1], length(s));
end;

procedure TBookListSerializer.writeLn(const s: string);
begin
  writeString(s + LineEnding);
end;

procedure booklistSave(stream: TStream; data: pointer);
var temp: TBookListSerializer;
  i: Integer;
begin
  with TBookList(data) do begin
    temp.stream := stream;
    temp.writeLn('<?xml version="1.0" encoding="UTF-8"?>');
    temp.writeLn('<books>');
    for i := 0 to count-1 do begin
      temp.writeLn('<book>');
      books[i].serialize(@temp.writeProp, @temp.writeDateProp);
      temp.writeLn('</book>');
    end;
    temp.writeLn('</books>');
  end;
end;

procedure TBookList.save(fileName: string);
begin
  if logging then
    log('TBookList.save('+fileName+') started');
  fileSaveSafe(fileName+'.xml', @booklistSave, self);
  if logging then
    log('TBookList.save('+fileName+') ended')
end;

function TBookList.toXQuery: IXQValue;
var l: TXQVList;
  i: Integer;
begin
  l:=TXQVList.create();
  for i := 0 to Count - 1 do
    l.add(TBookListReader.bookToPXP(books[i]));
  result := TXQValueSequence.create(l);
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
  ignore(sender);
  ignore(debugLevel);
  if logging then log(logged);
end;

procedure TBookListReader.applyPattern(pattern, name: string);
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

function TBookListReader.evaluateQuery(const query: IXQuery): IXQValue;
var
  oldCount: Integer;
  i: Integer;
  varlog: TXQVariableChangeLog;
begin
  if bookAccessSection<>nil then EnterCriticalsection(bookAccessSection^);
  try
    varlog := parser.variableChangeLog;
    oldCount := varlog.count;
    Result:=inherited evaluateQuery(query);
    for i := oldCount to parser.variableChangeLog.count - 1 do
      parserVariableRead(varlog.getName(i), varlog.get(i));
  finally
    if bookAccessSection<>nil then LeaveCriticalsection(bookAccessSection^);
  end;
end;

procedure TBookListReader.setVariable(name: string; value: IXQValue; namespace: string);
begin
  if bookAccessSection<>nil then EnterCriticalsection(bookAccessSection^);
  try
    inherited setVariable(name, value, namespace);
    //parserVariableRead(name, value);
  finally
    if bookAccessSection<>nil then LeaveCriticalsection(bookAccessSection^);
  end;
end;

class procedure TBookListReader.setAllBookProperties(book: TBook; const value: IXQValue);
var
  v: IXQValue;
  obj: TXQValueObject;
  i: Integer;
begin
  v := value.clone;
  if not (v is TXQValueObject) then raise EBookListReader.Create('Nested Buch ohne Eigenschaften');
  obj := v as TXQValueObject;;
  for i:=0 to obj.values.count-1 do
    setBookProperty(book, obj.values.getName(i), obj.values.get(i));
end;

class procedure TBookListReader.setBookProperty(book: TBook; variable: string; const value:IXQValue);
function strconv():string;
begin
  result:=strTrimAndNormalize(value.toString);
end;
function strconvlist(sep: string):string;
var  x: IXQValue;
begin
  result := '';
  for x in value do
    if result = '' then result += strTrimAndNormalize(x.toString)
    else result += sep + strTrimAndNormalize(x.toString);
end;

var
  basevariable, temp: String;
  pv: PIXQValue;
  newbook: TBook;

begin
  basevariable := variable;
  variable := LowerCase(variable);
  if (variable <> 'statusid') and strlibeginswith(@variable[1],length(variable),'status') then begin
    book.StatusStr:=strconv();
    if variable='status:problematic' then book.Status:=bsProblematicInStr
    else if variable='status:curious' then book.Status:=bsCuriousInStr
    else if pos(':', variable) > 0 then book.statusStr:=book.statusStr + ' Achtung: Ungültige Statusvariable "' + variable + '" in Template'
    else if book.statusStr <> '' then book.Status:=bsCuriousInStr
    else book.status := bsNormal;
  end else if striEqual(variable, 'issuedate') then book.issueDate:=trunc(value.toDateTime)
  else if striEqual(variable, 'duedate') then book.dueDate:=trunc(value.toDateTime)
  else if strlibeginswith(@variable[1],length(variable),'issuedate') then
    book.IssueDate:=dateParse(strconv(),strcopyfrom(variable,pos(':',variable)+1))
  else if strlibeginswith(@variable[1],length(variable),'duedate') then
    book.dueDate:=dateParse(strconv(),strcopyfrom(variable,pos(':',variable)+1))
  else if striEqual(variable, 'limitdate') or strlibeginswith(@variable[1],length(variable),'limitdate') then
    raise EBookListReader.create('The template is using the limitdate property which is deprecated. It should now be called duedate')
  else if strEqual(variable, 'holdings') then begin
    if book.holdings = nil then book.holdings := TBookList.create(book)
    else book.holdings.clear;
    for pv in value.GetEnumeratorPtrUnsafe do begin
      newbook := TBook.create;
      setAllBookProperties(newbook, pv^);
      book.holdings.add(newbook);
      newbook.decReference;
    end;
  end else begin
    if value.getSequenceCount > 1 then begin
      case variable of
        'orderconfirmationoptiontitles': temp := strconvlist('\|');
        'image-url': temp := strconvlist(LineEnding);
        else temp := strconv();
      end;
    end else temp := strconv();
    book.setProperty(basevariable, temp); //preserve case
  end;
end;

procedure TBookListReader.parserVariableRead(variable: string; value: IXQValue);
  function strconv(s:string):string;
  begin
    result:=StringReplace(s,#13,'',[rfReplaceAll]);
    result:=StringReplace(result,#10,'',[rfReplaceAll]);
    result:=trim(result);
  end;


var
 i,j : Integer;
 book: TXQValueObject;
 temp2: IXQValue;
 temp: TXQValue;
 s: string;
 sl: TStringList;
begin
  if logging then
    log('** Read variable: "'+variable+'" = "'+value.toXQuery+'"');
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
  end
  else if (variable='raise()') then raise EBookListReaderFromWebpage.create(LineEnding + LineEnding + value.toString)
  else if (variable = 'raise-login()') then raise ELoginException.create(LineEnding + LineEnding + value.toString)
  else if (variable='raise-internal()') then raise EBookListReader.create(LineEnding + LineEnding + value.toString)
  else if (variable='message()') then begin
     if pendingMessage <> nil then pendingMessage.free;
     pendingMessage := TPendingMessage.Create;
     case value.getProperty('kind').toString of
       'choose': pendingMessage.kind := pmkChoose;
       'confirm': pendingMessage.kind := pmkConfirm;
     end;
     pendingMessage.callback:=value.getProperty('callback').toString;
     pendingMessage.caption:=value.getProperty('caption').toString;
     for temp2 in value.getProperty('options') do
       arrayAdd(pendingMessage.options, temp2.toString);
     for temp2 in value.getProperty('option-values') do
       arrayAdd(pendingMessage.optionValues, temp2.toString);
  end else if variable = 'book' then begin
    if not (value is TXQValueObject) then raise EBookListReader.Create('Buch ohne Eigenschaften');
    temp2 := value.clone;
    book := temp2 as TXQValueObject;
    if book.hasProperty('_select', @temp) then begin
      currentBook := nil;
      case temp.kind of
        pvkObject: begin
          sl := TStringList.Create;
          sl.CaseSensitive := true;
          (temp as TXQValueObject).enumerateKeys(sl);
          for i:=0 to books.Count-1 do begin
            currentBook := books[i];
            for j := 0 to sl.Count - 1 do begin
              if (sl[j] = '_select') or (sl[j] = '_existing') then continue;
              if books[i].getProperty(sl[j]) <> temp.getProperty(sl[j]).toString then begin //todo: optimize
                currentBook := nil;
                break;
              end;
            end;
            if currentBook <> nil then break;
          end;
          sl.free;
        end;
      end;
      //s := temp.toString;
      if currentBook=nil then begin
        if logging then for i:=0 to books.Count-1 do log('Book: "'+books[i].toLimitString() + '" <> "'+temp.toXQuery()+'"');
        raise EBookListReader.create('Template wants to select book '+temp.toXQuery()+', but it doesn''t exist');
      end;
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
    else if book.hasProperty('_existing', @temp) then begin
      if not temp.toBoolean then raise EBookListReader.create('Das Buch hat einen _existing Marker, aber er sagt, das Buch existiere nicht : '+temp.toXQuery());
      if currentBook = nil then raise EBookListReader.Create('Das Template will ein existierendes Buch verändert, aber mir ist kein Buch bekannt.');
    end else begin
      currentBook := defaultBook;
      currentBook.clear;
    end;
    for i:=0 to book.values.count-1 do begin
      s := book.values.getName(i);
      if (s = '_existing') or (s = 'select(id)') or (s = 'select(current)') or (s = 'select(new)') or (s = '_select') then continue;
      setBookProperty(currentBook,s,book.values.get(i));
    end;
    currentBook.firstExistsDate:=trunc(now);
    currentBook.lastExistsDate:=trunc(now);
    if currentBook = defaultBook then  begin
      books
        .add(currentBook.Id,currentBook.Title,currentBook.Author,currentBook.Year)
           .assign(currentBook);
    end;
    temp2:=nil;
  end;
end;

constructor TBookListReader.create(atemplate:TMultiPageTemplate);
var
  temp: TXQVideLibriStaticContext;
  tempc: TXQEvaluationContext;
begin
  inherited create(atemplate, nil);
  defaultBook:=TBook.create;
  onLog:=@logall;
  parser.QueryEngine.GlobalNamespaces.add(XMlNamespaceVideLibri);
  parser.QueryEngine.GlobalNamespaces.add(XMlNamespaceVideLibri_VL);
  temp := TXQVideLibriStaticContext.create(self);
  temp.assign(parser.QueryEngine.StaticContext);
  parser.QueryEngine.StaticContext.free;
  parser.QueryEngine.StaticContext := temp;
  tempc := parser.QueryContext;
  tempc.staticContext := temp;
  parser.QueryContext := tempc;
end;

destructor TBookListReader.destroy();
begin
  defaultBook.free;
  inherited destroy();
end;

type TXQueryBookSerializer = object
  obj: TXQValueObject;
  procedure writeStr(const name, value: string);
  procedure writeDate(const name: string; date: integer);
end;

procedure TXQueryBookSerializer.writeStr(const name, value: string);
begin
  obj.setMutable(name,value);
end;

procedure TXQueryBookSerializer.writeDate(const name: string; date: integer);
  function xqvalueDate(const i: integer): IXQValue;
  begin
    result := TXQValueDateTime.create(baseSchema.date, i);
  end;
begin
  if date <> 0 then
    obj.setMutable(name, xqvalueDate(date));
end;

class function TBookListReader.bookToPXP(book: TBook): TXQValueObject;
var
  ser: TXQueryBookSerializer;
  i: Integer;
begin
  ser.obj := TXQValueObject.create();
  result := ser.obj;
  book.serialize(@ser.writeStr, @ser.writeDate);
  if not book.lend then //result.setMutable('statusId', BookStatusToSerializationStr(book.status))
    result.setMutable('statusId', 'history');
  for i:=0 to high(book.additional) do
    result.setMutable(book.additional[i].name, book.additional[i].value );
  result.setMutable('_existing', xqvalueTrue);
  if book.holdings <> nil then result.setMutable('holdings', book.holdings.toXQuery);
  //see queryHistory;
end;

procedure TBookListReader.selectBook(book: TBook);
begin
  parser.variableChangeLog.add('book', bookToPXP(book));
  currentBook:=book;
end;


function xqFunctionDelete_Current_Books(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  ignore(argc); ignore(argv);
  context.staticContext.sender.VariableChangelog.add('delete-current-books()', xqvalueTrue);
  result := xqvalue();
end;

function xqFunctionRaise_Login(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  requiredArgCount(argc, 0, 1);
  if argc = 0 then context.staticContext.sender.VariableChangelog.add('raise-login()', xqvalue())
  else context.staticContext.sender.VariableChangelog.add('raise-login()', argv[0]);
  result := xqvalue();
end;

function xqFunctionRaise_Internal(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  requiredArgCount(argc, 0, 1);
  if argc = 0 then context.staticContext.sender.VariableChangelog.add('raise-internal()', xqvalue())
  else context.staticContext.sender.VariableChangelog.add('raise-internal()', argv[0]);
  result := xqvalue();
end;

function xqFunctionRaise(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  requiredArgCount(argc, 0, 1);
  if argc = 0 then context.staticContext.sender.VariableChangelog.add('raise()', xqvalue())
  else context.staticContext.sender.VariableChangelog.add('raise()', argv[0]);
  result := xqvalue();
end;

//add function vl:choose(  callback action id,  caption,  option captions, option values )
//
//callback action is called with
//   choose-result := 0                                         if canceled cancelation
//   choose-result := index choosen by user                     if index outside value range
//   choose-result := option-values[ index choosen by user ]    else
// ATTENTION: Only eq can be used to test for 0, not =. Or use value instance of xs:decimal
function xqFunctionChoose(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
var
  temp: TXQValueObject;
begin
  requiredArgCount(argc, 4, 4);
  temp := TXQValueObject.create();
  temp.setMutable('kind', 'choose');
  temp.setMutable('callback', argv[0].toString);
  temp.setMutable('caption', argv[1].toString);
  temp.setMutable('options', argv[2]);
  temp.setMutable('option-values', argv[3]);
  context.staticContext.sender.VariableChangelog.add('message()', temp);
  result := xqvalue();
end;

//add function vl:confirm(  callback action id,  caption  )
//
//callback action is called with confirm-result := true/false
function xqFunctionConfirm(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
var
  temp: TXQValueObject;
begin
  requiredArgCount(argc, 2, 2);
  temp := TXQValueObject.create();
  temp.setMutable('kind', 'confirm');
  temp.setMutable('callback', argv[0].toString);
  temp.setMutable('caption', argv[1].toString);
  context.staticContext.sender.VariableChangelog.add('message()', temp);
  result := xqvalue();
end;

//add function vl:select-book(  query  )
//
//callback action is called with confirm-result := true/false
function xqFunctionSelectBook(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
var
  reader: TBookListReader;
  query: IXQValue;
  book: TBook;
begin
  requiredArgCount(argc, 1, 1);

  query := argv[0];

  reader := (context.staticContext as TXQVideLibriStaticContext).bookListReader;
  book := reader.books.findBook(query.getProperty('id').toString, query.getProperty('author').toString, query.getProperty('title').toString, query.getProperty('year').toString);
  if book = nil then
    raise EBookListReader.create('Failed to find book: ' + query.jsonSerialize(tnsText) +
                                  LineEnding + 'Perhaps it was returned?');
  result := reader.bookToPXP(book);
end;

//function vl:log-immediately(  data )
//
//Prints data to the log immediately. Log output remains during template rollback!
function xqFunctionLogImmediately(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
begin
  ignore(context);
  requiredArgCount(argc, 1, 1);
  if logging then log(argv[0].toXQuery());
  result := argv[0];
end;

var BookPropertyNormalizationRegex: record
  title, author, publisher, isbn, year: TWrappedRegExpr;
end;

function xqFunctionSetBookProperty(const context: TXQEvaluationContext; argc: SizeInt; argv: PIXQValue): IXQValue;
  procedure setProperty(const k, v: string);
  var
    book: IXQValue;
  begin
    // $book(k) := v
    book := context.staticContext.sender.VariableChangelog.get('book');
    if (book = nil) or (book.kind <> pvkObject) then exit;
    context.staticContext.sender.VariableChangelog.add('book', (book as TXQValueObject).setImmutable(k, v));
  end;

var
  key, href, value: String;
  node: TTreeNode;
  yearAt: Integer;

begin
  requiredArgCount(argc, 2, 2);
  key := strTrimAndNormalize(argv[0].toString, [#0..' ', ':']);
  value := strTrimAndNormalize(argv[1].toString);
  if argv[1].kind = pvkNode then begin
    node := argv[1].toNode;
    if node.typ = tetOpen then begin
      if not striEqual(node.value, 'a') then node := node.findNext(tetOpen, 'a', [], node.reverse);
      if node <> nil then begin
        href := node.getAttribute('href');
        if href <> '' then  begin
          if not strIsAbsoluteURI(href) then begin
            node := node.getDocument();
            if node <> nil then href := strResolveURI(href, (node as TTreeDocument).baseURI);
          end;
          href := StringReplace(href, ' ', '+', [rfReplaceAll]); //the search dialog searchs for ' ' to find the end of the url
          value += ' ( ' + href + ' )';
        end;
      end;
    end;
  end;

  with BookPropertyNormalizationRegex do begin
    if wregexprMatches(title, key) then setProperty('title', value)
    else if wregexprMatches(author, key) then setProperty('author', value)
    else if wregexprMatches(publisher, key) then begin
      yearAt := length(value);
      while (yearAt > 0) and (value[yearAt] in ['0'..'9']) do dec(yearAt);
      if yearAt <= length(value) - 2 then begin
        setProperty('year', strCopyFrom(value, yearAt + 1));
        while (yearAt > 0) and (value[yearAt] in [#0..' ','.',';',',']) do dec(yearAt);
        value := copy(value, 1, yearAt);
      end;
      setProperty('publisher', value);
    end else if wregexprMatches(isbn, key) then setProperty('isbn', value)
    else if wregexprMatches(year, key) then setProperty('year', value)
    else setProperty(key + '!', value);
  end;

  result := xqvalue();
end;

var vl: TXQNativeModule;
initialization
  XMlNamespaceVideLibri := TNamespace.create(XMLNamespaceURL_VideLibri, 'videlibri');
  XMlNamespaceVideLibri_VL := TNamespace.create(XMLNamespaceURL_VideLibri, 'vl');
  vl := TXQNativeModule.Create(XMLNamespaceVideLibri);
  vl.registerFunction('delete-current-books', 0, 0, @xqFunctionDelete_Current_Books, []);
  vl.registerFunction('raise', 0, 1, @xqFunctionRaise, []);
  vl.registerFunction('raise-internal', 0, 1, @xqFunctionRaise_Internal, []);
  vl.registerFunction('raise-login', 0, 1, @xqFunctionRaise_Login, []);
  vl.registerFunction('choose', 4, 4, @xqFunctionChoose, []);
  vl.registerFunction('confirm', 2, 2, @xqFunctionConfirm, []);
  vl.registerFunction('select-book', 1, 1, @xqFunctionSelectBook, []);
  vl.registerFunction('log-immediately', 1, 1, @xqFunctionLogImmediately, []);

  vl.registerFunction('set-book-property', 2, 2, @xqFunctionSetBookProperty, []);

  TXQueryEngine.registerNativeModule(vl);

  with BookPropertyNormalizationRegex do begin
    title := wregexprParse('Titel|Title', [wrfIgnoreCase]); //todo: but not "Einheitssachtitel"
    author := wregexprParse('Verantwortlichkeit|Author|Autor', [wrfIgnoreCase]);
    publisher := wregexprParse('Verlag', [wrfIgnoreCase]);
    isbn := wregexprParse('ISBN', [wrfIgnoreCase]);
    year := wregexprParse('Creation Date|Jahr', [wrfIgnoreCase]);
  end;
finalization
  vl.free
end.


