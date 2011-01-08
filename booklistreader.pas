unit booklistreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,bbutils,extendedhtmlparser,simplehtmlparser,simplexmlparser,dRegExpr,internetaccess;
  
type
  TBookList = class;
  TBookStatus=(bsNormal,bsUnknown,bsIsSearchedDONTUSETHIS,bsEarMarked, bsMaxLimitReached,bsProblematicInStr,bsCuriousInStr,bsAccountExpired);

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
    category,statusStr,otherInfo: string;
    issueDate,limitDate:longint;
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
    
    property books[i:longint]: TBook read getBook; default;
    property lendList: boolean read flendList write setLendList;
  end;

  TTemplateRealActionType = (tratLoadPage,tratCallAction);
  TTemplateRealAction=record
    typ: TTemplateRealActionType;
    //load page
    url:string;
    templateFile:string;
    template:string;
    postparams:string;
    //call action
    action: string;
      //callOnce: boolean;
  end;

  TTemplateAction=record
    name: string;
    actions:array of TTemplateRealAction;
    errors: array of record
      templateFile:string;
      template:string;
    end;
    singleBookStr: string; //Nur benötigt für Listen von Büchern
  end;
  PTemplateAction=^TTemplateAction;

  { TBookListTemplate }

  TBookListTemplate=class
  protected
    currentAction:PTemplateAction;
    currentTag: string;
    function readProperty(tagName: string; properties: TProperties): TParsingResult;
    function textRead(text: string):TParsingResult;
    function leaveTag(tagName: string):TParsingResult;
  public
    earMarkedRegEx,maxLimitRegEx,accountExpiredRegEx:TRegExpr;
    propertyAuthorRegEx, propertyTitleRegEx, propertyYearRegEx:TRegExpr;
    
    path,name:string;
    actions:array of TTemplateAction;

    variables: TStringList;
    
    constructor create(_dataPath,_name:string);
    procedure loadTemplates;
    destructor destroy;override;
    
    function findAction(_name:string):PTemplateAction;
    //function getAccountObject():TCustomAccountAccess;override;
  end;

  { TBookListReader }

  { EBookListReader }

  EBookListReader=class(Exception)
    details:string;
    constructor create;
    constructor create(s:string;more_details:string='');
  end;
  TBookListReader = class
  private
    currentBook,defaultBook: TBook;
    template:TBookListTemplate;
    procedure setBookProperty(book:TBook;variable,value:string);
    procedure parserVariableRead(variable: string; value: String);
  public
    bookAccessSection: ^TRTLCriticalSection;
    books: TBookList;
    internet:TInternetAccess;
    parser: THtmlTemplateParser;
    constructor create(atemplate:TBookListTemplate);
    destructor destroy();override;

    function findAction(name:string):PTemplateAction;
    procedure performAction(action:string);
    procedure performAction(const action:TTemplateAction);
    
    procedure selectBook(book:TBook);
  end;
  
const BOOK_NOT_EXTENDABLE=[bsEarMarked,bsMaxLimitReached,bsProblematicInStr,bsAccountExpired];
      BOOK_EXTENDABLE=[bsNormal,bsCuriousInStr];

function BookStatusToStr(book: TBook;verbose:boolean=false): string; //returns utf8


implementation
uses bbdebugtools;

function BookStatusToStr(book: TBook;verbose:boolean=false): string;
begin
  if book.lend =false then
    if verbose then exit('nicht ausgeliehen') else exit('');
  case book.Status of
    bsNormal: if verbose then exit('normal verlängerbar') else exit('');
    bsUnknown: exit('Ausleihstatus unbekannt');
    bsIsSearchedDONTUSETHIS: exit('Ausleihstatus wird ermittelt... (sollte nicht vorkommen, bitte melden!)');
    bsEarMarked:exit('vorgemerkt');
    bsMaxLimitReached: exit('maximale Ausleihfrist erreicht');
    bsAccountExpired: exit('Büchereikarte ist abgelaufen');
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
  limitDate:=0;
  issueDate:=0;
  SetLength(Additional,0);
end;

procedure TBook.assignNoReplace(book: TBook);
var i:longint;
begin
  if book=nil then exit;
  if category='' then category:=book.category;
  if statusStr='' then statusStr:=book.statusStr;
  if otherInfo='' then otherInfo:=book.otherInfo;
  if issueDate=0 then issueDate:=book.issueDate;
  if limitDate=0 then limitDate:=book.limitDate;
  if status=bsUnknown then status:=book.status;
  if charges=0 then charges:=book.charges;
  if lastExistsDate=0 then lastExistsDate:=book.lastExistsDate;
  if (firstExistsDate=0) or ((book.firstExistsDate<>0) and (book.firstExistsDate<firstExistsDate)) then
    firstExistsDate:=book.firstExistsDate;
  for i:=0 to high(book.additional) do
    if getProperty(book.additional[i].name,additional)='' then
      addProperty(book.additional[i].name,book.additional[i].value,additional);
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
  if book.limitDate<>0 then limitDate:=book.limitDate;
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
      limitDate:=StrToInt(truncNull(line));
      lastExistsDate:=StrToInt(truncNull(line));
      status:=TBookStatus(StrToInt(truncNull(line)));
      year:=truncNullDef(line,'');
      firstExistsDate:=StrToInt(truncNullDef(line,'0'));
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
                   IntToStr(issueDate)+#0+IntToStr(limitDate)+#0+
                   IntToStr(lastExistsDate)+#0+inttostr(integer(status))+#0+year+#0+IntToStr(firstExistsDate)+#0)
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



{ TBookListTemplate }


function TBookListTemplate.readProperty(tagName: string; properties: TProperties
  ): TParsingResult;
var i:longint;
    temp:string;
    regex: ^TRegExpr;
begin
  if SameText(tagName,'variable') then begin
    variables.Values[getProperty('name',properties)]:=getProperty('value',properties)
  end else if SameText(tagName,'earmarked') then begin
    earMarkedRegEx.Expression:=getProperty('matches',properties);
    if earMarkedRegEx.Expression='' then
      raise exception.create('tag <earmarked> ungültig in Template von '+path);
  end else if SameText(tagName,'maxlimit') then begin
    maxLimitRegEx.Expression:=getProperty('matches',properties);
    if maxLimitRegEx.Expression='' then
      raise exception.create('tag <maxLimitRegEx> ungültig in Template von '+path);
  end else if SameText(tagName,'accountExpired') then begin
    accountExpiredRegEx.Expression:=getProperty('matches',properties);
    if accountExpiredRegEx.Expression='' then
      raise exception.create('tag <accountExpiredRegEx> ungültig in Template von '+path);
  end else if SameText(tagName,'book-property') then begin
    temp:=LowerCase(getProperty('name',properties));
    if temp='author' then regex:=@propertyAuthorRegEx
    else if temp='title' then regex:=@propertyTitleRegEx
    else if temp='year' then regex:=@propertyYearRegEx
    else regex:=nil;
    if regex <> nil then begin
      if regex^=nil then regex^:=TRegExpr.Create();
      regex^.Expression:=getProperty('matches',properties);
      if regex^.Expression='' then
        raise exception.create('tag <book-property> ungültig in Template von '+path);
    end;
  end else if SameText(tagName,'error') then begin
    SetLength(currentAction^.errors,length(currentAction^.errors)+1);
    currentAction^.errors[high(currentAction^.errors)].
      templateFile:=Utf8ToAnsi(getProperty('templateFile',properties));
  end else if SameText(tagName,'page') then begin
    SetLength(currentAction^.actions,length(currentAction^.actions)+1);
    with currentAction^.actions[high(currentAction^.actions)] do begin
      typ:=tratLoadPage;
      postparams:='';
      for i:=0 to high(properties) do
        if SameText(properties[i].name,'url') then
          url:=Utf8ToAnsi(properties[i].value)
        else if SameText(properties[i].name,'templateFile') then
          templateFile:=Utf8ToAnsi(properties[i].value);
    end;
  end else if SameText(tagName,'post') then begin
    if (currentAction=nil) then raise EBookListReader.create('Template ungültig: post-Tag gefunden, ohne dass eine Aktion definiert wurde');
    if (length(currentAction^.actions)=0)or(currentAction^.actions[high(currentAction^.actions)].typ<>tratLoadPage) then raise EBookListReader.create('Template ungültig: post-Tag nicht innerhalb eines page-Tags');
    if currentAction^.actions[high(currentAction^.actions)].postparams<>'' then
      currentAction^.actions[high(currentAction^.actions)].postparams+='&';
    if getProperty('name',properties)<>'' then
      currentAction^.actions[high(currentAction^.actions)].postparams+=getProperty('name',properties)+'=';
    //for value see textread
  end else if SameText(tagName,'call') then begin
    SetLength(currentAction^.actions,length(currentAction^.actions)+1);
    with currentAction^.actions[high(currentAction^.actions)] do begin
      typ:=tratCallAction;
      action:=getProperty('action',properties);
    end;
  end else if SameText(tagName,'action') then begin
    SetLength(actions,length(actions)+1);
    FillChar(actions[high(actions)],sizeof(actions[high(actions)]),0);
    actions[high(actions)].name:=LowerCase(getProperty('id',properties));
    currentAction:=@actions[high(actions)];
    currentAction^.singleBookStr:=getProperty('singleBookStr',properties);
  end;
  currentTag:=tagName;
  result:=prContinue;
end;

function TBookListTemplate.textRead(text: string): TParsingResult;
var page: ^TTemplateRealAction;
begin
  if currentTag = 'post' then begin
    if (currentAction=nil) then raise EBookListReader.create('Template ungültig: post-Tag gefunden, ohne dass eine Aktion definiert wurde');
    if (length(currentAction^.actions)=0)or(currentAction^.actions[high(currentAction^.actions)].typ<>tratLoadPage) then raise EBookListReader.create('Template ungültig: post-Tag nicht innerhalb eines page-Tags');
    page:=@currentAction^.actions[high(currentAction^.actions)];
    page^.postparams+=text
  end;
  Result:=prContinue;
end;

function TBookListTemplate.leaveTag(tagName: string): TParsingResult;
begin
  currentTag:='';
  Result:=prContinue;
end;

constructor TBookListTemplate.create(_dataPath,_name:string);
begin
  IncludeTrailingPathDelimiter(_dataPath);
  self.path:=_dataPath;
  self.name:=_name;
  if not FileExists(_dataPath+'template') then
    raise Exception.Create('Template '+_dataPath+' nicht gefunden');
  variables:=TStringList.Create;
  earMarkedRegEx:=TRegExpr.Create('[vV]orgemerkt');
  maxLimitRegEx:=TRegExpr.Create('[fF]rist erreicht');
  accountExpiredRegEx:=TRegExpr.Create('Karte abgelaufen');
  parseXML(strLoadFromFile(_dataPath+'template'),@readProperty,@leaveTag,@textRead,eUTF8);
end;

procedure TBookListTemplate.loadTemplates;
  procedure load(var action:TTemplateAction);
  var i:longint;
  begin
    for i:=0 to high(action.actions) do begin
      if action.actions[i].templateFile='' then continue;
      action.actions[i].template:=strLoadFromFile(self.path+action.actions[i].templateFile);
      if action.actions[i].template='' then
        raise EBookListReader.create('Template-Datei "'+self.path+action.actions[i].templateFile+'" konnte nicht geladen werden');
    end;
    for i:=0 to high(action.errors) do begin
      action.errors[i].template:=strLoadFromFile(self.path+action.errors[i].templateFile);
      if action.errors[i].template='' then
        raise EBookListReader.create('Template-Datei "'+self.path+action.errors[i].templateFile+'" konnte nicht geladen werden');
    end;
  end;
var i:longint;
begin
  for i:=0 to high(actions) do
    load(actions[i]);
end;


destructor TBookListTemplate.destroy;
begin
  earMarkedRegEx.free;
  maxLimitRegEx.free;
  accountExpiredRegEx.free;
  variables.Free;

  propertyAuthorRegEx.Free; //nil.free is okay
  propertyTitleRegEx.Free;
  propertyYearRegEx.Free;
  inherited destroy;
end;

function TBookListTemplate.findAction(_name: string): PTemplateAction;
var i:longint;
begin
  result:=nil;
  for i:=0 to high(actions) do
    if actions[i].name=_name then exit(@actions[i]);;
end;

    { TBookListReader }

procedure TBookListReader.setBookProperty(book: TBook; variable, value: string
  );
  function strconv(s:string):string;
  begin
    result:=StringReplace(s,#13,'',[rfReplaceAll]);
    result:=StringReplace(result,#10,'',[rfReplaceAll]);
    result:=trim(result);
  end;
begin
  if variable='book.category' then book.Category:=strconv(value)
  else if variable='book.id' then book.Id:=strconv(value)
  else if variable='book.author' then book.Author:=strconv(value)
  else if variable='book.title' then book.Title:=strconv(value)
  else if variable='book.year' then book.Year:=strconv(value)
  else if strlibeginswith(@variable[1],length(variable),'book.status') then begin
    if variable='book.status:problematic' then book.Status:=bsProblematicInStr
    else if variable='book.status:curious' then book.Status:=bsCuriousInStr;
     if value<>'' then begin
      book.StatusStr:=strconv(value);
      if template.earMarkedRegEx.Exec(value) then book.Status:=bsEarMarked
      else if template.maxLimitRegEx.Exec(value) then book.Status:=bsMaxLimitReached
      else if template.accountExpiredRegEx.Exec(value) then book.Status:=bsAccountExpired;
    end else book.Status:=bsNormal;
       //(bsNormal,bsUnknown,bsIsSearched,bsEarMarked,bsMaxLimitReached,bsProblematicInStr,bsCuriousInStr);
  end else if strlibeginswith(@variable[1],length(variable),'book.issuedate') then
    book.IssueDate:=parseDate(strconv(value),strcopyfrom(variable,pos(':',variable)+1))
  else if strlibeginswith(@variable[1],length(variable),'book.limitdate') then
    book.LimitDate:=parseDate(strconv(value),strcopyfrom(variable,pos(':',variable)+1))
  else if stribeginswith(variable,'book.') then
    setProperty(copy(variable,pos('.',variable)+1,length(variable)),value,book.Additional);
end;

procedure TBookListReader.parserVariableRead(variable: string; value: String);
  function strconv(s:string):string;
  begin
    result:=StringReplace(s,#13,'',[rfReplaceAll]);
    result:=StringReplace(result,#10,'',[rfReplaceAll]);
    result:=trim(result);
  end;


var
 i: Integer;
begin
  if logging then
    log('** Read variable: "'+variable+'" = "'+value+'"');
  if variable='delete-current-books()' then begin
    books.clear();
  end else if variable='book-start()' then begin
  //reset
    currentBook:=defaultBook;
    currentBook.clear;
  end else if variable='book-end()' then begin
    currentBook.firstExistsDate:=trunc(now);
    currentBook.lastExistsDate:=trunc(now);
    books
      .add(currentBook.Id,currentBook.Title,currentBook.Author,currentBook.Year)
         .assignNoReplace(currentBook);
  end else if variable='book-select(id)' then begin
    currentBook := nil;
    for i:=0 to books.Count-1 do
      if books[i].id = value then
        currentBook:=books[i];
    if currentBook=nil then
      raise EBookListReader.create('Template wants to select book '+value+', but it doesn''t exist');
  end else if variable='book-select()' then begin
    //reset
    raise EBookListReader.create('not implemented yet');
  end else if variable='raise()' then begin
    raise EBookListReader.create(value);
  end else if currentBook<>nil then  //buch eigenschaften
    if (template.propertyYearRegEx <> nil) and (template.propertyYearRegEx.exec(variable)) then
      setBookProperty(currentBook,'book.year',value)
     else if (template.propertyAuthorRegEx <> nil) and (template.propertyAuthorRegEx.exec(variable)) then
      setBookProperty(currentBook,'book.author',value)
     else if (template.propertyTitleRegEx <> nil) and (template.propertyTitleRegEx.exec(variable)) then
      setBookProperty(currentBook,'book.title',value)
     else if stribeginswith(variable,'book.') then
      setBookProperty(currentBook,variable,value)

end;

constructor TBookListReader.create(atemplate:TBookListTemplate);
begin
  template:=atemplate;
  parser:=THtmlTemplateParser.create;
  defaultBook:=TBook.create;
end;

destructor TBookListReader.destroy();
begin
  parser.free;
  defaultBook.free;
  inherited destroy();
end;

function TBookListReader.findAction(name:string): PTemplateAction;
begin
  result:=template.findAction(name);
end;

procedure TBookListReader.performAction(action: string);
var act: PTemplateAction;
begin
  act:=findAction(action);
  if act=nil then raise EBookListReader.Create('Aktion '+action+' konnte nicht ausgeführt werden, da sie nicht gefunden wurde.');
  performAction(act^);
end;

procedure TBookListReader.performAction(const action:TTemplateAction);
var i:longint;
    page:string;
    aname: String;
    avalue: String;
    j: Integer;
begin
  if logging then begin
    log('Enter performAction, finternet:');
    //TODO: parser log
  end;

  try
    //OutputDebugString(pchar(lib.defaultVariables.Text));
    Assert(internet<>nil,'Internet nicht initialisiert');

    with action do begin
      try
        for i:=0 to high(actions) do
          case actions[i].typ of
            tratCallAction: begin
              if logging then log('Action call: '+actions[i].action);
              performAction(actions[i].action);
            end;
            tratLoadPage: begin
              if actions[i].template<>'' then begin
                if logging then log('Parse Template From File: '+template.path+actions[i].templateFile);
                parser.parseTemplate(actions[i].template,actions[i].templateFile);
              end;
              if logging then log('Get/Post internet page ->'+parser.replaceVars(actions[i].url)+'<-'#13#10'Post: '+parser.replaceVars(actions[i].postparams));
              if actions[i].postparams='' then
                page:=internet.get(parser.replaceVars(actions[i].url))
               else
                page:=internet.post(parser.replaceVars(actions[i].url),
                                               parser.replaceVars(actions[i].postparams));

              if logging then log('downloaded: '+inttostr(length(page))+' bytes');
              if page='' then
                raise EInternetException.Create(actions[i].url +' konnte nicht geladen werden');
              if actions[i].template<>'' then begin
                if logging then log('parse page: '+parser.replaceVars(actions[i].url));
                if bookAccessSection<>nil then EnterCriticalsection(bookAccessSection^);
                try
                  parser.parseHTML(page,true);
                  //simulate old parser interface
                  for j:=0 to parser.variableChangeLog.count-1 do begin
                    parser.variableChangeLog.GetNameValue(j,aname,avalue);
                    parserVariableRead(aname,avalue);
                  end;
                finally
                  if bookAccessSection<>nil then LeaveCriticalsection(bookAccessSection^);
                end;
              end;
              if logging then log('page finished');
            end;
          end;
        if logging then log('pages finished');
      except
        on  e:Exception do begin
          if logging then begin
            //log(parserLog.text);
            log('Exception message: '+e.message);
          end;
          for i:=0 to high(errors) do begin
            if logging then log('Check error: '+IntToStr(i)+': '+errors[i].templateFile);
            parser.parseTemplate(errors[i].template);
            //simulate old parser interface
            //TODO: this is absolutely not needed here
            for j:=0 to parser.variableChangeLog.count-1 do begin
              parser.variableChangeLog.GetNameValue(j,aname,avalue);
              parserVariableRead(aname,avalue);
            end;
            if logging then begin
              log('Error template loaded');
            end;
            try
              if logging then log('parser html: ');
              parser.parseHTML(page,true);
            except
              on e2: EBookListReader do begin
                if logging then log('New exception: '+e2.message);
                raise;
              end;
              on e: Exception do begin//frühere Exception wird weitergegeben/unverständlich
                if logging then log('earlier exception: '+e.Message);
              end;
            end;
          end;
          if e is EHTMLParseException then
            if pos('Die HTML Datei ist kürzer als das Template',e.message)=1 then begin
              if logging then log('ele.create');
              raise EBookListReader.create(e.message,'');
            end;
          raise;
        end;
      end;
    end;
  finally
    if logging then log('Leave performAction');
  end;
end;

procedure TBookListReader.selectBook(book: TBook);
var i:longint;
begin
  parser.variables.Values['book.id']:=book.id;
  parser.variables.Values['book.author']:=book.author;
  parser.variables.Values['book.title']:=book.title;
  parser.variables.Values['book.year']:=book.year;
  for i:=0 to high(book.additional) do
    parser.variables.Values['book.'+book.additional[i].name]:=book.additional[i].value;
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

end.

