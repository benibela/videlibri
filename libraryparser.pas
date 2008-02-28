//TODO: status per regex
unit libraryParser;

{$mode objfpc}{$H+}
interface

uses
  windows,Classes, SysUtils,extendedhtmlparser, simplexmlparser, inifiles,internetaccess,RegExpr;


type
  TLibrayTemplate=class;
  TCustomAccountAccess=class;
  TCustomAccountAccessClass=class of TCustomAccountAccess;

  { TLibrary }

  TLibrary=class
  protected
    defaultVariables:TStringList;
    function readProperty(tagName: string; properties: TProperties):boolean;
  public
    template:TLibrayTemplate;

    canModifySingleBooks:boolean;
    //function getPrettyNameShort():string;virtual;
    homepage:string;
    prettyNameLong:string;
    prettyNameShort:string;
    id:string;
    maxExtendCount: integer; //-1: if you can extend so frequently you want
    bestHomepageWidth,bestHomepageHeight: integer;
    allowHomepageNavigation: boolean;
    

    procedure loadFromFile(fileName:string);
    function getAccountObject():TCustomAccountAccess;virtual;
    destructor destroy;override;
  end;

  { TLibrayTemplate }

  TTemplatePageAccess=record
    url:string;
    templateFile:string;
    template:string;
    postparams:string;
  end;

  TTemplatePageAccessArray=array of TTemplatePageAccess;

  TTemplateAction=record
    pages:TTemplatePageAccessArray;
    errors: array of record
      templateFile:string;
      template:string;
    end;
    updateBefore,updateAfter:boolean;
    singleBookStr: string; //Nur benötigt für Bücherlisten
  end;
  PTemplateAction=^TTemplateAction;


  TLibrayTemplate=class
  protected
    currentAction:PTemplateAction;
    function readProperty(tagName: string; properties: TProperties): boolean;
  public
    usernameRegEx,passwordRegEx: TRegExpr;
    earMarkedRegEx,maxLimitRegEx,accountExpiredRegEx:TRegExpr;
    maxExtendCount: integer; //-1: if you can extend so frequently you want

    path:string;
    connect,update,updateSingle,extendAll,extendList,extendSingle:TTemplateAction;
    constructor create(_dataPath:string);
    procedure loadTemplates;
    destructor destroy;override;
    //function getAccountObject():TCustomAccountAccess;override;
  end;

  { TLibraryManager }

  TLibraryManager=class
  protected
    basePath,libraryPath: string;
    
    libraries: TList;
    templates:TStringList;
    function getAccountObject(libID: string):TCustomAccountAccess;
  public
    constructor create();
    destructor destroy();override;
    procedure init(apath:string);
    
    function getTemplate(templateName: string):TLibrayTemplate;
    function getAccount(libID,accountID: string):TCustomAccountAccess;overload;
    function getAccount(mixID: string):TCustomAccountAccess;overload;

    function enumeratePrettyLongNames: string;
    function enumeratePrettyShortNames: string;
    function getLibraryFromEnumeration(const pos:integer):TLibrary;
    function getLibraryCountInEnumeration:integer;
  end;

  TVariables=array of record
    name,value:string;
  end;
  TBookListType=(bltInOldData,bltInOldAdd,bltInCurrentFile,bltInCurrentDataUpdate);
  TBookStatus=(bsNormal,bsUnknown,bsIsSearched,bsEarMarked, bsMaxLimitReached,bsProblematicInStr,bsCuriousInStr,bsAccountExpired);
  PBook=^TBook;
  TBook=record
  //protected
    //persistent
    id,category,author,title,statusStr,otherInfo,year: string;
    issueDate,limitDate:longint;
    status: TBookStatus;

 // public
    lastExistsDate:longint;
    lib: TCustomAccountAccess;

    //temporary
    actuality: TBookListType;
    changed:boolean;
    charges: currency;
    additional: TVariables;

    twin: PBook; //contains the not updated version if in the bltCurrentData list
                                //contains the updated version otherwise
  end;
  TBookArray=array of PBook;

  TBookOutputType=(botAll,botCurrent,botOld);

  { TBookList }

  TBookLists=class
  protected
    ownerLib:TCustomAccountAccess;
    bookLists:array[TBookListType] of TList;
    bookListOldFileName,bookListCurFileName: string;
    bookListOldLoaded,bookListDataUpdateAvailable: boolean;
    keepHistory: boolean;
    //Call always from the sub threads
    function addBook(const id,title: string): PBook;
    //reset to stored state
    procedure deleteCurrentData();
    procedure compareBookList(final:boolean=true);//the final call sets stored dates (next limit, last update, ...)
    procedure updateSharedDates();
    procedure loadArrayFile(var bla: TList; fileName: string);
    procedure saveArrayFile(const bla: TList; fileName: string; replace: boolean);
    procedure save();
    procedure remove();

    procedure needOldBookList;
  public
    nextLimit,nextNotExtendableLimit:longint;
    //Call always from the MainThread
    constructor create(const AOwnerLib:TCustomAccountAccess; const oldFile,curFile: string);
    destructor destroy;override;

    //get book information
    //the books are sorted according to there lastExistsDate
    function getBookCount(typ: TBookOutputType): longint;
    function getBook(typ: TBookOutputType;i:longint): PBook;

    //This procedure is NOT thread save and
    //the call of ANY other function in this class
    //can result in a CRASH.
    //What it does:
    //replace bltInCurrentFile with bltInCurrentDataUpdate)
    //and add bltInOldAdd to bltInOldData
    procedure merge(deleteCurrent: boolean);
  end;

  ELibraryException=class(Exception)
    details:string;
    constructor create;
    constructor create(s:string;more_details:string='');
  end;

  { EWrongPasswordException }

  { ELoginException }

  ELoginException=class(ELibraryException)
    constructor create();
    constructor create(mes:string);
  end;

  TExtendType=(etAlways,etAllDepends,etSingleDepends,etNever);

  { TCustomAccountAccess }
                //first start info
  TCustomAccountAccess=class
  protected
    books: TBookLists;
    lib: TLibrary;
    internet: TInternetAccess;
    config: TIniFile;
    path,user,pass,FPrettyName:string;
    FExtendType: TExtendType;
    FExtendDays,FLastCheckDate:integer;

    FKeepHistory, connected: boolean;
    FCharges:Currency;
    function getCharges:currency;virtual;
  public
//    nextLimit,nextNotExtendableLimit:longint;
    isThreadRunning: boolean; //set to true before the thread is called
                              //set to false after the last change is done
                              //read whenever you want
    constructor create(alib: TLibrary);virtual;
    destructor destroy;override;
    
    
    procedure init(apath,userID:string);virtual;
    procedure save();
    procedure remove(); //DELETE the account

    //==============Access functions================
    //At first connect must be called
    function connect(AInternet:TInternetAccess=nil):boolean; virtual;
    //After disconnect you mustn't call any one except connect
    procedure disconnect(); virtual;
    
    //parseBooks will parse the books and get an book item for every one,
    //but it will not necessary get all information (=> needSingleBookCheck)
    //If needSingleBookCheck is set, then the books are possible not extended
    procedure parseBooks(extendAlways: boolean);   virtual;
    //parseBooks will parse every book to get more information
    procedure parseBooksOneByOne(extendAlways: boolean); virtual;
    //parseBooks will get more information about one book and extend it
    procedure parseSingleBooks(book: TBookArray;extendAlways: boolean); virtual;
    
    // /===========
    function checkMaximalBookTimeLimit:boolean;
    function shouldExtendSingle(const book:TBook) :boolean;
    function shouldExtendAll:boolean;


    function needSingleBookCheck():boolean;virtual;

    function getLibrary():TLibrary;

    function getBooks():TBookLists; virtual;

    procedure changeUser(const s:string); virtual;
    function getUser(): string;virtual;
    function getID():string; virtual;

    property charges: currency read getCharges;        //<0 means unknown

    
    property prettyName: string read FPrettyName write FPrettyName;
    property passWord: string read pass write pass;
    property lastCheckDate: integer read FLastCheckDate;
    property extendDays: integer read FExtendDays write FExtendDays;
    property extendType: TExtendType read FExtendType write FExtendType;
    property keepHistory: boolean read FKeepHistory write FKeepHistory;
  end;


  { TTemplateAccountAccess }

  TTemplateAccountAccess = class(TCustomAccountAccess)
  protected
    hasBeenUpdated:boolean;
    extendAllBooks:boolean;

    currentBook,defaultBook: PBook;

    parser: THtmlTemplateParser;
    procedure performAction(const action:TTemplateAction;const allowCalls:boolean=true);
    procedure parserVariableRead(variable: string; value: String);

    procedure selectBook(book:PBook);
    //procedure selectBook(variable,value: string); not needed

    procedure updateAll;
    procedure updateSingle(book: PBook);
    procedure extendAll;
    procedure extendList(book: TBookArray);
  public
    constructor create(alib: TLibrary);override;
    destructor destroy;override;
    //==============Access functions================
    function connect(AInternet:TInternetAccess=nil):boolean; override;
    procedure disconnect(); override;

    function needSingleBookCheck():boolean;override;

    procedure parseBooks(extendAlways: boolean);   override;
    //procedure parseBooksOneByOne(extendAlways: boolean); override;
    procedure parseSingleBooks(book: TBookArray;extendAlways: boolean); override;

    //function needSingleBookCheck():boolean;virtual;
  end;
const BOOK_NOT_EXTENDABLE=[bsEarMarked,bsMaxLimitReached,bsProblematicInStr,bsAccountExpired];
      BOOK_EXTENDABLE=[bsNormal,bsCuriousInStr];

var defaultInternet: TInternetAccess=nil;
implementation
uses applicationconfig,bbdebugtools,bbutils,FileUtil;
function currencyStrToCurrency(s:string):Currency;
begin
  s:=trim(s);
  if s='' then exit(0);
  if '.'<>DecimalSeparator then
    s:=StringReplace(s,'.',DecimalSeparator,[]);
{  if ','<>DecimalSeparator then
    s:=StringReplace(s,',',DecimalSeparator,[]);}
  if ','<>DecimalSeparator then
    s:=StringReplace(s,',',DecimalSeparator,[]);
  result:=StrToCurr(s);
{               temp:=trim(Utf8ToAnsi(pcharToString(text,textLen)));
               lastBookCharges:=strtoint(temp[length(temp-1])*10+temp[length(temp]))/100;
               delete(temp,length(temp)-2,3);
               lastBookCharges:=strtoint(lastBookCharges);}
end;

function TLibrary.readProperty(tagName: string; properties: TProperties):boolean;
var value:string;
begin
  tagName:=LowerCase(tagName);
  value:=getProperty('value',properties);
  if  tagName='homepage' then homepage:=value
  else if tagName='longname' then prettyNameLong:=value
  else if tagName='shortname' then prettyNameShort:=value
  else if tagName='longname' then prettyNameLong:=value
  else if tagName='singlebookextend' then canModifySingleBooks:=StrToBool(value)
  else if tagName='template' then template:=libraryManager.getTemplate(value)
  else if tagName='variable' then begin
    if defaultVariables=nil then defaultVariables:=TStringList.Create;
    defaultVariables.NameValueSeparator:='=';
    defaultVariables.Add(getProperty('name',properties)+defaultVariables.NameValueSeparator+value);
  end;
  Result:=true;
end;

procedure TLibrary.loadFromFile(fileName: string);
begin
  id:=ChangeFileExt(ExtractFileName(fileName),'');;
  parseXML(loadFileToStr(fileName),@readProperty,nil,nil,eWindows1252);
  if template<>nil then begin
    maxExtendCount:=template.maxExtendCount;
    canModifySingleBooks:=(length(template.extendSingle.pages)>0) or
                          (length(template.extendList.pages)>0) ;
  end;
end;

//==============================================================================
//                            TLibrary (+Manager)
//==============================================================================
function TLibrary.getAccountObject():TCustomAccountAccess;
begin
  result:=TTemplateAccountAccess.create(self);
  template.loadTemplates();
end;

destructor TLibrary.destroy;
begin
  defaultVariables.free;
  inherited destroy;
end;

function TLibraryManager.getAccountObject(libID: string):TCustomAccountAccess;
var i:integer;
begin
  Result:=nil;
  for i:=0 to libraries.count-1 do
    if TLibrary(libraries[i]).id=libID then
      exit(TLibrary(libraries[i]).getAccountObject());
  raise Exception('Bücherei '+libID+' ist unbekannt');
end;
constructor TLibraryManager.create();
begin
  libraries:=Tlist.create;
  templates:=tStringList.Create;
end;
destructor TLibraryManager.destroy();
var i:integer;
begin
  for i:=0 to libraries.Count-1 do
    TCustomAccountAccess(libraries[i]).free;
  libraries.free;
  for i:=0 to templates.Count-1 do
    TLibrayTemplate(templates.Objects[i]).free;
  templates.free;
  inherited;
end;

procedure TLibraryManager.init(apath: string);
var //tempLibrary:TLibrary;
    libraryFiles: TStringList;
    newLib:TLibrary;
    i:longint;
begin
  basePath:=apath;
  libraryPath:=dataPath+'libraries\';

  libraryFiles:=TStringList.Create;
  libraryFiles.LoadFromFile(libraryPath+'libraries.list');
  for i:=0 to libraryFiles.count-1 do begin
    newLib:=TLibrary.Create;
    newLib.loadFromFile(libraryPath+libraryFiles[i]);
    libraries.Add(newLib);
  end;
  libraryFiles.free;
end;

function TLibraryManager.getTemplate(templateName: string): TLibrayTemplate;
var i:longint;
begin
  i:=templates.IndexOf(templateName);
  if i>=0 then Result:=TLibrayTemplate(templates.Objects[i])
  else begin
    Result:=TLibrayTemplate.Create(libraryPath+'templates\'+templateName+'\');
    templates.AddObject(templateName,Result);
  end;
end;

function TLibraryManager.getAccount(libID,accountID: string):TCustomAccountAccess;
begin
  if logging then log('TLibraryManager.getAccount('+libID+','+accountID+') started');
  Result:=getAccountObject(libID);
  result.init(basePath,accountID);
  if logging then log('TLibraryManager.getAccount('+libID+','+accountID+') ended');
end;
function TLibraryManager.getAccount(mixID: string): TCustomAccountAccess;
var libID: string;
begin
  libID:=copy(mixID,1,3);
  delete(mixId,1,4);
  result:=getAccount(libID,mixID);
end;

function TLibraryManager.enumeratePrettyLongNames: string;
var i:integer;
begin
  result:='';
  for i:=0 to libraries.count-1 do
    result:=result+TLibrary(libraries[i]).prettyNameLong+#13#10;
end;
function TLibraryManager.enumeratePrettyShortNames: string;
var i:integer;
begin
  result:='';
  for i:=0 to libraries.count-1 do
    result:=result+TLibrary(libraries[i]).prettyNameShort+#13#10;
end;
function TLibraryManager.getLibraryFromEnumeration(const pos:integer):TLibrary;
begin
  Result:=TLibrary(libraries[pos]);
end;
function TLibraryManager.getLibraryCountInEnumeration:integer;
begin
  result:=libraries.count;
end;

//==============================================================================
//                            TBookList
//==============================================================================
function TBookLists.addBook(const id,title: string): PBook;
var i:integer; //TODO: optimize
    existAlready:integer;
begin
  if logging then log('TBookList.addBook started');
  existAlready:=-1;
  result:=nil;
  //Sucht die Instanz des Buches
  if keepHistory then
    for i:=0 to bookLists[bltInCurrentFile].count-1 do
      if (PBook(bookLists[bltInCurrentFile][i])^.id=id) and
         ((title='')or(PBook(bookLists[bltInCurrentFile][i])^.title=title)) then begin
        PBook(bookLists[bltInCurrentFile][i])^.actuality:=bltInCurrentDataUpdate;
        if PBook(bookLists[bltInCurrentFile][i])^.twin<>nil then
          result:=PBook(bookLists[bltInCurrentFile][i])^.twin;
{        result:=PBook(bookLists[bltInCurrentFile][i]);
        result^.lastExistsDate:=currentDate;
        exit;}
        existAlready:=i;
        break;
      end;
      
  if result=nil then new(result); //neues erstellen, wenn nicht vorhanden
  if existAlready<>-1 then begin
    //twin setzen, um zwischen alt und neu wechseln zu können
    PBook(bookLists[bltInCurrentFile][existAlready])^.twin:=result;
    result^.twin:=PBook(bookLists[bltInCurrentFile][existAlready]);
  end else result^.twin:=nil;
  
  //in die Liste einfügen
  bookLists[bltInCurrentDataUpdate].Add(result);
  
  //Initialisieren
  result^.id:=id;
  if title<>'' then result^.title:=title;
  result^.lib:=ownerLib;
  result^.actuality:=bltInCurrentDataUpdate;
  result^.lastExistsDate:=currentDate;
  result^.charges:=-1;
  if existAlready<>-1 then begin
    result^.issueDate:=result^.twin^.issueDate;
    result^.status:=result^.twin^.status;
    if result^.status in BOOK_EXTENDABLE then
      result^.status:=bsUnknown;
  end else begin
    result^.issueDate:=0;//Status:=bpsUnknown;
    result^.status:=bsUnknown;
  end;
  result^.statusStr:='';
  result^.changed:=existAlready=-1;
  if logging then log('TBookList.addBook ended');
end;
procedure TBookLists.deleteCurrentData();
var i:integer;
begin
  if logging then log('TBookLists.deleteCurrentData() started');
  for i:=0 to bookLists[bltInCurrentFile].Count-1 do begin
    PBook(bookLists[bltInCurrentFile][i])^.actuality:=bltInCurrentFile;
    PBook(bookLists[bltInCurrentFile][i])^.twin:=nil; //will be disposed in the next for-loop
  end;
  for i:=0 to bookLists[bltInCurrentDataUpdate].count-1 do
    if PBook(bookLists[bltInCurrentDataUpdate][i])^.actuality=bltInCurrentDataUpdate then
      dispose(PBook(bookLists[bltInCurrentDataUpdate][i]));
  bookLists[bltInCurrentDataUpdate].count:=0;
  if logging then log('TBookLists.deleteCurrentData() ended');
end;

//determines which books from bltInCurrentFile belongs to bltInOldAdd
procedure TBookLists.compareBookList(final: boolean=true);
var i:integer; //TODO: optimize
begin
  if logging then
    if final then log('TBookLists.compareBookList(true) started')
    else log('TBookLists.compareBookList(false) started');
  if keepHistory then
    for i:=0 to bookLists[bltInCurrentFile].count-1 do
      if PBook(bookLists[bltInCurrentFile][i])^.actuality<>bltInCurrentDataUpdate then begin
        PBook(bookLists[bltInCurrentFile][i])^.actuality:=bltInOldAdd;
        bookLists[bltInOldAdd].add(bookLists[bltInCurrentFile][i]);
      end else begin
        PBook(bookLists[bltInCurrentFile][i])^.actuality:=bltInCurrentFile;
      end;

  if final then updateSharedDates;

  bookListDataUpdateAvailable:=true;
  if logging then log('TBookLists.compareBookList() ended');
end;

procedure TBookLists.updateSharedDates();
var i:integer;
begin
  if logging then
    log('TBookLists.updateSharedDates(true) started');
  nextLimit:=MaxInt-1;
  nextNotExtendableLimit:=MaxInt;
  for i:=0 to bookLists[bltInCurrentFile].count-1 do
    if (PBook(bookLists[bltInCurrentFile][i])^.actuality in [bltInCurrentDataUpdate,bltInCurrentFile])
       and (PBook(bookLists[bltInCurrentFile][i])^.limitDate>0) then begin
      if PBook(bookLists[bltInCurrentFile][i])^.limitDate<nextLimit then
        nextLimit:=PBook(bookLists[bltInCurrentFile][i])^.limitDate;
      if PBook(bookLists[bltInCurrentFile][i])^.status in BOOK_NOT_EXTENDABLE then
        if PBook(bookLists[bltInCurrentFile][i])^.limitDate<nextNotExtendableLimit then
          nextNotExtendableLimit:=PBook(bookLists[bltInCurrentFile][i])^.limitDate;
    end;
  ownerLib.flastCheckDate:=currentDate;
  if logging then
    log('TBookLists.updateSharedDates(true) ended')
end;

procedure TBookLists.loadArrayFile(var bla: TList; fileName: string);
  function truncNull(var source: string):string;
  var p:integer;
  begin
    p:=pos(#0,source);
    result:=copy(source,1,p-1);
    delete(source,1,p);
  end;
  function truncNullDef(var source: string;def:string):string;
  var p:integer;
  begin
    p:=pos(#0,source);
    if p<=0 then exit(def);
    result:=copy(source,1,p-1);
    delete(source,1,p);
  end;
var sl:TStringList;
    line:string;
    i:integer;
    book:PBook;
begin
  if logging then
    log('TBookLists.loadArrayFile('+fileName+') started');
  if not FileExists(fileName) then exit;
  sl:=TStringList.create;
  sl.LoadFromFile(fileName);
  bla.Capacity:=sl.count;
  for i:=0 to sl.count-1 do begin
    new(book);
    fillchar(book^,sizeof(book^),0);
    with book^ do begin
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
      lib:=ownerLib;
      changed:=false;
    end;
    bla.add(book);
  end;
  sl.free;
  if logging then
    log('TBookLists.loadArrayFile('+fileName+') ended')
end;

procedure TBookLists.saveArrayFile(const bla: TList; fileName: string; replace: boolean);
var text:TextFile;
    i:integer;
begin
  if logging then
    log('TBookLists.saveArrayFile('+fileName+') started');
  if not replace and not FileExists(fileName) then replace:=true;
  assign(text,fileName);
  if replace then rewrite(text)
  else Append(text);
  for i:=0 to bla.count-1 do
    with PBook(bla[i])^ do
      if status in [bsProblematicInStr,bsCuriousInStr] then
        writeln(text,id+#0+category+#0+author+#0+title+#0+statusStr+#0+otherInfo+#0+
                     IntToStr(trunc(issueDate))+#0+IntToStr(trunc(limitDate))+#0+
                     IntToStr(trunc(lastExistsDate))+#0+inttostr(integer(status))+#0+year+#0)
       else
        writeln(text,id+#0+category+#0+author+#0+title+#0+#0+otherInfo+#0+
                     IntToStr(trunc(issueDate))+#0+IntToStr(trunc(limitDate))+#0+
                     IntToStr(trunc(lastExistsDate))+#0+inttostr(integer(status))+#0+year+#0);

  close(text);
  if logging then
    log('TBookLists.saveArrayFile('+fileName+') ended')
end;

procedure TBookLists.needOldBookList;
var temp:TList;
    i:integer;
begin
  if not keepHistory then exit;
  if not bookListOldLoaded then begin
    if bookLists[bltInOldData].count>0 then begin
      temp:=TList.create;
      temp.Assign(bookLists[bltInOldData]);
      bookLists[bltInOldData].count:=0;
      loadArrayFile(bookLists[bltInOldData],bookListOldFileName);
      bookLists[bltInOldData].capacity:=bookLists[bltInOldData].count+
                                        temp.count;
      for i:=0 to temp.count-1 do
        bookLists[bltInOldData].Add(temp[i]);
      temp.free;
    end else
      loadArrayFile(bookLists[bltInOldData],bookListOldFileName);
    bookListOldLoaded:=true;
    for i:=0 to bookLists[bltInOldData].count-1 do
      PBook(bookLists[bltInOldData][i])^.actuality:=bltInOldData;
  end;
end;

procedure  TBookLists.save;
var i:integer;
begin
  if logging then
    log('TBookLists.save started');
  if keepHistory then begin
    if currentDate-ownerLib.config.ReadInteger('base','last-history-backup',0)>HistoryBackupInterval then begin
      CopyFile(bookListOldFileName,bookListOldFileName+'.'+FormatDateTime('yyyymmdd',currentDate));
      ownerLib.config.WriteInteger('base','last-history-backup',currentDate);
    end;
    if bookListOldLoaded then saveArrayFile(bookLists[bltInOldData],bookListOldFileName,true)
    else if bookLists[bltInOldData].Count>0 then begin
      saveArrayFile(bookLists[bltInOldData],bookListOldFileName,false);
      for i:=0 to bookLists[bltInOldData].count-1 do
        dispose(PBook(bookLists[bltInOldData][i]));
      bookLists[bltInOldData].count:=0;
    end;
  end;
  saveArrayFile(bookLists[bltInCurrentFile],bookListCurFileName,true);
  if logging then
    log('TBookLists.save ended')
end;

procedure  TBookLists.remove;
begin
  DeleteFile(bookListCurFileName);
  DeleteFile(bookListOldFileName);
end;

constructor TBookLists.create(const AOwnerLib:TCustomAccountAccess; const oldFile,curFile: string);
var i:TBookListType;
    j:longint;
begin
  if logging then
    log('TBookLists.create started');
  ownerLib:=AOwnerLib;
  bookListCurFileName:=curFile;
  bookListOldFileName:=oldFile;
  bookListOldLoaded:=false;
  for i:=low(bookLists) to high(bookLists) do
    bookLists[i]:=TList.create;
  loadArrayFile(bookLists[bltInCurrentFile],curFile);
  nextLimit:=$7fffffff;
  nextNotExtendableLimit:=$7fffffff;
  for j:=0 to bookLists[bltInCurrentFile].count-1 do
    with PBook(bookLists[bltInCurrentFile][j])^ do begin
      actuality:=bltInCurrentFile;
      if limitDate<=0 then continue; //error/unbekanntes abgabedatum
      if limitDate<nextLimit then nextLimit:=limitDate;
      if (status in BOOK_NOT_EXTENDABLE) and (limitDate<nextNotExtendableLimit) then
        nextNotExtendableLimit:=limitDate;
    end;
  if nextLimit<applicationconfig.nextLimit then
    applicationconfig.nextLimit:=nextLimit;
  if nextNotExtendableLimit<applicationconfig.nextNotExtendableLimit then
    applicationconfig.nextNotExtendableLimit:=nextNotExtendableLimit;
  if logging then
    log('TBookLists.create ended')
end;
destructor TBookLists.destroy;
var j:integer;
    i:TBookListType;
begin
  for i:=low(bookLists) to high(bookLists) do begin
    for j:=0 to bookLists[i].count-1 do
      dispose(PBook(bookLists[i][j]));
    bookLists[i].free;
  end;
  inherited;
end;

function TBookLists.getBookCount(typ: TBookOutputType): longint;
begin
  if typ <> botCurrent then needOldBookList;
  case typ of
    botAll:  result:=bookLists[bltInCurrentFile].count+bookLists[bltInOldData].count;
    botCurrent:  result:=bookLists[bltInCurrentFile].count;
    botOld:  result:=bookLists[bltInOldData].count;
  end;
end;
function TBookLists.getBook(typ: TBookOutputType;i: longint): PBook;
begin
  if typ <> botCurrent then needOldBookList;
  case typ of
    botAll:  if i>=bookLists[bltInOldData].count then
                 result:=PBook(bookLists[bltInCurrentFile][i-bookLists[bltInOldData].count])
            else result:=PBook(bookLists[bltInOldData][i]);
    botCurrent: result:=PBook(bookLists[bltInCurrentFile][i]);
    botOld:     result:=PBook(bookLists[bltInOldData][i]);
  end;
end;

procedure TBookLists.merge(deleteCurrent: boolean);
var j:integer;
begin
  //Current book list REPLACE
  if logging then
    if deleteCurrent then log('TBookLists.merge(true) started')
    else log('TBookLists.merge(false) started');
  if bookListDataUpdateAvailable then begin
  
    for j:=0 to bookLists[bltInCurrentDataUpdate].count-1 do
      PBook(bookLists[bltInCurrentDataUpdate][j])^.actuality:=bltInCurrentDataUpdate;
    for j:=0 to bookLists[bltInCurrentFile].count-1 do
      if PBook(bookLists[bltInCurrentFile][j])^.actuality=bltInCurrentFile then
        dispose(PBook(bookLists[bltInCurrentFile][j]));
    bookLists[bltInCurrentFile].assign(bookLists[bltInCurrentDataUpdate]);
    for j:=0 to bookLists[bltInCurrentFile].count-1 do
      PBook(bookLists[bltInCurrentFile][j])^.twin:=nil; //disposed in the loop above
    if deleteCurrent then
      bookLists[bltInCurrentDataUpdate].clear();

    //Old book list ADD
    for j:=0 to bookLists[bltInOldAdd].count-1 do
      bookLists[bltInOldData].add(bookLists[bltInOldAdd][j]);
    bookLists[bltInOldAdd].Clear;
    bookListDataUpdateAvailable:=false;
  end;

  if logging then
    log('TBookLists.merge ended')
end;


//==============================================================================
//                            TCustomAccountAccess
//==============================================================================

constructor ELibraryException.create();
begin
  message:='Leider habe ich die Antwort der Bücherei auf die Anfrage nicht verstanden. Möglicher Ursachen:'#13#10+
                                   '1. Ein falsch eingegebenes Passwort/Geburtsdatum (lässt sich unter Extras\Einstellungen überprüfen)'#13#10+
                                   '2. Programmierfehler in mir oder dem WebOPAC der Bücherei'#13#10+
                                   '3. Änderung des WebOPACs'#13#10+
                                   '4. Eine unterbrochene Internetverbindung';
end;
constructor ELibraryException.create(s:string;more_details:string='');
begin
  if s='' then
    message:='Leider habe ich die Antwort der Bücherei auf die Anfrage nicht verstanden. Möglicher Ursachen:'#13#10+
                                  '1. Ein falsch eingegebenes Passwort/Geburtsdatum (lässt sich unter Extras\Einstellungen überprüfen)'#13#10+
                                   '2. Programmierfehler in mir oder dem WebOPAC der Bücherei'#13#10+
                                   '3. Änderung des WebOPACs'#13#10+
                                   '4. Eine unterbrochene Internetverbindung'
  else
    message:=s;
  details:=more_details;
end;

function TCustomAccountAccess.checkMaximalBookTimeLimit:boolean;
var i,extendDayLimit:integer;
begin
  if logging then log('TCustomAccountAccess.checkMaximalBookTimeLimit started');
  result:=false;
  extendDayLimit:=currentDate+extendDays;
  log(prettyName+':'+inttostr(books.bookLists[bltInCurrentDataUpdate].count-1)+' '+IntTostr(books.bookLists[bltInCurrentFile].count-1));
  for i:=0 to books.bookLists[bltInCurrentDataUpdate].count-1 do begin
  {  log(PBook(books.bookLists[bltInCurrentFile][i])^.title);
    log(inttostr(PBook(books.bookLists[bltInCurrentFile][i])^.limitDate));
    log(inttostr(longint(PBook(books.bookLists[bltInCurrentFile][i])^.status)));}
    if (PBook(books.bookLists[bltInCurrentDataUpdate][i])^.limitDate <= extendDayLimit) and
       (PBook(books.bookLists[bltInCurrentDataUpdate][i])^.status in BOOK_EXTENDABLE)then
      exit(true);
  end;
  if not connected then
    for i:=0 to books.bookLists[bltInCurrentFile].count-1 do begin
{    log(PBook(books.bookLists[bltInCurrentFile][i])^.title);
    log(inttostr(PBook(books.bookLists[bltInCurrentFile][i])^.limitDate));
    log(inttostr(longint(PBook(books.bookLists[bltInCurrentFile][i])^.status))); }
      if (PBook(books.bookLists[bltInCurrentFile][i])^.limitDate <= extendDayLimit) and
         (PBook(books.bookLists[bltInCurrentFile][i])^.status in BOOK_EXTENDABLE)then
        exit(true);
    end;
  if logging then log('TCustomAccountAccess.checkMaximalBookTimeLimit ended: false');
end;

function TCustomAccountAccess.shouldExtendSingle(
  const book: TBook): boolean;
begin
  result:=(extendType=etSingleDepends) and (book.status in BOOK_EXTENDABLE)
          and (book.limitDate<=currentDate+extendDays);
end;

function TCustomAccountAccess.shouldExtendAll: boolean;
begin
  result:=(extendType = etAlways) or
          ((extendType = etAllDepends) and checkMaximalBookTimeLimit);
end;


function TCustomAccountAccess.getCharges: currency;
begin
  result:=fcharges;
end;

constructor TCustomAccountAccess.create(alib:TLibrary);
begin
  connected:=false;
  books:=nil;
  currentDate:=longint(trunc(date));
  config:=nil;
  isThreadRunning:=false;
  lib:=alib;
  fcharges:=-1;
end;

destructor TCustomAccountAccess.destroy;
begin
  //if connected then disconnect;
  if books<>nil then
    books.free;
  if config<>nil then
    config.free;
  inherited;
end;

procedure TCustomAccountAccess.init(apath,userID:string);
begin
  self.path:=apath;
  self.user:=userID;
  
  if FileExists(path+getID()+'.old') then begin
    //Es handelt sich um eine ältere Version (<=0.95)
    //damals hießen die Dateien .old/.current und waren im Win1252 Zeichensatz
    saveFileFromStr(path+getID()+'.history',AnsiToUtf8(loadFileToStr(path+getID()+'.old')));
    saveFileFromStr(path+getID()+'.current',AnsiToUtf8(loadFileToStr(path+getID()+'.current')));
    DeleteFile(path+getID()+'.old');
  end;
  
  //Datenladen/cachen
  books:=TBookLists.create(self,path+getID()+'.history',path+getID()+'.current');
  config:=TIniFile.Create(path+getID()+'.config');
  pass:=config.ReadString('base','pass','');
  books.keepHistory:=config.ReadBool('base','keep-history',true);
  flastCheckDate:=config.ReadInteger('base','lastCheck',0);
  keepHistory:=config.readBool('base','keep-history',true);
  prettyName:=config.readString('base','prettyName',userID);
  extendDays:=config.readInteger('base','extend-days',7);
  extendType:=TExtendType(config.readInteger('base','extend',0));
  fcharges:=currency(config.readInteger('base','charge',-100))/100;;
end;

procedure TCustomAccountAccess.save();
begin
  if books<>nil then
    books.save();
  if config<>nil then begin
    config.WriteInteger('base','lastCheck',FLastCheckDate);
    config.WriteBool('base','keep-history',keepHistory);
    config.WriteString('base','prettyName',prettyName);
    config.WriteString ('base','pass',passWord);
    config.WriteInteger('base','extend-days',extendDays);
    config.WriteInteger('base','extend',integer(extendType));
    config.WriteInteger('base','charge',longint(trunc(charges*100)));

    config.UpdateFile;
  end;
end;

procedure TCustomAccountAccess.remove();
begin
  books.remove();
  FreeAndNil(config);
  DeleteFile(path+getID()+'.config');
end;

{function TCustomAccountAccess.lastCheck():longint;
begin
  result:=config.ReadInteger('base','lastCheck',0);
end;}

function TCustomAccountAccess.connect(AInternet:TInternetAccess=nil):boolean;
begin
  internet:=AInternet;
  if internet=nil then
    internet:=defaultInternet;
  result:=internet<>nil;
end;

procedure TCustomAccountAccess.disconnect();
begin
  connected:=false;
end;

procedure TCustomAccountAccess.parseBooks(extendAlways: boolean);
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;


{function TCustomAccountAccess.getCharges():currency;
begin
  result:=-1;
end;}


procedure TCustomAccountAccess.parseBooksOneByOne(extendAlways: boolean);
var i:integer;
    ar:TBookArray;
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
  setlength(ar,books.bookLists[bltInCurrentFile].count);
  for i:=0 to books.bookLists[bltInCurrentFile].count-1 do
    ar[i]:=PBook(books.bookLists[bltInCurrentFile][i]);
  parseSingleBooks(ar,extendAlways);
  books.updateSharedDates();
end;

procedure TCustomAccountAccess.parseSingleBooks(book: TBookArray;extendAlways: boolean);
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;


function TCustomAccountAccess.needSingleBookCheck():boolean;
begin
  result:=false;
end;

function TCustomAccountAccess.getLibrary():TLibrary;
begin
  result:=lib;
end;


function TCustomAccountAccess.getBooks():TBookLists;
begin
  result:=books;
end;

procedure TCustomAccountAccess.changeUser(const s:string);
var oldID,newID:string;
begin
  oldID:=getID();
  books.Free;
  config.free;
  
  user:=s;
  newID:=getID();
  RenameFile(path+oldID+'.history',path+newID+'.history');
  RenameFile(path+oldID+'.current',path+newID+'.current');
  RenameFile(path+oldID+'.config',path+newID+'.config');
  books:=TBookLists.create(self,path+getID()+'.history',path+getID()+'.current');
  config:=TIniFile.Create(path+getID()+'.config');
  //config.UpdateFile;
end;

function TCustomAccountAccess.getUser(): string;
begin
  result:=user;
end;

function TCustomAccountAccess.getID():string;
begin
  result:=lib.id+'#'+user;
end;

{ EWrongPasswordException }

constructor ELoginException.create();
begin
  create('');
end;

constructor ELoginException.create(mes: string);
begin
  if mes='' then mes:='Leider ist entweder der eingegebene Kontoname oder das Passwort ungültig.'#13#10#13#10'Über den Button Einstellungen können Sie Ihre Passworte überprüfen'#13#10+
                   'Tip: Bei der STB ist das Passwort das Geburtsdatum mit Punkten, bei FHB und UB (anfänglich) dieses Datum ohne Punkte.';
  Message:=mes;
end;

{ TLibrayTemplate }

function TLibrayTemplate.readProperty(tagName: string; properties: TProperties): boolean;
 procedure changeAction(newAction: PTemplateAction);
 begin
   currentAction:=newAction;
   currentAction^.singleBookStr:=getProperty('singleBookStr',properties);
   currentAction^.updateAfter:=StrToBoolDef(getProperty('update-after',properties),false);
   currentAction^.updateBefore:=StrToBoolDef(getProperty('update-before',properties),false);
 end;
  
var i:longint;
    name:string;
begin
  if SameText(tagName,'maxExtendCount') then
    maxExtendCount:=StrToInt(getProperty('value',properties))
  else if SameText(tagName,'earmarked') then begin
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
  end else if SameText(tagName,'username') then begin
    usernameRegEx:=TRegExpr.Create(Utf8ToAnsi(getProperty('matches',properties)));
    if usernameRegEx.Expression='' then
      raise exception.create('tag <usernameRegEx> ungültig in Template von '+path);
  end else if SameText(tagName,'password') then begin
    passwordRegEx:=TRegExpr.Create(Utf8ToAnsi(getProperty('matches',properties)));
    if passwordRegEx.Expression='' then
      raise exception.create('tag <passwordRegEx> ungültig in Template von '+path);
  end else if SameText(tagName,'error') then begin
    SetLength(currentAction^.errors,length(currentAction^.errors)+1);
    currentAction^.errors[high(currentAction^.errors)].
      templateFile:=Utf8ToAnsi(getProperty('templateFile',properties));
  end else if SameText(tagName,'page') then begin
    SetLength(currentAction^.pages,length(currentAction^.pages)+1);
    with currentAction^.pages[high(currentAction^.pages)] do begin
      postparams:='';
      for i:=0 to high(properties) do
        if SameText(properties[i].name,'url') then
          url:=Utf8ToAnsi(properties[i].value)
        else if SameText(properties[i].name,'templateFile') then
          templateFile:=Utf8ToAnsi(properties[i].value)
        else if SameText(properties[i].name,'post-params') then
          postparams:=Utf8ToAnsi(properties[i].value)
    end;
  end else if SameText(tagName,'action') then begin
    name:=LowerCase(getProperty('id',properties));
    if name='update' then begin
      changeAction(@update);
      if update.updateBefore or update.updateAfter then
        raise Exception.Create('Ungültiges Template in '+dataPath+#13#10'Es führt zu update-Rekursion');
    end else if name='update-single'then
      changeAction(@updateSingle)
    else if name='extend-all'then
      changeAction(@extendAll)
    else if name='extend-list' then
      changeAction(@extendList)
    else if name='extend-single'then
      changeAction(@extendSingle)
    else if name='connect'then
      changeAction(@connect)
    else raise Exception.Create('Unbekannte Templateaktion: '+name);
  end;
  result:=true;
end;

constructor TLibrayTemplate.create(_dataPath: string);
begin
  IncludeTrailingPathDelimiter(_dataPath);
  self.path:=_dataPath;
  earMarkedRegEx:=TRegExpr.Create('[vV]orgemerkt');
  maxLimitRegEx:=TRegExpr.Create('[fF]rist erreicht');
  accountExpiredRegEx:=TRegExpr.Create('Karte abgelaufen');
  maxExtendCount:=-1;
  parseXML(loadFileToStr(_dataPath+'template'),@readProperty,nil,nil,eUTF8);
end;

procedure TLibrayTemplate.loadTemplates;
  procedure load(var action:TTemplateAction);
  var i:longint;
  begin

    for i:=0 to high(action.pages) do begin
      if action.pages[i].templateFile='' then continue;
      action.pages[i].template:=loadFileToStr(self.path+action.pages[i].templateFile);
      if action.pages[i].template='' then
        raise ELibraryException.create('Template-Datei "'+self.path+action.pages[i].templateFile+'" konnte nicht geladen werden');
    end;
    for i:=0 to high(action.errors) do begin
      action.errors[i].template:=loadFileToStr(self.path+action.errors[i].templateFile);
      if action.errors[i].template='' then
        raise ELibraryException.create('Template-Datei "'+self.path+action.errors[i].templateFile+'" konnte nicht geladen werden');
    end;
  end;
begin
   load(connect);
   load(update);
   load(updateSingle);
   load(extendAll);
   load(extendList);
   load(extendSingle);
end;

destructor TLibrayTemplate.destroy;
begin
  usernameRegEx.free;
  passwordRegEx.free;
  earMarkedRegEx.free;
  maxLimitRegEx.free;
  accountExpiredRegEx.free;
  inherited;
  
end;


{ TTemplateAccountAccess }

procedure TTemplateAccountAccess.parserVariableRead(variable: string;
  value: String);


  function strconv(s:string):string;
  begin
    result:=StringReplace(result,#13,'',[rfReplaceAll]);
    result:=StringReplace(result,#10,'',[rfReplaceAll]);
    result:=trim(s);
  end;


begin
  if logging then
    log('** Read variable: "'+variable+'" = "'+value+'"');
  if variable='charge' then begin
    FCharges:=currencyStrToCurrency(value);
  end else //Benachrichtigungsfunktionen
   if variable='delete-current-books()' then begin
    books.deleteCurrentData();
  end else if variable='book-start()' then begin
    //reset
    currentBook:=defaultBook;
    currentBook^.Id:='';
    currentBook^.category:='';
    currentBook^.Title:='';
    currentBook^.Author:='';
    currentBook^.year:='';
    currentBook^.StatusStr:='';
    currentBook^.Status:=bsUnknown;
    currentBook^.limitDate:=0;
    currentBook^.issueDate:=0;
    SetLength(currentBook^.Additional,0);
  end else if variable='book-end()' then begin
    with books.addBook(currentBook^.Id,currentBook^.Title)^ do begin
      category:=currentBook^.Category;
      author:=currentBook^.Author;
      year:=currentBook^.Year;
      
      issueDate:=currentBook^.IssueDate;
      limitDate:=currentBook^.LimitDate;
      if currentBook^.status<>bsUnknown then begin
        statusStr:=currentBook^.StatusStr;
        status:=currentBook^.Status;
      end else if twin<>nil then begin
        statusStr:=twin^.StatusStr;
        status:=twin^.Status;
        currentBook^.statusStr:=twin^.StatusStr;
        currentBook^.status:=twin^.Status;
      end;
      additional:=currentBook^.Additional;
    end;
  end else if variable='book-select()' then begin
    //reset
    raise ELibraryException.create('not implemented yet');
  end else if variable='raise()' then begin
    raise ELibraryException.create(value);
  end else if variable='raise-login()' then begin
    raise ELoginException.create(value);
  end //Büchereigenschaften
  else if variable='book.category' then currentBook^.Category:=strconv(value)
  else if variable='book.id' then currentBook^.Id:=strconv(value)
  else if variable='book.author' then currentBook^.Author:=strconv(value)
  else if variable='book.title' then currentBook^.Title:=strconv(value)
  else if variable='book.year' then currentBook^.Year:=strconv(value)
  else if strlibeginswith(@variable[1],length(variable),'book.status') then begin
    if variable='book.status:problematic' then currentBook^.Status:=bsProblematicInStr
    else if variable='book.status:curious' then currentBook^.Status:=bsCuriousInStr;
    if value<>'' then begin
      currentBook^.StatusStr:=strconv(value);
      if lib.template.earMarkedRegEx.Exec(value) then currentBook^.Status:=bsEarMarked
      else if lib.template.maxLimitRegEx.Exec(value) then currentBook^.Status:=bsMaxLimitReached
      else if lib.template.accountExpiredRegEx.Exec(value) then currentBook^.Status:=bsAccountExpired;
    end else currentBook^.Status:=bsNormal;
       //(bsNormal,bsUnknown,bsIsSearched,bsEarMarked,bsMaxLimitReached,bsProblematicInStr,bsCuriousInStr);
  end else if strlibeginswith(@variable[1],length(variable),'book.issuedate') then
    currentBook^.IssueDate:=parseDate(Utf8ToAnsi(strconv(value)),copyfrom(variable,pos(':',variable)+1))
  else if strlibeginswith(@variable[1],length(variable),'book.limitdate') then
    currentBook^.LimitDate:=parseDate(Utf8ToAnsi(strconv(value)),copyfrom(variable,pos(':',variable)+1))
  else if strlibeginswith(variable,'book.') then begin
    SetLength(currentBook^.Additional,length(currentBook^.Additional)+1);
    currentBook^.Additional[high(currentBook^.Additional)].name:=variable;
    currentBook^.Additional[high(currentBook^.Additional)].value:=value;
  end;

{  else raise ELibraryException.create('Fehler im Büchereistrukturtemplate, entweder wurde VideLibri beschädigt, oder es ist fehlerhaft programmiert.'#13#10'Lösungsmöglichkeiten: Neuinstallation oder Update',
    'Der Fehler ist bei der Bücherei '+lib.prettyNameShort+' im Template '+lib.template.path+' aufgetreten. '#13#10+
    'Unbekannte Variable '+variable+' mit Wert '+value);}
end;

procedure TTemplateAccountAccess.selectBook(book: PBook);
var i:longint;
begin
  parser.variables.Values['book.id']:=book^.id;
  for i:=0 to high(book^.additional) do
    parser.variables.Values[book^.additional[i].name]:=book^.additional[i].value;
  currentBook:=book;
end;

{procedure TTemplateAccountAccess.selectBook(variable, value: string);
  function ok(const book: TBook): boolean;
  var i:longint;
  begin
    for i:=0 to high(book.additional) do
      if (CompareText(book.additional[i].name,variable)=0) and
         (CompareText(book.additional[i].value,value)=0) then
        exit(true);
    exit(false);
  end;

var i:longint;
begin
  for i:=0 to books.bookLists[bltInCurrentDataUpdate].Count-1 do
    if ok(PBook(books.bookLists[bltInCurrentDataUpdate][i])^) then begin
      selectBook(PBook(books.bookLists[bltInCurrentDataUpdate][i]));
      exit;
    end;
  for i:=0 to books.bookLists[bltInCurrentFile].Count-1 do
    if ok(PBook(books.bookLists[bltInCurrentFile][i])^) then begin
      selectBook(PBook(books.bookLists[bltInCurrentFile][i]));
      exit;
    end;
  raise ELibraryException.create('Medium mit '+variable+'='+value+' nicht gefunden.');
end;}

procedure TTemplateAccountAccess.updateAll;
begin
  if logging then log('Enter TTemplateAccountAccess.updateAll');
  performAction(lib.template.update);
  hasBeenUpdated:=true;
  if logging then log('Leave TTemplateAccountAccess.updateAll');
end;

procedure TTemplateAccountAccess.updateSingle(book: PBook);
begin
  if logging then
    log('enter TTemplateAccountAccess.updateSingle');
  if length(lib.template.updateSingle.pages)=0 then begin
    updateAll;
    exit;
  end;
  selectBook(book);
  performAction(lib.template.updateSingle);
  if logging then
    log('leave TTemplateAccountAccess.updateSingle');

end;

procedure TTemplateAccountAccess.extendAll;
var temp: TBookArray;
    listType:TBookListType;
    i:longint;
begin
  if logging then
    log('enter TTemplateAccountAccess.extendAll');
  if length(lib.template.extendAll.pages)>0 then begin
    performAction(lib.template.extendAll);
  end else begin
    if books.bookLists[bltInCurrentDataUpdate].Count=0 then
      listType:=bltInCurrentDataUpdate
     else
      listType:=bltInCurrentFile;
    SetLength(temp,books.bookLists[listType].Count-1);
    for i:=0 to books.bookLists[listType].count-1 do
      temp[i]:=books.bookLists[listType][i];
    if length(temp)>0 then
      extendList(temp);
  end;
  if logging then
    log('leave TTemplateAccountAccess.extendAll');
end;

procedure TTemplateAccountAccess.extendList(book: TBookArray);
  function realBook(book: PBook): PBook;
  begin
    if book^.twin=nil then exit(book)
    else if length(book^.additional)>=length(book^.twin^.additional) then exit(book)
    else if length(book^.additional)<length(book^.twin^.additional) then exit(book^.twin)
    else if book^.twin^.lastExistsDate<=book^.lastExistsDate then exit(book)
    else if book^.twin^.lastExistsDate>book^.lastExistsDate then exit(book^.twin);
  end;
var bookListStr:string;
    i:longint;
begin
  if length(book)=0 then exit;
  if logging then log('Enter TTemplateAccountAccess.extendList');
  if length(lib.template.extendList.pages)>0 then begin
    if logging then log('use extendList Template');
    if Lib.template.extendList.updateBefore then
      if not hasBeenUpdated then
        updateAll;
    bookListStr:='';
    for i:=0 to high(book) do begin
{      parser.variables.Values['book.extendID']:=realBook(book[i])^.bookDataAccess;
      log('b: '+book[i]^.bookDataAccess);
      if book[i]^.twin<>nil then log('b.t: '+book[i]^.twin^.bookDataAccess);
      parser.variables.Values['book.id']:=realBook(book[i])^.id;}
      selectBook(book[i]);
      bookListStr+= parser.replaceVars(lib.template.extendList.singleBookStr);
    end;
    if logging then log('bookList is: '+bookListStr);
    parser.variables.Values['book-list']:=bookListStr;
    performAction(lib.template.extendList,false);
    if Lib.template.extendList.updateAfter then
      updateAll;
  end else if length(lib.template.extendSingle.pages)>0 then begin
    if logging then log('use extendSingle Template');
    if Lib.template.extendSingle.updateBefore then begin
      if not hasBeenUpdated then
        updateAll();
    end;
    for i:=0 to high(book) do begin
      selectBook(book[i]);
      performAction(lib.template.extendSingle,false);
    end;
    if Lib.template.extendSingle.updateAfter then
      updateAll;
  end else if length(lib.template.extendAll.pages)>0 then begin
    if logging then log('use extendAll Template');
    performAction(lib.template.extendAll);
  end;
  if logging then log('Leave TTemplateAccountAccess.extendList');
end;


procedure TTemplateAccountAccess.performAction(const action:TTemplateAction;const allowCalls:boolean=true);
var i:longint;
    page:string;
    parserLog: TTemplateHTMLParserLogClass;
begin
  if logging then begin
    log('Enter performAction');
    parserLog:=TTemplateHTMLParserLogClass.Create;
    parserLog.parser:=parser;
    parser.onEnterTag:=@parserLog.et;
    parser.onLeaveTag:=@parserLog.lt;
    parser.onTextRead:=@parserLog.tr;
  end;

  try
    //OutputDebugString(pchar(lib.defaultVariables.Text));
    Assert(lib<>nil,'Keine Bücherei angegeben');
    Assert(lib.defaultVariables<>nil,'Keine Variablen angegeben');
    Assert(internet<>nil,'Internet nicht initialisiert');
    if allowCalls and (action.updateBefore) then
      if not hasBeenUpdated then
        updateAll;


    with action do begin
      for i:=0 to lib.defaultVariables.count-1 do
        parser.variables.Values[lib.defaultVariables.Names[i]]:=lib.defaultVariables.ValueFromIndex[i];
      parser.variables.Values['username']:=user;
      parser.variables.Values['password']:=passWord;

      try
        for i:=0 to high(pages) do begin
          if pages[i].template<>'' then begin
            if logging then log('Parse Template From File: '+lib.template.path+pages[i].templateFile);
            parser.parseTemplate(pages[i].template);
          end;
          if logging then log('Get/Post internet page'+parser.replaceVars(pages[i].url)+#13#10'Post: '+parser.replaceVars(pages[i].postparams));
          if pages[i].postparams='' then
            page:=internet.get(parser.replaceVars(pages[i].url))
           else
            page:=internet.post(parser.replaceVars(pages[i].url),
                                           parser.replaceVars(pages[i].postparams));
                                           
          if logging then log('downloaded');
          if page='' then
            raise EInternetException.Create(pages[i].url +' konnte nicht geladen werden');
          if pages[i].template<>'' then begin
            if logging then log('parse page: '+parser.replaceVars(pages[i].url));
            parser.parseHTML(page);
          end;
          if logging then log('page finished');
        end;
        if logging then log('pages finished');
      except
        on  e:Exception do begin
          if logging then begin
            log(parserLog.text);
            parserLog.text:='';
            log('Exception message: '+e.message);
          end;
          for i:=0 to high(errors) do begin
            if logging then log('Check error: '+IntToStr(i)+': '+errors[i].templateFile);
            parser.parseTemplate(errors[i].template);
            if logging then begin
              log('Error template loaded');
              parserLog.text:='';
            end;
            try
              if logging then log('parser html: ');
              parser.parseHTML(page);
              if logging then log('parserlog (1): '+parserLog.text);
            except
              on e2: ELibraryException do begin
                if logging then log('New exception: '+e2.message);
                raise;
              end;
              on e: Exception do begin//frühere Exception wird weitergegeben/unverständlich
                if logging then log('parserlog (2): '+parserLog.text);
              end;
            end;
          end;
          if e is EHTMLParseException then
            if pos('Die HTML Datei ist kürzer als das Template',e.message)=1 then begin
              if logging then log('ele.create');
              raise ELibraryException.create('',e.message);
            end;
          if logging then log('parserlog (3): '+parserLog.text);
          raise;
        end;
      end;
    end;

    if allowCalls and (action.updateAfter) then
        updateAll;
  finally
    if logging then begin
      parser.onEnterTag:=nil;
      parser.onLeaveTag:=nil;
      parser.onTextRead:=nil;
      parserLog.Free;
      log('Leave performAction');
    end;
  end;
end;

constructor TTemplateAccountAccess.create(alib: TLibrary);
begin
  inherited;
  assert(alib.template<>nil,'Keine Templatelibrary-Class');
  parser:=THtmlTemplateParser.create;
  parser.onVariableRead:=@parserVariableRead;
  hasBeenUpdated:=false;
  new(defaultBook);
  currentBook:=defaultBook;
end;

destructor TTemplateAccountAccess.destroy;
begin
  dispose(defaultBook);
  parser.free;
  inherited destroy;
end;

function TTemplateAccountAccess.connect(AInternet: TInternetAccess): boolean;
begin
  if logging then log('TTemplateAccountAccess.connect started');
  if connected then begin
    if logging then log('TTemplateAccountAccess.connect ended (already connected)');
    result:=inherited;
    exit;
  end;
  result:=inherited;
  hasBeenUpdated:=false;
  performAction(lib.template.connect);
  connected:=true;
  if logging then log('TTemplateAccountAccess.connect ended');
end;

procedure TTemplateAccountAccess.disconnect();
begin
  //inherited disconnect();
  //hasBeenUpdated:=false;
end;

function TTemplateAccountAccess.needSingleBookCheck(): boolean;
begin
  Result:=length(lib.template.updateSingle.pages)>0;
end;

procedure TTemplateAccountAccess.parseBooks(extendAlways: boolean);
var i:longint;
    temp: array of PBook;
begin
  if logging then log('TTemplateAccountAccess.parseBooks started');
  inherited;

  updateAll;


  if needSingleBookCheck() then begin
    extendAllBooks:=extendAlways or shouldExtendAll;
  end else
    if extendAlways or shouldExtendAll then
      extendAll
     else if extendType = etSingleDepends then begin
       setlength(temp,0);
       for i:=0 to books.bookLists[bltInCurrentDataUpdate].Count-1 do
         if (PBook(books.bookLists[bltInCurrentDataUpdate][i])^.limitDate <= currentDate+extendDays) and
           not (PBook(books.bookLists[bltInCurrentDataUpdate][i])^.status in [bsEarMarked,bsMaxLimitReached])then begin
           setlength(temp,length(temp)+1);
           temp[high(temp)]:=PBook(books.bookLists[bltInCurrentDataUpdate][i]);
         end;
       if length(temp)<>0 then
         extendList(temp);
     end;

  books.compareBookList(not needSingleBookCheck());

    //openPages(lib.template.extendAllPages);
  if logging then log('TTemplateAccountAccess.parseBooks ended');
end;

procedure TTemplateAccountAccess.parseSingleBooks(
  book: TBookArray; extendAlways: boolean);
var i:longint;
begin
  if logging then
    log('enter TTemplateAccountAccess.parseSingleBooks');
  inherited;
  
  if needSingleBookCheck() then
    for i:=0 to high(book) do
      updateSingle(book[i]);

  if extendAlways or shouldExtendAll then
    extendList(book)
   else begin
     for i:=high(book) downto 0 do
       if not shouldExtendSingle(book[i]^) then begin
         book[i]:=book[high(book)];
         SetLength(book,length(book)-1);
       end;
     extendList(book);
   end;

  if books.bookLists[bltInCurrentDataUpdate].Count>0 then
    books.compareBookList(needSingleBookCheck());

{
    if extendAlways or shouldExtendAll then
      extendList(book);
  end else begin
    if extendAlways or shouldExtendAll then
      extendList(book);
  end;}
  if logging then
    log('leave TTemplateAccountAccess.parseSingleBooks');
end;

end.


