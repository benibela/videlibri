unit libraryParser;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, extendedhtmlparser, simplexmlparser, inifiles,internetaccess,dRegExpr,booklistreader;


type
  TCustomAccountAccess=class;
  TCustomAccountAccessClass=class of TCustomAccountAccess;

  { TLibrary }

  TLibrary=class
  protected
    defaultVariables:TStringList;
    function readProperty(tagName: string; properties: TProperties):boolean;
  public
    template:TBookListTemplate;

    canModifySingleBooks:boolean;
    //function getPrettyNameShort():string;virtual;
    homepage:string;
    prettyNameLong:string;
    prettyNameShort:string;
    id:string;
    maxExtendCount: integer; //-1: if you can extend so frequently you want
    bestHomepageWidth,bestHomepageHeight: integer;
    //allowHomepageNavigation: boolean;

    usernameRegEx,passwordRegEx: TRegExpr;

    procedure loadFromFile(fileName:string);
    function getAccountObject():TCustomAccountAccess;virtual;
    constructor create;
    destructor destroy;override;
    
    property variables: TStringList read defaultVariables;
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
    
    function getTemplate(templateName: string):TBookListTemplate;
    function getAccount(libID,accountID: string):TCustomAccountAccess;overload;
    function getAccount(mixID: string):TCustomAccountAccess;overload;

    function enumeratePrettyLongNames: string;
    function enumeratePrettyShortNames: string;
    function getLibraryFromEnumeration(const pos:integer):TLibrary;
    function getLibraryCountInEnumeration:integer;
    
    procedure enumerateVariableValues(const varName: string; result: TStringList);
    procedure enumerateLibrariesWithValue(const varName, value: string; result: TList);
  end;

  TVariables=array of record
    name,value:string;
  end;
  TBookListType=(bltInOldData,bltInOldAdd,bltInCurrentFile,bltInCurrentDataUpdate);

  TBookOutputType=(botAll,botCurrent,botOld,botCurrentUpdate);

  { TBookList }

  { TBookLists }

  TBookLists=class
  private
    ownerLib:TCustomAccountAccess;
    bookLists:array[TBookListType] of TBookList;
    bookListOldFileName,bookListCurFileName: string;
    bookListOldLoaded: boolean;
    keepHistory: boolean;

    function getBooksCurrentFile: TBookList;inline;
    function getBooksCurrentUpdate: TBookList;inline;
    function getBooksOld: TBookList;
    procedure remove;
    procedure save;

    procedure updateSharedDates();
    procedure needOldBookList;
  public
    nextLimit,nextNotExtendableLimit:longint;
    //Call always from the MainThread
    constructor create(const AOwnerLib:TCustomAccountAccess; const oldFile,curFile: string);
    destructor destroy;override;
    
    //immer: übernimmt alte Daten aus currentFile (vor allem firstExistsDate)
    //finalchange = true:  ersetzt currentFile durch currentUpdate und fügt
    //                     die entfernten Bücher in bltInOldData ein
    //finalchange = false:
    procedure merge(finalChange: boolean);
    
    property currentUpdate: TBookList read getBooksCurrentUpdate;
    property current: TBookList read getBooksCurrentFile;
    property old: TBookList read getBooksOld;
  end;

  ELibraryException=class(Exception)
    details:string;
    constructor create;
    constructor create(s:string;more_details:string='');
  end;

  { ENeverEver }

  ENeverEverLibraryException=class(Exception)
    constructor create(mes: string);
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
  private
    FEnabled: boolean;
  protected
    fbooks: TBookLists;
    lib: TLibrary;
    internet: TInternetAccess;
    config: TIniFile;
    path,user,pass,FPrettyName:string;
    FExtendType: TExtendType;
    FExtendDays,FLastCheckDate:integer;

    FKeepHistory: boolean;
    FCharges:Currency;
    function getCharges:currency;virtual;
  public
//    nextLimit,nextNotExtendableLimit:longint;
    connected,isThreadRunning: boolean; //set to true before the thread is called
                              //set to false after the last change is done
                              //read whenever you want
    constructor create(alib: TLibrary);virtual;
    destructor destroy;override;
    
    
    procedure init(apath,userID:string);virtual;
    procedure save();
    procedure remove(); //DELETE the account

    //==============Access functions================
    //At first connect must be called
    function connect(AInternet:TInternetAccess=nil):boolean; virtual;abstract;
    //After disconnect you mustn't call any one except connect
    procedure disconnect(); virtual;
    
    procedure updateAll();   virtual;
    procedure updateSingle(book: TBook);virtual;
    procedure updateAllSingly; virtual;
    procedure extendAll(); virtual;
    procedure extendList(bookList:TBookList); virtual;

    function shouldExtendBook(book: TBook):boolean;
    function existsCertainBookToExtend: boolean;
    function needSingleBookCheck():boolean;virtual;

    function getLibrary():TLibrary;

    procedure changeUser(const s:string); virtual;
    function getUser(): string;virtual;
    function getID():string; virtual;

    property charges: currency read getCharges;        //<0 means unknown

    property books: TBookLists read fbooks;
    property prettyName: string read FPrettyName write FPrettyName;
    property passWord: string read pass write pass;
    property lastCheckDate: integer read FLastCheckDate;
    property extendDays: integer read FExtendDays write FExtendDays;
    property extendType: TExtendType read FExtendType write FExtendType;
    property keepHistory: boolean read FKeepHistory write FKeepHistory;
    property enabled: boolean read FEnabled write FEnabled;
  end;


  { TTemplateAccountAccess }

  TTemplateAccountAccess = class(TCustomAccountAccess)
  protected
    lastTodayUpdate: longint; //if connected, time of update (ms, Gettickcount)
    extendAllBooks:boolean;

    reader:TBookListReader;

    procedure parserVariableRead(variable: string; value: String);

    procedure setVariables();

    //procedure selectBook(variable,value: string); not needed

  public
    constructor create(alib: TLibrary);override;
    procedure init(apath,userID:string);override;
    destructor destroy;override;
    //==============Access functions================
    function connect(AInternet:TInternetAccess=nil):boolean; override;
    procedure disconnect(); override;

    function needSingleBookCheck():boolean;override;

    procedure updateAll;override;
    procedure updateSingle(book: TBook);override;
    procedure extendAll;override;
    procedure extendList(booksToExtend: TBookList);override;


    //function needSingleBookCheck():boolean;virtual;
  end;

implementation
uses applicationconfig,bbdebugtools,bbutils,FileUtil,LCLIntf;
function currencyStrToCurrency(s:string):Currency;
begin
  s:=trim(s);
  if s='' then exit(0);
  if '.'<>DecimalSeparator then
    s:=StringReplace(s,'.',DecimalSeparator,[]);
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
  end else if tagName='username' then usernameRegEx.Expression:=getProperty('matches',properties)
  else if tagName='password' then passwordRegEx.Expression:=getProperty('matches',properties)
  else if tagName='maxextendcount' then maxExtendCount:=StrToInt(value);
  Result:=true;
end;

procedure TLibrary.loadFromFile(fileName: string);
begin
  id:=ChangeFileExt(ExtractFileName(fileName),'');;
  maxExtendCount:=-1;
  parseXML(strLoadFromFile(fileName),@readProperty,nil,nil,eUTF8);
  if template<>nil then begin
    canModifySingleBooks:=(template.findAction('extend-single')<>nil)  or
                          (template.findAction('extend-list')<>nil) ;
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

constructor TLibrary.create;
begin
  usernameRegEx:=TRegExpr.Create('.');
  passwordRegEx:=TRegExpr.Create('.');
  bestHomepageWidth:=0;
  bestHomepageHeight:=0;
end;

destructor TLibrary.destroy;
begin
  usernameRegEx.Free;
  passwordRegEx.Free;
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
    TBookListTemplate(templates.Objects[i]).free;
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
  libraryPath:=dataPath+'libraries'+DirectorySeparator;

  libraryFiles:=TStringList.Create;
  libraryFiles.LoadFromFile(libraryPath+'libraries.list');
  for i:=0 to libraryFiles.count-1 do begin
    newLib:=TLibrary.Create;
    newLib.loadFromFile(libraryPath+libraryFiles[i]);
    libraries.Add(newLib);
  end;
  libraryFiles.free;
end;

function TLibraryManager.getTemplate(templateName: string): TBookListTemplate;
var i:longint;
begin
  i:=templates.IndexOf(templateName);
  if i>=0 then Result:=TBookListTemplate(templates.Objects[i])
  else begin
    Result:=TBookListTemplate.Create(libraryPath+'templates'+DirectorySeparator+templateName+DirectorySeparator,templateName);
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

procedure TLibraryManager.enumerateVariableValues(const varName: string;
  result: TStringList);
var i:longint;
begin
  result.clear;
  for i:=0 to libraries.count-1 do
    if  result.IndexOf(TLibrary(libraries[i]).defaultVariables.Values[varName])<0 then
      result.add(TLibrary(libraries[i]).defaultVariables.Values[varName]);
end;

procedure TLibraryManager.enumerateLibrariesWithValue(const varName,
  value: string; result: TList);
var i:longint;
begin
  result.clear;
  for i:=0 to libraries.count-1 do
    if TLibrary(libraries[i]).defaultVariables.Values[varName]=value then
      result.add(TLibrary(libraries[i]));
end;


procedure TBookLists.updateSharedDates();
var i:integer;
begin
  if logging then
    log('TBookLists.updateSharedDates started');
  nextLimit:=MaxInt-1;
  nextNotExtendableLimit:=MaxInt;
  for i:=0 to bookLists[bltInCurrentFile].count-1 do
    if bookLists[bltInCurrentFile][i].limitDate>0 then begin
      if bookLists[bltInCurrentFile][i].limitDate<nextLimit then
        nextLimit:=bookLists[bltInCurrentFile][i].limitDate;
      if bookLists[bltInCurrentFile][i].status in BOOK_NOT_EXTENDABLE then
        if bookLists[bltInCurrentFile][i].limitDate<nextNotExtendableLimit then
          nextNotExtendableLimit:=bookLists[bltInCurrentFile][i].limitDate;
    end;

  if logging then
    log('TBookLists.updateSharedDates ended')
end;


procedure TBookLists.needOldBookList;
var temp:TBookList;
begin
  if not keepHistory then exit;
  if not bookListOldLoaded then begin
    if bookLists[bltInOldData].count>0 then begin
      temp:=TBookList.create;
      temp.Assign(bookLists[bltInOldData]);
      bookLists[bltInOldData].clear;
      bookLists[bltInOldData].load(bookListOldFileName);
      bookLists[bltInOldData].addList(temp);
      temp.free;
    end else
      bookLists[bltInOldData].load(bookListOldFileName);
    bookListOldLoaded:=true;
  end;
end;

procedure TBookLists.remove;
var sl:TStringList;
    i:longint;
begin
  DeleteFile(bookListCurFileName);
  DeleteFile(bookListOldFileName);
  sl:=FindAllFiles(ExtractFilePath(bookListOldFileName),ExtractFileNameOnly(bookListOldFileName)+'.????????',false);
  for i:=0 to sl.count-1 do
    DeleteFile(sl[i]);
  sl.free;
end;

function TBookLists.getBooksCurrentFile: TBookList;
begin
  Result:=bookLists[bltInCurrentFile];
end;

function TBookLists.getBooksCurrentUpdate: TBookList;
begin
  Result:=bookLists[bltInCurrentDataUpdate];
end;

function TBookLists.getBooksOld: TBookList;
begin
  needOldBookList;
  Result:=bookLists[bltInOldData];
end;

procedure  TBookLists.save;
begin
  if logging then
    log('TBookLists.save started');
  if keepHistory then begin
    if (currentDate-ownerLib.config.ReadInteger('base','last-history-backup',0)>HistoryBackupInterval) and (FileExistsUTF8(bookListOldFileName)) then begin
      CopyFile(bookListOldFileName,bookListOldFileName+'.'+FormatDateTime('yyyymmdd',currentDate));
      ownerLib.config.WriteInteger('base','last-history-backup',currentDate);
    end;
    if bookLists[bltInOldData].Count>0 then
      if bookListOldLoaded then bookLists[bltInOldData].save(bookListOldFileName,saReplace)
      else begin
        bookLists[bltInOldData].save(bookListOldFileName,saAdd);
        bookLists[bltInOldData].clear;
      end;
  end;
  bookLists[bltInCurrentFile].save(bookListCurFileName,saReplace);
  if logging then
    log('TBookLists.save ended')
end;

constructor TBookLists.create(const AOwnerLib:TCustomAccountAccess; const oldFile,curFile: string);
var i:TBookListType;
begin
  if logging then
    log('TBookLists.create started');
  ownerLib:=AOwnerLib;
  bookListCurFileName:=curFile;
  bookListOldFileName:=oldFile;
  bookListOldLoaded:=false;
  for i:=low(bookLists) to high(bookLists) do
    bookLists[i]:=TBookList.create(ownerLib);
  bookLists[bltInCurrentDataUpdate].lendList:=true;
  bookLists[bltInCurrentFile].lendList:=true;
  bookLists[bltInCurrentFile].load(curFile);
  updateSharedDates();
  if nextLimit<applicationconfig.nextLimit then
    applicationconfig.nextLimit:=nextLimit;
  if nextNotExtendableLimit<applicationconfig.nextNotExtendableLimit then
    applicationconfig.nextNotExtendableLimit:=nextNotExtendableLimit;
  if logging then
    log('TBookLists.create ended')
end;
destructor TBookLists.destroy;
var i:TBookListType;
begin
  for i:=low(bookLists) to high(bookLists) do
    bookLists[i].free;
  inherited;
end;


procedure TBookLists.merge(finalChange: boolean);
begin
  //Current book list REPLACE
  if logging then
    if finalChange then log('TBookLists.merge(true) started')
    else log('TBookLists.merge(false) started');

  bookLists[bltInCurrentDataUpdate].mergeMissingInformation(bookLists[bltInCurrentFile]);
  if finalChange then begin
    if keepHistory then begin
      bookLists[bltInOldAdd].Assign(bookLists[bltInCurrentFile]);
      bookLists[bltInOldAdd].removeAllFrom(bookLists[bltInCurrentDataUpdate]);
      bookLists[bltInOldAdd].lendList:=false;
      bookLists[bltInOldData].AddList(bookLists[bltInOldAdd]);
      bookLists[bltInOldAdd].clear;
    end;
    
    bookLists[bltInCurrentFile].Assign(bookLists[bltInCurrentDataUpdate]);
    bookLists[bltInCurrentDataUpdate].clear;
    
    updateSharedDates();
    ownerLib.flastCheckDate:=currentDate;
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

function TCustomAccountAccess.getCharges: currency;
begin
  result:=fcharges;
end;

constructor TCustomAccountAccess.create(alib:TLibrary);
begin
  connected:=false;
  fbooks:=nil;
  currentDate:=longint(trunc(date));
  config:=nil;
  isThreadRunning:=false;
  lib:=alib;
  fcharges:=-1;
  FEnabled:=true;
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
  
  //Datenladen/cachen
  fbooks:=TBookLists.create(self,path+getID()+'.history',path+getID()+'.current');
  config:=TIniFile.Create(path+getID()+'.config');
  pass:=config.ReadString('base','pass','');
  books.keepHistory:=config.ReadBool('base','keep-history',true);
  flastCheckDate:=config.ReadInteger('base','lastCheck',0);
  keepHistory:=config.readBool('base','keep-history',true);
  prettyName:=config.readString('base','prettyName',userID);
  extendDays:=config.readInteger('base','extend-days',7);
  extendType:=TExtendType(config.readInteger('base','extend',0));
  fcharges:=currency(config.readInteger('base','charge',-100))/100;;
  FEnabled:=config.ReadBool('base','enabled',true);
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
    config.WriteBool('base','enabled',FEnabled);

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

procedure TCustomAccountAccess.disconnect();
begin
  connected:=false;
end;

procedure TCustomAccountAccess.updateAll();
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

procedure TCustomAccountAccess.updateSingle(book: TBook);
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

procedure TCustomAccountAccess.extendAll();
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

procedure TCustomAccountAccess.extendList(bookList: TBookList);
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

function TCustomAccountAccess.shouldExtendBook(book: TBook): boolean;
begin
  Result:=(book.status in BOOK_EXTENDABLE) and
          (book.limitDate<=currentDate+extendDays);
end;

function TCustomAccountAccess.existsCertainBookToExtend: boolean;
var i:longint;
begin
  result:=false;
  case extendType of
    etAlways, etNever: exit(false);
    etAllDepends, etSingleDepends:
      for i:=0 to books.bookLists[bltInCurrentFile].Count-1 do
        if shouldExtendBook(books.bookLists[bltInCurrentFile][i]) then
          exit(true);
  end;
end;

procedure TCustomAccountAccess.updateAllSingly;
var i:integer;
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
  for i:=0 to books.bookLists[bltInCurrentDataUpdate].count-1 do
    updateSingle(books.bookLists[bltInCurrentDataUpdate][i]);
  books.updateSharedDates();
end;

function TCustomAccountAccess.needSingleBookCheck():boolean;
begin
  result:=false;
end;

function TCustomAccountAccess.getLibrary():TLibrary;
begin
  result:=lib;
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
  fbooks:=TBookLists.create(self,path+getID()+'.history',path+getID()+'.current');
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


{ TTemplateAccountAccess }

procedure TTemplateAccountAccess.parserVariableRead(variable: string;  value: String);
begin
  if logging then
    log('** Read variable: "'+variable+'" = "'+value+'"');
  if variable='charge' then begin
    FCharges:=currencyStrToCurrency(value);
  end else if variable='raise-login()' then begin
    raise ELoginException.create(value);
  end //Büchereigenschaften
{  else raise ELibraryException.create('Fehler im Büchereistrukturtemplate, entweder wurde VideLibri beschädigt, oder es ist fehlerhaft programmiert.'#13#10'Lösungsmöglichkeiten: Neuinstallation oder Update',
    'Der Fehler ist bei der Bücherei '+lib.prettyNameShort+' im Template '+lib.template.path+' aufgetreten. '#13#10+
    'Unbekannte Variable '+variable+' mit Wert '+value);}
end;

procedure TTemplateAccountAccess.setVariables();
var i:longint;
begin
  for i:=0 to lib.defaultVariables.count-1 do
    reader.parser.variables.Values[lib.defaultVariables.Names[i]]:=lib.defaultVariables.ValueFromIndex[i];
  reader.parser.variables.Values['username']:=user;
  reader.parser.variables.Values['password']:=passWord;
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
  setVariables();
  reader.performAction('update-all');
  lastTodayUpdate:=GetTickCount;
  if logging then log('Leave TTemplateAccountAccess.updateAll');
end;

procedure TTemplateAccountAccess.updateSingle(book: TBook);
begin
  if logging then
    log('enter TTemplateAccountAccess.updateSingle');
  setVariables();
  reader.selectBook(book);
  reader.performAction('update-single');
  if logging then
    log('leave TTemplateAccountAccess.updateSingle');

end;

procedure TTemplateAccountAccess.extendAll;
begin
  if logging then
    log('enter TTemplateAccountAccess.extendAll');
  if reader.findAction('extend-all')<>nil then begin
    setVariables();
    reader.performAction('extend-all');
  end else extendList(reader.books);
  if logging then
    log('leave TTemplateAccountAccess.extendAll');
end;

procedure TTemplateAccountAccess.extendList(booksToExtend: TBookList);
var bookListStr:string;
    i:longint;
    extendAction: PTemplateAction;
    book:TBook;
begin
  if booksToExtend.Count=0 then exit;
  if logging then log('Enter TTemplateAccountAccess.extendList');
  setVariables();
  extendAction:=reader.findAction('extend-list');
  if extendAction<>nil then begin
    if logging then log('use extendList Template');
    bookListStr:='';
    for i:=0 to booksToExtend.Count-1 do begin
      reader.selectBook(booksToExtend[i]);
      bookListStr+= reader.parser.replaceVars(extendAction^.singleBookStr);
    end;
    if logging then log('bookList (count: '+inttostr(booksToExtend.count)+') is: '+bookListStr);
    reader.parser.variables.Values['book-list']:=bookListStr;
    reader.performAction(extendAction^);
  end else if reader.findAction('extend-single')<>nil then begin
    if logging then log('use extendSingle Template');
    extendAction:=reader.findAction('extend-single');
    for i:=0 to booksToExtend.count-1 do begin
      reader.selectBook(booksToExtend[i]);
      book:=reader.books.findBook(booksToExtend[i]);
      if book<>nil then reader.selectBook(book); //handles more instances of the same book
      reader.performAction(extendAction^);

    end;
  end else if reader.findAction('extend-all')<>nil then begin
    if logging then log('use extendAll Template');
    reader.performAction('extend-all');
  end;
  if logging then log('Leave TTemplateAccountAccess.extendList');
end;



constructor TTemplateAccountAccess.create(alib: TLibrary);
begin
  inherited;
  assert(alib.template<>nil,'Keine Templatelibrary-Class');
  lib:=alib;
  reader:=TBookListReader.create(alib.template);
  //parser.onVariableRead:=@parserVariableRead;
  lastTodayUpdate:=0;
  
end;

procedure TTemplateAccountAccess.init(apath, userID: string);
begin
  inherited init(apath, userID);
  reader.books:=books.bookLists[bltInCurrentDataUpdate];
end;

destructor TTemplateAccountAccess.destroy;
begin
  reader.free;
  inherited destroy;
end;

function TTemplateAccountAccess.connect(AInternet: TInternetAccess): boolean;
begin
  if logging then log('TTemplateAccountAccess.connect started');
  result:=false;
  internet:=ainternet;
  assert(internet <> nil);
  reader.internet:=internet;
  if connected then begin
    if logging then log('TTemplateAccountAccess.connect ended (already connected)');
    exit;
  end;
  lastTodayUpdate:=0;
  setVariables();
  reader.performAction('connect');
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
  Result:=reader.findAction('update-single')<>nil;
end;


(*procedure TTemplateAccountAccess.updateAndExtend(booksToExtend: TBookList);
  function shouldExtendBook(book: TBook):boolean;
  begin
    Result:=(reader.books[i].status in BOOK_EXTENDABLE) and
             ((booksToExtend.findBook(book)<>nil) or
              (book.limitDate<=currentDate+extendDays));
  end;
var booksToExtendCount,booksExtendableCount: longint;
    realBooksToExtend: TBookList;
begin

  updateAll;
  if reader.findAction('update-single')<>nil then
    for i:=0 to reader.books.Count-1 do
      updateSingle(reader.books[i]);


  booksExtendableCount:=0;
  for i:=0 to reader.books.Count-1 do
    if reader.books[i].status in BOOK_EXTENDABLE then
      booksExtendableCount+=1;

  case extendType of
    etNever: exit;
    etAlways: //extend always (when there are books which can be extended)
      booksToExtendCount:=booksExtendableCount;
    etAllDepends,etSingleDepends: begin
      for i:=0 to reader.books.count-1 do //check for books to extend
        if shouldExtendBook(reader.books[i]) then
          booksToExtend+=1;
      if (extendType=etAllDepends) and (booksToExtend>0) then
        booksToExtend:=booksExtendableCount;
    end;
  end;

  if booksToExtend=booksExtendableCount then
    extendAll
   else begin
    realBooksToExtend:=TBookList.Create;
    for i:=0 to reader.books.count-1 do
      if shouldExtendBook(reader.books[i]) then
        realBooksToExtend.add(reader.books[i]);
    extendList(realBooksToExtend);
    realBooksToExtend.free
   end;
end;

procedure TTemplateAccountAccess.fastExtend(booksToExtend: TBookList);
begin
  if GetTickCount-lastUpdate>10*60*1000 then updateAndExtend(booksToExtend)
  else begin
  end;
end;                                             *)


{ ENeverEver }

constructor ENeverEverLibraryException.create(mes: string);
begin
  inherited create(mes+' (dieser Fehler dürfte niemals auftreten)');
end;

end.


--schnelles VERLÄNGERN--
assume connected
if exists books-to-extend then begin
  if exists #extend-list then extend-list
  else if exists #extend-single then for all: #extend-single
  else extend-all
end
