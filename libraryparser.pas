unit libraryParser;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, simplehtmlparser, extendedhtmlparser, simplexmlparser, inifilessafe,internetaccess,regexpr,booklistreader,multipagetemplate,bbutils,simplehtmltreeparser,vlmaps;


type
  TCustomAccountAccess=class;
  TCustomAccountAccessClass=class of TCustomAccountAccess;

  { TLibrary }

  TTestingInfo = (tiUnknown, tiYes, tiNo, tiBroken);
  TLibrary=class
  protected
    ftestingSearch, ftestingAccount: TTestingInfo;
    defaultVariables:TStringList;
    fhomepageCatalogue: string;
    function readProperty(tagName: string; properties: TProperties):TParsingResult;
  public
    template:TMultiPageTemplate;


    canModifySingleBooks:boolean;
    segregatedAccounts: boolean;
    //function getPrettyNameShort():string;virtual;
    homepageBase:string;
    prettyNameLong:string;
    prettyNameShort:string;
    tableComment: string;
    id:string;
    deprecatedId: string;
    maxRenewCount: integer;

    usernameRegEx,passwordRegEx: TRegExpr;

    procedure loadFromFile(fileName:string);
    procedure loadFromString(data, fileName:string); //fileName is used for id
    function getAccountObject():TCustomAccountAccess;virtual;
    constructor create;
    destructor destroy;override;
    
    property variables: TStringList read defaultVariables;
    function homepageCatalogue: string;
    function location: string;
    function state: string;
    function country: string;
    function prettyLocation: string;
    function prettyState: string;
    function prettyCountry: string;
    function prettyCountryState: string;
    function prettyNameLongWithComment: string; inline;
    class function pretty(const l: string): string;
    class function unpretty(const l: string): string;
  end;

  TLibraryManager = class;

  TLibraryMetaDataEnumerator = object
  private
    i: integer;
    manager: TLibraryManager;
  public
    prettyCountryState, prettyLocation: string;
    newCountryState, newLocation: boolean;
    libraryId: string;
    function MoveNext: boolean;
  end;

  TLibraryIdStringList = class(TMapStringOwningObject)
    function DoCompareText(const s1, s2: string): PtrInt; override;
  end;

  TLibraryManager=class
  private
    function getCount: integer;
    function getLibraryFromIndex(i: integer): TLibrary;
    function getLibraryIdFromIndex(i: integer): string;
  protected
    basePath: string;
    
    flibraries: TList;
    flibraryIds: TLibraryIdStringList;
    function getAccountObject(libID: string):TCustomAccountAccess;
  public
    templates:TStringList;

    constructor create();
    destructor destroy();override;
    procedure init(apath:string);
    
    function getTemplate(templateName: string):TMultiPageTemplate;
    function getCombinedTemplate(template: TMultiPageTemplate; templateName: string):TMultiPageTemplate;
    function getAccount(libID,accountID: string):TCustomAccountAccess;overload; //+-encoded library name, verbatim user number
    function getAccount(mixID: string):TCustomAccountAccess;overload; //mixId: +-Encoded library name # +2-encoded user number

    function enumerateLibraryMetaData: TLibraryMetaDataEnumerator;

    function get(id: string): TLibrary;

    function getUserLibraries(): TList;
    function setUserLibrary(trueid, data: string): TLibrary;
    function downloadAndInstallUserLibrary(url: string): TLibrary;
    procedure deleteUserLibrary(trueid: string);
    procedure reloadTemplate(templateId: string);
    procedure reloadPendingTemplates(); //only call this synchronized thrugh updateThreadConfig.threadManagementSection

    property libraries[i: integer]: TLibrary read getLibraryFromIndex; default;
    property libraryIds[i: integer]: string read getLibraryIdFromIndex;
    property count: integer read getCount;
  private
    pendingReplacementsOld, pendingReplacementsNew: array of TMultiPageTemplate;
    procedure replaceTemplate(old, new: TMultiPageTemplate);

  end;

  TVariables=array of record
    name,value:string;
  end;
  TBookListType=(bltInOldData,bltInCurrentFile,bltInCurrentDataUpdate);

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

    procedure mergePersistentToCurrentUpdate;
    procedure completeUpdate(); //replaces currentFile with currentupdate and moves old books to old
    
    property currentUpdate: TBookList read getBooksCurrentUpdate;
    property current: TBookList read getBooksCurrentFile;
    property old: TBookList read getBooksOld;
  end;

  EVidelibriException = class(Exception) end;
  ELibraryException=class(EVidelibriException)
    details:string;
    constructor create;
    constructor create(s:string;more_details:string='');
  end;
  EImportException = class(EVidelibriException) end;

  TExtendType=(etAlways,etAllDepends,etSingleDepends,etNever);

  { TCustomAccountAccess }
                //first start info
  TCustomAccountAccess=class(TCustomBookOwner)
  private
    FEnabled: boolean;
    FTimeout: qword;
    function GetConnected: boolean;
    function GetUpdated: boolean;
    procedure SetAccountType(AValue: integer);
    procedure setPassword(AValue: string);
  protected
    fbooks: TBookLists;
    lib: TLibrary;
    internet: TInternetAccess;
    config: TSafeIniFile;
    path,user,pass:string;
    FExtendType: TExtendType;
    FExtendDays,FLastCheckDate:integer;
    FAccountType: integer;

    FKeepHistory, FConnected, FUpdated: boolean;
    FConnectingTime, FUpdateTime: qword;
    FCharges:Currency;
    procedure updateConnectingTimeout;
    function getCharges:currency;virtual;
    function GetNeedSingleBookUpdate: boolean; virtual;
    procedure initFromConfig;
  public
//    nextLimit,nextNotExtendableLimit:longint;
    thread: TThread;
    broken: longint;          //Iff equal currentDate, disable auto checking (only accessed by thread/thread-creator)
    constructor create(alib: TLibrary);virtual;
    destructor destroy;override;
    
    
    procedure init(apath,userID:string);virtual;
    procedure save();
    procedure remove(); //DELETE the account

    //==============Access functions================
    //At first connect must be called
    procedure connect(AInternet:TInternetAccess); virtual;abstract;
    procedure disconnect(); virtual;
    procedure resetConnection;
    
    procedure updateAll();   virtual;
    procedure updateSingle(book: TBook);virtual;
    procedure updateAllSingly; virtual;
    procedure extendAll(); virtual;
    procedure extendList(bookList:TBookList); virtual;

    //procedure orderSingle(book: TBook); virtual;
    //procedure orderList(booklist: TBookList); virtual;
    procedure cancelSingle(book: TBook); virtual;
    procedure cancelList(booklist: TBookList); virtual;

    function shouldExtendBook(book: TBook):boolean;
    function existsCertainBookToExtend: boolean;
    function needChecking: boolean;

    function getLibrary():TLibrary;

    procedure changeUser(const s:string); virtual;
    function getUser(): string;virtual;
    function getPlusEncodedID():string; virtual;

    property charges: currency read getCharges;        //<0 means unknown

    property books: TBookLists read fbooks;
    property passWord: string read pass write setPassword;
    property lastCheckDate: integer read FLastCheckDate;
    property extendDays: integer read FExtendDays write FExtendDays;
    property extendType: TExtendType read FExtendType write FExtendType;
    property keepHistory: boolean read FKeepHistory write FKeepHistory;
    property enabled: boolean read FEnabled write FEnabled;
    property connected: boolean read GetConnected;
    property updated: boolean read GetUpdated;
    property timeout: qword read FTimeout write FTimeout;
    property accountType: integer read FAccountType write SetAccountType;
    property needSingleBookUpdate: boolean read GetNeedSingleBookUpdate;
  end;


  { TTemplateAccountAccess }

  TTemplateAccountAccess = class(TCustomAccountAccess)
  protected
    reader:TBookListReader;
    function GetNeedSingleBookUpdate: boolean; override;
    procedure parserVariableRead(variable: string; value: String);


    //procedure selectBook(variable,value: string); not needed
    procedure setVariables();


  public
    procedure setVariables(parser: THtmlTemplateParser);


    constructor create(alib: TLibrary);override;
    procedure resetlib();
    procedure init(apath,userID:string);override;
    destructor destroy;override;


    procedure changeUser(const s:string); override;

    //==============Access functions================
    procedure connect(AInternet:TInternetAccess); override;
    procedure disconnect(); override;

    procedure updateAll;override;
    procedure updateSingle(book: TBook);override;
    procedure updateAllSingly; override;
    procedure extendAll;override;
    procedure extendList(booksToExtend: TBookList);override;

   // procedure orderList(booklist: TBookList); override;
    procedure cancelList(booklist: TBookList); override;
  end;

  TExportImportFlag = (eifCurrent, eifHistory, eifConfig, eifPassword);
  TExportImportFlags = set of TExportImportFlag;
  TExportImportFlagsArray = array of TExportImportFlags;

  procedure exportAccounts(const fn: string; accounts: array of TCustomAccountAccess; flags: array of TExportImportFlags );
  procedure importAccountsPrepare(const fn: string; out parser: TTreeParser; out accounts: TStringArray; out flags: TExportImportFlagsArray);
  //frees the parser
  procedure importAccounts(parser: TTreeParser; impAccounts: TStringArray; flags: TExportImportFlagsArray);

resourcestring
  rsCustomLibrary = 'selbst definierte';
  rsAllLibraries = 'alle';


implementation
uses applicationconfig,bbdebugtools,FileUtil,LCLIntf,LazFileUtils, xquery,androidutils,simpleinternet,mockinternetaccess,libraryAccess,strutils,math;

resourcestring
  rsNoTemplateLinkFound = 'Der Link verweist auf kein VideLibri-Template (es gibt kein Element < link rel="videlibri.description" auf der Seite)';


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

function decodeSafeFileName(s: string): string;
var
  i: Integer;
begin
  if pos('+', s) = 0 then exit(s);
  result := '';
  i := 1;
  while i <= length(s) do begin
    if   (s[i] = '+') and (i + 2 <= length(s)) then begin
      result += chr(strtoint('$'+copy(s,i+1,2)));
      i += 2;
    end else result += s[i];
    i += 1;
  end;
end;

function encodeToSafeFileName(s: string): string;
const OK_SET = ['a'..'z','A'..'Z','_','-','0'..'9'];
var
  i: Integer;
  ok: Boolean;
begin
  ok := true;
  for i := 1 to length(s) do
    if not (s[i] in OK_SET) then begin
      ok := false;
      break;
    end;
  if ok then exit(s);
  result := '';
  for i := 1 to length(s) do
    if s[i] in OK_SET then result += s[i]
    else result += '+' + IntToHex(ord(s[i]), 2);
end;

procedure exportAccounts(const fn: string; accounts: array of TCustomAccountAccess; flags: array of TExportImportFlags);
var
  f: TFileStream;
  tempsl: TStringList;
  i: Integer;
  j: Integer;
  procedure wln(const s: string);
  var
    le: String;
  begin
    if s = '' then exit;
    f.WriteBuffer(s[1], length(s));
    le := LineEnding;
    f.WriteBuffer(le[1], length(le));
  end;
  procedure dumpBooks(const bookFile, mode: string);
  var
    books: RawByteString;
    startpos: LongInt;
    endpos: LongInt;
  begin
    EnterCriticalSection(updateThreadConfig.libraryFileAccess);
    try
      if FileExists(bookFile) then
        books := strLoadFromFileUTF8(bookFile);
      startpos := strIndexOf(books, '<books');
      if startpos > 0 then begin
        startpos := strIndexOf(books, '>', startpos) + 1;
        endpos := strRpos('<', books); //exclusive
        if endpos > startpos then begin
          wln('     <books mode="'+xmlStrEscape(mode)+'">');
          f.WriteBuffer(books[startpos], endpos - startpos);
          wln('     </books>');
        end;
      end;
    finally
      LeaveCriticalSection(updateThreadConfig.libraryFileAccess);
    end;
  end;

begin
  ForceDirectories(ExtractFileDir(fn));
  f:=TFileStream.Create(fn,fmOpenWrite or fmCreate);
  tempsl := TStringList.Create;
  tempsl.DelimitedText := '=';
  tempsl.StrictDelimiter := true;
  try
    wln('<?xml version="1.0" encoding="UTF-8"?>');
    wln('<videlibri-dump>');
    for i := 0 to high(accounts) do with accounts[i] do begin
      wln('  <account name="'+xmlStrEscape(prettyName, true)+'" '+IfThen(eifConfig in flags[i],'id="'+xmlStrEscape(accounts[i].getPlusEncodedID(), true)+'"','')+'>');
      if eifConfig in flags[i] then begin
        wln('    <config><base>');
        config.ReadSectionValues('base', tempsl);
        for j := 0 to tempsl.count - 1 do
          if (eifPassword in flags[i]) or (tempsl.Names[j] <> 'pass') then
            wln('       <v n="'+xmlStrEscape(tempsl.Names[j], true)+'">'+xmlStrEscape(tempsl.ValueFromIndex[j])+'</v>');
        wln('     </base></config>');
      end;
      if eifCurrent in flags[i] then dumpBooks(books.bookListCurFileName + '.xml', 'current');
      if eifHistory in flags[i] then dumpBooks(books.bookListOldFileName + '.xml', 'history');
      wln('  </account>');
    end;
    wln('</videlibri-dump>');
  finally
    f.Free;
  end;
end;

procedure importAccountsPrepare(const fn: string; out parser: TTreeParser; out accounts: TStringArray; out
  flags: TExportImportFlagsArray);
var
  xq: TXQueryEngine;
  xv: IXQValue;
  tempv: xquery.IXQValue;
  i: Integer;
  hasConfig: IXQuery;
  hasPass: IXQuery;
  hasCurrent: IXQuery;
  hasHistory: IXQuery;
  node: TTreeNode;
begin
  parser := TTreeParser.Create;
  try
    parser.parseTreeFromFile(fn);
    xq := TXQueryEngine.create;
    try
      tempv := xq.evaluateXPath3('/videlibri-dump/account', parser.getLastTree);
      hasConfig := xq.parseXPath3('./config/base');
      hasPass := xq.parseXPath3('./config/base/v[@n="pass"]');
      hasCurrent := xq.parseXPath3('./books[@mode="current"]');
      hasHistory := xq.parseXPath3('./books[@mode="history"]');
      SetLength(accounts, tempv.Count);
      SetLength(flags, tempv.Count);
      for i := 0 to high(accounts) do begin
        xv := tempv.get(i+1);
        node := xv.toNode;
        accounts[i] := node.getAttribute('name');
        flags[i] := [];
        if hasConfig.evaluate(node).toBooleanEffective then Include(flags[i], eifConfig);
        if hasCurrent.evaluate(node).toBooleanEffective then Include(flags[i], eifCurrent);
        if hasHistory.evaluate(node).toBooleanEffective then Include(flags[i], eifHistory);
        if hasPass.evaluate(node).toBooleanEffective then Include(flags[i], eifPassword);
      end;
    finally
      xq.free;
    end;
  except
    parser.free;
    parser := nil;
    raise;
  end;
end;

procedure importAccounts(parser: TTreeParser; impAccounts: TStringArray; flags: TExportImportFlagsArray);
var getHistory, getCurrent: IXQuery;
  procedure importBooklist(parent: TTreeNode; list: TBookList; current: boolean);
  var xbook: IXQValue;
    book: TBook;
    node: TTreeNode;
    fbook: TBook;
    data: xquery.IXQValue;
  begin
    if current then data := getCurrent.evaluate(parent)
    else data := getHistory.evaluate(parent);

    for xbook in data do begin
      book := TBook.create;
      node := xbook.toNode.getFirstChild();
      while node <> nil do begin
        if (node.typ = tetOpen) and (node.value = 'v') then
          book.setProperty(node.getAttribute('n'), node.deepNodeText());
        node := node.getNextSibling();
      end;
      fbook := list.findBook(book);
      if fbook = nil then list.add(book)
      else begin
        fbook.assignIfNewer(book);
        book.free;
      end;
    end;

  end;

var realAccounts: array of TCustomAccountAccess;
    accountNodes: array of TTreeNode;
  i, j: Integer;
  xq: TXQueryEngine;
  getAccoutNode: IXQuery;
  id: String;
  getConfig: IXQuery;
  config: TTreeNode;
  xv: IXQValue;
  name: String;
begin
  xq := TXQueryEngine.create;
  try
    SetLength(realAccounts, length(impAccounts));
    SetLength(accountNodes, length(impAccounts));
    xq.VariableChangelog.Clear; xq.VariableChangelog.add('name', '');
    getAccoutNode := xq.parseXPath3('/videlibri-dump/account[@name=$name]');
    getConfig := xq.parseXPath3('./config/base/v');
    getCurrent := xq.parseXPath3('./books[@mode="current"]/book');
    getHistory := xq.parseXPath3('./books[@mode="history"]/book');
    for i := 0 to high(impAccounts) do begin
      if flags[i] = [] then continue;
      xq.VariableChangelog.Clear; xq.VariableChangelog.add('name', impAccounts[i]);
      accountNodes[i] := getAccoutNode.evaluate(parser.getLastTree).toNode;
      if accountNodes[i] = nil then raise EImportException.Create('Could not find account: '+impAccounts[i]+' in import file');
      id := accountNodes[i].getAttribute('id');
      if id <> '' then
        for j := 0 to accounts.Count - 1 do
          if accounts[j].getPlusEncodedID() = id then begin
            realAccounts[i] := accounts[j];
            break;
          end;
      if realAccounts[i] = nil then
        for j := 0 to accounts.Count - 1 do
          if accounts[j].prettyName = impAccounts[i] then begin
            realAccounts[i] := accounts[j];
            break;
          end;


      if (realAccounts[i] <> nil) and (realAccounts[i].thread <> nil) then
        raise EImportException.Create('Cannot import account ' + impAccounts[i] + ': It is busy talking with the library server.');
    end;
    for i := 0 to high(impAccounts) do begin
      if flags[i] = [] then continue;
      if (realAccounts[i] = nil) then
        if (accountNodes[i].getAttribute('id') = '') then
          raise EImportException.Create('Cannot import account: '+impAccounts[i]+LineEnding+'Account is currently not registered, and import files contains no configuration data.')
         else if not (eifConfig in flags[i]) then
          raise EImportException.Create('Cannot import account: '+impAccounts[i]+LineEnding+'Account is currently not registered, and you disabled configuration import.')
    end;


    for i := 0 to high(impAccounts) do
      if (flags[i] <> []) and (realAccounts[i] = nil) then begin
        realAccounts[i] := libraryManager.getAccount((accountNodes[i].getAttribute('id')));
        realAccounts[i].prettyName := impAccounts[i];
        accounts.add(realAccounts[i]);
      end;

    for i := 0 to high(impAccounts) do begin
      if flags[i] = [] then continue;
      if eifConfig in flags[i] then begin
        for xv in getConfig.evaluate(accountNodes[i]) do begin
          config := xv.toNode;
          name := config.getAttribute('n');
          if (name <> 'pass') or (eifPassword in flags[i]) then
            realAccounts[i].config.WriteString('base', name, config.deepNodeText());
        end;
        realAccounts[i].config.UpdateFile;
        realAccounts[i].initFromConfig;
      end;

      if eifHistory in flags[i] then importBookList(accountNodes[i], realAccounts[i].books.old, false);
      if eifCurrent in flags[i] then begin
        with realAccounts[i].books do begin
          importBookList(accountNodes[i], currentUpdate, true);
          //remove current books that are in old. Thus preventing duplicates in old when current is updated
          //todo: better check the imported old than our old
          current.removeAllFrom(currentUpdate);
          for j := current.Count - 1 downto 0 do
            if old.findBook(current[j]) <> nil then
              current.delete(j);
          current.addList(currentUpdate);
          currentUpdate.clear;
        end;
      end;

      realAccounts[i].books.updateSharedDates();
      realAccounts[i].save();
    end;

    parser.free;
  finally
    xq.free;
  end;
end;

function TLibraryIdStringList.DoCompareText(const s1, s2: string): PtrInt;
var
  l1, l2, l: SizeInt;
  c1, c2: Boolean;
  i, i1, i2: SizeInt;
begin
  l1 := length(s1);
  l2 := length(s2);
  l := min(l1, l2);
  for i := 1 to l do
    if s1[i] <> s2[i] then begin
      if i <= 3 then begin
        c1 := (s1[1] = '-') or (s1[1] = '_');
        c2 := (s2[1] = '-') or (s2[1] = '_');
        if c1 <> c2 then begin
          if c1 then exit(-1);
          if c2 then exit(1);
        end;
        //sort current country before other countries
        c1 := strBeginsWith(s1, localeCountry);
        c2 := strBeginsWith(s2, localeCountry);
        if c1 <> c2 then begin
          if c1 then exit(-1);
          if c2 then exit(1);
        end;
      end;
      i1 := i;
      i2 := i;
      if (s1[i1] = '+') and (i1 < l) then inc(i1);
      if (s2[i2] = '+') and (i2 < l) then inc(i2);
      result := ord(upCase(s1[i1])) - ord(upCase(s2[i2]));
      if result <> 0 then exit;
      result := l1 - l2;
      if result <> 0 then exit;
      result := i1 - i2;
      if result <> 0 then exit;
      break;
    end;
  result := l1 - l2;
end;

function TLibraryMetaDataEnumerator.MoveNext: boolean;
var
  exploded: TStringArray;
  newPrettyCountryState, newPrettyLocation: String;
begin
  i += 1;
  if i >= manager.count then exit(false);
  libraryId := manager.flibraryIds[i];
  exploded := strSplit(libraryId, '_');
  newPrettyCountryState := TLibrary.pretty(exploded[0]) + ' - ' + TLibrary.pretty(exploded[1]);
  newPrettyLocation := TLibrary.pretty(exploded[2]);
  newCountryState := (i = 0) or (newPrettyCountryState <> prettyCountryState);
  newLocation := (i = 0) or (newPrettyLocation <> prettyLocation);
  if newCountryState then prettyCountryState := newPrettyCountryState;
  if newLocation then prettyLocation := newPrettyLocation;
  result := true;
end;


function TLibrary.readProperty(tagName: string; properties: TProperties):TParsingResult;
var value:string;
  function parseTesting: TTestingInfo;
  begin
    case value of
      'yes': result := tiYes;
      'no': result := tiNo;
      //'unknown': result := tiYes;
      'broken': result := tiBroken;
      else result := tiUnknown;
    end;
  end;

begin
  tagName:=LowerCase(tagName);
  value:=getProperty('value',properties);
  if  tagName='homepage' then homepageBase:=value
  else if  tagName='catalogue' then fhomepageCatalogue:=value
  else if tagName='longname' then prettyNameLong:=value
  else if tagName='shortname' then prettyNameShort:=value
  else if tagName='longname' then prettyNameLong:=value
  else if tagName='singlebookrenew' then canModifySingleBooks:=StrToBool(value) //deprecated, will be overriden at the end of loadFromFile
  else if tagName='template' then template:=libraryManager.getCombinedTemplate(template, value)
  else if tagName='variable' then begin
    defaultVariables.NameValueSeparator:='=';
    defaultVariables.Add(getProperty('name',properties)+defaultVariables.NameValueSeparator+value);
  end else if tagName='username' then usernameRegEx.Expression:=getProperty('matches',properties)
  else if tagName='password' then passwordRegEx.Expression:=getProperty('matches',properties)
  else if tagName='maxrenewcount' then maxRenewCount:=StrToInt(value)
  else if tagName='deprecatedname' then deprecatedId:=value
  else if tagName='table-comment' then tableComment:=value
  else if tagName='segregated-accounts' then segregatedAccounts:=StrToBool(value)
  else if tagName='testing-search' then ftestingSearch:=parseTesting
  else if tagName='testing-account' then ftestingAccount:=parseTesting
  ;
  Result:=prContinue;
end;

procedure TLibrary.loadFromFile(fileName: string);
begin
  loadFromString(strLoadFromFileUTF8(fileName), '');
end;

procedure TLibrary.loadFromString(data, fileName: string);
begin
  id:=ChangeFileExt(ExtractFileName(fileName),'');;
  maxRenewCount:=-1;
  ftestingAccount := tiUnknown;
  ftestingSearch := tiUnknown;
  parseXML(data,@readProperty,nil,nil,CP_UTF8);
  if template<>nil then begin
    canModifySingleBooks:=(template.findAction('renew-single')<>nil)  or
                          (template.findAction('renew-list')<>nil) ;
  end;
  if prettyNameShort = '' then
    prettyNameShort:=strCopyFrom(id, strRpos('_', id)+1) + ' '+ prettyLocation;
  if (ftestingSearch = tiBroken) and (ftestingAccount = tiBroken) then tableComment := 'Nach Änderungen auf der Bibliothekswebseite kaputt'
  else if (ftestingSearch = tiYes) and (ftestingAccount in [tiBroken, tiNo]) then prettyNameLong += ' (nur Suche)'
  else if (ftestingSearch = tiYes) and (ftestingAccount = tiUnknown) then prettyNameLong += ' (nur Suche getestet)'
  else if (ftestingSearch in [tiBroken, tiNo]) and (ftestingAccount = tiYes) then prettyNameLong += ' (nur Konto)'
end;

//==============================================================================
//                            TLibrary (+Manager)
//==============================================================================
function TLibrary.getAccountObject():TCustomAccountAccess;
begin
  result:=TTemplateAccountAccess.create(self);
end;

constructor TLibrary.create;
begin
  defaultVariables:=TStringList.Create;
  usernameRegEx:=TRegExpr.Create('.');
  passwordRegEx:=TRegExpr.Create('.');
end;

destructor TLibrary.destroy;
begin
  usernameRegEx.Free;
  passwordRegEx.Free;
  defaultVariables.free;
  inherited destroy;
end;

type TInternetAccessNonSense = TMockInternetAccess;

function TLibrary.homepageCatalogue: string;
var
  parser: TMultipageTemplateReader;
  i: Integer;
  tempinternet: TInternetAccess;
begin
  if (fhomepageCatalogue = '') and (template.findAction('catalogue') <> nil)  then begin
    tempinternet := TInternetAccessNonSense.create(); //will crash if used
    parser := TMultipageTemplateReader.create(template,tempinternet, nil);
    parser.parser.KeepPreviousVariables:=kpvKeepValues; //used in book list reader. needed here, too?
    for i:=0 to defaultVariables.count-1 do
      parser.parser.variableChangeLog.ValuesString[defaultVariables.Names[i]]:=defaultVariables.ValueFromIndex[i];
    parser.callAction('catalogue');
    fhomepageCatalogue:=parser.parser.variableChangeLog['url'].toString;
    parser.free;
    tempinternet.free;
  end;
  if fhomepageCatalogue = '' then fhomepageCatalogue := homepageBase;
  result := fhomepageCatalogue;
end;

function TLibrary.location: string;
begin
  result := strSplit(id, '_')[2];
end;

function TLibrary.state: string;
begin
  result := strSplit(id, '_')[1];
end;

function TLibrary.country: string;
begin
  result := strSplit(id, '_')[0];
end;

function TLibrary.prettyLocation: string;
begin
  Result := pretty(location);
end;

function TLibrary.prettyState: string;
begin
  result := pretty(state);
end;

function TLibrary.prettyCountry: string;
begin
  result := pretty(country);
end;

function TLibrary.prettyCountryState: string;
begin
  result := prettycountry + ' - ' + prettyState;
end;

function TLibrary.prettyNameLongWithComment: string;
begin
  if tableComment = '' then exit(prettyNameLong);
  result := prettyNameLong + LineEnding + tableComment;
end;

class function TLibrary.pretty(const l: string): string;
begin
  Result := l;
  if pos('+', result) = 0 then exit;
  result := StringReplace(Result, '+ue', 'ü', [rfReplaceAll]);
  result := StringReplace(Result, '+oe', 'ö', [rfReplaceAll]);
  result := StringReplace(Result, '+ae', 'ä', [rfReplaceAll]);
  result := StringReplace(Result, '+sz', 'ß', [rfReplaceAll]);
  result := StringReplace(Result, '++', ' ', [rfReplaceAll]);
  //also Bridge.LibraryDetails.decodeIdEscapes
end;

class function TLibrary.unpretty(const l: string): string;
begin
  Result := l;
  result := StringReplace(Result, 'ü', '+ue', [rfReplaceAll]);
  result := StringReplace(Result, 'ö', '+oe', [rfReplaceAll]);
  result := StringReplace(Result, 'ä', '+ae', [rfReplaceAll]);
  result := StringReplace(Result, 'ß', '+sz', [rfReplaceAll]);
  result := StringReplace(Result, ' ', '++', [rfReplaceAll]);
  //also in maketable
end;

function TLibraryManager.getCount: integer;
begin
  result := flibraryIds.Count;
end;

function TLibraryManager.getLibraryFromIndex(i: integer): TLibrary;
begin
  if flibraryIds.Objects[i] <> nil then
    exit(TLibrary(flibraryIds.Objects[i]));
  result := get(flibraryIds[i]);
end;

function TLibraryManager.getLibraryIdFromIndex(i: integer): string;
begin
  result := flibraryIds[i];
end;

function TLibraryManager.getAccountObject(libID: string):TCustomAccountAccess;
var lib: TLibrary;
begin
  Result:=nil;
  lib := get(libID);
  if lib = nil then lib := libraries[0]; //exception will crash since nothing is initialized yet
  result := lib.getAccountObject();
end;


constructor TLibraryManager.create();
begin
  flibraries:=Tlist.create;
  templates:=tStringList.Create;
  flibraryIds := TLibraryIdStringList.Create;
  flibraryIds.CaseSensitive := true;
  flibraryIds.Sorted := true;
  flibraryIds.Duplicates := dupIgnore;
end;
destructor TLibraryManager.destroy();
var i:integer;
begin
  for i:=0 to flibraries.Count-1 do
    libraries[i].free;
  flibraries.free;
  for i:=0 to templates.Count-1 do
    TMultiPageTemplate(templates.Objects[i]).free;
  templates.free;
  flibraryIds.Free;
  inherited;
end;


procedure TLibraryManager.init(apath: string);
var i:longint;
    userlibs: TStringArray;
begin
  basePath:=apath;

  flibraryIds.Sorted := false;
  flibraryIds.text := assetFileAsString('libraries/libraries.list');
  for i:=flibraryIds.Count-1 downto 0 do
    if flibraryIds[i] = '' then
      flibraryIds.Delete(i);
  flibraryIds.Sort;
  flibraryIds.Sorted := true;
  userlibs := strSplit(userConfig.ReadString('base', 'user-libraries', ''), ',');
  for i := 0 to high(userlibs) do begin
    userlibs[i] := trim(userlibs[i]);
    if userlibs[i] = '' then continue;
    flibraryIds.Add(userlibs[i]);
  end;
end;

function TLibraryManager.getTemplate(templateName: string): TMultiPageTemplate;
var i:longint;
begin
  i:=templates.IndexOf(templateName);
  if i>=0 then Result:=TMultiPageTemplate(templates.Objects[i])
  else begin
    Result:=TMultiPageTemplate.Create();
    try
      result.loadTemplateWithCallback(@assetFileAsString, 'libraries/templates'+DirectorySeparator+templateName+DirectorySeparator,templateName);
    except
      result.free;
      raise
    end;
    templates.AddObject(templateName,Result);
  end;
end;

function TLibraryManager.getCombinedTemplate(template: TMultiPageTemplate; templateName: string): TMultiPageTemplate;
var i:longint;
  child: TMultiPageTemplate;
begin
  if template = nil then exit(getTemplate(templateName));
  i:=templates.IndexOf(template.name + '|' + templateName);
  if i>=0 then Result:=TMultiPageTemplate(templates.Objects[i])
  else begin
    result := template.clone;
    Result.name:=Result.name+'|'+templateName;
    child := getTemplate(templateName);
    for i := 0 to high(child.baseActions.children) do begin
      setlength(Result.baseActions.children, length(Result.baseActions.children) + 1);
      Result.baseActions.children[high(Result.baseActions.children)] := child.baseActions.children[i].clone;
    end;
    templates.AddObject(result.name,Result);
  end;
end;

function TLibraryManager.getAccount(libID,accountID: string):TCustomAccountAccess;
begin
  if logging then log('TLibraryManager.getAccount('+libID+','+accountID+') started');
  Result:=getAccountObject(libID);
  if libID = Result.lib.deprecatedId then begin
    if FileExists(basePath+libID+'#'+accountID+'.history') then
      RenameFileUTF8(basePath+libID+'#'+accountID+'.history', basePath+result.lib.id+'#'+accountID+'.history'); //old files, not xml files
    if FileExists(basePath+libID+'#'+accountID+'.current') then
      RenameFileUTF8(basePath+libID+'#'+accountID+'.current', basePath+result.lib.id+'#'+accountID+'.current');
    if FileExists(basePath+libID+'#'+accountID+'.history.xml') then
      RenameFileUTF8(basePath+libID+'#'+accountID+'.history.xml', basePath+result.lib.id+'#'+accountID+'.history.xml');
    if FileExists(basePath+libID+'#'+accountID+'.current.xml') then
      RenameFileUTF8(basePath+libID+'#'+accountID+'.current.xml', basePath+result.lib.id+'#'+accountID+'.current.xml');
    if FileExists(basePath+libID+'#'+accountID+'.config') then
      RenameFileUTF8(basePath+libID+'#'+accountID+'.config', basePath+result.lib.id+'#'+accountID+'.config');
    log('Import old '+basePath+libID+'#'+accountID+'.* => ' +basePath+result.lib.id+'#'+accountID+'.*');
  end;
  result.init(basePath,accountID);
  if logging then log('TLibraryManager.getAccount('+libID+','+accountID+') ended');
end;

function TLibraryManager.getAccount(mixID: string): TCustomAccountAccess;
var libID: string;
begin
  libID:=strSplitGet('#', mixID);
  mixID := decodeSafeFileName(mixID);
  result:=getAccount(libID,mixID);
end;

function TLibraryManager.enumerateLibraryMetaData: TLibraryMetaDataEnumerator;
begin
  result.manager := self;
  result.i := -1;
end;

function TLibraryManager.get(id: string): TLibrary;
var
  i, index: Integer;
  libraryFileName: String;
begin
  if logging then log('LOADING LIBRARY: ' + id);
  if not flibraryIds.Find(id, index) then begin
    for i:=0 to count-1 do begin
      result := getLibraryFromIndex(i);
      if assigned(result) and (result.deprecatedId = id) then
        exit;
    end;
    exit(nil);
  end;
  result := TLibrary( flibraryIds.Objects[index] );
  if Result = nil then begin
    result:=TLibrary.Create;
    try
      libraryFileName := 'libraries/'+id+'.xml';
      result.loadFromString(assetFileAsString(libraryFileName), libraryFileName);
    except
      FreeAndNil(result);
      if FileExistsUTF8(assetPath + libraryFileName) then raise; //user defined libraries must not raise an exception or they write some bullshit and cannot start the app anymore
    end;
    flibraryIds.Objects[index] := result;
  end;
end;

function TLibraryManager.getUserLibraries(): TList;
var
  temp: TStringArray;
  i: Integer;
begin
  temp := strSplit(userConfig.ReadString('base', 'user-libraries', ''), ',');
  result := tlist.Create;
  result.Capacity:=length(temp);
  for i:=0 to high(temp) do
    if Trim(temp[i])<>'' then begin
      result.Add(get(trim(temp[i])));
      if result.Last = nil then result.Delete(result.Count-1);
    end;
end;

function TLibraryManager.setUserLibrary(trueid, data: string): TLibrary;
var
  lib: TLibrary;
  userlibs: TStringArray;
  acc: TCustomAccountAccess;
begin
  if not DirectoryExists(userPath+'libraries') then
    ForceDirectories(userPath+'libraries');
  strSaveToFileUTF8(userPath+'libraries/'+trueid+'.xml', data);

  userlibs := strSplit(userConfig.ReadString('base', 'user-libraries', ''), ',');
  if arrayIndexOf(userlibs, trueId) < 0 then
    if userConfig.ReadString('base', 'user-libraries', '') = '' then userConfig.WriteString('base', 'user-libraries', trueId)
    else userConfig.WriteString('base', 'user-libraries', userConfig.ReadString('base', 'user-libraries', '')+','+trueId);

  lib := get(trueId);
  if lib = nil then begin
    lib := TLibrary.create;
    lib.id:=trueid;
    flibraryIds.AddObject(trueid, lib);
  end;
  lib.template:=nil;
  lib.variables.Clear;
  lib.loadFromString(data, 'libraries/'+trueid+'.xml');
  for acc in accounts do
    if acc.getLibrary().id = trueid then
      acc.resetConnection;

  result := lib;
end;

function TLibraryManager.downloadAndInstallUserLibrary(url: string): TLibrary;
var
  page: String;
  temp: IXQValue;
  description: String;
  id: String;
  templateUrl: String;
  template: String;
  templateId: String;
begin
  page := retrieve(url);
  temp := xqvalue();
  try
    temp := process(page, '<link rel="videlibri.description" href="{.}"/>');
  except
  end;
  if temp.isUndefined then begin
    raise ELibraryException.create(rsNoTemplateLinkFound);
    exit(nil);
  end;
  description := retrieve(strResolveURI(temp.toString, url));

  temp := process(description, '(//id/@value)[1]');
  if temp.isUndefined then id := '-_-_-_undefined'+IntToStr(Random(10000))
  else id := temp.toString;

  temp := process(page, '<link rel="videlibri.template" href="{.}"/>');
  if not temp.isUndefined then begin
    templateUrl := strResolveURI(temp.toString, url);
    template := retrieve(templateUrl);
    templateId := process(description, '(//template/@value)[1]').toString;

    if not DirectoryExists(userPath+'libraries/templates/'+templateId) then
      ForceDirectories(userPath+'libraries/templates/'+templateId);
    strSaveToFileUTF8(userPath+'libraries/templates/'+templateId+'/template', template);

    for temp in process(template, '(//@templateFile,//pattern/@href)') do
      if not strContains(temp.toString, '..') then
        strSaveToFileUTF8(userPath+'libraries/templates/'+templateId+'/'+temp.toString, retrieve(strResolveURI(temp.toString, templateUrl)));

    libraryManager.reloadTemplate(templateId);
  end;


  result := libraryManager.setUserLibrary(id, description);
end;

procedure TLibraryManager.deleteUserLibrary(trueid: string);
var
  temp: TStringArray;
  i: Integer;
begin
  temp := strSplit(userConfig.ReadString('base', 'user-libraries', ''), ',');
  for i:=high(temp) downto 0 do
    if Trim(temp[i])=trueid then
      arrayDelete(temp, i);
  userConfig.WriteString('base', 'user-libraries', strJoin(temp, ','));
end;

procedure TLibraryManager.reloadTemplate(templateId: string);
var
  i: Integer;
  oldIdx: Integer;
  newTemplate: TMultiPageTemplate;
  inUse: Boolean;
  oldTemplate: TMultiPageTemplate;
begin
  oldIdx := templates.IndexOf(templateId);
  {for i := 0 to templates.Count - 1 do
    if (templates[i] = templateId)  then begin
      if oldIdx = -1 then begin
        oldIdx:=i;
        break;
      end;
    end;}
  if oldIdx = -1 then exit;
  oldTemplate := TMultiPageTemplate(templates.Objects[oldIdx]);
  templates.Delete(oldIdx);
  newTemplate := getTemplate(templateId);

  inUse := false;
  for i := 0 to accounts.Count - 1 do begin
    if (accounts[i].getLibrary().template = oldTemplate)
        and assigned(accounts[i])
        and accounts[i].InheritsFrom(TTemplateAccountAccess)
        and (accounts[i].thread <> nil) then
      inUse:=true;
  end;
  if not inUse then replaceTemplate(oldTemplate, newTemplate)
  else begin
    if logging then log('Prepare pending template reload');
    EnterCriticalSection(updateThreadConfig.threadManagementSection);
    setlength(pendingReplacementsOld, length(pendingReplacementsOld)+1);
    pendingReplacementsOld[high(pendingReplacementsOld)] := oldTemplate;
    setlength(pendingReplacementsNew, length(pendingReplacementsNew)+1);
    pendingReplacementsNew[high(pendingReplacementsNew)] := newTemplate;
    LeaveCriticalSection(updateThreadConfig.threadManagementSection);
  end;
end;

procedure TLibraryManager.reloadPendingTemplates();
var
  i: Integer;
begin
  for i := 0 to high(pendingReplacementsOld) do
    replaceTemplate(pendingReplacementsOld[i], pendingReplacementsNew[i]);
  SetLength(pendingReplacementsOld,0);
  SetLength(pendingReplacementsNew,0);
end;

procedure TLibraryManager.replaceTemplate(old, new: TMultiPageTemplate);
var
  i: Integer;
  j: Integer;
  lib: TLibrary;
begin
  if logging then log('Replacing template: '+new.name);

  for i := 0 to count - 1 do begin
    if flibraryIds.Objects[i] = nil then continue;
    lib := TLibrary(flibraryIds.Objects[i]);
    if lib.template = old then begin
      lib.template := new;
      for j := 0 to accounts.Count - 1 do
        if (accounts[j].getLibrary() = lib)
            and assigned(accounts[j])
            and accounts[j].InheritsFrom(TTemplateAccountAccess)
            and (accounts[j].thread = nil) then
          TTemplateAccountAccess(accounts[j]).resetlib();
    end;
  end;

  if logging then log('End replacing');
end;


procedure TBookLists.updateSharedDates();
var i:integer;
begin
  if logging then
    log('TBookLists.updateSharedDates started');
  nextLimit:=MaxInt-1;
  nextNotExtendableLimit:=MaxInt;
  for i:=0 to bookLists[bltInCurrentFile].count-1 do
    if bookLists[bltInCurrentFile][i].dueDate>0 then begin
      if bookLists[bltInCurrentFile][i].dueDate<nextLimit then
        nextLimit:=bookLists[bltInCurrentFile][i].dueDate;
      if bookLists[bltInCurrentFile][i].status in BOOK_NOT_EXTENDABLE then
        if bookLists[bltInCurrentFile][i].dueDate<nextNotExtendableLimit then
          nextNotExtendableLimit:=bookLists[bltInCurrentFile][i].dueDate;
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
  DeleteFile(bookListCurFileName+'.xml');
  DeleteFile(bookListOldFileName+'.xml');
  sl:=FindAllFiles(ExtractFilePath(bookListOldFileName),ExtractFileNameOnly(bookListOldFileName)+'.????????',false);
  for i:=0 to sl.count-1 do
    DeleteFile(sl[i]);
  sl:=FindAllFiles(ExtractFilePath(bookListOldFileName),ExtractFileNameOnly(bookListOldFileName)+'.xml' +'.????????',false);
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
    if (currentDate-ownerLib.config.ReadInteger('base','last-history-backup',0)>HistoryBackupInterval) then begin
      if FileExistsUTF8(bookListOldFileName) then
        CopyFile(bookListOldFileName,bookListOldFileName+'.'+dateTimeFormat('yyyymmdd',currentDate));
      if FileExistsUTF8(bookListOldFileName + '.xml') then
        CopyFile(bookListOldFileName+ '.xml',bookListOldFileName+ '.xml'+'.'+dateTimeFormat('yyyymmdd',currentDate));
      ownerLib.config.WriteInteger('base','last-history-backup',currentDate);
    end;
    if bookLists[bltInOldData].Count>0 then begin
      if not bookListOldLoaded then
        needOldBookList;
      bookLists[bltInOldData].save(bookListOldFileName)
    end;
  end;
  bookLists[bltInCurrentFile].save(bookListCurFileName);
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

procedure TBookLists.mergePersistentToCurrentUpdate;
begin
  bookLists[bltInCurrentDataUpdate].mergePersistentFields(bookLists[bltInCurrentFile]);
end;

procedure TBookLists.completeUpdate;
var
  tempList: TBookList;
begin
  if logging then log('TBookLists.completeUpdate started');

  if keepHistory then begin
    tempList := TBookList.create();
    try
      tempList.Assign(bookLists[bltInCurrentFile]);
      tempList.removeAllFrom(bookLists[bltInCurrentDataUpdate]);
      tempList.lendList:=false;
      bookLists[bltInOldData].AddList(tempList);
    finally
      tempList.free;
    end;
  end;

  bookLists[bltInCurrentFile].Assign(bookLists[bltInCurrentDataUpdate]);
  //bookLists[bltInCurrentDataUpdate].clear;

  updateSharedDates();
  ownerLib.flastCheckDate:=currentDate;

   if logging then log('TBookLists.completeUpdate started')
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

function TCustomAccountAccess.GetConnected: boolean;
var tempTime: QWord;
begin
  tempTime := GetTickCount64;
  result:=FConnected and (tempTime >= FConnectingTime) and (tempTime - FConnectingTime < Timeout);
end;

function TCustomAccountAccess.GetNeedSingleBookUpdate: boolean;
begin
  result := false;
end;

function TCustomAccountAccess.GetUpdated: boolean;
var
  tempTime: QWord;
begin
  tempTime := GetTickCount64;
  result:=GetConnected and FUpdated and (tempTime >= FUpdateTime) and (tempTime - FUpdateTime < Timeout);
end;

procedure TCustomAccountAccess.updateConnectingTimeout;
begin
  FConnectingTime:=GetTickCount64;
end;

procedure TCustomAccountAccess.SetAccountType(AValue: integer);
begin
  if lib.segregatedAccounts then begin
    if avalue = 0 then avalue := 1;
  end else avalue := 0;
  if FAccountType=AValue then Exit;
  FAccountType:=AValue;
  resetConnection;
end;

procedure TCustomAccountAccess.setPassword(AValue: string);
begin
  if pass=AValue then Exit;
  pass:=AValue;
  resetConnection;
end;

function TCustomAccountAccess.getCharges: currency;
begin
  result:=fcharges;
end;

procedure TCustomAccountAccess.initFromConfig;
begin
//Datenladen/cachen
  pass:=config.ReadString('base','pass','');
  accountType:=config.ReadInteger('base','type',0);
  books.keepHistory:=config.ReadBool('base','keep-history',true);
  flastCheckDate:=config.ReadInteger('base','lastCheck',0);
  keepHistory:=config.readBool('base','keep-history',true);
  prettyName:=config.readString('base','prettyName', user);
  extendDays:=config.readInteger('base','extend-days',7);
  extendType:=TExtendType(config.readInteger('base','extend',0));
  fcharges:=currency(config.readInteger('base','charge',-100))/100.0;;
  FEnabled:=config.ReadBool('base','enabled',true);
end;

constructor TCustomAccountAccess.create(alib:TLibrary);
begin
  FConnected:=false;
  fbooks:=nil;
  currentDate:=longint(trunc(date));
  config:=nil;
  thread:=nil;
  lib:=alib;
  fcharges:=-1;
  FEnabled:=true;
  FTimeout:=10*60*1000;
  broken:=0;
end;

destructor TCustomAccountAccess.destroy;
begin
  //if connected then disconnect;
  if books<>nil then
    books.free;
  if config<>nil then
    config.free;
  if internet<>nil then
    internet.free;
  inherited;
end;

procedure TCustomAccountAccess.init(apath,userID:string);
var
  filePrefix: String;
begin
  self.path:=apath;
  self.user:=userID;

  filePrefix := path+getPlusEncodedID();
  if not FileExists(filePrefix+'.config') then
    if FileExists(path + lib.id+'#'+user+'.config') then begin
      RenameFile(path + lib.id+'#'+user+'.config', filePrefix + '.config');
      RenameFile(path + lib.id+'#'+user+'.history.xml', filePrefix + '.history.xml');
      RenameFile(path + lib.id+'#'+user+'.current.xml', filePrefix + '.current.xml');
    end;

  fbooks:=TBookLists.create(self,filePrefix+'.history',filePrefix+'.current');
  config:=TSafeIniFile.Create(filePrefix+'.config');
  initFromConfig;
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
    config.WriteInteger('base','type',accountType);
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
  DeleteFile(path+getPlusEncodedID()+'.config');
end;

{function TCustomAccountAccess.lastCheck():longint;
begin
  result:=config.ReadInteger('base','lastCheck',0);
end;}

procedure TCustomAccountAccess.disconnect();
begin
  fconnected:=false;
end;

procedure TCustomAccountAccess.resetConnection;
begin
  fconnected := false;
end;

procedure TCustomAccountAccess.updateAll();
begin
  if not connected then connect(internet);
end;

procedure TCustomAccountAccess.updateSingle(book: TBook);
begin
  ignore(book);
  if not connected then connect(internet);
end;

procedure TCustomAccountAccess.extendAll();
begin
  if not updated then updateAll();
end;

procedure TCustomAccountAccess.extendList(bookList: TBookList);
begin
  ignore(bookList);
  if not updated then updateAll();
end;

procedure TCustomAccountAccess.cancelSingle(book: TBook);
begin
  ignore(book);
  if not updated then updateAll();
end;

procedure TCustomAccountAccess.cancelList(booklist: TBookList);
begin
  ignore(booklist);
  if not updated then updateAll();
end;

function TCustomAccountAccess.shouldExtendBook(book: TBook): boolean;
begin
  Result:=(book.status in BOOK_EXTENDABLE) and
          (book.dueDate<=currentDate+extendDays);
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

function TCustomAccountAccess.needChecking: boolean;
begin
  result := (lastCheckDate <= currentDate-refreshInterval) or existsCertainBookToExtend
end;

procedure TCustomAccountAccess.updateAllSingly;
var i:integer;
begin
  if not connected then connect(internet);
  for i:=0 to books.bookLists[bltInCurrentDataUpdate].count-1 do
    updateSingle(books.bookLists[bltInCurrentDataUpdate][i]);
  books.updateSharedDates();
end;

function TCustomAccountAccess.getLibrary():TLibrary;
begin
  result:=lib;
end;

procedure TCustomAccountAccess.changeUser(const s:string);
var oldID,newID:string;
begin
  oldID:=getPlusEncodedID();
  books.Free;
  config.free;
  
  user:=s;
  newID:=getPlusEncodedID();
  RenameFile(path+oldID+'.history.xml',path+newID+'.history.xml');
  RenameFile(path+oldID+'.current.xml',path+newID+'.current.xml');
  RenameFile(path+oldID+'.config',path+newID+'.config');
  fbooks:=TBookLists.create(self,path+newID+'.history',path+newID+'.current');
  config:=TSafeIniFile.Create(path+newID+'.config');

  resetConnection;
  //config.UpdateFile;
end;

function TCustomAccountAccess.getUser(): string;
begin
  result:=user;
end;

function TCustomAccountAccess.getPlusEncodedID():string;
begin
  result:=lib.id+'#'+encodeToSafeFileName(user);
end;



{ TTemplateAccountAccess }

function TTemplateAccountAccess.GetNeedSingleBookUpdate: boolean;
begin
  result := reader.bookListHasBeenClearedAndMightNeedSingleUpdate and (reader.findAction('update-single')<>nil);
end;

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
begin
  setVariables(reader.parser);
end;

procedure TTemplateAccountAccess.setVariables(parser: THtmlTemplateParser);
  procedure setVar(const name, value: string);
  begin
    if not parser.variableChangeLog.hasVariable(name) then
      parser.variableChangeLog.ValuesString[name] := value;
  end;

var i:longint;
begin
  for i:=0 to lib.defaultVariables.count-1 do
    setVar(lib.defaultVariables.Names[i], lib.defaultVariables.ValueFromIndex[i]);
  parser.variableChangeLog.ValuesString['username'] := user;       //force account variables, since a missing password change is really annoying
  parser.variableChangeLog.ValuesString['password'] := passWord;
  parser.variableChangeLog.add('type', xqvalue(accountType));
end;

procedure TTemplateAccountAccess.resetlib();
begin
  //todo: fix memory leak
  reader:=TBookListReader.create(lib.template);
  reader.books:=books.bookLists[bltInCurrentDataUpdate];
  FConnected:=false;
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
  reader.callAction('update-all');
  updateConnectingTimeout;
  FUpdated:=true;
  FUpdateTime:=FConnectingTime;
  if logging then log('Leave TTemplateAccountAccess.updateAll');
end;

procedure TTemplateAccountAccess.updateSingle(book: TBook);
begin
  if logging then
    log('enter TTemplateAccountAccess.updateSingle');
  setVariables();
  reader.selectBook(book);
  reader.callAction('update-single');
  updateConnectingTimeout;
  FUpdated:=true;
  FUpdateTime:=FConnectingTime;
  if logging then
    log('leave TTemplateAccountAccess.updateSingle');

end;

procedure TTemplateAccountAccess.updateAllSingly;
begin
  inherited updateAllSingly;
  reader.bookListHasBeenClearedAndMightNeedSingleUpdate := false;
end;

procedure TTemplateAccountAccess.extendAll;
var
 booksExtendableCount: Integer;
 realBooksToExtend: TBookList;
 i: Integer;
begin
  if logging then
    log('enter TTemplateAccountAccess.extendAll');
  if reader.findAction('renew-all')<>nil then begin
    setVariables();
    reader.callAction('renew-all');
  end else begin
    booksExtendableCount:=0;
    for i:=0 to books.currentUpdate.count-1 do
      if books.currentUpdate[i].status in BOOK_EXTENDABLE then
        booksExtendableCount+=1;

    if booksExtendableCount = reader.books.count then  extendList(reader.books)
    else begin
      realBooksToExtend:=TBookList.Create;
      for i:=0 to books.currentUpdate.Count-1 do
        if books.currentUpdate[i].status in BOOK_EXTENDABLE then
          realBooksToExtend.add(books.currentUpdate[i]);
      extendList(realBooksToExtend);
      realBooksToExtend.free
    end;
  end;
  updateConnectingTimeout;
  FUpdateTime:=FConnectingTime; //treat renew like update, since renew usually reupdates
  if logging then
    log('leave TTemplateAccountAccess.extendAll');
end;

function xqvalueBookList(reader: TBookListReader; list: TBookList): TXQValueSequence;
var
  i: Integer;
begin
  result := TXQValueSequence.create(list.Count);
  for i:=0 to list.Count-1 do
    result.add(reader.bookToPXP(list[i]));
end;

procedure TTemplateAccountAccess.extendList(booksToExtend: TBookList);
var i:longint;
    extendAction: TTemplateAction;
    book:TBook;
begin
  if booksToExtend.Count=0 then exit;
  if logging then log('Enter TTemplateAccountAccess.extendList');
  setVariables();
  extendAction:=reader.findAction('renew-list');
  if reader.findAction('renew-single')<>nil then begin
    if logging then log('use renew-single Template');
    extendAction:=reader.findAction('renew-single');
    for i:=booksToExtend.count-1 downto 0 do begin
      reader.selectBook(booksToExtend[i]);
      book:=reader.books.findBook(booksToExtend[i]);
      if book<>nil then reader.selectBook(book); //handles more instances of the same book
      reader.callAction(extendAction);
    end;
  end else
  if extendAction<>nil then begin
    if logging then log('use extendList Template');
    //if logging then log('bookList (count: '+inttostr(extendBookList.seq.count)+') is: '+extendBookList.debugAsStringWithTypeAnnotation());
    reader.parser.variableChangeLog.add('renew-books', xqvalueBookList(reader, booksToExtend));
    reader.callAction(extendAction);
  end else if reader.findAction('renew-all')<>nil then begin
    if logging then log('use renew-all Template');
    reader.callAction('renew-all');
  end;
  updateConnectingTimeout;
  FUpdateTime:=FConnectingTime; //treat renew like update, since renew usually reupdates
  if logging then log('Leave TTemplateAccountAccess.extendList');
end;
{
procedure TTemplateAccountAccess.orderList(booklist: TBookList);
var
  action: TTemplateAction;
  i: Integer;
begin
  if booklist.Count = 0 then exit;
  if logging then log('Enter TTemplateAccountAccess.orderList');
  setVariables();
  action := reader.findAction('order-list');
  if action <> nil then begin
    reader.parser.variableChangeLog.add('order-books', xqvalueBookList(reader, booklist));
    reader.callAction(action);
  end else if reader.findAction('order-single') <> nil then begin
    action := reader.findAction('order-single');
    for i := 0 to booklist.Count do begin
      reader.selectBook(booklist[i]);
      reader.callAction(action);
    end;
  end;
  if logging then log('Leave TTemplateAccountAccess.orderList');
end;
  }
procedure TTemplateAccountAccess.cancelList(booklist: TBookList);
var
  action: TTemplateAction;
  i: Integer;
  curlist: TBookList;
  j: Integer;
begin
  if booklist.Count = 0 then exit;
  if logging then log('Enter TTemplateAccountAccess.cancelList');
  setVariables();
  action := reader.findAction('cancel-list');
  if action <> nil then begin
    reader.parser.variableChangeLog.add('cancel-books', xqvalueBookList(reader, booklist));
    reader.callAction(action);
  end else if reader.findAction('cancel-single') <> nil then begin
    action := reader.findAction('cancel-single');
    for i := 0 to booklist.Count - 1 do begin
      reader.selectBook(booklist[i]);
      reader.callAction(action);
    end;
  end;

  //remove books (todo, do this in the template?)
  curlist := books.currentUpdate;
  for i := curlist.Count - 1 downto 0 do
    if curlist[i].status in [bsProvided, bsOrdered] then
      for j := 0 to booklist.Count - 1 do
        if curlist[i].equalToKey(booklist[j]) then begin
          curlist.delete(i);
          booklist.delete(j);
          break;
        end;


  updateConnectingTimeout;
  FUpdateTime:=FConnectingTime; //treat cancel like update, since renew usually reupdates

  if logging then log('Leave TTemplateAccountAccess.cancelList');
end;



constructor TTemplateAccountAccess.create(alib: TLibrary);
begin
  inherited;
  assert(alib.template<>nil,'Keine Templatelibrary-Class');
  lib:=alib;
  reader:=TBookListReader.create(alib.template);
  //parser.onVariableRead:=@parserVariableRead;
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

procedure TTemplateAccountAccess.changeUser(const s: string);
begin
  inherited changeUser(s);
  reader.books:=books.bookLists[bltInCurrentDataUpdate];
end;

procedure TTemplateAccountAccess.connect(AInternet: TInternetAccess);
begin
  if logging then log('TTemplateAccountAccess.connect started');
  internet:=ainternet;
  assert(internet <> nil);
  reader.internet:=internet;
  if connected then begin
    if logging then log('TTemplateAccountAccess.connect ended (already connected)');
    exit;
  end;
  reader.parser.variableChangeLog.clear;
  Reader.parser.oldVariableChangeLog.clear;
  setVariables();
  reader.callAction('connect');
  fconnected:=true;
  updateConnectingTimeout;
  if logging then log('TTemplateAccountAccess.connect ended');
end;

procedure TTemplateAccountAccess.disconnect();
begin
  //inherited disconnect();
  //hasBeenUpdated:=false;
end;



end.
