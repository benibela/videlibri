unit libraryParser;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, simplehtmlparser, extendedhtmlparser, simplexmlparser, inifilessafe,internetaccess,regexpr,booklistreader,multipagetemplate,bbutils,simplehtmltreeparser;


type
  TCustomAccountAccess=class;
  TCustomAccountAccessClass=class of TCustomAccountAccess;

  { TLibrary }

  TLibrary=class
  protected
    defaultVariables:TStringList;
    fhomepageCatalogue: string;
    function readProperty(tagName: string; properties: TProperties):TParsingResult;
  public
    template:TMultiPageTemplate;


    canModifySingleBooks:boolean;
    //function getPrettyNameShort():string;virtual;
    homepageBase:string;
    prettyNameLong:string;
    prettyNameShort:string;
    tableComment: string;
    id:string;
    deprecatedId: string;
    maxRenewCount: integer; //-1: if you can renew so frequently you want

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

  { TLibraryManager }

  TLibraryManager=class
  protected
    basePath: string;
    
    flibraries: TList;
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

    function enumerateLocations: TStringArray;
    function enumerateLocations(prettyState: string): TStringArray;
    function enumerateStates(prettyCountry: string): TStringArray;
    function enumerateCountries(): TStringArray;
    function enumerateCountryStates(): TStringArray;
    function enumeratePrettyLongNames(location: string): TStringArray;
    function enumeratePrettyLongNames: string;
    function enumeratePrettyShortNames: string;
    function getLibraryFromEnumeration(const pos:integer):TLibrary;inline;
    function getLibraryFromEnumeration(location: string; pos:integer):TLibrary;
    function getLibraryCountInEnumeration:integer;

    procedure enumerateVariableValues(const varName: string; result: TStringList);
    procedure enumerateLibrariesWithValue(const varName, value: string; result: TList);

    function get(id: string): TLibrary;

    function getUserLibraries(): TList;
    function setUserLibrary(trueid, data: string): TLibrary;
    function downloadAndInstallUserLibrary(url: string): TLibrary;
    procedure deleteUserLibrary(trueid: string);
    procedure reloadTemplate(templateId: string);
    procedure reloadPendingTemplates(); //only call this synchronized thrugh updateThreadConfig.threadManagementSection

    property libraries[i: integer]: TLibrary read getLibraryFromEnumeration; default;
    property count: integer read getLibraryCountInEnumeration;
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

  { ENeverEver }

  ENeverEverLibraryException=class(Exception)
    constructor create(mes: string);
  end;

  { EWrongPasswordException }

  TExtendType=(etAlways,etAllDepends,etSingleDepends,etNever);

  { TCustomAccountAccess }
                //first start info
  TCustomAccountAccess=class
  private
    FEnabled: boolean;
    FTimeout: dword;
    function GetConnected: boolean;
    function GetUpdated: boolean;
    procedure setPassword(AValue: string);
  protected
    fbooks: TBookLists;
    lib: TLibrary;
    internet: TInternetAccess;
    config: TSafeIniFile;
    path,user,pass,FPrettyName:string;
    FExtendType: TExtendType;
    FExtendDays,FLastCheckDate:integer;

    FKeepHistory, FConnected, FUpdated: boolean;
    FConnectingTime, FUpdateTime: dword;
    FCharges:Currency;
    function getCharges:currency;virtual;
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
    function connect(AInternet:TInternetAccess):boolean; virtual;abstract;
    //After disconnect you mustn't call any one except connect
    procedure disconnect(); virtual;
    
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
    function needSingleBookCheck():boolean;virtual;

    function getLibrary():TLibrary;

    procedure changeUser(const s:string); virtual;
    function getUser(): string;virtual;
    function getPlusEncodedID():string; virtual;

    property charges: currency read getCharges;        //<0 means unknown

    property books: TBookLists read fbooks;
    property prettyName: string read FPrettyName write FPrettyName;
    property passWord: string read pass write setPassword;
    property lastCheckDate: integer read FLastCheckDate;
    property extendDays: integer read FExtendDays write FExtendDays;
    property extendType: TExtendType read FExtendType write FExtendType;
    property keepHistory: boolean read FKeepHistory write FKeepHistory;
    property enabled: boolean read FEnabled write FEnabled;
    property connected: boolean read GetConnected;
    property updated: boolean read GetUpdated;
    property timeout: dword read FTimeout write FTimeout;
  end;


  { TTemplateAccountAccess }

  TTemplateAccountAccess = class(TCustomAccountAccess)
  protected
    lastTodayUpdate: longint; //if connected, time of update (ms, Gettickcount)
    extendAllBooks:boolean;

    reader:TBookListReader;

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
    function connect(AInternet:TInternetAccess):boolean; override;
    procedure disconnect(); override;

    function needSingleBookCheck():boolean;override;

    procedure updateAll;override;
    procedure updateSingle(book: TBook);override;
    procedure extendAll;override;
    procedure extendList(booksToExtend: TBookList);override;

   // procedure orderList(booklist: TBookList); override;
    procedure cancelList(booklist: TBookList); override;

    //function needSingleBookCheck():boolean;virtual;
  end;

  TExportImportFlag = (eifCurrent, eifHistory, eifConfig, eifPassword);
  TExportImportFlags = set of TExportImportFlag;
  TExportImportFlagsArray = array of TExportImportFlags;

  procedure exportAccounts(const fn: string; accounts: array of TCustomAccountAccess; flags: array of TExportImportFlags );
  procedure importAccountsPrepare(const fn: string; out parser: TTreeParser; out accounts: TStringArray; out flags: TExportImportFlagsArray);
  //frees the parser
  procedure importAccounts(parser: TTreeParser; impAccounts: TStringArray; flags: TExportImportFlagsArray);


implementation
uses applicationconfig,bbdebugtools,FileUtil,LCLIntf,LazFileUtils, xquery,androidutils,simpleinternet,mockinternetaccess,libraryAccess,strutils;

resourcestring
  rsNoTemplateLinkFound = 'Der Link verweist auf kein VideLibri-Template (es gibt kein Element < link rel="videlibri.description" auf der Seite)';
  rsLibraryNotFound = 'Bücherei nicht gefunden: %s:%s';


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

  finally
    xq.free;
    parser.free;
  end;
end;


function TLibrary.readProperty(tagName: string; properties: TProperties):TParsingResult;
var value:string;
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
  ;
  Result:=prContinue;
end;

procedure TLibrary.loadFromFile(fileName: string);
begin
  loadFromString(strLoadFromFileUTF8(fileName), '');
end;

procedure TLibrary.loadFromString(data, fileName: string);
var
  temp: TStringArray;
begin
  id:=ChangeFileExt(ExtractFileName(fileName),'');;
  maxRenewCount:=-1;
  parseXML(data,@readProperty,nil,nil,eUTF8);
  if template<>nil then begin
    canModifySingleBooks:=(template.findAction('renew-single')<>nil)  or
                          (template.findAction('renew-list')<>nil) ;
  end;
  if prettyNameShort = '' then
    prettyNameShort:=strCopyFrom(id, strRpos('_', id)+1) + ' '+ prettyLocation;
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
  action: TTemplateAction;
  parser: TMultipageTemplateReader;
  i: Integer;
  tempinternet: TInternetAccess;
begin
  if (fhomepageCatalogue = '') and (template.findAction('catalogue') <> nil)  then begin
    tempinternet := TInternetAccessNonSense.create(); //will crash if used
    parser := TMultipageTemplateReader.create(template,tempinternet);
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
end;

class function TLibrary.unpretty(const l: string): string;
begin
  Result := l;
  result := StringReplace(Result, 'ü', '+ue', [rfReplaceAll]);
  result := StringReplace(Result, 'ö', '+oe', [rfReplaceAll]);
  result := StringReplace(Result, 'ä', '+ae', [rfReplaceAll]);
  result := StringReplace(Result, 'ß', '+sz', [rfReplaceAll]);
end;

function TLibraryManager.getAccountObject(libID: string):TCustomAccountAccess;
var i:integer;
begin
  Result:=nil;
  for i:=0 to flibraries.count-1 do
    if (TLibrary(libraries[i]).id=libID) or (TLibrary(libraries[i]).deprecatedId = libID) then
      exit(TLibrary(libraries[i]).getAccountObject());
  result := libraries[0].getAccountObject(); //exception will crash since nothing is initialized yet
  //raise Exception('Bücherei '+libID+' ist unbekannt');
end;
constructor TLibraryManager.create();
begin
  flibraries:=Tlist.create;
  templates:=tStringList.Create;
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
  inherited;
end;

function libraryLocationCompare(lib1: TObject; lib2: pointer): integer;
var
  loc1: String;
  loc2: String;
begin
  loc1 := TLibrary(lib1).location;
  loc2 := TLibrary(lib1).location;
  result := CompareStr(loc1, loc2);
end;

procedure TLibraryManager.init(apath: string);
var //tempLibrary:TLibrary;
    libraryFiles: TStringList;
    newLib:TLibrary;
    i:longint;
    userlibs: TStringArray;
    next: Pointer;
begin
  basePath:=apath;

  libraryFiles:=TStringList.Create;
  libraryFiles.text := assetFileAsString('libraries/libraries.list');
  for i:=libraryFiles.Count-1 downto 0 do
    if libraryFiles[i] = '' then libraryFiles.Delete(i);
  for i:=0 to libraryFiles.count-1 do begin
    newLib:=TLibrary.Create;
    newLib.loadFromString(assetFileAsString('libraries/'+libraryFiles[i]), 'libraries/'+libraryFiles[i]);
    flibraries.Add(newLib);
  end;
  libraryFiles.free;
  userlibs := strSplit(userConfig.ReadString('base', 'user-libraries', ''), ',');
  for i := 0 to high(userlibs) do begin
    userlibs[i] := trim(userlibs[i]);
    if userlibs[i] = '' then continue;
    newLib:=TLibrary.Create;
    try
      newLib.loadFromString(assetFileAsString('libraries/'+userlibs[i]+'.xml'), 'libraries/'+userlibs[i]+'.xml');
      next := binarySearch(@flibraries.List^[0], @flibraries.List^[flibraries.Count-1], sizeof(pointer), @libraryLocationCompare, newLib, bsFirst, [bsGreater]);
      if next = nil then flibraries.Insert(0, newLib)
      else flibraries.Insert((next - @flibraries.List[0]) div sizeof(Pointer), newLib)
    except
      newLib.free;
    end;
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
    if FileExists(basePath+libID+'#'+accountID+'.history') then CopyFile(basePath+libID+'#'+accountID+'.history', basePath+result.lib.id+'#'+accountID+'.history'); //old files, not xml files
    if FileExists(basePath+libID+'#'+accountID+'.current') then CopyFile(basePath+libID+'#'+accountID+'.current', basePath+result.lib.id+'#'+accountID+'.current');
    CopyFile(basePath+libID+'#'+accountID+'.config', basePath+result.lib.id+'#'+accountID+'.config');
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

function TLibraryManager.enumerateLocations: TStringArray;
var sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  for i:= 0 to flibraries.count -1 do
    if sl.IndexOf(TLibrary(libraries[i]).prettyLocation) < 0 then
      sl.Add(TLibrary(libraries[i]).prettyLocation);
  sl.Sort;
  setlength(result, sl.Count);
  for i := 0 to high(result) do
    result[i] := sl[i];
  sl.free;
end;

function TLibraryManager.enumerateLocations(prettyState: string): TStringArray;
var
  i: Integer;
begin
  if pos(' - ', prettyState) > 0 then prettyState := strSplit(prettyState, ' - ')[1];
  for i := 0 to flibraries.Count-1 do
    if (self[i].prettyState = prettyState) and not (arrayContains(result, self[i].prettyLocation)) then
      arrayAdd(result, self[i].prettyLocation);
  stableSort(result);
end;

function TLibraryManager.enumerateStates(prettyCountry: string): TStringArray;
var
  i: Integer;
begin
  for i := 0 to flibraries.Count-1 do
    if (self[i].prettyCountry = prettyCountry) and not (arrayContains(result, self[i].prettyState)) then
      arrayAdd(result, self[i].prettyState);
  stableSort(result);
end;

function TLibraryManager.enumerateCountries: TStringArray;
var
  i: Integer;
begin
  for i := 0 to flibraries.Count-1 do
    if not (arrayContains(result, self[i].prettyCountry)) then
      arrayAdd(result, self[i].prettyCountry);
end;

function TLibraryManager.enumerateCountryStates: TStringArray;
var
  country: String;
  state: String;
begin
  for country in enumerateCountries do
    for state in enumerateStates(country) do
      arrayAdd(result, country + ' - ' + state);
end;

function TLibraryManager.enumeratePrettyLongNames(location: string): TStringArray;
var
  i: Integer;
begin
  result := nil;
  location := TLibrary.unpretty(location);
  location:='_'+location+'_';
  for i:=0 to flibraries.count-1 do
    if strContains(TLibrary(libraries[i]).id, location) then
      arrayAdd(result, TLibrary(libraries[i]).prettyNameLong);
end;

function TLibraryManager.enumeratePrettyLongNames: string;
var i:integer;
begin
  result:='';
  for i:=0 to flibraries.count-1 do
    result:=result+TLibrary(libraries[i]).prettyNameLong+#13#10;
end;
function TLibraryManager.enumeratePrettyShortNames: string;
var i:integer;
begin
  result:='';
  for i:=0 to flibraries.count-1 do
    result:=result+TLibrary(libraries[i]).prettyNameShort+#13#10;
end;

function TLibraryManager.getLibraryFromEnumeration(location: string; pos: integer): TLibrary;
var
  i: Integer;
begin
  location := TLibrary.unpretty(location);
  location:='_'+location+'_';
  for i:=0 to flibraries.count-1 do
    if strContains(TLibrary(libraries[i]).id, location) then begin
      if pos = 0 then
        exit(TLibrary(libraries[i]));
      pos -= 1;
    end;
  raise ELibraryException.Create(Format(rsLibraryNotFound, [location, IntToStr(pos)]));
end;

function TLibraryManager.getLibraryFromEnumeration(const pos:integer):TLibrary;
begin
  Result:=TLibrary(flibraries[pos]);
end;
function TLibraryManager.getLibraryCountInEnumeration:integer;
begin
  result:=flibraries.count;
end;

procedure TLibraryManager.enumerateVariableValues(const varName: string;
  result: TStringList);
var i:longint;
begin
  result.clear;
  for i:=0 to flibraries.count-1 do
    if  result.IndexOf(TLibrary(libraries[i]).defaultVariables.Values[varName])<0 then
      result.add(TLibrary(libraries[i]).defaultVariables.Values[varName]);
end;

procedure TLibraryManager.enumerateLibrariesWithValue(const varName,
  value: string; result: TList);
var i:longint;
begin
  result.clear;
  for i:=0 to flibraries.count-1 do
    if TLibrary(libraries[i]).defaultVariables.Values[varName]=value then
      result.add(TLibrary(libraries[i]));
end;

function TLibraryManager.get(id: string): TLibrary;
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    if TLibrary(libraries[i]).id = id then exit(TLibrary(libraries[i]));
  exit(nil);
end;

function TLibraryManager.getUserLibraries: TList;
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
  next: Pointer;
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
    next := binarySearch(@flibraries.List[0], @flibraries.List[flibraries.Count-1], sizeof(pointer), @libraryLocationCompare, lib, bsFirst, [bsGreater]);
    if next = nil then flibraries.Insert(0, lib)
    else flibraries.Insert((next - @flibraries.List[0]) div sizeof(Pointer), lib)
  end;
  lib.template:=nil;
  lib.variables.Clear;
  lib.loadFromString(data, 'libraries/'+trueid+'.xml');

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
  lib: TLibrary;
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
    if (accounts[i].getLibrary().template = oldTemplate) and (accounts[i] is TTemplateAccountAccess) and (accounts[i].thread <> nil) then
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

procedure TLibraryManager.reloadPendingTemplates;
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
    lib := getLibraryFromEnumeration(i);
    if lib.template = old then begin
      lib.template := new;
      for j := 0 to accounts.Count - 1 do
        if (accounts[j].getLibrary() = lib) and (accounts[j] is TTemplateAccountAccess) and (accounts[j].thread = nil) then
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
      CopyFile(bookListOldFileName,bookListOldFileName+'.'+dateTimeFormat('yyyymmdd',currentDate));
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
begin
  result:=FConnected and (GetTickCount - FConnectingTime < Timeout);
end;

function TCustomAccountAccess.GetUpdated: boolean;
begin
  result:=GetConnected and FUpdated and (GetTickCount - FUpdateTime < Timeout);
end;

procedure TCustomAccountAccess.setPassword(AValue: string);
begin
  if pass=AValue then Exit;
  pass:=AValue;
  fconnected := false;
end;

function TCustomAccountAccess.getCharges: currency;
begin
  result:=fcharges;
end;

procedure TCustomAccountAccess.initFromConfig;
begin
//Datenladen/cachen
  pass:=config.ReadString('base','pass','');
  books.keepHistory:=config.ReadBool('base','keep-history',true);
  flastCheckDate:=config.ReadInteger('base','lastCheck',0);
  keepHistory:=config.readBool('base','keep-history',true);
  prettyName:=config.readString('base','prettyName', user);
  extendDays:=config.readInteger('base','extend-days',7);
  extendType:=TExtendType(config.readInteger('base','extend',0));
  fcharges:=currency(config.readInteger('base','charge',-100))/100;;
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

procedure TCustomAccountAccess.updateAll();
begin
  if not connected then
    if not connect(internet) then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

procedure TCustomAccountAccess.updateSingle(book: TBook);
begin
  if not connected then
    if not connect(internet) then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

procedure TCustomAccountAccess.extendAll();
begin
  if not connected then
    if not connect(internet) then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

procedure TCustomAccountAccess.extendList(bookList: TBookList);
begin
  if not connected then
    if not connect(internet) then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

{procedure TCustomAccountAccess.orderSingle(book: TBook);
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

procedure TCustomAccountAccess.orderList(booklist: TBookList);
begin
  if not connected then
    if not connect then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;}

procedure TCustomAccountAccess.cancelSingle(book: TBook);
begin
  if not connected then
    if not connect(internet) then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
end;

procedure TCustomAccountAccess.cancelList(booklist: TBookList);
begin
  if not connected then
    if not connect(internet) then
      raise ELibraryException.Create('Zugriff auf die Bücherei fehlgeschlagen'#13#10#13#10'Bitte überprüfen Sie Ihre Internetverbindung');
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

procedure TCustomAccountAccess.updateAllSingly;
var i:integer;
begin
  if not connected then
    if not connect(internet) then
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

  fconnected := false;
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

procedure TTemplateAccountAccess.setVariables;
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
end;

procedure TTemplateAccountAccess.resetlib;
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
  lastTodayUpdate:=GetTickCount;
  FConnectingTime:=GetTickCount;
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
  FConnectingTime:=GetTickCount;
  FUpdated:=true;
  FUpdateTime:=FConnectingTime;
  if logging then
    log('leave TTemplateAccountAccess.updateSingle');

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
  FConnectingTime:=GetTickCount;
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
var bookListStr:string;
    i:longint;
    extendAction: TTemplateAction;
    book:TBook;
    extendBookList: TXQValueSequence;
begin
  if booksToExtend.Count=0 then exit;
  if logging then log('Enter TTemplateAccountAccess.extendList');
  setVariables();
  extendAction:=reader.findAction('renew-list');
  if extendAction<>nil then begin
    if logging then log('use extendList Template');
    //if logging then log('bookList (count: '+inttostr(extendBookList.seq.count)+') is: '+extendBookList.debugAsStringWithTypeAnnotation());
    reader.parser.variableChangeLog.add('renew-books', xqvalueBookList(reader, booksToExtend));
    reader.callAction(extendAction);
  end else if reader.findAction('renew-single')<>nil then begin
    if logging then log('use renew-single Template');
    extendAction:=reader.findAction('renew-single');
    for i:=booksToExtend.count-1 downto 0 do begin
      reader.selectBook(booksToExtend[i]);
      book:=reader.books.findBook(booksToExtend[i]);
      if book<>nil then reader.selectBook(book); //handles more instances of the same book
      reader.callAction(extendAction);
    end;
  end else if reader.findAction('renew-all')<>nil then begin
    if logging then log('use renew-all Template');
    reader.callAction('renew-all');
  end;
  FConnectingTime:=GetTickCount;
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


  FConnectingTime:=GetTickCount;
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

procedure TTemplateAccountAccess.changeUser(const s: string);
begin
  inherited changeUser(s);
  reader.books:=books.bookLists[bltInCurrentDataUpdate];
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
  reader.parser.variableChangeLog.clear;
  Reader.parser.oldVariableChangeLog.clear;
  setVariables();
  reader.callAction('connect');
  fconnected:=true;
  FConnectingTime:=GetTickCount;
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
              (book.dueDate<=currentDate+extendDays));
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
    etAlways: //renew always (when there are books which can be extended)
      booksToExtendCount:=booksExtendableCount;
    etAllDepends,etSingleDepends: begin
      for i:=0 to reader.books.count-1 do //check for books to renew
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
