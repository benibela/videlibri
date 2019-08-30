unit librarySearcher;

{$mode objfpc}{$H+}{$ModeSwitch autoderef}

interface

uses
  Classes, SysUtils, booklistreader, libraryParser, multipagetemplate, xquery, bbutils, commoninterface;

type
TBookSearchOptions = class(TBook);

{ TLibrarySearcher }

TLibrarySearcher = class
private
  flibsToSearch: TList;
  fsearchBook: TBookSearchOptions;
  FSearchNextPageAvailable: boolean;
  fsearchResult: TBookList;
  fsearchResultCount: integer;
  FConnected: boolean;
  FTimeout, FLastAccessTime: QWORD;
  template: TMultiPageTemplate;
  fsearchparams: TFormParams;
  function GetConnected: boolean;
  procedure setSearchParams(AValue: TFormParams);
  procedure updateAccessTimeout;
public
  bookListReader:TBookListReader;

  constructor create(searchTemplate: TMultiPageTemplate);
  destructor Destroy; override;
  
  function addLibrary(lib: tlibrary): boolean;
  function getLibraryIds: string;
  function getCacheFile: string;
  procedure clear;
  
  procedure connect;
  procedure search;
  procedure searchNext;
  procedure details(book: tbook);
  procedure orderSingle(book: tbook);
  procedure completePendingMessage(pm: TPendingMessage; idx: integer);
  procedure disconnect;


  property Connected: boolean read GetConnected;
  property Timeout: qword read FTimeout write FTimeout;

  property SearchParams: TFormParams read fsearchParams write setSearchParams;
  property SearchOptions: TBookSearchOptions read fsearchBook;
  property SearchResult: TBookList read fsearchResult;
  property SearchResultCount: integer read fsearchResultCount;
  property SearchNextPageAvailable: boolean read FSearchNextPageAvailable ;
end;

var defaultSearchParams: TFormParams;
implementation
uses internetAccess, bookproperties, applicationconfig;

resourcestring
  //rsDefault = 'Standard';
  rsKeywords = 'Stichworte';
  rsLanguage = 'Sprache';
  rsSort = 'Sortierung';
  rsMediaType = 'Medientyp';
  rsFree = 'Freitext';

type TFormParamsHelper = class helper for TFormParams
  procedure setCaptions;
  class function createFromXQValue(const v: IXQValue): TFormParams; static;
private
  class function createDefault: TFormParams; static;
end;

procedure TFormParamsHelper.setCaptions;
var
  i: SizeInt;
begin
  for i := 0 to high(inputs) do
    with inputs[i] do
      if (caption = '') and (name <> '') then begin
        caption := getBookPropertyPretty(name);
        if caption = '' then
          case name of
            'keywords': caption := rsKeywords;
            'sort': caption := rsSort;
            'mediaType': caption := rsMediaType;
            'language': caption := rsLanguage;
            'free': caption := rsFree;
          end;
        if caption <> '' then caption := caption + ': ';
      end;
end;

class function TFormParamsHelper.createDefault: TFormParams;
begin
  result := TFormParams.create;
  with result do begin
    SetLength(inputs, 5);
    inputs[0] := TFormInput.Create; inputs[0].name := 'title';
    inputs[1] := TFormInput.Create; inputs[1].name := 'author';
    inputs[2] := TFormInput.Create; inputs[2].name := 'keywords';
    inputs[3] := TFormInput.Create; inputs[3].name := 'year';
    inputs[4] := TFormInput.Create; inputs[4].name := 'isbn';
    setCaptions;
  end;
end;

class function TFormParamsHelper.createFromXQValue(const v: IXQValue): TFormParams;
var
  pv, pw: PIXQValue;
  fi: TFormInput;
  fs: TFormSelect;
  options, value: IXQValue;
  i,j : SizeInt;
begin
  result := TFormParams.Create;
  with result do begin
    SetLength(inputs, v.Count);
    i := 0;
    for pv in v.GetEnumeratorPtrUnsafe do begin
      case pv.kind of
        pvkString: begin
          fi := TFormInput.Create;
          fi.name := pv.toString;
        end;
        pvkObject: begin
          options := pv.getProperty('options');
          value := pv.getProperty('value');
          if options.isUndefined then fi := TFormInput.Create
          else begin
            fs := TFormSelect.Create;
            fi := fs;
            SetLength(fs.optionCaptions, options.Count);
            SetLength(fs.optionValues, options.Count);
            j := 0;
            for pw in options.GetEnumeratorPtrUnsafe do begin
              case pw.kind of
                pvkString: {todo};
                pvkObject: begin
                  fs.optionCaptions[j] := pw.getProperty('name').toString;
                  fs.optionValues[j] := pw.getProperty('value').toString;
                end;
                pvkNode: begin
                  fs.optionCaptions[j] := pw.toNode.innerText();
                  fs.optionValues[j] := pw.toNode.getAttribute('value');
                  if value.isUndefined and (fi.value = '') and (pw.toNode.getAttribute('selected') <> '') then
                    fi.value := fs.optionValues[j];
                end;
                else raise EVideLibriInterfaceException.Create('Invalid option: '+pw.toXQuery);
              end;
              inc(j);
            end;
          end;
          fi.name := pv.getProperty('name').toString;
          fi.caption := pv.getProperty('caption').toString;
          if fi.value = '' then fi.value := pv.getProperty('value').toString;
        end;
        else raise EVideLibriInterfaceException.Create('Invalid form input: '+pv.toXQuery);
      end;
      inputs[i] := fi;
      inc(i);
    end;
    setCaptions;
  end;
end;

function TLibrarySearcher.GetConnected: boolean;
var
  tempTime: QWord;
begin
  tempTime := GetTickCount64;
  result:=FConnected and (tempTime >= FLastAccessTime) and (tempTime - FLastAccessTime < Timeout);
end;

procedure TLibrarySearcher.setSearchParams(AValue: TFormParams);
begin
  if fsearchParams=AValue then Exit;
  EnterCriticalSection(bookListReader.bookAccessSection^);
  avalue._AddRef;
  fsearchparams._ReleaseIfNonNil;
  fsearchparams := AValue;
  LeaveCriticalSection(bookListReader.bookAccessSection^);
end;

procedure TLibrarySearcher.updateAccessTimeout;
begin
  FLastAccessTime := GetTickCount64;
end;

constructor TLibrarySearcher.create(searchTemplate: TMultiPageTemplate);
begin
  fsearchBook:=TBookSearchOptions.create;
  flibsToSearch:=TList.Create;
  fsearchResult:=TBookList.Create;
  template:=searchTemplate;

  bookListReader:=TBookListReader.create(template);
  bookListReader.internet:=defaultInternetAccessClass.create();

  FTimeout:=10*60*1000;
  FConnected:=false;

  fsearchparams := defaultSearchParams;
  fsearchparams._AddRef;
end;

destructor TLibrarySearcher.Destroy;
begin
  bookListReader.internet.free;
  bookListReader.free;
  fsearchResult.free;
  flibsToSearch.Free;
  fsearchBook.Free;
  fsearchparams._ReleaseIfNonNil;
  inherited Destroy;
end;

function xqvalueSeqAppend(const v1, v2: ixqvalue): ixqvalue;
var
  templist: TXQVList;
begin
  templist := TXQVList.create(2);
  templist.add(v1);
  templist.add(v2);
  xqvalueSeqSqueezed(result, templist);
end;

function TLibrarySearcher.addLibrary(lib: tlibrary): boolean;
var
  j: Integer;
  vars: TXQVariableChangeLog;
  value: ixqvalue;
  name, newvalue: String;
begin
  flibsToSearch.Add(lib);
  vars := bookListReader.parser.variableChangeLog;
  for j := 0 to lib.variables.Count-1 do begin
    name := lib.variables.Names[j];
    newvalue := lib.variables.ValueFromIndex[j];
    if vars.hasVariable(name, value) then vars.add(name, xqvalueSeqAppend(value, xqvalue(newvalue)))
    else vars.ValuesString[name] := newvalue;
  end;
  WriteBarrier;
  Result:=true;
end;

function TLibrarySearcher.getLibraryIds: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to flibsToSearch.Count - 1 do
    result := result + TLibrary(flibsToSearch[i]).id;
end;

function TLibrarySearcher.getCacheFile: string;
begin
  result := userPath + '/' + getLibraryIds + '.cache';
end;

procedure TLibrarySearcher.clear;
begin
  flibsToSearch.Clear;
  fsearchResult.clear;
  fsearchBook.clear;
  bookListReader.parser.variableChangeLog.clear;
  bookListReader.parser.oldVariableChangeLog.clear;
end;


procedure TLibrarySearcher.connect;
var
  connectAction: TTemplateAction;
  temp: IXQValue;
  newsearchparams: TFormParams;
begin
  assert(Assigned(bookListReader.bookAccessSection));
  connectAction := bookListReader.findAction('search-connect');
  if connectAction <> nil then begin
    bookListReader.callAction(connectAction);

    temp := bookListReader.parser.variableChangeLog.get('search-params');
    if temp.Count > 0 then newsearchparams := TFormParams.createFromXQValue(temp)
    else newsearchparams := defaultSearchParams;
    SearchParams := newsearchparams;
    if fsearchparams <> defaultSearchParams then
      strSaveToFileUTF8(getCacheFile, '{"search-params": ' + fsearchparams.toJSON() + '}');
  end;
  FConnected:=true;
  updateAccessTimeout;
end;

procedure TLibrarySearcher.search;
begin
  fsearchResult.clear;
  bookListReader.books:=fsearchResult;
  bookListReader.selectBook(SearchOptions);
  bookListReader.parser.variableChangeLog.add('search-next-page-available', false);
  bookListReader.callAction('search');
  FSearchNextPageAvailable := bookListReader.parser.variableChangeLog.get('search-next-page-available').toBooleanEffective;
  fsearchResultCount := bookListReader.parser.variableChangeLog.get('search-result-count').toInt64;
  updateAccessTimeout;
end;

procedure TLibrarySearcher.searchNext;
begin
  bookListReader.parser.variableChangeLog.add('search-next-page-available', false);
  bookListReader.callAction('search-next-page');
  FSearchNextPageAvailable := bookListReader.parser.variableChangeLog.get('search-next-page-available').toBooleanEffective;
  fsearchResultCount := bookListReader.parser.variableChangeLog.get('search-result-count').toInt64;
  updateAccessTimeout;
end;

procedure TLibrarySearcher.details(book: tbook);
var
  i: Integer;
  j: Integer;
begin
  bookListReader.selectBook(book);
  bookListReader.callAction('search-details');
  if book.isbn = '' then
    for i := 0 to high(book.additional) do
      if striEqual(book.additional[i].name, 'ISBN!') or striEqual(book.additional[i].name, 'ISSN!') then begin
        book.isbn := book.additional[i].value;
        for j := i+1 to high(book.additional) do
          book.additional[j-1] := book.additional[j];
        setlength(book.additional, length(book.additional)-1);
        exit;
      end;
  updateAccessTimeout;
end;

procedure TLibrarySearcher.orderSingle(book: tbook);
var
  owningBook: TBook;
begin
  owningBook := book.owningBook;
  if book <> owningBook then begin
    bookListReader.parser.variableChangeLog.add('holding', TBookListReader.bookToPXP(book));
    book := owningBook;
  end;
  bookListReader.selectBook(book);
  if assigned(book.owningAccount) and book.owningAccount.InheritsFrom(TTemplateAccountAccess) then TTemplateAccountAccess(book.owningAccount).setVariables(bookListReader.parser);
  bookListReader.callAction('order-single');
  updateAccessTimeout;
end;

procedure TLibrarySearcher.completePendingMessage(pm: TPendingMessage; idx: integer);
begin
  case pm.kind of
    pmkConfirm: bookListReader.parser.variableChangeLog.add('confirm-result', idx > 0);
    pmkChoose:
      if (idx >= 0) and (idx <= high(pm.optionValues)) then bookListReader.parser.variableChangeLog.add('choose-result', pm.optionValues[idx])
      else bookListReader.parser.variableChangeLog.add('choose-result', idx + 1 {xpath uses 1-based indices});
  end;
  bookListReader.callAction(pm.callback);
  pm.free;
  updateAccessTimeout
end;

procedure TLibrarySearcher.disconnect;
begin
  FConnected:=false;
end;

initialization
  defaultSearchParams := TFormParams.createDefault;
  defaultSearchParams._AddRef;
finalization
  defaultSearchParams._Release;
end.

