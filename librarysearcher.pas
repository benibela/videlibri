unit librarySearcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, booklistreader, libraryParser, multipagetemplate, xquery, bbutils;

type
TBookSearchOptions = class(TBook);

{ TLibrarySearcher }

TLibrarySearcher = class
private
  fhomeBranch: integer;
  fhomeBranches: TStringArray;
  fsearchBranch: integer;
  fsearchBranches: TStringArray;
  flibsToSearch: TList;
  fsearchBook: TBookSearchOptions;
  FSearchNextPageAvailable: boolean;
  fsearchResult: TBookList;
  fsearchResultCount: integer;
  FConnected: boolean;
  FTimeout, FLastAccessTime: QWORD;
  template: TMultiPageTemplate;
  flocation:string;
  function GetConnected: boolean;
  procedure updateAccessTimeout;
  procedure setHomeBranch(AValue: Integer);
  procedure setSearchBranch(AValue: Integer);
public
  bookListReader:TBookListReader;

  constructor create(searchTemplate: TMultiPageTemplate);
  destructor Destroy; override;
  
  function addLibrary(lib: tlibrary): boolean;
  function getLibraryIds: string;
  procedure clear;
  
  procedure connect;
  procedure search;
  procedure searchNext;
  procedure details(book: tbook);
  procedure orderSingle(book: tbook);
  procedure completePendingMessage(pm: TPendingMessage; idx: integer);
  procedure disconnect;

  property HomeBranch: Integer read fhomebranch write setHomeBranch;
  property SearchBranch: Integer read fsearchBranch write setSearchBranch;

  property Connected: boolean read GetConnected;
  property Timeout: qword read FTimeout write FTimeout;

  property HomeBranches: TStringArray read fhomeBranches write fhomeBranches;
  property SearchBranches: TStringArray read fsearchBranches write fsearchBranches;

  property SearchOptions: TBookSearchOptions read fsearchBook;
  property SearchResult: TBookList read fsearchResult;
  property SearchResultCount: integer read fsearchResultCount;
  property SearchNextPageAvailable: boolean read FSearchNextPageAvailable ;
end;

implementation
uses internetAccess;

resourcestring
  rsDefault = 'Standard';

function TLibrarySearcher.GetConnected: boolean;
var
  tempTime: QWord;
begin
  tempTime := GetTickCount64;
  result:=FConnected and (tempTime >= FLastAccessTime) and (tempTime - FLastAccessTime < Timeout);
end;

procedure TLibrarySearcher.updateAccessTimeout;
begin
  FLastAccessTime := GetTickCount64;
end;

procedure TLibrarySearcher.setHomeBranch(AValue: Integer);
begin
  if fhomeBranch=AValue then Exit;
  fhomeBranch:=AValue;
  bookListReader.parser.variableChangeLog.add('home-branch-id', AValue);
end;

procedure TLibrarySearcher.setSearchBranch(AValue: Integer);
begin
  if fsearchBranch=AValue then Exit;
  fsearchBranch:=AValue;
  bookListReader.parser.variableChangeLog.add('search-branch-id', AValue);
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
end;

destructor TLibrarySearcher.Destroy;
begin
  bookListReader.internet.free;
  bookListReader.free;
  fsearchResult.free;
  flibsToSearch.Free;
  fsearchBook.Free;
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
  i: Integer;
  connectAction: TTemplateAction;
  temp: IXQValue;
begin
  assert(Assigned(bookListReader.bookAccessSection));
  connectAction := bookListReader.findAction('search-connect');
  if connectAction <> nil then begin
    bookListReader.callAction(connectAction);

    EnterCriticalSection(bookListReader.bookAccessSection^);
    try
      temp := bookListReader.parser.variableChangeLog.get('home-branches');
      if temp.getSequenceCount > 0 then begin
        SetLength(fhomebranches, temp.getSequenceCount+1);
        fhomebranches[0] := '(' + rsDefault + ')';
        for i:=1 to temp.getSequenceCount do
          fhomebranches[i] := temp.get(i).toString;
      end;

      temp := bookListReader.parser.variableChangeLog.get('search-branches');
      if temp.getSequenceCount > 0 then begin
        SetLength(fsearchBranches, temp.getSequenceCount+1);
        fsearchBranches[0] :='(' + rsDefault + ')';
        for i:=1 to temp.getSequenceCount do
          fsearchBranches[i] := temp.get(i).toString;
      end;
    finally
      LeaveCriticalSection(bookListReader.bookAccessSection^);
    end;
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

end.

