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
  FTimeout, FLastAccessTime: DWORD;
  template: TMultiPageTemplate;
  flocation:string;
  function GetConnected: boolean;
  procedure setHomeBranch(AValue: Integer);
  procedure setSearchBranch(AValue: Integer);
public
  bookListReader:TBookListReader;

  constructor create(searchTemplate: TMultiPageTemplate);
  destructor Destroy; override;
  
  procedure setLocation(s:string);
  function addLibrary(lib: tlibrary): boolean;
  function getLibraryIds: string;
  procedure clear;
  
  procedure connect;
  procedure search;
  procedure searchNext;
  procedure details(book: tbook);
  function orderNeedsConfirmation(book: TBook): boolean;
  procedure orderConfirmSingle(book: tbook);
  procedure orderSingle(book: tbook);
  procedure completePendingMessage(pm: TPendingMessage; idx: integer);
  procedure disconnect;

  property Location: string read flocation write setLocation;
  property HomeBranch: Integer read fhomebranch write setHomeBranch;
  property SearchBranch: Integer read fsearchBranch write setSearchBranch;

  property Connected: boolean read GetConnected;
  property Timeout: dword read FTimeout write FTimeout;

  property HomeBranches: TStringArray read fhomeBranches;
  property SearchBranches: TStringArray read fsearchBranches;

  property SearchOptions: TBookSearchOptions read fsearchBook;
  property SearchResult: TBookList read fsearchResult;
  property SearchResultCount: integer read fsearchResultCount;
  property SearchNextPageAvailable: boolean read FSearchNextPageAvailable ;
end;

implementation
uses internetAccess, LCLIntf;

resourcestring
  rsDefault = 'Standard';

function TLibrarySearcher.GetConnected: boolean;
begin
  result:=FConnected and (GetTickCount - FLastAccessTime < Timeout);
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

procedure TLibrarySearcher.setLocation(s: string);
var
  temp: String;
begin
  flocation:=s;
  if pos('digibib', s) > 0 then s := copy(s, 1, pos(' ', s)-1);
  temp := template.findVariableValue('location:'+s);
  if temp <> '' then
    bookListReader.parser.variableChangeLog.ValuesString['location']:=temp;
  temp := template.findVariableValue('view:'+s);
  if temp <> '' then
    bookListReader.parser.variableChangeLog.ValuesString['view']:=temp;
  {if (s <> template.name) //not a standard search template
      and (bookListReader.parser.variableChangeLog.ValuesString['location']='') //not a digibib template
      and (template.findAction('update-all') = nil) and (template.findAction('update-single') = nil) //and not a account template
      then
    raise Exception.Create('Unbekannter Suchort');}
end;

function TLibrarySearcher.addLibrary(lib: tlibrary): boolean;
var
  j: Integer;
begin
  flibsToSearch.Add(lib);
  for j := 0 to lib.variables.Count-1 do
    bookListReader.parser.variableChangeLog.ValuesString[lib.variables.Names[j]] := lib.variables.ValueFromIndex[j];
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
var selectedLibraries: string;
  i: Integer;
  j: Integer;
  connectAction: TTemplateAction;
  temp: IXQValue;
begin
  selectedLibraries:='';
  for i:=0 to flibsToSearch.count-1 do
    selectedLibraries+=TLibrary(flibsToSearch[i]).variables.values['searchid-'+template.Name]+template.findVariableValue('after-id');
  bookListReader.parser.variableChangeLog.ValuesString['selectedLibraries']:=selectedLibraries;

  assert(Assigned(bookListReader.bookAccessSection));
  EnterCriticalSection(bookListReader.bookAccessSection^);
  SetLength(fhomebranches, 0);
  SetLength(fsearchBranches, 0);
  LeaveCriticalSection(bookListReader.bookAccessSection^);

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
  FLastAccessTime:=GetTickCount;
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
  FLastAccessTime:=GetTickCount;
end;

procedure TLibrarySearcher.searchNext;
begin
  bookListReader.parser.variableChangeLog.add('search-next-page-available', false);
  bookListReader.callAction('search-next-page');
  FSearchNextPageAvailable := bookListReader.parser.variableChangeLog.get('search-next-page-available').toBooleanEffective;
  fsearchResultCount := bookListReader.parser.variableChangeLog.get('search-result-count').toInt64;
  FLastAccessTime:=GetTickCount;
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
  FLastAccessTime:=GetTickCount;
end;

function TLibrarySearcher.orderNeedsConfirmation(book: TBook): boolean;
begin
  result := bookListReader.findAction('order-confirm-single') <> nil;
end;

procedure TLibrarySearcher.orderConfirmSingle(book: tbook);
begin
  bookListReader.selectBook(book);
  if book.owner is TTemplateAccountAccess then TTemplateAccountAccess(book.owner).setVariables(bookListReader.parser);
  bookListReader.callAction('order-confirm-single');
  FLastAccessTime:=GetTickCount;
end;

procedure TLibrarySearcher.orderSingle(book: tbook);
begin
  bookListReader.selectBook(book);
  if book.owner is TTemplateAccountAccess then TTemplateAccountAccess(book.owner).setVariables(bookListReader.parser);
  bookListReader.callAction('order-single');
  FLastAccessTime:=GetTickCount;
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
  FLastAccessTime:=GetTickCount;
end;

procedure TLibrarySearcher.disconnect;
begin
  FConnected:=false;
end;

end.

