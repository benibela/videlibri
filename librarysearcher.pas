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
  template: TMultiPageTemplate;
  flocation:string;
  function GetSearchNextPageAvailable: boolean;
  procedure setHomeBranch(AValue: Integer);
  procedure setSearchBranch(AValue: Integer);
public
  bookListReader:TBookListReader;

  constructor create(searchTemplate: TMultiPageTemplate);
  destructor Destroy; override;
  
  procedure setLocation(s:string);
  function addLibrary(lib: tlibrary): boolean;
  procedure clear;
  
  procedure connect;
  procedure search;
  procedure searchNext;
  procedure details(book: tbook);
  function orderNeedsConfirmation(book: TBook): boolean;
  procedure orderConfirmSingle(book: tbook);
  procedure orderSingle(book: tbook);
  procedure disconnect;

  property Location: string read flocation write setLocation;
  property HomeBranch: Integer read fhomebranch write setHomeBranch;
  property SearchBranch: Integer read fsearchBranch write setSearchBranch;

  property HomeBranches: TStringArray read fhomeBranches;
  property SearchBranches: TStringArray read fsearchBranches;

  property SearchOptions: TBookSearchOptions read fsearchBook;
  property SearchResult: TBookList read fsearchResult;
  property SearchResultCount: integer read fsearchResultCount;
  property SearchNextPageAvailable: boolean read GetSearchNextPageAvailable ;
end;

implementation
uses internetAccess;

{ TLibrarySearcher }

function TLibrarySearcher.GetSearchNextPageAvailable: boolean;
begin
  result := bookListReader.parser.variableChangeLog.get('search-next-page-available').toBooleanEffective;
end;

procedure TLibrarySearcher.setHomeBranch(AValue: integer);
begin
  if fhomeBranch=AValue then Exit;
  fhomeBranch:=AValue;
  bookListReader.parser.variableChangeLog.add('home-branch-id', AValue);
end;

procedure TLibrarySearcher.setSearchBranch(AValue: integer);
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

  SetLength(fhomebranches, 0);
  SetLength(fsearchBranches, 0);
  connectAction := bookListReader.findAction('search-connect');
  if connectAction <> nil then begin
    bookListReader.callAction(connectAction);

    temp := bookListReader.parser.variableChangeLog.get('home-branches');
    if temp.getSequenceCount > 0 then begin
      SetLength(fhomebranches, temp.getSequenceCount+1);
      fhomebranches[0] := '(Standard)';
      for i:=1 to temp.getSequenceCount do
        fhomebranches[i] := temp.getChild(i).toString;
    end;

    temp := bookListReader.parser.variableChangeLog.get('search-branches');
    if temp.getSequenceCount > 0 then begin
      SetLength(fsearchBranches, temp.getSequenceCount+1);
      fsearchBranches[0] := '(Standard)';
      for i:=1 to temp.getSequenceCount do
        fsearchBranches[i] := temp.getChild(i).toString;
    end;
  end;
end;

procedure TLibrarySearcher.search;
begin
  fsearchResult.clear;
  bookListReader.books:=fsearchResult;
  bookListReader.selectBook(SearchOptions);
  bookListReader.parser.variableChangeLog.add('search-next-page-available', false);
  bookListReader.callAction('search');
  fsearchResultCount := bookListReader.parser.variableChangeLog.get('search-result-count').toInt64;
end;

procedure TLibrarySearcher.searchNext;
begin
  bookListReader.parser.variableChangeLog.add('search-next-page-available', false);
  bookListReader.callAction('search-next-page');
  fsearchResultCount := bookListReader.parser.variableChangeLog.get('search-result-count').toInt64;
end;

procedure TLibrarySearcher.details(book: tbook);
begin
  bookListReader.selectBook(book);
  bookListReader.callAction('search-details');
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
end;

procedure TLibrarySearcher.orderSingle(book: tbook);
begin
  bookListReader.selectBook(book);
  if book.owner is TTemplateAccountAccess then TTemplateAccountAccess(book.owner).setVariables(bookListReader.parser);
  bookListReader.callAction('order-single');
end;

procedure TLibrarySearcher.disconnect;
begin
end;

end.
