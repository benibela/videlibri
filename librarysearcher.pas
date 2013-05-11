unit librarySearcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, booklistreader, libraryParser, multipagetemplate;

type
TBookSearchOptions = class(TBook);

{ TLibrarySearcher }

TLibrarySearcher = class
private
  flibsToSearch: TList;
  fsearchBook: TBookSearchOptions;
  fsearchResult: TBookList;
  template: TMultiPageTemplate;
  flocation:string;
public
  bookListReader:TBookListReader;

  constructor create(searchTemplate: TMultiPageTemplate);
  destructor Destroy; override;
  
  procedure setLocation(s:string);
  function addLibrary(lib: tlibrary): boolean;
  procedure clear;
  
  procedure connect;
  procedure search;
  procedure details(book: tbook);
  procedure disconnect;

  property Location: string read flocation write setLocation;

  property SearchOptions: TBookSearchOptions read fsearchBook;
  property SearchResult: TBookList read fsearchResult;
end;

implementation
uses internetAccess;

{ TLibrarySearcher }

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
begin
  flocation:=s;
  if pos('digibib', s) > 0 then s := copy(s, 1, pos(' ', s)-1);
  bookListReader.parser.variableChangeLog.ValuesString['location']:=template.findVariableValue('location:'+s);
  bookListReader.parser.variableChangeLog.ValuesString['view']:=template.findVariableValue('view:'+s);
  if (s <> template.name) //not a standard search template
      and (bookListReader.parser.variableChangeLog.ValuesString['location']='') //not a digibib template
      and (template.findAction('update-all') = nil) and (template.findAction('update-single') = nil) //and not a account template
      then
    raise Exception.Create('Unbekannter Suchort');
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
begin
  selectedLibraries:='';
  for i:=0 to flibsToSearch.count-1 do
    selectedLibraries+=TLibrary(flibsToSearch[i]).variables.values['searchid-'+template.Name]+template.findVariableValue('after-id');
  bookListReader.parser.variableChangeLog.ValuesString['selectedLibraries']:=selectedLibraries;


  bookListReader.callAction('connect');
end;

procedure TLibrarySearcher.search;
begin
  fsearchResult.clear;
  bookListReader.books:=fsearchResult;
  bookListReader.selectBook(SearchOptions);
  bookListReader.callAction('search');
end;

procedure TLibrarySearcher.details(book: tbook);
begin
  bookListReader.selectBook(book);
  bookListReader.callAction('search-details');
end;

procedure TLibrarySearcher.disconnect;
begin
end;

end.
