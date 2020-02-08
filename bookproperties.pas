unit bookproperties;

{$mode objfpc}{$H+}

interface

const
  defaultBookListViewLendColumns: array[0..11] of string = (
    'id',
    'category',
    'author',
    'title',
    'year',
    'issueDate',
    'dueDate',
    '?account',
    'status',
    'isbn',
    'libraryBranch',
    'renewCount'
  );

  defaultBookListViewSearchColumns: array[0..3] of string = (
    'author',
    'title',
    'year',
    'status'
  );

  defaultDetailsProperties: array[0..8] of string = (
    'id',
    'barcode',
    'category',
    'author',
    'title',
    'year',
    'isbn',
    'publisher',
    'location'
  );

  defaultHoldingColumns: array[0..10] of string = (
    'id',
    'barcode',
    'category',
    'publisher',
    'author',
    'title',
    'year',
    'libraryBranch',
    'libraryLocation',
    'status',
    'pendingOrders'
  );



resourcestring
  rsBookPropertyNone = 'Keine';
  rsBookPropertyID = 'ID';
  rsBookPropertyCategory = 'Kategorie';
  rsBookPropertyAuthor = 'Verfasser';
  rsBookPropertyTitle = 'Titel';
  rsBookPropertyYear = 'Jahr';
  rsBookPropertyIssueDate = 'Ausleihe';
  rsBookPropertyLimitDate = 'Frist';
  rsBookPropertyLibraryBranch = 'Zweigstelle';
  rsBookPropertyAccount = 'Konto';
  rsBookPropertyStatusComment = 'Bemerkung';
  rsBookPropertyISBN = 'ISBN';
  rsBookPropertyFirstExistsDate = 'bekannt von';
  rsBookPropertyLastExistsDate = 'bekannt bis';

resourcestring
  rsBookPropertyLimitWeek = 'Fristwoche';
  rsBookPropertyIssueWeek = 'Ausleihwoche';

resourcestring
  rsBookPropertyLocationLend = 'Ausleihbibliothek';
  rsBookPropertyLocationSearch = 'Ort';
  rsBookPropertyStatus = 'Ausleihstatus';
  rsBookPropertyPublisher = 'Verlag';
  rsBookPropertyRenewCount = 'Verlängerungen';
  rsBookPropertyLibraryLocation = 'Standort';
  rsBookPropertyBarcode = 'Mediennummer';
  rsBookPropertyPendingOrders = 'Vorbestellungen';


function getBookPropertyPretty(const s: string): string;
function getBookPropertyDefaultWidth(const s: string): integer;



const
  groupingPropertyNames: array[0..15] of string = (rsBookPropertyNone, rsBookPropertyAccount,
                                                   rsBookPropertyLimitWeek, rsBookPropertyLimitDate, rsBookPropertyStatusComment, rsBookPropertyRenewCount,
                                                   rsBookPropertyIssueWeek, rsBookPropertyIssueDate,
                                                   rsBookPropertyLocationLend, rsBookPropertyLibraryLocation,
                                                   rsBookPropertyID, rsBookPropertyBarcode, rsBookPropertyCategory, rsBookPropertyTitle, rsBookPropertyAuthor, rsBookPropertyYear);
  groupingPropertyMap: array[0..15] of string = ('', '_account',
                                                 '_dueWeek', 'dueDate', '_status', 'renewCount',
                                                 '_issueWeek', 'issueDate',
                                                 'libraryBranch', 'libraryLocation',
                                                 'id', 'barcode', 'category', 'title', 'author', 'year');

var
  translationMap: array[0..18] of record
    key, text: string;
    defaultWidth: UIntPtr;
  end = (
  (key: 'id'; text: rsBookPropertyID; defaultWidth: 80),
  (key: 'category'; text: rsBookPropertyCategory; defaultWidth: 50),
  (key: 'author'; text: rsBookPropertyAuthor; defaultWidth: 120),
  (key: 'title'; text: rsBookPropertyTitle; defaultWidth: 150),
  (key: 'year'; text: rsBookPropertyYear; defaultWidth: 30),
  (key: 'issueDate'; text: rsBookPropertyIssueDate; defaultWidth: 70),
  (key: 'dueDate'; text: rsBookPropertyLimitDate; defaultWidth: 70),
  (key: 'libraryBranch'; text: rsBookPropertyLibraryBranch; defaultWidth: 40),
  (key: 'renewCount'; text: rsBookPropertyRenewCount; defaultWidth: 25),
  (key: '?account'; text: rsBookPropertyAccount; defaultWidth: 80),
  (key: 'status'; text: rsBookPropertyStatusComment; defaultWidth: 250),
  (key: 'isbn'; text: rsBookPropertyISBN; defaultWidth: 80),
  (key: '_firstExistsDate'; text: rsBookPropertyFirstExistsDate; defaultWidth: 70),
  (key: '_lastExistsDate'; text: rsBookPropertyLastExistsDate; defaultWidth: 70),

  (key: 'publisher'; text: rsBookPropertyPublisher; defaultWidth: 50),
  (key: 'location'; text: rsBookPropertyLocationSearch; defaultWidth: 50),
  (key: 'pendingOrders'; text: rsBookPropertyPendingOrders; defaultWidth: 30),

  (key: 'barcode'; text: rsBookPropertyBarcode; defaultWidth: 30),
  (key: 'libraryLocation'; text: rsBookPropertyLibraryLocation; defaultWidth: 40)

  );

implementation

uses contnrs, bbutils;


var translationHash: TFPStringHashTable;
    widthHash: TFPDataHashTable;

function getBookPropertyPretty(const s: string): string;
var
  i: Integer;
  lkey: String;
begin
  if translationHash = nil then begin
    translationHash := TFPStringHashTable.CreateWith(137, @RSHash);
    for i := 0 to high(translationMap) do begin
      lkey := translationMap[i].key.ToLowerInvariant;
      translationHash.Add(lkey, translationMap[i].text);
      if lkey.StartsWith('?') or lkey.StartsWith('_') then
        translationHash.Add(lkey.RemoveFromLeft(1), translationMap[i].text);
    end;
  end;
  result := translationHash[s.ToLowerInvariant];
end;

function getBookPropertyDefaultWidth(const s: string): integer;
var
  i: Integer;
  lkey: String;
begin
  if widthHash = nil then begin
    widthHash := TFPDataHashTable.CreateWith(137, @RSHash);
    for i := 0 to high(translationMap) do begin
      lkey := translationMap[i].key.ToLowerInvariant;
      widthHash.Add(lkey, UIntToPtr(translationMap[i].defaultWidth));
      if lkey.StartsWith('?') or lkey.StartsWith('_') then
        widthHash.Add(lkey.RemoveFromLeft(1), UIntToPtr(translationMap[i].defaultWidth));
    end;
  end;
  result := PtrToUInt(widthHash[s.ToLowerInvariant]);
end;

end.

