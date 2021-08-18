unit jsonscannerhelper;

{$mode objfpc}{$H+}{$ModeSwitch typehelpers}{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,fastjsonscanner;

type
EJSONScannerHelperException = class(Exception);
TReadObject = function (var scanner: TJSONScanner; const typeId: string): TObject of object;
TJSONScannerHelper = record helper for TJSONScanner
  procedure expectCurrentToken(token: TJSONToken);
  function fetchNonWSToken: TJSONToken;
  function fetchExpectedToken(token: TJSONToken): TJSONToken;
  function fetchStringObjectPropertyValue: string;
  function fetchSerializedTypedObject(callback: TReadObject): TObject; //['type', {obj}]
  procedure skipObjectPropertyValue;
  function CurTokenString: string;
  procedure raiseUnexpectedError();
end;

implementation
uses math;

procedure TJSONScannerHelper.raiseUnexpectedError();
var
  at: String;
begin
  at := Copy(CurLine, CurColumn - 30, min(CurColumn - 1, 30) ) + '|' + Copy(CurLine, CurColumn, 30 ) ;
  raise EJSONScannerHelperException.create('Konfigurationsdatei konnte nicht geladen werden. Unerwartetes Symbol hier: '+at);
end;

procedure TJSONScannerHelper.expectCurrentToken(token: TJSONToken);
begin
  if CurToken <> token then
    raiseUnexpectedError();
end;

function TJSONScannerHelper.fetchNonWSToken: TJSONToken;
begin
  repeat
    result := FetchToken;
  until result <> tkWhitespace;
end;

function TJSONScannerHelper.fetchExpectedToken(token: TJSONToken): TJSONToken;
begin
  result := fetchNonWSToken;
  expectCurrentToken(token);
end;

function TJSONScannerHelper.fetchStringObjectPropertyValue: string;
begin
  fetchNonWSToken; expectCurrentToken(tkColon);
  fetchNonWSToken; expectCurrentToken(tkString);
  result := CurTokenString;
  fetchNonWSToken;
end;

function TJSONScannerHelper.fetchSerializedTypedObject(callback: TReadObject): TObject;
var additionalArray: boolean;
  typeId: String;
begin
  additionalArray := curtoken = tkSquaredBraceOpen;
  if additionalArray  then fetchNonWSToken;
  expectCurrentToken(tkString);
  typeId := CurTokenString;
  fetchNonWSToken; expectCurrentToken(tkComma);
  fetchNonWSToken;
  result := callback(self, typeId);
  if additionalArray then begin
    fetchNonWSToken; expectCurrentToken(tkSquaredBraceClose);
  end;
end;

procedure TJSONScannerHelper.skipObjectPropertyValue;
var nesting: sizeint;
begin
  fetchExpectedToken(tkColon);
  fetchNonWSToken;
  case CurToken of
    tkString: ;
    tkSquaredBraceOpen: begin
      nesting := 1;
      while nesting > 0 do
        case fetchNonWSToken of
          tkSquaredBraceOpen: inc(nesting);
          tkSquaredBraceClose: dec(nesting);
          tkEOF: exit;
          else ;
        end;
    end;
    else expectCurrentToken(tkString);
  end;
  fetchNonWSToken;
end;

function TJSONScannerHelper.CurTokenString: string;
begin
  result := decodeJSONString(CurTokenStart, CurTokenLength, jecEscapeNothing, nil);
end;

end.

