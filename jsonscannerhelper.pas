unit jsonscannerhelper;

{$mode objfpc}{$H+}{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils,jsonscanner;

type
EJSONScannerHelperException = class(Exception);
TJSONScannerHelper = class helper for TJSONScanner
  procedure expectCurrentToken(token: TJSONToken);
  function fetchNonWSToken: TJSONToken;
  function fetchExpectedToken(token: TJSONToken): TJSONToken;
  procedure skipObjectPropertyValue;
  procedure raiseUnexpectedError();
end;

implementation
uses math;

procedure TJSONScannerHelper.raiseUnexpectedError();
var
  at: String;
begin
  at := Copy(CurLine, CurColumn - 30, min(CurColumn - 1, 30) ) + '|' + Copy(CurLine, CurColumn, 30 ) ;
  raise EJSONScannerHelperException.create('Konfigurationsdatei konnte nicht geladen werden. Unerwartetes ' + CurTokenString + '<'+inttostr(ord(CurToken)) + '> '+at);
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
        end;
    end;
    else expectCurrentToken(tkString);
  end;
  fetchNonWSToken;
end;

end.

