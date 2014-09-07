program assetversiontests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils,bbutils
  { you can add units after this };


var versionNumber: integer = 1234;

function testAssetVersion(const res: string): boolean;
  function readAttrib(pos: integer): string;
  var p, marker: pchar;
  begin //highspeed xml attribute reader for ascii attributes
    result := '';
    p := @res[pos];
    while (p^ in [' ', #9, #10, #13]) do inc(p);
    if p^ <> '=' then exit(); inc(p);
    while (p^ in [' ', #9, #10, #13]) do inc(p);
    if not (p^ in ['"', '''']) then exit(); inc(p);
    if p^ = #0 then exit();
    marker := p;
    inc(p);
    while not (p^ in [#0, '"', '''']) do inc(p);
    result := strFromPchar(marker, p - marker);
  end;


var
  actions, actionsEnd: LongInt;
  maxVersion: Integer;
  skip: Boolean;
  i: Integer;
begin
  result := true;
  actions := strIndexOf(res, '<actions');
  if actions <= 0 then exit;
  actionsEnd := strIndexOf(res, '>', actions);
  maxVersion := -1;
  skip := false;
  for i := actions + length('<actions') to actionsEnd - length('version-mismatch="skip"') do
    if strBeginsWith(@res[i], 'max-version') then
      maxVersion := StrToIntDef(readAttrib(i+length('max-version')), -1)
    else if strBeginsWith(@res[i], 'version-mismatch') then
      skip := striEqual(readAttrib(i + length('version-mismatch')), 'skip');
  if skip and (maxVersion <> -1) and (maxVersion < versionNumber) then result := false; //ABORT!
end;

var ti: integer;
  header: String;
procedure test(xml: string; expected: boolean);
begin
  inc(ti);
  if testAssetVersion(xml) = expected then exit;
  writeln('failed ',ti ,' ', expected, ' <> ',not expected, ' ', xml);
end;

begin
  test('<actions max-version="1660" version-mismatch="skip">', true);
  test('<actions max-version="1234" version-mismatch="skip">', true);
  test('<actions max-version="1233" version-mismatch="skip">', false);
  test('<actions max-version   ="1660" version-mismatch=   "skip">', true);
  test('<actions max-version   ="1234" version-mismatch=   "skip">', true);
  test('<actions max-version   ="1233" version-mismatch=   "skip">', false);
  test('<actions max-version=   "1660" version-mismatch    ="skip">', true);
  test('<actions max-version=   "1234" version-mismatch    ="skip">', true);
  test('<actions max-version=    "1233" version-mismatch   ="skip">', false);
  test('<actions max-version     =   "1660" version-mismatch    =      "skip">', true);
  test('<actions max-version    =   "1234" version-mismatch    =      "skip">', true);
  test('<actions max-version    =    "1233" version-mismatch   =      "skip">', false);

  header := '<?xml version="1.0" encoding="UTF-8"?>'#13#10#13;
  test(header+'<actions max-version="1660" version-mismatch="skip">', true);
  test(header+'<actions max-version="1234" version-mismatch="skip">', true);
  test(header+'<actions max-version="1233" version-mismatch="skip">', false);
  test(header+'<actions max-version   ="1660" version-mismatch=   "skip">', true);
  test(header+'<actions max-version   ="1234" version-mismatch=   "skip">', true);
  test(header+'<actions max-version   ="1233" version-mismatch=   "skip">', false);
  test(header+'<actions max-version=   "1660" version-mismatch    ="skip">', true);
  test(header+'<actions max-version=   "1234" version-mismatch    ="skip">', true);
  test(header+'<actions max-version=    "1233" version-mismatch   ="skip">', false);
  test(header+'<actions max-version     =   "1660" version-mismatch    =      "skip">', true);
  test(header+'<actions max-version    =   "1234" version-mismatch    =      "skip">', true);
  test(header+'<actions max-version    =    "1233" version-mismatch   =      "skip">', false);


  test('<actions max-version="1660" version-mismatch="skipx">', true);
  test('<actions max-version="1234" version-mismatch="skipx">', true);
  test('<actions max-version="1233" version-mismatch="skipx">', true);
  test('<actions max-version   ="1660" version-mismatch=   "skipx">', true);
  test('<actions max-version   ="1234" version-mismatch=   "skipx">', true);
  test('<actions max-version   ="1233" version-mismatch=   "skipx">', true);
  test('<actions max-version=   "1660" version-mismatch    ="skipx">', true);
  test('<actions max-version=   "1234" version-mismatch    ="skipx">', true);
  test('<actions max-version=    "1233" version-mismatch   ="skipx">', true);
  test('<actions max-version     =   "1660" version-mismatch    =      "skixp">', true);
  test('<actions max-version    =   "1234" version-mismatch    =      "skixp">', true);
  test('<actions max-version    =    "1233" version-mismatch   =      "skxip">', true);

  test('<actions max-version=" version-mismatch="skip">', true);
  test('<actions max-version=" version-mismatch="skip">', true);
  test('<actions max-version=" version-mismatch="skip">', true);
  test('<actions max-version   ="1660 version-mismatch=   "skip">', true);
  test('<actions max-version   ="1234 version-mismatch=   "skip">', true);
  test('<actions max-version   ="1233 version-mismatch=   "skip">', true);
  test('<actions max-version=   "1660 version-mismatch    ="skip">', true);
  test('<actions max-version=   "1234 version-mismatch    ="skip">', true);
  test('<actions max-version=    "1233 version-mismatch   ="skip">', true);
  test('<actions max-version     =   "1660 version-mismatch    =      "skip">', true);
  test('<actions max-version    =   "1234 version-mismatch    =      "skip">', true);
  test('<actions max-version    =    "1233 version-mismatch   =      "skip">', true);
end.

