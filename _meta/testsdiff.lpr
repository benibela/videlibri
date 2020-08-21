program testsdiff;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils, bbutils, fpjson, jsonparser
  { you can add units after this };

var fsl1, fsl2: tstringlist;

procedure error(const s: string; linenr: integer = -1);
begin
  writeln(stderr, s);
  if linenr >= 0 then begin
    writeln(stderr, 'Line: ',linenr);
    writeln(stderr, '<', fsl1[linenr]);
    writeln(stderr, '>', fsl2[linenr]);
  end;
  halt;
end;

var  i:integer;

  procedure jsonCompare(j1, j2: TJSONData);
    procedure jerror(const s: string);
    begin
      error('json: '+s, i);
    end;
  var
    o1, o2: TJSONObject;
    j: Integer;
  begin
    if j1.JSONType <> j2.JSONType then jerror('invalid type') ;
    if j1.Count <> j2.Count then jerror('invalid count');
    case j1.JSONType of
      jtNumber:
      {jitNumberInteger: if j1.AsInteger <> j2.AsInteger then jerror('invalid integer');
      jitNumberInt64: if j1.AsInt64 <> j2.AsInt64 then jerror('invalid integer');
      jitNumberQWord: if j1.AsQWord <> j2.AsQWord then jerror('invalid integer');
      jitNumberFloat: }if j1.AsFloat <> j2.AsFloat then jerror('invalid integer');
      jtString: if j1.AsString <> j2.AsString then jerror('invalid integer');
      jtBoolean: if j1.AsBoolean <> j2.AsBoolean then jerror('invalid integer');
      jtArray: for j := 0 to j1.Count - 1 do begin
        jsonCompare(j1.Items[j], j2.Items[j]);
      end;
      jtObject: begin
        o1 := j1 as TJSONObject;
        o2 := j2 as TJSONObject;
        for j := 0 to j1.Count - 1 do
          jsonCompare(o1.Items[j], o2.Elements[o1.Names[j]]);
        for j := 0 to j1.Count - 1 do
          jsonCompare(o1.Elements[o2.Names[j]], o2.Items[j]);
      end
      else;
    end;

  end;

var  l1, l2, s2, s1: String;
  j1, j2: TJSONData;
begin
  fsl1 := TStringList.Create;
  fsl1.LoadFromFile(ParamStr(1));
  fsl2 := TStringList.Create;
  fsl2.LoadFromFile(ParamStr(2));
  if fsl1.Count <> fsl2.Count then error('Count mismatch: '+inttostr(fsl1.Count) +' <> ' + inttostr(fsl2.Count));
  for i := 0 to fsl1.Count - 1 do begin
    l1 := fsl1[i];
    l2 := fsl2[i];
    if l1 = l2 then continue;
    if not strContains(l1, ':=') or not strContains(l2, ':=') then error('Unequal lines', i);
    if strBefore(l1, ':=').Trim <> strBefore(l2, ':=').Trim then error('Assignment to different variables', i) ;
    s1 := strAfter(l1, ':=').Trim();
    s2 := strAfter(l2, ':=').Trim();
    if s1 = s2 then continue;
    if (not s1.beginsWith('{') and not s1.beginsWith('[')) or (not s2.beginsWith('{') and not s2.beginsWith('[')) then error('Different assignment to ' + strBefore(l1, ':=').Trim );
    j1 := GetJSON(s1);
    j2 := GetJSON(s2);
    jsonCompare(j1, j2);
  end;
//  if length(fsl1.text) = length(fsl2.Text) then exit;
  writeln('ok'#9,length(fsl1.text), #9, length(fsl2.Text));
end.

