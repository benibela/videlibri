xquery version "3.1-xidel";
module namespace ig="ig";

declare function ig:ancestors($e){
  let $parent-id := $e/@extends
  let $parent := $e/root()//class[@id = $parent-id]
  where $parent
  return (ig:ancestors($parent), $parent)
};


declare function ig:jni-name($c){
  typeswitch ($c)
    case element(class) return "de/benibela/videlibri/jni/"||$c/@id
    default return 
      "de/benibela/videlibri/jni/"||$c
};



declare function ig:pascal-make-class($s){
$s/(let $virtual := if (@extends) then "override;" else "virtual;" return x"
type T{@id} = class{@extends!concat("(T",.,")")}
  {join(( if (./string) then join(./string/@name, ", ") || ": string;" else (),
  ./array/x"{@name}: array of T{classref/@ref};",
  ./property-array/x"{@name}: TProperties;",
  if (not(@extends)) then "procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;" else (),
  if (.//classref) then "destructor destroy; override;" else ()), "&#x0A;  ")
  }
  class function fromJSON(json: string): T{@id};
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); {$virtual}
  class function readTypedObject(scanner: TJSONScanner): T{@id}; //['type', {{obj}}]
  class function readObject(obj: T{@id}; scanner: TJSONScanner): T{@id};
  procedure readProperty(scanner: TJSONScanner);  {$virtual}
  class function typeId: string; {$virtual}
public
  {{$ifdef android}}
  function toJava: jobject; {$virtual}
  {{$endif}}
end;")
};
declare function ig:pascal-make($r){
x"unit commoninterface;
{{$mode objfpc}}{{$H+}}
{{$ModeSwitch typehelpers}}{{$ModeSwitch advancedrecords}}{{$ModeSwitch autoderef}}
interface
 uses sysutils, simplexmlparser, xquery.internals.common, jsonscanner {{$ifdef android}}, commoninterface, jni, bbjniutils{{$endif}};
 type EVideLibriInterfaceException = class(Exception);
 { let $arrayrefs := distinct-values($r/api//array/classref/@ref)
   where exists($arrayrefs)
   return ("type " || join($arrayrefs ! x"T{.} = class; T{.}Array = array of T{.}; "))
 }
 { $r/api/class/ig:pascal-make-class(.) }
 {{$ifdef android}}
 procedure initBridge;
 {{$endif}}
implementation
uses bbutils, math;

type TPropertyHelper = record helper for TProperty
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
end;

procedure TPropertyHelper.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin
    appendJSONObjectStart();
    appendJSONObjectKeyColon('name'); appendJSONString(name); appendJSONObjectComma();
    appendJSONObjectKeyColon('value'); appendJSONString(value); 
    appendJSONObjectEnd();
  end;
end;

procedure raiseJSONParsingError(scanner: TJSONScanner);
var
  at: String;
begin
  at := Copy(scanner.CurLine, scanner.CurColumn - 30, min(scanner.CurColumn - 1, 30) ) + '|' + Copy(scanner.CurLine, scanner.CurColumn, 30 ) ;
  raise EVideLibriInterfaceException.create('Konfigurationsdatei konnte nicht geladen werden. Unerwartetes ' + scanner.CurTokenString + '<'+inttostr(ord(scanner.CurToken)) + '> '+at);
end;         

type TJSONScannerHelper = class helper for TJSONScanner
  procedure expectToken(token: TJSONToken);
  function fetchNonWSToken: TJSONToken;
  procedure skipUnknownProperty;
end;
procedure TJSONScannerHelper.expectToken(token: TJSONToken);
begin
  if CurToken <> token then
    raiseJSONParsingError(self);
end;

function TJSONScannerHelper.fetchNonWSToken: TJSONToken;
begin
  repeat
    result := FetchToken;
  until result <> tkWhitespace;
end;

procedure TJSONScannerHelper.skipUnknownProperty;
var nesting: sizeint;
begin
  fetchNonWSToken; expectToken(tkColon);
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
    else expectToken(tkString);
  end;
  fetchNonWSToken;
end;

type TPropertiesRecordArrayList = specialize TRecordArrayList<TProperty>;
procedure readArray(var p: TProperties; scanner: TJSONScanner); overload;
var temp: TPropertiesRecordArrayList;
  pname: String;
begin
  scanner.fetchNonWSToken; scanner.expectToken(tkSquaredBraceOpen);
  temp.init;
  while true do begin
    case scanner.fetchNonWSToken of
      tkCurlyBraceOpen: begin
        temp.add(default(TProperty));
        while true do case scanner.fetchNonWSToken of
          tkString: begin
            pname := scanner.CurTokenString;
            scanner.fetchNonWSToken; scanner.expectToken(tkColon);
            scanner.fetchNonWSToken; scanner.expectToken(tkString);
            case pname of
              'name': temp.last.name := scanner.CurTokenString;
              'value': temp.last.value := scanner.CurTokenString;
            end;
          end;
          tkComma: ;
          tkCurlyBraceClose, tkEOF: break;
          else raiseJSONParsingError(scanner);
        end;
      end;
      tkComma:;
      tkSquaredBraceClose: break;
      else raiseJSONParsingError(scanner);
    end;
  end;

  p := temp.toSharedArray;
end;        

{let $arrayrefs := distinct-values($r/api//array/classref/@ref)
   where exists($arrayrefs)
return $arrayrefs!x"
type T{.}ArrayList = specialize TCopyingPtrArrayList<T{.}>;
procedure readArray(var p: T{.}Array; scanner: TJSONScanner); overload;
var temp: T{.}ArrayList;
begin
  scanner.fetchNonWSToken; scanner.expectToken(tkSquaredBraceOpen);
  temp.init;
  while true do begin
    case scanner.fetchNonWSToken of
      tkString: temp.add(T{.}.readTypedObject(scanner));
      tkComma: ;
      tkSquaredBraceClose: break;
      tkCurlyBraceOpen: temp.add(T{.}.readObject(T{.}.create, scanner));
      else raiseJSONParsingError(scanner);
    end;
  end;

  p := temp.toSharedArray;
end;"}        

{
$r/api/class/(
.[not(@extends)]!
x"procedure T{@id}.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin
    appendJSONObjectStart;
    self.appendToJSON(builder);
    appendJSONObjectEnd;
  end;
end;

function T{@id}.toJSON(): string;
var b:  TJSONXHTMLStrBuilder;
begin
  b.init(@result);
  toJSON(b);
  b.final;
end;

",

x"procedure T{@id}.appendToJSON(var builder: TJSONXHTMLStrBuilder);{if (./array, ./property-array) then "&#x0A;var i: sizeint;" else ()}
begin{@extends!"
  inherited;
  builder.appendJSONObjectComma;"
  }
  with builder do begin    
    {join(./*/(typeswitch(.) 
      case element(string) return x"appendJSONObjectKeyColon('{@name}'); appendJSONString(self.{@name});"
      case element(array) | element(property-array) return x"appendJSONObjectKeyColon('{@name}'); appendJSONArrayStart();
    if length({@name}) > 0 then begin{
      if (classref) then x"
      appendJSONString({@name}[0].typeId);
      appendJSONArrayComma();
      " else ()}
      {@name}[0].toJSON(builder);
    end;
    for i := 1 to high({@name}) do begin
      appendJSONArrayComma();
      {if (classref) then x"
      appendJSONString({@name}[i].typeId);
      appendJSONArrayComma();
      " else ()}
      {@name}[i].toJSON(builder);
    end;
    appendJSONArrayEnd();"
      default return ()), " appendJSONObjectComma;&#x0A;    " )}
    
  end;
end;

class function T{@id}.fromJSON(json: string): T{@id};
var scanner: TJSONScanner;
begin
  scanner := TJSONScanner.Create(json, [joUTF8, joIgnoreTrailingComma]);
  scanner.fetchNonWSToken;
  result := readObject(T{@id}.create, scanner);
  scanner.free;
end;

class function T{@id}.readObject(obj: T{@id}; scanner: TJSONScanner): T{@id};
begin
  result := obj;
  scanner.expectToken(tkCurlyBraceOpen); scanner.fetchNonWSToken;
  while true do begin    
    case scanner.curtoken of
      tkString: result.readProperty(scanner);
      tkEOF: exit;
      tkCurlyBraceClose: exit;
      tkComma: scanner.fetchNonWSToken;
      else raiseJSONParsingError(scanner);
    end;
  end;
end;

class function T{@id}.readTypedObject(scanner: TJSONScanner): T{@id}; 
var additionalArray: boolean;
begin
  additionalArray := scanner.curtoken = tkSquaredBraceOpen;
  if additionalArray  then scanner.fetchNonWSToken;
  scanner.expectToken(tkString);
  case scanner.CurTokenString of
  {let $id := @id return (., /api/class[$id = ig:ancestors(.)/@id])!x"
    '{@id}': result := T{@id}.create;"}
  else raiseJSONParsingError(scanner);
  end;
  scanner.fetchNonWSToken; scanner.expectToken(tkComma);
  scanner.fetchNonWSToken;
  result := readObject(result, scanner);
  if additionalArray then begin
    scanner.fetchNonWSToken; scanner.expectToken(tkSquaredBraceClose);
  end;
end;

procedure T{@id}.readProperty(scanner: TJSONScanner);
begin
  case scanner.CurTokenString of
    {join(*/(typeswitch (.) 
      case element(string) return x"'{@name}': begin
        scanner.fetchNonWSToken; scanner.expectToken(tkColon); 
        scanner.fetchNonWSToken; scanner.expectToken(tkString); 
        {@name} := scanner.CurTokenString;
        scanner.fetchNonWSToken;
      end;"
      case element(array) | element(property-array) return x"'{@name}': begin
        scanner.fetchNonWSToken; scanner.expectToken(tkColon); 
        readArray({@name}, scanner);
        scanner.fetchNonWSToken;
      end;"
      default return ()
    ), "&#x0A;    ")
    }
    else {if (@extends) then "inherited;" else "scanner.skipUnknownProperty();"
    }
  end;
end;

class function T{@id}.typeId: string;
begin
  result := '{@id}'
end;

", .[.//classref]/(
x"destructor T{@id}.destroy;{if (./array[classref]) then "&#x0A;var i: integer;" else ()}
begin {./array[classref]/x"&#x0A;  for i := 0 to high({@name}) do {@name}[i].free;"}
  inherited;
end;"
))
}

{{$ifdef android}}
var {$r/api/class/x"
  {@id}Class: jclass;
  {@id}ClassInit: jmethodID;
"}

{ distinct-values($r/api//array/classref/@ref)!x"
function arrayToJArray(const a: T{.}Array): jobject; overload;
var
  i: Integer;
begin
  with j do begin
    result := newObjectArray(length(a), {.}Class, nil);
    for i := 0 to high(a) do
      setObjectArrayElementAndDelete(result, i, a[i].toJava);
  end;
end;
" }

{ $r/api/class/(let $allprops := (ig:ancestors(.),.)/* return x"
function T{@id}.toJava: jobject;
var temp: array[0..{count($allprops) - 1}] of jvalue;
begin
  with j do begin
 {for $p at $i1 in $allprops let $i := $i1 - 1 return 
   typeswitch ($p) 
     case element(string) return x"   temp[{$i}].l := stringToJString(self.{$p/@name});&#x0A;"
     case element(array) | element(property-array) return x"   temp[{$i}].l := arrayToJArray(self.{$p/@name});&#x0A;"
     default return ()
   }
    result := newObject({@id}Class, {@id}ClassInitWithData, @temp[0]); 
 {for $p at $i in $allprops return x"   deleteLocalRef(temp[{$i - 1}].l);&#x0A;"}
 end;
end;
")
}


procedure initBridge;
begin
  with needJ do begin {$r/api/class/x"
    {@id}Class := newGlobalRefAndDelete(getclass('{ig:jni-name(.)}'));
    {@id}ClassInit := getmethod({@id}Class, '<init>', '({join((ig:ancestors(.),.)/*/(typeswitch(.)
      case element(string) return "Ljava/lang/String;" 
      case element(array) return concat("[L", ig:jni-name(*))
      default return ())
      , "")})V');"}
  end;
end;
{{$endif}}
end.
"
};

declare function ig:kotlin-make-prop($s){
  $s/(typeswitch (.) 
    case element(string) return @name || ": String" 
    case element(array) return x"{@name}: Array<{classref/@ref}>" 
    case element(property-array) return x"{@name}: Array<String>" 
    default return () 
  )
};

declare function ig:kotlin-make-class($s){
  let $a := ig:ancestors($s)
  let $aprops := $a/*
  return
  $s/(x"
  public class {@id}( 
    { join ((
    $aprops!ig:kotlin-make-prop(.),
    */ig:kotlin-make-prop(.)!concat("public val ",.)
  ), ",&#x0A;    ")
  }
  ) {@extends!x": {.}({join($aprops!@name, ", ")})"} {{
  }}")
};
declare function ig:kotlin-make($r){
  x"{ $r/api/class/ig:kotlin-make-class(.)} "
};


 