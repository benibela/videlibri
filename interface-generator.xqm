xquery version "3.1-xidel";
module namespace ig="ig";

declare function ig:error($e){
  error(QName("interface"), $e)
};

declare function ig:properties($e){
  $e/*
};

declare function ig:parent($e){
  $e/@extends[. ne "FastInterfacedObject"]
};

declare function ig:ancestors($e){
  let $parent-id := ig:parent($e)
  let $parent := $e/root()//class[@id = $parent-id]
  where $parent
  return (ig:ancestors($parent), $parent)
};

declare function ig:ancestor-and-self-properties($e){
  ig:properties((ig:ancestors($e),$e))
};


declare function ig:jni-name($c){
  typeswitch ($c)
    case element(class) return "de/benibela/videlibri/jni/"||$c/@id
    case element(string) return "java/lang/String"
    case element(classref) return "de/benibela/videlibri/jni/"||$c/@ref
    default return 
      "de/benibela/videlibri/jni/"||$c
};

declare function ig:jni-full-name($c){
  $c/(typeswitch(.)
      case element(string) return "Ljava/lang/String;" 
      case element(array) return concat("[L", ig:jni-name(*), ";")
      case element(classref) return concat("L", ig:jni-name(.), ";")
      case element(int) return "I"
      case element(long) return "J"
      case element(double) return "D"
      case element(boolean) return "Z"
      default return ())
};


declare function ig:pascal-make-fields-of-type($s, $type){
  if (empty($s)) then () 
  else join($s/@name, ", ") || ": " || $type || ";"
};

declare function ig:pascal-make-fields($s, $prefix){
  ig:pascal-make-fields-of-type($s/string, $prefix || "string"),
  ig:pascal-make-fields-of-type($s/int, $prefix || "int"),
  ig:pascal-make-fields-of-type($s/long, $prefix || "int64"),
  ig:pascal-make-fields-of-type($s/double, $prefix || "double"),
  ig:pascal-make-fields-of-type($s/boolean, $prefix || "boolean"),
  $s/array/(@name || ig:pascal-make-fields(., "array of ")),
  $s/classref/ig:pascal-make-fields-of-type(., $prefix||"T"||@ref)
};

declare function ig:pascal-make-fields($s){
  ig:pascal-make-fields($s, "")  
};

declare function ig:pascal-make-class($s){
$s/(let $virtual := if (ig:parent(.)) then "override;" else "virtual;" return x"
type T{@id} = class{@extends!concat("(T",.,")")}
  {join(
    (ig:pascal-make-fields(.),
     
      if (@serialize-json and not(ig:parent(.))) then "procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;" else (),
      if (.//classref) then "destructor destroy; override;" else (),
  if (@serialize-json) then
  x"class function fromJSON(json: string): T{@id};
  class function fromJSON(var json: TJSONScanner): T{@id};
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); {$virtual}
  class function readObjectOfType(var scanner: TJSONScanner; const serializedTypeId: string): TObject; 
  class function readTypedObject(var scanner: TJSONScanner): T{@id}; //['type', {{obj}}]
  class function readObject(obj: T{@id}; var scanner: TJSONScanner): T{@id};
  procedure readProperty(var scanner: TJSONScanner);  {$virtual}
  class function typeId: string; {$virtual}" else ()
), "&#x0A;  ")
}
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
 uses sysutils, simplexmlparser, xquery.internals.common, fastjsonscanner, jsonscannerhelper {{$ifdef android}}, jni, bbjniutils{{$endif}};
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


type TStringArray = array of string;
procedure readArray(var sa: TStringArray; var scanner: TJSONScanner); overload;
var temp: TStringArrayList;
begin
  scanner.fetchNonWSToken; scanner.expectCurrentToken(tkSquaredBraceOpen);
  temp.init;
  while true do begin
    case scanner.fetchNonWSToken of
      tkString: temp.add(scanner.CurTokenString);
      tkComma:;
      tkSquaredBraceClose: break;
      else scanner.raiseUnexpectedError();
    end;
  end;

  sa := temp.toSharedArray;
end;        

type TStringHelper = type helper for string
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
end;
procedure TStringHelper.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  builder.appendJSONString(self);
end;

{let $arrayrefs := distinct-values($r/api//array/classref/@ref)
   where exists($arrayrefs)
return $arrayrefs!x"
type T{.}ArrayList = specialize TCopyingPtrArrayList<T{.}>;
procedure readArray(var p: T{.}Array; var scanner: TJSONScanner); overload;
var temp: T{.}ArrayList;
begin
  scanner.fetchNonWSToken; scanner.expectCurrentToken(tkSquaredBraceOpen);
  temp.init;
  while true do begin
    case scanner.fetchNonWSToken of
      tkString: temp.add(T{.}.readTypedObject(scanner));
      tkComma: ;
      tkSquaredBraceClose: break;
      tkCurlyBraceOpen: temp.add(T{.}.readObject(T{.}.create, scanner));
      else scanner.raiseUnexpectedError();
    end;
  end;

  p := temp.toSharedArray;
end;"}        

{
$r/api/class/(
.[@serialize-json and not(ig:parent(.))]!
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

let $temp := . where @serialize-json return x"procedure T{@id}.appendToJSON(var builder: TJSONXHTMLStrBuilder);{if (./array) then "&#x0A;var i: sizeint;" else ()}
begin{ig:parent(.)!"
  inherited;
  builder.appendJSONObjectComma;"
  }
  with builder do begin    
    {join(./*/(typeswitch(.) 
      case element(string) return x"appendJSONObjectKeyColon('{@name}'); appendJSONString(self.{@name});"
      case element(int) | element(long) return x"appendJSONObjectKeyColon('{@name}'); appendNumber(self.{@name});"
      case element(double) return ig:error("double not implemented")
      case element(boolean) return x"appendJSONObjectKeyColon('{@name}'); if self.{@name} then append('true') else append('false');"
      case element(array) return x"appendJSONObjectKeyColon('{@name}'); appendJSONArrayStart();
    for i := 0 to high({@name}) do begin
      if i > 0 then appendJSONArrayComma();{
      if (classref) then x"
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
  scanner := default(TJSONScanner);
  scanner.init(json, [joUTF8, joIgnoreTrailingComma]);
  scanner.fetchNonWSToken;
  result := fromJSON(scanner);
  scanner.done;
end;
class function T{@id}.fromJSON(var json: TJSONScanner): T{@id};
begin
  result := readObject(T{@id}.create, json);
end;

class function T{@id}.readObject(obj: T{@id}; var scanner: TJSONScanner): T{@id};
begin
  result := obj;
  scanner.expectCurrentToken(tkCurlyBraceOpen); scanner.fetchNonWSToken;
  while true do begin    
    case scanner.curtoken of
      tkString: result.readProperty(scanner);
      tkEOF: exit;
      tkCurlyBraceClose: exit;
      tkComma: scanner.fetchNonWSToken;
      else scanner.raiseUnexpectedError();
    end;
  end;
end;

class function T{@id}.readObjectOfType(var scanner: TJSONScanner; const serializedTypeId: string): TObject;
var temp: T{@id};
begin
  case serializedTypeId of
  {let $id := @id return (., /api/class[$id = ig:ancestors(.)/@id])!x"
    '{@id}': temp := T{@id}.create;"}
  else begin scanner.raiseUnexpectedError(); temp := nil; end;
  end;
  result := readObject(temp, scanner);
end;

class function T{@id}.readTypedObject(var scanner: TJSONScanner): T{@id}; 
begin
  result := scanner.fetchSerializedTypedObject(@readObjectOfType) as T{@id};
end;

procedure T{@id}.readProperty(var scanner: TJSONScanner);
begin
  case scanner.CurTokenString of
    {join(*/(typeswitch (.) 
      case element(string) return x"'{@name}': {@name} := scanner.fetchStringObjectPropertyValue;"
      case element(int) return x"'{@name}': {@name} := StrToIntDef(scanner.fetchStringObjectPropertyValue, 0);"
      case element(long) return x"'{@name}': {@name} := StrToInt64Def(scanner.fetchStringObjectPropertyValue, 0);"
      case element(double) return x"'{@name}': {@name} := StrToFloatDef(scanner.fetchStringObjectPropertyValue, 0);"
      case element(boolean) return x"'{@name}': {@name} := scanner.fetchStringObjectPropertyValue = 'true';"
      case element(array) return x"'{@name}': begin
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkColon); 
        readArray({@name}, scanner);
        scanner.fetchNonWSToken;
      end;"
      default return ()
    ), "&#x0A;    ")
    }
    else {if (ig:parent(.)) then "inherited;" else "scanner.skipObjectPropertyValue();"
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
function arrayToJArrayCI(const a: T{.}Array): jobject; overload;
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
function arrayToJArrayCI(const a: array of string): jobject; overload;
begin;
  result := j.arrayToJArray(a);
end;

{ $r/api/class/(let $allprops := ig:ancestor-and-self-properties(.) return x"
function T{@id}.toJava: jobject;
var temp: array[0..{count($allprops) - 1}] of jvalue;
begin
  with j do begin
 {for $p at $i1 in $allprops let $i := $i1 - 1 return 
   typeswitch ($p) 
     case element(string)  return x"   temp[{$i}].l := stringToJString(self.{$p/@name});&#x0A;"
     case element(int)     return x"   temp[{$i}].i := self.{$p/@name};&#x0A;"
     case element(long)    return x"   temp[{$i}].j := self.{$p/@name};&#x0A;"
     case element(double)  return x"   temp[{$i}].d := self.{$p/@name};&#x0A;"
     case element(boolean) return x"   temp[{$i}].z := booleanToJboolean(self.{$p/@name});&#x0A;"
     case element(array)   return x"   temp[{$i}].l := arrayToJArrayCI(self.{$p/@name});&#x0A;"
     case element(classref)   return x"   temp[{$i}].l := self.{$p/@name}.toJava;&#x0A;"
     default return ig:error("unknown property type")
   }
    result := newObject({@id}Class, {@id}ClassInit, @temp[0]); 
 {for $p at $i in $allprops where $p[self::array or self::string or self::classref] return x"   deleteLocalRef(temp[{$i - 1}].l);&#x0A;"}
 end;
end;
")
}


procedure initBridge;
begin
  with needJ do begin {$r/api/class/x"
    {@id}Class := newGlobalRefAndDelete(getclass('{ig:jni-name(.)}'));
    {@id}ClassInit := getmethod({@id}Class, '<init>', '({join(ig:ancestor-and-self-properties(.)/ig:jni-full-name(.)
      , "")})V');"}
  end;
end;
{{$endif}}
end.
"
};

declare function ig:kotlin-make-prop-type($s, $prefix, $suffix){
  $prefix || 
  $s/(typeswitch (.) 
    case element(string) return "String" 
    case element(int) return "Int" 
    case element(long) return "Long" 
    case element(double) return "Double" 
    case element(boolean) return "Boolean" 
    case element(classref) return @ref
    case element(array) return ig:kotlin-make-prop-type(*, "Array<", ">")
    default return () 
  ) || $suffix
};

declare function ig:kotlin-make-prop($s){
  $s/(typeswitch (.) 
    case element(string)|element(int)|element(long)|element(double)|element(boolean)|element(array)|element(classref) 
    return x"{@name}: " || ig:kotlin-make-prop-type(., "", "")
    default return () 
  )
};

declare function ig:kotlin-make-class($s){
  let $a := ig:ancestors($s)
  let $fieldtype := ($s/@kotlin-var, "val")[1] || " "
  let $aprops := ig:properties($a)
  return
  $s/(x"
  {(@kotlin-class, "open")[1]} class {@id}( 
    { join ((
    $aprops!ig:kotlin-make-prop(.),
    */ig:kotlin-make-prop(.)!concat($fieldtype,.)
  ), ",&#x0A;    ") 
  }
  ) {ig:parent(.)!x": {.}({join($aprops!@name, ", ")})"}  {{
    override fun equals(other: Any?): Boolean =
       other != null &amp;&amp; {join(("javaClass == other.javaClass", "other is " || @id, (*,$aprops)/(
         typeswitch (.)
           case element(string)|element(int)|element(long)|element(double)|element(boolean)|element(classref) return concat(@name, " == other.", @name)
           case element(array) return concat(@name, ".contentEquals(other.", @name,")")
           default return ()
       )), " &amp;&amp; ")}

  }}")
};
declare function ig:kotlin-make($r){
  x"@file:Suppress(""EqualsOrHashCode"", ""unused"")
package de.benibela.videlibri.jni;
{ $r/api/class/ig:kotlin-make-class(.)} "
};


 
 
 
declare function ig:pascal-make-function-native-declaration($f){ 
  $f/x",(name:'{@id}'; signature: '({(* except return-type)/ig:jni-full-name(.)}){ig:jni-full-name(return-type/*)}'; fnPtr: @Java_de_benibela_VideLibri_Bridge_{@id})"
};
 
declare function ig:pascal-make-function($f){ 
  $f/x"
function Java_de_benibela_VideLibri_Bridge_{@id}(env:PJNIEnv; this:jobject {(:todo:)()}): jobject; cdecl;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.{@id} (started)');
  result := nil;
  try
    //todo
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.{@id} (ended)');
end;
  "
};

declare function ig:java-make-function-native-declaration($f){ 
  $f/x"static public native {return-type/classref/@ref} {@id}();"
};
 
 
declare function ig:code-to-add-manually($r){
  let $functions := $r/api/function
  return (
  $functions/ig:java-make-function-native-declaration(.),
  "","","","",
  $functions/ig:pascal-make-function-native-declaration(.), 
  "","","","",
  $functions/ig:pascal-make-function(.)
  )
};