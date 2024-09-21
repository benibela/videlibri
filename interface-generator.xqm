xquery version "3.1-xidel";
module namespace ig="ig";

declare function ig:error($e){
  error(QName("interface"), $e)
};
declare function ig:error($e, $at){
  error(QName("interface"), $e || " at: " || serialize($at))
};

declare function ig:properties($e){
  $e/*[not(self::comment)]
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

declare function ig:descendants($e){
  let $allclasses := $e/root()//class
  let $id := $e/@id
  return $allclasses[ig:ancestors(.)/@id = $id ]
};

declare function ig:jni-name($c){
  typeswitch ($c)
    case element(class) return "de/benibela/videlibri/jni/"||$c/@id
    case element(string) return "java/lang/String"
    case element(classref) return "de/benibela/videlibri/jni/"||$c/@ref
    default return 
      "de/benibela/videlibri/jni/"||$c
};

declare function ig:jni-pascal-suffix($c){
  $c/(typeswitch(.)
      case element(string) return "S" 
      case element(array) return "A"
      case element(classref) return "L"
      case element(int) | element(intenumref) return "I" 
      case element(long) return "J"
      case element(double) return "D"
      case element(boolean) return "Z"
      default return ())
};

declare function ig:jni-full-name($c){
  $c/(typeswitch(.)
      case element(string) return "Ljava/lang/String;" 
      case element(array) return concat("[L", ig:jni-name(*), ";")
      case element(classref) return concat("L", ig:jni-name(.), ";")
      case element(intenumref) return "I"
      default return ig:jni-pascal-suffix(.))
};




declare function ig:pascal-call-constructor($e){ $e / (
  switch (@pascal-type)
    case "class" return x"T{@id}.create"
    default return x"default(T{@id})"
  )
};

declare function ig:pascal-make-fields-of-type($s, $type){
  if (empty($s)) then () 
  else join($s/@name, ", ") || ": " || $type || ";"
};

declare function ig:pascal-make-fields($s, $prefix){
  $s/comment/x"//{.}", 
  ig:pascal-make-fields-of-type($s/string, $prefix || "string"),
  ig:pascal-make-fields-of-type($s/int, $prefix || "int32"),
  ig:pascal-make-fields-of-type($s/long, $prefix || "int64"),
  ig:pascal-make-fields-of-type($s/double, $prefix || "double"),
  ig:pascal-make-fields-of-type($s/boolean, $prefix || "boolean"),
  $s/array/(@name || ig:pascal-make-fields(., "array of ")),
  $s/classref/ig:pascal-make-fields-of-type(., $prefix||"T"||@ref),
  $s/intenumref/ig:pascal-make-fields-of-type(., $prefix||"T"||@ref)
};

declare function ig:pascal-make-fields($s){
  ig:pascal-make-fields($s, "")  
};

declare function ig:pascal-make-intenum($e){ $e/ (
  let $values := ./value
  let $prefix := @pascal-prefix
  return x"T{@id} = ( {string-join($values ! x"{$prefix}{@name} = {(data(@value), position() - 1)[1]}" , ", ")} );
"
)
};

declare variable $ig:pascal-true-classes := /api/class[@pascal-type="class"]/@id;

declare function ig:pascal-make-class($s){
$s/(
let $type := (@pascal-type, "record")[1]
let $virtual := if ($type = "record") then "" else if (ig:parent(.)) then "override;" else "virtual;" 
let $record-static := if ($type = "record") then "static;" else ()
return x"
type 
{x"T{@id}Class = class of T{@id};"[$type="class"]}
T{@id} = {$type}{@extends!concat("(T",.,")")}
  {join(
    (ig:pascal-make-fields(.),
     
      if ($type ne "class" and exists((@serialize-json, .//classref[@ref=$ig:pascal-true-classes], .//@default))) then ig:error("JSON and non-POD types require Pascal class type") else (),
     
      if (@serialize-json and not(ig:parent(.))) then "procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;" else (),
      if (./classref,.//@default) then "constructor create; " || $virtual  else (),
      if (.//classref/@ref = $ig:pascal-true-classes) then "destructor destroy; override;" else (),
  if (@serialize-json) then
  x"class function fromJSON(const json: string): T{@id}; {$virtual, $record-static}
  class function fromJSON(const json: IXQValue): T{@id}; {$virtual, $record-static}
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); {$virtual}
  procedure setPropertiesFromJSON(const json: IXQValue); {$virtual}
  class function typeId: string; {$virtual, $record-static}
{ if (ig:parent(.)) then () else x"  class function classFromTypeId(const id: string): T{@id}Class;{$record-static}
  class function fromJSONWithType(const typ: string; const json: IXQValue): TObject;{$record-static}
"
}" else ()
), "&#x0A;  ")
}{if (@pascal-jvm or @jvm-pascal) then x"
public
  {{$ifdef android}}
  {if (@pascal-jvm) then x"function toJava: jobject; {$virtual}" else ()}
  {if (@jvm-pascal) then x"class function fromJava(jvm: jobject): T{@id}; {$virtual,$record-static}
  class function fromJavaAndDelete(jvm: jobject): T{@id}; {$virtual,$record-static}
  " else ()}
  {{$endif}}
" else ()
}
end; ")
};
declare function ig:pascal-make($r){
x"unit commoninterface;
{{$mode objfpc}}{{$H+}}
{{$ModeSwitch typehelpers}}{{$ModeSwitch advancedrecords}}{{$ModeSwitch autoderef}}
interface
 uses sysutils, xquery.internals.common, xquery, fastjsonreader {{$ifdef android}}, jni, bbjniutils{{$endif}};
 type EVideLibriInterfaceException = class(Exception);
 { $r/api/intenum/ig:pascal-make-intenum(.) }
 { $r/api/class/ig:pascal-make-class(.) }
 { let $arrayrefs := distinct-values($r/api//array/classref/@ref)
   where exists($arrayrefs)
   return ("type " || join($arrayrefs ! ((:x"T{.} = class;"[. = $r/$ig:pascal-true-classes], :)x"T{.}Array = array of T{.};")))
 }
 {{$ifdef android}}
 procedure initBridge;
 {{$endif}}
 
 function parseJSON(const s: string): IXQValue;
implementation
uses bbutils;

function parseJSON(const s: string): IXQValue;
begin
  result := TXQJsonParser.parse(s, [jpoAllowMultipleTopLevelItems, jpoLiberal, jpoAllowTrailingComma, jpoJSONiq]);
end;

procedure readArray(var sa: TStringArray; const json: IXQValue); overload;
var
  i: SizeInt = 0;
  pv: PIXQValue;
begin
  sa := nil;
  if json.kind <> pvkArray then
    exit;
  SetLength(sa, json.Size);
  for pv in json.GetEnumeratorMembersPtrUnsafe do begin
    sa[i] := pv^.toString;
    inc(i);
  end;
end;

type TGetObject = function (const typ: string; const json: IXQValue): TObject of object;
function readObjectArray(const json: IXQValue; callback: TGetObject): TObjectArrayList; overload;
var
  pv: PIXQValue;
  typ: String;
  temp: TObject;
begin
  result.init;
  if json.kind <> pvkArray then exit;
  typ := '';
  for pv in json.GetEnumeratorMembersPtrUnsafe do begin
    case pv^.kind of
    pvkString: typ := pv^.toString;
    pvkArray: if pv^.get(2).kind = pvkObject then begin
      typ := pv^.get(1).toString;
      temp := callback(typ, pv^.get(2));
      if  temp <> nil then result.add(temp);
      typ := '';
    end;
    pvkObject: begin
      temp := callback(typ, pv^);
      if  temp <> nil then result.add(temp);
      typ := '';
    end;
    end;
  end;
end;


type TStringHelper = type helper for string
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
end;

procedure TStringHelper.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  builder.appendJSONString(self);
end;

{for $array in distinct-values($r/api//array/classref/@ref)
 where $r/api/class[@id = $array][@serialize-json]
return $array!x"
procedure readArray(var p: T{.}Array; const json: IXQValue); overload;
var
  objList: TObjectArrayList;
  i: SizeInt;
  callback: TGetObject;
begin
  callback := @T{.}.fromJSONWithType;
  objList := readObjectArray(json, callback);
  setlength(p, objList.count);
  for i := 0 to high(p) do p[i] := objList[i] as T{.}
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
      case element(classref) return x"appendJSONObjectKeyColon('{@name}'); {@name}.toJSON(builder);"
      default return ig:error("invalid type for serialization")), " appendJSONObjectComma;&#x0A;    " )}
    
  end;
end;

class function T{@id}.fromJSON(const json: string): T{@id};
begin
  result := fromJSON(parseJSON(json))
end;
class function T{@id}.fromJSON(const json: IXQValue): T{@id};
begin
  result := T{@id}.create;
  result.setPropertiesFromJSON(json)
end;

procedure T{@id}.setPropertiesFromJSON(const json: IXQValue);
begin
  {join(ig:properties(.)/(typeswitch (.) 
    case element(string) return x"{@name} := json.getProperty('{@name}').toString();"
    case element(int) return x"{@name} := json.getProperty('{@name}').toInt64();"
    case element(long) return x"{@name} := json.getProperty('{@name}').toInt64();"
    case element(double) return x"{@name} := json.getProperty('{@name}').toFloat();"
    case element(boolean) return x"{@name} := json.getProperty('{@name}').toBoolean();"
    case element(array) return x"readArray({@name}, json.getProperty('{@name}'));"
    case element(classref) return x"{@name}.setPropertiesFromJSON(json.getProperty('{@name}'));"
    default return ig:error("invalid type for deserialization")
  ), "&#x0A;    ")
  }
  {if (ig:parent(.)) then "inherited;" else ()  }
end;

class function T{@id}.typeId: string;
begin
  result := '{@id}'
end;

",
if (empty(@serialize-json) or ig:parent(.)) then () else
let $descendants := ig:descendants(.) return x" 
class function T{@id}.classFromTypeId(const id: string): T{@id}Class;
begin{ 
  if (count($descendants) = 0) then x"
  ignore(id);
  result := T{@id}"
  else x"
  case id of
    { $descendants ! x"'{@id}': result := T{@id}; " }
    else result := T{@id};
  end;"}
end;
class function T{@id}.fromJSONWithType(const typ: string; const json: IXQValue): TObject;
begin
  result := classFromTypeId(typ).fromJSON(json);
end;

",
 .[./classref,.//@default]/(
x"constructor T{@id}.create;
begin
  inherited;{
  ./classref/x"&#x0A;  {@name} := T{@ref}.create;",
  .//@default/../x"&#x0A;  {@name} := {if (self::String) then x"'{@default}'" else @default};"
  }
end;
"
),
 .[.//classref/@ref = $ig:pascal-true-classes]/(
x"destructor T{@id}.destroy;{if (./array/classref/@ref = $ig:pascal-true-classes) then "&#x0A;var i: integer;" else ()}
begin {./array[classref/@ref = $ig:pascal-true-classes]/x"&#x0A;  for i := 0 to high({@name}) do {@name}[i].free;"}{
  ./classref[@ref = $ig:pascal-true-classes]/x"&#x0A;  {@name}.free;"}
  inherited;
end;"
))
}

{{$ifdef android}}

{ ($r/api/class[@jvm-pascal]//array/string)[1]!"
procedure fromJavaArrayAndDelete(var sa: TStringArray; jvm: jarray);
var
  i: sizeint;
begin
  with j do begin
    SetLength(sa, getArrayLength(jvm));
    for i := 0 to high(sa) do
      sa[i] := getStringArrayElement(jvm, i);
    deleteLocalRef(jvm);
  end;
end;

",
distinct-values($r/api/class[@jvm-pascal]//array/classref/@ref)!x"
procedure fromJavaArrayAndDelete(var sa: T{.}Array; jvm: jarray); //does not support inheritance
var
  i: sizeint;
begin
  with j do begin
    SetLength(sa, getArrayLength(jvm));
    for i := 0 to high(sa) do
      sa[i] := T{.}.fromJava(getObjectArrayElement(jvm, i));
    deleteLocalRef(jvm);
  end;
end;

" }

{$r/api/class[@pascal-jvm or @jvm-pascal]/x"
var
  {@id}Class: jclass;
  {@id}ClassInit: jmethodID;
",for $c in $r/api/class[@jvm-pascal] let $fields := ig:properties($c) where exists($fields) return $c/x"
  {@id}Fields: record
    {join($fields!(@name || ig:jni-pascal-suffix(.)), ", ") }: jfieldID;
  end;
"}

{ distinct-values($r/api/class[@pascal-jvm]//array/classref/@ref)!x"
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

{ $r/api/class[@pascal-jvm]/(let $allprops := ig:ancestor-and-self-properties(.) return x"
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
     case element(intenumref) return x"   temp[{$i}].i := ord(self.{$p/@name});&#x0A;"
     case element(comment) return ()
     default return ig:error("unknown property type (toJava)")
   }
    result := newObject({@id}Class, {@id}ClassInit, @temp[0]); 
 {for $p at $i in $allprops where $p[self::array or self::string or self::classref] return x"   deleteLocalRef(temp[{$i - 1}].l);&#x0A;"}
 end;
end;
")
}{ $r/api/class[@jvm-pascal]/(let $allprops := ig:ancestor-and-self-properties(.) return x"
class function T{@id}.fromJava(jvm: jobject): T{@id};
begin
  result := {ig:pascal-call-constructor(.)};
  with j, result, {@id}Fields do begin
 {for $p at $i1 in $allprops return    
   $p/(typeswitch (.) 
     case element(string)|element(int)|element(long)|element(double)|element(boolean) 
     return x"   {@name} := get{name()}Field( jvm, {@name}{ig:jni-pascal-suffix(.)} );&#x0A;"
     case element(array)   return x"   fromJavaArrayAndDelete({@name}, getObjectField( jvm, {@name}{ig:jni-pascal-suffix(.)} ));&#x0A;"
     case element(classref)   return x"   {@name} := T{@ref}.fromJavaAndDelete(getObjectField( jvm, {@name}{ig:jni-pascal-suffix(.)} ));&#x0A;"
     case element(intenumref)   return x"   {@name} := T{@ref}(getIntField( jvm, {@name}I ));&#x0A;"
     case element(comment) return ()
     default return ig:error("unknown property type. (fromJava)")
   )}
 end;
end;
class function T{@id}.fromJavaAndDelete(jvm: jobject): T{@id};
begin
  result := fromJava(jvm);
  j.deleteLocalRef(jvm);
end;
")
}


procedure initBridge;
begin
  with needJ do begin {$r/api/class[@pascal-jvm or @jvm-pascal]/join((x"
    {@id}Class := newGlobalRefAndDelete(getclass('{ig:jni-name(.)}'));
    {@id}ClassInit := getmethod({@id}Class, '<init>', '({join(ig:ancestor-and-self-properties(.)/ig:jni-full-name(.)
      , "")})V');",
    if (@jvm-pascal) then 
      ig:properties(.)/x"    {../@id}Fields.{@name}{ig:jni-pascal-suffix(.)} := getfield({../@id}Class, '{@name}', '{ig:jni-full-name(.)}');"
    else ()
    ), "&#x0A;")}
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
    case element(intenumref) return @ref || "Int"
    case element(array) return ig:kotlin-make-prop-type(*, "Array<", ">")
    default return () 
  ) || $suffix
};

declare function ig:kotlin-prop-default($s){
  $s/(typeswitch (.) 
    case element(string) return '""' 
    case element(int) return "0" 
    case element(long) return "0" 
    case element(double) return "0" 
    case element(boolean) return "false"
    case element(array) return "emptyArray()"
    case element(intenumref) return "0"
    default return "" 
  )
};

declare function ig:kotlin-make-prop($s, $default){
  $s/(typeswitch (.) 
    case element(string)|element(int)|element(long)|element(double)|element(boolean)|element(array)|element(classref)|element(intenumref) 
    return x"{@name}: " || ig:kotlin-make-prop-type(., "", 
      if (@default) then  
        if (self::string) then x' = "{@default}"'
        else " = " || @default
      else if ($default) then " = " || ig:kotlin-prop-default($s) 
      else "" )
    default return () 
  )
};

declare function ig:kotlin-make-intenum($s){
  let $id := $s/@id
  let $values := $s/value
  return
  $s/x"typealias {@id}Int = Int
object {@id} {{
{join($values ! x"    const val {@name} = {(data(@value), position() - 1)[1]}", "&#xA;") }
}}
"

};

declare function ig:kotlin-make-class($s){
  let $a := ig:ancestors($s)
  let $fieldtype := (if ($s/@jvm-pascal) then  "@JvmField " else "") ||  ($s/@kotlin-var, "val")[1] || " "
  let $addDefault := empty(($s/classref))
  let $aprops := ig:properties($a)
  return
  $s/(x"
  {(@kotlin-class, "open")[1]} class {@id}( 
    { join ((
    $aprops!ig:kotlin-make-prop(., $addDefault),
    */(
      if (self::comment) then x"//{.}"
      else ig:kotlin-make-prop(., $addDefault)!concat($fieldtype,.)
    )
  ), ",&#x0A;    ") 
  }
  ) {ig:parent(.)!x": {.}({join($aprops!@name, ", ")})"}  {{
    override fun equals(other: Any?): Boolean =
       other != null &amp;&amp; {join(("javaClass == other.javaClass", "other is " || @id, (*,$aprops)/(
         typeswitch (.)
           case element(string)|element(int)|element(long)|element(double)|element(boolean)|element(classref)|element(intenumref) return concat(@name, " == other.", @name)
           case element(array) return concat(@name, ".contentEquals(other.", @name,")")
           case element(comment) return ()
           default return ig:error("Unknown field type", .)
       )), " &amp;&amp; ")}
    override fun hashCode(): Int =
      super.hashCode() { ig:properties(.)/concat("xor ",
        typeswitch (.)
          case element(string)|element(int)|element(long)|element(double)|element(boolean)|element(classref)|element(intenumref) return @name || ".hashCode()"
          case element(array) return concat(@name, ".contentHashCode()")
          case element(comment) return ()
          default return ig:error("Unknown field type", .)
      , ".rotateLeft(",position(),")" ) }
  }}")
};
declare function ig:kotlin-make($r){
  x"@file:Suppress(""unused"")
package de.benibela.videlibri.jni

{ string-join(($r/api/*/(
  typeswitch(.)
    case element(intenum) return ig:kotlin-make-intenum(.)
    case element(class) return ig:kotlin-make-class(.)
    case element(comment) return ("", "", x:lines(.)!concat("//", .))
    case element(function) return ()
    default return ig:error("unknown element: ", .)
  )), "&#x0A;") }
"};














 
 
declare function ig:pascal-native-arg-type($arg){
  $arg/(typeswitch(.)
      case element(string) return "jobject" 
      case element(array) return "jobject"
      case element(classref) return "jobject"
      case element(intenumref) return "jint"
      case element(int) return "jint"
      case element(long) return "jlong"
      case element(double) return "jdouble"
      case element(boolean) return "jboolean"
      default return ())
};
 
declare function ig:pascal-make-function-native-declaration($f){ 
  $f/x",(name:'{@id}'; signature: '({string-join(arg/ig:jni-full-name(*))}){(return-type/ig:jni-full-name(*),"V")[1]}'; fnPtr: @Java_de_benibela_VideLibri_Bridge_{@id})"
};
 
declare function ig:pascal-make-function($f){ 
  $f/x"
function Java_de_benibela_VideLibri_Bridge_{@id}(env:PJNIEnv; this:jobject{
  arg!concat("; ", @name, ": ", ig:pascal-native-arg-type(*))
}): jobject; cdecl;
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


declare function ig:java-make-type($s){
  $s/(typeswitch (.) 
    case element(string) return "String" 
    case element(int) return "int" 
    case element(long) return "long" 
    case element(double) return "double" 
    case element(boolean) return "boolean" 
    case element(classref) return @ref
    case element(intenumref) return @ref || "Int"
    case element(array) return ig:java-make-type(*) || "[]"
    default return ig:error("ig:java-make-type: unknown type", $s) 
  ) 
};


declare function ig:java-make-function-native-declaration($f){ 
  $f/x"static public native {if (empty(return-type)) then "void" else ig:java-make-type(return-type/*)} {@id}({join(arg/ig:java-make-type(*), ", ")});"
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