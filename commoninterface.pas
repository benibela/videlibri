unit commoninterface;
{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}{$ModeSwitch advancedrecords}{$ModeSwitch autoderef}
interface
 uses sysutils, xquery.internals.common, xquery, fastjsonreader {$ifdef android}, jni, bbjniutils{$endif};
 type EVideLibriInterfaceException = class(Exception);
 type TFormInput = class; TFormInputArray = array of TFormInput; 
 
type 
TFormInputClass = class of TFormInput;
TFormInput = class
  name, caption, value: string;
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;
  class function fromJSON(const json: string): TFormInput; virtual;
  class function fromJSON(const json: IXQValue): TFormInput; virtual;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); virtual;
  procedure setPropertiesFromJSON(const json: IXQValue); virtual;
  class function typeId: string; virtual;
  class function classFromTypeId(const id: string): TFormInputClass;
  class function fromJSONWithType(const typ: string; const json: IXQValue): TObject;

public
  {$ifdef android}
  function toJava: jobject; virtual;
  {$endif}

end;  
type 
TFormSelectClass = class of TFormSelect;
TFormSelect = class(TFormInput)
  optionCaptions: array of string;
  optionValues: array of string;
  class function fromJSON(const json: string): TFormSelect; override;
  class function fromJSON(const json: IXQValue): TFormSelect; override;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); override;
  procedure setPropertiesFromJSON(const json: IXQValue); override;
  class function typeId: string; override;

public
  {$ifdef android}
  function toJava: jobject; override;
  {$endif}

end;  
type 
TFormParamsClass = class of TFormParams;
TFormParams = class(TFastInterfacedObject)
  inputs: array of TFormInput;
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;
  destructor destroy; override;
  class function fromJSON(const json: string): TFormParams; virtual;
  class function fromJSON(const json: IXQValue): TFormParams; virtual;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); virtual;
  procedure setPropertiesFromJSON(const json: IXQValue); virtual;
  class function typeId: string; virtual;
  class function classFromTypeId(const id: string): TFormParamsClass;
  class function fromJSONWithType(const typ: string; const json: IXQValue): TObject;

public
  {$ifdef android}
  function toJava: jobject; virtual;
  {$endif}

end;  
type 
TVersionInfoClass = class of TVersionInfo;
TVersionInfo = class
  version, platform: string;
public
  {$ifdef android}
  function toJava: jobject; virtual;
  {$endif}

end;  
type 
TBookListDisplayOptionsClass = class of TBookListDisplayOptions;
TBookListDisplayOptions = class
  groupingKey, sortingKey, filterKey: string;
  noDetailsInOverview, showRenewCount: boolean;
  x: TVersionInfo;
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;
  destructor destroy; override;
  class function fromJSON(const json: string): TBookListDisplayOptions; virtual;
  class function fromJSON(const json: IXQValue): TBookListDisplayOptions; virtual;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); virtual;
  procedure setPropertiesFromJSON(const json: IXQValue); virtual;
  class function typeId: string; virtual;
  class function classFromTypeId(const id: string): TBookListDisplayOptionsClass;
  class function fromJSONWithType(const typ: string; const json: IXQValue): TObject;

public
  {$ifdef android}
  function toJava: jobject; virtual;
  {$endif}

end; 
 {$ifdef android}
 procedure initBridge;
 {$endif}
 
 function parseJSON(const s: string): IXQValue;
implementation
uses bbutils;

function parseJSON(const s: string): IXQValue;
begin
  result := TXQJsonParser.parse(s, [jpoAllowMultipleTopLevelItems, jpoLiberal, jpoAllowTrailingComma]);
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


procedure readArray(var p: TFormInputArray; const json: IXQValue); overload;
var
  objList: TObjectArrayList;
  i: SizeInt;
  callback: TGetObject;
begin
  callback := @TFormInput.fromJSONWithType;
  objList := readObjectArray(json, callback);
  setlength(p, objList.count);
  for i := 0 to high(p) do p[i] := objList[i] as TFormInput
end;        

procedure TFormInput.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin
    appendJSONObjectStart;
    self.appendToJSON(builder);
    appendJSONObjectEnd;
  end;
end;

function TFormInput.toJSON(): string;
var b:  TJSONXHTMLStrBuilder;
begin
  b.init(@result);
  toJSON(b);
  b.final;
end;

 procedure TFormInput.appendToJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin    
    appendJSONObjectKeyColon('name'); appendJSONString(self.name); appendJSONObjectComma;
    appendJSONObjectKeyColon('caption'); appendJSONString(self.caption); appendJSONObjectComma;
    appendJSONObjectKeyColon('value'); appendJSONString(self.value);
    
  end;
end;

class function TFormInput.fromJSON(const json: string): TFormInput;
begin
  result := fromJSON(parseJSON(json))
end;
class function TFormInput.fromJSON(const json: IXQValue): TFormInput;
begin
  result := TFormInput.create;
  result.setPropertiesFromJSON(json)
end;

procedure TFormInput.setPropertiesFromJSON(const json: IXQValue);
begin
  name := json.getProperty('name').toString();
    caption := json.getProperty('caption').toString();
    value := json.getProperty('value').toString();
  
end;

class function TFormInput.typeId: string;
begin
  result := 'FormInput'
end;

  
class function TFormInput.classFromTypeId(const id: string): TFormInputClass;
begin
  case id of
    'FormSelect': result := TFormSelect; 
    else result := TFormInput;
  end;
end;
class function TFormInput.fromJSONWithType(const typ: string; const json: IXQValue): TObject;
begin
  result := classFromTypeId(typ).fromJSON(json);
end;

 procedure TFormSelect.appendToJSON(var builder: TJSONXHTMLStrBuilder);
var i: sizeint;
begin
  inherited;
  builder.appendJSONObjectComma;
  with builder do begin    
    appendJSONObjectKeyColon('optionCaptions'); appendJSONArrayStart();
    for i := 0 to high(optionCaptions) do begin
      if i > 0 then appendJSONArrayComma();
      optionCaptions[i].toJSON(builder);
    end;
    appendJSONArrayEnd(); appendJSONObjectComma;
    appendJSONObjectKeyColon('optionValues'); appendJSONArrayStart();
    for i := 0 to high(optionValues) do begin
      if i > 0 then appendJSONArrayComma();
      optionValues[i].toJSON(builder);
    end;
    appendJSONArrayEnd();
    
  end;
end;

class function TFormSelect.fromJSON(const json: string): TFormSelect;
begin
  result := fromJSON(parseJSON(json))
end;
class function TFormSelect.fromJSON(const json: IXQValue): TFormSelect;
begin
  result := TFormSelect.create;
  result.setPropertiesFromJSON(json)
end;

procedure TFormSelect.setPropertiesFromJSON(const json: IXQValue);
begin
  readArray(optionCaptions, json.getProperty('optionCaptions'));
    readArray(optionValues, json.getProperty('optionValues'));
  inherited;
end;

class function TFormSelect.typeId: string;
begin
  result := 'FormSelect'
end;

 procedure TFormParams.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin
    appendJSONObjectStart;
    self.appendToJSON(builder);
    appendJSONObjectEnd;
  end;
end;

function TFormParams.toJSON(): string;
var b:  TJSONXHTMLStrBuilder;
begin
  b.init(@result);
  toJSON(b);
  b.final;
end;

 procedure TFormParams.appendToJSON(var builder: TJSONXHTMLStrBuilder);
var i: sizeint;
begin
  with builder do begin    
    appendJSONObjectKeyColon('inputs'); appendJSONArrayStart();
    for i := 0 to high(inputs) do begin
      if i > 0 then appendJSONArrayComma();
      appendJSONString(inputs[i].typeId);
      appendJSONArrayComma();
      
      inputs[i].toJSON(builder);
    end;
    appendJSONArrayEnd();
    
  end;
end;

class function TFormParams.fromJSON(const json: string): TFormParams;
begin
  result := fromJSON(parseJSON(json))
end;
class function TFormParams.fromJSON(const json: IXQValue): TFormParams;
begin
  result := TFormParams.create;
  result.setPropertiesFromJSON(json)
end;

procedure TFormParams.setPropertiesFromJSON(const json: IXQValue);
begin
  readArray(inputs, json.getProperty('inputs'));
  
end;

class function TFormParams.typeId: string;
begin
  result := 'FormParams'
end;

  
class function TFormParams.classFromTypeId(const id: string): TFormParamsClass;
begin
  ignore(id);
  result := TFormParams
end;
class function TFormParams.fromJSONWithType(const typ: string; const json: IXQValue): TObject;
begin
  result := classFromTypeId(typ).fromJSON(json);
end;

 destructor TFormParams.destroy;
var i: integer;
begin 
  for i := 0 to high(inputs) do inputs[i].free;
  inherited;
end; procedure TBookListDisplayOptions.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin
    appendJSONObjectStart;
    self.appendToJSON(builder);
    appendJSONObjectEnd;
  end;
end;

function TBookListDisplayOptions.toJSON(): string;
var b:  TJSONXHTMLStrBuilder;
begin
  b.init(@result);
  toJSON(b);
  b.final;
end;

 procedure TBookListDisplayOptions.appendToJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin    
    appendJSONObjectKeyColon('noDetailsInOverview'); if self.noDetailsInOverview then append('true') else append('false'); appendJSONObjectComma;
    appendJSONObjectKeyColon('showRenewCount'); if self.showRenewCount then append('true') else append('false'); appendJSONObjectComma;
    appendJSONObjectKeyColon('groupingKey'); appendJSONString(self.groupingKey); appendJSONObjectComma;
    appendJSONObjectKeyColon('sortingKey'); appendJSONString(self.sortingKey); appendJSONObjectComma;
    appendJSONObjectKeyColon('filterKey'); appendJSONString(self.filterKey);
    
  end;
end;

class function TBookListDisplayOptions.fromJSON(const json: string): TBookListDisplayOptions;
begin
  result := fromJSON(parseJSON(json))
end;
class function TBookListDisplayOptions.fromJSON(const json: IXQValue): TBookListDisplayOptions;
begin
  result := TBookListDisplayOptions.create;
  result.setPropertiesFromJSON(json)
end;

procedure TBookListDisplayOptions.setPropertiesFromJSON(const json: IXQValue);
begin
  noDetailsInOverview := json.getProperty('noDetailsInOverview').toBoolean();
    showRenewCount := json.getProperty('showRenewCount').toBoolean();
    groupingKey := json.getProperty('groupingKey').toString();
    sortingKey := json.getProperty('sortingKey').toString();
    filterKey := json.getProperty('filterKey').toString();
  
end;

class function TBookListDisplayOptions.typeId: string;
begin
  result := 'BookListDisplayOptions'
end;

  
class function TBookListDisplayOptions.classFromTypeId(const id: string): TBookListDisplayOptionsClass;
begin
  ignore(id);
  result := TBookListDisplayOptions
end;
class function TBookListDisplayOptions.fromJSONWithType(const typ: string; const json: IXQValue): TObject;
begin
  result := classFromTypeId(typ).fromJSON(json);
end;

 destructor TBookListDisplayOptions.destroy;
begin 
  inherited;
end;

{$ifdef android}
var 
  FormInputClass: jclass;
  FormInputClassInit: jmethodID;
 
  FormSelectClass: jclass;
  FormSelectClassInit: jmethodID;
 
  FormParamsClass: jclass;
  FormParamsClassInit: jmethodID;
 
  VersionInfoClass: jclass;
  VersionInfoClassInit: jmethodID;
 
  BookListDisplayOptionsClass: jclass;
  BookListDisplayOptionsClassInit: jmethodID;



function arrayToJArrayCI(const a: TFormInputArray): jobject; overload;
var
  i: Integer;
begin
  with j do begin
    result := newObjectArray(length(a), FormInputClass, nil);
    for i := 0 to high(a) do
      setObjectArrayElementAndDelete(result, i, a[i].toJava);
  end;
end;

function arrayToJArrayCI(const a: array of string): jobject; overload;
begin;
  result := j.arrayToJArray(a);
end;


function TFormInput.toJava: jobject;
var temp: array[0..2] of jvalue;
begin
  with j do begin
    temp[0].l := stringToJString(self.name);
    temp[1].l := stringToJString(self.caption);
    temp[2].l := stringToJString(self.value);

    result := newObject(FormInputClass, FormInputClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);
    deleteLocalRef(temp[1].l);
    deleteLocalRef(temp[2].l);

 end;
end;
 
function TFormSelect.toJava: jobject;
var temp: array[0..4] of jvalue;
begin
  with j do begin
    temp[0].l := stringToJString(self.name);
    temp[1].l := stringToJString(self.caption);
    temp[2].l := stringToJString(self.value);
    temp[3].l := arrayToJArrayCI(self.optionCaptions);
    temp[4].l := arrayToJArrayCI(self.optionValues);

    result := newObject(FormSelectClass, FormSelectClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);
    deleteLocalRef(temp[1].l);
    deleteLocalRef(temp[2].l);
    deleteLocalRef(temp[3].l);
    deleteLocalRef(temp[4].l);

 end;
end;
 
function TFormParams.toJava: jobject;
var temp: array[0..0] of jvalue;
begin
  with j do begin
    temp[0].l := arrayToJArrayCI(self.inputs);

    result := newObject(FormParamsClass, FormParamsClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);

 end;
end;
 
function TVersionInfo.toJava: jobject;
var temp: array[0..1] of jvalue;
begin
  with j do begin
    temp[0].l := stringToJString(self.version);
    temp[1].l := stringToJString(self.platform);

    result := newObject(VersionInfoClass, VersionInfoClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);
    deleteLocalRef(temp[1].l);

 end;
end;
 
function TBookListDisplayOptions.toJava: jobject;
var temp: array[0..5] of jvalue;
begin
  with j do begin
    temp[0].z := booleanToJboolean(self.noDetailsInOverview);
    temp[1].z := booleanToJboolean(self.showRenewCount);
    temp[2].l := stringToJString(self.groupingKey);
    temp[3].l := stringToJString(self.sortingKey);
    temp[4].l := stringToJString(self.filterKey);
    temp[5].l := self.x.toJava;

    result := newObject(BookListDisplayOptionsClass, BookListDisplayOptionsClassInit, @temp[0]); 
    deleteLocalRef(temp[2].l);
    deleteLocalRef(temp[3].l);
    deleteLocalRef(temp[4].l);
    deleteLocalRef(temp[5].l);

 end;
end;



procedure initBridge;
begin
  with needJ do begin 
    FormInputClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/FormInput'));
    FormInputClassInit := getmethod(FormInputClass, '<init>', '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V'); 
    FormSelectClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/FormSelect'));
    FormSelectClassInit := getmethod(FormSelectClass, '<init>', '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;[Ljava/lang/String;)V'); 
    FormParamsClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/FormParams'));
    FormParamsClassInit := getmethod(FormParamsClass, '<init>', '([Lde/benibela/videlibri/jni/FormInput;)V'); 
    VersionInfoClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/VersionInfo'));
    VersionInfoClassInit := getmethod(VersionInfoClass, '<init>', '(Ljava/lang/String;Ljava/lang/String;)V'); 
    BookListDisplayOptionsClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/BookListDisplayOptions'));
    BookListDisplayOptionsClassInit := getmethod(BookListDisplayOptionsClass, '<init>', '(ZZLjava/lang/String;Ljava/lang/String;Ljava/lang/String;Lde/benibela/videlibri/jni/VersionInfo;)V');
  end;
end;
{$endif}
end.

