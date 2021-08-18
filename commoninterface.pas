unit commoninterface;
{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}{$ModeSwitch advancedrecords}{$ModeSwitch autoderef}
interface
 uses sysutils, simplexmlparser, xquery.internals.common, fastjsonscanner, jsonscannerhelper {$ifdef android}, jni, bbjniutils{$endif};
 type EVideLibriInterfaceException = class(Exception);
 type TFormInput = class; TFormInputArray = array of TFormInput; 
 
type TFormInput = class
  name, caption, value: string;
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;
  class function fromJSON(json: string): TFormInput;
  class function fromJSON(var json: TJSONScanner): TFormInput;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); virtual;
  class function readTypedObject(var scanner: TJSONScanner): TFormInput; //['type', {obj}]
  class function readObject(obj: TFormInput; var scanner: TJSONScanner): TFormInput;
  procedure readProperty(var scanner: TJSONScanner);  virtual;
  class function typeId: string; virtual;
public
  {$ifdef android}
  function toJava: jobject; virtual;
  {$endif}
end; 
type TFormSelect = class(TFormInput)
  optionCaptions: array of string;
  optionValues: array of string;
  class function fromJSON(json: string): TFormSelect;
  class function fromJSON(var json: TJSONScanner): TFormSelect;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); override;
  class function readTypedObject(var scanner: TJSONScanner): TFormSelect; //['type', {obj}]
  class function readObject(obj: TFormSelect; var scanner: TJSONScanner): TFormSelect;
  procedure readProperty(var scanner: TJSONScanner);  override;
  class function typeId: string; override;
public
  {$ifdef android}
  function toJava: jobject; override;
  {$endif}
end; 
type TFormParams = class(TFastInterfacedObject)
  inputs: array of TFormInput;
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;
  destructor destroy; override;
  class function fromJSON(json: string): TFormParams;
  class function fromJSON(var json: TJSONScanner): TFormParams;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); virtual;
  class function readTypedObject(var scanner: TJSONScanner): TFormParams; //['type', {obj}]
  class function readObject(obj: TFormParams; var scanner: TJSONScanner): TFormParams;
  procedure readProperty(var scanner: TJSONScanner);  virtual;
  class function typeId: string; virtual;
public
  {$ifdef android}
  function toJava: jobject; virtual;
  {$endif}
end; 
type TVersionInfo = class
  version, platform: string;
public
  {$ifdef android}
  function toJava: jobject; virtual;
  {$endif}
end;
 {$ifdef android}
 procedure initBridge;
 {$endif}
implementation
uses bbutils, math;


type TStringArray = array of string;
procedure readArray(var sa: TStringArray; var scanner: TJSONScanner); overload;
var temp: TStringArrayList;
  pname: String;
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


type TFormInputArrayList = specialize TCopyingPtrArrayList<TFormInput>;
procedure readArray(var p: TFormInputArray; var scanner: TJSONScanner); overload;
var temp: TFormInputArrayList;
begin
  scanner.fetchNonWSToken; scanner.expectCurrentToken(tkSquaredBraceOpen);
  temp.init;
  while true do begin
    case scanner.fetchNonWSToken of
      tkString: temp.add(TFormInput.readTypedObject(scanner));
      tkComma: ;
      tkSquaredBraceClose: break;
      tkCurlyBraceOpen: temp.add(TFormInput.readObject(TFormInput.create, scanner));
      else scanner.raiseUnexpectedError();
    end;
  end;

  p := temp.toSharedArray;
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

class function TFormInput.fromJSON(json: string): TFormInput;
var scanner: TJSONScanner;
begin
  scanner := default(TJSONScanner);
  scanner.init(json, [joUTF8, joIgnoreTrailingComma]);
  scanner.fetchNonWSToken;
  result := fromJSON(scanner);
  scanner.done;
end;
class function TFormInput.fromJSON(var json: TJSONScanner): TFormInput;
begin
  result := readObject(TFormInput.create, json);
end;

class function TFormInput.readObject(obj: TFormInput; var scanner: TJSONScanner): TFormInput;
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

class function TFormInput.readTypedObject(var scanner: TJSONScanner): TFormInput; 
var additionalArray: boolean;
begin
  additionalArray := scanner.curtoken = tkSquaredBraceOpen;
  if additionalArray  then scanner.fetchNonWSToken;
  scanner.expectCurrentToken(tkString);
  case scanner.CurTokenString of
  
    'FormInput': result := TFormInput.create; 
    'FormSelect': result := TFormSelect.create;
  else scanner.raiseUnexpectedError();
  end;
  scanner.fetchNonWSToken; scanner.expectCurrentToken(tkComma);
  scanner.fetchNonWSToken;
  result := readObject(result, scanner);
  if additionalArray then begin
    scanner.fetchNonWSToken; scanner.expectCurrentToken(tkSquaredBraceClose);
  end;
end;

procedure TFormInput.readProperty(var scanner: TJSONScanner);
begin
  case scanner.CurTokenString of
    'name': begin
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkColon); 
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkString); 
        name := scanner.CurTokenString;
        scanner.fetchNonWSToken;
      end;
    'caption': begin
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkColon); 
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkString); 
        caption := scanner.CurTokenString;
        scanner.fetchNonWSToken;
      end;
    'value': begin
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkColon); 
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkString); 
        value := scanner.CurTokenString;
        scanner.fetchNonWSToken;
      end;
    else scanner.skipObjectPropertyValue();
  end;
end;

class function TFormInput.typeId: string;
begin
  result := 'FormInput'
end;

 procedure TFormSelect.appendToJSON(var builder: TJSONXHTMLStrBuilder);
var i: sizeint;
begin
  inherited;
  builder.appendJSONObjectComma;
  with builder do begin    
    appendJSONObjectKeyColon('optionCaptions'); appendJSONArrayStart();
    if length(optionCaptions) > 0 then begin
      optionCaptions[0].toJSON(builder);
    end;
    for i := 1 to high(optionCaptions) do begin
      appendJSONArrayComma();
      
      optionCaptions[i].toJSON(builder);
    end;
    appendJSONArrayEnd(); appendJSONObjectComma;
    appendJSONObjectKeyColon('optionValues'); appendJSONArrayStart();
    if length(optionValues) > 0 then begin
      optionValues[0].toJSON(builder);
    end;
    for i := 1 to high(optionValues) do begin
      appendJSONArrayComma();
      
      optionValues[i].toJSON(builder);
    end;
    appendJSONArrayEnd();
    
  end;
end;

class function TFormSelect.fromJSON(json: string): TFormSelect;
var scanner: TJSONScanner;
begin
  scanner := default(TJSONScanner);
  scanner.init(json, [joUTF8, joIgnoreTrailingComma]);
  scanner.fetchNonWSToken;
  result := fromJSON(scanner);
  scanner.done;
end;
class function TFormSelect.fromJSON(var json: TJSONScanner): TFormSelect;
begin
  result := readObject(TFormSelect.create, json);
end;

class function TFormSelect.readObject(obj: TFormSelect; var scanner: TJSONScanner): TFormSelect;
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

class function TFormSelect.readTypedObject(var scanner: TJSONScanner): TFormSelect; 
var additionalArray: boolean;
begin
  additionalArray := scanner.curtoken = tkSquaredBraceOpen;
  if additionalArray  then scanner.fetchNonWSToken;
  scanner.expectCurrentToken(tkString);
  case scanner.CurTokenString of
  
    'FormSelect': result := TFormSelect.create;
  else scanner.raiseUnexpectedError();
  end;
  scanner.fetchNonWSToken; scanner.expectCurrentToken(tkComma);
  scanner.fetchNonWSToken;
  result := readObject(result, scanner);
  if additionalArray then begin
    scanner.fetchNonWSToken; scanner.expectCurrentToken(tkSquaredBraceClose);
  end;
end;

procedure TFormSelect.readProperty(var scanner: TJSONScanner);
begin
  case scanner.CurTokenString of
    'optionCaptions': begin
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkColon); 
        readArray(optionCaptions, scanner);
        scanner.fetchNonWSToken;
      end;
    'optionValues': begin
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkColon); 
        readArray(optionValues, scanner);
        scanner.fetchNonWSToken;
      end;
    else inherited;
  end;
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
    if length(inputs) > 0 then begin
      appendJSONString(inputs[0].typeId);
      appendJSONArrayComma();
      
      inputs[0].toJSON(builder);
    end;
    for i := 1 to high(inputs) do begin
      appendJSONArrayComma();
      
      appendJSONString(inputs[i].typeId);
      appendJSONArrayComma();
      
      inputs[i].toJSON(builder);
    end;
    appendJSONArrayEnd();
    
  end;
end;

class function TFormParams.fromJSON(json: string): TFormParams;
var scanner: TJSONScanner;
begin
  scanner := default(TJSONScanner);
  scanner.init(json, [joUTF8, joIgnoreTrailingComma]);
  scanner.fetchNonWSToken;
  result := fromJSON(scanner);
  scanner.done;
end;
class function TFormParams.fromJSON(var json: TJSONScanner): TFormParams;
begin
  result := readObject(TFormParams.create, json);
end;

class function TFormParams.readObject(obj: TFormParams; var scanner: TJSONScanner): TFormParams;
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

class function TFormParams.readTypedObject(var scanner: TJSONScanner): TFormParams; 
var additionalArray: boolean;
begin
  additionalArray := scanner.curtoken = tkSquaredBraceOpen;
  if additionalArray  then scanner.fetchNonWSToken;
  scanner.expectCurrentToken(tkString);
  case scanner.CurTokenString of
  
    'FormParams': result := TFormParams.create;
  else scanner.raiseUnexpectedError();
  end;
  scanner.fetchNonWSToken; scanner.expectCurrentToken(tkComma);
  scanner.fetchNonWSToken;
  result := readObject(result, scanner);
  if additionalArray then begin
    scanner.fetchNonWSToken; scanner.expectCurrentToken(tkSquaredBraceClose);
  end;
end;

procedure TFormParams.readProperty(var scanner: TJSONScanner);
begin
  case scanner.CurTokenString of
    'inputs': begin
        scanner.fetchNonWSToken; scanner.expectCurrentToken(tkColon); 
        readArray(inputs, scanner);
        scanner.fetchNonWSToken;
      end;
    else scanner.skipObjectPropertyValue();
  end;
end;

class function TFormParams.typeId: string;
begin
  result := 'FormParams'
end;

 destructor TFormParams.destroy;
var i: integer;
begin 
  for i := 0 to high(inputs) do inputs[i].free;
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



function arrayToJArray(const a: TFormInputArray): jobject; overload;
var
  i: Integer;
begin
  with j do begin
    result := newObjectArray(length(a), FormInputClass, nil);
    for i := 0 to high(a) do
      setObjectArrayElementAndDelete(result, i, a[i].toJava);
  end;
end;

function arrayToJArray(const a: array of string): jobject; overload;
begin;
  result := j.arrayToJArray(a);
end;


function TFormInput.toJava: jobject;
var temp: array[0..2] of jvalue;
begin
    temp[0].l := j.stringToJString(self.name);
    temp[1].l := j.stringToJString(self.caption);
    temp[2].l := j.stringToJString(self.value);

  with j do begin
    result := newObject(FormInputClass, FormInputClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);
    deleteLocalRef(temp[1].l);
    deleteLocalRef(temp[2].l);

 end;
end;
 
function TFormSelect.toJava: jobject;
var temp: array[0..4] of jvalue;
begin
    temp[0].l := j.stringToJString(self.name);
    temp[1].l := j.stringToJString(self.caption);
    temp[2].l := j.stringToJString(self.value);
    temp[3].l := arrayToJArray(self.optionCaptions);
    temp[4].l := arrayToJArray(self.optionValues);

  with j do begin
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
    temp[0].l := arrayToJArray(self.inputs);

  with j do begin
    result := newObject(FormParamsClass, FormParamsClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);

 end;
end;
 
function TVersionInfo.toJava: jobject;
var temp: array[0..1] of jvalue;
begin
    temp[0].l := j.stringToJString(self.version);
    temp[1].l := j.stringToJString(self.platform);

  with j do begin
    result := newObject(VersionInfoClass, VersionInfoClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);
    deleteLocalRef(temp[1].l);

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
  end;
end;
{$endif}
end.

