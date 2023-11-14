//This file has been generated automatically. Do not edit it, do not read it.
//Refer to the interface.pretty file
unit commoninterface;
{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}{$ModeSwitch advancedrecords}{$ModeSwitch autoderef}
interface
 uses sysutils, xquery.internals.common, xquery, fastjsonreader {$ifdef android}, jni, bbjniutils{$endif};
 type EVideLibriInterfaceException = class(Exception);
 TImportExportFlag = ( eifCurrent = 1, eifHistory = 2, eifConfig = 4, eifPassword = 8 );
 TLibraryTestingInfo = ( tiUnknown = 0, tiYes = 1, tiNo = 2, tiBroken = 3 );
 TBookStatus = ( bsUnknown = 0, bsProblematic = 5, bsNormal = 6, bsOrdered = 8, bsProvided = 9, bsReserved = 10, bsAvailable = 100, bsLend = 101, bsVirtual = 102, bsPresentation = 103, bsInterLoan = 104 );
 TPendingExceptionKind = ( ekUnknown = 0, ekInternet = 1, ekLogin = 2 );

 
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

TVersionInfo = record
  version, platform, buildId: string;
public
  {$ifdef android}
  function toJava: jobject; 
  
  {$endif}

end;  
type 
TBookListDisplayOptionsClass = class of TBookListDisplayOptions;
TBookListDisplayOptions = class
  groupingKey, sortingKey, filterKey: string;
  showHistory, noBorrowedBookDetails, showRenewCount, alwaysFilterOnHistory: boolean;
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;
  constructor create; virtual;
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
  class function fromJava(jvm: jobject): TBookListDisplayOptions; virtual;
  class function fromJavaAndDelete(jvm: jobject): TBookListDisplayOptions; virtual;
  
  {$endif}

end;  
type 
TNotificationConfigClass = class of TNotificationConfig;
TNotificationConfig = class
  lastTitle, lastText: string;
  serviceDelay, lastAskedForPermission: int32;
  lastTime: int64;
  enabled: boolean;
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;
  constructor create; virtual;
  class function fromJSON(const json: string): TNotificationConfig; virtual;
  class function fromJSON(const json: IXQValue): TNotificationConfig; virtual;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); virtual;
  procedure setPropertiesFromJSON(const json: IXQValue); virtual;
  class function typeId: string; virtual;
  class function classFromTypeId(const id: string): TNotificationConfigClass;
  class function fromJSONWithType(const typ: string; const json: IXQValue): TObject;

public
  {$ifdef android}
  function toJava: jobject; virtual;
  class function fromJava(jvm: jobject): TNotificationConfig; virtual;
  class function fromJavaAndDelete(jvm: jobject): TNotificationConfig; virtual;
  
  {$endif}

end;  
type 
TOptionsAndroidOnlyClass = class of TOptionsAndroidOnly;
TOptionsAndroidOnly = class
  importExportFileName: string;
  accountCountBackup: int32;
  logging, hasBeenStartedAtLeastOnce: boolean;
  filterHistory: array of string;
  additionalCertificatesBase64: array of string;
  bookListDisplayOptions: TBookListDisplayOptions;
  notifications: TNotificationConfig;
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;
  constructor create; virtual;
  destructor destroy; override;
  class function fromJSON(const json: string): TOptionsAndroidOnly; virtual;
  class function fromJSON(const json: IXQValue): TOptionsAndroidOnly; virtual;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); virtual;
  procedure setPropertiesFromJSON(const json: IXQValue); virtual;
  class function typeId: string; virtual;
  class function classFromTypeId(const id: string): TOptionsAndroidOnlyClass;
  class function fromJSONWithType(const typ: string; const json: IXQValue): TObject;

public
  {$ifdef android}
  function toJava: jobject; virtual;
  class function fromJava(jvm: jobject): TOptionsAndroidOnly; virtual;
  class function fromJavaAndDelete(jvm: jobject): TOptionsAndroidOnly; virtual;
  
  {$endif}

end;  
type 
TOptionsSharedClass = class of TOptionsShared;
TOptionsShared = class
  nearTime, refreshInterval: int32;
  userLibIds: array of string;
  procedure toJSON(var builder: TJSONXHTMLStrBuilder);
  function toJSON(): string;
  class function fromJSON(const json: string): TOptionsShared; virtual;
  class function fromJSON(const json: IXQValue): TOptionsShared; virtual;
protected
  procedure appendToJSON(var builder: TJSONXHTMLStrBuilder); virtual;
  procedure setPropertiesFromJSON(const json: IXQValue); virtual;
  class function typeId: string; virtual;
  class function classFromTypeId(const id: string): TOptionsSharedClass;
  class function fromJSONWithType(const typ: string; const json: IXQValue): TObject;

public
  {$ifdef android}
  function toJava: jobject; virtual;
  class function fromJava(jvm: jobject): TOptionsShared; virtual;
  class function fromJavaAndDelete(jvm: jobject): TOptionsShared; virtual;
  
  {$endif}

end;  
type 

TImportExportData = record
  flags: int32;
  nativePtr: int64;
  accountsToImport: array of string;
public
  {$ifdef android}
  function toJava: jobject; 
  class function fromJava(jvm: jobject): TImportExportData; static;
  class function fromJavaAndDelete(jvm: jobject): TImportExportData;  static;
  
  {$endif}

end;  
type 
TLibraryVariableClass = class of TLibraryVariable;
TLibraryVariable = class
  name, value: string;
public
  {$ifdef android}
  function toJava: jobject; virtual;
  class function fromJava(jvm: jobject): TLibraryVariable; virtual;
  class function fromJavaAndDelete(jvm: jobject): TLibraryVariable; virtual;
  
  {$endif}

end;  
type 
TLibraryDetailsClass = class of TLibraryDetails;
TLibraryDetails = class
  id, prettyName, prettyNameShort, fhomepageUrl, fcatalogueUrl, fcatalogueUrlFromTemplate, tableComment, accountComment, templateId, email: string;
  segregatedAccounts: boolean;
  variables: array of TLibraryVariable;
  testingSearch: TLibraryTestingInfo;
  testingAccount: TLibraryTestingInfo;
  destructor destroy; override;
public
  {$ifdef android}
  function toJava: jobject; virtual;
  class function fromJava(jvm: jobject): TLibraryDetails; virtual;
  class function fromJavaAndDelete(jvm: jobject): TLibraryDetails; virtual;
  
  {$endif}

end;  
type 

TTemplateDetails = record
  description: string;
  variablesNames: array of string;
  variablesDescription: array of string;
  variablesDefault: array of string;
public
  {$ifdef android}
  function toJava: jobject; 
  
  {$endif}

end;  
type 

TPendingException = record
  accountPrettyNames, error, libraryIds, searchQuery, details, anonymousDetails, firstAccountUser, firstAccountLib: string;
  kind: TPendingExceptionKind;
public
  {$ifdef android}
  function toJava: jobject; 
  
  {$endif}

end;  
type 

TPendingExceptions = record
  exceptions: array of TPendingException;
public
  {$ifdef android}
  function toJava: jobject; 
  
  {$endif}

end; 
 type TFormInputArray = array of TFormInput; TLibraryVariableArray = array of TLibraryVariable; TPendingExceptionArray = array of TPendingException;
 {$ifdef android}
 procedure initBridge;
 {$endif}
 
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
    appendJSONObjectKeyColon('showHistory'); if self.showHistory then append('true') else append('false'); appendJSONObjectComma;
    appendJSONObjectKeyColon('noBorrowedBookDetails'); if self.noBorrowedBookDetails then append('true') else append('false'); appendJSONObjectComma;
    appendJSONObjectKeyColon('showRenewCount'); if self.showRenewCount then append('true') else append('false'); appendJSONObjectComma;
    appendJSONObjectKeyColon('groupingKey'); appendJSONString(self.groupingKey); appendJSONObjectComma;
    appendJSONObjectKeyColon('sortingKey'); appendJSONString(self.sortingKey); appendJSONObjectComma;
    appendJSONObjectKeyColon('filterKey'); appendJSONString(self.filterKey); appendJSONObjectComma;
    appendJSONObjectKeyColon('alwaysFilterOnHistory'); if self.alwaysFilterOnHistory then append('true') else append('false');
    
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
  showHistory := json.getProperty('showHistory').toBoolean();
    noBorrowedBookDetails := json.getProperty('noBorrowedBookDetails').toBoolean();
    showRenewCount := json.getProperty('showRenewCount').toBoolean();
    groupingKey := json.getProperty('groupingKey').toString();
    sortingKey := json.getProperty('sortingKey').toString();
    filterKey := json.getProperty('filterKey').toString();
    alwaysFilterOnHistory := json.getProperty('alwaysFilterOnHistory').toBoolean();
  
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

 constructor TBookListDisplayOptions.create;
begin
  inherited;
  showRenewCount := true; 
  groupingKey := '_dueWeek'; 
  sortingKey := 'dueDate'; 
  alwaysFilterOnHistory := true;
end;
 procedure TNotificationConfig.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin
    appendJSONObjectStart;
    self.appendToJSON(builder);
    appendJSONObjectEnd;
  end;
end;

function TNotificationConfig.toJSON(): string;
var b:  TJSONXHTMLStrBuilder;
begin
  b.init(@result);
  toJSON(b);
  b.final;
end;

 procedure TNotificationConfig.appendToJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin    
    appendJSONObjectKeyColon('enabled'); if self.enabled then append('true') else append('false'); appendJSONObjectComma;
    appendJSONObjectKeyColon('serviceDelay'); appendNumber(self.serviceDelay); appendJSONObjectComma;
    appendJSONObjectKeyColon('lastTime'); appendNumber(self.lastTime); appendJSONObjectComma;
    appendJSONObjectKeyColon('lastTitle'); appendJSONString(self.lastTitle); appendJSONObjectComma;
    appendJSONObjectKeyColon('lastText'); appendJSONString(self.lastText); appendJSONObjectComma;
    appendJSONObjectKeyColon('lastAskedForPermission'); appendNumber(self.lastAskedForPermission);
    
  end;
end;

class function TNotificationConfig.fromJSON(const json: string): TNotificationConfig;
begin
  result := fromJSON(parseJSON(json))
end;
class function TNotificationConfig.fromJSON(const json: IXQValue): TNotificationConfig;
begin
  result := TNotificationConfig.create;
  result.setPropertiesFromJSON(json)
end;

procedure TNotificationConfig.setPropertiesFromJSON(const json: IXQValue);
begin
  enabled := json.getProperty('enabled').toBoolean();
    serviceDelay := json.getProperty('serviceDelay').toInt64();
    lastTime := json.getProperty('lastTime').toInt64();
    lastTitle := json.getProperty('lastTitle').toString();
    lastText := json.getProperty('lastText').toString();
    lastAskedForPermission := json.getProperty('lastAskedForPermission').toInt64();
  
end;

class function TNotificationConfig.typeId: string;
begin
  result := 'NotificationConfig'
end;

  
class function TNotificationConfig.classFromTypeId(const id: string): TNotificationConfigClass;
begin
  ignore(id);
  result := TNotificationConfig
end;
class function TNotificationConfig.fromJSONWithType(const typ: string; const json: IXQValue): TObject;
begin
  result := classFromTypeId(typ).fromJSON(json);
end;

 constructor TNotificationConfig.create;
begin
  inherited;
  enabled := true; 
  serviceDelay := 15;
end;
 procedure TOptionsAndroidOnly.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin
    appendJSONObjectStart;
    self.appendToJSON(builder);
    appendJSONObjectEnd;
  end;
end;

function TOptionsAndroidOnly.toJSON(): string;
var b:  TJSONXHTMLStrBuilder;
begin
  b.init(@result);
  toJSON(b);
  b.final;
end;

 procedure TOptionsAndroidOnly.appendToJSON(var builder: TJSONXHTMLStrBuilder);
var i: sizeint;
begin
  with builder do begin    
    appendJSONObjectKeyColon('logging'); if self.logging then append('true') else append('false'); appendJSONObjectComma;
    appendJSONObjectKeyColon('bookListDisplayOptions'); bookListDisplayOptions.toJSON(builder); appendJSONObjectComma;
    appendJSONObjectKeyColon('filterHistory'); appendJSONArrayStart();
    for i := 0 to high(filterHistory) do begin
      if i > 0 then appendJSONArrayComma();
      filterHistory[i].toJSON(builder);
    end;
    appendJSONArrayEnd(); appendJSONObjectComma;
    appendJSONObjectKeyColon('importExportFileName'); appendJSONString(self.importExportFileName); appendJSONObjectComma;
    appendJSONObjectKeyColon('additionalCertificatesBase64'); appendJSONArrayStart();
    for i := 0 to high(additionalCertificatesBase64) do begin
      if i > 0 then appendJSONArrayComma();
      additionalCertificatesBase64[i].toJSON(builder);
    end;
    appendJSONArrayEnd(); appendJSONObjectComma;
    appendJSONObjectKeyColon('notifications'); notifications.toJSON(builder); appendJSONObjectComma;
    appendJSONObjectKeyColon('hasBeenStartedAtLeastOnce'); if self.hasBeenStartedAtLeastOnce then append('true') else append('false'); appendJSONObjectComma;
    appendJSONObjectKeyColon('accountCountBackup'); appendNumber(self.accountCountBackup);
    
  end;
end;

class function TOptionsAndroidOnly.fromJSON(const json: string): TOptionsAndroidOnly;
begin
  result := fromJSON(parseJSON(json))
end;
class function TOptionsAndroidOnly.fromJSON(const json: IXQValue): TOptionsAndroidOnly;
begin
  result := TOptionsAndroidOnly.create;
  result.setPropertiesFromJSON(json)
end;

procedure TOptionsAndroidOnly.setPropertiesFromJSON(const json: IXQValue);
begin
  logging := json.getProperty('logging').toBoolean();
    bookListDisplayOptions.setPropertiesFromJSON(json.getProperty('bookListDisplayOptions'));
    readArray(filterHistory, json.getProperty('filterHistory'));
    importExportFileName := json.getProperty('importExportFileName').toString();
    readArray(additionalCertificatesBase64, json.getProperty('additionalCertificatesBase64'));
    notifications.setPropertiesFromJSON(json.getProperty('notifications'));
    hasBeenStartedAtLeastOnce := json.getProperty('hasBeenStartedAtLeastOnce').toBoolean();
    accountCountBackup := json.getProperty('accountCountBackup').toInt64();
  
end;

class function TOptionsAndroidOnly.typeId: string;
begin
  result := 'OptionsAndroidOnly'
end;

  
class function TOptionsAndroidOnly.classFromTypeId(const id: string): TOptionsAndroidOnlyClass;
begin
  ignore(id);
  result := TOptionsAndroidOnly
end;
class function TOptionsAndroidOnly.fromJSONWithType(const typ: string; const json: IXQValue): TObject;
begin
  result := classFromTypeId(typ).fromJSON(json);
end;

 constructor TOptionsAndroidOnly.create;
begin
  inherited;
  bookListDisplayOptions := TBookListDisplayOptions.create; 
  notifications := TNotificationConfig.create; 
  accountCountBackup := -1;
end;
 destructor TOptionsAndroidOnly.destroy;
begin 
  bookListDisplayOptions.free; 
  notifications.free;
  inherited;
end; procedure TOptionsShared.toJSON(var builder: TJSONXHTMLStrBuilder);
begin
  with builder do begin
    appendJSONObjectStart;
    self.appendToJSON(builder);
    appendJSONObjectEnd;
  end;
end;

function TOptionsShared.toJSON(): string;
var b:  TJSONXHTMLStrBuilder;
begin
  b.init(@result);
  toJSON(b);
  b.final;
end;

 procedure TOptionsShared.appendToJSON(var builder: TJSONXHTMLStrBuilder);
var i: sizeint;
begin
  with builder do begin    
    appendJSONObjectKeyColon('nearTime'); appendNumber(self.nearTime); appendJSONObjectComma;
    appendJSONObjectKeyColon('refreshInterval'); appendNumber(self.refreshInterval); appendJSONObjectComma;
    appendJSONObjectKeyColon('userLibIds'); appendJSONArrayStart();
    for i := 0 to high(userLibIds) do begin
      if i > 0 then appendJSONArrayComma();
      userLibIds[i].toJSON(builder);
    end;
    appendJSONArrayEnd();
    
  end;
end;

class function TOptionsShared.fromJSON(const json: string): TOptionsShared;
begin
  result := fromJSON(parseJSON(json))
end;
class function TOptionsShared.fromJSON(const json: IXQValue): TOptionsShared;
begin
  result := TOptionsShared.create;
  result.setPropertiesFromJSON(json)
end;

procedure TOptionsShared.setPropertiesFromJSON(const json: IXQValue);
begin
  nearTime := json.getProperty('nearTime').toInt64();
    refreshInterval := json.getProperty('refreshInterval').toInt64();
    readArray(userLibIds, json.getProperty('userLibIds'));
  
end;

class function TOptionsShared.typeId: string;
begin
  result := 'OptionsShared'
end;

  
class function TOptionsShared.classFromTypeId(const id: string): TOptionsSharedClass;
begin
  ignore(id);
  result := TOptionsShared
end;
class function TOptionsShared.fromJSONWithType(const typ: string; const json: IXQValue): TObject;
begin
  result := classFromTypeId(typ).fromJSON(json);
end;

 destructor TLibraryDetails.destroy;
var i: integer;
begin 
  for i := 0 to high(variables) do variables[i].free;
  inherited;
end;

{$ifdef android}


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

 
procedure fromJavaArrayAndDelete(var sa: TLibraryVariableArray; jvm: jarray); //does not support inheritance
var
  i: sizeint;
begin
  with j do begin
    SetLength(sa, getArrayLength(jvm));
    for i := 0 to high(sa) do
      sa[i] := TLibraryVariable.fromJava(getObjectArrayElement(jvm, i));
    deleteLocalRef(jvm);
  end;
end;




var
  FormInputClass: jclass;
  FormInputClassInit: jmethodID;
 
var
  FormSelectClass: jclass;
  FormSelectClassInit: jmethodID;
 
var
  FormParamsClass: jclass;
  FormParamsClassInit: jmethodID;
 
var
  VersionInfoClass: jclass;
  VersionInfoClassInit: jmethodID;
 
var
  BookListDisplayOptionsClass: jclass;
  BookListDisplayOptionsClassInit: jmethodID;
 
var
  NotificationConfigClass: jclass;
  NotificationConfigClassInit: jmethodID;
 
var
  OptionsAndroidOnlyClass: jclass;
  OptionsAndroidOnlyClassInit: jmethodID;
 
var
  OptionsSharedClass: jclass;
  OptionsSharedClassInit: jmethodID;
 
var
  ImportExportDataClass: jclass;
  ImportExportDataClassInit: jmethodID;
 
var
  LibraryVariableClass: jclass;
  LibraryVariableClassInit: jmethodID;
 
var
  LibraryDetailsClass: jclass;
  LibraryDetailsClassInit: jmethodID;
 
var
  TemplateDetailsClass: jclass;
  TemplateDetailsClassInit: jmethodID;
 
var
  PendingExceptionClass: jclass;
  PendingExceptionClassInit: jmethodID;
 
var
  PendingExceptionsClass: jclass;
  PendingExceptionsClassInit: jmethodID;
 
  BookListDisplayOptionsFields: record
    showHistoryZ, noBorrowedBookDetailsZ, showRenewCountZ, groupingKeyS, sortingKeyS, filterKeyS, alwaysFilterOnHistoryZ: jfieldID;
  end;
 
  NotificationConfigFields: record
    enabledZ, serviceDelayI, lastTimeJ, lastTitleS, lastTextS, lastAskedForPermissionI: jfieldID;
  end;
 
  OptionsAndroidOnlyFields: record
    loggingZ, bookListDisplayOptionsL, filterHistoryA, importExportFileNameS, additionalCertificatesBase64A, notificationsL, hasBeenStartedAtLeastOnceZ, accountCountBackupI: jfieldID;
  end;
 
  OptionsSharedFields: record
    nearTimeI, refreshIntervalI, userLibIdsA: jfieldID;
  end;
 
  ImportExportDataFields: record
    accountsToImportA, flagsI, nativePtrJ: jfieldID;
  end;
 
  LibraryVariableFields: record
    nameS, valueS: jfieldID;
  end;
 
  LibraryDetailsFields: record
    idS, prettyNameS, prettyNameShortS, fhomepageUrlS, fcatalogueUrlS, fcatalogueUrlFromTemplateS, tableCommentS, accountCommentS, templateIdS, variablesA, segregatedAccountsZ, emailS, testingSearchI, testingAccountI: jfieldID;
  end;



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
 
function arrayToJArrayCI(const a: TLibraryVariableArray): jobject; overload;
var
  i: Integer;
begin
  with j do begin
    result := newObjectArray(length(a), LibraryVariableClass, nil);
    for i := 0 to high(a) do
      setObjectArrayElementAndDelete(result, i, a[i].toJava);
  end;
end;
 
function arrayToJArrayCI(const a: TPendingExceptionArray): jobject; overload;
var
  i: Integer;
begin
  with j do begin
    result := newObjectArray(length(a), PendingExceptionClass, nil);
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
var temp: array[0..2] of jvalue;
begin
  with j do begin
    temp[0].l := stringToJString(self.version);
    temp[1].l := stringToJString(self.platform);
    temp[2].l := stringToJString(self.buildId);

    result := newObject(VersionInfoClass, VersionInfoClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);
    deleteLocalRef(temp[1].l);
    deleteLocalRef(temp[2].l);

 end;
end;
 
function TBookListDisplayOptions.toJava: jobject;
var temp: array[0..6] of jvalue;
begin
  with j do begin
    temp[0].z := booleanToJboolean(self.showHistory);
    temp[1].z := booleanToJboolean(self.noBorrowedBookDetails);
    temp[2].z := booleanToJboolean(self.showRenewCount);
    temp[3].l := stringToJString(self.groupingKey);
    temp[4].l := stringToJString(self.sortingKey);
    temp[5].l := stringToJString(self.filterKey);
    temp[6].z := booleanToJboolean(self.alwaysFilterOnHistory);

    result := newObject(BookListDisplayOptionsClass, BookListDisplayOptionsClassInit, @temp[0]); 
    deleteLocalRef(temp[3].l);
    deleteLocalRef(temp[4].l);
    deleteLocalRef(temp[5].l);

 end;
end;
 
function TNotificationConfig.toJava: jobject;
var temp: array[0..5] of jvalue;
begin
  with j do begin
    temp[0].z := booleanToJboolean(self.enabled);
    temp[1].i := self.serviceDelay;
    temp[2].j := self.lastTime;
    temp[3].l := stringToJString(self.lastTitle);
    temp[4].l := stringToJString(self.lastText);
    temp[5].i := self.lastAskedForPermission;

    result := newObject(NotificationConfigClass, NotificationConfigClassInit, @temp[0]); 
    deleteLocalRef(temp[3].l);
    deleteLocalRef(temp[4].l);

 end;
end;
 
function TOptionsAndroidOnly.toJava: jobject;
var temp: array[0..7] of jvalue;
begin
  with j do begin
    temp[0].z := booleanToJboolean(self.logging);
    temp[1].l := self.bookListDisplayOptions.toJava;
    temp[2].l := arrayToJArrayCI(self.filterHistory);
    temp[3].l := stringToJString(self.importExportFileName);
    temp[4].l := arrayToJArrayCI(self.additionalCertificatesBase64);
    temp[5].l := self.notifications.toJava;
    temp[6].z := booleanToJboolean(self.hasBeenStartedAtLeastOnce);
    temp[7].i := self.accountCountBackup;

    result := newObject(OptionsAndroidOnlyClass, OptionsAndroidOnlyClassInit, @temp[0]); 
    deleteLocalRef(temp[1].l);
    deleteLocalRef(temp[2].l);
    deleteLocalRef(temp[3].l);
    deleteLocalRef(temp[4].l);
    deleteLocalRef(temp[5].l);

 end;
end;
 
function TOptionsShared.toJava: jobject;
var temp: array[0..2] of jvalue;
begin
  with j do begin
    temp[0].i := self.nearTime;
    temp[1].i := self.refreshInterval;
    temp[2].l := arrayToJArrayCI(self.userLibIds);

    result := newObject(OptionsSharedClass, OptionsSharedClassInit, @temp[0]); 
    deleteLocalRef(temp[2].l);

 end;
end;
 
function TImportExportData.toJava: jobject;
var temp: array[0..2] of jvalue;
begin
  with j do begin
    temp[0].l := arrayToJArrayCI(self.accountsToImport);
    temp[1].i := self.flags;
    temp[2].j := self.nativePtr;

    result := newObject(ImportExportDataClass, ImportExportDataClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);

 end;
end;
 
function TLibraryVariable.toJava: jobject;
var temp: array[0..1] of jvalue;
begin
  with j do begin
    temp[0].l := stringToJString(self.name);
    temp[1].l := stringToJString(self.value);

    result := newObject(LibraryVariableClass, LibraryVariableClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);
    deleteLocalRef(temp[1].l);

 end;
end;
 
function TLibraryDetails.toJava: jobject;
var temp: array[0..13] of jvalue;
begin
  with j do begin
    temp[0].l := stringToJString(self.id);
    temp[1].l := stringToJString(self.prettyName);
    temp[2].l := stringToJString(self.prettyNameShort);
    temp[3].l := stringToJString(self.fhomepageUrl);
    temp[4].l := stringToJString(self.fcatalogueUrl);
    temp[5].l := stringToJString(self.fcatalogueUrlFromTemplate);
    temp[6].l := stringToJString(self.tableComment);
    temp[7].l := stringToJString(self.accountComment);
    temp[8].l := stringToJString(self.templateId);
    temp[9].l := arrayToJArrayCI(self.variables);
    temp[10].z := booleanToJboolean(self.segregatedAccounts);
    temp[11].l := stringToJString(self.email);
    temp[12].i := ord(self.testingSearch);
    temp[13].i := ord(self.testingAccount);

    result := newObject(LibraryDetailsClass, LibraryDetailsClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);
    deleteLocalRef(temp[1].l);
    deleteLocalRef(temp[2].l);
    deleteLocalRef(temp[3].l);
    deleteLocalRef(temp[4].l);
    deleteLocalRef(temp[5].l);
    deleteLocalRef(temp[6].l);
    deleteLocalRef(temp[7].l);
    deleteLocalRef(temp[8].l);
    deleteLocalRef(temp[9].l);
    deleteLocalRef(temp[11].l);

 end;
end;
 
function TTemplateDetails.toJava: jobject;
var temp: array[0..3] of jvalue;
begin
  with j do begin
    temp[0].l := stringToJString(self.description);
    temp[1].l := arrayToJArrayCI(self.variablesNames);
    temp[2].l := arrayToJArrayCI(self.variablesDescription);
    temp[3].l := arrayToJArrayCI(self.variablesDefault);

    result := newObject(TemplateDetailsClass, TemplateDetailsClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);
    deleteLocalRef(temp[1].l);
    deleteLocalRef(temp[2].l);
    deleteLocalRef(temp[3].l);

 end;
end;
 
function TPendingException.toJava: jobject;
var temp: array[0..8] of jvalue;
begin
  with j do begin
    temp[0].i := ord(self.kind);
    temp[1].l := stringToJString(self.accountPrettyNames);
    temp[2].l := stringToJString(self.error);
    temp[3].l := stringToJString(self.libraryIds);
    temp[4].l := stringToJString(self.searchQuery);
    temp[5].l := stringToJString(self.details);
    temp[6].l := stringToJString(self.anonymousDetails);
    temp[7].l := stringToJString(self.firstAccountUser);
    temp[8].l := stringToJString(self.firstAccountLib);

    result := newObject(PendingExceptionClass, PendingExceptionClassInit, @temp[0]); 
    deleteLocalRef(temp[1].l);
    deleteLocalRef(temp[2].l);
    deleteLocalRef(temp[3].l);
    deleteLocalRef(temp[4].l);
    deleteLocalRef(temp[5].l);
    deleteLocalRef(temp[6].l);
    deleteLocalRef(temp[7].l);
    deleteLocalRef(temp[8].l);

 end;
end;
 
function TPendingExceptions.toJava: jobject;
var temp: array[0..0] of jvalue;
begin
  with j do begin
    temp[0].l := arrayToJArrayCI(self.exceptions);

    result := newObject(PendingExceptionsClass, PendingExceptionsClassInit, @temp[0]); 
    deleteLocalRef(temp[0].l);

 end;
end;

class function TBookListDisplayOptions.fromJava(jvm: jobject): TBookListDisplayOptions;
begin
  result := TBookListDisplayOptions.create;
  with j, result, BookListDisplayOptionsFields do begin
    showHistory := getbooleanField( jvm, showHistoryZ );
    noBorrowedBookDetails := getbooleanField( jvm, noBorrowedBookDetailsZ );
    showRenewCount := getbooleanField( jvm, showRenewCountZ );
    groupingKey := getstringField( jvm, groupingKeyS );
    sortingKey := getstringField( jvm, sortingKeyS );
    filterKey := getstringField( jvm, filterKeyS );
    alwaysFilterOnHistory := getbooleanField( jvm, alwaysFilterOnHistoryZ );

 end;
end;
class function TBookListDisplayOptions.fromJavaAndDelete(jvm: jobject): TBookListDisplayOptions;
begin
  result := fromJava(jvm);
  j.deleteLocalRef(jvm);
end;
 
class function TNotificationConfig.fromJava(jvm: jobject): TNotificationConfig;
begin
  result := TNotificationConfig.create;
  with j, result, NotificationConfigFields do begin
    enabled := getbooleanField( jvm, enabledZ );
    serviceDelay := getintField( jvm, serviceDelayI );
    lastTime := getlongField( jvm, lastTimeJ );
    lastTitle := getstringField( jvm, lastTitleS );
    lastText := getstringField( jvm, lastTextS );
    lastAskedForPermission := getintField( jvm, lastAskedForPermissionI );

 end;
end;
class function TNotificationConfig.fromJavaAndDelete(jvm: jobject): TNotificationConfig;
begin
  result := fromJava(jvm);
  j.deleteLocalRef(jvm);
end;
 
class function TOptionsAndroidOnly.fromJava(jvm: jobject): TOptionsAndroidOnly;
begin
  result := TOptionsAndroidOnly.create;
  with j, result, OptionsAndroidOnlyFields do begin
    logging := getbooleanField( jvm, loggingZ );
    bookListDisplayOptions := TBookListDisplayOptions.fromJavaAndDelete(getObjectField( jvm, bookListDisplayOptionsL ));
    fromJavaArrayAndDelete(filterHistory, getObjectField( jvm, filterHistoryA ));
    importExportFileName := getstringField( jvm, importExportFileNameS );
    fromJavaArrayAndDelete(additionalCertificatesBase64, getObjectField( jvm, additionalCertificatesBase64A ));
    notifications := TNotificationConfig.fromJavaAndDelete(getObjectField( jvm, notificationsL ));
    hasBeenStartedAtLeastOnce := getbooleanField( jvm, hasBeenStartedAtLeastOnceZ );
    accountCountBackup := getintField( jvm, accountCountBackupI );

 end;
end;
class function TOptionsAndroidOnly.fromJavaAndDelete(jvm: jobject): TOptionsAndroidOnly;
begin
  result := fromJava(jvm);
  j.deleteLocalRef(jvm);
end;
 
class function TOptionsShared.fromJava(jvm: jobject): TOptionsShared;
begin
  result := TOptionsShared.create;
  with j, result, OptionsSharedFields do begin
    nearTime := getintField( jvm, nearTimeI );
    refreshInterval := getintField( jvm, refreshIntervalI );
    fromJavaArrayAndDelete(userLibIds, getObjectField( jvm, userLibIdsA ));

 end;
end;
class function TOptionsShared.fromJavaAndDelete(jvm: jobject): TOptionsShared;
begin
  result := fromJava(jvm);
  j.deleteLocalRef(jvm);
end;
 
class function TImportExportData.fromJava(jvm: jobject): TImportExportData;
begin
  result := default(TImportExportData);
  with j, result, ImportExportDataFields do begin
    fromJavaArrayAndDelete(accountsToImport, getObjectField( jvm, accountsToImportA ));
    flags := getintField( jvm, flagsI );
    nativePtr := getlongField( jvm, nativePtrJ );

 end;
end;
class function TImportExportData.fromJavaAndDelete(jvm: jobject): TImportExportData;
begin
  result := fromJava(jvm);
  j.deleteLocalRef(jvm);
end;
 
class function TLibraryVariable.fromJava(jvm: jobject): TLibraryVariable;
begin
  result := TLibraryVariable.create;
  with j, result, LibraryVariableFields do begin
    name := getstringField( jvm, nameS );
    value := getstringField( jvm, valueS );

 end;
end;
class function TLibraryVariable.fromJavaAndDelete(jvm: jobject): TLibraryVariable;
begin
  result := fromJava(jvm);
  j.deleteLocalRef(jvm);
end;
 
class function TLibraryDetails.fromJava(jvm: jobject): TLibraryDetails;
begin
  result := TLibraryDetails.create;
  with j, result, LibraryDetailsFields do begin
    id := getstringField( jvm, idS );
    prettyName := getstringField( jvm, prettyNameS );
    prettyNameShort := getstringField( jvm, prettyNameShortS );
    fhomepageUrl := getstringField( jvm, fhomepageUrlS );
    fcatalogueUrl := getstringField( jvm, fcatalogueUrlS );
    fcatalogueUrlFromTemplate := getstringField( jvm, fcatalogueUrlFromTemplateS );
    tableComment := getstringField( jvm, tableCommentS );
    accountComment := getstringField( jvm, accountCommentS );
    templateId := getstringField( jvm, templateIdS );
    fromJavaArrayAndDelete(variables, getObjectField( jvm, variablesA ));
    segregatedAccounts := getbooleanField( jvm, segregatedAccountsZ );
    email := getstringField( jvm, emailS );
    testingSearch := TLibraryTestingInfo(getIntField( jvm, testingSearchI ));
    testingAccount := TLibraryTestingInfo(getIntField( jvm, testingAccountI ));

 end;
end;
class function TLibraryDetails.fromJavaAndDelete(jvm: jobject): TLibraryDetails;
begin
  result := fromJava(jvm);
  j.deleteLocalRef(jvm);
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
    VersionInfoClassInit := getmethod(VersionInfoClass, '<init>', '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V'); 
    BookListDisplayOptionsClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/BookListDisplayOptions'));
    BookListDisplayOptionsClassInit := getmethod(BookListDisplayOptionsClass, '<init>', '(ZZZLjava/lang/String;Ljava/lang/String;Ljava/lang/String;Z)V');
    BookListDisplayOptionsFields.showHistoryZ := getfield(BookListDisplayOptionsClass, 'showHistory', 'Z');
    BookListDisplayOptionsFields.noBorrowedBookDetailsZ := getfield(BookListDisplayOptionsClass, 'noBorrowedBookDetails', 'Z');
    BookListDisplayOptionsFields.showRenewCountZ := getfield(BookListDisplayOptionsClass, 'showRenewCount', 'Z');
    BookListDisplayOptionsFields.groupingKeyS := getfield(BookListDisplayOptionsClass, 'groupingKey', 'Ljava/lang/String;');
    BookListDisplayOptionsFields.sortingKeyS := getfield(BookListDisplayOptionsClass, 'sortingKey', 'Ljava/lang/String;');
    BookListDisplayOptionsFields.filterKeyS := getfield(BookListDisplayOptionsClass, 'filterKey', 'Ljava/lang/String;');
    BookListDisplayOptionsFields.alwaysFilterOnHistoryZ := getfield(BookListDisplayOptionsClass, 'alwaysFilterOnHistory', 'Z'); 
    NotificationConfigClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/NotificationConfig'));
    NotificationConfigClassInit := getmethod(NotificationConfigClass, '<init>', '(ZIJLjava/lang/String;Ljava/lang/String;I)V');
    NotificationConfigFields.enabledZ := getfield(NotificationConfigClass, 'enabled', 'Z');
    NotificationConfigFields.serviceDelayI := getfield(NotificationConfigClass, 'serviceDelay', 'I');
    NotificationConfigFields.lastTimeJ := getfield(NotificationConfigClass, 'lastTime', 'J');
    NotificationConfigFields.lastTitleS := getfield(NotificationConfigClass, 'lastTitle', 'Ljava/lang/String;');
    NotificationConfigFields.lastTextS := getfield(NotificationConfigClass, 'lastText', 'Ljava/lang/String;');
    NotificationConfigFields.lastAskedForPermissionI := getfield(NotificationConfigClass, 'lastAskedForPermission', 'I'); 
    OptionsAndroidOnlyClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/OptionsAndroidOnly'));
    OptionsAndroidOnlyClassInit := getmethod(OptionsAndroidOnlyClass, '<init>', '(ZLde/benibela/videlibri/jni/BookListDisplayOptions;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Lde/benibela/videlibri/jni/NotificationConfig;ZI)V');
    OptionsAndroidOnlyFields.loggingZ := getfield(OptionsAndroidOnlyClass, 'logging', 'Z');
    OptionsAndroidOnlyFields.bookListDisplayOptionsL := getfield(OptionsAndroidOnlyClass, 'bookListDisplayOptions', 'Lde/benibela/videlibri/jni/BookListDisplayOptions;');
    OptionsAndroidOnlyFields.filterHistoryA := getfield(OptionsAndroidOnlyClass, 'filterHistory', '[Ljava/lang/String;');
    OptionsAndroidOnlyFields.importExportFileNameS := getfield(OptionsAndroidOnlyClass, 'importExportFileName', 'Ljava/lang/String;');
    OptionsAndroidOnlyFields.additionalCertificatesBase64A := getfield(OptionsAndroidOnlyClass, 'additionalCertificatesBase64', '[Ljava/lang/String;');
    OptionsAndroidOnlyFields.notificationsL := getfield(OptionsAndroidOnlyClass, 'notifications', 'Lde/benibela/videlibri/jni/NotificationConfig;');
    OptionsAndroidOnlyFields.hasBeenStartedAtLeastOnceZ := getfield(OptionsAndroidOnlyClass, 'hasBeenStartedAtLeastOnce', 'Z');
    OptionsAndroidOnlyFields.accountCountBackupI := getfield(OptionsAndroidOnlyClass, 'accountCountBackup', 'I'); 
    OptionsSharedClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/OptionsShared'));
    OptionsSharedClassInit := getmethod(OptionsSharedClass, '<init>', '(II[Ljava/lang/String;)V');
    OptionsSharedFields.nearTimeI := getfield(OptionsSharedClass, 'nearTime', 'I');
    OptionsSharedFields.refreshIntervalI := getfield(OptionsSharedClass, 'refreshInterval', 'I');
    OptionsSharedFields.userLibIdsA := getfield(OptionsSharedClass, 'userLibIds', '[Ljava/lang/String;'); 
    ImportExportDataClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/ImportExportData'));
    ImportExportDataClassInit := getmethod(ImportExportDataClass, '<init>', '([Ljava/lang/String;IJ)V');
    ImportExportDataFields.accountsToImportA := getfield(ImportExportDataClass, 'accountsToImport', '[Ljava/lang/String;');
    ImportExportDataFields.flagsI := getfield(ImportExportDataClass, 'flags', 'I');
    ImportExportDataFields.nativePtrJ := getfield(ImportExportDataClass, 'nativePtr', 'J'); 
    LibraryVariableClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/LibraryVariable'));
    LibraryVariableClassInit := getmethod(LibraryVariableClass, '<init>', '(Ljava/lang/String;Ljava/lang/String;)V');
    LibraryVariableFields.nameS := getfield(LibraryVariableClass, 'name', 'Ljava/lang/String;');
    LibraryVariableFields.valueS := getfield(LibraryVariableClass, 'value', 'Ljava/lang/String;'); 
    LibraryDetailsClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/LibraryDetails'));
    LibraryDetailsClassInit := getmethod(LibraryDetailsClass, '<init>', '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;[Lde/benibela/videlibri/jni/LibraryVariable;ZLjava/lang/String;II)V');
    LibraryDetailsFields.idS := getfield(LibraryDetailsClass, 'id', 'Ljava/lang/String;');
    LibraryDetailsFields.prettyNameS := getfield(LibraryDetailsClass, 'prettyName', 'Ljava/lang/String;');
    LibraryDetailsFields.prettyNameShortS := getfield(LibraryDetailsClass, 'prettyNameShort', 'Ljava/lang/String;');
    LibraryDetailsFields.fhomepageUrlS := getfield(LibraryDetailsClass, 'fhomepageUrl', 'Ljava/lang/String;');
    LibraryDetailsFields.fcatalogueUrlS := getfield(LibraryDetailsClass, 'fcatalogueUrl', 'Ljava/lang/String;');
    LibraryDetailsFields.fcatalogueUrlFromTemplateS := getfield(LibraryDetailsClass, 'fcatalogueUrlFromTemplate', 'Ljava/lang/String;');
    LibraryDetailsFields.tableCommentS := getfield(LibraryDetailsClass, 'tableComment', 'Ljava/lang/String;');
    LibraryDetailsFields.accountCommentS := getfield(LibraryDetailsClass, 'accountComment', 'Ljava/lang/String;');
    LibraryDetailsFields.templateIdS := getfield(LibraryDetailsClass, 'templateId', 'Ljava/lang/String;');
    LibraryDetailsFields.variablesA := getfield(LibraryDetailsClass, 'variables', '[Lde/benibela/videlibri/jni/LibraryVariable;');
    LibraryDetailsFields.segregatedAccountsZ := getfield(LibraryDetailsClass, 'segregatedAccounts', 'Z');
    LibraryDetailsFields.emailS := getfield(LibraryDetailsClass, 'email', 'Ljava/lang/String;');
    LibraryDetailsFields.testingSearchI := getfield(LibraryDetailsClass, 'testingSearch', 'I');
    LibraryDetailsFields.testingAccountI := getfield(LibraryDetailsClass, 'testingAccount', 'I'); 
    TemplateDetailsClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/TemplateDetails'));
    TemplateDetailsClassInit := getmethod(TemplateDetailsClass, '<init>', '(Ljava/lang/String;[Ljava/lang/String;[Ljava/lang/String;[Ljava/lang/String;)V'); 
    PendingExceptionClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/PendingException'));
    PendingExceptionClassInit := getmethod(PendingExceptionClass, '<init>', '(ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V'); 
    PendingExceptionsClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/PendingExceptions'));
    PendingExceptionsClassInit := getmethod(PendingExceptionsClass, '<init>', '([Lde/benibela/videlibri/jni/PendingException;)V');
  end;
end;
{$endif}
end.
