unit androidutils;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils,  IniFiles{$ifdef android}, jni, bbjniutils, libraryParser, LCLProc, booklistreader, librarySearcherAccess, androidinternetaccess, multipagetemplate{$endif};

//procedure deleteLocalRef(jobj: pointer);

var assetPath: string;

procedure beginAssetRead;
function assetFileAsString(name: string): string;
procedure endAssetRead;

function iniFileFromString(data: string): TIniFile;

function getUserConfigPath: string;

function loaded: integer;
procedure uninit;

{$ifdef android}
procedure androidAllThreadsDone();
{$endif}





implementation
uses bbutils, accountlist, applicationconfig, bbdebugtools, libraryAccess, simplehtmltreeparser;

{$ifndef android}
function assetFileAsString(name: string): string;
begin
  if FileExists(userPath + name) then result := strLoadFromFileUTF8(userPath + name)
  else result := strLoadFromFileUTF8(assetPath + name);
end;

function getUserConfigPath: string;
begin
  result := GetAppConfigDir(false);
end;

procedure beginAssetRead;begin end;
procedure endAssetRead;begin end;
procedure uninit;begin end;
function loaded: LongInt; begin end;

{$else}

var assets: jobject = nil;
    assetCount: integer = 0;
    jmAssetManager_Open_StringInputStream, jmInputStream_Read_B, jmInputStream_Close: jmethodID;
    videlibriContextInterface: jclass;
    videLibriContextMethodUserPath: jmethodID;
    bridgeClass: jclass;
    bridgeCallbackMethods: record VLAllThreadsDone, VLInstallationDone: jmethodID; end;
    accountClass, bookClass: jobject;
    accountClassInit, bookClassInit: jmethodID;
    accountFields: record
      LibIdS, NameS, PassS, PrettyNameS, ExtendDaysI, ExtendZ, HistoryZ: jfieldID;
//      internalIdMethod: jmethodID;
    end;
    bookFields: record
      authorS, titleS, issueDateGC, dueDateGC, dueDatePrettyS, accountL, historyZ, statusI: jfieldID;
      setPropertyMethod, getPropertyMethod: jmethodID;
    end;
    gregorianCalenderClass: jclass;
    gregorianCalenderClassInit: jmethodID;
    searcherClass: jclass;
    searcherFields: record
      nativePtrJ, totalResultCountI, nextPageAvailableZ: jfieldID;
    end;
    searcherResultInterface: jclass;
    searcherOnConnected, searcherOnSearchFirstPageComplete, searcherOnSearchNextPageComplete, searcherOnSearchDetailsComplete, searcherOnOrderComplete, searcherOnOrderConfirm,  searcherOnTakePendingMessage, searcherOnPendingMessageCompleted, searcherOnException: jmethodID;

function assetFileAsString(name: string): string;
begin
  if FileExists(userPath + name) then exit(strLoadFromFileUTF8(userPath + name));
  beginAssetRead;
  try
    result := j.getAssetAsString(assets, name, jmAssetManager_Open_StringInputStream, jmInputStream_Read_B, jmInputStream_Close);
  finally
    endAssetRead;
  end;
end;

procedure beginAssetRead;
begin
  if assetCount = 0 then begin
    assets := needJ.getAssets;
    jmAssetManager_Open_StringInputStream := j.commonMethods_AssetManager_Open_StringInputStream;
    jmInputStream_Read_B := j.commonMethods_InputStream_Read_B;
    jmInputStream_Close := j.commonMethods_InputStream_Close;
  end;
  assetCount += 1;
end;

procedure endAssetRead;
begin
  assetCount -= 1;
end;


function getUserConfigPath: string;
begin
  result := j.jStringToStringAndDelete(j.callObjectMethodChecked(jContextObject, videLibriContextMethodUserPath));
end;


procedure uninit;
begin
  if jContextObject <> nil then begin
    try
      finalizeApplicationConfig;
      j.deleteGlobalRef(jContextObject);
      jContextObject:=nil;
    except
      on e: Exception do ;
    end;
  end;
end;

procedure androidAllThreadsDone;
begin
  if logging then bbdebugtools.log('androidAllThreadsDone started');
  needJ.callStaticVoidMethod(bridgeClass, bridgeCallbackMethods.VLAllThreadsDone);
  if logging then bbdebugtools.log('androidAllThreadsDone ended');
end;


procedure Java_de_benibela_VideLibri_Bridge_VLInit(env:PJNIEnv; this:jobject; videlibri: jobject); cdecl;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInit (started)');
  try
    defaultHttpClientClass:=j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/VideLibriHttpClient'));

    videlibriContextInterface :=  j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge$VideLibriContext'));
    jContextObject := needj.env^^.NewGlobalRef(j.env, videlibri);
    videLibriContextMethodUserPath := j.getmethod(videlibriContextInterface, 'userPath', '()Ljava/lang/String;');

    bridgeClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge'));
    bridgeCallbackMethods.VLAllThreadsDone := j.getstaticmethod(bridgeClass, 'allThreadsDone', '()V');
    bridgeCallbackMethods.VLInstallationDone := j.getstaticmethod(bridgeClass, 'installationDone', '(I)V');

    accountClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge$Account'));
    accountClassInit := j.getmethod(accountClass, '<init>', '()V');
    with accountFields  do begin
      LibIdS := j.getfield(accountClass, 'libId', 'Ljava/lang/String;');
      NameS := j.getfield(accountClass, 'name', 'Ljava/lang/String;');
      PassS := j.getfield(accountClass, 'pass', 'Ljava/lang/String;');
      PrettyNameS := j.getfield(accountClass, 'prettyName', 'Ljava/lang/String;');
      ExtendDaysI := j.getfield(accountClass, 'extendDays', 'I');
      ExtendZ := j.getfield(accountClass, 'extend', 'Z');
      HistoryZ := j.getfield(accountClass, 'history', 'Z');
  //    internalIdMethod := j.getmethod(accountClass, 'internalId', '()Ljava/lang/String;');
    end;

    bookClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge$Book'));
    bookClassInit := j.getmethod(bookClass, '<init>', '()V');
    with bookFields do begin
      authorS := j.getfield(bookClass, 'author', 'Ljava/lang/String;');
      titleS := j.getfield(bookClass, 'title', 'Ljava/lang/String;');
      issueDateGC := j.getfield(bookClass, 'issueDate', 'Ljava/util/GregorianCalendar;');
      dueDateGC := j.getfield(bookClass, 'dueDate', 'Ljava/util/GregorianCalendar;');
      dueDatePrettyS := j.getfield(bookClass, 'dueDatePretty', 'Ljava/lang/String;');
      accountL := j.getfield(bookClass, 'account', 'Lde/benibela/videlibri/Bridge$Account;');
      historyZ := j.getfield(bookClass, 'history', 'Z');
      setPropertyMethod := j.getmethod(bookClass, 'setProperty', '(Ljava/lang/String;Ljava/lang/String;)V');
      getPropertyMethod := j.getmethod(bookClass, 'getProperty', '(Ljava/lang/String;)Ljava/lang/String;');
      statusI := j.getfield(bookClass, 'status', 'I');
    end;


    gregorianCalenderClass := j.newGlobalRefAndDelete(j.getclass('java/util/GregorianCalendar'));
    gregorianCalenderClassInit := j.getmethod(gregorianCalenderClass, '<init>', '(III)V');

    searcherClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge$SearcherAccess'));
    with searcherFields do begin
      nativePtrJ := j.getfield(searcherClass, 'nativePtr', 'J');
      totalResultCountI := j.getfield(searcherClass, 'totalResultCount', 'I');
      nextPageAvailableZ := j.getfield(searcherClass, 'nextPageAvailable', 'Z');
    end;
    searcherOnConnected := j.getmethod(searcherClass, 'onConnected', '([Ljava/lang/String;[Ljava/lang/String;)V');
    searcherOnSearchFirstPageComplete := j.getmethod(searcherClass, 'onSearchFirstPageComplete', '([Lde/benibela/videlibri/Bridge$Book;)V');
    searcherOnSearchNextPageComplete := j.getmethod(searcherClass, 'onSearchNextPageComplete', '([Lde/benibela/videlibri/Bridge$Book;)V');
    searcherOnSearchDetailsComplete := j.getmethod(searcherClass, 'onSearchDetailsComplete', '(Lde/benibela/videlibri/Bridge$Book;)V');
    searcherOnOrderComplete := j.getmethod(searcherClass, 'onOrderComplete', '(Lde/benibela/videlibri/Bridge$Book;)V');
    searcherOnOrderConfirm := j.getmethod(searcherClass, 'onOrderConfirm', '(Lde/benibela/videlibri/Bridge$Book;)V');
    searcherOnTakePendingMessage := j.getmethod(searcherClass, 'onTakePendingMessage', '(ILjava/lang/String;[Ljava/lang/String;)V');
    searcherOnPendingMessageCompleted := j.getmethod(searcherClass, 'onPendingMessageCompleted', '()V');
    searcherOnException := j.getmethod(searcherClass, 'onException', '()V');

    beginAssetRead;
    initApplicationConfig;
    endAssetRead;

 // bbdebugtools.OnLog := TStringNotifyEvent(procedureToMethod(TProcedure(@onLog)));
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInit (ended)');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLFInit(env:PJNIEnv; this:jobject); cdecl;
begin
{  try
    finalizeApplicationConfig;
    j.deleteGlobalRef(jContextObject);
    jContextObject:=nil;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;            }
end;

function Java_de_benibela_VideLibri_Bridge_VLGetLibraries(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  lib: TLibrary;
  i: Integer;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraries (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    result := j.newObjectArray(libraryManager.count, j.getclass('java/lang/String'), nil);
    for i := 0 to libraryManager.count - 1 do begin
      lib := libraryManager[i];
      j.env^^.SetObjectArrayElement(j.env, result, i, j.stringToJString(lib.id+'|'+lib.prettyLocation+'|'+lib.prettyNameLong+'|'+lib.prettyNameShort));
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraries (ended)');
end;

type TTemplateDetails = record
  variablesNames, variablesDescription, variablesDefault: jfieldID;
end;
function getTemplateDetailsFields(c: jclass): TTemplateDetails;
begin
  with result, j do begin
    variablesNames := getfield(c, 'variablesNames', '[Ljava/lang/String;');
    variablesDescription := getfield(c, 'variablesDescription', '[Ljava/lang/String;');
    variablesDefault := getfield(c, 'variablesDefault', '[Ljava/lang/String;');
  end;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetTemplateDetails(env:PJNIEnv; this:jobject; id: jstring): jobject; cdecl;
var
  i: Integer;
  detailClass: jclass;
  namesArray: jobject;
  valuesArray: jobject;
  template: TMultiPageTemplate;
  meta: TTemplateActionMeta;
  names: jobject;
  defs: jobject;
  desc: jobject;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetTemplateDetails (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    template := libraryManager.getTemplate(j.jStringToStringAndDelete(id));
    if template = nil then exit(nil);
    meta := nil;
    for i := 0 to high(template.baseActions.children) do
      if template.baseActions.children[i] is TTemplateActionMeta then
        meta := template.baseActions.children[i] as TTemplateActionMeta ;

    if meta = nil then exit(nil);

    detailClass := j.getclass('de/benibela/videlibri/Bridge$TemplateDetails');
    result := j.newObject(detailClass, j.getmethod(detailClass, '<init>', '()V'));
    with getTemplateDetailsFields(detailClass), j do begin
      names := j.newObjectArray(length(meta.variables), j.commonClasses_String, nil);
      defs := j.newObjectArray(length(meta.variables), j.commonClasses_String, nil);
      desc := j.newObjectArray(length(meta.variables), j.commonClasses_String, nil);
      SetObjectField(result, variablesNames, names);
      SetObjectField(result, variablesDefault, defs);
      SetObjectField(result, variablesDescription, desc);

      for i:=0 to high(meta.variables) do begin
        setStringArrayElement(names, i, meta.variables[i].name);
        setStringArrayElement(desc, i, meta.variables[i].description);
        if meta.variables[i].hasDef then
          setStringArrayElement(defs, i, meta.variables[i].def);
      end;
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetTemplateDetails (ended)');
end;


type TLibraryDetails = record
  homepageBase, homepageCatalogue, prettyName, id, templateId, variableNames, variableValues: jfieldID;
end;
function getLibraryDetailsFields(c: jclass): TLibraryDetails;
begin
   with result, j do begin
     homepageBase := getfield(c, 'homepageBase', 'Ljava/lang/String;');
     homepageCatalogue := getfield(c, 'homepageCatalogue', 'Ljava/lang/String;');
     prettyName := getfield(c, 'prettyName', 'Ljava/lang/String;');
     id := getfield(c, 'id', 'Ljava/lang/String;');
     templateId := getfield(c, 'templateId', 'Ljava/lang/String;');
     variableNames := getfield(c, 'variableNames', '[Ljava/lang/String;');
     variableValues := getfield(c, 'variableValues', '[Ljava/lang/String;');
   end;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetLibraryDetails(env:PJNIEnv; this:jobject; id: jstring): jobject; cdecl;
var
  lib: TLibrary;
  i: Integer;
  detailClass: jclass;
  namesArray: jobject;
  valuesArray: jobject;
  libId: String;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraryDetails (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    libId := j.jStringToStringAndDelete(id);
    lib := libraryManager.get(libId);
    detailClass := j.getclass('de/benibela/videlibri/Bridge$LibraryDetails');
    result := j.newObject(detailClass, j.getmethod(detailClass, '<init>', '()V'));

    with getLibraryDetailsFields(detailClass), j do begin
      SetStringField(result, homepageBase, lib.homepageBase);
      SetStringField(result, homepageCatalogue, lib.homepageCatalogue);
      SetStringField(result, prettyName, lib.prettyNameLong);
      SetStringField(result, id, lib.id);
      if lib.template <> nil then SetStringField(result, templateId, lib.template.name)
      else SetStringField(result, templateId, '');
      namesArray := newObjectArray(lib.variables.count, commonClasses_String, nil);
      valuesArray := newObjectArray(lib.variables.count, commonClasses_String, nil);
      for i := 0 to lib.variables.count-1 do begin
        setObjectArrayElement(namesArray, i, stringToJString(lib.variables.Names[i]));
        setObjectArrayElement(valuesArray, i, stringToJString(lib.variables.ValueFromIndex[i]));
      end;
      SetObjectField(result, variableNames, namesArray);
      SetObjectField(result, variableValues, valuesArray);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraryDetails (ended)');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSetLibraryDetails(env:PJNIEnv; this:jobject; id: jstring; details: jobject); cdecl;
var
  lib: TLibrary;
  i: Integer;
  detailClass: jclass;
  namesArray: jobject;
  valuesArray: jobject;
  names: jobject;
  values: jobject;
  libid: String;
  libXml: String;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetLibraryDetails (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    libid := j.jStringToStringAndDelete(id);
    if details = nil then begin
      //null means delete
      libraryManager.deleteUserLibrary(libid);
    end else begin
      detailClass := j.getclass('de/benibela/videlibri/Bridge$LibraryDetails');

      with getLibraryDetailsFields(detailClass), j do begin
        libXml := '<?xml version="1.0" encoding="UTF-8"?>'+LineEnding;
        libXml += '<library>'+LineEnding;
        libXml += '   <longName value="'+xmlStrEscape(getStringField(details, prettyName))+'"/>'+LineEnding;
        if getStringField(details, homepageBase) <> '' then
          libXml += '   <homepage value="'+xmlStrEscape(getStringField(details, homepageBase))+'"/>'+LineEnding;
        if getStringField(details, homepageCatalogue) <> '' then
          libXml += '   <catalogue value="'+xmlStrEscape(getStringField(details, homepageCatalogue))+'"/>'+LineEnding;
        libXml += '   <template value="'+xmlStrEscape(getStringField(details, templateId))+'"/>'+LineEnding;
        names := getObjectField(details, variableNames);
        values:= getObjectField(details, variableValues);
        for i := 0 to getArrayLength(names) - 1 do
          libXml += '   <variable name="'+xmlStrEscape(jStringToStringAndDelete(getObjectArrayElement(names, i)))+'" '+
                                 'value="'+xmlStrEscape(jStringToStringAndDelete(getObjectArrayElement(values, i)))+'"/>'+LineEnding;

        libXml += '</library>';
        libraryManager.setUserLibrary(libid, libXml);
      end;
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetLibraryDetails (ended)');
end;

type

{ TInstallLibraryThread }

TInstallLibraryThread = class(TThread)
  url: string;
  constructor Create(aurl: string);
  procedure Execute; override;
end;

constructor TInstallLibraryThread.Create(aurl: string);
begin
  inherited Create(true);
  url := aurl;
  FreeOnTerminate:=true;
end;

procedure TInstallLibraryThread.Execute;
var
  ok: jint;
begin
  ok := -1;
  if logging then bbdebugtools.log('TInstallLibraryThread (started)');
  try
    needJ;
    libraryManager.downloadAndInstallUserLibrary(url);
    ok := 1;
  except
    on e:Exception do storeException(e, nil, '', url);
  end;
  needJ.callStaticVoidMethod(bridgeClass, bridgeCallbackMethods.VLInstallationDone, @ok);
  if logging then bbdebugtools.log('TInstallLibraryThread (ended)');
  jvmref^^.DetachCurrentThread(jvmref);
end;


function Java_de_benibela_VideLibri_Bridge_VLInstallLibrary(env:PJNIEnv; this:jobject; url: jobject): jobject; cdecl;
var
  surl: string;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInstallLibrary (started)');
  try
    surl := j.jStringToStringAndDelete(url);
    TInstallLibraryThread.Create(surl).Start;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInstallLibrary (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLGetTemplates(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  i: Integer;
 // templ: String;
  templates: TStringList;
  path: jobject;
  list: jobject;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetTemplates (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    beginAssetRead;

    path := j.stringToJString('libraries/templates');
    list:= j.callObjectMethodChecked(assets, j.getmethod('android/content/res/AssetManager', 'list', '(Ljava/lang/String;)[Ljava/lang/String;'), @path);
    j.deleteLocalRef(path);

    endAssetRead;


    templates := TStringList.Create;

    for i := 0 to j.getArrayLength(list) - 1 do
      templates.Add(j.getStringArrayElement(list, i));
    templates.Sorted:=true;

    for i := 0 to libraryManager.templates.count - 1 do
      if not strContains(libraryManager.templates[i], '|') and (templates.IndexOf(libraryManager.templates[i])<0)  then
        templates.Add(libraryManager.templates[i]);

    result := j.newObjectArray(templates.Count, j.getclass('java/lang/String'), nil);
    for i := 0 to templates.count - 1 do
      j.setObjectArrayElement(result, i, j.stringToJString(templates[i]));

    templates.free;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetTemplates (ended)');
end;


function Java_de_benibela_VideLibri_Bridge_VLAddAccount(env:PJNIEnv; this:jobject; acc: jobject): jobject; cdecl;
var
  temp: TExtendType;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLAddAccount (started)');
  try
    with accountFields do begin
      if j.getBooleanField(acc, ExtendZ) then temp := etSingleDepends
      else temp := etNever;
      {bbdebugtools.log('AddAccount B');
      bbdebugtools.log('AddAccount B: '+j.getStringField(acc, LibIdS));
      bbdebugtools.log('AddAccount B: '+j.getStringField(acc, PrettyNameS));
      bbdebugtools.log('AddAccount B: '+j.getStringField(acc, NameS));
      bbdebugtools.log('AddAccount B: '+j.getStringField(acc, PassS));}
      accounts.Add(j.getStringField(acc, LibIdS) , j.getStringField(acc, PrettyNameS),
                   j.getStringField(acc, NameS), j.getStringField(acc, PassS),
                   temp, j.getIntField(acc, ExtendDaysI), j.getBooleanField(acc, HistoryZ));
    end;
    accounts.save;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLAddAccount (ended)');
end;

function findAccountIndex(jacc: jobject): integer;
var
  libId: String;
  pretty: String;
  name: String;
  i: Integer;
begin
  with accountFields do begin
    libId := j.getStringField(jacc, LibIdS);
    pretty := j.getStringField(jacc, PrettyNameS);
    name := j.getStringField(jacc, NameS);
  end;

  for i := 0 to accounts.count-1 do
    if (accounts[i].prettyName = pretty) and (accounts[i].getLibrary().id = libId) and (accounts[i].getUser() = name) then
      exit(i);
  exit(-1);
end;

function Java_de_benibela_VideLibri_Bridge_VLDeleteAccount(env:PJNIEnv; this:jobject; acc: jobject): jobject; cdecl;
var
  oldAcc: Integer;
  temp: TCustomAccountAccess;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLDeleteAccount (started)');
  try
    oldAcc := findAccountIndex(acc);
    if oldAcc >= 0 then begin
      temp := accounts[oldAcc];
      accounts.Delete(oldAcc);
      accounts.save;
      temp.remove();
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLDeleteAccount (ended)');
end;


procedure changeAccount(
      i: integer;
      enabled: boolean;
      prettyName, username, password: string;
      keepHistory: boolean;
      extendType: TExtendType; extendDays: integer);
var oldacc: TCustomAccountAccess;
begin
  oldacc := accounts[i];
  oldacc.prettyName:=prettyName;
  oldacc.passWord:=password;
  oldacc.keepHistory:=keepHistory;

  oldacc.extendType:=extendType;
  oldacc.extendDays:=extendDays;

  oldacc.enabled:=enabled;

  if oldacc.getUser() <> username then begin
    oldacc.changeUser(username);
    accounts.Strings[i] := oldacc.getPlusEncodedID();
    accounts.save;
  end else
    oldacc.save;
end;

function Java_de_benibela_VideLibri_Bridge_VLChangeAccount(env:PJNIEnv; this:jobject; joldacc, jnewacc: jobject): jobject; cdecl;
var
  oldAccI: Integer;
  temp: TExtendType;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLChangeAccount (started)');
  try
    oldAccI := findAccountIndex(joldacc);
    if oldAccI >= 0 then
      with accountFields do begin
        if j.getBooleanField(jnewacc, ExtendZ) then temp := etSingleDepends
        else temp := etNever;
        changeAccount(oldAccI,
                      true,
                      j.getStringField(jnewacc, PrettyNameS), j.getStringField(jnewacc, NameS), j.getStringField(jnewacc, PassS),
                      j.getBooleanField(jnewacc, HistoryZ),
                      temp, j.getIntField(jnewacc, ExtendDaysI)
                      );
      end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLChangeAccount (ended)');
end;


function Java_de_benibela_VideLibri_Bridge_VLGetAccounts(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  i: Integer;
  temp: jobject;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetAccounts (started)');
  try
    result := j.newObjectArray(accounts.Count, accountClass, nil);
    for i := 0 to accounts.Count - 1 do begin
      temp := j.newObject(accountClass, accountClassInit);
      j.SetObjectArrayElement(result, i,  temp);
      with accountFields  do begin
        j.SetObjectField(temp, LibIdS, j.stringToJString(accounts[i].getLibrary().id));
        j.SetObjectField(temp, NameS, j.stringToJString(accounts[i].getUser()));
        j.SetObjectField(temp, PassS, j.stringToJString(accounts[i].passWord));
        j.SetObjectField(temp, PrettyNameS, j.stringToJString(accounts[i].prettyName));
        j.SetIntField(temp, ExtendDaysI, accounts[i].extendDays);
        j.SetBooleanField(temp, ExtendZ, accounts[i].extendType <> etNever);
        j.SetBooleanField(temp, HistoryZ, accounts[i].keepHistory);
      end;
      j.deleteLocalRef(temp);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetAccounts (ended)');
end;

function getRealAccount(acc: jobject): TCustomAccountAccess;
var
  libId: String;
  user: String;
  i: Integer;
begin
  libId := needj.getStringField(acc, accountFields.LibIdS);
  user := j.getStringField(acc, accountFields.NameS);
  log('Account: '+libId+' '+user);
  for i := 0 to accounts.Count-1 do
    if (accounts[i].getUser() = user) and (accounts[i].getLibrary().id = libId) then
      exit(accounts[i]);
  result := nil;
end;

function getRealAccountChecked(acc: jobject): TCustomAccountAccess;
begin
  result := getRealAccount(acc);
  if result = nil then
    needJ.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Konto nicht gefunden: '+needj.getStringField(acc, accountFields.LibIdS)+':'+ j.getStringField(acc, accountFields.NameS));
end;

type

{ TJBookSerializer }

TJBookSerializer = object
  book: jobject;
  includeDates: boolean;
  procedure writeProp(n,v:string);
  procedure writeDateProp(n: string; d: integer);
end;



procedure TJBookSerializer.writeProp(n, v: string);
var args: array[0..1] of jvalue;
begin
  case n of
  'title': j.SetStringField(book, bookFields.titleS, v);
  'author': j.SetStringField(book, bookFields.authorS, v);
  else begin
    args[0].l := j.stringToJString(n);
    args[1].l := j.stringToJString(v);
    j.callVoidMethod(book, bookFields.setPropertyMethod, @args);
    j.deleteLocalRef(args[0].l);
    j.deleteLocalRef(args[1].l);
  end;
  end;
end;

procedure TJBookSerializer.writeDateProp(n: string; d: integer);
var args: array[0..2] of jvalue;
    temp: jobject;
    field: jfieldID;
begin
  if not includeDates then exit;
  case n of
  'issueDate': begin field := bookFields.issueDateGC; if d = 0 then exit; end;
  'dueDate': field := bookFields.dueDateGC;
  else exit;
  end;
  if d = 0 then j.SetObjectField(book, field, nil)
  else begin
    dateDecode(d, @args[0].i, @args[1].i, @args[2].i);
    args[1].i -= 1;
    temp := j.newObject(gregorianCalenderClass, gregorianCalenderClassInit, @args);
    j.SetObjectField(book, field, temp);
    j.deleteLocalRef(temp);
  end;

  if bookFields.dueDateGC = bookFields.dueDateGC then
    j.SetStringField(book, bookFields.dueDatePrettyS, DateToPrettyStr(d));
end;

function bookToJBook(book: TBook; includeDates: boolean = true; includeAllStrings: boolean = false): jobject;
var temp: TJBookSerializer;
  i: Integer;
begin
  temp.book := j.newObject(bookClass, bookClassInit);
  temp.includeDates := includeDates;
  book.serialize(@temp.writeProp,@temp.writeDateProp);
  case book.status of
    bsNormal, bsCuriousInStr: j.SetIntField(temp.book, bookFields.statusI, 1);
    bsProblematicInStr: j.SetIntField(temp.book, bookFields.statusI, 2);
    bsOrdered: j.SetIntField(temp.book, bookFields.statusI, 3);
    bsProvided: j.SetIntField(temp.book, bookFields.statusI, 4);
    else j.SetIntField(temp.book, bookFields.statusI, 0);
  end;
  if includeAllStrings then
    for i := 0 to high(book.additional) do
      temp.writeProp(book.additional[i].name, book.additional[i].value);
  result := temp.book;
end;

function jbookToBook(jbook: jobject): TBook;
var
  more: jobject;
  get: jmethodID;
  iv: jvalue;
  first: jfieldID;
  second: jfieldID;
  pair: jobject;
  size: jint;
  i: Integer;
  jaccount: jobject;
  accountId: String;
  accountIndex: Integer;
begin
  result := TBook.create;
  with bookFields do begin
    result.author := j.getStringField(jbook, authorS);
    result.title := j.getStringField(jbook, titleS);
    more := j.getObjectField(jbook, j.getfield(bookClass, 'more', 'Ljava/util/ArrayList;'));

    size := j.callIntMethodChecked(more, j.getmethod('java/util/ArrayList', 'size', '()I'));
    get := j.getmethod('java/util/ArrayList', 'get', '(I)Ljava/lang/Object;');
    first := j.getfield('de/benibela/videlibri/Bridge$Book$Pair', 'first', 'Ljava/lang/String;');
    second := j.getfield('de/benibela/videlibri/Bridge$Book$Pair', 'second', 'Ljava/lang/String;');
    for i := 0 to size - 1 do begin
      iv.i:=i;
      pair := j.callObjectMethodChecked(more, get, @iv);
      result.setProperty(j.getStringField(pair, first), j.getStringField(pair, second));
      j.deleteLocalRef(pair);
    end;


    jaccount := j.getObjectField(jbook, accountL);
    if jaccount <> nil then begin
      result.owner := getRealAccount(jaccount);
      j.deleteLocalRef(jaccount);
    end;

    {entrySet := j.callObjectMethodChecked(more, j.getmethod('java/util/Map', 'entrySet', '()Ljava/util/Set;'));
    iterator := j.callObjectMethodChecked(entrySet, j.getmethod('java/util/Set', 'iterator', '()Ljava/util/Iterator;'));

    hasNext := j.getmethod('java/util/Iterator', 'hasNext', '()Z');
    next := j.getmethod('java/util/Iterator', 'next', '()Ljava/lang/Object;');
    getKey := j.getmethod('java/util/Map$Entry', 'getKey', '()Ljava/lang/Object;');
    getValue := j.getmethod('java/util/Map$Entry', 'getValue', '()Ljava/lang/Object;');

    while j.callBooleanMethodChecked(iterator, hasNext) do begin
      entry := j.callObjectMethodChecked(iterator, next);
      result.setProperty(j.jStringToStringAndDelete(j.callObjectMethodChecked(entry, getKey)),
                       j.jStringToStringAndDelete(j.callObjectMethodChecked(entry, getValue)));
      j.deleteLocalRef(entry);
    end;}
  end;
end;

function jbookToBookAndDelete(jbook: jobject): TBook;
begin
  result := jbookToBook(jbook);
  j.deleteLocalRef(jbook);
end;

function Java_de_benibela_VideLibri_Bridge_VLGetBooks(env:PJNIEnv; this:jobject; jacc: jobject; jhistory: jboolean): jobject; cdecl;
var
  acc: TCustomAccountAccess;
  history: Boolean;
  books: TBookList;
  i: Integer;
  book: jobject;
begin
  if logging then bbdebugtools.log('Java_de_benibela_VideLibri_Bridge_VLGetBooks started');
  try
    acc := getRealAccountChecked(jacc);
    if acc = nil then exit;
  except
    on e: Exception do begin
      j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
      exit;
    end;
  end;
  history := jhistory <> JNI_FALSE;

  if logging then bbdebugtools.log(acc.prettyName);

  system.EnterCriticalSection(updateThreadConfig.libraryAccessSection);


  try
    try
      if history then books := acc.books.old
      else books := acc.books.current;

      result := j.newObjectArray(books.Count, bookClass, nil);
      for i := 0 to books.Count - 1 do begin
        book := bookToJBook(books[i]);
        j.SetBooleanField(book, bookFields.historyZ, history);
        j.SetObjectField(book, bookFields.accountL, jacc);
        j.SetObjectArrayElement(result, i, book);
        j.deleteLocalRef(book);
      end;
    finally
      system.LeaveCriticalsection(updateThreadConfig.libraryAccessSection)
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;

  if logging then bbdebugtools.log('Java_de_benibela_VideLibri_Bridge_VLGetBooks ended');

end;

function Java_de_benibela_VideLibri_Bridge_VLUpdateAccounts(env:PJNIEnv; this:jobject; jacc: jobject; jautoupdate: jboolean; jforceExtend: jboolean): jboolean; cdecl;
var
  acc: TCustomAccountAccess;
  autoupdate: boolean;
  forceExtend: boolean;
begin
  if logging then log('VLUpdateAccounts');
  result := JNI_FALSE;
  try
    acc := getRealAccountChecked(jacc);
    if acc = nil then begin bbdebugtools.log('x'); exit;end;

    autoupdate := jautoupdate <> JNI_FALSE;
    forceExtend := jforceExtend <> JNI_FALSE;

    //if ignoreConnErrors and (account.broken = currentDate) then
    //  exit;

 {   EnterCriticalSection(updateThreadConfig.threadManagementSection);
    updateThreadConfig.updateThreadsRunning+=1;
    updateThreadConfig.listUpdateThreadsRunning+=1;
    LeaveCriticalSection(updateThreadConfig.threadManagementSection);
    acc.isThreadRunning:=true;

    updateBooksDirectBlocking(acc, @updateThreadConfig, autoupdate, autoupdate, forceExtend);   }
    if updateAccountBookData(acc, autoupdate, autoupdate, forceExtend) then
      result := JNI_TRUE;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('VLUpdateAccounts ended');

end;

function Java_de_benibela_VideLibri_Bridge_VLBookOperation(env:PJNIEnv; this:jobject; jbooks: jobject; operation: jint): jobject; cdecl;
var
  acc: TCustomAccountAccess;
  autoupdate: boolean;
  forceExtend: boolean;
  books: TBookList;
  book: TBook;
  i: Integer;
begin
  if logging then log('VLBookOperation');

  try
    books := TBookList.create();
    books.Capacity:=j.getArrayLength(jbooks);
    for i := 0 to j.getArrayLength(jbooks) - 1 do begin
      book := jbookToBookAndDelete(j.getObjectArrayElement(jbooks, i));
      if book.owner <> nil then books.add(book);
    end;
    case operation of
      1: extendBooks(books);
      2: cancelBooks(books);
    end;
    books.free;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('VLBookOperation ended');

  result := nil;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetPendingExceptions(env:PJNIEnv; this:jobject): jobject;
var
  pendingExceptionClass: jclass;
  pendingExceptionClassInit: jmethodID;
  pendingExceptionFields: record
    accountPrettyNamesS, errorS, libraryS, searchQueryS, detailsS, anonymousDetailsS: jfieldID;
  end;


var
  details, anonymousDetails, libs, queries: String;
  names: String;
  i: Integer;
  temp: jobject;
  k: Integer;
begin
  if logging then bbdebugtools.log('Bridge_VLGetPendingExceptions started');


  try
    pendingExceptionClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/Bridge$PendingException'));
    pendingExceptionClassInit := j.getmethod(pendingExceptionClass, '<init>', '()V');
    with pendingExceptionFields do begin
      accountPrettyNamesS := j.getfield(pendingExceptionClass, 'accountPrettyNames', 'Ljava/lang/String;');
      errorS := j.getfield(pendingExceptionClass, 'error', 'Ljava/lang/String;');
      detailsS := j.getfield(pendingExceptionClass, 'details', 'Ljava/lang/String;');
      libraryS := j.getfield(pendingExceptionClass, 'library', 'Ljava/lang/String;');
      searchQueryS := j.getfield(pendingExceptionClass, 'searchQuery', 'Ljava/lang/String;');
      anonymousDetailsS := j.getfield(pendingExceptionClass, 'anonymousDetails', 'Ljava/lang/String;');
    end;

    system.EnterCriticalSection(exceptionStoring);
    try
      result := j.newObjectArray(length(errorMessageList), pendingExceptionClass, nil);
      for i :=  0 to high(errorMessageList) do begin
        temp := j.newObject(pendingExceptionClass, pendingExceptionClassInit);
        with pendingExceptionFields do begin
          details := '';
          anonymousDetails := '';
          names := '';
          libs := '';
          for k := 0 to high(errorMessageList[i].details) do begin
            details += errorMessageList[i].details[k].details+LineEnding+LineEnding;
            anonymousDetails += errorMessageList[i].details[k].anonymouseDetails+LineEnding+LineEnding;
            if not strContains(libs, errorMessageList[i].details[k].libraryId) then
              libs += errorMessageList[i].details[k].libraryId + ' ';
            if errorMessageList[i].details[k].searchQuery <> '' then
              queries += errorMessageList[i].details[k].searchQuery + ' ';
            if names <> '' then names += ', ';
            if errorMessageList[i].details[k].account <> nil then
              names += errorMessageList[i].details[k].account.prettyName;
          end;
          j.SetStringField(temp, detailsS, details);
          j.SetStringField(temp, anonymousDetailsS, anonymousDetails);
          j.SetStringField(temp, accountPrettyNamesS, names);
          j.SetStringField(temp, errorS, errorMessageList[i].error);
          j.SetStringField(temp, libraryS, libs);
          j.SetStringField(temp, searchQueryS, queries);
        end;
        j.SetObjectArrayElement(result, i, temp);
        j.deleteLocalRef(temp);
      end;
      SetLength(errorMessageList, 0);
    finally
      system.LeaveCriticalSection(exceptionStoring);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('Bridge_VLGetPendingExceptions ended');
end;

function Java_de_benibela_VideLibri_Bridge_VLGetNotifications(env:PJNIEnv; this:jobject): jobject;
    function grammar(number: integer): string;
    begin
      if number = 1 then exit('1 Buch')
      else exit(IntToStr(number)+' Bücher');
    end;
    function grammar(number: integer; adjective:string): string;
    begin
      if number = 1 then exit('1 '+adjective+'s Buch')
      else exit(IntToStr(number)+' '+adjective+' Bücher');
    end;
var title, text: string;
  booksOverdue: TList;
  booksSoonNotExtendable: TList;
  booksSoon: TList;
  minDateOverdue: integer;
  minDateSoonNotExtendable: integer;
  minDateSoon: integer;
begin
  if logging then bbdebugtools.log('Bridge_VLGetNotifications started');

  try
    title := '';
    text := '';


    findBooksThatMustBeReturned(booksOverdue, booksSoonNotExtendable, booksSoon,
                                minDateOverdue, minDateSoonNotExtendable, minDateSoon);

    if ((lastWarnDate + WarnInterval <= currentDate) and (booksOverdue.Count + booksSoonNotExtendable.Count + booksSoon.Count > 0))
       or (booksOverdue.Count > 0) then begin
      if booksOverdue.Count > 0 then begin
        title := grammar(booksOverdue.Count, 'überfällige')+' ! ';
        text := format('%s seit %s' + LineEnding,
                      [grammar(booksOverdue.Count), DateToPrettyGrammarStr('dem ','',minDateOverdue)]);
      end;
      if (booksSoonNotExtendable.Count > 0) or (booksSoon.Count > 0) then begin
        title += grammar(booksSoonNotExtendable.Count+booksSoon.Count, 'fällige');
        if booksSoonNotExtendable.Count > 0 then begin
          if text <> '' then text += '; ';
          text += format('%s bis %s',
                        [grammar(booksSoonNotExtendable.Count), DateToPrettyGrammarStr('zum ','',minDateSoonNotExtendable)]);
        end;
        if booksSoon.Count > 0 then begin
          if text <> '' then text += '; ';
          text += format('%s bis %s',
                       [grammar(booksSoon.Count, 'verlängerbare'), DateToPrettyGrammarStr('zum ','',minDateSoon)]);
        end;
      end;
    end;

    if title = '' then result := nil
    else begin
      result := j.newObjectArray(2, j.getclass('java/lang/String'), nil);
      j.SetObjectArrayElement(result, 0, j.stringToJString(title));
      j.SetObjectArrayElement(result, 1, j.stringToJString(text));
    end;

    booksSoon.Free;
    booksSoonNotExtendable.free;
    booksOverdue.Free;

    lastWarnDate:=currentDate;
    userConfig.WriteInteger('base','last-warn-date',currentDate);
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;

  if logging then bbdebugtools.log('Bridge_VLGetNotifications ended');
end;


type

{ TLibrarySearcherAccessWrapper }

 TLibrarySearcherAccessWrapper = class(TLibrarySearcherAccess)
  jsearcher: jobject;
  tempBooks: TBookList;
  pendingMessage: TPendingMessage;
  pendingMessageBook: TBook;
  procedure OnConnectedImpl(sender: TObject);
  procedure OnSearchPageCompleteImpl(sender: TObject; firstPage, nextPageAvailable: boolean);
  procedure OnDetailsCompleteImpl(sender: TObject; book: TBook);
  procedure OnOrderCompleteImpl(sender: TObject; book: TBook);
  procedure OnOrderConfirmImpl(sender: TObject; book: TBook);
  procedure OnTakePendingMessageImpl(sender: TObject; book: TBook; apendingMessage: TPendingMessage);
  procedure OnPendingMessageCompletedImpl(Sender: TObject);
  procedure OnExceptionImpl(Sender: TObject);
  constructor create;
  destructor destroy; override;
end;

procedure TLibrarySearcherAccessWrapper.OnConnectedImpl(sender: TObject);
var args: array[0..1] of jvalue;
begin
  needJ;
  args[0].l := j.arrayToJArray(searcher.HomeBranches) ;
  args[1].l := j.arrayToJArray(searcher.SearchBranches);
  j.callVoidMethod(jsearcher, searcherOnConnected, @args);
  j.deleteLocalRef(args[0].l);
  j.deleteLocalRef(args[1].l);
end;

procedure TLibrarySearcherAccessWrapper.OnSearchPageCompleteImpl(sender: TObject; firstPage, nextPageAvailable: boolean);
var
  books: jobject;
  i: Integer;
  temp: jobject;
begin
  needJ;
  j.SetIntField(jsearcher, searcherFields.totalResultCountI, searcher.SearchResultCount);
  j.SetBooleanField(jsearcher, searcherFields.nextPageAvailableZ, nextPageAvailable);
  books := j.newObjectArray(searcher.SearchResult.Count, bookClass, nil);
  for i := 0 to searcher.SearchResult.Count-1 do begin
    temp := bookToJBook(searcher.SearchResult[i], false, true);
    j.SetObjectArrayElement(books, i, temp);
    j.deleteLocalRef(temp);
  end;
  if firstPage then j.callVoidMethod(jsearcher, searcherOnSearchFirstPageComplete, @books)
  else j.callVoidMethod(jsearcher, searcherOnSearchNextPageComplete, @books);
  j.deleteLocalRef(books);
end;

procedure TLibrarySearcherAccessWrapper.OnDetailsCompleteImpl(sender: TObject; book: TBook);
var
  jbook: jobject;
begin
  jbook := bookToJBook(book, false, true);
  j.callVoidMethod(jsearcher, searcherOnSearchDetailsComplete, @jbook);
  j.deleteLocalRef(jbook);
end;

procedure TLibrarySearcherAccessWrapper.OnOrderCompleteImpl(sender: TObject; book: TBook);
var
  jbook: jobject;
  acc: TCustomAccountAccess;
  temp: TBook;
begin
  if book.owner <> nil then begin
    //see androidutils/bookSearchForm
    self.beginBookReading;
    EnterCriticalSection(updateThreadConfig.libraryAccessSection);
    acc := TCustomAccountAccess(book.owner);
    temp := book.clone; temp.status:=bsOrdered;
    acc.books.current.add(temp);
    temp := book.clone; temp.status:=bsOrdered;
    acc.books.currentUpdate.add(temp); //cannot know which one is the correct one? one will be discarded?
    acc.save();
    LeaveCriticalSection(updateThreadConfig.libraryAccessSection);
    self.endBookReading;
  end;

  jbook := bookToJBook(book, false, true);
  j.callVoidMethod(jsearcher, searcherOnOrderComplete, @jbook);
  j.deleteLocalRef(jbook);
end;

procedure TLibrarySearcherAccessWrapper.OnOrderConfirmImpl(sender: TObject; book: TBook);
var
  jbook: jobject;
begin
  jbook := bookToJBook(book, false, true);
  j.callVoidMethod(jsearcher, searcherOnOrderConfirm, @jbook);
  j.deleteLocalRef(jbook);
end;

procedure TLibrarySearcherAccessWrapper.OnTakePendingMessageImpl(sender: TObject; book: TBook; apendingMessage: TPendingMessage);
var args: array[0..2] of jvalue;
begin
  if pendingMessage <> nil then FreeAndNil(pendingMessage);
  pendingMessageBook := book;
  pendingMessage := apendingMessage;

  case apendingMessage.kind of
    pmkConfirm: args[0].i := 1;
    pmkChoose: args[0].i := 2;
  end;
  args[1].l := j.stringToJString(apendingMessage.caption);
  args[2].l := j.arrayToJArray(apendingMessage.options);

  j.callVoidMethod(jsearcher, searcherOnTakePendingMessage, @args[0]);
end;

procedure TLibrarySearcherAccessWrapper.OnPendingMessageCompletedImpl(Sender: TObject);
begin
  j.callVoidMethod(jsearcher, searcherOnPendingMessageCompleted);
end;

procedure TLibrarySearcherAccessWrapper.OnExceptionImpl(Sender: TObject);
begin
  j.callVoidMethod(jsearcher, searcherOnException);
end;

constructor TLibrarySearcherAccessWrapper.create;
begin
  tempBooks:=TBookList.Create;
end;

destructor TLibrarySearcherAccessWrapper.destroy;
begin
  while operationActive do Sleep(10);
  tempBooks.free;
  j.deleteGlobalRef(jsearcher);
  inherited destroy;
end;


function unwrapSearcher(const searcher: jobject): TLibrarySearcherAccessWrapper;
begin
  result := TLibrarySearcherAccessWrapper(PtrInt(j.getLongField(searcher, searcherFields.nativePtrJ)));
end;

function wrapSearcherPtr(const searcher: TLibrarySearcherAccess): int64;
begin
  result:=PtrInt(searcher);
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchConnect(env:PJNIEnv; this:jobject; searcher: jobject; libJid: jobject); cdecl;
var
  searcherAccess: TLibrarySearcherAccessWrapper;
  lib: TLibrary;
  more: jobject;
  libId: String;
  temp: jobject;
begin
  if logging then log('Bridge_VLSearchConnect started');
  try
    libId:=j.jStringToStringAndDelete(libJid);

    lib := libraryManager.get(libId);
    if lib = nil then begin
      j.SetLongField(searcher, searcherFields.nativePtrJ, wrapSearcherPtr(nil));
      exit;
    end;

    searcherAccess := TLibrarySearcherAccessWrapper.create();

    searcherAccess.OnConnected:=@searcherAccess.OnConnectedImpl;
    searcherAccess.OnSearchPageComplete:=@searcherAccess.OnSearchPageCompleteImpl;
    searcherAccess.OnDetailsComplete:=@searcherAccess.OnDetailsCompleteImpl;
    searcherAccess.OnOrderConfirm:=@searcherAccess.OnOrderConfirmImpl;
    searcherAccess.OnOrderComplete:=@searcherAccess.OnOrderCompleteImpl;
    searcherAccess.OnTakePendingMessage:=@searcherAccess.OnTakePendingMessageImpl;
    searcherAccess.OnPendingMessageCompleted:=@searcherAccess.OnPendingMessageCompletedImpl;
    searcherAccess.OnException:=@searcherAccess.OnExceptionImpl;

    j.SetLongField(searcher, searcherFields.nativePtrJ, wrapSearcherPtr(searcherAccess));
    searcherAccess.jsearcher:=j.env^^.NewGlobalRef(j.env, searcher);

    searcherAccess.newSearch( lib.template );
  //searcherAccess.searcher.clear;
    searcherAccess.searcher.addLibrary(lib);

    searcherAccess.searcher.setLocation(lib.prettyLocation); //for digibib search (also needed for libs that only have digibib search, not just meta search)
    searcherAccess.connectAsync;


  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchConnect ended');
end;


procedure Java_de_benibela_VideLibri_Bridge_VLSearchStart(env:PJNIEnv; this:jobject; searcher: jobject; query: jobject; homeBranch, searchBranch: integer); cdecl;
var
  searcherAccess: TLibrarySearcherAccessWrapper;
  tempAccount: jobject;
  more: jobject;
  temp: jobject;
begin
  if logging then log('Bridge_VLSearchStart started');
  try
    with bookFields do begin
      searcherAccess := unwrapSearcher(searcher);

      searcherAccess.prepareNewSearchWithoutDisconnect;

      searcherAccess.searcher.SearchOptions.author:= j.getStringField(query, authorS);
      searcherAccess.searcher.SearchOptions.title:= j.getStringField(query, titleS);
      temp := j.stringToJString('year');
      searcherAccess.searcher.SearchOptions.year:= j.callStringMethod(query, bookFields.getPropertyMethod, @temp);
      temp := j.stringToJString('isbn');
      searcherAccess.searcher.SearchOptions.isbn:= j.callStringMethod(query, bookFields.getPropertyMethod, @temp);
      temp := j.stringToJString('keywords');
      searcherAccess.searcher.SearchOptions.setProperty('keywords', j.callStringMethod(query, bookFields.getPropertyMethod, @temp));

      if homeBranch >= 0 then searcherAccess.searcher.HomeBranch:=homeBranch;
      if searchBranch >= 0 then searcherAccess.searcher.searchBranch:=searchBranch;

      searcherAccess.searchAsync;

    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchStart ended');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchNextPage(env:PJNIEnv; this:jobject; searcher: jobject); cdecl;
var
  sa: TLibrarySearcherAccess;
begin
  if logging then log('Bridge_VLSearchNextPage started');
  try
    sa := unwrapSearcher(searcher);
    sa.searchNextAsync;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchNextPage started');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchDetails(env:PJNIEnv; this:jobject; searcher: jobject; jbook: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
  book: TBook;

begin
  if logging then log('Bridge_VLSearchDetails started');
  try
    sa := unwrapSearcher(searcher);
    book := jbookToBook (jbook);
    sa.tempBooks.add(book);
    sa.detailsAsyncSave(book);
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchDetails ended');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchOrder(env:PJNIEnv; this:jobject; searcher: jobject; jbooks: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
  accountId: String;
  accountIndex: Integer;
  book: TBook;
  i: Integer;

begin
  if logging then log('Bridge_VLSearchOrder started');
  try
    sa := unwrapSearcher(searcher);
    for i := 0 to j.getArrayLength(jbooks) - 1 do begin
      book :=  jbookToBookAndDelete(j.getObjectArrayElement(jbooks, i));
      if book.owner = nil then begin
        log('Owner of book '+book.title+' not found');
        continue;
      end;
      sa.orderAsync(TCustomAccountAccess(book.owner), book);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchOrder ended');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchOrderConfirmed(env:PJNIEnv; this:jobject; searcher: jobject; jbooks: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
  accountId: String;
  accountIndex: Integer;
  book: TBook;
  i: Integer;

begin
  if logging then log('Bridge_VLSearchOrderConfirmed started');
  try
    sa := unwrapSearcher(searcher);
    for i := 0 to j.getArrayLength(jbooks) - 1 do begin
      book :=  jbookToBookAndDelete(j.getObjectArrayElement(jbooks, i));
      sa.orderConfirmedAsync(book);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchOrderConfirmed ended');
end;


procedure Java_de_benibela_VideLibri_Bridge_VLSearchCompletePendingMessage(env:PJNIEnv; this:jobject; searcher: jobject; res: jint); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
  accountId: String;
  accountIndex: Integer;
  book: TBook;
  i: Integer;

begin
  if logging then log('Bridge_VLSearchOrderConfirmed started');
  try
    sa := unwrapSearcher(searcher);
    if sa.pendingMessage <> nil then
      sa.completePendingMessage(sa.pendingMessageBook, sa.pendingMessage, res);
    sa.pendingMessage := nil;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchOrderConfirmed ended');
end;


procedure Java_de_benibela_VideLibri_Bridge_VLSearchEnd(env:PJNIEnv; this:jobject; searcher: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
begin
  sa := unwrapSearcher(searcher);
  sa.free;
  j.SetLongField(searcher, searcherFields.nativePtrJ, 0);
end;

type TJOptionClass = record
  c: jclass;
  init: jmethodID;
  nearTimeI, loggingZ, refreshIntervalI, roUserLibIdsAS: jfieldID;

end;

function getOptionClass: TJOptionClass;
begin
  with result do begin
    c := j.getclass('de/benibela/videlibri/Bridge$Options');
    init := j.getmethod(c, '<init>', '()V');
    nearTimeI := j.getfield(c, 'nearTime', 'I');
    loggingZ := j.getfield(c, 'logging', 'Z');
    refreshIntervalI := j.getfield(c, 'refreshInterval','I');
    roUserLibIdsAS := j.getfield(c, 'roUserLibIds', '[Ljava/lang/String;');
  end;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetOptions(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  lib: TLibrary;
  i: Integer;
  userlibs: TList;
  temp, temp2: jobject;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptions (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    with getOptionClass do begin
      result := j.newObject(c, init);
      j.SetIntField(result, nearTimeI, userConfig.ReadInteger('base','near-time',3));
      j.SetIntField(result, refreshIntervalI, RefreshInterval);
      j.SetBooleanField(result, loggingZ, logging);

      userlibs := libraryManager.getUserLibraries();
      temp := j.newObjectArray(userlibs.Count, j.commonClasses_String, nil);
      for i := 0 to userlibs.Count-1 do begin
        temp2 := j.stringToJString(TLibrary(userlibs[i]).id);
        j.setObjectArrayElement(temp, i, temp2);
        j.deleteLocalRef(temp2);
      end;
      j.SetObjectField(result, roUserLibIdsAS, temp);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptions (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLSetOptions(env:PJNIEnv; this:jobject; options: jobject): jobject; cdecl;
var
  lib: TLibrary;
  i: Integer;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptions (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    with getOptionClass do begin
      userConfig.WriteInteger('base','near-time', j.getIntField(options, nearTimeI));
      redTime := currentDate + j.getIntField(options, nearTimeI);

      RefreshInterval:=j.getIntField(options, refreshIntervalI);
      userConfig.WriteInteger('access','refresh-interval',refreshInterval);

      logging := j.getBooleanField(options, loggingZ);
      userConfig.WriteBool('base','logging', logging);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptions (ended)');
end;








const nativeMethods: array[1..27] of JNINativeMethod=
  ((name:'VLInit';          signature:'(Lde/benibela/videlibri/Bridge$VideLibriContext;)V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLInit)
   ,(name:'VLFinalize';      signature:'()V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLFInit)

   ,(name:'VLGetLibraries'; signature:'()[Ljava/lang/String;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetLibraries)
   ,(name:'VLGetLibraryDetails'; signature:'(Ljava/lang/String;)Lde/benibela/videlibri/Bridge$LibraryDetails;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetLibraryDetails)
   ,(name:'VLSetLibraryDetails'; signature:'(Ljava/lang/String;Lde/benibela/videlibri/Bridge$LibraryDetails;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLSetLibraryDetails)
   ,(name:'VLInstallLibrary'; signature:'(Ljava/lang/String;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLInstallLibrary)
   ,(name:'VLGetTemplates'; signature:'()[Ljava/lang/String;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetTemplates)
   ,(name:'VLGetTemplateDetails'; signature:'(Ljava/lang/String;)Lde/benibela/videlibri/Bridge$TemplateDetails;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetTemplateDetails)

   ,(name:'VLAddAccount'; signature:'(Lde/benibela/videlibri/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLAddAccount)
   ,(name:'VLDeleteAccount'; signature:'(Lde/benibela/videlibri/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLDeleteAccount)
   ,(name:'VLChangeAccount'; signature:'(Lde/benibela/videlibri/Bridge$Account;Lde/benibela/videlibri/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLChangeAccount)
   ,(name:'VLGetAccounts'; signature:'()[Lde/benibela/videlibri/Bridge$Account;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetAccounts)
   ,(name:'VLGetBooks'; signature:'(Lde/benibela/videlibri/Bridge$Account;Z)[Lde/benibela/videlibri/Bridge$Book;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetBooks)

   ,(name:'VLUpdateAccount'; signature:'(Lde/benibela/videlibri/Bridge$Account;ZZ)Z'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLUpdateAccounts)
   ,(name:'VLBookOperation'; signature:'([Lde/benibela/videlibri/Bridge$Book;I)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLBookOperation)
   ,(name:'VLTakePendingExceptions'; signature: '()[Lde/benibela/videlibri/Bridge$PendingException;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetPendingExceptions)

   ,(name:'VLGetNotifications'; signature: '()[Ljava/lang/String;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetNotifications)

   ,(name:'VLSearchConnect'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;Ljava/lang/String;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchConnect)
   ,(name:'VLSearchStart'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;Lde/benibela/videlibri/Bridge$Book;II)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchStart)
   ,(name:'VLSearchNextPage'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchNextPage)
   ,(name:'VLSearchDetails'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;Lde/benibela/videlibri/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchDetails)
   ,(name:'VLSearchOrder'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;[Lde/benibela/videlibri/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchOrder)
   ,(name:'VLSearchOrderConfirmed'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;[Lde/benibela/videlibri/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchOrderConfirmed)
   ,(name:'VLSearchCompletePendingMessage'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;I)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchCompletePendingMessage)
   ,(name:'VLSearchEnd'; signature: '(Lde/benibela/videlibri/Bridge$SearcherAccess;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchEnd)

   ,(name:'VLSetOptions'; signature: '(Lde/benibela/videlibri/Bridge$Options;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSetOptions)
   ,(name:'VLGetOptions'; signature: '()Lde/benibela/videlibri/Bridge$Options;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetOptions)
  );




function loaded: integer;
var bridgeClass: jclass;
begin
  needJ;
  bridgeClass := j.env^^.FindClass(j.env, 'de/benibela/videlibri/Bridge');
  if (not assigned(bridgeClass)) or (j.env^^.ExceptionCheck(j.env)<>0) then begin
    bbdebugtools.log('failed to find VideLibri Bridge');
    exit(JNI_ERR);
  end;
  if j.env^^.RegisterNatives(j.env, bridgeClass, @NativeMethods[low(nativeMethods)],length(NativeMethods)) < 0 then begin
    bbdebugtools.log('failed to register methods');
    exit(JNI_ERR);
  end;
  result := JNI_VERSION_1_4;
end;

{$endif}


function iniFileFromString(data: string): TIniFile;
var
  stream: TStringStream;
begin
  stream := TStringStream.Create(data);
  result := TIniFile.Create(stream);
  stream.free;
end;




end.

