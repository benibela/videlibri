unit androidutils;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, IniFiles, applicationconfig, jni, bbjniutils, libraryParser, booklistreader, librarySearcherAccess,
{$ifdef android}okhttpinternetaccess, {$ENDIF} multipagetemplate, xquery, xquery.internals.common;

//procedure deleteLocalRef(jobj: pointer);

var assetPath: string;

procedure beginAssetRead;
function assetFileAsString(name: rawbytestring): string;
procedure endAssetRead;

function iniFileFromString(data: string): TIniFile;

function getUserConfigPath: string;

function loaded: integer;
procedure uninit;

{$ifdef android}
type TCallbackHolderAndroid = class (TCallbackHolder)
  class procedure allThreadsDone(); override;
end;
{$endif}





implementation
uses bbutils, accountlist, bbdebugtools, libraryAccess, simplehtmltreeparser, math, bbutilsbeta, commoninterface;

threadvar skippedpaths: TStringArray;

resourcestring
  rsAlertTitleOverdueOne = '1 überfälliges Buch!';
  rsAlertTitleOverdueMany = '%d überfällige Bücher!';
  rsAlertTextOverdueOne = '1 Buch seit %1:s';
  rsAlertTextOverdueMany = '%d Bücher seit %1:s';
  rsAlertTitleSoonOne = '1 fälliges Buch';
  rsAlertTitleSoonMany = '%d fällige Bücher';
  rsAlertTextDueOne = '1 Buch fällig bis %1:s';
  rsAlertTextDueMany = '%d Bücher fällig bis %1:s';
  rsAlertTextRenewableDueOne = '1 verlängerbares Buch fällig bis %1:s';
  rsAlertTextRenewableDueMany = '%d verlängerbare Bücher fällig bis %1:s';

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

function userAssetFileAsString(name: rawbytestring; var res: string): boolean;
begin
  if length(skippedpaths) > 0 then begin
    if arrayContains(skippedpaths, copy(name, 1, max(strRpos('/', name), strRpos(DirectorySeparator, name)))) then
      exit(false);
  end;
  if not FileExists(userPath + name) then exit(false);
  res := strLoadFromFileUTF8(userPath + name);
  if not strEndsWith(name, 'template') then result := true
  else begin
    result := testAssetVersion(res);
    if not result then arrayAdd(skippedpaths, copy(name, 1, max(strRpos('/', name), strRpos(DirectorySeparator, name))));
  end;
end;

{$ifndef android}
function assetFileAsString(name: rawbytestring): string;
begin
  result := '';
  if userAssetFileAsString(name, result) then exit;
  result := strLoadFromFileUTF8(assetPath + name);
end;

function getUserConfigPath: string;
begin
  result := GetAppConfigDir(false);
end;

procedure beginAssetRead;begin end;
procedure endAssetRead;begin end;
procedure uninit;begin end;
function loaded: LongInt; begin result := 0; end;

{$else}

threadvar assets: jobject;
    assetCount: integer;
var
    bridgeClass: jclass;
    bridgeFields: record userPath: jfieldID; end;
    bridgeCallbackMethods: record VLAllThreadsDone, VLInstallationDone: jmethodID; end;
    accountClass, bookClass: jobject;
    accountClassInitWithData, bookClassInit, bookClassInitWithTitle, bookClassInitWithData: jmethodID;
    accountFields: record
      LibIdS, NameS, PassS, TypeI, PrettyNameS, ExtendDaysI, ExtendZ, HistoryZ: jfieldID;
//      internalIdMethod: jmethodID;
    end;
    bookFields: record
      authorS, titleS, idS, yearS, issueDateI, dueDateI, firstExistsDateI, accountL, historyZ, statusI, holdingsL, additionalPropertiesL: jfieldID;
      setPropertyMethod, getPropertyMethod: jmethodID;
    end;
    searcherClass: jclass;
    searcherFields: record
      searchParamsL, nativePtrJ, totalResultCountI, nextPageAvailableZ: jfieldID;
    end;
    //searcherResultInterface: jclass;
    searcherOnConnected, searcherOnSearchFirstPageComplete, searcherOnSearchNextPageComplete, searcherOnSearchDetailsComplete, searcherOnOrderComplete,  searcherOnTakePendingMessage, searcherOnPendingMessageCompleted, searcherOnException: jmethodID;
    globalStrings: record
      libraryBranch, libraryLocation, isbn, category, status, renewCount, cancelable: jobject;
    end;

function assetFileAsString(name: rawbytestring): string;
begin
  if userAssetFileAsString(name, result) then exit;
  beginAssetRead;
  try
    result := j.getAssetAsString(assets, name);
  finally
    endAssetRead;
  end;
end;

procedure beginAssetRead;
begin
  if assetCount = 0 then
    assets := needJ.getAssets;
  assetCount += 1;
end;

procedure endAssetRead;
begin
  assetCount -= 1;
  if assetCount = 0 then assets.deleteLocalRef();
end;


function getUserConfigPath: string;
begin
  with j do
    result := jStringToString(env^^.GetStaticObjectField(env, bridgeClass, bridgeFields.userPath));
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


class procedure TCallbackHolderAndroid.allThreadsDone;
begin
  if logging then bbdebugtools.log('androidAllThreadsDone started');
  needJ.callStaticVoidMethod(bridgeClass, bridgeCallbackMethods.VLAllThreadsDone);
  if logging then bbdebugtools.log('androidAllThreadsDone ended');
end;

type TOkHttpBuildCallbackObject = object
  procedure onBuild(sender: TOKHTTPInternetAccess; builder: jobject);
end;

procedure TOkHttpBuildCallbackObject.onBuild(sender: TOKHTTPInternetAccess; builder: jobject);
var
  customizer: jclass;
begin
  customizer := j.getclass('de/benibela/internettools/okhttp/ClientBuilderCustomizer');
  customizer.callStaticVoidMethodChecked(customizer.getstaticmethod('customize', '(Lokhttp3/OkHttpClient$Builder;)V'), @builder);
  customizer.deleteLocalRef();
end;

procedure throwExceptionToJava(e: Exception);
begin
  with j do begin
    if e is EJNIException then
      Throw('de/benibela/videlibri/jni/InternalError', e as EJNIException)
    else if (e is EFileNotFoundException)
         or (e is EDirectoryNotFoundException)
         or (e is EPathNotFoundException)
         or (e is EInOutError)
         or (e is EStreamError)
    then
      ThrowNew('de/benibela/videlibri/jni/InternalErrorFile', e.Message)
    else if e is EExternal then
      ThrowNew('de/benibela/videlibri/jni/InternalErrorExternal', e.Message)
    else
      ThrowNew('de/benibela/videlibri/jni/InternalError', e.Message)
  end
end;

procedure Java_de_benibela_VideLibri_Bridge_VLInit(env:PJNIEnv; this:jobject; context: jobject); cdecl;

  procedure initLocale;
  var LocaleClass: jclass;
  Locale_getDefault, Locale_getCountry, Locale_getLanguage: jmethodID;
  locale: jobject;
  begin
    with j do begin
      LocaleClass := getclass('java/util/Locale');
      Locale_getDefault := getstaticmethod(LocaleClass, 'getDefault', '()Ljava/util/Locale;');
      Locale_getLanguage := getmethod(LocaleClass, 'getLanguage', '()Ljava/lang/String;');
      Locale_getCountry := getmethod(LocaleClass, 'getCountry', '()Ljava/lang/String;');
      //Locale_getLanguage := getmethod(LocaleClass, 'getLanguage', '()Ljava/lang/String;');
      locale := callStaticObjectMethodChecked(LocaleClass, Locale_getDefault);
      if locale <> nil then begin;
        localeLanguage := jStringToStringAndDelete(callObjectMethodChecked(locale, Locale_getLanguage));
        localeCountry := jStringToStringAndDelete(callObjectMethodChecked(locale, Locale_getCountry));
        if localeLanguage = '' then localeLanguage := 'de';
        if localeCountry = '' then localeCountry := 'DE';
        deleteLocalRef(locale);
      end;
      deleteLocalRef(LocaleClass);
    end;
  end;

var tempOkHttpBuild: TOkHttpBuildCallbackObject;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInit (started)');
  needJ;
  try
    okhttpinternetaccess.onBuildCallback := @tempOkHttpBuild.onBuild;

    with needJ do begin
      jContextObject := env^^.NewGlobalRef(env, context);

      bridgeClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge'));
      bridgeCallbackMethods.VLAllThreadsDone := getstaticmethod(bridgeClass, 'allThreadsDone', '()V');
      bridgeCallbackMethods.VLInstallationDone := getstaticmethod(bridgeClass, 'installationDone', '(I)V');
      bridgeFields.userPath:= getstaticfield(bridgeClass, 'userPath', 'Ljava/lang/String;');

      accountClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge$Account'));
      accountClassInitWithData := getmethod(accountClass, '<init>', '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;IZIZILjava/lang/String;)V');
      with accountFields  do begin
        LibIdS := getfield(accountClass, 'libId', 'Ljava/lang/String;');
        NameS := getfield(accountClass, 'name', 'Ljava/lang/String;');
        PassS := getfield(accountClass, 'pass', 'Ljava/lang/String;');
        TypeI := getfield(accountClass, 'type', 'I');
        PrettyNameS := getfield(accountClass, 'prettyName', 'Ljava/lang/String;');
        ExtendDaysI := getfield(accountClass, 'extendDays', 'I');
        ExtendZ := getfield(accountClass, 'extend', 'Z');
        HistoryZ := getfield(accountClass, 'history', 'Z');
    //    internalIdMethod := getmethod(accountClass, 'internalId', '()Ljava/lang/String;');
      end;

      bookClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge$Book'));
      bookClassInit := getmethod(bookClass, '<init>', '()V');
      bookClassInitWithTitle := getmethod(bookClass, '<init>', '(Ljava/lang/String;)V');
      bookClassInitWithData := getmethod(bookClass, '<init>', '(ILde/benibela/videlibri/jni/Bridge$Account;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V');
      with bookFields do begin
        idS := getfield(bookClass, 'id', 'Ljava/lang/String;');
        authorS := getfield(bookClass, 'author', 'Ljava/lang/String;');
        titleS := getfield(bookClass, 'title', 'Ljava/lang/String;');
        yearS := getfield(bookClass, 'year', 'Ljava/lang/String;');
        issueDateI := getfield(bookClass, 'issueDate', 'I');
        dueDateI := getfield(bookClass, 'dueDate', 'I');
        firstExistsDateI := getfield(bookClass, 'firstExistsDate', 'I');
        accountL := getfield(bookClass, 'account', 'Lde/benibela/videlibri/jni/Bridge$Account;');
        historyZ := getfield(bookClass, 'history', 'Z');
        holdingsL := getfield(bookClass, 'holdings', '[Lde/benibela/videlibri/jni/Bridge$Book;');
        additionalPropertiesL := getfield(bookClass, 'additionalProperties', 'Ljava/util/ArrayList;');
        setPropertyMethod := getmethod(bookClass, 'setProperty', '(Ljava/lang/String;Ljava/lang/String;)V');
        getPropertyMethod := getmethod(bookClass, 'getProperty', '(Ljava/lang/String;)Ljava/lang/String;');
        statusI := getfield(bookClass, 'status', 'I');
      end;



      searcherClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/SearcherAccessPascal'));
      with searcherFields do begin
        nativePtrJ := getfield(searcherClass, 'nativePtr', 'J');
        totalResultCountI := getfield(searcherClass, 'totalResultCount', 'I');
        nextPageAvailableZ := getfield(searcherClass, 'nextPageAvailable', 'Z');
        searchParamsL := getfield(searcherClass, 'searchParams', 'Lde/benibela/videlibri/jni/FormParams;');
      end;
      searcherOnConnected := getmethod(searcherClass, 'onConnected', '(Lde/benibela/videlibri/jni/FormParams;)V');
      searcherOnSearchFirstPageComplete := getmethod(searcherClass, 'onSearchFirstPageComplete', '([Lde/benibela/videlibri/jni/Bridge$Book;)V');
      searcherOnSearchNextPageComplete := getmethod(searcherClass, 'onSearchNextPageComplete', '([Lde/benibela/videlibri/jni/Bridge$Book;)V');
      searcherOnSearchDetailsComplete := getmethod(searcherClass, 'onSearchDetailsComplete', '(Lde/benibela/videlibri/jni/Bridge$Book;)V');
      searcherOnOrderComplete := getmethod(searcherClass, 'onOrderComplete', '(Lde/benibela/videlibri/jni/Bridge$Book;)V');
      searcherOnTakePendingMessage := getmethod(searcherClass, 'onTakePendingMessage', '(ILjava/lang/String;[Ljava/lang/String;)V');
      searcherOnPendingMessageCompleted := getmethod(searcherClass, 'onPendingMessageCompleted', '()V');
      searcherOnException := getmethod(searcherClass, 'onException', '()V');


      callbacks := TCallbackHolderAndroid;

      with globalStrings do begin
        libraryBranch := newGlobalRefAndDelete(NewStringUTF('libraryBranch'));
        libraryLocation := newGlobalRefAndDelete(NewStringUTF('libraryLocation'));
        isbn := newGlobalRefAndDelete(NewStringUTF('isbn'));
        category := newGlobalRefAndDelete(NewStringUTF('category'));
        status := newGlobalRefAndDelete(NewStringUTF('status'));
        renewCount := newGlobalRefAndDelete(NewStringUTF('renewCount'));
        cancelable := newGlobalRefAndDelete(NewStringUTF('cancelable'));
      end;
    end;

    commoninterface.initBridge;
    initLocale;

    beginAssetRead;
    initApplicationConfig;
    endAssetRead;

 // bbdebugtools.OnLog := TStringNotifyEvent(procedureToMethod(TProcedure(@onLog)));

  except
    on e: Exception do throwExceptionToJava(e);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/InternalError', 'Interner Fehler: '+e.Message);
  end;            }
end;

function Java_de_benibela_VideLibri_Bridge_VLGetLibraryIds(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  i: Integer;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraryIds (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  with j do
  try
    result := newStringArray(libraryManager.count);
    for i := 0 to libraryManager.count - 1 do
      setStringArrayElement(result, i, libraryManager.libraryIds[i]);
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraryIds (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLGetTemplateDetails(env:PJNIEnv; this:jobject; id: jstring): jobject; cdecl;
var
  i: Integer;
  template: TMultiPageTemplate;
  meta: TTemplateActionMeta;
  details: TTemplateDetails;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetTemplateDetails (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  result := nil;
  try
    template := libraryManager.getTemplate(j.jStringToString(id));
    if template = nil then exit(nil);
    meta := nil;
    for i := 0 to high(template.baseActions.children) do
      if assigned(template.baseActions.children[i]) and (template.baseActions.children[i].InheritsFrom(TTemplateActionMeta)) then
        meta := template.baseActions.children[i] as TTemplateActionMeta ;

    if meta = nil then exit(nil);

    details := default(TTemplateDetails);
    details.description := meta.description;
    SetLength(details.variablesDefault, length(meta.variables));
    SetLength(details.variablesDescription, length(meta.variables));
    SetLength(details.variablesNames, length(meta.variables));
    for i:=0 to high(meta.variables) do begin
      details.variablesNames[i] := meta.variables[i].name;
      details.variablesDescription[i] :=  meta.variables[i].description;
      if meta.variables[i].hasDef then details.variablesDefault[i] := meta.variables[i].def
      //else details.variablesDefault[i] := ''
    end;
    result := details.toJava;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetTemplateDetails (ended)');
end;



function Java_de_benibela_VideLibri_Bridge_VLGetLibraryDetails(env:PJNIEnv; this:jobject; id: jobject ; needCatalogUrl: jboolean): jobject; cdecl;

var
  lib: TLibrary;
  libId: String;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraryDetails (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  result := nil;
  try
    libId := j.jStringToString(id);
    try
      lib := libraryManager.get(libId);
      if lib = nil then exit(nil);
      if needCatalogUrl <> JNI_FALSE then lib.catalogUrl;
      result := lib.toJava;
    except
      on e: Exception do begin
        storeException(e, nil, libid, '');
        result := nil;
      end;
    end;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraryDetails (ended)');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSetLibraryDetails(env:PJNIEnv; this:jobject; id: jstring; jdetails: jobject); cdecl;
var
  libid: String;
  libXml: String;
  libDetails: TLibraryDetails;
  vpair: TLibraryVariable;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetLibraryDetails (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    libid := j.jStringToString(id);
    if jdetails = nil then begin
      //null means delete
      libraryManager.deleteUserLibrary(libid);
    end else begin
      libDetails := TLibraryDetails.fromJava(jdetails);
      libXml := '<?xml version="1.0" encoding="UTF-8"?>'+LineEnding;
      libXml += '<library>'+LineEnding;
      libXml += '   <longName value="'+xmlStrEscape(libDetails.prettyName)+'"/>'+LineEnding;
      if libDetails.fhomepageUrl <> '' then
        libXml += '   <homepage value="'+xmlStrEscape(libDetails.fhomepageUrl)+'"/>'+LineEnding;
      if libDetails.fcatalogueUrl <> '' then
        libXml += '   <catalogue value="'+xmlStrEscape(libDetails.fcatalogueUrl)+'"/>'+LineEnding;
      libXml += '   <template value="'+xmlStrEscape(libDetails.templateId)+'"/>'+LineEnding;
      if libDetails.tableComment <> '' then libXml += '   <table-comment value="'+xmlStrEscape(libDetails.tableComment)+'"/>'+LineEnding;
      for vpair in libDetails.variables do
        libXml += '   <variable name="'+xmlStrEscape(vpair.name)+'" '+ 'value="'+xmlStrEscape(vpair.value)+'"/>'+LineEnding;
      if libDetails.segregatedAccounts then
        libXml += '  <segregated-accounts value="true"/>';

      libXml += '  <testing-search value="yes"/><testing-account value="yes"/>';
      libXml += '</library>';
      libdetails.free;
      libraryManager.setUserLibrary(libid, libXml);
    end;
  except
    on e: Exception do throwExceptionToJava(e);
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
    surl := j.jStringToString(url);
    TInstallLibraryThread.Create(surl).Start;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInstallLibrary (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLReloadLibrary(env:PJNIEnv; this:jobject; id: jobject): jobject; cdecl;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLReloadLibrary (started)');
  try
    libraryManager.reloadLibrary(j.jStringToString(id));
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLReloadLibrary (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLReloadTemplate(env:PJNIEnv; this:jobject; id: jobject): jobject; cdecl;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLReloadTemplate (started)');
  try
    libraryManager.reloadTemplate(j.jStringToString(id));
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLReloadTemplate (ended)');
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
  result := nil;
  try
    beginAssetRead;

    path := j.stringToJString('libraries/templates');
    list:= j.callObjectMethodChecked(assets, j.getmethod(jCommonClasses.android.AssetManager.classRef, 'list', '(Ljava/lang/String;)[Ljava/lang/String;'), @path);
    j.deleteLocalRef(path);

    endAssetRead;


    templates := TStringList.Create;

    for i := 0 to j.getArrayLength(list) - 1 do
      templates.Add(j.getStringArrayElement(list, i));
    templates.Sorted:=true;

    libraryManager.enumerateBuiltInTemplates(templates);
    libraryManager.enumerateUserTemplates(templates);

    result := j.newObjectArray(templates.Count, jCommonClasses.&String.classRef, nil);
    for i := 0 to templates.count - 1 do
      j.setObjectArrayElement(result, i, j.stringToJString(templates[i]));

    templates.free;
  except
    on e: Exception do throwExceptionToJava(e);
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
                   temp, j.getIntField(acc, ExtendDaysI), j.getBooleanField(acc, HistoryZ),
                   j.getIntField(acc, TypeI) );
    end;
    accounts.save;
  except
    on e: Exception do throwExceptionToJava(e);
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
    on e: Exception do throwExceptionToJava(e);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLDeleteAccount (ended)');
end;


procedure changeAccount(
      i: integer;
      enabled: boolean;
      prettyName, username, password: string;
      keepHistory: boolean;
      extendType: TExtendType; extendDays: integer; accountType: integer);
var oldacc: TCustomAccountAccess;
begin
  oldacc := accounts[i];
  oldacc.prettyName:=prettyName;
  oldacc.passWord:=password;
  oldacc.accountType:=accountType;
  oldacc.keepHistory:=keepHistory;

  oldacc.extendType:=extendType;
  oldacc.extendDays:=extendDays;

  oldacc.enabled:=enabled;

  oldacc.saveConfig();
  if oldacc.getUser() <> username then begin
    EnterCriticalSection(updateThreadConfig.libraryAccessSection);
    try
      oldacc.changeUser(username);
      accounts.Strings[i] := oldacc.getPlusEncodedID();
      accounts.save;
    finally
      LeaveCriticalSection(updateThreadConfig.libraryAccessSection);
    end;
  end;
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
                      temp, j.getIntField(jnewacc, ExtendDaysI),
                      j.getIntField(jnewacc, TypeI)
                      );
      end;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLChangeAccount (ended)');
end;

function accountToJAccount(account: TCustomAccountAccess): jobject;
var args: array[0..9] of jvalue;
  i: Integer;
begin
  with j  do begin
    args[0].l := stringToJString(account.getLibrary().id);
    args[1].l := stringToJString(account.getUser());
    args[2].l := stringToJString(account.passWord);
    args[3].l := stringToJString(account.prettyName);
    args[4].i := account.accountType;
    args[5].z := booleanToJboolean(account.extendType <> etNever);
    args[6].i := account.extendDays;
    args[7].z := booleanToJboolean(account.keepHistory);
    try
      account.lock.enter;
      args[8].i := account.lastCheckDate;
      args[9].l := stringToJString(account.expiration);
    finally
      account.lock.leave;
    end;
    result := newObject(accountClass, accountClassInitWithData, @args[0]);
    for i := 0 to 3 do deleteLocalRef(args[i].l);
    deleteLocalRef(args[9].l);
  end;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetAccounts(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  i: Integer;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetAccounts (started)');
  try
    result := j.newObjectArray(accounts.Count, accountClass, nil);
    with j do
      for i := 0 to accounts.Count - 1 do
        setObjectArrayElementAndDelete(result, i,  accountToJAccount(accounts[i]));
  except
    on e: Exception do throwExceptionToJava(e);
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
  //log('Account: '+libId+' '+user);
  for i := 0 to accounts.Count-1 do
    if (accounts[i].getUser() = user) and (accounts[i].getLibrary().id = libId) then
      exit(accounts[i]);
  result := nil;
end;

function getRealAccountChecked(acc: jobject): TCustomAccountAccess;
begin
  result := getRealAccount(acc);
  if result = nil then
    needJ.ThrowNew('de/benibela/videlibri/jni/InternalError', 'Konto nicht gefunden: '+needj.getStringField(acc, accountFields.LibIdS)+':'+ j.getStringField(acc, accountFields.NameS));
end;

function bookToJBook(book: TBook; jaccount: jobject; includeDates: boolean = true; includeAllStrings: boolean = false): jobject;
var myj: ^TJavaEnv;
    jbook: jclass;

  procedure putProperty(name: jobject; const value: string);
  var args: array[0..1] of jvalue;
  begin
    with myj^ do begin
      args[0].l := name;
      args[1].l := stringToJString(value);
      callVoidMethod(jbook, bookFields.setPropertyMethod, @args);
      deleteLocalRef(args[1].l);
    end;
  end;
  procedure putProperty(const name, value: string);
  var jname: jobject;
  begin
    jname := myj^.stringToJString(name);
    putProperty(jname, value);
    myj^.deleteLocalRef(jname);
  end;

const INIT_STR = 2;


var
  i, tempi: Integer;
  jholdings, tempbook: jobject;
  args: array[0..INIT_STR+3] of jvalue;

begin
  myj := @j;
  with j do with book do begin
    tempi := 5;
    if renewCount > 0 then inc(tempi);
    if includeAllStrings then inc(tempi, length(book.additional));


    args[0].i := tempi;
    args[1].l := jaccount;
    args[INIT_STR].l := stringToJString(id);
    args[INIT_STR+1].l := stringToJString(author);
    args[INIT_STR+2].l := stringToJString(title);
    args[INIT_STR+3].l := stringToJString(year);

    jbook := newObject(bookClass, bookClassInitWithData, @args[0]);
    deleteLocalRef(args[INIT_STR].l);
    deleteLocalRef(args[INIT_STR+1].l);
    deleteLocalRef(args[INIT_STR+2].l);
    deleteLocalRef(args[INIT_STR+3].l);

    putProperty(globalStrings.libraryBranch, libraryBranch);
    putProperty(globalStrings.libraryLocation, libraryLocation);
    putProperty(globalStrings.isbn, isbn);
    putProperty(globalStrings.category, category);
    putProperty(globalStrings.status, statusStr);
    if renewCount > 0 then putProperty(globalStrings.renewCount, inttostr(renewCount));
    case cancelable of
      tUnknown: putProperty(globalStrings.cancelable, '?');
      tTrue: putProperty(globalStrings.cancelable, 'true');
      tFalse: putProperty(globalStrings.cancelable, 'false');
    end;
    if includeAllStrings then
      for i := 0 to high(book.additional) do
        putProperty(book.additional[i].name, book.additional[i].value);

    if includeDates then begin
      SetIntField(jbook, bookFields.issueDateI, issueDate);
      SetIntField(jbook, bookFields.dueDateI, dueDate);
      SetIntField(jbook, bookFields.firstExistsDateI, firstExistsDate);
    end;

    SetIntField(jbook, bookFields.statusI, ord(book.status));

  end;
  with j do
  if (book.holdings<> nil) and (book.holdings.Count> 0) then begin
    jholdings := newObjectArray(book.holdings.Count, bookClass, nil);
    for i := 0 to book.holdings.Count-1 do begin
      tempbook := bookToJBook(book.holdings[i], jaccount, true, includeAllStrings);
      setObjectArrayElement(jholdings,i,tempbook);
      deleteLocalRef(tempbook);
    end;
    SetObjectField(jbook, bookFields.holdingsL, jholdings);
    deleteLocalRef(jholdings);
  end;
  result := jbook;
end;

function jbookToBookAndDelete(jbook: jobject): TBook; forward;

function jbookToBook(jbook: jobject): TBook;
var
  more: jobject;
  iv: jvalue;
  size: jint;
  i: Integer;
  jaccount, jholdings, key, value: jobject;
begin
  result := TBook.create;
  with bookFields do with j do begin
    result.id := getStringField(jbook, idS);
    result.author := getStringField(jbook, authorS);
    result.title := getStringField(jbook, titleS);
    result.year := getStringField(jbook, yearS);
    result.dueDate := getIntField(jbook, dueDateI);
    result.issueDate := getIntField(jbook, issueDateI);
    result.firstExistsDate := getIntField(jbook, firstExistsDateI);
    more := getObjectField(jbook, additionalPropertiesL);
    if more <> nil then begin
      size := callIntMethodChecked(more, jCommonClasses.ArrayList.size);
      iv.i := 0;
      while iv.i < size do begin
        key := callObjectMethod(more, jCommonClasses.ArrayList.get_I, @iv);
        inc(iv.i);
        if iv.i >= size then break;
        value := callObjectMethod(more, jCommonClasses.ArrayList.get_I, @iv);
        inc(iv.i);
        result.setProperty(jStringToStringAndDelete(key), jStringToStringAndDelete(value));
      end;
      deleteLocalRef(more);
    end;

    jaccount := getObjectField(jbook, accountL);
    if jaccount <> nil then begin
      result.owningAccount := getRealAccount(jaccount);
      deleteLocalRef(jaccount);
    end;

    jholdings := getObjectField(jbook, holdingsL);
    if jholdings <> nil then begin
      result.holdings := TBookList.create(result);
      for i:=0 to getArrayLength(jholdings) - 1 do
        result.holdings.add(jbookToBookAndDelete(getObjectArrayElement(jholdings, i)));
      deleteLocalRef(jholdings);
    end;

    {entrySet := callObjectMethodChecked(more, getmethod('java/util/Map', 'entrySet', '()Ljava/util/Set;'));
    iterator := callObjectMethodChecked(entrySet, getmethod('java/util/Set', 'iterator', '()Ljava/util/Iterator;'));

    hasNext := getmethod('java/util/Iterator', 'hasNext', '()Z');
    next := getmethod('java/util/Iterator', 'next', '()Ljava/lang/Object;');
    getKey := getmethod('java/util/Map$Entry', 'getKey', '()Ljava/lang/Object;');
    getValue := getmethod('java/util/Map$Entry', 'getValue', '()Ljava/lang/Object;');

    while callBooleanMethodChecked(iterator, hasNext) do begin
      entry := callObjectMethodChecked(iterator, next);
      result.setProperty(jStringToStringAndDelete(callObjectMethodChecked(entry, getKey)),
                       jStringToStringAndDelete(callObjectMethodChecked(entry, getValue)));
      deleteLocalRef(entry);
    end;}
  end;
end;

function jbookToBookAndDelete(jbook: jobject): TBook;
begin
  result := jbookToBook(jbook);
  j.deleteLocalRef(jbook);
end;

procedure updateJavaPascalDate();
var timeField: jfieldID;
begin
  timeField := j.getstaticfield(bridgeClass, 'currentPascalDate', 'I');
  j.env^^.SetStaticIntField(j.env, bridgeClass, timeField, trunc(now));
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
  result := nil;
  try
    acc := getRealAccountChecked(jacc);
    if acc = nil then exit;
  except
    on e: Exception do begin
      throwExceptionToJava(e);
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
      with j do
      for i := 0 to books.Count - 1 do begin
        book := bookToJBook(books[i], jacc);
        SetBooleanField(book, bookFields.historyZ, history);
        SetObjectArrayElement(result, i, book);
        deleteLocalRef(book);
      end;
    finally
      system.LeaveCriticalsection(updateThreadConfig.libraryAccessSection)
    end;

    updateJavaPascalDate();

  except
    on e: Exception do throwExceptionToJava(e);
  end;

  if logging then bbdebugtools.log('Java_de_benibela_VideLibri_Bridge_VLGetBooks ended');

end;

procedure Java_de_benibela_VideLibri_Bridge_VLChangeBook(env:PJNIEnv; this:jobject; joldBook, jnewBook: jobject); cdecl;
var
  oldBook, newBook, tempBook: TBook;
  account: TCustomAccountAccess;
  bookList: TBookList;
  oldBookIndex: LongInt;
  function getBookListFromBook(book: tbook): boolean;
  begin
    result := book.owningAccount is TCustomAccountAccess;
    if result then begin
      account := book.owningAccount as TCustomAccountAccess;
      bookList := account.books.old;
    end else begin
      account := nil;
      booklist := nil;
    end;
  end;

begin
  tempBook := nil;
  oldBook := nil;
  newBook := nil;
  system.EnterCriticalSection(updateThreadConfig.libraryAccessSection);
  try
    try
      if joldBook <> nil then begin
        tempBook := jbookToBook(joldBook);
        if not getBookListFromBook(tempBook) then exit;
        oldBookIndex := bookList.findBookIndex(tempBook);
        if oldBookIndex < 0 then exit;
        oldBook := bookList[oldBookIndex];
      end;
      if jnewBook = nil then begin
        if oldBook = nil then exit
        else bookList.delete(oldBookIndex);
      end else begin
        newBook := jbookToBook(jnewBook);
        if oldBook = nil then begin
          if not getBookListFromBook(newBook) then exit;
          bookList.add(newBook);
        end else
          oldBook.assignAll(newBook);
      end;
      account.saveBooks();


    finally
      system.LeaveCriticalsection(updateThreadConfig.libraryAccessSection);
      if tempBook <> nil then tempBook.decReference;
      if newBook <> nil then newBook.decReference;
    end;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetCriticalBook(env:PJNIEnv; this:jobject): jobject; cdecl;
var redBook, yellowBook, book: TBook;
  i, j: Integer;
  account: TCustomBookOwner;
  jaccount: jobject;
begin
  if logging then bbdebugtools.log('Java_de_benibela_VideLibri_Bridge_VLGetCriticalBook started');
  result := nil;
  try
    updateGlobalTimeCache;
    try
      system.EnterCriticalsection(updateThreadConfig.libraryAccessSection);
      redBook := nil;
      yellowBook := nil;
      for i:=0 to accounts.count-1 do
        with (accounts[i]) do
          for j:=0  to books.current.count-1 do begin
            book := books.current[j];
            if book.status in BOOK_NOT_LEND then continue;
            if (book.dueDate<=redTime) then redBook := book;
            if (book.status in BOOK_NOT_EXTENDABLE) then yellowBook := book;
          end;
      if redBook <> nil then book := redBook
      else book := yellowBook;
      if book = nil then result := nil
      else begin
        account := book.owningAccount;
        jaccount:=nil;
        if (account <> nil) and (account.InheritsFrom(TCustomAccountAccess)) then
          jaccount := accountToJAccount(TCustomAccountAccess(account));
        result := bookToJBook(book, jaccount, true);
        jaccount.deleteLocalRef();
      end;
    finally
      system.LeaveCriticalsection(updateThreadConfig.libraryAccessSection)
    end;

    updateJavaPascalDate();

  except
    on e: Exception do throwExceptionToJava(e);
  end;

  if logging then bbdebugtools.log('Java_de_benibela_VideLibri_Bridge_VLGetCriticalBook ended');
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
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('VLUpdateAccounts ended');

end;

function Java_de_benibela_VideLibri_Bridge_VLBookOperation(env:PJNIEnv; this:jobject; jbooks: jobject; operation: jint): jobject; cdecl;
var
  books: TBookList;
  book: TBook;
  i: Integer;
begin
  if logging then log('VLBookOperation');

  try
    books := TBookList.create();
    books.Capacity:=j.getArrayLength(jbooks);
    with j do
    for i := 0 to getArrayLength(jbooks) - 1 do begin
      book := jbookToBookAndDelete(getObjectArrayElement(jbooks, i));
      if book.owningAccount <> nil then books.add(book);
    end;
    case operation of
      1: extendBooks(books);
      2: cancelBooks(books);
    end;
    books.free;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('VLBookOperation ended');

  result := nil;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetPendingExceptions(env:PJNIEnv; this:jobject): jobject;
var
  pendingExceptions: TPendingExceptions;
  i, k: Integer;
begin
  if logging then bbdebugtools.log('Bridge_VLGetPendingExceptions started');
  result := nil;

  try
    system.EnterCriticalSection(exceptionStoring);
    try
      pendingExceptions := Default(TPendingExceptions);
      Setlength(pendingExceptions.exceptions, length(errorMessageList));
      for i :=  0 to high(errorMessageList) do begin
        with pendingExceptions.exceptions[i] do begin
          details := '';
          anonymousDetails := '';
          accountPrettyNames := '';
          libraryIds := '';
          searchQuery := '';
          firstAccountUser := '';
          for k := 0 to high(errorMessageList[i].details) do begin
            details += errorMessageList[i].details[k].details+LineEnding+LineEnding;
            anonymousDetails += errorMessageList[i].details[k].anonymouseDetails+LineEnding+LineEnding;
            if not strContains(libraryIds, errorMessageList[i].details[k].libraryId) then
              libraryIds += errorMessageList[i].details[k].libraryId + ' ';
            if errorMessageList[i].details[k].searchQuery <> '' then
              searchQuery += errorMessageList[i].details[k].searchQuery + ' ';
            if accountPrettyNames <> '' then accountPrettyNames += ', ';
            with errorMessageList[i].details[k] do
              if account <> nil then begin
                accountPrettyNames += account.prettyName;
                if firstAccountUser = '' then begin
                  firstAccountUser := account.getUser();
                  firstAccountLib := account.getLibrary().id;
                end;
              end;
          end;
          kind := errorMessageList[i].kind;
          error := errorMessageList[i].error;
        end;
      end;
      SetLength(errorMessageList, 0);
    finally
      system.LeaveCriticalSection(exceptionStoring);
    end;
    result := pendingExceptions.toJava;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('Bridge_VLGetPendingExceptions ended');
end;

function Java_de_benibela_VideLibri_Bridge_VLSendFeedback(env:PJNIEnv; this:jobject; jdata: jobject): jboolean; cdecl;
var
  i: Integer;
  data: string = '';
begin
  if logging then log('VLSendFeedback');

  try
    with needj do
      for i := 0 to getArrayLength(jdata) - 1 do
        data += getStringArrayElement(jdata, i);
    result := j.booleanToJboolean(applicationconfig.sendFeedback(data));
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('VLSendFeedback ended');
end;


function Java_de_benibela_VideLibri_Bridge_VLGetNotifications(env:PJNIEnv; this:jobject): jobject;
  function plural(count: integer; sing, plur: string): string;
  begin
    if count = 1 then result := sing
    else result := plur;
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
  result := nil;
  try
    title := '';
    text := '';


    findBooksThatMustBeReturned(booksOverdue, booksSoonNotExtendable, booksSoon,
                                minDateOverdue, minDateSoonNotExtendable, minDateSoon);

    if ((lastWarnDate + WarnInterval <= currentDate) and (booksOverdue.Count + booksSoonNotExtendable.Count + booksSoon.Count > 0))
       or (booksOverdue.Count > 0) then begin
      if booksOverdue.Count > 0 then begin
        title := format(plural(booksOverdue.Count, rsAlertTitleOverdueOne, rsAlertTitleOverdueMany), [booksOverdue.Count]) + ' ';
        text :=  format(plural(booksOverdue.Count, rsAlertTextOverdueOne,  rsAlertTextOverdueMany), [booksOverdue.Count, DateToPrettyStrGrammarDateItself(minDateOverdue)])
      end;
      if (booksSoonNotExtendable.Count > 0) or (booksSoon.Count > 0) then begin
        title += format(plural(booksSoonNotExtendable.Count+booksSoon.Count, rsAlertTitleSoonOne, rsAlertTitleSoonMany), [booksSoonNotExtendable.Count+booksSoon.Count]);
        if booksSoonNotExtendable.Count > 0 then begin
          if text <> '' then text += '; ';
          text +=  format(plural(booksSoonNotExtendable.Count, rsAlertTextDueOne,  rsAlertTextDueMany), [booksSoonNotExtendable.Count, DateToPrettyStrGrammarFuture(minDateSoonNotExtendable)])
        end;
        if booksSoon.Count > 0 then begin
          if text <> '' then text += '; ';
          text +=  format(plural(booksSoon.Count, rsAlertTextRenewableDueOne,  rsAlertTextRenewableDueMany), [booksSoon.Count, DateToPrettyStrGrammarFuture(minDateSoon)])
        end;
      end;
    end;

    if title = '' then result := nil
    else with j do begin
      result := newStringArray(2);
      setStringArrayElement(result, 0, title);
      setStringArrayElement(result, 1, text);
    end;

    booksSoon.Free;
    booksSoonNotExtendable.free;
    booksOverdue.Free;

    lastWarnDate:=currentDate;
    try
      userConfig.WriteInteger('base','last-warn-date',currentDate);
    except
      on e: EStreamError do ; //bug report: de.benibela.videlibri.InternalError: Interner Fehler: Stream write error at de.benibela.videlibri.Bridge.VLGetNotifications(Native Method)

    end;
  except
    on e: Exception do throwExceptionToJava(e);
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
  procedure OnTakePendingMessageImpl(sender: TObject; book: TBook; apendingMessage: TPendingMessage);
  procedure OnPendingMessageCompletedImpl(Sender: TObject);
  procedure OnExceptionImpl(Sender: TObject);
  constructor create;
  destructor destroy; override;
private
  function searchParamsToJava: jobject;
end;

function TLibrarySearcherAccessWrapper.searchParamsToJava: jobject;
var
  params: TFormParams;
begin
  //beginResultReading;
  params := searcher.SearchParams;
  params._AddRef;
  //endResultReading;
  result := params.toJava;
  params._Release;
end;

procedure TLibrarySearcherAccessWrapper.OnConnectedImpl(sender: TObject);
var args: array[0..0] of jvalue;
  params: TFormParams;
begin
  needJ;
  if searcher = nil then begin
    if logging then log('No searcher');
    exit;
  end;
  args[0].l := searchParamsToJava;
  j.callVoidMethod(jsearcher, searcherOnConnected, @args);
  j.deleteLocalRef(args[0].l);
end;

procedure TLibrarySearcherAccessWrapper.OnSearchPageCompleteImpl(sender: TObject; firstPage, nextPageAvailable: boolean);
var
  books: jobject;
  i: Integer;
  temp: jobject;
begin
  needJ;
  if searcher = nil then begin
    if logging then log('No searcher');
    exit;
  end;

  with j do begin
    SetIntField(jsearcher, searcherFields.totalResultCountI, searcher.SearchResultCount);
    SetBooleanField(jsearcher, searcherFields.nextPageAvailableZ, nextPageAvailable);
    books := newObjectArray(searcher.SearchResult.Count, bookClass, nil);
    for i := 0 to searcher.SearchResult.Count-1 do begin
      temp := bookToJBook(searcher.SearchResult[i], nil, false, true);
      SetObjectArrayElement(books, i, temp);
      deleteLocalRef(temp);
    end;
    WriteBarrier;
    if firstPage then callVoidMethod(jsearcher, searcherOnSearchFirstPageComplete, @books)
    else callVoidMethod(jsearcher, searcherOnSearchNextPageComplete, @books);
    deleteLocalRef(books);
  end;
end;

procedure TLibrarySearcherAccessWrapper.OnDetailsCompleteImpl(sender: TObject; book: TBook);
var
  jbook: jobject;
begin
  jbook := bookToJBook(book, nil, false, true);
  j.callVoidMethod(jsearcher, searcherOnSearchDetailsComplete, @jbook);
  j.deleteLocalRef(jbook);
end;

procedure TLibrarySearcherAccessWrapper.OnOrderCompleteImpl(sender: TObject; book: TBook);
var
  jbook: jobject;
  acc: TCustomAccountAccess;
  temp: TBook;
begin
  if book.owningAccount <> nil then begin
    //see androidutils/bookSearchForm
    self.beginBookReading;
    EnterCriticalSection(updateThreadConfig.libraryAccessSection);
    acc := TCustomAccountAccess(book.owningAccount);
    temp := book.clone; temp.status:=bsOrdered;
    acc.books.current.add(temp);
    temp := book.clone; temp.status:=bsOrdered;
    acc.books.currentUpdate.add(temp); //cannot know which one is the correct one? one will be discarded?
    acc.saveBooks();
    LeaveCriticalSection(updateThreadConfig.libraryAccessSection);
    self.endBookReading;
  end;

  jbook := bookToJBook(book, nil, false, true);
  j.callVoidMethod(jsearcher, searcherOnOrderComplete, @jbook);
  j.deleteLocalRef(jbook);
end;

procedure TLibrarySearcherAccessWrapper.OnTakePendingMessageImpl(sender: TObject; book: TBook; apendingMessage: TPendingMessage);
var args: array[0..2] of jvalue;
begin
  if pendingMessage <> nil then FreeAndNil(pendingMessage);
  pendingMessageBook := book;
  pendingMessage := apendingMessage;

  case apendingMessage.kind of
    pmkAlert: args[0].i := 0;
    pmkConfirm: args[0].i := 1;
    pmkChoose: args[0].i := 2;
  end;
  with j do begin
    args[1].l := stringToJString(apendingMessage.caption);
    args[2].l := arrayToJArray(apendingMessage.options);
    callVoidMethod(jsearcher, searcherOnTakePendingMessage, @args[0]);
    deleteLocalRef(args[1].l);
    deleteLocalRef(args[2].l);
  end;
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
  if result = nil then raise Exception.create('Internal error: Searcher has been destroyed');
end;

function wrapSearcherPtr(const searcher: TLibrarySearcherAccess): int64;
begin
  result:=PtrInt(searcher);
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchConnect(env:PJNIEnv; this:jobject; searcher: jobject; libJid: jobject); cdecl;
var
  searcherAccess: TLibrarySearcherAccessWrapper;
  lib: TLibrary;
  libId: String;
  temp: jobject;
begin
  if logging then log('Bridge_VLSearchConnect started');
  try
    libId:=j.jStringToString(libJid);

    lib := libraryManager.get(libId);
    if lib = nil then begin
      j.SetLongField(searcher, searcherFields.nativePtrJ, wrapSearcherPtr(nil));
      exit;
    end;

    searcherAccess := TLibrarySearcherAccessWrapper.create();

    searcherAccess.OnConnected:=@searcherAccess.OnConnectedImpl;
    searcherAccess.OnSearchPageComplete:=@searcherAccess.OnSearchPageCompleteImpl;
    searcherAccess.OnDetailsComplete:=@searcherAccess.OnDetailsCompleteImpl;
    searcherAccess.OnOrderComplete:=@searcherAccess.OnOrderCompleteImpl;
    searcherAccess.OnTakePendingMessage:=@searcherAccess.OnTakePendingMessageImpl;
    searcherAccess.OnPendingMessageCompleted:=@searcherAccess.OnPendingMessageCompletedImpl;
    searcherAccess.OnException:=@searcherAccess.OnExceptionImpl;

    j.SetLongField(searcher, searcherFields.nativePtrJ, wrapSearcherPtr(searcherAccess));
    searcherAccess.jsearcher:=j.env^^.NewGlobalRef(j.env, searcher);

    WriteBarrier;

    searcherAccess.newSearch( lib.template );
  //searcherAccess.searcher.clear;
    searcherAccess.searcher.addLibrary(lib);

    searcherAccess.connectAsync;
    searcherAccess.beginResultReading;
    try
      temp := searcherAccess.searchParamsToJava;
    finally
      searcherAccess.endResultReading;
    end;
    j.SetObjectField(searcherAccess.jsearcher, searcherFields.searchParamsL, temp);
    j.deleteLocalRef(temp);

  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('Bridge_VLSearchConnect ended');
end;


procedure Java_de_benibela_VideLibri_Bridge_VLSearchStart(env:PJNIEnv; this:jobject; searcher: jobject; query: jobject); cdecl;
var
  searcherAccess: TLibrarySearcherAccessWrapper;
  temp: jobject;
  tempbook: TBook;
begin
  if logging then log('Bridge_VLSearchStart started');
  try
    with bookFields do begin
      searcherAccess := unwrapSearcher(searcher);

      searcherAccess.prepareNewSearchWithoutDisconnect;
      if searcherAccess.searcher = nil then begin
        if logging then log('No searcher');
        exit;
      end;

      with j do begin
        tempbook := jbookToBook(query);
        //that is not thread safe, but as long as no smtSearch message is handled it should be okay
        searcherAccess.searcher.SearchOptions.assignAll(tempbook);
        tempbook.free;
      end;

      WriteBarrier;

      searcherAccess.searchAsync;

    end;
  except
    on e: Exception do throwExceptionToJava(e);
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
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('Bridge_VLSearchNextPage ended');
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
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('Bridge_VLSearchDetails ended');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchOrder(env:PJNIEnv; this:jobject; searcher: jobject; jbooks: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
  book: TBook;
  i: Integer;

begin
  if logging then log('Bridge_VLSearchOrder started');
  try
    sa := unwrapSearcher(searcher);
    with j do
    for i := 0 to getArrayLength(jbooks) - 1 do begin
      book :=  jbookToBookAndDelete(getObjectArrayElement(jbooks, i));
      if book.owningAccount = nil then begin
        log('Owner of book '+book.title+' not found');
        continue;
      end;
      sa.orderAsync(TCustomAccountAccess(book.owningAccount), book);
    end;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('Bridge_VLSearchOrder ended');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchOrderWithHolding(env:PJNIEnv; this:jobject; searcher: jobject; jbooks: jobject; jholdingids: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
  book: TBook;
  i: Integer;
  holdingIds: TLongintArray;

begin
  if logging then log('Bridge_VLSearchOrder started');
  try
    sa := unwrapSearcher(searcher);
    holdingIds := j.getIntArray(jholdingids);
    with j do
    for i := 0 to getArrayLength(jbooks) - 1 do begin //array is always single element
      book :=  jbookToBookAndDelete(getObjectArrayElement(jbooks, i));
      if book.owningAccount = nil then begin
        log('Owner of book '+book.title+' not found');
        continue;
      end;

      sa.orderAsync(TCustomAccountAccess(book.owningAccount), book.holdings[holdingIds[0]]);
    end;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('Bridge_VLSearchOrder ended');
end;




procedure Java_de_benibela_VideLibri_Bridge_VLSearchCompletePendingMessage(env:PJNIEnv; this:jobject; searcher: jobject; res: jint); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
begin
  if logging then log('Bridge_VLSearchOrderCompletePendingMessage started');
  try
    sa := unwrapSearcher(searcher);
    if sa.pendingMessage <> nil then
      sa.completePendingMessage(sa.pendingMessageBook, sa.pendingMessage, res);
    sa.pendingMessage := nil;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('Bridge_VLSearchOrderCompletePendingMessage ended');
end;


procedure Java_de_benibela_VideLibri_Bridge_VLSearchEnd(env:PJNIEnv; this:jobject; searcher: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
begin
  if logging then log('Bridge_VLSearchEnd started');
  try
    sa := unwrapSearcher(searcher);
    sa.freeAsync;
    j.SetLongField(searcher, searcherFields.nativePtrJ, 0);
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then log('Bridge_VLSearchEnd stopped');
end;


function Java_de_benibela_VideLibri_Bridge_VLGetOptions(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  i: Integer;
  temp: TOptionsShared;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptions (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  result := nil;
  try
    temp := TOptionsShared.Create;
    temp.nearTime := userConfig.ReadInteger('base','near-time',3);
    temp.refreshInterval := RefreshInterval;
    temp.userLibIds := strSplit(userConfig.ReadString('base', 'user-libraries', ''), ',');
    while (length(temp.userLibIds) > 0) and (temp.userLibIds[high(temp.userLibIds)] = '') do setlength(temp.userLibIds, high(temp.userLibIds));
    for i := 0 to high(temp.userLibIds) do temp.userLibIds[i] := trim(temp.userLibIds[i]); //is this needed?
    result := temp.toJava;
    temp.free;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptions (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLSetOptions(env:PJNIEnv; this:jobject; options: jobject): jobject; cdecl;
var
  temp: TOptionsShared;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptions (started)');
  result := nil;
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    temp := TOptionsShared.fromJava(options);
    userConfig.WriteInteger('base','near-time', temp.nearTime);
    updateGlobalTimeCache;

    RefreshInterval:=temp.refreshInterval;
    userConfig.WriteInteger('access','refresh-interval',refreshInterval);
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptions (ended)');
end;



function Java_de_benibela_VideLibri_Bridge_VLGetOptionsAndroidOnly(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  options: TOptionsAndroidOnly;
  androidConfigPath: String;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptionsAndroidOnly (started)');
  androidConfigPath := userPath + 'user-config-android.json';
  result := nil;
  try
    if FileExists(androidConfigPath) then options := TOptionsAndroidOnly.fromJSON(strLoadFromFile(androidConfigPath))
    else begin
      if logging then log('Config '+androidConfigPath + ' does not exist');
      options := TOptionsAndroidOnly.Create;
      options.logging := userConfig.ReadBool('base', 'logging', logging);
    end;
    logging := options.logging;
    result := options.toJava;
    options.free
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptionsAndroidOnly (ended)');
end;


function Java_de_benibela_VideLibri_Bridge_VLSetOptionsAndroidOnly(env:PJNIEnv; this:jobject; options: jobject): jobject; cdecl;
var
  androidConfigPath: String;
  temp: TOptionsAndroidOnly;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptionsAndroidOnly (started)');
  androidConfigPath := userPath + 'user-config-android.json';
  result := nil;
  try
    temp := TOptionsAndroidOnly.fromJava(options);
    logging := temp.logging;
    strSaveToFile(androidConfigPath, temp.toJSON());
    temp.free;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptionsAndroidOnly (ended)');
end;


const ExportImportMap: array[0..3] of TImportExportFlag = (eifCurrent,eifHistory,eifConfig,eifPassword);
function exportImportFlagsToInt(flags: TImportExportFlags): Integer;
var
  f: TImportExportFlag;
begin
  result := 0;
  for f in ExportImportMap do
    if f in flags then result := result or ord(f);
end;

function exportImportIntToFlags(flags: integer): TImportExportFlags;
var
  f: TImportExportFlag;
begin
  result := [];
  for f in ExportImportMap do
    if (ord(f)) and flags <> 0 then include(result, f);
end;

function Java_de_benibela_VideLibri_Bridge_VLExportAccounts(env:PJNIEnv; this:jobject; filename: jstring; accounts: jobjectArray; flags: jint): jobject; cdecl;
var accs: array of TCustomAccountAccess;
    flgs: TImportExportFlagsArray;
    acc: jobject;
    i: Integer;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLExportAccounts (started)');
  result := nil;
  try
    setlength(accs, j.getArrayLength(accounts));
    if length(accs) > 0 then begin
      SetLength(flgs, length(accs));
      flgs[0] := exportImportIntToFlags(flags);
      for i := 0 to high(accs) do begin
        acc := j.getObjectArrayElement(accounts,i);
        accs[i] := getRealAccountChecked(acc);
        j.deleteLocalRef(acc);
        flgs[i] := flgs[0];
      end;

      exportAccounts(j.jStringToString(filename), accs, flgs);
    end;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLExportAccounts (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLImportAccountsPrepare(env:PJNIEnv; this:jobject; filename: jstring): jobject; cdecl;
var
  parser: TTreeParser;
  importData: TImportExportData;
  accounts: TStringArray;
  flags: TImportExportFlagsArray;
  combinedFlags: Integer;
  i: Integer;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLImportAccountsPrepare (started)');
  try
    importAccountsPrepare(j.jStringToString(filename), parser, accounts, flags);

    importData.nativePtr := ptrint(parser);
    importData.accountsToImport := accounts;

    combinedFlags := 0;
    for i := 0 to high(flags) do
      combinedFlags := combinedFlags or exportImportFlagsToInt(flags[i]);

    importData.flags := combinedFlags;
    result := importData.toJava;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLImportAccountsPrepare (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLImportAccounts(env:PJNIEnv; this:jobject; data: jobject): jobject; cdecl;
var
  parser: TTreeParser;
  flags: TImportExportFlagsArray = nil;
  combinedFlags: TImportExportFlags;
  i: Integer;
  importData: TImportExportData;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLImportAccounts (started)');
  result := nil;
  try
    importData := TImportExportData.fromJava(data);
    parser := TTreeParser(PtrInt(importData.nativePtr));
    combinedFlags := exportImportIntToFlags(importData.flags);
    setlength(flags, length(importdata.accountsToImport));
    for i := 0 to high(flags) do
      flags[i] := combinedFlags;
    importAccounts(parser, importData.accountsToImport, flags);
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLImportAccounts (ended)');
end;


function Java_de_benibela_VideLibri_Bridge_VLXQuery(env:PJNIEnv; this:jobject; query: jobject): jobject; cdecl;
var r: TBookListReader;
  b, list:ixqvalue;
  book: TBook;
  p: TXQProperty;
  newelement, accs, tempjobject: jobject;
  pos, accountId, i: integer;
  accs2: array of jobject;
  i64: Int64;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLXQuery (started)');
  result := nil;
  r:=TBookListReader.create(nil);
  book := TBook.create;
  EnterCriticalsection(updateThreadConfig.libraryAccessSection);
  try
    try
      list := queryHistory(r,j.jStringToString(query));
      result := j.newObjectArray(list.getSequenceCount, bookClass,nil);
      accs := Java_de_benibela_VideLibri_Bridge_VLGetAccounts(env,this);
      SetLength(accs2, accounts.Count);
      pos := 0;
      for b in list do begin
        case b.kind of
          pvkObject: begin
            book.clear;
            book.owningAccount:=nil;
            accountId := -1;
            for p in b.getEnumeratorStringPropertiesUnsafe do
              case p.key of
                '_accountPtr': begin
                  i64 := p.value.toInt64;
                  for i := 0 to accounts.Count - 1 do
                    if i64 = PtrInt(accounts[i]) then begin
                      accountId := i;
                      break;
                    end;
                end;//book.owner := tobject(p.Value.toInt64);
                'statusId': if p.Value.toString = 'history' then book.lend := false
                else begin
                  book.lend := true;
                  book.setProperty(p.key, p.Value.toString);
                end
                else book.setProperty(p.key, p.Value.toString);
              end;
            if accountId <> -1 then begin
              if accs2[accountId] = nil then accs2[accountId] := j.getObjectArrayElement(accs, accountId);
              newelement := bookToJBook(book, accs2[accountId]);
            end else
              newelement := bookToJBook(book, nil);
            j.SetBooleanField(newelement, bookFields.historyZ, not book.lend);
          end;
          else with j do begin
            tempjobject :=  stringToJString(b.toString);
            newelement:=newObject(bookClass, bookClassInitWithTitle, @tempjobject);
            deleteLocalRef(tempjobject);
          end;
        end;
        j.setObjectArrayElement(Result, pos, newelement);
        j.deleteLocalRef(newelement);
        inc(pos);
      end;
    finally
      book.free;
      LeaveCriticalsection(updateThreadConfig.libraryAccessSection);
      r.free;
    end;
  except
    on e: Exception do begin
      newelement:=j.newObject(bookClass, bookClassInit);;
      j.SetStringField(newelement, bookFields.titleS, e.Message);
      result := j.newObjectArray(1, bookClass, newelement);
    end;
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLXQuery (ended)');
end;


function Java_de_benibela_VideLibri_Bridge_VLGetCoverURLs(env:PJNIEnv; this:jobject; ISBN: jobject ; maxWidth: jint ; maxHeight: jint): jobject; cdecl;
var
  covers: TStringArray;
  pisbn: String;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetCoverURLs (started)');
  result := nil;
  try
    pisbn := needJ.jStringToString(ISBN);
    //if logging then log('ISBN: '+pisbn);
    covers := tbook.getCoverURLs(pisbn, maxWidth, maxHeight);
    //if logging then log('covers');
    result := j.arrayToJArray(covers);
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetCoverURLs (ended)');
end;


function Java_de_benibela_VideLibri_Bridge_VLGetVersion(env:PJNIEnv; this:jobject ): jobject; cdecl;
const CPU_PLATFORM = {$ifdef CPUARM}{$ifdef CPU32}'arm 32-bit'{$endif}{$endif}
                     {$ifdef CPUAARCH64}'aarch64'{$endif}
                     {$ifdef CPUX86}{$ifdef CPU32}'x86 32-bit'{$endif}{$endif}
                     {$ifdef CPUX86_64}'x86_64'{$endif};
var
  vi: TVersionInfo;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetVersion (started)');
  result := nil;
  try
    vi := default(TVersionInfo);
    vi.version := FloatToStr(versionNumber / 1000);
    vi.platform := CPU_PLATFORM;
    vi.buildId := versionBuildId;
    result := vi.toJava;
  except
    on e: Exception do throwExceptionToJava(e);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetVersion (ended)');
end;




const nativeMethods: array[1..40] of JNINativeMethod=
  ((name:'VLInit';          signature:'(Landroid/content/Context;)V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLInit)
   ,(name:'VLFinalize';      signature:'()V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLFInit)

   ,(name:'VLGetLibraryIds'; signature:'()[Ljava/lang/String;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetLibraryIds)
   ,(name:'VLGetLibraryDetails'; signature: '(Ljava/lang/String;Z)Lde/benibela/videlibri/jni/LibraryDetails;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetLibraryDetails)
   ,(name:'VLSetLibraryDetails'; signature:'(Ljava/lang/String;Lde/benibela/videlibri/jni/LibraryDetails;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLSetLibraryDetails)
   ,(name:'VLInstallLibrary'; signature:'(Ljava/lang/String;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLInstallLibrary)
   ,(name:'VLGetTemplates'; signature:'()[Ljava/lang/String;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetTemplates)
   ,(name:'VLGetTemplateDetails'; signature: '(Ljava/lang/String;)Lde/benibela/videlibri/jni/TemplateDetails;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetTemplateDetails)
   ,(name:'VLReloadLibrary'; signature:'(Ljava/lang/String;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLReloadLibrary)
   ,(name:'VLReloadTemplate'; signature:'(Ljava/lang/String;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLReloadTemplate)

   ,(name:'VLGetAccounts'; signature:'()[Lde/benibela/videlibri/jni/Bridge$Account;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetAccounts)
   ,(name:'VLAddAccount'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLAddAccount)
   ,(name:'VLDeleteAccount'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLDeleteAccount)
   ,(name:'VLChangeAccount'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;Lde/benibela/videlibri/jni/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLChangeAccount)
   ,(name:'VLGetBooks'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;Z)[Lde/benibela/videlibri/jni/Bridge$Book;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetBooks)
   ,(name:'VLChangeBook'; signature:'(Lde/benibela/videlibri/jni/Bridge$Book;Lde/benibela/videlibri/jni/Bridge$Book;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLChangeBook)
   ,(name:'VLGetCriticalBook'; signature:'()Lde/benibela/videlibri/jni/Bridge$Book;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetCriticalBook)

   ,(name:'VLUpdateAccount'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;ZZ)Z'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLUpdateAccounts)
   ,(name:'VLBookOperation'; signature:'([Lde/benibela/videlibri/jni/Bridge$Book;I)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLBookOperation)
   ,(name:'VLTakePendingExceptions'; signature: '()Lde/benibela/videlibri/jni/PendingExceptions;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetPendingExceptions)
   ,(name:'VLSendFeedback'; signature: '([Ljava/lang/String;)Z'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSendFeedback)

   ,(name:'VLGetNotifications'; signature: '()[Ljava/lang/String;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetNotifications)

   ,(name:'VLSearchConnect'; signature: '(Lde/benibela/videlibri/jni/SearcherAccessPascal;Ljava/lang/String;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchConnect)
   ,(name:'VLSearchStart'; signature: '(Lde/benibela/videlibri/jni/SearcherAccessPascal;Lde/benibela/videlibri/jni/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchStart)
   ,(name:'VLSearchNextPage'; signature: '(Lde/benibela/videlibri/jni/SearcherAccessPascal;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchNextPage)
   ,(name:'VLSearchDetails'; signature: '(Lde/benibela/videlibri/jni/SearcherAccessPascal;Lde/benibela/videlibri/jni/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchDetails)
   ,(name:'VLSearchOrder'; signature: '(Lde/benibela/videlibri/jni/SearcherAccessPascal;[Lde/benibela/videlibri/jni/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchOrder)
   ,(name:'VLSearchOrder'; signature: '(Lde/benibela/videlibri/jni/SearcherAccessPascal;[Lde/benibela/videlibri/jni/Bridge$Book;[I)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchOrderWithHolding)
   ,(name:'VLSearchCompletePendingMessage'; signature: '(Lde/benibela/videlibri/jni/SearcherAccessPascal;I)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchCompletePendingMessage)
   ,(name:'VLSearchEnd'; signature: '(Lde/benibela/videlibri/jni/SearcherAccessPascal;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchEnd)

   ,(name:'VLSetOptions'; signature: '(Lde/benibela/videlibri/jni/OptionsShared;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSetOptions)
   ,(name:'VLGetOptions'; signature: '()Lde/benibela/videlibri/jni/OptionsShared;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetOptions)
   ,(name:'VLGetOptionsAndroidOnly'; signature: '()Lde/benibela/videlibri/jni/OptionsAndroidOnly;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetOptionsAndroidOnly)
   ,(name:'VLSetOptionsAndroidOnly'; signature: '(Lde/benibela/videlibri/jni/OptionsAndroidOnly;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSetOptionsAndroidOnly)

   ,(name:'VLExportAccounts'; signature: '(Ljava/lang/String;[Lde/benibela/videlibri/jni/Bridge$Account;I)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLExportAccounts)
   ,(name:'VLImportAccountsPrepare'; signature: '(Ljava/lang/String;)Lde/benibela/videlibri/jni/ImportExportData;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLImportAccountsPrepare)
   ,(name:'VLImportAccounts'; signature: '(Lde/benibela/videlibri/jni/ImportExportData;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLImportAccounts)

   ,(name:'VLXQuery'; signature: '(Ljava/lang/String;)[Lde/benibela/videlibri/jni/Bridge$Book;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLXQuery)

   ,(name:'VLGetCoverURLs'; signature: '(Ljava/lang/String;II)[Ljava/lang/String;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetCoverURLs)

   ,(name:'VLGetVersion'; signature: '()Lde/benibela/videlibri/jni/VersionInfo;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetVersion)
   );




function loaded: integer;
var bridgeClass: jclass;
begin
  needJ;
  bridgeClass := j.env^^.FindClass(j.env, 'de/benibela/videlibri/jni/Bridge');
  if (not assigned(bridgeClass)) or (j.ExceptionCheck()) then begin
    bbdebugtools.log('failed to find VideLibri Bridge');
    exit(JNI_ERR);
  end;
  if j.env^^.RegisterNatives(j.env, bridgeClass, @NativeMethods[low(nativeMethods)],length(NativeMethods)) < 0 then begin
    bbdebugtools.log('failed to register methods');
    exit(JNI_ERR);
  end;
  setCustomClassLoaderFromLoadedClass(bridgeClass);
  result := bbjniutils.JNI_VERSION_DEFAULT;
end;

{$endif}


function iniFileFromString(data: string): TIniFile;
var
  stream: TStringAsMemoryStream;
begin
  stream := TStringAsMemoryStream.Create(data);
  result := TIniFile.Create(stream);
  //stream.free;
end;

end.

