unit androidutils;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, IniFiles, applicationconfig, jni, bbjniutils, libraryParser, LCLProc, booklistreader, librarySearcherAccess,
{$ifdef android}okhttpinternetaccess, {$ENDIF} multipagetemplate, xquery;

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
uses bbutils, accountlist, bbdebugtools, libraryAccess, simplehtmltreeparser, math;

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
  rsAlertTextRenewableDueMany = '%d verlängerbares Bücher fällig bis %1:s';

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
    videlibriContextInterface: jclass;
    videLibriContextMethodUserPath: jmethodID;
    bridgeClass: jclass;
    bridgeCallbackMethods: record VLAllThreadsDone, VLInstallationDone: jmethodID; end;
    accountClass, bookClass, libraryDetailsClass: jobject;
    accountClassInitWithData, bookClassInit, bookClassInitWithTitle, bookClassInitWithData, libraryDetailsClassInitWithData: jmethodID;
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
      homeBranchesL, searchBranchesL, nativePtrJ, totalResultCountI, nextPageAvailableZ: jfieldID;
    end;
    //searcherResultInterface: jclass;
    searcherOnConnected, searcherOnSearchFirstPageComplete, searcherOnSearchNextPageComplete, searcherOnSearchDetailsComplete, searcherOnOrderComplete, searcherOnOrderConfirm,  searcherOnTakePendingMessage, searcherOnPendingMessageCompleted, searcherOnException: jmethodID;
    importExportDataClass: jclass;
    importExportDataClassInit: jmethodID;
    importExportDataFields: record
      accountsToImportAL, flagsI, nativePtrJ: jfieldID;
    end;
    optionsClass: jclass;
    optionsClassInit: jmethodID;
    optionsClassFields: record
      nearTimeI, loggingZ, refreshIntervalI, roUserLibIdsAS: jfieldID;
    end;

    //arrayListClass: jclass;
    arrayListMethods: record
      get, size: jmethodID;
    end;

    globalStrings: record
      libraryBranch, isbn, category, status, renewCount, cancelable: jobject;
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



procedure Java_de_benibela_VideLibri_Bridge_VLInit(env:PJNIEnv; this:jobject; videlibri: jobject); cdecl;

  procedure initLocale;
  var LocaleClass: jclass;
  Locale_getDefault, Locale_getCountry: jmethodID;
  locale, country: jobject;
  begin
    with j do begin
      LocaleClass := getclass('java/util/Locale');
      Locale_getDefault := getstaticmethod(LocaleClass, 'getDefault', '()Ljava/util/Locale;');
      Locale_getCountry := getmethod(LocaleClass, 'getCountry', '()Ljava/lang/String;');
      //Locale_getLanguage := getmethod(LocaleClass, 'getLanguage', '()Ljava/lang/String;');
      locale := callStaticObjectMethod(LocaleClass, Locale_getDefault);
      if locale <> nil then begin;
        country := callObjectMethod(locale, Locale_getCountry);
        if country <> nil then begin
          localeCountry := jStringToString(country);
          deleteLocalRef(country);
        end;
        deleteLocalRef(locale);
      end;
      deleteLocalRef(LocaleClass);
    end;
  end;

var tempOkHttpBuild: TOkHttpBuildCallbackObject;
  arrayListClass: jclass;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInit (started)');
  needJ;
  try
    okhttpinternetaccess.onBuildCallback := @tempOkHttpBuild.onBuild;

    with needJ do begin
      videlibriContextInterface :=  newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge$VideLibriContext'));
      jContextObject := env^^.NewGlobalRef(env, videlibri);
      videLibriContextMethodUserPath := getmethod(videlibriContextInterface, 'userPath', '()Ljava/lang/String;');

      bridgeClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge'));
      bridgeCallbackMethods.VLAllThreadsDone := getstaticmethod(bridgeClass, 'allThreadsDone', '()V');
      bridgeCallbackMethods.VLInstallationDone := getstaticmethod(bridgeClass, 'installationDone', '(I)V');

      accountClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge$Account'));
      accountClassInitWithData := getmethod(accountClass, '<init>', '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;IZIZ)V');
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

      libraryDetailsClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge$LibraryDetails'));
      libraryDetailsClassInitWithData := getmethod(libraryDetailsClass, '<init>', '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;[Ljava/lang/String;Z)V');



      searcherClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge$SearcherAccess'));
      with searcherFields do begin
        nativePtrJ := getfield(searcherClass, 'nativePtr', 'J');
        totalResultCountI := getfield(searcherClass, 'totalResultCount', 'I');
        nextPageAvailableZ := getfield(searcherClass, 'nextPageAvailable', 'Z');
        homeBranchesL := getfield(searcherClass, 'homeBranches', '[Ljava/lang/String;');
        searchBranchesL := getfield(searcherClass, 'searchBranches', '[Ljava/lang/String;');
      end;
      searcherOnConnected := getmethod(searcherClass, 'onConnected', '([Ljava/lang/String;[Ljava/lang/String;)V');
      searcherOnSearchFirstPageComplete := getmethod(searcherClass, 'onSearchFirstPageComplete', '([Lde/benibela/videlibri/jni/Bridge$Book;)V');
      searcherOnSearchNextPageComplete := getmethod(searcherClass, 'onSearchNextPageComplete', '([Lde/benibela/videlibri/jni/Bridge$Book;)V');
      searcherOnSearchDetailsComplete := getmethod(searcherClass, 'onSearchDetailsComplete', '(Lde/benibela/videlibri/jni/Bridge$Book;)V');
      searcherOnOrderComplete := getmethod(searcherClass, 'onOrderComplete', '(Lde/benibela/videlibri/jni/Bridge$Book;)V');
      searcherOnOrderConfirm := getmethod(searcherClass, 'onOrderConfirm', '(Lde/benibela/videlibri/jni/Bridge$Book;)V');
      searcherOnTakePendingMessage := getmethod(searcherClass, 'onTakePendingMessage', '(ILjava/lang/String;[Ljava/lang/String;)V');
      searcherOnPendingMessageCompleted := getmethod(searcherClass, 'onPendingMessageCompleted', '()V');
      searcherOnException := getmethod(searcherClass, 'onException', '()V');

      optionsClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge$Options'));
      optionsClassInit := getmethod(optionsClass, '<init>', '()V');;
      with optionsClassFields do begin
        nearTimeI := getfield(optionsClass, 'nearTime', 'I');
        loggingZ := getfield(optionsClass, 'logging', 'Z');
        refreshIntervalI := getfield(optionsClass, 'refreshInterval','I');
        roUserLibIdsAS := getfield(optionsClass, 'roUserLibIds', '[Ljava/lang/String;');
      end;


      importExportDataClass := newGlobalRefAndDelete(getclass('de/benibela/videlibri/jni/Bridge$ImportExportData'));
      importExportDataClassInit := getmethod(importExportDataClass, '<init>', '()V');
      importExportDataFields.nativePtrJ := getfield(importExportDataClass, 'nativePtr', 'J');
      importExportDataFields.flagsI := getfield(importExportDataClass, 'flags', 'I');
      importExportDataFields.accountsToImportAL := getfield(importExportDataClass, 'accountsToImport', '[Ljava/lang/String;');

      arrayListClass := getclass('java/util/ArrayList');
      arrayListMethods.get := getmethod(arrayListClass, 'get', '(I)Ljava/lang/Object;');
      arrayListMethods.size := getmethod(arrayListClass, 'size', '()I');
      deleteLocalRef(arrayListClass);

      callbacks := TCallbackHolderAndroid;

      with globalStrings do begin
        libraryBranch := newGlobalRefAndDelete(NewStringUTF('libraryBranch'));
        isbn := newGlobalRefAndDelete(NewStringUTF('isbn'));
        category := newGlobalRefAndDelete(NewStringUTF('category'));
        status := newGlobalRefAndDelete(NewStringUTF('status'));
        renewCount := newGlobalRefAndDelete(NewStringUTF('renewCount'));
        cancelable := newGlobalRefAndDelete(NewStringUTF('cancelable'));
      end;
    end;

    initLocale;

    beginAssetRead;
    initApplicationConfig;
    endAssetRead;

 // bbdebugtools.OnLog := TStringNotifyEvent(procedureToMethod(TProcedure(@onLog)));

  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraryIds (ended)');
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
  template: TMultiPageTemplate;
  meta: TTemplateActionMeta;
  detailClass: jclass;
  names: jobject;
  defs: jobject;
  desc: jobject;
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

    detailClass := j.getclass('de/benibela/videlibri/jni/Bridge$TemplateDetails');
    result := j.newObject(detailClass, j.getmethod(detailClass, '<init>', '()V'));
    with getTemplateDetailsFields(detailClass), j do begin
      names := newStringArray(length(meta.variables));
      defs := newStringArray(length(meta.variables));
      desc := newStringArray(length(meta.variables));
      SetObjectField(result, variablesNames, names);
      SetObjectField(result, variablesDefault, defs);
      SetObjectField(result, variablesDescription, desc);

      for i:=0 to high(meta.variables) do begin
        setStringArrayElement(names, i, meta.variables[i].name);
        setStringArrayElement(desc, i, meta.variables[i].description);
        if meta.variables[i].hasDef then
          setStringArrayElement(defs, i, meta.variables[i].def);
      end;

      deleteLocalRef(names);
      deleteLocalRef(defs);
      deleteLocalRef(desc);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetTemplateDetails (ended)');
end;


type TLibraryDetails = record
  homepageBase, homepageCatalogue, prettyName, prettyNameShort, id, templateId, tableComment, variableNames, variableValues, segregatedAccountsZ: jfieldID;
end;
function getLibraryDetailsFields(c: jclass): TLibraryDetails;
begin
   with result, j do begin
     homepageBase := getfield(c, 'homepageBase', 'Ljava/lang/String;');
     homepageCatalogue := getfield(c, 'homepageCatalogue', 'Ljava/lang/String;');
     prettyName := getfield(c, 'prettyName', 'Ljava/lang/String;');
     prettyNameShort := getfield(c, 'prettyNameShort', 'Ljava/lang/String;');
     id := getfield(c, 'id', 'Ljava/lang/String;');
     templateId := getfield(c, 'templateId', 'Ljava/lang/String;');
     tableComment := getfield(c, 'tableComment', 'Ljava/lang/String;');
     variableNames := getfield(c, 'variableNames', '[Ljava/lang/String;');
     variableValues := getfield(c, 'variableValues', '[Ljava/lang/String;');
     segregatedAccountsZ := getfield(c, 'segregatedAccounts', 'Z');
   end;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetLibraryDetails(env:PJNIEnv; this:jobject; id: jstring): jobject; cdecl;
var
  lib: TLibrary;
  i: Integer;
  namesArray: jobject;
  valuesArray: jobject;
  libId: String;
  args: array[0..9] of jvalue;
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

      with j do begin
        args[0].l := stringToJString(lib.homepageBase);
        args[1].l := stringToJString(lib.homepageCatalogue);
        args[2].l := stringToJString(lib.prettyNameLong);
        args[3].l := stringToJString(lib.prettyNameShort);
        args[4].l := stringToJString(lib.id);
        if lib.template <> nil then args[5].l := stringToJString(lib.template.name)
        else args[5].l := stringToJString('');
        args[6].l := stringToJString(lib.tableComment);
        args[9].z := booleanToJboolean(lib.segregatedAccounts);

        namesArray := newStringArray(lib.variables.count);
        valuesArray := newStringArray(lib.variables.count);
        for i := 0 to lib.variables.count-1 do begin
          setStringArrayElement(namesArray, i, lib.variables.Names[i]);
          setStringArrayElement(valuesArray, i, lib.variables.ValueFromIndex[i]);
        end;
        args[7].l := namesArray;
        args[8].l := valuesArray;
        result := newObject(libraryDetailsClass, libraryDetailsClassInitWithData, @args[0]);
        for i := 0 to 8 do deleteLocalRef(args[i].l);
      end;
    except
      on e: Exception do begin
        storeException(e, nil, libid, '');
        result := nil;
      end;
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetLibraryDetails (ended)');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSetLibraryDetails(env:PJNIEnv; this:jobject; id: jstring; details: jobject); cdecl;
var
  i: Integer;
  names: jobject;
  values: jobject;
  libid: String;
  libXml: String;
  temp: string;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetLibraryDetails (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    libid := j.jStringToString(id);
    if details = nil then begin
      //null means delete
      libraryManager.deleteUserLibrary(libid);
    end else begin
      with getLibraryDetailsFields(libraryDetailsClass), j do begin
        libXml := '<?xml version="1.0" encoding="UTF-8"?>'+LineEnding;
        libXml += '<library>'+LineEnding;
        libXml += '   <longName value="'+xmlStrEscape(getStringField(details, prettyName))+'"/>'+LineEnding;
        if getStringField(details, homepageBase) <> '' then
          libXml += '   <homepage value="'+xmlStrEscape(getStringField(details, homepageBase))+'"/>'+LineEnding;
        if getStringField(details, homepageCatalogue) <> '' then
          libXml += '   <catalogue value="'+xmlStrEscape(getStringField(details, homepageCatalogue))+'"/>'+LineEnding;
        libXml += '   <template value="'+xmlStrEscape(getStringField(details, templateId))+'"/>'+LineEnding;
        temp := getStringField(details, tableComment);
        if temp <> '' then libXml += '   <table-comment value="'+xmlStrEscape(temp)+'"/>'+LineEnding;
        names := getObjectField(details, variableNames);
        values:= getObjectField(details, variableValues);
        for i := 0 to getArrayLength(names) - 1 do
          libXml += '   <variable name="'+xmlStrEscape(jStringToStringAndDelete(getObjectArrayElement(names, i)))+'" '+
                                 'value="'+xmlStrEscape(jStringToStringAndDelete(getObjectArrayElement(values, i)))+'"/>'+LineEnding;
        if getBooleanField(details, segregatedAccountsZ) then
          libXml += '  <segregated-accounts value="true"/>';

        libXml += '</library>';
        libraryManager.setUserLibrary(libid, libXml);
      end;
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLInstallLibrary (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLReloadLibrary(env:PJNIEnv; this:jobject; id: jobject): jobject; cdecl;
var
  surl: string;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLReloadLibrary (started)');
  try
    libraryManager.reloadLibrary(j.jStringToString(id));
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLReloadLibrary (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLReloadTemplate(env:PJNIEnv; this:jobject; id: jobject): jobject; cdecl;
var
  surl: string;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLReloadTemplate (started)');
  try
    libraryManager.reloadTemplate(j.jStringToString(id));
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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

    libraryManager.enumerateUserTemplates(templates);

    result := j.newObjectArray(templates.Count, j.getclass('java/lang/String'), nil);
    for i := 0 to templates.count - 1 do
      j.setObjectArrayElement(result, i, j.stringToJString(templates[i]));

    templates.free;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
                      temp, j.getIntField(jnewacc, ExtendDaysI),
                      j.getIntField(jnewacc, TypeI)
                      );
      end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLChangeAccount (ended)');
end;

function accountToJAccount(account: TCustomAccountAccess): jobject;
var args: array[0..7] of jvalue;
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
    result := j.newObject(accountClass, accountClassInitWithData, @args[0]);
    for i := 0 to 3 do deleteLocalRef(args[i].l);
  end;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetAccounts(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  i: Integer;
  temp: jobject;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetAccounts (started)');
  try
    result := j.newObjectArray(accounts.Count, accountClass, nil);
    with j do
    for i := 0 to accounts.Count - 1 do begin
      temp := accountToJAccount(accounts[i]);
      SetObjectArrayElement(result, i,  temp);
      deleteLocalRef(temp);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    needJ.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Konto nicht gefunden: '+needj.getStringField(acc, accountFields.LibIdS)+':'+ j.getStringField(acc, accountFields.NameS));
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
  i: Integer;
  tempi: integer;
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

    tempi := 0;
    case book.status of
      bsNormal, bsCuriousInStr: tempi := 1;
      bsProblematicInStr: tempi := 2;
      bsOrdered: tempi := 3;
      bsProvided: tempi := 4;
      bsReserved: tempi := 3; //todo

      bsAvailable: tempi := 100;
      bsLend: tempi := 101;
      bsVirtual: tempi := 102;
      bsPresentation: tempi := 103;
      bsInterLoan: tempi := 104;

      else ;
    end;
    SetIntField(jbook, bookFields.statusI, tempi);

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
      size := callIntMethodChecked(more, arrayListMethods.size);
      iv.i := 0;
      while iv.i < size do begin
        key := callObjectMethod(more, arrayListMethods.get, @iv);
        inc(iv.i);
        if iv.i >= size then break;
        value := callObjectMethod(more, arrayListMethods.get, @iv);
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

function Java_de_benibela_VideLibri_Bridge_VLGetBooks(env:PJNIEnv; this:jobject; jacc: jobject; jhistory: jboolean): jobject; cdecl;
var
  acc: TCustomAccountAccess;
  history: Boolean;
  books: TBookList;
  i: Integer;
  book: jobject;
  timeField: jfieldID;
begin
  if logging then bbdebugtools.log('Java_de_benibela_VideLibri_Bridge_VLGetBooks started');
  result := nil;
  try
    acc := getRealAccountChecked(jacc);
    if acc = nil then exit;
  except
    on e: Exception do begin
      j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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

    timeField := j.getstaticfield(bridgeClass, 'currentPascalDate', 'I');
    j.env^^.SetStaticIntField(j.env, bridgeClass, timeField, trunc(now));


  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;

  if logging then bbdebugtools.log('Java_de_benibela_VideLibri_Bridge_VLGetBooks ended');

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

  except
    on e: Exception do needJ.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('VLBookOperation ended');

  result := nil;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetPendingExceptions(env:PJNIEnv; this:jobject): jobject;
var
  pendingExceptionClass: jclass;
  pendingExceptionClassInit: jmethodID;
  pendingExceptionFields: record
    kindI, accountPrettyNamesS, errorS, libraryS, searchQueryS, detailsS, anonymousDetailsS, firstAccountUserS, firstAccountLibS: jfieldID;
  end;


var
  details, anonymousDetails, libs, queries: String;
  names, firstAccountUser, firstAccountLib: String;
  i: Integer;
  temp: jobject;
  k: Integer;
begin
  if logging then bbdebugtools.log('Bridge_VLGetPendingExceptions started');
  result := nil;

  try
    pendingExceptionClass := j.newGlobalRefAndDelete(j.getclass('de/benibela/videlibri/jni/Bridge$PendingException'));
    pendingExceptionClassInit := j.getmethod(pendingExceptionClass, '<init>', '()V');
    with pendingExceptionFields do begin
      kindI := j.getfield(pendingExceptionClass, 'kind', 'I');
      accountPrettyNamesS := j.getfield(pendingExceptionClass, 'accountPrettyNames', 'Ljava/lang/String;');
      errorS := j.getfield(pendingExceptionClass, 'error', 'Ljava/lang/String;');
      detailsS := j.getfield(pendingExceptionClass, 'details', 'Ljava/lang/String;');
      libraryS := j.getfield(pendingExceptionClass, 'library', 'Ljava/lang/String;');
      searchQueryS := j.getfield(pendingExceptionClass, 'searchQuery', 'Ljava/lang/String;');
      anonymousDetailsS := j.getfield(pendingExceptionClass, 'anonymousDetails', 'Ljava/lang/String;');
      firstAccountLibS := j.getfield(pendingExceptionClass, 'firstAccountLib', 'Ljava/lang/String;');
      firstAccountUserS := j.getfield(pendingExceptionClass, 'firstAccountUser', 'Ljava/lang/String;');
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
          queries := '';
          firstAccountUser := '';
          for k := 0 to high(errorMessageList[i].details) do begin
            details += errorMessageList[i].details[k].details+LineEnding+LineEnding;
            anonymousDetails += errorMessageList[i].details[k].anonymouseDetails+LineEnding+LineEnding;
            if not strContains(libs, errorMessageList[i].details[k].libraryId) then
              libs += errorMessageList[i].details[k].libraryId + ' ';
            if errorMessageList[i].details[k].searchQuery <> '' then
              queries += errorMessageList[i].details[k].searchQuery + ' ';
            if names <> '' then names += ', ';
            with errorMessageList[i].details[k] do
              if account <> nil then begin
                names += account.prettyName;
                if firstAccountUser = '' then begin
                  firstAccountUser := account.getUser();
                  firstAccountLib := account.getLibrary().id;
                end;
              end;
          end;
          temp.SetIntField(kindI, ord(errorMessageList[i].kind));
          temp.SetStringField(detailsS, details);
          temp.SetStringField(anonymousDetailsS, anonymousDetails);
          temp.SetStringField(accountPrettyNamesS, names);
          temp.SetStringField(errorS, errorMessageList[i].error);
          temp.SetStringField(libraryS, libs);
          temp.SetStringField(searchQueryS, queries);
          temp.SetStringField(firstAccountLibS, firstAccountLib);
          temp.SetStringField(firstAccountUserS, firstAccountUser);
        end;
        j.SetObjectArrayElement(result, i, temp);
        j.deleteLocalRef(temp);
      end;
      SetLength(errorMessageList, 0);
    finally
      system.LeaveCriticalSection(exceptionStoring);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('Bridge_VLGetPendingExceptions ended');
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
      on e: EStreamError do ; //bug report: de.benibela.videlibri.Bridge$InternalError: Interner Fehler: Stream write error at de.benibela.videlibri.Bridge.VLGetNotifications(Native Method)

    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
  if searcher = nil then begin
    if logging then log('No searcher');
    exit;
  end;
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
    acc.save();
    LeaveCriticalSection(updateThreadConfig.libraryAccessSection);
    self.endBookReading;
  end;

  jbook := bookToJBook(book, nil, false, true);
  j.callVoidMethod(jsearcher, searcherOnOrderComplete, @jbook);
  j.deleteLocalRef(jbook);
end;

procedure TLibrarySearcherAccessWrapper.OnOrderConfirmImpl(sender: TObject; book: TBook);
var
  jbook: jobject;
begin
  jbook := bookToJBook(book, nil, false, true);
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

    searcherAccess.connectAsync;
    searcherAccess.beginResultReading;
    try
      temp := j.arrayToJArray(SearcherAccess.searcher.HomeBranches);
      j.SetObjectField(searcherAccess.jsearcher, searcherFields.homeBranchesL, temp);
      j.deleteLocalRef(temp);
      temp := j.arrayToJArray(SearcherAccess.searcher.SearchBranches);
      j.SetObjectField(searcherAccess.jsearcher, searcherFields.searchBranchesL, temp);
      j.deleteLocalRef(temp);
    finally
      searcherAccess.endResultReading;
    end;

  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchConnect ended');
end;


procedure Java_de_benibela_VideLibri_Bridge_VLSearchStart(env:PJNIEnv; this:jobject; searcher: jobject; query: jobject; homeBranch, searchBranch: integer); cdecl;
var
  searcherAccess: TLibrarySearcherAccessWrapper;
  temp: jobject;
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
        //that is not thread safe, but as long as no smtSearch message is handled it should be okay
        searcherAccess.searcher.SearchOptions.author:= getStringField(query, authorS);
        searcherAccess.searcher.SearchOptions.title:= getStringField(query, titleS);
        searcherAccess.searcher.SearchOptions.year:= getStringField(query, yearS);
        temp := stringToJString('isbn');
        searcherAccess.searcher.SearchOptions.isbn:= callStringMethod(query, bookFields.getPropertyMethod, @temp);
        deleteLocalRef(temp);
        temp := stringToJString('keywords');
        searcherAccess.searcher.SearchOptions.setProperty('keywords', callStringMethod(query, bookFields.getPropertyMethod, @temp));
        deleteLocalRef(temp);
      end;

      if homeBranch >= 0 then searcherAccess.searcher.HomeBranch:=homeBranch;
      if searchBranch >= 0 then searcherAccess.searcher.searchBranch:=searchBranch;

      searcherAccess.searchAsync;

    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchOrder ended');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLSearchOrderConfirmed(env:PJNIEnv; this:jobject; searcher: jobject; jbooks: jobject); cdecl;
var
  sa: TLibrarySearcherAccessWrapper;
  book: TBook;
  i: Integer;

begin
  if logging then log('Bridge_VLSearchOrderConfirmed started');
  try
    sa := unwrapSearcher(searcher);
    with j do
    for i := 0 to getArrayLength(jbooks) - 1 do begin
      book :=  jbookToBookAndDelete(getObjectArrayElement(jbooks, i));
      sa.orderConfirmedAsync(book);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchOrderConfirmed ended');
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
    sa.free;
    j.SetLongField(searcher, searcherFields.nativePtrJ, 0);
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('Bridge_VLSearchEnd stopped');
end;


function Java_de_benibela_VideLibri_Bridge_VLGetOptions(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  i: Integer;
  userlibs: TList;
  temp: jobject;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptions (started)');
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  result := nil;
  try
    with optionsClassFields, j do begin
      result := newObject(optionsClass, optionsClassInit);
      SetIntField(result, nearTimeI, userConfig.ReadInteger('base','near-time',3));
      SetIntField(result, refreshIntervalI, RefreshInterval);
      SetBooleanField(result, loggingZ, logging);

      userlibs := libraryManager.getUserLibraries();
      temp := newStringArray(userlibs.Count);
      for i := 0 to userlibs.Count-1 do
        setStringArrayElement(temp, i, TLibrary(userlibs[i]).id);
      SetObjectFieldAndDelete(result, roUserLibIdsAS, temp);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLGetOptions (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLSetOptions(env:PJNIEnv; this:jobject; options: jobject): jobject; cdecl;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptions (started)');
  result := nil;
  //bbdebugtools.log(strFromPtr(libraryManager));
  //bbdebugtools.log(IntToStr(libraryManager.count));
  try
    with optionsClassFields, j do begin
      userConfig.WriteInteger('base','near-time', getIntField(options, nearTimeI));
      updateGlobalTimeCache;

      RefreshInterval:=getIntField(options, refreshIntervalI);
      userConfig.WriteInteger('access','refresh-interval',refreshInterval);

      logging := getBooleanField(options, loggingZ);
      userConfig.WriteBool('base','logging', logging);
    end;
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLSetOptions (ended)');
end;


const ExportImportMap: array[0..3] of TExportImportFlag = (eifCurrent,eifHistory,eifConfig,eifPassword);
function exportImportFlagsToInt(flags: TExportImportFlags): Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to high(ExportImportMap) do
    if ExportImportMap[i] in flags then result := result or (1 shl i);
end;

function exportImportIntToFlags(flags: integer): TExportImportFlags;
var
  i: Integer;
begin
  result := [];
  for i := 0 to high(ExportImportMap) do
    if (1 shl i) and flags <> 0 then include(result, ExportImportMap[i]);
end;

function Java_de_benibela_VideLibri_Bridge_VLExportAccounts(env:PJNIEnv; this:jobject; filename: jstring; accounts: jobjectArray; flags: jint): jobject; cdecl;
var accs: array of TCustomAccountAccess;
    flgs: TExportImportFlagsArray;
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
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLExportAccounts (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLImportAccountsPrepare(env:PJNIEnv; this:jobject; filename: jstring): jobject; cdecl;
var
  parser: TTreeParser;
  accounts: TStringArray;
  flags: TExportImportFlagsArray;
  combinedFlags: Integer;
  i: Integer;
  jaccounts: jobject;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLImportAccountsPrepare (started)');
  try
    result := j.newObject(importExportDataClass, importExportDataClassInit);

    importAccountsPrepare(j.jStringToString(filename), parser, accounts, flags);

    j.SetLongField(result, importExportDataFields.nativePtrJ, ptrint(parser));

    jaccounts := j.newObjectArray(length(accounts), j.getclass('java/lang/String'), nil);
    for i := 0 to high(accounts) do
      j.setObjectArrayElement(jaccounts, i, j.NewStringUTF(accounts[i]));
    j.SetObjectField(result, importExportDataFields.accountsToImportAL, jaccounts);

    combinedFlags := 0;
    for i := 0 to high(flags) do
      combinedFlags := combinedFlags or exportImportFlagsToInt(flags[i]);

    j.SetIntField(result, importExportDataFields.flagsI, combinedFlags);
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLImportAccountsPrepare (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLImportAccounts(env:PJNIEnv; this:jobject; data: jobject): jobject; cdecl;
var
  parser: TTreeParser;
  jaccounts: jobject;
  accounts: TStringArray;
  flags: TExportImportFlagsArray;
  combinedFlags: TExportImportFlags;
  i: Integer;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLImportAccounts (started)');
  result := nil;
  try
    parser := TTreeParser(PtrInt(j.getLongField(data, importExportDataFields.nativePtrJ)));
    jaccounts := j.getObjectField(data, importExportDataFields.accountsToImportAL);
    combinedFlags := exportImportIntToFlags(j.getIntField(data, importExportDataFields.flagsI));
    setlength(flags, j.getArrayLength(jaccounts));
    for i := 0 to high(flags) do
      flags[i] := combinedFlags;
    setlength(accounts, j.getArrayLength(jaccounts));
    for i := 0 to high(accounts) do
      accounts[i] := j.jStringToStringAndDelete(j.getObjectArrayElement(jaccounts, i));
    importAccounts(parser, accounts, flags);
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
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
            for p in b.getPropertyEnumerator do
              case p.Name of
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
                  book.setProperty(p.Name, p.Value.toString);
                end
                else book.setProperty(p.Name, p.Value.toString);
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


function Java_de_benibela_VideLibri_Bridge_VLNormalizeISBN(env:PJNIEnv; this:jobject; isbn: jstring; removeSep: jboolean; conversion: jint): jobject; cdecl;
begin
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLNormalizeISBN (started)');
  result := nil;
  try
    with needJ do
      result := stringToJString(TBook.getNormalizedISBN(jStringToString(isbn), removeSep <> JNI_FALSE, conversion));
  except
    on e: Exception do j.ThrowNew('de/benibela/videlibri/jni/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then bbdebugtools.log('de.benibela.VideLibri.Bride.VLNormalizeISBN (ended)');
end;




const nativeMethods: array[1..36] of JNINativeMethod=
  ((name:'VLInit';          signature:'(Lde/benibela/videlibri/jni/Bridge$VideLibriContext;)V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLInit)
   ,(name:'VLFinalize';      signature:'()V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLFInit)

   ,(name:'VLGetLibraryIds'; signature:'()[Ljava/lang/String;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetLibraryIds)
   ,(name:'VLGetLibraryDetails'; signature:'(Ljava/lang/String;)Lde/benibela/videlibri/jni/Bridge$LibraryDetails;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetLibraryDetails)
   ,(name:'VLSetLibraryDetails'; signature:'(Ljava/lang/String;Lde/benibela/videlibri/jni/Bridge$LibraryDetails;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLSetLibraryDetails)
   ,(name:'VLInstallLibrary'; signature:'(Ljava/lang/String;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLInstallLibrary)
   ,(name:'VLReloadLibrary'; signature:'(Ljava/lang/String;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLReloadLibrary)
   ,(name:'VLGetTemplates'; signature:'()[Ljava/lang/String;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetTemplates)
   ,(name:'VLGetTemplateDetails'; signature:'(Ljava/lang/String;)Lde/benibela/videlibri/jni/Bridge$TemplateDetails;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetTemplateDetails)
   ,(name:'VLReloadTemplate'; signature:'(Ljava/lang/String;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLReloadTemplate)

   ,(name:'VLAddAccount'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLAddAccount)
   ,(name:'VLDeleteAccount'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLDeleteAccount)
   ,(name:'VLChangeAccount'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;Lde/benibela/videlibri/jni/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLChangeAccount)
   ,(name:'VLGetAccounts'; signature:'()[Lde/benibela/videlibri/jni/Bridge$Account;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetAccounts)
   ,(name:'VLGetBooks'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;Z)[Lde/benibela/videlibri/jni/Bridge$Book;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetBooks)
   ,(name:'VLGetCriticalBook'; signature:'()Lde/benibela/videlibri/jni/Bridge$Book;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetCriticalBook)

   ,(name:'VLUpdateAccount'; signature:'(Lde/benibela/videlibri/jni/Bridge$Account;ZZ)Z'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLUpdateAccounts)
   ,(name:'VLBookOperation'; signature:'([Lde/benibela/videlibri/jni/Bridge$Book;I)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLBookOperation)
   ,(name:'VLTakePendingExceptions'; signature: '()[Lde/benibela/videlibri/jni/Bridge$PendingException;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetPendingExceptions)

   ,(name:'VLGetNotifications'; signature: '()[Ljava/lang/String;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetNotifications)

   ,(name:'VLSearchConnect'; signature: '(Lde/benibela/videlibri/jni/Bridge$SearcherAccess;Ljava/lang/String;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchConnect)
   ,(name:'VLSearchStart'; signature: '(Lde/benibela/videlibri/jni/Bridge$SearcherAccess;Lde/benibela/videlibri/jni/Bridge$Book;II)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchStart)
   ,(name:'VLSearchNextPage'; signature: '(Lde/benibela/videlibri/jni/Bridge$SearcherAccess;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchNextPage)
   ,(name:'VLSearchDetails'; signature: '(Lde/benibela/videlibri/jni/Bridge$SearcherAccess;Lde/benibela/videlibri/jni/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchDetails)
   ,(name:'VLSearchOrder'; signature: '(Lde/benibela/videlibri/jni/Bridge$SearcherAccess;[Lde/benibela/videlibri/jni/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchOrder)
   ,(name:'VLSearchOrder'; signature: '(Lde/benibela/videlibri/jni/Bridge$SearcherAccess;[Lde/benibela/videlibri/jni/Bridge$Book;[I)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchOrderWithHolding)
   ,(name:'VLSearchOrderConfirmed'; signature: '(Lde/benibela/videlibri/jni/Bridge$SearcherAccess;[Lde/benibela/videlibri/jni/Bridge$Book;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchOrderConfirmed)
   ,(name:'VLSearchCompletePendingMessage'; signature: '(Lde/benibela/videlibri/jni/Bridge$SearcherAccess;I)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchCompletePendingMessage)
   ,(name:'VLSearchEnd'; signature: '(Lde/benibela/videlibri/jni/Bridge$SearcherAccess;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSearchEnd)

   ,(name:'VLSetOptions'; signature: '(Lde/benibela/videlibri/jni/Bridge$Options;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLSetOptions)
   ,(name:'VLGetOptions'; signature: '()Lde/benibela/videlibri/jni/Bridge$Options;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLGetOptions)

   ,(name:'VLExportAccounts'; signature: '(Ljava/lang/String;[Lde/benibela/videlibri/jni/Bridge$Account;I)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLExportAccounts)
   ,(name:'VLImportAccountsPrepare'; signature: '(Ljava/lang/String;)Lde/benibela/videlibri/jni/Bridge$ImportExportData;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLImportAccountsPrepare)
   ,(name:'VLImportAccounts'; signature: '(Lde/benibela/videlibri/jni/Bridge$ImportExportData;)V'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLImportAccounts)

   ,(name:'VLXQuery'; signature: '(Ljava/lang/String;)[Lde/benibela/videlibri/jni/Bridge$Book;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLXQuery)

   ,(name:'VLNormalizeISBN'; signature: '(Ljava/lang/String;ZI)Ljava/lang/String;'; fnPtr: @Java_de_benibela_VideLibri_Bridge_VLNormalizeISBN)
   );




function loaded: integer;
var bridgeClass: jclass;
begin
  needJ;
  bridgeClass := j.env^^.FindClass(j.env, 'de/benibela/videlibri/jni/Bridge');
  if (not assigned(bridgeClass)) or (j.env^^.ExceptionCheck(j.env)<>0) then begin
    bbdebugtools.log('failed to find VideLibri Bridge');
    exit(JNI_ERR);
  end;
  if j.env^^.RegisterNatives(j.env, bridgeClass, @NativeMethods[low(nativeMethods)],length(NativeMethods)) < 0 then begin
    bbdebugtools.log('failed to register methods');
    exit(JNI_ERR);
  end;
  setCustomClassLoaderFromLoadedClass(bridgeClass);
  result := JNI_VERSION_1_4;
end;

{$endif}


function iniFileFromString(data: string): TIniFile;
var
  stream: TStringAsMemoryStream;
begin
  stream := TStringAsMemoryStream.Create(data);
  result := TIniFile.Create(stream);
  stream.free;
end;

end.

