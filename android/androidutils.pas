unit androidutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  IniFiles{$ifdef android}, jni, bbjniutils, libraryParser, booklistreader{$endif};

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
uses bbutils, accountlist, applicationconfig, bbdebugtools, libraryAccess;

{$ifndef android}
function assetFileAsString(name: string): string;
begin
  result := strLoadFromFileUTF8(assetPath + name);
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
    jmAssetManager_Open_StringInputStream, jmInputStream_Read_B: jmethodID;
    videLibriMethodUserPath, videLibriMethodVLAllThreadsDone: jmethodID;
    accountClass, bookClass: jobject;
    accountClassInit, bookClassInit: jmethodID;
    accountFields: record
      LibIdS, NameS, PassS, PrettyNameS, ExtendDaysI, ExtendZ, HistoryZ: jfieldID;
    end;
    bookFields: record
      authorS, titleS, issueDateGC, dueDateGC: jfieldID;
    end;
    gregorianCalenderClass: jclass;
    gregorianCalenderClassInit: jmethodID;

function assetFileAsString(name: string): string;
begin
  beginAssetRead;
  try
    result := j.getAssetAsString(assets, name, jmAssetManager_Open_StringInputStream, jmInputStream_Read_B);
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
  end;
  assetCount += 1;
end;

procedure endAssetRead;
begin
  assetCount -= 1;
end;


function getUserConfigPath: string;
begin
  result := j.jStringToStringAndDelete(j.callObjMethodChecked(jActivityObject, videLibriMethodUserPath));
end;


procedure uninit;
begin
  if jActivityObject <> nil then begin
    j.env^^.DeleteGlobalRef(j.env, jActivityObject);
    jActivityObject := nil;
  end;
end;

procedure androidAllThreadsDone;
begin
  needJ.callObjMethod(jActivityObject, videLibriMethodVLAllThreadsDone);
end;

procedure Java_de_benibela_VideLibri_Bridge_VLInit(env:PJNIEnv; this:jobject; videlibri: jobject); cdecl;
var videlibriClass: jclass;
begin
  if logging then log('de.benibela.VideLibri.Bride.VLInit (started)');
  videlibriClass := j.getclass('com/benibela/videlibri/VideLibri');
  jActivityObject := needj.env^^.NewGlobalRef(j.env, videlibri);
  videLibriMethodUserPath := j.getmethod(videlibriClass, 'userPath', '()Ljava/lang/String;');
  videLibriMethodVLAllThreadsDone := j.getmethod(videlibriClass, 'allThreadsDone', '()V');

  accountClass := j.newGlobalRefAndDelete(j.getclass('com/benibela/videlibri/Bridge$Account'));
  accountClassInit := j.getmethod(accountClass, '<init>', '()V');
  with accountFields  do begin
    LibIdS := j.getfield(accountClass, 'libId', 'Ljava/lang/String;');
    NameS := j.getfield(accountClass, 'name', 'Ljava/lang/String;');
    PassS := j.getfield(accountClass, 'pass', 'Ljava/lang/String;');
    PrettyNameS := j.getfield(accountClass, 'prettyName', 'Ljava/lang/String;');
    ExtendDaysI := j.getfield(accountClass, 'extendDays', 'I');
    ExtendZ := j.getfield(accountClass, 'extend', 'Z');
    HistoryZ := j.getfield(accountClass, 'history', 'Z');
  end;

  bookClass := j.newGlobalRefAndDelete(j.getclass('com/benibela/videlibri/Bridge$Book'));
  bookClassInit := j.getmethod(bookClass, '<init>', '()V');
  with bookFields do begin
    authorS := j.getfield(bookClass, 'author', 'Ljava/lang/String;');
    titleS := j.getfield(bookClass, 'title', 'Ljava/lang/String;');
    issueDateGC := j.getfield(bookClass, 'issueDate', 'Ljava/util/GregorianCalendar;');
    dueDateGC := j.getfield(bookClass, 'dueDate', 'Ljava/util/GregorianCalendar;');
  end;

  gregorianCalenderClass := j.newGlobalRefAndDelete(j.getclass('java/util/GregorianCalendar'));
  gregorianCalenderClassInit := j.getmethod(gregorianCalenderClass, '<init>', '(III)V');



  initApplicationConfig;
  if logging then log('de.benibela.VideLibri.Bride.VLInit (ended)');
end;

procedure Java_de_benibela_VideLibri_Bridge_VLFInit(env:PJNIEnv; this:jobject); cdecl;
begin
  finalizeApplicationConfig;
end;

function Java_de_benibela_VideLibri_Bridge_VLGetLibraries(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  lib: TLibrary;
  i: Integer;
begin
  if logging then log('de.benibela.VideLibri.Bride.VLGetLibraries (started)');
  log(strFromPtr(libraryManager));
  log(IntToStr(libraryManager.count));
  try
    result := j.newObjectArray(libraryManager.count, j.getclass('java/lang/String'), j.stringToJString(''));
    for i := 0 to libraryManager.count - 1 do begin
      lib := libraryManager[i];
      j.env^^.SetObjectArrayElement(j.env, result, i, j.stringToJString(lib.id+'|'+lib.prettyLocation+'|'+lib.prettyNameLong+'|'+lib.prettyNameShort));
    end;
  except
    on e: Exception do j.ThrowNew('com/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('de.benibela.VideLibri.Bride.VLGetLibraries (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLAddAccount(env:PJNIEnv; this:jobject; acc: jobject): jobject; cdecl;
var
  temp: TExtendType;
begin
  if logging then log('de.benibela.VideLibri.Bride.VLAddAccount (started)');
  try
    with accountFields do begin
      log('AddAccount A');
      if j.getBooleanField(acc, ExtendZ) then temp := etSingleDepends
      else temp := etNever;
      log('AddAccount B');
      log('AddAccount B: '+j.getStringField(acc, LibIdS));
      log('AddAccount B: '+j.getStringField(acc, PrettyNameS));
      log('AddAccount B: '+j.getStringField(acc, NameS));
      log('AddAccount B: '+j.getStringField(acc, PassS));
      accounts.Add(j.getStringField(acc, LibIdS) , j.getStringField(acc, PrettyNameS),
                   j.getStringField(acc, NameS), j.getStringField(acc, PassS),
                   temp, j.getIntField(acc, ExtendDaysI), j.getBooleanField(acc, HistoryZ));
      log('AddAccount C');
    end;
    accounts.save;
  except
    on e: Exception do j.ThrowNew('com/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  result := nil;
  if logging then log('de.benibela.VideLibri.Bride.VLAddAccount (ended)');
end;

function Java_de_benibela_VideLibri_Bridge_VLGetAccounts(env:PJNIEnv; this:jobject): jobject; cdecl;
var
  i: Integer;
  temp: jobject;
begin
  if logging then log('de.benibela.VideLibri.Bride.VLGetAccounts (started)');
  try
    result := j.newObjectArray(accounts.Count, accountClass, nil);
    for i := 0 to accounts.Count - 1 do begin
      temp := j.newObject(accountClass, accountClassInit);
      j.SetObjectArrayElement(result, 0,  temp);
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
    on e: Exception do j.ThrowNew('com/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
  if logging then log('de.benibela.VideLibri.Bride.VLGetAccounts (ended)');
end;

function getRealAccount(acc: jobject): TCustomAccountAccess;
var
  libId: String;
  user: String;
  i: Integer;
begin
  libId := needj.getStringField(acc, accountFields.LibIdS);
  user := j.getStringField(acc, accountFields.NameS);
  for i := 0 to accounts.Count-1 do
    if (accounts[i].getUser() = user) and (accounts[i].getLibrary().id = libId) then
      exit(accounts[i]);
  result := nil;
  needJ.ThrowNew('com/benibela/videlibri/Bridge$InternalError', 'Konto nicht gefunden: '+libId+':'+user);
end;


type

{ TJBookSerializer }

TJBookSerializer = object
  book: jobject;
  procedure writeProp(n,v:string);
  procedure writeDateProp(n: string; d: integer);
end;

procedure TJBookSerializer.writeProp(n, v: string);
begin
  case n of
  'title': j.SetStringField(book, bookFields.titleS, v);
  'author': j.SetStringField(book, bookFields.authorS, v);

  end;
end;

procedure TJBookSerializer.writeDateProp(n: string; d: integer);
var args: array[0..2] of jvalue;
    temp: jobject;
    field: jfieldID;
begin
  case n of
  'issueDate': field := bookFields.issueDateGC;
  'dueDate': field := bookFields.dueDateGC;
  else exit;
  end;
  dateDecode(d, @args[0].i, @args[1].i, @args[2].i);
  temp := j.newObject(gregorianCalenderClass, gregorianCalenderClassInit, @args);
  j.SetObjectField(book, field, temp);
  j.deleteLocalRef(temp);
end;


function Java_de_benibela_VideLibri_Bridge_VLGetBooks(env:PJNIEnv; this:jobject; jacc: jobject; jhistory: jboolean): jobject; cdecl;
var
  acc: TCustomAccountAccess;
  history: Boolean;
  books: TBookList;
  i: Integer;
  temp: TJBookSerializer;
begin
  acc := getRealAccount(jacc);
  if acc = nil then exit;
  history := jhistory <> JNI_FALSE;

  system.EnterCriticalSection(updateThreadConfig.libraryAccessSection);


  try
    try
      if history then books := acc.books.current
      else books := acc.books.old;

      result := j.newObjectArray(books.Count, bookClass, nil);
      for i := 0 to books.Count - 1 do begin
        temp.book := j.newObject(bookClass, bookClassInit);
        books[i].serialize(@temp.writeProp,@temp.writeDateProp);
        j.SetObjectArrayElement(result, i, temp.book);
        j.deleteLocalRef(temp.book);
      end;
    finally
      system.LeaveCriticalsection(updateThreadConfig.libraryAccessSection)
    end;
  except
    on e: Exception do j.ThrowNew('com/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;
end;

function Java_de_benibela_VideLibri_Bridge_VLUpdateAccounts(env:PJNIEnv; this:jobject; jacc: jobject; jautoupdate: jvalue; jforceExtend: jvalue): jobject; cdecl;
var
  acc: TCustomAccountAccess;
  autoupdate: boolean;
  forceExtend: boolean;
begin
  log('VLUpdateAccounts');
  acc := getRealAccount(jacc);
  if acc = nil then begin log('x'); exit;end;
  log('a');

  autoupdate := jautoupdate.z <> JNI_FALSE;
  forceExtend := jforceExtend.z <> JNI_FALSE;

  //if ignoreConnErrors and (account.broken = currentDate) then
  //  exit;
  try
 {   EnterCriticalSection(updateThreadConfig.threadManagementSection);
    updateThreadConfig.updateThreadsRunning+=1;
    updateThreadConfig.listUpdateThreadsRunning+=1;
    LeaveCriticalSection(updateThreadConfig.threadManagementSection);
    acc.isThreadRunning:=true;

    updateBooksDirectBlocking(acc, @updateThreadConfig, autoupdate, autoupdate, forceExtend);   }
    updateAccountBookData(acc, autoupdate, autoupdate, forceExtend);
  except
    on e: Exception do j.ThrowNew('com/benibela/videlibri/Bridge$InternalError', 'Interner Fehler: '+e.Message);
  end;

  result := nil;
end;

const nativeMethods: array[1..7] of JNINativeMethod=
  ((name:'VLInit';          signature:'(Lcom/benibela/videlibri/VideLibri;)V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLInit)
   ,(name:'VLFinalize';      signature:'()V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLFInit)
   ,(name:'VLGetLibraries'; signature:'()[Ljava/lang/String;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetLibraries)
   ,(name:'VLAddAccount'; signature:'(Lcom/benibela/videlibri/Bridge$Account;)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLAddAccount)
   ,(name:'VLGetAccounts'; signature:'()[Lcom/benibela/videlibri/Bridge$Account;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetAccounts)
   ,(name:'VLGetBooks'; signature:'(Lcom/benibela/videlibri/Bridge$Account;Z)[Lcom/benibela/videlibri/Bridge$Book;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetBooks)
   ,(name:'VLUpdateAccount'; signature:'(Lcom/benibela/videlibri/Bridge$Account;ZZ)V'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLUpdateAccounts)
  );




function loaded: integer;
var bridgeClass: jclass;
begin
  needJ;

  bridgeClass := j.env^^.FindClass(j.env, 'com/benibela/videlibri/Bridge');
  if (not assigned(bridgeClass)) or (j.env^^.ExceptionCheck(j.env)<>0) then begin
    log('failed to find VideLibri Bridge');
    exit(JNI_ERR);
  end;
  if j.env^^.RegisterNatives(j.env, bridgeClass, @NativeMethods[low(nativeMethods)],length(NativeMethods)) < 0 then begin
    log('failed to register methods');
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

