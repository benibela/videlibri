unit androidutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  IniFiles{$ifdef android}, jni, bbjniutils, libraryParser{$endif};

//procedure deleteLocalRef(jobj: pointer);

var assetPath: string;

procedure beginAssetRead;
function assetFileAsString(name: string): string;
procedure endAssetRead;

function iniFileFromString(data: string): TIniFile;

function getUserConfigPath: string;

function loaded: integer;
procedure uninit;







implementation
uses bbutils, accountlist, applicationconfig, bbdebugtools;

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
    videLibriMethodUserPath: jmethodID;

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

procedure Java_de_benibela_VideLibri_Bridge_VLInit(env:PJNIEnv; this:jobject; videlibri: jobject); cdecl;
var videlibriClass: jclass;
begin
  videlibriClass := j.getclass('com/benibela/videlibri/VideLibri');
  jActivityObject := needj.env^^.NewGlobalRef(j.env, videlibri);
  videLibriMethodUserPath := j.getmethod(videlibriClass, 'userPath', '()Ljava/lang/String;');
  initApplicationConfig;
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
  result := j.env^^.NewObjectArray(j.env,   libraryManager.count, j.getclass('java/lang/String'), j.stringToJString(''));
  for i := 0 to libraryManager.count - 1 do begin
    lib := libraryManager[i];
    j.env^^.SetObjectArrayElement(j.env, result, i, j.stringToJString(lib.id+'|'+lib.prettyLocation+'|'+lib.prettyNameLong+'|'+lib.prettyNameShort));
  end;
end;

const nativeMethods: array[0..2] of JNINativeMethod=
  ((name:'VLInit';          signature:'(Lcom/benibela/videlibri/VideLibri;)V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLInit),
   (name:'VLFinalize';      signature:'()V';                   fnPtr:@Java_de_benibela_VideLibri_Bridge_VLFInit),
   (name:'VLGetLibraries'; signature:'()[Ljava/lang/String;'; fnPtr:@Java_de_benibela_VideLibri_Bridge_VLGetLibraries)
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
  if j.env^^.RegisterNatives(j.env, bridgeClass, @NativeMethods[0],length(NativeMethods)) < 0 then begin
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

