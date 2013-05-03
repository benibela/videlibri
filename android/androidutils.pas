unit androidutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles{$ifdef android}, jni, bbjniutils, CustomDrawnInt{$endif};

//procedure deleteLocalRef(jobj: pointer);

var assetPath: string;

procedure beginAssetRead;
function assetFileAsString(name: string): string;
procedure endAssetRead;

function iniFileFromString(data: string): TIniFile;

function getUserConfigPath: string;

procedure uninit;

implementation
uses bbutils;

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

{$else}

var assets: jobject = nil;
    assetCount: integer = 0;
    jmAssetManager_Open_StringInputStream, jmInputStream_Read_B: jmethodID;
    videlibri: jobject = nil;
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

function needVidelibri: jobject;
var
  videlibriClass: jclass;
  lvidelibri: jobject;

begin
  if videlibri <> nil then exit(videlibri);
  videlibriClass := needj.getclass('com/pascal/videlibri/VideLibri');
  lvidelibri := j.env^^.NewObjectA(j.env, videlibriClass, j.getmethod(videlibriClass, '<init>', '(Lcom/pascal/videlibri/LCLActivity;)V'), @javaActivityObject);
  j.RethrowJavaExceptionIfThereIsOne();
  videlibri := j.env^^.NewGlobalRef(j.env, lvidelibri);
  j.deleteLocalRef(videlibri);
  if (videlibri = nil) then raise Exception.Create('Failed to create VideLibri class');
  videLibriMethodUserPath := j.getmethod(videlibriClass, 'userPath', '()Ljava/lang/String;');
end;

function getUserConfigPath: string;
begin
  needVidelibri;
  result := j.jStringToStringAndDelete(j.callObjMethodChecked(videlibri, videLibriMethodUserPath));
end;

procedure uninit;
begin
  if videlibri <> nil then begin
    j.env^^.DeleteGlobalRef(j.env, videlibri);
    videlibri := nil;
  end;
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

