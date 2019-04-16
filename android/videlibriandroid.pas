library videlibriandroid;

{$mode objfpc}{$H+}

{$DEFINE UseCThreads}

uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
cthreads,
{$ENDIF}{$ENDIF}
applicationconfig, libraryAccess, jni, bbjniutils, androidutils;

exports
  bbjniutils.JNI_OnLoad name 'JNI_OnLoad',
  bbjniutils.JNI_OnUnload name 'JNI_OnUnload';

function onLoad: integer;
begin
  result := androidutils.loaded;
end;

procedure onUnload;
begin
  androidutils.uninit;
end;

begin
  bbjniutils.onLoad:=@onLoad;
  bbjniutils.onUnload:=@onUnload;
end.

