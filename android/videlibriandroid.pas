library videlibriandroid;

{$mode objfpc}{$H+}


uses
  Interfaces, applicationconfig, jni, bbjniutils, androidutils;

exports
  bbjniutils.JNI_OnLoad name 'JNI_OnLoad',
  bbjniutils.JNI_OnUnload name 'JNI_OnUnload';

function onLoad: integer;
begin
  result := androidutils.loaded;
end;

begin
  bbjniutils.onLoad:=@onLoad;
end.

