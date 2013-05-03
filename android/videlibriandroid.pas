library videlibriandroid;

{$mode objfpc}{$H+}


uses
  customdrawnint, Interfaces, Forms,
  customdrawn_android,customdrawndrawers, sendBackError, bookwatchmain, newaccountwizard_u,applicationconfig, jni, bbjniutils, androidutils;

exports
  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';

procedure MyActivityOnCreate;
begin
  DefaultStyle := dsAndroid;
  Application.Initialize;
  initApplicationConfig;
  Application.CreateForm(TmainForm, mainForm);
  Application.Run;
  //finalizeApplicationConfig;
end;

begin
  CDWidgetset.ActivityClassName := 'com/pascal/videlibri/LCLActivity';
  CDWidgetset.ActivityOnCreate := @MyActivityOnCreate;
end.

