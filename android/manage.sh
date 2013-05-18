#!/bin/bash

export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.26/jre 
export SDK_HOME=/home/benito/opt/android-sdk-linux/platform-tools/

if [[ $1 == "build" ]]; then

  if [[ $2 == "arm" || $3 == "arm" || $2 != "x86" ]]; then BUILDARM=true
  else BUILDARM=false; fi

  if [[ $2 == "x86" || $3 == "x86" || $2 != "arm" ]]; then BUILDX86=true
  else BUILDX86=false; fi


  if [[ $2 == "release" ]]; then BUILDMODE=release
  else BUILDMODE=debug;  fi

  #echo $BUILDARM :: $BUILDX86

  if ! $BUILDARM ; then rm android/libs/armeabi/liblclapp.so; fi;
  if ! $BUILDX86 ; then rm android/libs/x86/liblclapp.so; fi;

  if $BUILDARM; then
    if /opt/lazarus/lazbuild --os=android --ws=customdrawn --cpu=arm videlibriandroid.lpi; then echo; else echo "FAILED!"; exit 1; fi
  fi

  if $BUILDX86; then
    if /opt/lazarus/lazbuild --os=android --ws=customdrawn --cpu=i386 videlibriandroid.lpi; then echo; else echo "FAILED!"; exit 1; fi
  fi

  if [[ $BUILDMODE == "release" ]]; then
    strip --strip-all android/libs/armeabi/liblclapp.so
    strip --strip-all android/libs/x86/liblclapp.so
  fi

  cd android
  ant $BUILDMODE || (echo "FAILED!"; exit)

  $SDK_HOME/adb uninstall de.benibela.videlibri || (echo "FAILED!"; exit)
  $SDK_HOME/adb install bin/videlibri-debug.apk || (echo "FAILED!"; exit)
elif [[ $1 == "install" ]]; then

  cd android
  $SDK_HOME/adb uninstall de.benibela.videlibri || (echo "FAILED!"; exit)
  $SDK_HOME/adb install bin/videlibri-debug.apk || (echo "FAILED!"; exit)
elif [[ $1 == "clean" ]]; then
  rm android/libs/armeabi/liblclapp.so; 
  rm android/libs/x86/liblclapp.so; 
  cd android
  ant clean
fi



