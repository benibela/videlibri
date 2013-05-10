#!/bin/bash

#source ../../../../manageUtils.sh
export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.26/jre 
export SDK_HOME=/home/benito/opt/android-sdk-linux/platform-tools/

rm android/libs/armeabi/liblclapp.so 
if /opt/lazarus/lazbuild --os=android --ws=customdrawn --cpu=i386 videlibriandroid.lpi; then echo; else echo "FAILED!"; exit 1; fi


cd android
if ant debug; then echo;  else echo "FAILED!"; exit; fi
$SDK_HOME/adb uninstall de.benibela.videlibri 
$SDK_HOME/adb install bin/videlibri-debug.apk || (echo "FAILED!"; exit 1)

