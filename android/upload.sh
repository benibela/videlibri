#!/bin/bash

#source ../../../../manageUtils.sh
export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.26/jre 
export SDK_HOME=/home/benito/opt/android-sdk-linux/platform-tools/

cd android
$SDK_HOME/adb uninstall com.benibela.videlibri || (echo "FAILED!"; exit)
$SDK_HOME/adb install bin/videlibri-debug.apk || (echo "FAILED!"; exit)

