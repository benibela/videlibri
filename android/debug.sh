#!/bin/bash

#source ../../../../manageUtils.sh
export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.26/jre 
export SDK_HOME=/home/benito/opt/android-sdk-linux/platform-tools/
export PATH=$SDK_HOME:$PATH

cd android

/home/benito//opt/android-ndk-r8e/ndk-gdb  -x debug.gdb --start

