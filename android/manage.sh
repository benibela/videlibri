#!/bin/bash

#export JAVA_HOME=/usr/lib/jvm/java-6-sun-1.6.0.26/jre 
#export JAVA_HOME=/usr/lib/jvm/java-7-oracle/jre 
export JAVA_HOME=/usr/lib/jvm/java-8-oracle/jre 
export SDK_HOME=/home/benito/opt/android-sdk-linux/platform-tools/
FPC_DIRECTORY=/usr/local/lib/fpc/3.1.1
FPC_ARM=$FPC_DIRECTORY/ppcrossarm
FPC_386=$FPC_DIRECTORY/ppcross386

case "$1" in
build)
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
    FORCE=""
    if [[ ! -f android/libs/armeabi/liblclapp.so ]]; then FORCE=-B; fi
    if /opt/lazarus/lazbuild $FORCE --os=android --ws=nogui --compiler=$FPC_ARM --cpu=arm videlibriandroid.lpi; then echo; else echo "FAILED!"; exit 1; fi
  fi

  if $BUILDX86; then
    FORCE=""
    if [[ ! -f android/libs/x86/liblclapp.so ]]; then FORCE=-B; fi
    if /opt/lazarus/lazbuild $FORCE --compiler=$FPC_386 --os=android --ws=nogui --cpu=i386 videlibriandroid.lpi; then echo; else echo "FAILED!"; exit 1; fi
  fi

  STRIP=true
  if [[ $BUILDMODE == "release" ]] || [[ $STRIP == "true" ]]; then
    if [[ $BUILDMODE == "release" ]]; then
      cp android/libs/armeabi/liblclapp.so liblclapp.unstripped.arm.so
      cp android/libs/x86/liblclapp.so liblclapp.unstripped.x86.so
    else
      cp android/libs/armeabi/liblclapp.so liblclapp.unstripped.debug.arm.so
      cp android/libs/x86/liblclapp.so liblclapp.unstripped.debug.x86.so
    fi
    arm-linux-strip --strip-all android/libs/armeabi/liblclapp.so
    strip --strip-all android/libs/x86/liblclapp.so
  fi

  ./manage.sh build-java $BUILDMODE
;;

build-java)
  BUILDMODE="$2"
  if [[ -z "$BUILDMODE" ]]; then BUILDMODE=debug; fi
  case "$BUILDMODE" in
  debug) GRADLEMODE=assembleDebug;;
  release) ???;;
  esac
  
  ./gradlew $GRADLEMODE || (echo "FAILED!"; exit)
  
  cd android
  $SDK_HOME/adb uninstall de.benibela.videlibri || (echo "FAILED!"; exit)
  $SDK_HOME/adb install build/outputs/apk/android-$BUILDMODE.apk || (echo "FAILED!"; exit)
;;

install)
  if [[ $2 == "release" ]]; then BUILDMODE=release
  else BUILDMODE=debug;  fi

  cd android
  $SDK_HOME/adb uninstall de.benibela.videlibri || (echo "FAILED!"; exit)
  $SDK_HOME/adb install build/outputs/apk/android-$BUILDMODE.apk || (echo "FAILED!"; exit)
  
;;

clean)
  rm android/libs/armeabi/liblclapp.so; 
  rm android/libs/x86/liblclapp.so; 
  ./gradlew clean
;;

clean-java)
  ./gradlew clean
;;

brokenServers)
   export PASSWORD=password
   export KEYSTORE=android/res/raw/keystore.bks 
   export SERVERLIST=../data/libraries/brokenServers.list
   export RESSERVERLIST=android/res/values/brokenServers.xml
   export TMPFILE=__vl__certificate.pem
   export KEYTOOL=keytool
   #/usr/lib/jvm/java-6-sun/jre/bin/keytool
   export BOUNCYCASTLE=/usr/share/java/bcprov-1.46.jar
   FINGERPRINTFILE=keystore.bks.fingerprints
   TEMPKEYSTORE=__vl__keystore.bks 


   echo '<?xml version="1.0" encoding="utf-8"?>' > $RESSERVERLIST
   echo "<resources>" >> $RESSERVERLIST
   echo '<string-array name="broken_servers">' >> $RESSERVERLIST
   
   rm $KEYSTORE
   rm $FINGERPRINTFILE
   i=0
   while read server; do
     if [[ -n "$server" ]]; then      
       echo
       echo
       echo =====================================================================
       echo ==========================$server==========================
       echo =====================================================================
       echo "<item>CN=$server</item>" >> $RESSERVERLIST
       echo something | openssl s_client -connect $server:443 > $TMPFILE
       if grep -qv "BEGIN CERTIFICATE" $TMPFILE; then 
         #openssl fails to negotiate protocol version for some servers. only tls1 prints certificate data
         echo something | openssl s_client -connect $server:443 -tls1 > $TMPFILE 
       fi
       
       cp $KEYSTORE $TEMPKEYSTORE
       yes | $KEYTOOL       -import       -v       -trustcacerts       -alias $i       -file <(openssl x509 -in $TMPFILE)       -keystore $KEYSTORE       -storetype BKS       -provider org.bouncycastle.jce.provider.BouncyCastleProvider       -providerpath $BOUNCYCASTLE       -storepass $PASSWORD
       
       echo -en "$server\t" >> $FINGERPRINTFILE
       if diff -q $KEYSTORE $TEMPKEYSTORE; then
         echo FAIL >> $FINGERPRINTFILE
       else
         keytool -list -keystore $KEYSTORE -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS -storepass $PASSWORD  | grep -E "trusted|fingerprint" | while read line1; do read line2; echo "$line1: $line2"; done | sort -n | tail -1  | sed -Ee 's/,[^:]+,//' >> $FINGERPRINTFILE
       fi
       
       ((i=i+1))
     fi
   done <  $SERVERLIST

   echo '</string-array>' >> $RESSERVERLIST
   echo "</resources>" >> $RESSERVERLIST
   
   echo 
   echo
   echo
   
    $KEYTOOL -list -keystore $KEYSTORE -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS -storepass $PASSWORD
   
   echo 
   echo
   echo
   echo 
   echo
   echo
   rm $TEMPKEYSTORE

   #keytool -list -keystore $KEYSTORE -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS -storepass $PASSWORD  | grep -E "trusted|fingerprint" | while read line1; do read line2; echo "$line1: $line2"; done | sort -n | paste ../data/libraries/brokenServers.list - | sed -Ee 's/, *[A-Za-z]{3} *[0-9]+, *[0-9]{4},//' | tee keystore.bks.fingerprints
   cat $FINGERPRINTFILE

   
   rm $TMPFILE
;;
  
esac


