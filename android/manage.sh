#!/bin/bash
if [ -z "$ANDROID_HOME" ]; then
  if [ -d ~/opt/android/sdk/platform-tools/ ]; then ANDROID_HOME=~/opt/android/sdk/
  else echo Failed to find Android SDK. Set ANDROID_HOME variable; exit
  fi
  if [ -d ~/opt/android/studio/jre ]; then JAVA_HOME=~/opt/android/studio/jre; fi
fi
if [ -z "$JAVA_HOME" ]; then
  if [ -d ~/opt/android/studio/jre ]; then JAVA_HOME=~/opt/android/studio/jre 
  else echo Failed to find java. Set JAVA_HOME variable; exit 2
  fi
fi

ADB=$ANDROID_HOME/platform-tools/adb

export ANDROID_HOME
export JAVA_HOME
export JRE_HOME="$JAVA_HOME"
export JDK_HOME="$JAVA_HOME"
export JAVA_ROOT="$JAVA_HOME"
export JAVA_BINDIR="$JAVA_HOME/bin"


if [ -z "$FPC_DIRECTORY" ]; then 
if [ -d /usr/local/lib/fpc/3.1.1 ]; then
FPC_DIRECTORY=/usr/local/lib/fpc/3.1.1
else echo Failed to find fpc 3.1.1. Install FreePascal; exit 2
fi
fi
export FPC_DIRECTORY

FPC_ARM=$FPC_DIRECTORY/ppcrossarm
if [ ! -f $FPC_ARM ]; then echo Failed to find fpc arm target. Install FreePascal cross compiler; exit 2; fi
FPC_386=$FPC_DIRECTORY/ppcross386
if [ ! -f $FPC_386 ]; then echo Failed to find fpc 386 target. Install FreePascal cross compiler; exit 2; fi

LAZBUILD=lazbuild
which $LAZBUILD 2>/dev/null >/dev/null || (
  echo Failed to find lazbuild. Install Lazarus.
)

case "$1" in
build)
  mkdir -p android/libs/armeabi/ android/libs/x86
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
    if $LAZBUILD $FORCE --os=android --ws=nogui --compiler=$FPC_ARM --cpu=arm videlibriandroid.lpi; then echo; else echo "FAILED!"; exit 1; fi
  fi

  if $BUILDX86; then
    FORCE=""
    if [[ ! -f android/libs/x86/liblclapp.so ]]; then FORCE=-B; fi
    if $LAZBUILD $FORCE --compiler=$FPC_386 --os=android --ws=nogui --cpu=i386 videlibriandroid.lpi; then echo; else echo "FAILED!"; exit 1; fi
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
  release) GRADLEMODE=assembleRelease;;
  esac
  
  ./gradlew $GRADLEMODE || { echo "FAILED!"; exit 1; }
  
  if [[ "$TRAVIS" != true ]]; then
    cd android
    #$ADB uninstall de.benibela.videlibri || { echo "FAILED!"; exit 1;}
    $ADB install -r build/outputs/apk/android-$BUILDMODE.apk || { echo "FAILED!"; exit 1;}
  fi
;;

install)
  if [[ $2 == "release" ]]; then BUILDMODE=release
  else BUILDMODE=debug;  fi

  cd android
  #$ADB uninstall de.benibela.videlibri || { echo "FAILED!"; exit 1;}
  $ADB install -r build/outputs/apk/android-$BUILDMODE.apk || { echo "FAILED!"; exit 1;}
  
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
   export PASSWORD=psswrd
   export KEYSTORE=android/res/raw/keystore.bks 
   export KEYSTOREOLD=android/res/raw/keystoreold.bks 
   export SERVERLIST=../data/libraries/brokenServers.list
   export RESSERVERLIST=android/res/values/brokenServers.xml
   export TMPFILE=__vl__certificate.pem
   export KEYTOOL=keytool
   export LANG=C.utf8
   export LC_ALL=C.utf8
   #/usr/lib/jvm/java-6-sun/jre/bin/keytool
   export BOUNCYCASTLE=/usr/share/java/bcprov-1.58.jar
   FINGERPRINTFILE=keystore.bks.fingerprints
   FINGERPRINTFILEOLD=keystoreold.bks.fingerprints
   TEMPKEYSTORE=__vl__keystore.bks 



   echo '<?xml version="1.0" encoding="utf-8"?>' > $RESSERVERLIST
   echo "<resources>" >> $RESSERVERLIST
   echo '<string-array name="broken_servers">' >> $RESSERVERLIST
   
   rm $KEYSTORE $KEYSTOREOLD $FINGERPRINTFILE $FINGERPRINTFILEOLD
   i=0
   (cat $SERVERLIST; ls certs/*.cer) |  while read server; do
     if [[ -n "$server" ]]; then      
       echo
       echo
       echo =====================================================================
       echo ==========================$server==========================
       echo =====================================================================
       if [[ "$server" =~ certs.*cer ]]; then
         cp $server $TMPFILE
        else
         echo "<item>CN=$server</item>" >> $RESSERVERLIST
         echo something | openssl s_client -servername $server -connect $server:443 > $TMPFILE
         if ! grep -q "BEGIN CERTIFICATE" $TMPFILE; then 
           #openssl fails to negotiate protocol version for some servers. only tls1 prints certificate data
           echo something | openssl s_client -servername $server -connect $server:443 -tls1 > $TMPFILE;
           if ! grep -q "BEGIN CERTIFICATE" $TMPFILE; then  
             cp certs/$server $TMPFILE
           fi
         fi
       fi
       
       cp $KEYSTORE $TEMPKEYSTORE
       yes | $KEYTOOL       -import       -v       -trustcacerts       -alias $i       -file <(openssl x509 -in $TMPFILE)       -keystore $KEYSTORE       -storetype BKS       -provider org.bouncycastle.jce.provider.BouncyCastleProvider       -providerpath $BOUNCYCASTLE       -storepass $PASSWORD
       
       echo -en "$server\t" >> $FINGERPRINTFILE
       if diff -q $KEYSTORE $TEMPKEYSTORE; then
         echo FAIL >> $FINGERPRINTFILE
       else
         keytool -list -keystore $KEYSTORE -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS -storepass $PASSWORD  | grep -E "trusted|fingerprint" | while read line1; do read line2; echo "$line1: $line2"; done | sort -n | tail -1  | sed -Ee 's/,[^:]+,//' >> $FINGERPRINTFILE
         
         
         echo -en "$server\t" >> $FINGERPRINTFILEOLD
         yes | $KEYTOOL       -import       -v       -trustcacerts       -alias $i       -file <(openssl x509 -in $TMPFILE)       -keystore $KEYSTOREOLD       -storetype BKS-V1       -provider org.bouncycastle.jce.provider.BouncyCastleProvider       -providerpath $BOUNCYCASTLE       -storepass $PASSWORD
         keytool -list -keystore $KEYSTOREOLD -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS-V1 -storepass $PASSWORD  | grep -E "trusted|fingerprint" | while read line1; do read line2; echo "$line1: $line2"; done | sort -n | tail -1  | sed -Ee 's/,[^:]+,//' >> $FINGERPRINTFILEOLD
       fi
       
       ((i=i+1))
     fi
   done 

   echo '</string-array>' >> $RESSERVERLIST
   echo "</resources>" >> $RESSERVERLIST
   
   
    $KEYTOOL -list -keystore $KEYSTORE -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS -storepass $PASSWORD
    $KEYTOOL -list -keystore $KEYSTOREOLD -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS-V1 -storepass $PASSWORD
   
   echo 
   echo
   echo
   echo 
   echo
   echo
   rm $TEMPKEYSTORE

   #keytool -list -keystore $KEYSTORE -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS -storepass $PASSWORD  | grep -E "trusted|fingerprint" | while read line1; do read line2; echo "$line1: $line2"; done | sort -n | paste ../data/libraries/brokenServers.list - | sed -Ee 's/, *[A-Za-z]{3} *[0-9]+, *[0-9]{4},//' | tee keystore.bks.fingerprints
   cat $FINGERPRINTFILE
   if ! diff $FINGERPRINTFILE $FINGERPRINTFILEOLD; then 
     echo OLD NEW FINGERPRINT MISMATCH
   fi
   

   
   rm $TMPFILE
;;
  
esac


