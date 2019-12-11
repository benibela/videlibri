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

FPC_ARM=ppcrossarm
FPC_386=ppcross386
FPC_ARM64=ppcrossa64
if hash ppcrossx64 2>/dev/null; then FPC_X64=ppcrossx64
else FPC_X64=ppcx64
fi
LAZBUILD=lazbuild

hash $LAZBUILD || { echo >&2 "Failed to find Lazarus build command. Install Lazarus."; exit 1; }

hash $ADB || { echo >&2 "Failed to find adb. Install Android SDK."; exit 1; }

function nativeBuild(){
  flag=$1
  abi=$2
  strip=$3
  compiler=$4
  cpu=$5
  path=android/libs/$abi/

  mkdir -p $path
  if ! $flag ; then rm $path/liblclapp.so; rm -r $path; fi;
  if $flag ; then
    which $compiler > /dev/null || { echo >&2 "Failed to find fpc $compiler. Install FreePascal cross compiler."; exit 1; }
    if [[ ! -f $path/liblclapp.so ]]; then FORCE=-B; fi
    if $LAZBUILD $FORCE --os=android --ws=nogui --compiler="$(which $compiler)" --cpu=$cpu videlibriandroid.lpi; then echo; else echo "FAILED!"; exit 1; fi
  fi

  STRIP=true
  if [[ $BUILDMODE == "release" ]] || [[ $STRIP == "true" ]]; then
    cp $path/liblclapp.so liblclapp.unstripped.$BUILDMODE.$abi.so
    $strip --strip-all $path/liblclapp.so
  fi
}

case "$1" in
build)
  BUILDARM=false
  BUILDX86=false
  BUILDARM64=false
  BUILDX64=false
  case "$2" in
    release) 
      BUILDMODE=release
      BUILDARM=true
      BUILDX86=true
      BUILDARM64=true
      BUILDX64=true
    ;;
    arm) 
      BUILDMODE=debug
      BUILDARM=true
    ;;
    x86) 
      BUILDMODE=debug
      BUILDX86=true
    ;;
    arm64) 
      BUILDMODE=debug
      BUILDARM64=true
    ;;
    x64) 
      BUILDMODE=debug
      BUILDX64=true
    ;;
    *)
      BUILDMODE=debug
      BUILDARM=true
      BUILDX86=true
      BUILDARM64=true
      BUILDX64=true
    ;;
  esac

  nativeBuild $BUILDARM     armeabi    arm-linux-androideabi-strip   $FPC_ARM    arm
  nativeBuild $BUILDX86     x86        i686-linux-android-strip      $FPC_386    i386
  nativeBuild $BUILDARM64   arm64-v8a  aarch64-linux-android-strip   $FPC_ARM64  aarch64
  nativeBuild $BUILDX64     x86_64     strip                         $FPC_X64    x86_64

  ./manage.sh build-gradle $BUILDMODE
;;

build-gradle|build-java)
  BUILDMODE="$2"
  if [[ -z "$BUILDMODE" ]]; then BUILDMODE=debug; fi
  case "$BUILDMODE" in
  debug) GRADLEMODE=assembleDebug;;
  release) GRADLEMODE=assembleRelease;;
  esac
  
  ./gradlew $GRADLEMODE || { echo "FAILED!"; exit 1; }
  
  if readelf  -l android/libs/*/*.so  | grep RELRO; then
    echo -------------------------------------- 
    echo RELRO is enabled
    echo VideLibri cannot start with RELRO
    echo pass -k-znorelro  to fpc
    echo --------------------------------------
    exit 1
  fi

  if [[ "$TRAVIS" != true ]]; then
    cd android
    #$ADB uninstall de.benibela.videlibri || { echo "FAILED!"; exit 1;}
    $ADB install -r build/outputs/apk/$BUILDMODE/android-$BUILDMODE.apk || { echo "FAILED!"; exit 1;}
  fi
;;

install)
  case "$2" in
    release) BUILDMODE=release;;
    *) BUILDMODE=debug;;
  esac

  cd android
  #$ADB uninstall de.benibela.videlibri || { echo "FAILED!"; exit 1;}
  $ADB install -r build/outputs/apk/$BUILDMODE/android-$BUILDMODE.apk || { echo "FAILED!"; exit 1;}
  
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
   for bouncy in /usr/share/java/bcprov*.jar; do
     export BOUNCYCASTLE=$bouncy
     break;
   done;
   FINGERPRINTFILE=keystore.bks.fingerprints
   FINGERPRINTFILEOLD=keystoreold.bks.fingerprints
   TEMPKEYSTORE=__vl__keystore.bks 



   echo '<?xml version="1.0" encoding="utf-8"?>' > $RESSERVERLIST
   echo "<resources>" >> $RESSERVERLIST
   echo '<string-array name="broken_servers"  translatable="false">' >> $RESSERVERLIST
   
   rm $KEYSTORE $KEYSTOREOLD $FINGERPRINTFILE $FINGERPRINTFILEOLD
   i=0
   (cat $SERVERLIST; ls certs/*.cer certs/intermediate/*.cer) |  while read server; do
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
         LANG=C keytool -list -v -alias $i -keystore $KEYSTORE -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS -storepass $PASSWORD  | grep SHA256: >> $FINGERPRINTFILE
         
         
         echo -en "$server\t" >> $FINGERPRINTFILEOLD
         yes | $KEYTOOL       -import       -v       -trustcacerts       -alias $i       -file <(openssl x509 -in $TMPFILE)       -keystore $KEYSTOREOLD       -storetype BKS-V1       -provider org.bouncycastle.jce.provider.BouncyCastleProvider       -providerpath $BOUNCYCASTLE       -storepass $PASSWORD
         LANG=C keytool -list -v -alias $i -keystore $KEYSTOREOLD -provider org.bouncycastle.jce.provider.BouncyCastleProvider -providerpath $BOUNCYCASTLE -storetype BKS-V1 -storepass $PASSWORD | grep SHA256: >> $FINGERPRINTFILEOLD
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

setupbinutils)
  targetdir=$2
  if [[ -z "$targetdir" ]]; then targetdir=~/bin; fi
  function singleplatform(){
    platform=$1
    path=$ANDROID_HOME/ndk-bundle/toolchains/$platform*/prebuilt/linux-x86_64/bin/
    ln -srv $path/*-ld $targetdir
    ln -srv $path/*-ld.bfd $targetdir
    ln -srv $path/*-as $targetdir
    ln -srv $path/*-strip $targetdir
    ln -srv $path/*-addr2line $targetdir
  }
  singleplatform arm
  singleplatform x86-
  singleplatform aarch64
  singleplatform x86_64
;;
  
setupfpccrosscompile)
  set -e
  prefix=/usr/local/
  copyprefix=/usr
  localfpc=$(which ppcx64)
  echo FPC Cross Compiling: Using old FPC: $localfpc
  echo Start FPC Cross Compiling: arm android
  make crossinstall OS_TARGET=android CPU_TARGET=arm BINUTILSPREFIX=arm-linux-androideabi- INSTALL_PREFIX=$prefix PP=$localfpc
  echo Start FPC Cross Compiling: i386 android
  make crossinstall OS_TARGET=android CPU_TARGET=i386 BINUTILSPREFIX=i686-linux-android- INSTALL_PREFIX=$prefix PP=$localfpc
  echo Start FPC Cross Compiling: aarch64 android
  make crossinstall OS_TARGET=android CPU_TARGET=aarch64 BINUTILSPREFIX=aarch64-linux-android- INSTALL_PREFIX=$prefix PP=$localfpc
  echo Start FPC Cross Compiling: x86_64 android
  make crossinstall OS_TARGET=android CPU_TARGET=x86_64 BINUTILSPREFIX=x86_64-linux-android- INSTALL_PREFIX=$prefix PP=$localfpc
  for ppc in $prefix/lib/fpc/3.{1..9}.*/ppc*; do ln -sfv $ppc $copyprefix/bin/; done  
;;

setupfpccrosscfg)
  cat <<< '
  -Fu/usr/local/lib/fpc/$fpcversion/units/$fpctarget
  -Fu/usr/local/lib/fpc/$fpcversion/units/$fpctarget/*
  -Fu/usr/local/lib/fpc/$fpcversion/units/$fpctarget/rtl  
  
  #ifdef android
  '

  function singleplatform(){
    platformdefine=$1; shift
    platformbinutils=$1; shift
    echo "#IFDEF $platformdefine"
    echo -XP$platformbinutils
    echo -k-znorelro
    for l in "$@"; do 
      echo -Fl$l 
    done;
    echo "#ENDIF"
  }

  androidpath=$ANDROID_HOME/ndk-bundle/platforms/android-24
  
  singleplatform CPUARM arm-linux-androideabi- $androidpath/arch-arm/usr/lib /usr/lib/gcc/arm-linux-androideabi/* /usr/arm-linux-androideabi/lib
  singleplatform CPUI386    i686-linux-android-    $androidpath/arch-x86/usr/lib/ /usr/lib/gcc/i686-linux-gnu/* /usr/lib/gcc/i586-mingw32msvc/*
  singleplatform CPUAARCH64 aarch64-linux-android- $androidpath/arch-arm64/usr/lib/
  singleplatform CPUX64     x86_64-linux-android-  $androidpath/arch-x86_64/usr/lib64/
  
  
  echo "#endif"
;;
  
esac


