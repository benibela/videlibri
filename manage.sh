#/bin/bash


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $DIR/../../../manageUtils.sh

sfProject videlibri

VIDELIBRIBASE=$HGROOT/programs/internet/VideLibri

function pushhg(){
  PUBLICHG=$HGROOT/../videlibrixidelpublichg
  if [[ -e $PUBLICHG ]]; then
    syncHg $VIDELIBRIBASE/_hg.filemap $HGROOT $PUBLICHG
    $PUBLICHG/makebadge.sh
  fi
}


INTVERSION=`grep versionNumber:integer $VIDELIBRIBASE/applicationconfig.pas | grep -oE "[0-9]+" `;
VERSION=`echo "scale=3;$INTVERSION/1000" | bc`;
VERSION=`export LC_NUMERIC=C; printf "%1.3f\n" $VERSION`;

case "$1" in
hg)  
  pushhg
  cd $VIDELIBRIBASE/_meta/version/
  webUpload version.xml changelog.xml /updates/
;;

mirror)
  (cd _meta/tests; ./unittests.sh) || exit;
  pushhg
  SF_PROJECT= 
  mirroredProject videlibri
  syncHg $VIDELIBRIBASE/_hg.standalone.filemap
;;

linux32)
    rm videlibri
		lazCompileLinux32 bookWatch
    cp videlibri videlibri.unstripped.linux32
    strip --strip-all videlibri

		./_meta/build.deb.sh 
    
		
		fileUpload videlibri_$VERSION-1_i386.deb "/VideLibri/VideLibri\ $VERSION/"
		webUpload  videlibri_$VERSION-1_i386.deb /updates/videlibri-linux32.deb
		;;

linux64)
    rm videlibri
		lazCompileLinux64 bookWatch
    cp videlibri videlibri.unstripped.linux64
    strip --strip-all videlibri

		./_meta/build.deb.sh 
    
		
		fileUpload videlibri_$VERSION-1_amd64.deb "/VideLibri/VideLibri\ $VERSION/"
		webUpload  videlibri_$VERSION-1_amd64.deb /updates/videlibri-linux64.deb
		;;
		
win32)
		rm videlibri.exe
		lazCompileWin32 bookWatch
    cp $VIDELIBRIBASE/videlibri.exe $VIDELIBRIBASE/videlibri.unstripped.exe
		strip --strip-all $VIDELIBRIBASE/videlibri.exe
		cd $VIDELIBRIBASE  #innosetup does not understand absolute linux paths
		sed _meta/installer/videlibri.iss -i -Ee "s/^( *AppVer(Name|sion)=[^0-9]*)[^\r]*(\r)?\$/\1$VERSION\3/"
		wine ~/.wine/drive_c/programs/programming/InnoSetup/Compil32.exe _meta/installer/videlibri.iss

		cd $VIDELIBRIBASE/Output
		fileUpload videlibri-setup.exe "/VideLibri/VideLibri\ $VERSION/"
		webUpload  videlibri-setup.exe /updates/
		;;

android) 
    cd android
    ANDROIDVERSION=$(xidel android/AndroidManifest.xml -e "/manifest/@*:versionCode")
    if [[ "$ANDROIDVERSION" != "$INTVERSION" ]]; then 
      echo Android version mismatch $ANDROIDVERSION != $INTVERSION; 
      sed -Ee '0,/RE/s/android:versionCode *= *["][0-9]+["]/android:versionCode="'"$INTVERSION"'"/' -i android/AndroidManifest.xml
      sed -Ee '0,/RE/s/android:versionName *= *["][0-9.]+["]/android:versionName="'"$VERSION"'"/' -i android/AndroidManifest.xml
      head android/AndroidManifest.xml
      #read; 
    fi
    ANDROIDVERSION=$(xidel android/AndroidManifest.xml -e "/manifest/@*:versionCode")
    if [[ "$ANDROIDVERSION" != "$INTVERSION" ]]; then echo Android version mismatch "$ANDROIDVERSION" != "$INTVERSION"; read; vim android/AndroidManifest.xml; fi
    ANDROIDVERSION=$(xidel android/AndroidManifest.xml -e "/manifest/@*:versionName")
    if [[ "$ANDROIDVERSION" != "$VERSION" ]]; then echo Android version mismatch "$ANDROIDVERSION" != "$VERSION" ; read; vim android/AndroidManifest.xml; fi
    ANDROIDVERSION=$(xidel android/AndroidManifest.xml -e "/manifest/@*:versionCode")
    if [[ "$ANDROIDVERSION" != "$INTVERSION" ]]; then echo Android version mismatch; read; exit; fi
    ANDROIDVERSION=$(xidel android/AndroidManifest.xml -e "/manifest/@*:versionName")
    if [[ "$ANDROIDVERSION" != "$VERSION" ]]; then echo Android version mismatch; read; exit; fi
    
    ./manage.sh clean
    ./manage.sh build release
    ./manage.sh symbols
    cp android/build/outputs/apk/release/android-release.apk /tmp/videlibri_$VERSION-release.apk
    cd /tmp
		fileUpload videlibri_$VERSION-release.apk "/VideLibri/VideLibri\ $VERSION/"
    (cd ~/opt/fdroidmy/; ./publish.sh)
;;

downloadTable) 
  URL=http://sourceforge.net/projects/videlibri/files/VideLibri/VideLibri%20$VERSION/
  PROJNAME=VideLibri
  ~/hg/programs/internet/xidel/xidel $URL --extract-exclude=pname,url --dot-notation=on  --extract-kind=xquery  \
     -e "declare variable \$pname := '$PROJNAME'; ()" \
     -e "declare variable \$url := '$URL'; ()" \
     -e 'declare variable $lang := 2; 
         declare function verboseName($n){ concat ( 
           if (contains($n, "win") or contains($n, ".exe")) then "Windows: " 
           else if (contains($n, "linux")) then "Universal Linux: " 
           else if (contains($n, ".deb")) then "Debian/Ubuntu: " 
           else if (contains($n, ".apk")) then "Android: " 
           else if (contains($n, "src")) then ("Source:", "Quellcode:")[$lang] 
           else "", 
           if (contains($n, "32") or contains($n, "386")) then "32 Bit" 
           else if (contains($n, "64"))then "64 Bit" 
           else ""  )   
         };
         declare function T($s){ $s[$lang] };
         <div>
         { T((x"The following {$pname} downloads are available on the ",x"Es gibt die folgenden {$pname}-Downloads auf der ")) }
         <a href="{$url}">{T(("sourceforge download page", "SourceForge-Downloadseite"))}</a>:
         <br/><br/>
         <table class="downloadTable">
         <tr><th>{("Operating System", "Betriebsystem")[$lang]}</th><th>{("Filename", "Dateiname")[$lang]}</th><th>{("Size", "Dateigröße")[$lang]}</th></tr>
         { for $link in match(<TABLE id="files_list"><t:loop><TR class="file">
           <TH>{{link := {{"verboseName": verboseName(.),
                           "name": data(.),
                           "href": if (a/@href) then a/@href else $url || . || "/download" 
                         }} }}</TH>
           <td/><td>{{link.size := .}}</td></TR></t:loop></TABLE>, /).link 
           order by $link.verboseName descending 
           return <tr><td>{$link.verboseName}</td><td>
           <a href="{$link.href}">{$link.name}</a></td><td>{$link.size/text()}</td></tr>}</table></div>'     --printed-node-format xml > $VIDELIBRIBASE/_meta/sfsite/downloadTable.html
  cat $VIDELIBRIBASE/_meta/sfsite/downloadTable.html
  (cd _meta/sfsite; rm _publish/downloads.html; make )

;;

  libraries.list)
     export LANG=C.UTF-8
     LIBS=$(ls data/libraries/*.xml | grep -oE "[^_]+_[^_]+.xml" | sort | xargs -I{} find data/libraries/ -name "*{}" -maxdepth 1 | sort | uniq)
     grep -oE "[^/]*$" <<<"$LIBS" | sed -e 's/\.xml$//' > data/libraries/libraries.list
    ;;
    
    
  create-common-interface)
    xidel interface.xml --module interface-generator.xqm -e 'ig:pascal-make(/)' | tee commoninterface.pas
    xidel interface.xml --module interface-generator.xqm -e 'ig:kotlin-make(/)' | tee android/android/src/de/benibela/videlibri/jni/CommonInterface.kt
    xidel interface.xml --module interface-generator.xqm -e 'ig:code-to-add-manually(/)' 
#    xidel interface.xml --module interface-generator.xqm -e 'ig:pascal-kotlin-make-bridge(/)'

    
    #android/android/src/de/benibela/videlibri/PreferenceSeekBar.kt
    ;;
  
	changelog)
    sed -e 's/<stable  *value="[0-9]*"/<stable value="'$INTVERSION'"/'  -i $VIDELIBRIBASE/_meta/version/version.xml;
    CHANGELOGVERSION=$(grep version= _meta/version/changelog.xml | head -2 | tail -1 | grep -oE '[0-9]+' | head -1)
    if [[ "$CHANGELOGVERSION" != "$INTVERSION" ]]; then 
      sed '/<download/d' -i _meta/version/changelog.xml 
      sed '/changelog *prog/a\
      '"<build version=\"$INTVERSION\" date=\"$(date +%Y-%m-%d)\">"'\
        <download url="http://videlibri.sourceforge.net/updates/videlibri-setup.exe" platform="WINDOWS" execute="&quot;$DOWNLOAD&quot;  /SP- /noicons &quot;/dir=$OLDPATH&quot; " restart="true"/>\
        <download url="http://videlibri.sourceforge.net/updates/videlibri-linux32.deb" platform="LINUX32" execute=""/>\
        <download url="http://videlibri.sourceforge.net/updates/videlibri-linux64.deb" platform="LINUX64" execute=""/>\
        <change></change>\
        <change></change>\
        <fix></fix>\
        <fix></fix>\
        <fix></fix>\
        <add></add>\
        <add></add>\
        <add></add>\
     </build>' -i  _meta/version/changelog.xml
    fi

    
    function xmledit(){
      fn=$1
      vim $fn
      until xmllint  $fn > /dev/null; do
        echo ------------Invalid XML---------------
        read
        vim $fn
        echo editing done
      done
    }
		xmledit $VIDELIBRIBASE/_meta/version/version.xml
		xmledit $VIDELIBRIBASE/_meta/version/changelog.xml
		./manage.sh web
		;;
		
	web)
		cd $VIDELIBRIBASE/_meta/sfsite
		./maketable.sh
		make
		cd ../version/
		rsync -av -e ssh version.xml changelog.xml "benibela,videlibri@web.sourceforge.net:/home/project-web/videlibri/htdocs/updates"
		;;
		
	script)
	  $VIDELIBRIBASE/_meta/sfsite
	  cp /home/firefox/.mozilla/firefox/bpg2hcdr.default/gm_scripts/Webscraper/Webscraper.user.js script.user.js
	  rsync -av -e ssh script.user.js "benibela,videlibri@web.sourceforge.net:/home/project-web/videlibri/htdocs/"
	  ;;
  

  help)
    wine "/home/theo/.wine/drive_c/Programme/HTML Help Workshop/hhc.exe" _meta/help/videlibri.hhp 
    mv _meta/videlibri.chm data/
    cd _meta/help
    webUpload *.css *.gif *.html /help/
  ;;
  
  colorize-icons)
    function greeny { convert "$1" -modulate 100,100,133  "$2";  }
    function reddy { convert "$1" -modulate 100,100,68  "$2";  }

    greeny data/yellow48.png data/green48.png 
    reddy data/yellow48.png data/red48.png     
    greeny android/android/res/drawable-hdpi/icon.png android/android/res/drawable-hdpi/icong.png
    reddy android/android/res/drawable-hdpi/icon.png android/android/res/drawable-hdpi/iconr.png
    greeny android/android/res/drawable-xhdpi/icon.png android/android/res/drawable-xhdpi/icong.png
    reddy android/android/res/drawable-xhdpi/icon.png android/android/res/drawable-xhdpi/iconr.png
    
  ;;
  
defaults)
  setFileDefaults  VideLibri/VideLibri%20$VERSION/
;;

src)
  pushhg
  SRCDIR=/tmp/videlibri-$VERSION-src
  rm -R $SRCDIR
  cp -r $PUBLICHG $SRCDIR
  cd /tmp
  rm -Rvf $SRCDIR/programs/internet/xidel $SRCDIR/programs/internet/sourceforgeresponder/
  cp $VIDELIBRIBASE/applicationconfig.pas $SRCDIR/programs/internet/VideLibri/
  cp $VIDELIBRIBASE/_meta/version/* $SRCDIR/programs/internet/VideLibri/_meta/version/
  cp $VIDELIBRIBASE/android/android/AndroidManifest.xml $SRCDIR/programs/internet/VideLibri/android/android/
  tar -cvzf /tmp/videlibri-$VERSION.src.tar.gz --exclude=.hg videlibri-$VERSION-src
  fileUpload videlibri-$VERSION.src.tar.gz "/VideLibri/VideLibri\ $VERSION/"
;;

	release)
	  ./manage.sh changelog		
	  rm ~/hg/components/pascal/lib/*/*.o ~/hg/components/pascal/lib/*/*.ppu ~/hg/components/pascal/*.ppu ~/hg/components/pascal/data/*.ppu  lib/*/*.ppu
	  
		./manage.sh linux64
		./manage.sh win32
		./manage.sh linux32
		./manage.sh android
	  ./manage.sh src
	  echo Sleeping because SF is slow to show the downloads
	  sleep 240
	  ./manage.sh downloadTable
	  echo do not forget to close the commit window
		thg commit
	  ./manage.sh defaults
    hg tag "VIDELIBRI $VERSION"
	  ./manage.sh mirror
		;;
 
 

esac

