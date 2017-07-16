#/bin/bash


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $DIR/../../../manageUtils.sh

sfProject videlibri

VIDELIBRIBASE=$HGROOT/programs/internet/VideLibri

function pushhg(){
  PUBLICHG=$HGROOT/../videlibrixidelpublichg
  syncHg $VIDELIBRIBASE/_hg.filemap $HGROOT $PUBLICHG
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
  pushhg
  SF_PROJECT= 
  mirroredProject videlibri
  syncHg $VIDELIBRIBASE/_hg.standalone.filemap
;;

linux32)
		find ~/hg -name "*.ppu" | grep -v /lib/ | xargs rm
		lazCompileLinux32 bookWatch
    cp videlibri videlibri.unstripped.linux32
    strip --strip-all videlibri

		sudo checkinstall --pkgarch=i386 --install=no --pkgname=videlibri --default  --pkgversion=$VERSION --nodoc --maintainer="Benito van der Zander \<benito@benibela.de\>" --reset-uids=yes --requires="libgtk2.0-0, libssl-dev" bash _meta/install_direct.sh 
		
		fileUpload videlibri_$VERSION-1_i386.deb "/VideLibri/VideLibri\ $VERSION/"
		webUpload  videlibri_$VERSION-1_i386.deb /updates/videlibri-linux32.deb
		;;

linux64)
		find ~/hg -name "*.ppu" | grep -v /lib/ | xargs rm
		lazCompileLinux64 bookWatch
    cp videlibri videlibri.unstripped.linux64
    strip --strip-all videlibri

		sudo checkinstall --install=no --pkgname=VideLibri --default  --pkgversion=$VERSION --nodoc --maintainer="Benito van der Zander \<benito@benibela.de\>" --reset-uids=yes --requires="libgtk2.0-0, libssl-dev" bash _meta/install_direct.sh 
		
		fileUpload videlibri_$VERSION-1_amd64.deb "/VideLibri/VideLibri\ $VERSION/"
		webUpload  videlibri_$VERSION-1_amd64.deb /updates/videlibri-linux64.deb
		;;
		
win32)
		find . -name "*.ppu" | grep -v /lib/ | xargs rm
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
    cp android/build/outputs/apk/android-release.apk /tmp/videlibri_$VERSION-release.apk
    cd /tmp
		fileUpload videlibri_$VERSION-release.apk "/VideLibri/VideLibri\ $VERSION/"
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
           <a href="{$link.href}">{$link.name}</a></td><td>{$link.size/text()}</td></tr>}</table></div>'     --printed-node-format html > $VIDELIBRIBASE/_meta/sfsite/downloadTable.html
  cat $VIDELIBRIBASE/_meta/sfsite/downloadTable.html
  webUpload $VIDELIBRIBASE/_meta/sfsite/downloadTable.html /

;;

  supportTable)
     LIBS=$(ls data/libraries/*.xml | grep -oE "[^_]+_[^_]+.xml" | sort | xargs -I{} find data/libraries/ -name "*{}" -maxdepth 1)
     grep -oE "[^/]*$" <<<"$LIBS" > data/libraries/libraries.list
     
     
     TABLE=_meta/sfsite/supportTable.html 
     echo  '
      <table class="bibsupport">
      <thead>
       <tr><th>Name der Bücherei</th><th class="supportsearch">Suche funktioniert<br><i>(zuletzt getestet)</i></th><th class="supportaccount">Ausleihenanzeige funktioniert<br><i>(zuletzt getestet)</i></th><!--<th>Verlängerung funktioniert<br><i>(zuletzt getestet)</i></th>--><th class="supporttablesystem">Büchereisystem</th></tr>
      </thead>' > $TABLE

    xidel \
      --extract-exclude "city,newcity" \
      --printed-node-format html \
      --xquery 'declare function state($element){
        let $value := normalize-space($element/@value),
            $class := if (starts-with($value, "ja")) then "supported" 
                      else if (starts-with($value, "nein")) then "unsupported" 
                      else "unknown"
        return
        if (exists($element/@date) and $element/@date != "") then (
          <td class="{$class}"><span>{$value} <i>({string($element/@date)})</i></span></td>
        ) else (
          <td class="{$class}"><span>{$value}</span></td>
        )
      };()' \
      --xquery 'declare function row($element, $homepage){
        $element / <tr><td><a href="{$homepage/@value}" rel="nofollow">{ let $name := ($homepage/@name, .//longName/@value)[1] return if (contains($name, "(alpha)")) then substring-before($name, "(alpha") else data($name)}</a>{if (.//table-comment) then (<br/>, <i> { data(.//table-comment/@value) } </i> ) else () } </td>
        {state(.//testing-search), state(.//testing-account)(:, state(.//testing-renew):)}
        <td class="supporttablesystem">{string(.//template/@value) ! (if (matches(., "aleph|ulbdue")) then <a href="#aleph">{.}</a> else <a href="#{.}">{.}</a> )}</td></tr> 
      };()' \
      -e 'city:=("nimbo")' \
      $LIBS  \
      -e 'newcity := replace(replace(replace(replace(extract($url, "/[^_]+_[^_]+_([^/]*)_", 1), "[+]ue", "ü"), "[+]oe", "ö"), "[+]ae", "ä"), "[+]sz", "ß")' \
      --xquery 'if ($newcity  != $city and not(//homepage/@nolist = "true")) then <tr class="city"><td colspan="6"><b>{$newcity}</b></td></tr> else ()'   \
      --xquery 'if (//homepage/@nolist = "true") then () else city := $newcity' \
      --xquery 'if (//homepage/@nolist = "true") then () else //homepage/row(/,.)' \
       >> $TABLE;
      echo '</table>' >> $TABLE
      webUpload $VIDELIBRIBASE/$TABLE /
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
		rsync -av -e ssh index.html index_en.html index.php all.css "benibela,videlibri@web.sourceforge.net:/home/project-web/videlibri/htdocs/"
		cd ../version/
		rsync -av -e ssh version.xml changelog.xml "benibela,videlibri@web.sourceforge.net:/home/project-web/videlibri/htdocs/updates"
		;;
		
	script)
	  $VIDELIBRIBASE/_meta/sfsite
	  cp /home/firefox/.mozilla/firefox/bpg2hcdr.default/gm_scripts/Webscraper/Webscraper.user.js script.user.js
	  rsync -av -e ssh script.user.js "benibela,videlibri@web.sourceforge.net:/home/project-web/videlibri/htdocs/"
	  ;;
  

  help)
    wine /migration/migration/p/programming/htmlHelpWorkshop/HHC.EXE _meta/help/videlibri.hhp 
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
  tar -cvzf /tmp/videlibri-$VERSION.src.tar.gz --exclude=.hg videlibri-$VERSION-src
  fileUpload videlibri-$VERSION.src.tar.gz "/VideLibri/VideLibri\ $VERSION/"
;;

	release)
		./manage.sh linux64
		./manage.sh win32
		./manage.sh linux32
	  ./manage.sh changelog		
		./manage.sh android
	  ./manage.sh src
	  sleep 10 #sf is slow to show the downloads
	  ./manage.sh downloadTable
	  echo do not forget to close the commit window
		thg commit
	  ./manage.sh defaults
    hg tag "VIDELIBRI $VERSION"
		;;
 
 

esac

