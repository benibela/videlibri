#/bin/bash


source ../../../manageUtils.sh

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

linux32)
		find ~/hg -name "*.ppu" | grep -v /lib/ | xargs rm
		lazCompileLinux32 bookWatch
    strip --strip-all videlibri

		checkinstall --pkgarch=i386 --install=no --pkgname=videlibri --default  --pkgversion=$VERSION --nodoc --maintainer="Benito van der Zander \<benito@benibela.de\>" --requires="libgtk2.0-0" bash _meta/install_direct.sh 
		
		fileUpload videlibri_$VERSION-1_i386.deb "/VideLibri/VideLibri\ $VERSION/"
		webUpload  videlibri_$VERSION-1_i386.deb /updates/videlibri-linux32.deb
		;;

linux64)
		find ~/hg -name "*.ppu" | grep -v /lib/ | xargs rm
		lazCompileLinux64 bookWatch
    strip --strip-all videlibri

		checkinstall --install=no --pkgname=VideLibri --default  --pkgversion=$VERSION --nodoc --maintainer="Benito van der Zander \<benito@benibela.de\>" --requires="libgtk2.0-0" bash _meta/install_direct.sh 
		
		fileUpload videlibri_$VERSION-1_amd64.deb "/VideLibri/VideLibri\ $VERSION/"
		webUpload  videlibri_$VERSION-1_amd64.deb /updates/videlibri-linux64.deb
		;;
		
win32)
		find . -name "*.ppu" | grep -v /lib/ | xargs rm
		lazCompileWin32 bookWatch
		strip --strip-all $VIDELIBRIBASE/videlibri.exe
		cd $VIDELIBRIBASE  #innosetup does not understand absolute linux paths
		wine ~/.wine/drive_c/programs/programming/InnoSetup/Compil32.exe _meta/installer/videlibri.iss

		cd $VIDELIBRIBASE/Output
		fileUpload videlibri-setup.exe "/VideLibri/VideLibri\ $VERSION/"
		webUpload  videlibri-setup.exe /updates/
		;;

android) 
    cd android
    ANDROIDVERSION=$(xidel android/AndroidManifest.xml -e "/manifest/@*:versionCode")
    if [[ "$ANDROIDVERSION" != "$INTVERSION" ]]; then echo Android version mismatch; read; exit; fi
    ANDROIDVERSION=$(xidel android/AndroidManifest.xml -e "/manifest/@*:versionName")
    if [[ "$ANDROIDVERSION" != "$VERSION" ]]; then echo Android version mismatch; read; exit; fi
    
    ./manage.sh clean
    ./manage.sh build release
    cp android/bin/videlibri-release.apk /tmp/videlibri_$VERSION-release.apk
    cd /tmp
		fileUpload videlibri_$VERSION-release.apk "/VideLibri/VideLibri\ $VERSION/"
;;

downloadTable) 
  URL=http://sourceforge.net/projects/videlibri/files/VideLibri/VideLibri%20$VERSION/
  PROJNAME=VideLibri
  ~/hg/programs/internet/xidel/xidel $URL --extract-exclude=pname --dot-notation=on  --extract-kind=xquery  \
     -e "declare variable \$pname := '$PROJNAME'" \
     -e "declare variable \$url := '$URL'" \
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
         { for $link in match(<TABLE id="files_list"><t:loop><TR class="file"><TH><A class="name">{{link := object(), link.verboseName := verboseName(.), link.a := .}}</A></TH><td/><td>{{link.size := .}}</td></TR></t:loop></TABLE>, /).link 
           order by $link.verboseName descending 
           return <tr><td>{$link.verboseName}</td><td><a href="{$link.a/@href}">{$link.a/text()}</a></td><td>{$link.size/text()}</td></tr>}</table></div>'     --printed-node-format html > $VIDELIBRIBASE/_meta/sfsite/downloadTable.html
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
       <tr><th>Name der Bücherei</th><th>Suche funktioniert<br><i>(zuletzt getestet)</i></th><th>Ausleihenanzeige funktioniert<br><i>(zuletzt getestet)</i></th><th>Verlängerung funktioniert<br><i>(zuletzt getestet)</i></th><th>Büchereisystem</th></tr>
      </thead>' > $TABLE

    xidel \
      --extract-exclude "city,newcity" \
      --printed-node-format html \
      --xquery 'declare function state($element){
        if (exists($element/@date) and $element/@date != "") then (
          <td>{string($element/@value)} <i>({string($element/@date)})</i></td>
        ) else (
          <td>{string($element/@value)}</td>
        )
      }' \
      --xquery 'declare function row($element, $homepage){
        $element / <tr><td><a href="{$homepage/@value}" rel="nofollow">{ let $name := ($homepage/@name, .//longName/@value)[1] return if (contains($name, "(alpha)")) then substring-before($name, "(alpha") else data($name)}</a></td>
        {state(.//testing-search), state(.//testing-account), state(.//testing-renew)}
        <td>{string(.//template/@value)}</td></tr> 
      }' \
      -e 'city:=("nimbo")' \
      $LIBS  \
      -e 'newcity := replace(replace(replace(replace(filter($url, "/[^_]+_[^_]+_([^/]*)_", 1), "[+]ue", "ü"), "[+]oe", "ö"), "[+]ae", "ä"), "[+]sz", "ß")' \
      --xquery 'if ($newcity  != $city and not(//homepage/@nolist = "true")) then <tr class="city"><td colspan="6"><b>{$newcity}</b></td></tr> else ()'   \
      --xquery 'if (//homepage/@nolist = "true") then () else city := $newcity' \
      --xquery 'if (//homepage/@nolist = "true") then () else //homepage/row(/,.)' \
       >> $TABLE;
      echo '</table>' >> $TABLE
      webUpload $VIDELIBRIBASE/$TABLE /
    ;;
  
	changelog)
		sed -e 's/<stable  *value="[0-9]*"/<stable value="'$INTVERSION'"/'  -i $VIDELIBRIBASE/_meta/version/version.xml;
		vim $VIDELIBRIBASE/_meta/version/version.xml
		vim $VIDELIBRIBASE/_meta/version/changelog.xml
		./manage.sh web
		;;
		
	web)
		cd $VIDELIBRIBASE/_meta/sfsite
		rsync -av -e ssh index.html index_en.html index.php all.css "benibela,videlibri@web.sourceforge.net:/home/project-web/videlibri/htdocs/"
		cd ../version/
		rsync -av -e ssh version.xml changelog.xml "benibela,videlibri@web.sourceforge.net:/home/project-web/videlibri/htdocs/updates"
		;;
		
	webscript)
	  $VIDELIBRIBASE/_meta/sfsite
	  cp /home/firefox/.mozilla/firefox/bpg2hcdr.default/gm_scripts/Webscraper/Webscraper.user.js script.user.js
	  rsync -av -e ssh script.user.js "benibela,videlibri@web.sourceforge.net:/home/project-web/videlibri/htdocs/"
	  ;;
  

  help)
    /migration/migration/p/programming/htmlHelpWorkshop/HHC.EXE _meta/help/videlibri.hhp 
    mv _meta/videlibri.chm data/
    cd _meta/help
    webUpload *.css *.gif *.html /help/
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
	  ./manage.sh downloadTable
		thg commit
	  ./manage.sh defaults
		;;

esac

