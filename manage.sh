
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
		webUpload  videlibri_$VERSION-1_i386.deb /updates/
		;;

linux64)
		find ~/hg -name "*.ppu" | grep -v /lib/ | xargs rm
		lazCompileLinux64 bookWatch
    strip --strip-all videlibri

		checkinstall --install=no --pkgname=VideLibri --default  --pkgversion=$VERSION --nodoc --maintainer="Benito van der Zander \<benito@benibela.de\>" --requires="libgtk2.0-0" bash _meta/install_direct.sh 
		
		fileUpload videlibri_$VERSION-1_amd64.deb "/VideLibri/VideLibri\ $VERSION/"
		webUpload  videlibri_$VERSION-1_amd64.deb /updates/
		;;


downloadTable) 
  URL=http://sourceforge.net/projects/videlibri/files/VideLibri/VideLibri%20$VERSION/
  PROJNAME=VideLibri
  ~/hg/programs/internet/xidel/xidel $URL --extract-exclude=pname  --extract-kind=xquery  \
     -e "pname := '$PROJNAME'" \
     -e 'declare variable $lang := 2; 
         declare function verboseName($n){ concat ( 
           if (contains($n, "win") or contains($n, ".exe")) then "Windows: " 
           else if (contains($n, "linux")) then "Universal Linux: " 
           else if (contains($n, ".deb")) then "Debian/Ubuntu: " 
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
         { for $link in match(<TABLE id="files_list"><t:loop><TR class="file warn"><TH><A class="name">{{link := object(), link.verboseName := verboseName(.), link.a := .}}</A></TH><td/><td>{{link.size := .}}</td></TR></t:loop></TABLE>, /).link 
           order by $link.verboseName descending 
           return <tr><td>{$link.verboseName}</td><td><a href="{$link.a/@href}">{$link.a/text()}</a></td><td>{$link.size/text()}</td></tr>}</table></div>'     --printed-node-format html > $VIDELIBRIBASE/_meta/sfsite/downloadTable.html
  cat $VIDELIBRIBASE/_meta/sfsite/downloadTable.html
  webUpload $VIDELIBRIBASE/_meta/sfsite/downloadTable.html /

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

esac

