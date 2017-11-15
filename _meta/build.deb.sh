#!/bin/sh
VIDELIBRIBASE=.
builddir=/tmp/videlibridebbuild
metadir=$VIDELIBRIBASE/_meta
debiandir=$metadir/debian

INTVERSION=`grep versionNumber:integer $VIDELIBRIBASE/applicationconfig.pas | grep -oE "[0-9]+" `;
VERSION=`echo "scale=3;$INTVERSION/1000" | bc`;
VERSION=`export LC_NUMERIC=C; printf "%1.3f\n" $VERSION`;



rm -rf $builddir/
mkdir -p $builddir/DEBIAN $builddir/usr/bin $builddir/usr/share/doc/videlibri $builddir/usr/share/videlibri
$metadir/install_direct.sh $builddir
strip --strip-all $builddir/usr/bin/videlibri
cp -r $debiandir/control $debiandir/conffiles $builddir/DEBIAN/
cp $debiandir/copyright $builddir/usr/share/doc/videlibri/copyright
xidel $metadir/version/changelog.xml -e '/changelog/build!(
  x"videlibri ({format-number(xs:decimal(@version)*0.001, "0.000")}) unstable; urgency=low", 
   "", .//(fix,add,change)/("  * "||.),
   "",
   " -- Benito van der Zander <benito@benibela.de>  "||format-date(xs:date(@date), "[FNn,3-3], [D] [MNn,3-3] [Y]")||" 00:00:00 +0100", "" )' | gzip -9 -n -c | $builddir/usr/share/doc/videlibri/changelog.gz


sed -Ee "s/Version:.*/Version: $VERSION/" -i $builddir/DEBIAN/control
if file videlibri | grep x86-64; then arch=amd64; 
else if file videlibri | grep ARM; then arch=arm; else arch=i386; 
fi; fi
sed -Ee "s/Architecture:.*/Architecture: $arch/" -i $builddir/DEBIAN/control

pkg=videlibri_$VERSION-1_$arch.deb
fakeroot dpkg-deb -b $builddir/ $pkg

echo $pkg