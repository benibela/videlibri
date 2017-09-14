#/bin/sh
builddir="$1"

echo "copying files"
mkdir -p $builddir/usr/share/videlibri/ $builddir/usr/share/videlibri/data $builddir/usr/share/applications &&
cp videlibri $builddir/usr/bin/ &&
cp -R data/libraries $builddir/usr/share/videlibri/data &&
cp data/lclstrconsts.de.po $builddir/usr/share/videlibri/data &&
cp data/*.ico $builddir/usr/share/videlibri/data &&
cp data/videlibri.chm $builddir/usr/share/videlibri/data &&
cp data/*.png $builddir/usr/share/videlibri/data &&
cp data/smallYellow.ico $builddir/usr/share/videlibri/data &&
cp _meta/installer/add/data/machine.config $builddir/usr/share/videlibri/data  

echo "install desktop"
#desktop-file-install videlibri.desktop #stupid thing crashes
cp videlibri.desktop $builddir/usr/share/applications/


