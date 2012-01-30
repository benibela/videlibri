#/bin/sh
echo "copying files"
mkdir -p /usr/share/videlibri/ && 
cp videlibri /usr/bin/ &&
cp -R data /usr/share/videlibri 
echo "install desktop"
#desktop-file-install videlibri.desktop # #stupid thing crashes
cp videlibri.desktop /usr/share/applications
echo "autostart"
cp videlibri.desktop ~/.config/autostart/
