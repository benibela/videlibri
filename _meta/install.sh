#/bin/sh
echo "copying files"
mkdir -p /usr/share/videlibri/ && 
cp videlibri /usr/bin/ &&
cp -R data /usr/share/videlibri 
echo "install desktop"
desktop-file-install videlibri.desktop
echo "autostart"
cp videlibri.desktop ~/.config/autostart/
