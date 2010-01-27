#/bin/sh
echo "removing files"
rm  /usr/bin/videlibri &&
rm -R /usr/share/videlibri
echo "removing autostart"
rm ~/.config/autostart/videlibri.desktop
echo "removing desktop"
rm /usr/share/applications/videlibri.desktop
