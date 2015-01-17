#!/bin/bash



libData=$1
publishId=$2
tmp=/tmp/tmpvl$publishId

source ../../../manageUtils.sh
sfProject videlibri
VIDELIBRIBASE=$HGROOT/programs/internet/VideLibri



eval $(xidel --output-format bash $libData -e 'name:=//longName/@value, templateId:=//template/@value')
libDataNew=$(basename $libData | sed -e 's/[.]xml/New.xml/' )
libDataIdNew=$( sed -e 's/[.]xml//' <<<"$libDataNew")
if [[ "$3" = "--override" ]]; then templateIdNew=${templateId}
message="Da dieses Template ein existierendes Template überschreibt, muss VideLibri nach der Installation neugestartet werden"
else templateIdNew=${templateId}New
message=
fi
templatePath=$(grep -oE '.*/' <<<$libData)templates


rm -rf $tmp
mkdir $tmp
mkdir $tmp/newlibs


cat > $tmp/$publishId.html <<EOF 
<html>
<head>
<meta charset="utf-8"/>
<link rel="videlibri.description" href="newlibs/$libDataNew"/>
<link rel="videlibri.template" href="$templateIdNew/template"/>
</head>
<body>
Neues Template f&uuml;r  (die) "$name" <span style="font-size: 50%">vom $(LC_ALL=de_DE.utf-8 date)</span><br><br>

<p>Installationshinweise für: <a href="/help/templateinstallation.html#android">Android</a> und <a href="/help/templateinstallation.html#desktop">Desktop</a>-Version.

<p>Adresse des Templates, die zur Installation in VideLibri eingegeben werden muss: http://videlibri.sourceforge.net/test/$publishId.html

<p>$message

<p>

<p style="font-size: 75%; margin-top: 2em">Quellcode des Templates anzeigen: <a href="newlibs/$libDataNew">Bibliotheksmetadata</a>, <a href="view-source:$templateIdNew/template">Template selbst</a>, <a href="$templateIdNew/">verwendete Patterns</a>.

</body>
</html>
EOF

#cat $tmp/$publishId.html

cp -Lr $templatePath/$templateId $tmp/$templateIdNew
cp $libData $tmp/newlibs/$libDataNew

#sed -e '' -i $tmp/newlibs/$libDataNew
xmlstarlet ed -L -u //longName/@value -v "$name (Neu)" $tmp/newlibs/$libDataNew
xmlstarlet ed -L -u //template/@value -v "$templateIdNew" $tmp/newlibs/$libDataNew
xmlstarlet ed -L -s /library -t elem -n "id" -v "" $tmp/newlibs/$libDataNew
xmlstarlet ed -L -s /library/id -t attr -n "value" -v "$libDataIdNew" $tmp/newlibs/$libDataNew
xmlstarlet ed -L -u //id/@value -v "$libDataIdNew" $tmp/newlibs/$libDataNew
#xmlstarlet ed -L -u //id -v "$libDataIdNew" $tmp/newlibs/$libDataNew
if [[ "$3" = "--override" ]]; then
INTVERSION=`grep 'versionNumber *: *integer' $VIDELIBRIBASE/applicationconfig.pas | grep -oE "[0-9]+" | head -1 `;
#xmlstarlet cannot change template (invalid xml) ??
#xmlstarlet ed -L --insert "/actions" --type attr -n "max-version" -v "$INTVERSION" $tmp/$templateIdNew/template
#xmlstarlet ed -L --insert "/actions" --type attr -n "version-mismatch" -v "skip" $tmp/$templateIdNew/template
sed "s/<actions/<actions max-version=\"$INTVERSION\" version-mismatch=\"skip\"/" -i  $tmp/$templateIdNew/template
fi

cd $tmp
ls
webUpload * /test/
webUpload newlibs/* /test/newlibs/
webUpload $templateIdNew/* /test/$templateIdNew/
