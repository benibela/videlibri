#!/bin/bash

#Use: like ./publish-test-lib.sh data/libraries/DE_Berlin_Berlin_UbTU.xml berlin-tu --override
# path to metadata, new temporary id, new lib vs template override


libData=$1
publishId=$2
tmp=/tmp/tmpvl$publishId

source ../../../manageUtils.sh
sfProject videlibri
VIDELIBRIBASE=$HGROOT/programs/internet/VideLibri



eval $(xidel --output-format bash $libData -e 'name:=//longName/@value, templateId:=//template/@value')
templateSetMaxVersion=
libDataSetMaxVersion=
libDataIdSuffix=New
if [[ "$3" = "--override" ]]; then 
  templateIdNew=${templateId}
  templateSetMaxVersion=true
  message="Da dieses Template ein existierendes Template überschreibt, müssen ältere VideLibri-Versionen nach der Installation neugestartet werden"
elif [[ "$3" = "--override-all" ]]; then 
  templateIdNew=${templateId}
  templateSetMaxVersion=true
  libDataSetMaxVersion=true
  message="Da dieses Template ein existierendes Template überschreibt, müssen ältere VideLibri-Versionen nach der Installation neugestartet werden"
  libDataIdSuffix=
elif [[ "$3" = "--new-library" ]]; then 
  templateIdNew=
  message=
  libDataIdSuffix=
else
  templateIdNew=${templateId}New
  message=
fi
libDataNew=$(basename $libData | sed -e 's/[.]xml/'$libDataIdSuffix'.xml/' )
libDataIdNew=$( sed -e 's/[.]xml//' <<<"$libDataNew")
templatePath=$(grep -oE '.*/' <<<$libData)templates


rm -rf $tmp
mkdir $tmp
mkdir $tmp/newlibs

export libDataNew && export templateIdNew && export name && export publishId && export message && export libDataNew

xidel --html --variable libDataNew,templateIdNew,name,publishId,message,libDataNew  > $tmp/$publishId.html --xquery '
<html>
<head>
<meta charset="utf-8"/>
<link rel="videlibri.description" href="newlibs/{$libDataNew}"/>
{<link rel="videlibri.template" href="{$templateIdNew}/template"/>[$templateIdNew]}
</head>
<body>
{if ($templateIdNew) then "Neues Template" else "Neue Daten"} für  (die) "{$name}" <span style="font-size: 50%">vom {format-dateTime(current-dateTime(), "[Y]-[M02]-[D02] [h02]:[m02]:[s02]")}</span><br/><br/>

<p>Installationshinweise für: <a href="/help/templateinstallation.html#android">Android</a> und <a href="/help/templateinstallation.html#desktop">Desktop</a>-Version.</p>

<p>Die Adresse, die als "Template-Adresse" zur Installation in VideLibri eingegeben werden muss: <code style="font-weight: bold">https://www.videlibri.de/test/{$publishId}.html</code></p>

<p>{$message}</p>

<p></p>

<p style="font-size: 66%; margin-top: 2em">Quellcode anzeigen: 
<a href="newlibs/{$libDataNew}">Bibliotheksmetadata</a>{
  if ($templateIdNew) then (
    ", ",
   <a href="{$templateIdNew}/template">Template selbst</a>,", ",
   <a href="{$templateIdNew}/">verwendete Patterns</a>
  ) else ()}.</p>
</body>
</html>
'

#cat $tmp/$publishId.html


cp $libData $tmp/newlibs/$libDataNew

if [[ -n "$templateIdNew" ]]; then
cp -Lr $templatePath/$templateId $tmp/$templateIdNew
cat > $tmp/$templateIdNew/.htaccess <<EOF
ForceType text/plain
Options +Indexes
EOF
xmlstarlet ed -L -u "//template/@value[. != 'digibib']" -v "$templateIdNew" $tmp/newlibs/$libDataNew
fi

#sed -e '' -i $tmp/newlibs/$libDataNew
xmlstarlet ed -L -u //longName/@value -v "$name (Neu)" $tmp/newlibs/$libDataNew
xmlstarlet ed -L -s /library -t elem -n "id" -v "" $tmp/newlibs/$libDataNew
xmlstarlet ed -L -s /library/id -t attr -n "value" -v "$libDataIdNew" $tmp/newlibs/$libDataNew
xmlstarlet ed -L -u //id/@value -v "$libDataIdNew" $tmp/newlibs/$libDataNew
#xmlstarlet ed -L -u //id -v "$libDataIdNew" $tmp/newlibs/$libDataNew
if [[ "$templateSetMaxVersion" = "true" ]]; then
INTVERSION=`grep 'versionNumber *: *integer' $VIDELIBRIBASE/applicationconfig.pas | grep -oE "[0-9]+" | head -1 `;
#xmlstarlet cannot change template (invalid xml) ??
#xmlstarlet ed -L --insert "/actions" --type attr -n "max-version" -v "$INTVERSION" $tmp/$templateIdNew/template
#xmlstarlet ed -L --insert "/actions" --type attr -n "version-mismatch" -v "skip" $tmp/$templateIdNew/template
sed "s/<actions/<actions max-version=\"$INTVERSION\" version-mismatch=\"skip\"/" -i  $tmp/$templateIdNew/template
fi
if [[ "$libDataSetMaxVersion" = "true" ]]; then
INTVERSION=`grep 'versionNumber *: *integer' $VIDELIBRIBASE/applicationconfig.pas | grep -oE "[0-9]+" | head -1 `;
xmlstarlet ed -L --insert "/library" --type attr -n "max-version" -v "$INTVERSION" $tmp/newlibs/$libDataNew
xmlstarlet ed -L --insert "/library" --type attr -n "version-mismatch" -v "skip" $tmp/newlibs/$libDataNew
fi


cd $tmp
ls
webUpload * /test/
webUpload newlibs/* /test/newlibs/
if [[ -n "$templateIdNew" ]]; then
webUpload $templateIdNew/* /test/$templateIdNew/
fi

echo here you go: https://www.videlibri.de/test/$publishId.html
