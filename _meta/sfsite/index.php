<!DOCTYPE html>
<html lang="de">	
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<meta http-equiv="content-language" content="de"/>
<link rel="stylesheet" type="text/css" href="all.css"/>
<title>VideLibri - Die Bibliotheks-App</title></head>

<?php
//from http://stackoverflow.com/questions/3770513/detect-browser-language-in-php

// parse list of comma separated language tags and sort it by the quality value
function parseLanguageList($languageList) {
    if (is_null($languageList)) {
        if (!isset($_SERVER['HTTP_ACCEPT_LANGUAGE'])) {
            return array();
        }
        $languageList = $_SERVER['HTTP_ACCEPT_LANGUAGE'];
    }
    $languages = array();
    $languageRanges = explode(',', trim($languageList));
    foreach ($languageRanges as $languageRange) {
        if (preg_match('/(\*|[a-zA-Z0-9]{1,8}(?:-[a-zA-Z0-9]{1,8})*)(?:\s*;\s*q\s*=\s*(0(?:\.\d{0,3})|1(?:\.0{0,3})))?/', trim($languageRange), $match)) {
            if (!isset($match[2])) {
                $match[2] = '1.0';
            } else {
                $match[2] = (string) floatval($match[2]);
            }
            if (!isset($languages[$match[2]])) {
                $languages[$match[2]] = array();
            }
            $languages[$match[2]][] = strtolower($match[1]);
        }
    }
    krsort($languages);
    return $languages;
}

// compare two parsed arrays of language tags and find the matches
function findMatches($accepted, $available) {
    $matches = array();
    $any = false;
    foreach ($accepted as $acceptedQuality => $acceptedValues) {
        $acceptedQuality = floatval($acceptedQuality);
        if ($acceptedQuality === 0.0) continue;
        foreach ($available as $availableQuality => $availableValues) {
            $availableQuality = floatval($availableQuality);
            if ($availableQuality === 0.0) continue;
            foreach ($acceptedValues as $acceptedValue) {
                if ($acceptedValue === '*') {
                    $any = true;
                }
                foreach ($availableValues as $availableValue) {
                    $matchingGrade = matchLanguage($acceptedValue, $availableValue);
                    if ($matchingGrade > 0) {
                        $q = (string) ($acceptedQuality * $availableQuality * $matchingGrade);
                        if (!isset($matches[$q])) {
                            $matches[$q] = array();
                        }
                        if (!in_array($availableValue, $matches[$q])) {
                            $matches[$q][] = $availableValue;
                        }
                    }
                }
            }
        }
    }
    if (count($matches) === 0 && $any) {
        $matches = $available;
    }
    krsort($matches);
    return $matches;
}

// compare two language tags and distinguish the degree of matching
function matchLanguage($a, $b) {
    $a = explode('-', $a);
    $b = explode('-', $b);
    for ($i=0, $n=min(count($a), count($b)); $i<$n; $i++) {
        if ($a[$i] !== $b[$i]) break;
    }
    return $i === 0 ? 0 : (float) $i / count($a);
}

$accepted = parseLanguageList($_SERVER['HTTP_ACCEPT_LANGUAGE']);
$available = parseLanguageList('en, de, DE');
$matches = findMatches($accepted, $available);
if (count($matches) == 0 || count(reset($matches)) == 0 || (reset(reset($matches)) != "de" && reset(reset($matches)) != "DE"))
  print ("<style>#navigation a.enlink { text-decoration: blink; font-weight: bold; color: #AA0000} </style>");
  //$
?>
<body>
<h1><a name="home">VideLibri - Die Bibliotheks-App</a></h1>

<ul id="navigation">
  <li style="border-left: 1px solid blue;"><a href="#home">Start</a></li>
  <li><a href="#features">Features</a></li>
  <li><a href="#libraries">Unterstützte Büchereien</a></li>
  <li><a href="#screenshots">Screenshots</a></li>
  <li><a href="#downloads">Download</a></li>
  <li><a href="#spinoffs">Spin-Offs</a></li>
  <li><a href="#contact">Kontakt</a></li>
  
  <li style="right: 1em; position:absolute; margin-top: -1em"><a class="enlink" href="index_en.html">English Version</a></li>
</ul>

<div class="content" style="border:none;">


<div style="float: right">
<a href="https://play.google.com/store/apps/details?id=de.benibela.videlibri" rel="nofollow" style="vertical-align:center;display:block">
  <img alt="Android app on Google Play"
       src="https://developer.android.com/images/brand/de_app_rgb_wo_45.png" style="position:relative;top: -40px" width="125" height="45"/>

  <img alt="Android app on Google Play" width="126" height="126" src="img/qrcode_pretty.png" />
</a>
</div>

Mit VideLibri kann man auf die Katalogen/WebOPACs zahlreicher Bibliotheken zugreifen, um darin zu suchen und seine Ausleihen zu sehen oder zu verlängern.

</div>


<h2><div><a name="features">Features</a></div></h2>
<div class="content">
VideLibri hat folgende Vorteile gegenüber den normalen Web-Katalogen der Büchereien:
<ul class="f1list">
<li><h3>Automatische Verlängerung</h3>
VideLibri verlängert alle ausgeliehenen Bücher automatisch, so dass man dies nie vergessen kann. 
</li>
<li><h3>Zuverlässige Benachrichtigung über Ausleihfristen</h3>
Bei den Benachrichtigungen der Büchereien passiert es ständig, dass die Benachrichtigungsemails nicht ankommen, oder ihr Katalog nicht erreichbar ist. 
VideLibri ist zuverlässiger, da es auf dem lokalen Rechner läuft und eine Art fail-safe Prinzip realisiert: es wird immer gewarnt, wenn nicht gezeigt werden kann, dass keine Bücher abzugeben sind. 
</li>
<li><h3>Kein permanenter Internetzugang nötig</h3>
Um den normalen Katalog aufzurufen, muss man immer eine Internetverbindung haben. VideLibri dagegen speichert  alle Ausleihdaten auf der Festplatte, so dass es auch funktioniert, wenn man zeitweilig nicht online ist, weil man beispielsweise im Zug sitzt.
</li>
<li><h3>Gesamthistorie aller Ausleihen</h3>
Es speichert auch alle jemals ausgeliehenen Bücher, wodurch  man jederzeit nachsehen kann, wann man, welche Bücher ausgeliehen hatte. Das ist nützlich, wenn man bei einer Arbeit vergessen hat, das Literaturverzeichnis zu schreiben, oder einfach nur wissen will, wie viel man ausgeliehen hatte.  (Paranoide können die Funktion aber auch abschalten)
</li>
<li><h3>Übersicht über mehrere Ausleihkonten</h3>
VideLibri zeigt die Ausleihdaten von mehreren Konten gleichzeitig an, so dass man, wenn man in mehreren Büchereien angemeldet ist, nicht jeden Katalog einzeln aufrufen muss. Auch wenn in einer mehrköpfigen Familie jeder eine eigene Ausleihkarte hat, wie von der Benutzerordnung vorgeschrieben, stellt es sicher, dass niemand/kein Kind vergessen hat, etwas abzugeben.
</li>
<li><h3>Besseres Interface</h3>
VideLibri ist nach der Installation sehr viel einfacher zu benutzen, als die meisten Webkataloge/WebOPACs der Büchereien. Man muss nur einmal klicken, um die Ausleihliste zu öffnen, und braucht nicht jedesmal sein Passwort einzugeben. Zudem werden alle Bücher farbkodiert in einer sortierbaren Liste angezeigt und die jeweiligen Abgabezeiten nach Wochen getrennt.
</li>
<li><h3>Mobile Android-Version</h3>
VideLibri läuft als App nativ auf Androidsystemen und bietet so eine bessere und bequemere Integration in das System als Webkataloge. 
</li>
<li><h3>Mehr Details</h3>
Viele Bibliotheken zeigen in ihrem Online-Katalog keine Details an. So fehlt bei den Stadtbüchereien Düsseldorfs normalerweise der Autor; während die Berliner Staatsbibliothek weder Autor noch Titel anzeigt und stattdessen lediglich die Inventarnummer nennt. VideLibri dagegen kann diese Informationen anzeigen, im Falle der Stabi wird automatisch im Katalog gesucht, für andere Büchereien kann man manuell die Digibib-Suche starten<sup class="desktop">D</sup> und bekommt dann sogar ein Titelbild<sup class="desktop">D</sup> angezeigt. 
</li>

<li><h3>E-Mailbenachrichtigung<sup class="desktop">D</sup></h3> 
VideLibri kann einen E-Mailbericht über alle Medien versenden, in dem fällige und nicht fällige Bücher aufgelistet sind.

<li><h3>Open-Source</h3>
Jeder kann VideLibri genauso ändern, wie er es haben will, und auch beliebige neue Büchereien hinzufügen.
</li>
</ul>

Einige andere Features sind:
<!--    *  Es können die Medien von mehreren Konten gleichzeitig betrachtet und verlängert werden
    * Bald fällige Medien werden automatisch verlängert
    * Abgabezeiten in unterschiedlichen Wochen werden markiert.
    * Vor dem endgültigem Ablauf der Leihfrist wird man vom Programm benachrichtigt
    * Die Liste der ausgeliehenen Medien wird automatisch über die Internetseiten der Büchereien aktualisiert.
    * Die momentan ausgeliehenen Medien werden gespeichert, so dass man seine Ausleihen sehen kann, ohne dafür ins Internet zu müssen.
    * Es kann nach beliebigen (auch nicht ausgeliehenen) Medien gesucht werden
    * VideLibri wird (je nach Einstellung) bei jedem Systemstart minimiert in der Symbolleiste gestartet
    * Alle jemals ausgeliehenen Medien können gespeichert
    * Es gibt eine Statistik über alle ausgeliehenen Medien
    * BibTeX-Export für die Erstellung von Literaturverzeichnisse.
    * Automatisches Update
    * Links zu den Homepages und Katalogen der einzelnen Büchereien 
-->
<ul>
<li>Abgabezeiten in unterschiedlichen Wochen werden markiert<sup class="desktop">D</sup></li>
<li>Es kann nach beliebigen Medien gesucht werden</li>
<li>Vormerkungen und Bestellungen im Katalog</li>
<li>VideLibri wird (je nach Einstellung) bei jedem Systemstart minimiert in der Symbolleiste gestartet</li>
<li>Es gibt eine Statistik über alle ausgeliehenen Medien<sup class="desktop">D</sup></li>
<li>BibTeX-Export für die Erstellung von Literaturverzeichnissen<sup class="desktop">D</sup></li>
<li>Vollständige XQuery-Unterstützung für neue Templates</li>
<li>Automatisches Update<sup class="desktop">D</sup></li>
<li>Speicherung von gesuchten Medien in der Kontohistorie, was zur Literaturverwaltung oder als Merkliste verwendet werden kann.<sup class="desktop">D</sup></li>
</ul>

Nicht alle Features funktionieren mit allen Bibliotheken. Mit <sup class="desktop">D</sup>-markiere Features sind nur in der Desktopversion enthalten, und fehlen in der Androidversion.<br/><br/>

Es gibt allerdings auch ein paar Nachteile im Vergleich zu den online Web-Katalogen:
<ul>
<li>Es muss erst heruntergeladen und eingerichtet werden, so dass man nicht schnell auf einem fremden Computer seine Ausleihdaten nachsehen kann. (man kann es aber auf einem USB-Stick installieren) </li>
<li>Es unterstützt nicht alle Büchereien und muss die Internetseite der Bücherei kennen. Wenn die Bücherei ihre Internetseite plötzlich ändert, lassen sich also die Daten nicht mehr aktualisieren. (da alles gecacht ist, funktioniert VideLibri aber trotzdem noch einen Monat weiter. Allerdings ist der Bibliotheksserver auch üblicherweise abgestürzt, so dass der Online-Katalog selbst nicht aufgerufen werden kann, und der VideLibri-Cache die einzige verfügbare Ausleihenübersicht ist) </li>
</ul>

Praktisch sollte man das Programm genau dann benutzen, wenn man jedes Jahr mehrere hunderte Bücher ausleiht. 
</div>

<h2><div><a name="screenshots">Screenshots</a></div></h2>
<div class="content">
<center>
<img src="http://sourceforge.net/dbimage.php?id=280463" title="Allgemeine Übersicht über ausgeliehene Bücher (von den Büchereien in Düsseldorf und Aachen)" alt="Allgemeine Übersicht über ausgeliehene Bücher (von den Büchereien in Düsseldorf und Aachen)"/>

<img src="http://sourceforge.net/dbimage.php?id=280465" title="Ausleihstatistik (über alle von meiner Familie in Düsseldorf und Aachen ausgeliehenen Bücher)" alt="Ausleihstatistik (über alle von meiner Familie in Düsseldorf und Aachen ausgeliehenen Bücher)"/>


<img src="http://sourceforge.net/dbimage.php?id=306557" title="Suche in der Digibib. (öffnet sich automatisch nach Doppelklick auf ein ausgeliehenes Buch, und ist z.B.: bei den Stadtbüchereien von Düsseldorf nötig, um mehr als Autor oder Titel angezeigt zu bekommen)" alt="Suche in der Digibib. (öffnet sich automatisch nach Doppelklick auf ein ausgeliehenes Buch, und ist z.B.: bei den Stadtbüchereien von Düsseldorf nötig, um mehr als Autor oder Titel angezeigt zu bekommen)"/>

<div>
<img src="http://sourceforge.net/p/videlibri/screenshot/android.png" title="VideLibri 1.5 unter Android" alt="VideLibri 1.5 unter Android"/>
<div style="display: inline-block">
<img src="http://sourceforge.net/p/videlibri/screenshot/android-search-list.png" title="VideLibri 1.5 unter Android (Liste der Suchergebnisse)" alt="VideLibri 1.5 unter Android (Liste der Suchergebnisse)"/>
<img src="http://sourceforge.net/p/videlibri/screenshot/android-search-result.png" title="VideLibri 1.5 unter Android (Details eines Suchergebnisses)" alt="VideLibri 1.5 unter Android (Details eines Suchergebnis)"/>
</div>
</div>

</center>
</div>
<h2><div><a name="libraries">Unterstützte Büchereien</a></div></h2>
<div class="content">
Prinzipiell unterstützt VideLibri sämtliche existierenden Bibliotheken, da jeder durch das zugrundeliegende Templatesystem beliebige Datenquellen hinzufügen kann.<br>
Es liegen aber auch einige Templates standardmäßig bei, welche erfolgreich mit den folgenden Büchereien getestet wurden:<br><br>

<?php include 'supportTable.html'; ?>

<br>

? in der Tabelle heißt, dass mir dort keine Ausleihkarte oder verlängerbare Medien zur Verfügung standen, und ich die entsprechende Funktionalität dort nicht direkt testen konnte. Wenn es das gleiche System wie eine oben mit "ja" markierte Bibliothek verwendet, funktioniert es aber vermutlich trotzdem.

<br><br>

Man kann es aber auch mit anderen Bibliotheken benutzen, wenn man dort eine Ausleihkarte hat. Wegen dem zugrundeliegenden Templatesystem ist es nicht einmal nötig VideLibri umzuprogrammieren. Um ein solches Template für eine neue Bibliothek hinzuzufügen, speichert man im wesentlichen jede Seite des WebOPACs ab, und markiert die Stellen, die Medieninformationen wie Autor/Titel enthalten, mit semantischen Annotationen. Wie genau  das funktioniert, ist ausführlich in der <a href="http://videlibri.sourceforge.net/help/neuebibliothek.html">Hilfe</a> beschrieben.  Programmierkenntnisse sind dabei nur erforderlich, wenn der Katalog irgendwelche Merkwürdigkeiten, wie beispielsweise ungültiges HTML oder einen komplizierten XSS-Schutz, aufweist, oder man das Template für mehrere unterschiedliche Bibliotheken verwenden will. Das  <a href="http://userscripts.org/scripts/show/434588">VideLibri-Greasemonkey-Skript</a> (<a href="http://videlibri.sourceforge.net/script.user.js">Installations-Mirror</a>) kann verwendet werden, um ein Kontozugriff-Template  automatisch  durch Markieren der Buchinformationen in Firefox zu erstellen. (siehe <a href="http://www.youtube.com/watch?v=PUrBJ6wOXvE">Video</a>, wie man das Skript verwenden kann)<br>
Zudem kann eine interessierte Bibliothek ein Videlibri-Template auf ihrem Server hinterlegen und mit den entsprechenden Linkmetatags verlinken. Dann reicht die Eingabe der Serveraddresse in Videlibri, um das Template automatisch herunterzuladen und zu installieren.<br>
Verwendet die Bücherei ein aktuelles aleph/libero/sisis/pica/aDISWeb-System ist es nicht mal nötig ein Template zu schreiben, weil man dann einfach die Serveradresse in die xml-Dateien im data/libraries Verzeichnis eintragen kann. <br/><br/>

Nichtsdestotrotz kann ich aber bei Anfragen einer Bibliothek oder eines Lesers auch direkte Unterstützung für den entsprechenden Katalog in VideLibri einbauen. Für eine vollständige Unterstützung benötigte ich allerdings auch gültige Kontozugangsdaten mit verlängerbaren Büchern. Ein Template für das bloße Anzeigen der Ausleihen kann ich in der Regel innerhalb eines Tages erstellt, ein Template für alle Funktionen inklusive Vormerkungen kann aber auch schon mal eine Woche dauern (und Testen, ob es keine Sonderfälle gibt noch länger). Auch bei Fragen zu den Templates können Sie mir gerne eine <a href="#contact">Mail</a> schreiben.

</div>

<h2><div><a name="downloads">Downloads</a></div></h2>
<div class="content">
<?php include 'downloadTable.html'; ?>

<br><br>
Die Windowsversion besteht aus einem automatischen Installer und ist 32 Bit, so dass sie auf allen Windowscomputern laufen sollte. <br>
Für Linux muss jeweils die passende Version heruntergeladen werden.

<br><br> 

Es gibt auch einen <a href="updates/changelog.xml">Changelog</a>, das alle Änderungen der letzten Version auflistet (und ein <a href="https://sourceforge.net/p/videlibri/code/ci/tip/log/">Repositorylog</a>, welches alle Entwicklungsschritte zeigt.).<br><br>

Den gesamten Quellcode gibt es in einem <a href="https://sourceforge.net/p/videlibri/code/ci/tip/tree/">Mercurial-Repository</a>.<br>
Das Programm ist in FreePascal/Lazarus geschrieben, und sollte sich einfach compilern lassen, indem man die bookWatch.lpi in Lazarus öffnet und in den Projekteinstellungen sein Betriebssystem auswählt. 
Damit sollte es unter Windows/Linux/Mac mit gtk/qt/win32/cocoa-Interface laufen (getestet mit Windows/Linux und gtk/qt/win32). Man benötigt aber mindestens Lazarus 1.0 und FPC 2.6.0.
<br>
Das Selbst-Compilern einer Androidversion ist komplizierter und erfordert sowohl ein installiertes Android-SDK wie auch ein Android-NDK. Nach Anpassung der Pfade kann das <code>android/manage.sh</code>-Skript verwendet werden, um es automatisch zu compilern.
</div>
<br><br><br><br>
<h2><div><a name="spinoffs">Spin-offs</a></div></h2>
<div class="content">
Mit dem VideLibri-Framework ist es auch trivial andere Webseiten in lokale Programme umzuwandeln, und, da der Quellcode dieser Programme größtenteils mit VideLibri übereinstimmt, liegen sie im selben <a href="https://sourceforge.net/p/videlibri/code/ci/tip/tree/">Repository</a>:
<h3><a name="xibel">Xidel</a></h3>
Xidel ist VideLibri ohne libri, also ohne irgendwas, das mit Bibliotheken und Büchern zu tun, und nützlich, um Internetseiten abzufragen.<br>
Es hat sogar eine eigene <a href="xidel.html">Xidel-Seite</a>.
<h3>SourceForgeResponder</h3>
Der SFR ist ein Dämon, der jede an localhost geschickte E-Mail in eine Antwort auf der SourceForge-Seite umwandelt und alle Antworten in einem Ideatorrent als Mail verschickt.<br>
Er lässt ziemlich einfach zu verwenden:
<ol>
<li>Zuerst muss er im Hintergrund mit übergebenen Benutzernamen/Passwort gestartet werden</li>
<li>Dann kann auf jede SourceForge-Benachrichtigungsmail geantwortet werden, indem eine E-Mail an localhost geschickt wird, welche die SF-Mail vollständig zitiert.</li>
<li>Der Dämon wird diese Mail dann automatisch in einen Post auf der SF-Seite umwandeln (das dauert allerdings 5 Sekunden, während der er momentan keine neuen Mails empfangen kann)</li>
</ol>
Achtung: Falls das Posten der Antwort nicht funktioniert, verfällt die Nachricht ohne irgendwelche Hinweise außer den Fehlermeldungen auf stdout. Das ist aber eigentlich kein Problem, da SourceForge sowieso auf jeden Beitrag eine Benachrichtungsmail verschickt, an derem Ausbleiben man den Fehler erkennen kann.<br>
Die Details stehen im Quellcode.
</div>
<h2><div><a id="contact">Kontakt</a></div></h2>
Autor: Benito van der Zander, <a href="benito_NOSPAM_benibela.de">benito_NOSPAM_benibela.de</a>, <a href="http://www.benibela.de/index_en.html">www.benibela.de</a><br><br>
<a href="datenschutz.html">Datenschutzerklärung</a>
<div id="sf-logo"><a href="http://sourceforge.net/projects/videlibri"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=359854&amp;type=1" width="125" height="37" border="0" alt="SourceForge.net Logo" /></a></div>

<!-- Piwik -->
<script type="text/javascript">
var pkBaseURL = (("https:" == document.location.protocol) ? "https://videlibri.sourceforge.net/piwik/" : "http://videlibri.sourceforge.net/piwik/");
document.write(unescape("%3Cscript src='" + pkBaseURL + "piwik.js' type='text/javascript'%3E%3C/script%3E"));
</script><script type="text/javascript">
try {
var piwikTracker = Piwik.getTracker(pkBaseURL + "piwik.php", 1);
piwikTracker.trackPageView();
piwikTracker.enableLinkTracking();
} catch( err ) {}
</script><noscript><p><img src="http://videlibri.sourceforge.net/piwik/piwik.php?idsite=1" style="border:0" alt="" /></p></noscript>
<!-- End Piwik Tracking Code -->


</body>
</html>
