<!DOCTYPE html>
<html lang="de">	
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<meta http-equiv="content-language" content="de"/>
<link rel="stylesheet" type="text/css" href="all.css"/>
<title>VideLibri - Ein Bücher-Ausleihverwaltungsprogramm</title></head>

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
?>
<body>
<h1><a name="home">VideLibri - Ein Bücher-Ausleihverwaltungsprogramm</a></h1>

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
VideLibri ist eine Erweiterung der normalen Büchereikataloge/WebOPACs, die einem alle jemals ausgeliehenen Bücher anzeigt und automatisch  verlängert.
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
<li><h3>Kein permanter Internetzugang nötig</h3>
Um den normalen Katalog aufzurufen, muss man immer eine Internetverbindung haben. VideLibri dagegen speichert  alle Ausleihdaten auf der Festplatte, so dass es auch funktioniert, wenn man zeitweilig nicht online ist, weil man beispielsweise im Zug sitzt.
</li>
<li><h3>Gesamthistorie aller Ausleihen</h3>
Es speichert auch alle jemals ausgeliehenen Bücher, wodurch  man jederzeit nachsehen kann, wann man, welche Bücher ausgeliehen hatte. Das ist nützlich, wenn man bei einer Arbeit vergessen hat, das Literaturverzeichnis zu schreiben, oder einfach nur wissen will, wieviel man ausgeliehen hatte.  (Paranoide können die Funktion aber auch abschalten)
</li>
<li><h3>Übersicht über mehrere Ausleihkonten</h3>
VideLibri zeigt die Ausleihdaten von mehreren Konten gleichzeitig an, so dass man, wenn man in mehreren Büchereien angemeldet ist, nicht jeden Katalog einzeln aufrufen muss. Auch wenn in einer mehrköpfigen Familie jeder eine eigene Ausleihkarte hat, wie von der Benutzerordnung vorgeschrieben, stellt es sicher, dass niemand/kein Kind vergessen hat, etwas abzugeben.
</li>
<li><h3>Mehr Details</h3>
Viele Bibliotheken zeigen in ihrem Online-Katalog keine Details an. So fehlt bei den Stadtbüchereien Düsseldorfs normalerweise der Autor; während die Berliner Staatsbibliothek weder Autor noch Titel anzeigt und stattdessen lediglich die Inventarnummer nennt. VideLibri dagegen kann diese Informationen anzeigen, im Falle der Stabi wird automatisch im Katalog gesucht, für andere Büchereien kann man manuell die Digibibsuche starten und bekommt dann sogar ein Titelbild angezeigt. 
</li>
<li><h3>Besseres Interface</h3>
VideLibri ist nach der Installation sehr viel einfacher zu benutzen, als die meisten Webkataloge/WebOPACs der Büchereien. Man muss nur einmal klicken, um die Ausleihliste zu öffnen, und braucht nicht jedesmal sein Passwort einzugeben. Zudem werden alle Bücher farbkodiert in einer sortierbaren Liste angezeigt und die jeweiligen Abgabezeiten nach Wochen getrennt.
</li>
<li><h3>E-Mailbenachrichtigung</h3> 
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
    * BibTex-Export für die Erstellung von Literaturverzeichnisse.
    * Automatisches Update
    * Links zu den Homepages der einzelnen Büchereien und der digi-bib
-->
<ul>
<li>Abgabezeiten in unterschiedlichen Wochen werden markiert</li>
<li>Es kann nach beliebigen (auch nicht ausgeliehenen, mittels der digibib) Medien gesucht werden</li>
<li>VideLibri wird (je nach Einstellung) bei jedem Systemstart minimiert in der Symbolleiste gestartet</li>
<li>Es gibt eine Statistik über alle ausgeliehenen Medien</li>
<li>BibTeX-Export für die Erstellung von Literaturverzeichnissen</li>
<li>Vollständige XQuery-Unterstützung für neue Templates</li>
<li>Automatisches Update</li>
</ul>

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
</center>
</div>
<h2><div><a name="libraries">Unterstützte Büchereien</a></div></h2>
<div class="content">
Bislang  wurde VideLibri mit diesen Büchereien getestet:<br><br>

<table class="bibsupport">
<thead>
<tr><th>Name der Bücherei</th><th>Ausleihenanzeige funktioniert<br><i>(zuletzt getestet)</i></th><th>Verlängerung funktioniert<br><i>(zuletzt getestet)</i></th><th>Suche funktioniert<br><i>(zuletzt getestet)</i></th><th>Büchereisystem</th></tr>
</thead>
<tr class="city"><td colspan=6><b>Aachen</b></td></tr>
<tr><td><a href="http://www.aachen.de/DE/stadt_buerger/bildung/oeffentliche_bibliothek/stadtteile/index.html" rel="nofollow">Stadtbibliothek Aachen</a></td>
        <td>ja (digibib) <i>(2011-05-16)</i></td> <td>ja <i>(2011-05-16)</i></td><td>nicht erlaubt</td><td>libero</td></tr>
<tr><td><a href="http://www.bth.rwth-aachen.de/" rel="nofollow">Die Hochschulbibliothek der RWTH Aachen</a></td>
        <td>ja (digibib) (direkt in 1.5) <i>(2011-05-16)</i></td><td>ja <i>(2012-07-08)</td><td>ja <i>(2011-05-16)</i></td><td>sisis</td></tr>

<tr class="city"><td colspan=6><b>Anhalt</b></td></tr>
<tr><td><a href="http://www.hs-anhalt.de/hochschulbibliothek" rel="nofollow">Hochschulbibliothek Anhalt</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr  class="city"><td colspan=6><b>Berlin</b></td></tr>
  <tr><td><a href="http://staatsbibliothek-berlin.de/" rel="nofollow">Staatsbibliothek zu Berlin </a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td><td>ja <i>(2012-12-23)</i></td> <td>ja <i>(2012-05-16)</i></td><td>pica/bibdia</td></tr>  
  <tr><td><a href="http://www.ub.tu-berlin.de/" rel="nofollow">Universitätsbibliothek der TU Berlin</a></td>
        <td>nein</td> <td>ja <i>(2012-07-12)</i></td><td>Ja <i>(2012-07-12)</i></td> <td>aleph</td></tr>
  <tr><td><a href="http://www.ub.hu-berlin.de/" rel="nofollow">Universitätsbibliothek der HU Berlin</a></td>
        <td>nein</td><td>ja <i>(2012-03-03)</i></td> <td>?</td><td>aleph</td></tr>
  <tr><td><a href="http://www.ub.fu-berlin.de/" rel="nofollow">Universitätsbibliothek der FU Berlin</a></td>
        <td>nein</td><td>ja <i>(2012-12-23)</i></td> <td>?</td><td>aleph</td></tr>

<tr class="city"><td colspan=6><b>Braunschweig</b></td></tr>
<tr><td><a href="http://www.biblio.tu-bs.de/" rel="nofollow">Universitätsbibliothek Braunschweig</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Bremen</b></td></tr>
<tr><td><a href="http://www.suub.uni-bremen.de/" rel="nofollow">Staats- und Universitätsbibliothek Bremen</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Clausthal</b></td></tr>
<tr><td><a href="http://www.suub.uni-bremen.de/" rel="nofollow">Universitätsbibliothek Clausthal</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Düsseldorf</b></td></tr>
  <tr><td><a href="http://www.duesseldorf.de/stadtbuechereien/index.shtml" rel="nofollow">Stadtbüchereien Düsseldorf</a></td> <td>ja (digibib) <i>(2013-04-26)</i></td> <td>ja <i>(täglich)</i></td><td>ja <i>(täglich)</i></td><td></td></tr>
  <tr><td><a href="http://www.bibl.fh-duesseldorf.de/" rel="nofollow">Fachhochschulbibliothek Düsseldorf</a></td> <td>ja (digibib) <i>(2011-05-16)</i></td> <td>ja <i>(2011-05-16)</i></td><td>ja <i>(2011-05-16)</i></td><td>libero</td></tr>
  <tr><td><a href="http://www.ub.uni-duesseldorf.de/" rel="nofollow">Universitäts- und Landesbibliothek Düsseldorf</a></td><td>ja (digibib) <i>(2011-05-16)</i></td><td>nicht mehr</td><td>nicht mehr</td><td>aleph</td></tr>

<tr class="city"><td colspan=6><b>Elsfleth</b></td></tr>
<tr><td><a href="http://www.jade-hs.de/?id=1465" rel="nofollow">Bibliothek der Jadehochschule Elsfleth</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Emden</b></td></tr>
<tr><td><a href="http://www.jalb.de/" rel="nofollow">Johannes a Lasco Bibliothek Emden</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>
<tr><td><a href="http://www.ostfriesischelandschaft.de/4.html" rel="nofollow">Landschaftsbibliothek Aurich</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>
<tr><td><a href="http://www.hs-emden-leer.de/einrichtungen/bibliothek.html" rel="nofollow">Hochschulbibliothek Emden/Leer</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Erfurt</b></td></tr>
<tr><td><a href="http://www.fh-erfurt.de/bibo/" rel="nofollow">Fachhochschulbibliothek Erfurt</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>
<tr><td><a href="http://www.uni-erfurt.de/bibliothek" rel="nofollow">Universitäts- und Forschungsbibliothek Erfurt/Gotha</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Göttingen</b></td></tr>
<tr><td><a href="http://www.sub.uni-goettingen.de/sub-aktuell/" rel="nofollow">Niedersächsische Staats- und Universitätsbibliothek</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Greifswald</b></td></tr>
<tr><td><a href="http://www.ub.uni-greifswald.de" rel="nofollow">Universitätsbibliothek Greifswald</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Halle</b></td></tr>
<tr><td><a href="http://www.bibliothek.uni-halle.de/" rel="nofollow">Universitäts- und Landesbibliothek Halle</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr  class="city"><td colspan=6><b>Hamburg</b></td></tr>
  <tr><td><a href="http://kataloge.uni-hamburg.de/" rel="nofollow">Campus Katalog Hamburg</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td><td>ja <i>(2011-05-16)</i></td><td>ja <i>(2011-05-16)</i></td><td>pica</td></tr>
  <tr><td><a href="http://www.hsu-bibliothek.de/" rel="nofollow">Bibliothek der Helmut-Schmidt-Universität Hamburg</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Hannover</b></td></tr>
<tr><td><a href="http://www.hs-hannover.de/bibl/" rel="nofollow">Bibliothek der Hochschule Hannover</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>
<tr><td><a href="http://www.tib.uni-hannover.de/" rel="nofollow">Technische Informationsbibliothek und Universitätsbibliothek Hannover</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>


<tr class="city"><td colspan=6><b>Hildesheim</b></td></tr>
<tr><td><a href="http://www.uni-hildesheim.de/index.php?id=bibliothek" rel="nofollow">Universitätsbibliothek Hildesheim</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>ja <i>(2013-04-26)</i></td> <td>ja <i>(2013-04-01)</i></td> <td>pica</td></tr>

<tr class="city"><td colspan=6><b>Ilmenau</b></td></tr>
<tr><td><a href="http://www.tu-ilmenau.de/ub/" rel="nofollow">Universitätsbibliothek Ilmenau</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>


<tr class="city"><td colspan=6><b>Jena</b></td></tr>
<tr><td><a href="http://www.thulb.uni-jena.de/" rel="nofollow">Thüringer Universitäts- und Landesbibliothek</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>
<tr><td><a href="http://www.fh-jena.de/bib/" rel="nofollow">Bibliothek der EAH Jena</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Kiel</b></td></tr>
<tr><td><a href="http://www.ub.uni-kiel.de/" rel="nofollow">Universitätsbibliothek Kiel</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>
<tr><td><a href="http://www.fh-kiel.de/index.php?id=2874" rel="nofollow">Bibliotheken der FH Kiel</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Lüneburg</b></td></tr>
<tr><td><a href="http://www.leuphana.de/bibliothek.html" rel="nofollow">Universitätsbibliothek Lüneburg</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Merseburg</b></td></tr>
<tr><td><a href="http://www.ub.ovgu.de/" rel="nofollow">Universitätsbibliothek Magdeburg</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>
<tr><td><a href="http://www.hs-merseburg.de/bibliothek" rel="nofollow">Hochschulbibliothek der Hochschule Merseburg</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr  class="city"><td colspan=6><b>München</b></td></tr>
  <tr><td><a href="https://ssl.muenchen.de/aDISWeb" rel="nofollow">Münchener Stadtbibliothek</a></td>
        <td>nein</td><td>ja <i>(2012-10-03)</i></td><td>ja <i>(2012-10-01)</i></td><td>adisweb</td></tr>


<tr class="city"><td colspan=6><b>Nordhausen</b></td></tr>
<tr><td><a href="http://www.fh-nordhausen.de/bibliothek.html" rel="nofollow">Fachhochschulbibliothek Nordhausen</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Osnabrück</b></td></tr>
<tr><td><a href="http://www.ub.uni-osnabrueck.de/" rel="nofollow">Universitätsbibliothek Osnabrück</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>
<tr><td><a href="http://www.bib.hs-osnabrueck.de/" rel="nofollow">Bibliothek der Hochschule Osnabrück</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Schmalkalden</b></td></tr>
<tr><td><a href="http://www.fh-schmalkalden.de/Bibliothek.html" rel="nofollow">Bibliothek der FH Schmalkalden</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Weimar</b></td></tr>
<tr><td><a href="http://www.uni-weimar.de/ub" rel="nofollow">Universitätsbibliothek Weimar</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

<tr class="city"><td colspan=6><b>Wilhelmshaven</b></td></tr>
<tr><td><a href="http://www.jade-hs.de/?id=1469" rel="nofollow">Bibliothek der Jadehochschule Wilhelmshaven</a></td>
        <td>in 1.5 <i>(2013-04-26)</i></td> <td>?</td><td>?</td><td>pica</td></tr>

</table>
<br>

? in der Tabelle heißt, dass mir dort keine Ausleihkarte oder verlängerbare Medien zur Verfügung standen, und ich es nicht direkt testen konnte. Wenn es das gleiche System wie eine oben mit "ja" markierte Bibliothek verwendet, funktioniert es aber vermutlich trotzdem.

<br><br>

Man kann es aber auch mit anderen Bibliotheken benutzen, wenn man dort eine Ausleihkarte hat. Dazu ist es nicht einmal nötig VideLibri umzuprogrammieren, da VideLibri auf einem Templatesystem basiert. Um ein Template für eine neue Bibliothek hinzuzufügen, speichert man im wesentlichen jede Seite des WebOPACs ab, und markiert die Stellen, die Medieninformationen wie Autor/Titel enthalten, mit semantischen Annotationen. Wie genau  das funktioniert, ist in der <a href="http://videlibri.hg.sourceforge.net/hgweb/videlibri/videlibri/raw-file/tip/programs/internet/VideLibri/_meta/help/neuebibliothek.html">Hilfe</a> beschrieben. Programmierkenntnisse sind dabei nur erforderlich, wenn der Katalog irgendwelche Merkwürdigkeiten, wie beispielsweise ungültiges HTML oder einen komplizierten XSS-Schutz, aufweist.<br>
Verwendet die Bücherei ein aktuelles aleph/libero/sisis/pica-System ist es nicht mal nötig ein Template zu schreiben, weil man dann einfach die Serveradresse in die xml-Dateien im data/libraries Verzeichnis eintragen kann. 
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
Damit sollte es unter Windows/Linux/Mac mit gtk/qt/win32/cocoa-Interface laufen (getestet mit Window/Linux und gtk/qt/win32). Man benötigt aber mindestens Lazarus 1.0 und FPC 2.6.0.
<br>
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
Autor: Benito van der Zander, <a href="benito_NOSPAM_benibela.de">benito_NOSPAM_benibela.de</a>, <a href="http://www.benibela.de/index_en.html">www.benibela.de</a><br>
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
