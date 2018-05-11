#!/bin/sh

xidel --xquery ' 
  declare variable $librarypath := "../../data/libraries/";
  declare variable $testingpath := "../testingrecords/";
  declare variable $text-fixes := {"url": "Anpassung an neue OPAC-Webadresse.", "css": "Anpassung an geändertes CSS-Design auf der Bibliothekswebseite."};
  declare variable $texts := {"no-reply": "Bibliothek hat auf Nachfrage nicht reagiert."};
  declare function local:unescape($s) { replace(replace(replace(replace(replace($s, "[+]sz", "ß"), "[+]ae", "ä"), "[+]oe", "ö"), "[+]ue", "ü"), "[+][+]", " ") };
  declare function local:test-result($kind, $status, $append) {
    let $ok := $status = "yes"
    return 
      <span class="{$kind, "ok"[$ok]}">{
        if ($ok) then "ja" 
        else if (count($status) = 0) then "?"
        else if ($status = "?") then "?"
        else if ($status = "no") then "nein"
        else "nein"||$status,
        if (exists($append)) then (<br/>, <span class="time-line">({"ja: "[not($ok)]}{$append})</span>) else ()
     }</span>
  };
  declare function local:format-date($date){
    join( reverse(tokenize($date, "-")), ".")
  };
  declare function local:make-offset($date){
    if ($date) then
      xs:string(xs:date($date) + xs:dayTimeDuration("P15D"))
    else 
      $date
  };
  declare variable $systems := let $raw := {
    "aDISWeb":         {"name": "aDIS/BMS", "search": "2013-05-17", "account": "2012-10-03"},
    "aleph_base":      {"name": "Aleph"},
    "aleph_ubFUb":     {"name": "Aleph (der FU-Berlin)", "account": "2011-03-07"},
    "aleph_ubTUb":     {"name": "Aleph (der TU-Berlin)", "account": "2011-03-07"},
    "aleph_ubHUb":     {"name": "Aleph (der HU-Berlin)", "account": "2011-03-07"},
    "ulbdue":          {"name": "Aleph (der Universitätsbibliothek Düsseldorf)", "account": "2006-08-10"},
    "bibliotheca":     {"name": "Bibliotheca", "search": "2014-08-19", "account": "2014-08-19"}, 
    "bibliothecaplus": {"name": "Bibliotheca+/OPEN", "search": "2014-09-13", "account": "2014-09-13"},
    "digibib":         {"name": "Digibib", "search": "2008-07-19"},
    "libero5":         {"name": "Libero 5", "search": "2013-07-01", "account": "2006-12-19"},
    "paia":            {"name": "PAIA", "account": "2013-03-22"},
    "primo":           {"name": "Primo", "search": "2013-07-10", "account": "2013-03-19"},
    "sru":             {"name": "SRU", "search": "2013-05-23"},
    "wasnrw":          {"name": "der Stadtbüchereien Düsseldorf", "account": "2006-08-06"},
    "bibdia":          {"name": "Biber Bibdia mit PICA)", "search": "", "account": "2018-03-15"},
    "bibdia_stabib":   {"name": "Stabikat (Bibdia mit PICA)", "search": "", "account": "2011-03-07"},
    "pica":            {"name": "PICA", "search": "2013-04-26", "account": "2011-04-23"},
    "lbs":             {"name": "PICA mit LBS", "search": "2014-07-31", "account": "2014-07-31"},
    "netbiblio":       {"name": "Netbiblio", "search": "2017-01-22", "account": "2017-01-22"},
    "sisis":           {"name": "SISIS-SunRise", "search": "2013-04-25", "account": "2010-02-16"},
    "summon":          {"name": "Summon", "search": "2015-11-22", "account": "2015-11-22"},
    "koha":            {"name": "Koha", "search": "2018-01-04", "account": "2018-01-04"}, 
    "vufind":          {"name": "VuFind", "account": "2018-02-11"},
    "zones18":         {"name": "Zones 1.8", "search": "2013-10-07", "account": "2013-10-07"}
  } return {| for $k in jn:keys($raw) let $o := $raw($k) return {$k: {"name": $o("name"), "account": local:make-offset($o("account")), "search": local:make-offset($o("search")), "origin": ($o("account"), $o("search"))[1]  }  } |};
  (:<html>
  
  <head>   <meta  charset="utf-8"/> <link rel="stylesheet" type="text/css" href="test.css"/>   </head>
  <body>
  <div class="content">:)
  <div id="testing-table"> <div class="header-row">
    <span class="libname test-header"><b>Stadt / Name der Bibliothek</b><br/>Zeitpunkte</span>
    <span class="search test-header"><b>Suche funktioniert</b><br/>(Zeitraum)</span>
    <span class="account test-header"><b>Kontozugriff funktioniert</b><br/>(Zeitraum)</span>
    </div>
  {
  let $alltests := file:list($testingpath, true(), "*.xml")!doc($testingpath || .)!tests!test
  let $libraries := {| file:list($librarypath, false(), "*.xml")!(
    let $lib := doc($librarypath || .)/* return {extract(., "/?([^/]+).xml", 1): {"lib": $lib, "name": replace($lib/longName/@value/data(),"\(.*\)", "")}} ) |} 
  return
  for $test in $alltests
  let $id := $test/@id
  group by $city := local:unescape(tokenize($id, "_")[3])
  order by $city
  return <div class="city">
    <h3> {$city} </h3>
    {
      for $test in $test
      group by $id := $test/@id/data()
      let $name := $libraries($id[1])[1].name
      let $webid := replace($id[1], "[+]", "")
      return
      <div class="library">
        {let $ordered-tests := (for $test in $test let $date := $test/@date order by $date return $test)
         let $basesystemid := tokenize($ordered-tests[1]!@system, " ")
         let $basesystem := $systems(tokenize($basesystemid, " ")[. != "digibib"])
         return (
           let $search-ok-tests := $ordered-tests[@search="yes"]
           let $search-ok-from := substring-before($search-ok-tests[1]!@date, "-")
           let $search-ok-to := substring-before($search-ok-tests[last()]!@date, "-")
           let $search-ok-text := if ($search-ok-from = $search-ok-to) then $search-ok-from 
                                  else x"{$search-ok-from} - {$search-ok-to}" 
           let $account-ok-tests := $ordered-tests[@account="yes"]
           let $account-ok-from := substring-before($account-ok-tests[1]!@date, "-")
           let $account-ok-to-pre := substring-before($account-ok-tests[last()]!@date, "-")
           let $account-ok-to := if ($account-ok-to-pre < $search-ok-to) then
                                    if ($account-ok-to-pre != $account-ok-from) then $account-ok-to-pre || " - ?" 
                                    else "?"
                                 else $account-ok-to-pre
           let $account-ok-text := if ($account-ok-from = $account-ok-to) then $account-ok-from 
                                  else x"{$account-ok-from} - {$account-ok-to}" 
           return
           <div><a class="libname" id="{$webid}" href="#{$webid}"><h4>{$name}</h4></a>{
           local:test-result("search", ($ordered-tests!@search)[last()],  $search-ok-text),
           local:test-result("account", ($ordered-tests!@account)[last()], $account-ok-text[exists($account-ok-tests)])}</div>,
           <div class="log">{
             <div><span class="date">{local:format-date( $basesystem("origin") )}</span><span class="text">Unterstützung für das Bibliothek-System {$basesystem("name")}.</span> </div>[exists($basesystem)],
             if ( $basesystem("account") < $ordered-tests[1]/@date ) then (
             if (substring-before($basesystem("account"), "-") != substring-before($ordered-tests[1]/@date, "-")) then <br/> else (),
             <div><span class="date">{local:format-date($ordered-tests[1]/@date)}</span><span class="text">Eintragung der "{$name}" in die Liste der das Bibliothekssystem {$basesystem("name")} verwendenden Bibliotheken (um zuvor mit VideLibri auf die "{$name}" zuzugreifen, müsste man zumindest die OPAC-Webadresse eingeben).</span> </div> 
             ) else (),
                
             for $test at $i in $ordered-tests 
             return (
             if (substring-before($ordered-tests[$i - 1]/@date, "-") != substring-before($test/@date, "-")) then <br/> else (),
             let $rows := $test!(
               [@fix ! <span class="text">{$text-fixes(.)}</span>],
               if ($ordered-tests[$i - 1]/@system != $test/@system) then 
                 let $told := tokenize($ordered-tests[$i - 1]!@system, " ")
                 let $tnew := tokenize($test!@system, " ")
                 let $old := $told[. != "digibib"]
                 let $new := $tnew[. != "digibib"]
                 return if ($old != $new) then
                 [<span class="text">Umstellung auf das Bibliothekssystem {$systems($new).name}.</span>]
                 else if (not(($told = "digibib")) and ($tnew = "digibib")  ) then
                 [<span class="text">Suche in der Bibliothek über die Digibib des HBZ.</span>]
                 else ()
               else (),
               [local:test-result("search", @search, () ), local:test-result("account", @account, ())],
               [@comment ! <span class="text">Kommentar der Bibliothek: "{data()}"</span>],
               [@text ! <span class="text">{($texts(.), data())[1]}</span>],
               [text ! <span class="text">{node()}</span>]
              )
             for $row at $i in $rows[exists(.())]
             return 
              <div>
               <span class="date">{if ($i = 1) then  local:format-date($test/@date) else " &#xA0;"}</span>
               {$row()}
             </div>)
           }</div>
        )}
      </div>
    }
  </div>}</div>(:</div>
  </body></html>:)
  
' --input-format xml --output-format xml > testing-table.html
