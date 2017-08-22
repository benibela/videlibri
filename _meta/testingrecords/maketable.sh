#!/bin/sh

xidel --xquery ' 
  declare variable $text-fixes := {"url": "Anpassung an neue OPAC-Webadresse.", "css": "Anpassung an geändertes CSS-Design auf der Bibliothekswebseite."};
  declare variable $texts := {"no-reply": "Bibliothek hat auf Nachfrage nicht reagiert."};
  declare function local:unescape($s) { replace(replace(replace(replace($s, "[+]sz", "ß"), "[+]ae", "ä"), "[+]oe", "ö"), "[+]ue", "ü") };
  declare function local:test-result($kind, $status) {
    let $ok := $status = "yes"
    return 
      <span class="{$kind, "ok"[$ok]}">{
        if ($ok) then "ja" 
        else if (count($status) = 0) then "?"
        else if ($status = "no") then "nein"
        else "nein"||$status
     }</span>
  };
  <html>
  <head>    <link rel="stylesheet" type="text/css" href="test.css"/>   </head>
  <body>
  <div class="content">
  <div id="testing-table"> <div>
    <span class="libname test-header">Name der Bibliothek</span>
    <span class="search test-header">Suche funktioniert</span>
    <span class="account test-header">Kontozugriff funktioniert</span>
    </div>
  {
  let $alltests := file:list(".", true(), "*.xml")!doc(.)!tests!test
  let $librarypath := "../../data/libraries/"
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
      return
      <div>
        {let $ordered-tests := (for $test in $test let $date := $test/@date order by $date return $test)
         return (
           <div><span class="libname"><h4>{$name}</h4></span>{
           local:test-result("search", ($ordered-tests/@search)[last()]),
           local:test-result("account", ($ordered-tests/@account)[last()])}</div>,
           <div class="log">{
             for $test in $ordered-tests 
             return <div>
               <span class="date">{$test/@date/data()}</span>
               { $test/(
               @fix ! <span class="fix">{$text-fixes(.)}</span>,
                 @search ! local:test-result("search", . ),
                 @account ! local:test-result("account", .),
                 @text ! <span class="text">{($texts(.), .)[1]}</span>
               )}
             </div>
           }</div>
        )}
      </div>
    }
  </div>}</div></div>
  </body></html>
  
' --input-format xml --output-format html

#testing-table .date {float: left; width: 30%; padding-left: 2em}
#testing-table .libname {float: left; width: 30%}
#testing-table .search {float: left; width: 30%}
#testing-table .account {float: left; width: 30%}
#testing-table h4 { display: inline }

#testing-table div {overflow: auto}

.content { 
  max-width: 60em;
  margin-left: auto;
  margin-right: auto;
}

/** {border: 1px solid red}*/