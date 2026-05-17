declare function local:selected-ZTEXT($target){
  "selected=ZTEXT       "||$target
};
declare function local:htmlOnLink($html, $target){
  form($html/ancestor::form[1], local:selected-ZTEXT($target)) 
    otherwise vl:raise-internal("Link zur Seite " || $target || " nicht gefunden")
};
declare function local:aDISLink($link, $fallback-target){
  let $href := $link/@href 
  return
  if (starts-with($href, "javascript:") or $href = "#") then 
    let $code := extract($href, "javascript:top\.htmlOnLink\(['""](.*)['""]\)", 1)[.] 
                 otherwise $fallback-target
    return local:htmlOnLink($link, $code)
  else
   $href
};
()