module namespace vl = "http://www.videlibri.de";
declare function vl:delete-current-books() { 
  books-deleted := true() 
}; 
declare function vl:raise($x) { 
  raised := $x
}; 
declare function vl:raise-login($x) { 
  vl:raise("login error: " || $x)
}; 
declare function vl:raise-login() { 
  vl:raise("login error")
}; 
declare function vl:raise-timeout($x) { 
  vl:raise("Timeout: " || $x)
}; 
declare function vl:raise-internal($x) { 
  vl:raise($x)
}; 
declare function vl:choose($a,$b,$c,$d) { 
  $message := {"kind": "choose", "callback": $a, "caption": $b, "options": $c, "option-values": $d }
};
declare function vl:confirm($a,$b) { 
  $message := {"kind": "confirm", "callback": $a, "caption": $b }
};
declare function vl:alert($a,$b) { 
  $message := {"kind": "confirm", "callback": $a, "caption": $b }
};
declare function vl:alert($a) { vl:alert("", $a) };
declare function vl:set-book-property($a,$b) { 
  $book($a||'!!') := $b 
};
declare function vl:select-book($q) { 
  $q
};
