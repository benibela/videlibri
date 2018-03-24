module namespace vl = "http://www.videlibri.de";
declare function vl:delete-current-books() { 
  books-deleted := true() 
}; 
declare function vl:raise($x) { 
  raised := $x
}; 
declare function vl:raise-login($x) { 
(:  vl:raise("login error:" || $x):)
  vl:raise($x)
}; 
declare function vl:raise-internal($x) { 
  vl:raise($x)
}; 
declare function vl:choose($a,$b,$c,$d) { 
  message-choose :=  join(($a,$b,$c,$d))
};
declare function vl:confirm($a,$b) { 
  message-confirm := join(($a,$b))
};
declare function vl:set-book-property($a,$b) { 
  $book($a||'!!') := $b 
};
