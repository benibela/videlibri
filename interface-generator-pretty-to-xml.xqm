xquery version "3.1-xidel";
module namespace igp="igp";

declare function igp:error($message, $info){
  error(QName("interface"), join($message, $info))
};
declare function igp:error($message){
  igp:error($message, ())
};

declare function igp:make-type($type, $attrib){
  if (ends-with($type, "[]")) then 
    <array>{$attrib, igp:make-type( substring($type, 1, string-length($type) - 2), ())}</array>
  else let $type2 :=   
  switch($type)
    case "String" return ["string", ()]
(:    case "Int" return ["int", ()]:)
    case "" return igp:error("empty type")
    default return ["classref", attribute ref {$type}]
  return element {$type2(1)} { $attrib, $type2(2) }
  
};

declare function igp:make-function($f){
  let $split := extract($f, "fun ([^(]+)\((.*)\):(.*)", (1,2,3))!normalize-space()
  return
  <function id="{$split[1]}">
    <return-type>{igp:make-type($split[3], ())}</return-type>
  </function>
};

declare function igp:make-class($c){
  let $annotations := $c[starts-with(., "@")]
  let $nameAndParent := extract($c[count($annotations) + 1], "class ([^:\s]+)( *: *([^{]+))? *\{", (1,3))!normalize-space()
  return
    <class id="{$nameAndParent[1]}">{
 if ($nameAndParent[2]) then attribute extends {$nameAndParent[2]} else (),
  for $a in $annotations return
    switch ($a) 
      case "@SerializeJson" return attribute serialize-json {} 
      default return error("a", "invalid annotation"),      
  for $x in subsequence($c, count($annotations) + 2, count($c) - count($annotations) - 2)!translate(., " ", "") return 
    igp:make-type( substring-after($x, ":"), attribute name { substring-before($x, ":") } )
}
  </class>
};

declare function igp:make($input) {
<api>
{
  for tumbling window $w in x:lines($input)!normalize-space() 
     start $s when $s ne "" 
     end $e when $e eq "}" or starts-with($s, "fun ")
  return if (starts-with($s, "fun")) then igp:make-function($w)
  else igp:make-class($w)
}
</api>
};
