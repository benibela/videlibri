xquery version "3.1-xidel";
module namespace igp="igp";

declare function igp:error($message, $info){
  error(QName("interface"), join(($message, $info)))
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
    case "Int" return ["int", ()]
    case "Long" return ["long", ()]
    case "Double" return ["double", ()]
    case "Boolean" return ["boolean", ()]
    case "" return igp:error("empty type")
    default return ["classref", attribute ref {$type}]
  return element {$type2(1)} { $attrib, $type2(2) }
  
};

declare function igp:make-function($f){
  let $split := extract($f, "fun ([^(]+)\((.*)\)(:(.*))?", (1,2,4))!normalize-space()
  return
  <function id="{$split[1]}">
    {tokenize($split[2], ",")[normalize-space()]!<arg name="{substring-before(., ":")=>normalize-space()}">{igp:make-type(substring-after(., ":")=>normalize-space(), ())}</arg>,
    if (exists($split[3][.])) then <return-type>{igp:make-type($split[3], ())}</return-type> else ()}
  </function>
};

declare function igp:annotation-to-attribute($a){
  switch ($a) 
    case "@Kotlin->Pascal" return attribute jvm-pascal {} 
    case "@Pascal->Kotlin" return attribute pascal-jvm {} 
    case "@Kotlin<->Pascal" case "@Pascal<->Kotlin" return (attribute jvm-pascal {}, attribute pascal-jvm {} )
    default return igp:error("invalid annotation", $a)
};

declare function igp:make-class($annotations, $nameAndParent, $children){
  let $nameAndParent := extract($nameAndParent, "^([^:\s]+)( *: *([^{]+))?$", (1,3))!normalize-space()
  return
    <class id="{$nameAndParent[1]}">{
 if ($nameAndParent[2]) then attribute extends {$nameAndParent[2]} else (),
  for $a in $annotations return
    switch ($a) 
      case "@SerializeJson" return attribute serialize-json {} 
      case "@KotlinVar" return attribute kotlin-var {"var"} 
      case "@KotlinDataClass" return attribute kotlin-class {"data"} 
      case "@PascalClass" return attribute pascal-type {"class"} 
      default return igp:annotation-to-attribute($a),
  for $w in $children
  let $x := $w!translate(., " ", "") 
  let $split := extract($x, "([^:]+):([^=]+)(=\s*(.*))?", (1,2,4) )
  let $name := $split[1]!normalize-space()
  let $type := $split[2]!normalize-space()
  let $default := $split[3]
  let $default := $default!(if ($type = "String") then replace(., '^"|"$', '') else .) 
  return if (starts-with($x, "//")) then <comment>{$w!replace(., "^//", "")}</comment>
  else igp:make-type($type, (attribute name { $name }, $default[.] ! attribute default { . } ) )
}
  </class>
};

declare function igp:make-enum($annotations, $name, $children){
  <intenum id="{$name}">{
    $annotations ! (
      if (starts-with(., "@PascalPrefix")) then attribute pascal-prefix { extract(., "\(\s*([a-zA-Z]+)", 1) }
      else igp:annotation-to-attribute(.)
    ),
    for $c in $children ! normalize-space(.)[.] 
    let $hasValue := contains($c, "=")
    let $n := if ($hasValue) then substring-before($c, "=") => normalize-space() else $c
    let $v := if ($hasValue) then substring-after($c, "=") => normalize-space() else ()
    return <value name="{$n}">{$v ! attribute value {.} }</value>
    
  }</intenum>
};

declare function igp:make($input) {
  let $firstPass := 
    <api>
    <!-- This file has been generated automatically. Do not edit it, do not read it.
         Refer to the corresponding .pretty file
    -->
    {
    for tumbling window $w in x:lines($input)!normalize-space() 
       start $s when $s ne "" 
       end $e next $f when ( $e eq "}" or starts-with($s, "fun ") )
                         or (starts-with($s, "//") and not(starts-with($f, "//")))
                            
    return 
    if (starts-with($s, "//")) then <comment>{$w!replace(., "^//", "")}</comment>
    else if (starts-with($s, "fun")) then igp:make-function($w)
    else 
      let $annotations := $w[starts-with(., "@")]
      let $typeName := extract($w[count($annotations) + 1], "([a-z]+)\s+([^{]+)\{", (1,2))!normalize-space()
      let $children := subsequence($w, count($annotations) + 2, count($w) - count($annotations) - 2)
      return switch ($typeName[1]) 
        case "class" return igp:make-class($annotations, $typeName[2], $children)
        case "enum" return igp:make-enum($annotations, $typeName[2], $children)
        default return ""
    } 
    </api>
  let $enumTypes := $firstPass//intenum/@id!string()
  return x:replace-nodes( $firstPass, $firstPass//classref, function($ref){
    if ($ref/@ref = $enumTypes) then <intenumref>{$ref/@*,$ref/*}</intenumref>
    else $ref
  } )
};
