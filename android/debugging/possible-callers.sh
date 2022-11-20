#!/bin/bash

#symbol to start from
export leaf=fpc_pchar_to_ansistr

#distance between caller and symbol
export level=4

libraryso=liblclapp.so

#create dump
if [[ ! -e lclappdump ]]; then
  aarch64-linux-gnu-objdump -j .text -D libraryso > lclappdump
fi

#print caller~callee
if [[ ! -e lclappdump.trace ]]; then
  cat lclappdump | grep '<' \
  | sed -e 's/^[^<]*//' \
  | sed 's:<\([^+]*\)[^>]*>\( *//.*\)\?:\1:' \
  | awk 'BEGIN { FS = ":" } \
         NF>1 { w=$1; } \
         NF==1 && w != $1 { print w "~" $0 }' \
  | sort -u > lclappdump.trace
fi



xidel lclappdump.trace --variable leaf,level -e '
  declare function local:print($callers, $leaf, $indent, $level){
    if ($level = 0) then ()
    else
    let $parents := $callers?$leaf
    let $indent2 := $indent || "    "
    let $level2 := $level - 1
    for $p in $parents 
    return ($indent || $p, 
            local:print($callers, $p, $indent2, $level2), "")
  };

  let $lines := x:lines($raw)
  let $callers := map:merge(
                    for $l in $lines 
                    let $split := tokenize($l, "~")
                    where $split[1] 
                    return { $split[2]: $split[1] }
                  , {"duplicates": "combine"}
                  )
  for $level in 1 to xs:integer($level) return
  ("LEVEL: " || $level,
  local:print($callers, $leaf, "", $level),
  "","","-----------------------------",""
  )
'

