#!/bin/bash
XIDEL=/home/benito/hg/programs/internet/xidel/xidel
 
IFS=
curfile=

declare -a minus
 
 
while read line; do
  case ${line:0:1} in
    d) curfile=$line;
       minus=()
       declare -a minus
       minusidx=0
       read; read; read; ;;
    " ") ;;
    -) minus+=("$line")
     #  echo  ${minus[@]}
     #  echo
     #  echo
       ;;
    
    +)  #echo ${minus[@]}
        #echo $minusidx
        #echo   ?? ${minus[$minusidx]#*{} ??
    #echo vs. ?? $line ??
       $XIDEL -q --xpath3 - <<EOF
         let \$a := {${minus[$minusidx]#*{}
         return let \$b := {${line#*{}
         return let \$res := 
           (: (every \$k in \$a() satisfies deep-equal(\$a(\$k), \$b(\$k))  ) 
           and (every \$k in \$b() satisfies deep-equal(\$a(\$k), \$b(\$k))  )  :)
           (\$a(), \$b()) [not(deep-equal(\$a(.), \$b(.))) ]
         return if (count(\$res) eq 0) then () else ("$curfile", \$res ! x"{.} => {\$a(.)} vs. {\$b(.)}", "")
EOF
       (( minusidx = $minusidx + 1 ))
    ;;
 esac
# echo $line;
done
