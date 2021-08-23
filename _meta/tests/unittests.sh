#!/bin/bash
DIR="$( cd "$( dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")" )" && pwd )"
VLDIR="$DIR/../../"
if xidel -e 1 2>/dev/null; then TEMPLATEPARSER="xidel"
else TEMPLATEPARSER="$VLDIR/../xidel/xidel"; fi
TEMPLATEPARSERARGS="--deprecated-trim-nodes --output-key-order ascending --dot-notation=on --extract=\"book:=map{}\" --print-type-annotations  --extract-kind=xml-pattern --module $VLDIR/mockvidelibri.xqm --xmlns:videlibri http://www.videlibri.de "
TEMPLATEPATH=$VLDIR/data/libraries/templates
INPATH=$DIR/
OUTPATH=/tmp/
GENERATING=

#command line reading from https://stackoverflow.com/a/39376824/1501222
OPTS=$(getopt -o "" --long "inpath:,tmppath:,generate" -n "$0" -- "$@")
if [ $? != 0 ] ; then echo "Error in command line arguments." >&2 ; exit 1 ; fi
eval set -- "$OPTS"

while true; do
  case "$1" in
    --inpath ) INPATH="$2"; shift 2 ;;
    --tmppath ) OUTPATH="$2"; shift 2 ;;
    --generate ) GENERATING=true; shift ;;
    -- ) shift; break ;;
    * ) break ;;
  esac
done

FILTER=$1

if [[ -n "$GENERATING"  ]]; then
echo "GENERATING"
OUTPATH=$INPATH
fi


function testCrossDir {
  system=$1; shift
  TEMP="$system/$1"; shift
  pagesDir=$1; shift
  if [[ $# -eq 0 ]]; then error="missing test cases $TEMP"; echo $error;  exit; fi
  for page in "$@"; do 
    TEMPLATES=(${TEMPLATES[@]} "$TEMP") 
    PAGES=(${PAGES[@]} "$pagesDir/$page")
  done
 
}
function test {
  system=$1; shift
  pattern="$1"; shift
  testCrossDir "$system" "$pattern" "$system" "$@"
}

TEMPLATES=()
PAGES=()

source "$INPATH/testcases.inc.sh"

#echo $OUTPATH/stderr
#echo > $OUTPATH/stderr
error=0
prev_system=
for ((i=0;i<${#TEMPLATES[@]};i++)); do
  #echo '#####>-----------------------RESET--------------------<#####'  > $OUTPATH/stderr
  [[ ${TEMPLATES[i]} =~ ([^{]*/)?([^/{]+)/[^/]+ ]]
  system=${BASH_REMATCH[2]}
  if [[ ! "$system" =~ $FILTER ]]; then     continue; fi
  if [[ "$system" != "$prev_system" ]]; then
    echo
    echo -ne "$system: \t."
    prev_system=$system
  else echo -n .;  fi
  
  TFILE="$TEMPLATEPATH/${TEMPLATES[i]}"
  EXTRA=
  if [[ $TFILE =~ ([^{]+)[{](.+)[}] ]]; then
    TFILE=${BASH_REMATCH[1]}
    EXTRA="-e ${BASH_REMATCH[2]}"
  fi

  #echo $TEMPLATEPARSER $EXTRA $TEMPLATEPARSERARGS $INPATH/${PAGES[i]} --extract-file=$TFILE  
  if ! eval $TEMPLATEPARSER $EXTRA $TEMPLATEPARSERARGS $INPATH/${PAGES[i]} --extract-file=$TFILE  > $OUTPATH/${PAGES[i]}.result 2> $OUTPATH/stderr; then
    echo EXCEPTION >> $OUTPATH/${PAGES[i]}.result
    cat $OUTPATH/stderr >> $OUTPATH/${PAGES[i]}.result
  fi
  
  if diff -q $INPATH/${PAGES[i]}.result $OUTPATH/${PAGES[i]}.result; then tempasasas=42; else 
    echo
    echo -n ERROR: 
    echo -e "when testing: ${TEMPLATES[i]} \t\t\twith\t\t $INPATH/${PAGES[i]}"
    echo $TEMPLATEPARSER $FUNCTIONS $EXTRA $TEMPLATEPARSERARGS $INPATH/${PAGES[i]} --extract-file=$TFILE  
    error=1;     
    cat $OUTPATH/stderr
    git diff --color-words $INPATH/${PAGES[i]}.result $OUTPATH/${PAGES[i]}.result; 
  fi
done;

echo
echo

echo $error
if [[ error -ne 0 ]]; then echo -----ERROR\!\!\!-----; exit 2; fi

#$TEMPLATEPARSER$TEMPLATES/wasnrw/start $INPAGES/was/start.html > $OUTPAGES/was/start
#$TEMPLATEPARSER$TEMPLATES/wasnrw/KontoServlet $INPAGES/wasnrw/BenutzerkontoServlet_books.html $OUTPAGES/was/BenutzerkontoServlet_books.ht


