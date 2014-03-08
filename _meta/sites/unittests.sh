#!/bin/bash

TEMPLATEPARSER="../../../xidel/xidel"
TEMPLATEPARSERARGS="--dot-notation=on --extract=\"book:=object()\" --print-type-annotations  --extract-kind=template"
TEMPLATEPATH=../../data/libraries/templates
INPATH=./
OUTPATH=/tmp/

if [ "$1" = "--generate" ]; then
echo "GENERATING"
OUTPATH=./
fi

function ADDTEMPLATE {
  TEMP=$1
  COUNT=$2
  for ((i=0;$i<$COUNT;i++)); do   
    TEMPLATES=(${TEMPLATES[@]} $TEMP) 
  done
}


#=============WAS==============
mkdir -p $OUTPATH/wasnrw
TEMPLATES=(wasnrw/start wasnrw/KontoServlet wasnrw/KontoServlet wasnrw/KontoServlet)
PAGES=(wasnrw/start.html wasnrw/BenutzerkontoServlet_books.html wasnrw/BenutzerkontoServlet_books2.html wasnrw/BenutzerkontoServlet_books3.html)

#==============Primo========
mkdir -p $OUTPATH/primo
ADDTEMPLATE primo/searchList{'baseurl:=\"http://example.org/\"'} 5
PAGES=(${PAGES[@]} primo/searchList.hub.html primo/searchList0.hub.html primo/searchListWait.hub.html primo/searchList.fub.html primo/searchList.tub.html)


#=============ALEPH ULBD==============
mkdir -p $OUTPATH/ulbdue
TEMPLATES=(${TEMPLATES[@]} ulbdue/start ulbdue/login aleph_base/loggedIn2005 aleph_base/details ulbdue/update ulbdue/update)
PAGES=(${PAGES[@]} ulbdue/start.html ulbdue/login.html ulbdue/loggedIn.html ulbdue/details_1.html ulbdue/update_singlebook.html ulbdue/update_empty.html)

#=============ALEPH TU/UdK Berlin========
mkdir -p $OUTPATH/aleph_ubTUb
TEMPLATES=(${TEMPLATES[@]} aleph_base/loggedIn2007 aleph_ubTUb/update aleph_base/details)
PAGES=(${PAGES[@]} aleph_ubTUb/loggedIn.html aleph_ubTUb/update.html aleph_ubTUb/details.html)

#=============ALEPH HU Berlin============
mkdir -p $OUTPATH/aleph_ubHUb
TEMPLATES=(${TEMPLATES[@]} aleph_base/loggedIn2005 aleph_base/details aleph_base/details aleph_base/details aleph_ubHUb/update)
PAGES=(${PAGES[@]} aleph_ubHUb/loggedIn.html aleph_ubHUb/details1ex.html aleph_ubHUb/details2ex.html aleph_ubHUb/details3ex.html aleph_ubHUb/update-1.html)


#=============ALEPH FU Berlin============
mkdir -p $OUTPATH/aleph_ubFUb
TEMPLATES=(${TEMPLATES[@]} aleph_base/details aleph_ubFUb/update aleph_ubFUb/update aleph_ubFUb/update aleph_base/loggedIn2007)
PAGES=(${PAGES[@]} aleph_ubFUb/details.html aleph_ubFUb/update-2books.html aleph_ubFUb/update-0books.html aleph_ubFUb/update-6books.html aleph_ubFUb/loggedIn.html)

#============BIBDIA StaBib===============
mkdir -p $OUTPATH/bibdia_stabib
TEMPLATES=(${TEMPLATES[@]} bibdia_stabib/update bibdia_stabib/update bibdia_stabib/fastUpdate bibdia_stabib/update pica/searchDetails pica/searchDetails pica/searchDetails) 
PAGES=(${PAGES[@]} bibdia_stabib/list.html bibdia_stabib/list_abhol.html bibdia_stabib/list_f.html bibdia_stabib/list_empty.html bibdia_stabib/stabikat1.html bibdia_stabib/stabikat2.html bibdia_stabib/stabikat3.html)

#=============LIBERO==============
mkdir -p $OUTPATH/libero5

TEMPLATES=(${TEMPLATES[@]} libero5/start libero5/update libero5/update libero5/update libero5/update)
PAGES=(${PAGES[@]} libero5/start.html libero5/update_empty.html libero5/update55sp73_empty.html libero5/update55sp4_2books.html libero5/update55sp73_books.html)

#=============SISIS==============
mkdir -p $OUTPATH/sisis


ADDTEMPLATE sisis/start 2
PAGES=(${PAGES[@]} sisis/start.do.html sisis/start_schweinfurt.do.html)

ADDTEMPLATE sisis/loggedIn 4
PAGES=(${PAGES[@]} sisis/userAccount.do_empty.html sisis/userAccount.do_singlebook.html sisis/userAccount.do_singlebook2.html sisis/userAccount.do_2books.html)

#search
ADDTEMPLATE sisis/searchList 17
PAGES=(${PAGES[@]} sisis/search_rwth.do.html sisis/searchHitlist_rwth.do.html sisis/search_altdorf.do.html sisis/searchHitList_altdorf.do.html sisis/search_Z3988_haw-aw.do.html sisis/search_augsburg.do.html sisis/search_augsburg2.do.html sisis/search_basel.do.html sisis/search_chemnitz.do.html sisis/search_liestal.do.html sisis/search_tum.do.html sisis/search_pulheim.do.html sisis/searchHitlist_pulheim.do.html sisis/search_winterthur.do.html sisis/search_amberg1.html sisis/search_rwth1.html  sisis/search_last_fuerth.do.html)  

ADDTEMPLATE sisis/searchSingle 3
PAGES=(${PAGES[@]} sisis/searchSingle_basel.do.html sisis/searchSingle_aachen.do.html sisis/searchSingle_regensburg.html)

ADDTEMPLATE sisis/orderConfirmation 2
PAGES=(${PAGES[@]} sisis/orderConfirmation_aachen.html sisis/orderConfirmation_pulheim.html  )

ADDTEMPLATE 'sisis/singleExtended{b:={\"id\":123}}' 1
PAGES=(${PAGES[@]} sisis/singleExtended_h.html  )

ADDTEMPLATE sisis/searchSingleExemplar 15
PAGES=(${PAGES[@]} sisis/searchSingleExemplar_aachen.html  sisis/searchSingleExemplar_altdorf.html  sisis/searchSingleExemplar_altdorf2.html  sisis/searchSingleExemplar_altdorf3.html   sisis/searchSingleExemplar_amberg.html   sisis/searchSingleExemplar_amberg2.html  sisis/searchSingleExemplar_amberg3.html   sisis/searchSingleExemplar_augsburg.html  sisis/searchSingleExemplar_basel.html  sisis/searchSingleExemplar_chemnitz.html   sisis/searchSingleExemplar_dortmund.html  sisis/searchSingleExemplar_dortmund2.html  sisis/searchSingleExemplar_dortmund3.html  sisis/searchSingleExemplar_pulheim.html  sisis/searchSingleExemplar_pulheim2.html )

ADDTEMPLATE sisis/searchSingleAvailability 15

PAGES=(${PAGES[@]} sisis/searchSingleAvailability_aachen.html sisis/searchSingleAvailability_altdorf.html sisis/searchSingleAvailability_altdorf2.html sisis/searchSingleAvailability_altdorf3.html sisis/searchSingleAvailability_amberg.html sisis/searchSingleAvailability_amberg2.html sisis/searchSingleAvailability_amberg3.html sisis/searchSingleAvailability_augsburg.html sisis/searchSingleAvailability_basel.html sisis/searchSingleAvailability_chemnitz.html sisis/searchSingleAvailability_dortmund.html sisis/searchSingleAvailability_dortmund2.html sisis/searchSingleAvailability_dortmund3.html sisis/searchSingleAvailability_pulheim.html sisis/searchSingleAvailability_pulheim2.html )


#==============PICA================
mkdir -p $OUTPATH/pica

ADDTEMPLATE pica/update 3
PAGES=(${PAGES[@]} pica/update3.html pica/update_abholen.html pica/update_luebeck.html)

ADDTEMPLATE pica/extend 1
PAGES=(${PAGES[@]} pica/extend2.html)

#=============ADISWEB==============
mkdir -p $OUTPATH/aDISWeb


ADDTEMPLATE aDISWeb/accountOverview 4
PAGES=(${PAGES[@]} aDISWeb/accountOverview_munich.html aDISWeb/accountOverview_nurnberg.html aDISWeb/accountOverview_nurnberg.fake.html aDISWeb/accountOverview_testhdm.html)

ADDTEMPLATE aDISWeb/list 7
PAGES=(${PAGES[@]} aDISWeb/list_munich.html aDISWeb/list_nurnberg.html aDISWeb/list_testhdm.html aDISWeb/list_testhdm2.html  aDISWeb/list_provided_testhdm.html aDISWeb/list_requested_munich.html aDISWeb/list_requested_testhdm.html)


ADDTEMPLATE aDISWeb/search 7
PAGES=(${PAGES[@]} aDISWeb/search_stuttgart_abk.do.html aDISWeb/search_no-result.aalen.html aDISWeb/search_no-result.voebb.html aDISWeb/search_single-page.biberach.html aDISWeb/search_go-back.freiburg.html aDISWeb/search_single-result_hdmtest.html aDISWeb/search_single-result_hdmtest2.html)

ADDTEMPLATE aDISWeb/searchDetails 4
PAGES=(${PAGES[@]} aDISWeb/searchDetails_albstadt.html aDISWeb/searchDetails_heilbronn.html aDISWeb/searchDetails_heilbronn2.html aDISWeb/searchDetails_single-result_hdmtest.html)

ADDTEMPLATE aDISWeb/connected 3
PAGES=(${PAGES[@]} aDISWeb/connected_furtwangen.html aDISWeb/connected_voebb1.html aDISWeb/connected_voebb2.html)

ADDTEMPLATE aDISWeb/searchInputForm 1
PAGES=(${PAGES[@]} aDISWeb/searchInputForm_mannheim-hsb.html )

#==============Zones========
mkdir -p $OUTPATH/zones18
ADDTEMPLATE zones18/loggedIn 1
PAGES=(${PAGES[@]} zones18/loggedIn.html)

ADDTEMPLATE 'zones18/list{requestId:=0}' 2
PAGES=(${PAGES[@]} zones18/list.html zones18/list10.html)

ADDTEMPLATE 'zones18/list{requestId:=1}' 1
PAGES=(${PAGES[@]} zones18/listIncremental.html)

ADDTEMPLATE zones18/bulkRenew 2 
PAGES=(${PAGES[@]} zones18/bulkRenew.html zones18/bulkRenew2.html)

ADDTEMPLATE zones18/listOrders 6
PAGES=(${PAGES[@]} zones18/orderList1.html zones18/orderList1unterwegs.html zones18/orderList2.html zones18/orderList2canceled.html zones18/orderList3+canceled.html zones18/orderList3+canceled_real.html )

ADDTEMPLATE zones18/cancelConfirm 1
PAGES=(${PAGES[@]} zones18/cancelConfirm.html)

ADDTEMPLATE 'zones18/searchList{requestId:=0}' 6
PAGES=(${PAGES[@]} zones18/searchList.biel.html zones18/searchList.cologne.html zones18/searchList.hannover.html zones18/searchList.kiel.html zones18/searchList.koblenz.html zones18/searchList_notfound.html)


ADDTEMPLATE 'zones18/searchList{requestId:=1}' 5
PAGES=(${PAGES[@]} zones18/searchListIncremental.biel.html zones18/searchListIncremental.cologne.html zones18/searchListIncremental.hannover.html zones18/searchListIncremental.kiel.html zones18/searchListIncremental.koblenz.html)

ADDTEMPLATE zones18/searchDetails 7
PAGES=(${PAGES[@]} zones18/searchDetails.html zones18/searchDetailsCollection.html zones18/searchDetailsSubTitle.html zones18/searchDetails.biel.html zones18/searchDetails.cologne.html zones18/searchDetails.hannover.html zones18/searchDetails.koblenz.html)


ADDTEMPLATE zones18/orderConfirmation{vl:choose} 1
PAGES=(${PAGES[@]} zones18/orderConfirm.html)


#=============DIGIBIB==============
mkdir -p $OUTPATH/digibib

DISE=../search/templates/digibib/search
DIDE=../search/templates/digibib/details
TEMPLATES=(${TEMPLATES[@]} $DISE $DISE $DIDE $DIDE $DIDE $DIDE $DIDE)
PAGES=(${PAGES[@]} digibib/search.html digibib/search2.html digibib/details.html digibib/details2.html digibib/details3.html digibib/details4.html digibib/details5.html)  

error=0
prev_system=
for ((i=0;i<${#TEMPLATES[@]};i++)); do
  [[ ${TEMPLATES[i]} =~ (.*/)?([^/]+)/[^/]+ ]]
  system=${BASH_REMATCH[2]}
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
    if [[ $EXTRA =~ vl:choose ]]; then
      EXTRA="-e \"declare function vl:choose(\\\$a,\\\$b,\\\$c,\\\$d) { message-choose :=  join((\\\$a,\\\$b,\\\$c,\\\$d))}\""
    fi
#    if [[ $EXTRA =~ vl:raise ]]; then
#      EXTRA="-e \"declare function vl:raise(\\\$x) { raised := \\\$x }\""
#    fi
  fi

  
  eval $TEMPLATEPARSER  $EXTRA $TEMPLATEPARSERARGS $INPATH/${PAGES[i]} --extract-file=$TFILE  > $OUTPATH/${PAGES[i]}.result 2> $OUTPATH/stderr
  if diff -q $INPATH/${PAGES[i]}.result $OUTPATH/${PAGES[i]}.result; then tempasasas=42; else 
    echo
    echo -n ERROR: 
    echo -e "when testing: ${TEMPLATES[i]} \t\t\twith\t\t $INPATH/${PAGES[i]}"
    echo $TEMPLATEPARSER  $EXTRA $TEMPLATEPARSERARGS $INPATH/${PAGES[i]} --extract-file=$TFILE
    error=1;     
    cat $OUTPATH/stderr
    git diff --color-words $INPATH/${PAGES[i]}.result $OUTPATH/${PAGES[i]}.result; 
  fi
done;

echo
echo

echo $error
if [[ error -ne 0 ]]; then echo -----ERROR\!\!\!-----; fi

#$TEMPLATEPARSER$TEMPLATES/wasnrw/start $INPAGES/was/start.html > $OUTPAGES/was/start
#$TEMPLATEPARSER$TEMPLATES/wasnrw/KontoServlet $INPAGES/wasnrw/BenutzerkontoServlet_books.html $OUTPAGES/was/BenutzerkontoServlet_books.ht


