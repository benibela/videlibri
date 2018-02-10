#!/bin/bash

TEMPLATEPARSER="../../../xidel/xidel"
TEMPLATEPARSERARGS="--dot-notation=on --extract=\"book:=object()\" --print-type-annotations  --extract-kind=xml-pattern --xmlns:vl videlibri --xmlns:videlibri videlibri"
TEMPLATEPATH=../../data/libraries/templates
INPATH=./
OUTPATH=/tmp/

if [ "$1" = "--generate" ]; then
echo "GENERATING"
OUTPATH=./
FILTER=$2
else FILTER=$1
fi


function ADDTEMPLATE {
  TEMP=$1
  COUNT=$2
  for ((i=0;$i<$COUNT;i++)); do   
    TEMPLATES=(${TEMPLATES[@]} $TEMP) 
  done
}

function test {
  system=$1; shift
  TEMP="$system/$1"; shift
  for page in "$@"; do 
    TEMPLATES=(${TEMPLATES[@]} "$TEMP") 
    PAGES=(${PAGES[@]} "$system/$page")
  done
 
}


#=============WAS==============
mkdir -p $OUTPATH/wasnrw
TEMPLATES=(wasnrw/start wasnrw/KontoServlet wasnrw/KontoServlet wasnrw/KontoServlet)
PAGES=(wasnrw/start.html wasnrw/BenutzerkontoServlet_books.html wasnrw/BenutzerkontoServlet_books2.html wasnrw/BenutzerkontoServlet_books3.html)



#=============ALEPH ULBD==============
mkdir -p $OUTPATH/ulbdue $OUTPATH/aleph_base
TEMPLATES=(${TEMPLATES[@]} ulbdue/start ulbdue/login aleph_base/loggedIn2005 aleph_base/details ulbdue/update ulbdue/update)
PAGES=(${PAGES[@]} ulbdue/start.html ulbdue/login.html ulbdue/loggedIn.html ulbdue/details_1.html ulbdue/update_singlebook.html ulbdue/update_empty.html)
test aleph_base details details.htm

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
TEMPLATES=(${TEMPLATES[@]} bibdia_stabib/update bibdia_stabib/update bibdia_stabib/fastUpdate bibdia_stabib/update bibdia_stabib/update 'pica/searchDetails{\(\)/\(base:=\"\"\)}' 'pica/searchDetails{\(\)/\(base:=\"\"\)}' 'pica/searchDetails{\(\)/\(base:=\"\"\)}') 
PAGES=(${PAGES[@]} bibdia_stabib/list.html bibdia_stabib/list_abhol.html bibdia_stabib/list_f.html bibdia_stabib/list_empty.html bibdia_stabib/list_new.html bibdia_stabib/stabikat1.html bibdia_stabib/stabikat2.html bibdia_stabib/stabikat3.html)

#=============LIBERO==============
mkdir -p $OUTPATH/libero5

test libero5 start start.html
test libero5 update update_empty.html update55sp73_empty.html update55sp4_2books.html update55sp73_books.html update.login.faileden.html update55sp4_2books.fakesaarbrucken.html update6orders.saar.html update.session-lost.html update.verl.html update6_1book_verl.html
test libero5 searchDetails searchDetails_order_verl.html searchDetails_link_bochum.html
test libero5 orderConfirm orderConfirm.verl.html
test libero5 orderComplete orderfail.verl.html ordersuccess.verl.html 

#=============SISIS==============
mkdir -p $OUTPATH/sisis
mkdir -p $OUTPATH/sisis/touchpoint

ADDTEMPLATE sisis/start 3
PAGES=(${PAGES[@]} sisis/start.do.html sisis/start_schweinfurt.do.html sisis/touchpoint/start_chem.html )

ADDTEMPLATE sisis/loggedIn 4
PAGES=(${PAGES[@]} sisis/userAccount.do_empty.html sisis/userAccount.do_singlebook.html sisis/userAccount.do_singlebook2.html sisis/userAccount.do_2books.html )

test sisis "loggedIn{touchpoint:=true\(\)}" touchpoint/userAccount_0provided_chem.html touchpoint/userAccount_0requests_chem.html touchpoint/userAccount_1-10lend_chem.html touchpoint/userAccount_11-20lend_chem.html touchpoint/userAccount_empty_chem.html 


#search
test sisis searchList search_rwth.do.html searchHitlist_rwth.do.html search_altdorf.do.html searchHitList_altdorf.do.html search_Z3988_haw-aw.do.html search_augsburg.do.html search_augsburg2.do.html search_basel.do.html search_chemnitz.do.html search_liestal.do.html search_tum.do.html search_pulheim.do.html searchHitlist_pulheim.do.html search_winterthur.do.html search_amberg1.html search_rwth1.html  search_last_fuerth.do.html search_oberhausen.do.html touchpoint/searchList_1stp_chem.html touchpoint/searchList_2ndp_chem.html touchpoint/searchList_winterthur.html touchpoint/searchList_speedHit_winterthur.html search.empty.aschaffenburg.html 

ADDTEMPLATE sisis/searchSingle 4
PAGES=(${PAGES[@]} sisis/searchSingle_basel.do.html sisis/searchSingle_aachen.do.html sisis/searchSingle_regensburg.html sisis/touchpoint/searchHit_chem.html)

ADDTEMPLATE sisis/orderConfirmation{vl:confirm} 2
PAGES=(${PAGES[@]} sisis/orderConfirmation_aachen.html sisis/orderConfirmation_pulheim.html  )

ADDTEMPLATE 'sisis/singleExtended{b:={\"id\":123}}' 1
PAGES=(${PAGES[@]} sisis/singleExtended_h.html  )

ADDTEMPLATE sisis/searchSingleExemplar 15
PAGES=(${PAGES[@]} sisis/searchSingleExemplar_aachen.html  sisis/searchSingleExemplar_altdorf.html  sisis/searchSingleExemplar_altdorf2.html  sisis/searchSingleExemplar_altdorf3.html   sisis/searchSingleExemplar_amberg.html   sisis/searchSingleExemplar_amberg2.html  sisis/searchSingleExemplar_amberg3.html   sisis/searchSingleExemplar_augsburg.html  sisis/searchSingleExemplar_basel.html  sisis/searchSingleExemplar_chemnitz.html   sisis/searchSingleExemplar_dortmund.html  sisis/searchSingleExemplar_dortmund2.html  sisis/searchSingleExemplar_dortmund3.html  sisis/searchSingleExemplar_pulheim.html  sisis/searchSingleExemplar_pulheim2.html )

ADDTEMPLATE sisis/searchSingleAvailability 15

PAGES=(${PAGES[@]} sisis/searchSingleAvailability_aachen.html sisis/searchSingleAvailability_altdorf.html sisis/searchSingleAvailability_altdorf2.html sisis/searchSingleAvailability_altdorf3.html sisis/searchSingleAvailability_amberg.html sisis/searchSingleAvailability_amberg2.html sisis/searchSingleAvailability_amberg3.html sisis/searchSingleAvailability_augsburg.html sisis/searchSingleAvailability_basel.html sisis/searchSingleAvailability_chemnitz.html sisis/searchSingleAvailability_dortmund.html sisis/searchSingleAvailability_dortmund2.html sisis/searchSingleAvailability_dortmund3.html sisis/searchSingleAvailability_pulheim.html sisis/searchSingleAvailability_pulheim2.html )


#==============PICA================
mkdir -p $OUTPATH/pica

ADDTEMPLATE pica/update 4
PAGES=(${PAGES[@]} pica/update3.html pica/update_abholen.html pica/update_luebeck.html pica/update_vormerk.khsb.html)

#use join, because test blows up with a space
test pica 'extend{id-list:=join\(\(\"12/435:X\",\"XYZ\"\)\)}' extend2.html extend2.colon.html extend.colon.html

test pica 'searchDetails{\(\)/\(base:=\"\"\)}' searchDetails.html searchDetails.swb.html


#================LBS=============
mkdir -p $OUTPATH/lbs
ADDTEMPLATE lbs/list 1
PAGES=(${PAGES[@]} lbs/loans.stralsund.html )

#=============ADISWEB==============
mkdir -p $OUTPATH/aDISWeb


ADDTEMPLATE aDISWeb/accountOverview 4
PAGES=(${PAGES[@]} aDISWeb/accountOverview_munich.html aDISWeb/accountOverview_nurnberg.html aDISWeb/accountOverview_nurnberg.fake.html aDISWeb/accountOverview_testhdm.html)

ADDTEMPLATE aDISWeb/list{splitIndex:=0} 8
PAGES=(${PAGES[@]} aDISWeb/list_munich.html aDISWeb/list_nurnberg.html aDISWeb/list_testhdm.html aDISWeb/list_testhdm2.html  aDISWeb/list_provided_testhdm.html aDISWeb/list_requested_munich.html aDISWeb/list_requested_testhdm.html aDISWeb/list_orders_munich.html)


test aDISWeb search search_stuttgart_abk.do.html search_no-result.aalen.html search_no-result.voebb.html search_single-page.biberach.html search_go-back.freiburg.html search_single-result_hdmtest.html search_single-result_hdmtest2.html search_single-result_nuernberg.html search_nurnberg.html search_munich.html search_dortmund.html  search_due.html search_go-back.voebb.html  search_new.voebb.html search_new5.voebb.html search_duesseldorf.html search_strangenum.ulm.html search_single-page.isbn.voebb.html

test aDISWeb searchBasicForm search_go-back.voebb..html search_due..html  search_go-back.freiburg..html search_go-back.single-result.voebb.html search_new.voebbBF.html search_duesseldorfBF.html 

test aDISWeb searchDetails searchDetails_albstadt.html searchDetails_due.html searchDetails_heilbronn.html searchDetails_heilbronn2.html searchDetails_single-result_hdmtest.html searchDetails_newdue.html searchDetails_new.voebb.html

ADDTEMPLATE aDISWeb/connected 3
PAGES=(${PAGES[@]} aDISWeb/connected_furtwangen.html aDISWeb/connected_voebb1.html aDISWeb/connected_voebb2.html)

ADDTEMPLATE aDISWeb/searchInputForm 1
PAGES=(${PAGES[@]} aDISWeb/searchInputForm_mannheim-hsb.html )

ADDTEMPLATE 'aDISWeb/orderConfirmation{\(\)/\(username:=\$password:=\"\"\)}' 3
PAGES=(${PAGES[@]} aDISWeb/orderConfirmation_hdmtest.html aDISWeb/orderConfirmation_due.html aDISWeb/orderConfirmation_voebb.html )

ADDTEMPLATE aDISWeb/orderConfirmed 5
PAGES=(${PAGES[@]} aDISWeb/orderConfirmed1_due.html aDISWeb/orderConfirmed2_due.html aDISWeb/orderConfirmed2_hdmtest.html aDISWeb/orderConfirmed_failed2_hdmtest.html aDISWeb/orderConfirmed_failed_hdmtest.html)

test aDISWeb orderMultipleDistrict orderTimeout.html orderInCollection.html 



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

test zones18 'listOrders{requestId:=0}' orderList1.html orderList1unterwegs.html orderList2.html orderList2canceled.html orderList3+canceled.html orderList3+canceled_real.html 
test zones18 cancelConfirm cancelConfirm.html
test zones18 'searchList{requestId:=0}' searchList.biel.html searchList.cologne.html searchList.hannover.html searchList.kiel.html searchList.koblenz.html searchList_notfound.html
test zones18 'searchList{requestId:=1}' searchListIncremental.biel.html searchListIncremental.cologne.html searchListIncremental.hannover.html searchListIncremental.kiel.html searchListIncremental.koblenz.html
test zones18 searchDetails searchDetails.html searchDetailsCollection.html searchDetailsSubTitle.html searchDetails.biel.html searchDetails.cologne.html searchDetails.hannover.html searchDetails.koblenz.html searchDetails.koblenz2.html 
test zones18 orderConfirmation orderConfirm.html


#==============Primo========
mkdir -p $OUTPATH/primo
ADDTEMPLATE 'primo/searchStart{\(\)/\(search-keys:=\"\"\)}' 3
PAGES=(${PAGES[@]} primo/searchStart.fub.html primo/searchStart.hub.html  primo/searchStart.tub.html)
ADDTEMPLATE primo/searchList  5
PAGES=(${PAGES[@]} primo/searchList.hub.html primo/searchList0.hub.html primo/searchList.fub.html primo/searchList.tub.html primo/searchList3.tub.html)

test primo list list.tub.html list.orders.tub.html list.orders2.tub.html list.fub.html list.orders.fub.html list.new.tub.htm list.orders.new.tub.htm 
test primo searchDetails{vl:set-book-property} searchDetails.tub.html 
test primo searchDetailsLocations{vl:set-book-property} location.hu.berlin.html location.tu.berlin.html    locations.fu.berlin.html locations.hu.berlin.html locations.tu.berlin.html  locationsdiv.fu.html

#=============Bibliothea==============
mkdir -p $OUTPATH/bibliotheca
ADDTEMPLATE bibliotheca/list 4
PAGES=(${PAGES[@]} bibliotheca/list.stralsund.html bibliotheca/list.neustadt.html bibliotheca/listmixed.neustadt.html bibliotheca/list.stralsundasfakeheidelberg.html)

ADDTEMPLATE bibliotheca/searchList 2
PAGES=(${PAGES[@]} bibliotheca/searchList.stralsund.html bibliotheca/searchList.neustadt.html)

ADDTEMPLATE bibliotheca/searchDetails 1
PAGES=(${PAGES[@]} bibliotheca/searchDetails.neustadt.html)



#=============Bibliothea+ OPEN==============
mkdir -p $OUTPATH/bibliothecaplus

test bibliothecaplus list list.stralsund.html list.darmstadt.html list.with-orders.darmstadt.html
test bibliothecaplus searchList'{\$last-visited-page:=0,\$current-page:=1,\$search-reverse-keys:=\(\)}' searchList.empty.html searchList.html searchList.darmstadt.html searchList.holzgerlingen.html searchList.holzgerlingen0.html
test bibliothecaplus searchListHeaderOnly'{\$last-visited-page:=0,\$current-page:=1}' searchListHeaderOnly.html
test bibliothecaplus searchDetails'{\$search-reverse-keys:=\(\)}' searchDetails.html searchDetails.darmstadt.html searchDetails.holzgerlingen.html searchDetails.trier.html searchDetails.errorlangen.html

#=============SUMMON==============
mkdir -p $OUTPATH/summon
ADDTEMPLATE summon/loans 5
PAGES=(${PAGES[@]} summon/loans-none.html summon/loans-orders.html summon/loans-orders-none.html summon/loans-renewable.html summon/loans-renewconfirmed.html )

ADDTEMPLATE summon/searchList 2
PAGES=(${PAGES[@]} summon/searchList0.html summon/searchList.html)

ADDTEMPLATE summon/searchDetails 3
PAGES=(${PAGES[@]} summon/searchDetails.html summon/searchDetails_multiex.html summon/searchDetails_orderable.html)

#=============netbiblio==============
mkdir -p $OUTPATH/netbiblio
test netbiblio list list.html list.orders.html 

test netbiblio searchList searchList.0.html searchList.html  searchList.singlepage.html searchList.en.html
test netbiblio searchDetails{vl:set-book-property} searchDetails.html searchDetails.ebook.html  

#=============vufind==============
mkdir -p $OUTPATH/vufind
test vufind list checkedout.due.html checkedout.en.due.html holds1.due.html holds1.en.due.html holds0.due.html holds0.en.due.html

#=============DIGIBIB==============
mkdir -p $OUTPATH/digibib

DISE=../search/templates/digibib/search
DIDE=../search/templates/digibib/details{vl:set-book-property}
TEMPLATES=(${TEMPLATES[@]} $DISE $DISE $DIDE $DIDE $DIDE $DIDE $DIDE $DIDE $DIDE $DIDE)
PAGES=(${PAGES[@]} digibib/search.html digibib/search2.html digibib/details.html digibib/details2.html digibib/details3.html digibib/details4.html digibib/details5.html digibib/details6new.html digibib/details7new.fhmuenster.html digibib/details7holdings.fhmuenster.html)  

test digibib list list.html list.empty.html 

#=============KOHA==============
mkdir -p $OUTPATH/koha
test koha user user.2.en.pf.htm user.2.pf.htm user.adminkuhn.htm user.holds.en.pf.htm
test koha searchDetails searchDetails.ebook.pf.htm searchDetails.2.pf.htm
#test koha 
#test koha reserve'{holding:={"id":123}}' reserve.pf.htm

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
    if [[ $EXTRA =~ vl:confirm ]]; then
      EXTRA="-e \"declare function vl:confirm(\\\$a,\\\$b) { message-confirm :=  join((\\\$a,\\\$b))};()\""
    fi
    if [[ $EXTRA =~ vl:set-book-property ]]; then
      EXTRA="-e \"declare function vl:set-book-property(\\\$a,\\\$b) { \\\$book(\\\$a||'!!') := \\\$b }; ()\""
    fi
  fi
  FUNCTIONS="-e \"
    declare function vl:delete-current-books() { books-deleted := true() }; 
    declare function vl:choose(\\\$a,\\\$b,\\\$c,\\\$d) { message-choose :=  join((\\\$a,\\\$b,\\\$c,\\\$d))};
    declare function vl:raise(\\\$x) { raised := \\\$x }; 
    declare function vl:raise-login(\\\$x) { raised := \\\$x }; 
    declare function vl:raise-internal(\\\$x) { raised := \\\$x }; 
    ()\""

  #echo $TEMPLATEPARSER $FUNCTIONS $EXTRA $TEMPLATEPARSERARGS $INPATH/${PAGES[i]} --extract-file=$TFILE  
  if ! eval $TEMPLATEPARSER $FUNCTIONS $EXTRA $TEMPLATEPARSERARGS $INPATH/${PAGES[i]} --extract-file=$TFILE  > $OUTPATH/${PAGES[i]}.result 2> $OUTPATH/stderr; then
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


