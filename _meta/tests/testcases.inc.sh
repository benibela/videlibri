#called from unittests.sh


#=============KRZN WEBOPAC==============
mkdir -p $OUTPATH/wasnrw
test wasnrw start start.html
test wasnrw KontoServlet BenutzerkontoServlet_books.html BenutzerkontoServlet_books2.html BenutzerkontoServlet_books3.html


#=============ALEPH ULBD==============
mkdir -p $OUTPATH/ulbdue $OUTPATH/aleph_base
testCrossDir aleph_base loggedIn2005 ulbdue loggedIn.html
testCrossDir aleph_base details ulbdue details_1.html
test aleph_base details details.htm details.hold.htm
test ulbdue start start.html
test ulbdue login login.html
test ulbdue update update_singlebook.html update_empty.html
test ulbdue update-hold bor-hold.html bor-hold.0.html

#=============ALEPH TU/UdK Berlin========
mkdir -p $OUTPATH/aleph_ubTUb
testCrossDir aleph_base loggedIn2007 aleph_ubTUb loggedIn.html
testCrossDir aleph_base details aleph_ubTUb details.html
test aleph_ubTUb update update.html

#=============ALEPH HU Berlin============
mkdir -p $OUTPATH/aleph_ubHUb
testCrossDir aleph_base loggedIn2005 aleph_ubHUb loggedIn.html
testCrossDir aleph_base details aleph_ubHUb details1ex.html details2ex.html details3ex.html
test aleph_ubHUb update update-1.html
 

#=============ALEPH FU Berlin============
mkdir -p $OUTPATH/aleph_ubFUb
testCrossDir aleph_base details  aleph_ubFUb details.html 
test aleph_ubFUb update update-2books.html update-0books.html update-6books.html 
testCrossDir aleph_base loggedIn2007 aleph_ubFUb loggedIn.html

#============BIBDIA===============
mkdir -p $OUTPATH/bibdia
test bibdia update user.medk.duisburg.0.html user.medk.duisburg.1.html user.vorm.duisburg.1.html user.vorm.duisburg.1b.html
test bibdia quickDetails quickDetails.duisburg.htm quickDetails.en.duisburg.html quickDetails2.duisburg.htm quickDetails.essen.htm  quickDetails2.essen.htm 

#============BIBDIA StaBib===============
mkdir -p $OUTPATH/bibdia_stabib
test bibdia_stabib update list.html list_abhol.html list_empty.html list_new.html update.0.html update.html best.html
testCrossDir pica 'searchDetails{\(\)/\(base:=\"\"\)}' bibdia_stabib stabikat1.html stabikat2.html stabikat3.html
test bibdia_stabib simpleCheck simpleCheck.html

#=============LIBERO==============
mkdir -p $OUTPATH/libero5

test libero5 start start.html
test libero5 update update_empty.html update55sp73_empty.html update55sp4_2books.html update55sp73_books.html update.login.faileden.html update55sp4_2books.fakesaarbrucken.html update6orders.saar.html update.session-lost.html update.verl.html update6_1book_verl.html
test libero5 searchDetails searchDetails_order_verl.html searchDetails_link_bochum.html searchDetails_fhbd.html
test libero5 orderConfirm orderConfirm.verl.html
test libero5 orderComplete orderfail.verl.html ordersuccess.verl.html 

#=============SISIS==============
mkdir -p $OUTPATH/sisis
mkdir -p $OUTPATH/sisis/touchpoint

test sisis start start.do.html start_schweinfurt.do.html
test sisis start touchpoint/start_chem.html touchpoint/start_erlangen.html
test sisis start-search"{touchpoint:=false\(\)}" start-search.do.html start-search_schweinfurt.do.html
test sisis start-search"{touchpoint:=true\(\)}" touchpoint/start-search_chem.html touchpoint/start-search_erlangen.html

test sisis loggedIn userAccount.do_empty.html userAccount.do_singlebook.html userAccount.do_singlebook2.html userAccount.do_2books.html 
testCrossDir sisis "loggedIn{touchpoint:=true\(\)}" sisis/touchpoint userAccount_0provided_chem.html userAccount_0requests_chem.html userAccount_1-10lend_chem.html userAccount_11-20lend_chem.html userAccount_empty_chem.html 
testCrossDir sisis searchSingleTPShowHolding sisis/touchpoint/ searchSingleTPShowHolding.chemnitz.html  searchSingleTPShowHolding.bamberg.html searchSingleTPShowHolding.munich.html
testCrossDir sisis searchSingleTPDocument sisis/touchpoint/ searchSingleTPDocument.chemnitz.htm

#search
test sisis searchList{curPos:=1} search_Z3988_haw-aw.do.html search_augsburg.do.html search_augsburg2.do.html search_basel.do.html search_liestal.do.html search_tum.do.html search_pulheim.do.html searchHitlist_pulheim.do.html search_winterthur.do.html search_amberg1.html  search_last_fuerth.do.html search.empty.aschaffenburg.html search_oberhausen.do.html search_leipzig.html  
testCrossDir sisis searchList{curPos:=1}  sisis/touchpoint/ searchList_winterthur.html searchList_speedHit_winterthur.html searchList_chemnitz.html searchList_rwth.html
test sisis searchSingle searchSingle_basel.do.html searchSingle_aachen.do.html searchSingle_freiburg.html  searchSingle_regensburg.html touchpoint/searchHit_chem.html touchpoint/searchHit_rwth.html

test sisis orderConfirmation'{username:=\"foo\",password:=\"bar\"}' orderConfirmation_aachen.html orderConfirmation_pulheim.html  

test sisis 'singleExtended{b:={\"id\":123}}' singleExtended_h.html  

test sisis searchSingleExemplar searchSingleExemplar_aachen.html  searchSingleExemplar_altdorf.html  searchSingleExemplar_altdorf2.html  searchSingleExemplar_altdorf3.html   searchSingleExemplar_amberg.html   searchSingleExemplar_amberg2.html  searchSingleExemplar_amberg3.html   searchSingleExemplar_augsburg.html  searchSingleExemplar_basel.html  searchSingleExemplar_chemnitz.html   searchSingleExemplar_dortmund.html  searchSingleExemplar_dortmund2.html  searchSingleExemplar_dortmund3.html  searchSingleExemplar_pulheim.html  searchSingleExemplar_pulheim2.html searchSingleExemplar_freiburg.html searchSingleExemplar_leipzig.html

test sisis searchSingleAvailability  searchSingleAvailability_aachen.html searchSingleAvailability_altdorf.html searchSingleAvailability_altdorf2.html searchSingleAvailability_altdorf3.html searchSingleAvailability_amberg.html searchSingleAvailability_amberg2.html searchSingleAvailability_amberg3.html searchSingleAvailability_augsburg.html searchSingleAvailability_basel.html searchSingleAvailability_chemnitz.html searchSingleAvailability_dortmund.html searchSingleAvailability_dortmund2.html searchSingleAvailability_dortmund3.html searchSingleAvailability_pulheim.html searchSingleAvailability_pulheim2.html 


#==============PICA================
mkdir -p $OUTPATH/pica

test pica update update3.html update_abholen.html update_luebeck.html update_vormerk.khsb.html

#use join, because test blows up with a space
test pica 'extend{id-list:=join\(\(\"12/435:X\",\"XYZ\"\)\)}' extend2.html extend2.colon.html extend.colon.html

test pica 'searchDetails{\(\)/\(base:=\"\"\)}' searchDetails.html searchDetails.swb.html
test pica searchList searchList.merseburg.html

#================LBS=============
mkdir -p $OUTPATH/lbs
test lbs list loans.stralsund.html 

#=============ADISWEB==============
mkdir -p $OUTPATH/aDISWeb


test aDISWeb accountOverview accountOverview_munich.html accountOverview_nurnberg.html accountOverview_nurnberg.fake.html accountOverview_testhdm.html
test aDISWeb 'list{splitIndex:=0,mode:=\"lend\"}' list_munich.html list_nurnberg.html list_testhdm.html list_testhdm2.html 
test aDISWeb 'list{splitIndex:=0,mode:=\"provided\"}' list_provided_testhdm.html
test aDISWeb 'list{splitIndex:=0,mode:=\"requested\"}' list_requested_munich.html list_requested_testhdm.html
test aDISWeb 'list{splitIndex:=0,mode:=\"orderedMagazine\"}' list_orders_munich.html


test aDISWeb search search_stuttgart_abk.do.html search_no-result.aalen.html search_no-result.voebb.html search_single-page.biberach.html search_go-back.freiburg.html search_single-result_hdmtest.html search_single-result_hdmtest2.html search_single-result_nuernberg.html search_nurnberg.html search_munich.html search_dortmund.html  search_due.html search_go-back.voebb.html  search_new.voebb.html search_new5.voebb.html search_duesseldorf.html search_strangenum.ulm.html search_single-page.isbn.voebb.html search_newer.voebb.html

test aDISWeb searchBasicForm search_go-back.voebb..html search_due..html  search_go-back.freiburg..html search_go-back.single-result.voebb.html search_new.voebbBF.html search_duesseldorfBF.html 

test aDISWeb searchDetails searchDetails_albstadt.html searchDetails_due.html searchDetails_heilbronn.html searchDetails_heilbronn2.html searchDetails_single-result_hdmtest.html searchDetails_newdue.html searchDetails_new.voebb.html searchDetails_new2.voebb.html searchDetails_munich.html

test aDISWeb connected connected_furtwangen.html connected_voebb1.html connected_voebb2.html

test aDISWeb searchInputForm searchInputForm_mannheim-hsb.html 

test aDISWeb 'orderConfirmation{\(\)/\(username:=\$password:=\"\"\)}' orderConfirmation_hdmtest.html orderConfirmation_due.html orderConfirmation_voebb.html orderTimeout.html

test aDISWeb orderConfirmed orderConfirmed1_due.html orderConfirmed2_due.html orderConfirmed2_hdmtest.html orderConfirmed_failed2_hdmtest.html orderConfirmed_failed_hdmtest.html orderConfirmed_munich.html orderConfirmed_munich2.html orderConfirmed_voebb.html



#==============Zones========
mkdir -p $OUTPATH/zones18
test zones18 loggedIn loggedIn.html

test zones18 'list{requestId:=0}' list.html list10.html
test zones18 'list{requestId:=1}' listIncremental.html

test zones18 bulkRenew bulkRenew.html bulkRenew2.html

test zones18 'listOrders{requestId:=0}' orderList1.html orderList1unterwegs.html orderList2.html orderList2canceled.html orderList3+canceled.html orderList3+canceled_real.html 
test zones18 cancelConfirm cancelConfirm.html
test zones18 'searchList{requestId:=0}' searchList.biel.html searchList.cologne.html searchList.hannover.html searchList.kiel.html searchList.koblenz.html searchList_notfound.html
test zones18 'searchList{requestId:=1}' searchListIncremental.biel.html searchListIncremental.cologne.html searchListIncremental.hannover.html searchListIncremental.kiel.html searchListIncremental.koblenz.html
test zones18 searchDetails searchDetails.html searchDetailsCollection.html searchDetailsSubTitle.html searchDetails.biel.html searchDetails.cologne.html searchDetails.cologne2.html searchDetails.hannover.html searchDetails.koblenz.html searchDetails.koblenz2.html 
test zones18 orderConfirmation orderConfirm.html


#==============Primo========
mkdir -p $OUTPATH/primo
test primo 'searchStart{\(\)/\(search-keys:=\"\"\)}' searchStart.fub.html searchStart.hub.html  searchStart.tub.html
test primo searchList searchList.hub.html searchList0.hub.html searchList.fub.html searchList.tub.html searchList3.tub.html

test primo list list.tub.html list.orders.tub.html list.orders2.tub.html list.fub.html list.orders.fub.html list.new.tub.htm list.orders.new.tub.htm 
test primo searchDetails searchDetails.tub.html 
test primo searchDetailsLocations location.hu.berlin.html location.tu.berlin.html    locations.fu.berlin.html locations.hu.berlin.html locations.tu.berlin.html  locationsdiv.fu.html

#=============Bibliothea==============
mkdir -p $OUTPATH/bibliotheca
test bibliotheca list list.stralsund.html list.neustadt.html listmixed.neustadt.html list.stralsundasfakeheidelberg.html
test bibliotheca searchList searchList.stralsund.html searchList.neustadt.html searchList.mainz.html
test bibliotheca searchDetails searchDetails.neustadt.html



#=============Bibliothea+ OPEN==============
mkdir -p $OUTPATH/bibliothecaplus

test bibliothecaplus list list.stralsund.html list.darmstadt.html list.with-orders.darmstadt.html list.ludwig.orders.html list.darmstadt.fake.html
test bibliothecaplus searchList'{\$last-visited-page:=0,\$current-page:=1,\$search-reverse-keys:=\(\)}' searchList.empty.html searchList.html searchList.darmstadt.html searchList.holzgerlingen.html searchList.holzgerlingen0.html
test bibliothecaplus searchListHeaderOnly'{\$last-visited-page:=0,\$current-page:=1}' searchListHeaderOnly.html
test bibliothecaplus searchDetails'{\$search-reverse-keys:=\(\)}' searchDetails.html searchDetails.darmstadt.html searchDetails.holzgerlingen.html searchDetails.trier.html searchDetails.errorlangen.html searchDetails.heddesheim.htm searchDetails.new.darmstadt.htm searchDetails.new.orderable.darmstadt.htm searchDetails.ludwig.htm

#=============SUMMON==============
mkdir -p $OUTPATH/summon
test summon loans loans-none.html loans-orders.html loans-orders-none.html loans-renewable.html loans-renewconfirmed.html 

test summon searchList searchList0.html searchList.html

test summon searchDetails searchDetails.html searchDetails_multiex.html searchDetails_orderable.html

#=============netbiblio==============
mkdir -p $OUTPATH/netbiblio
test netbiblio list list.html list.orders.html 

test netbiblio searchList searchList.0.html searchList.html  searchList.singlepage.html searchList.en.html
test netbiblio searchDetails searchDetails.html searchDetailsNew.html searchDetails.ebook.html  

#=============vufind==============
mkdir -p $OUTPATH/vufind
test vufind list checkedout.due.html checkedout.en.due.html checkedout.overdue.html checkedout.en.overdue.html holds1.due.html holds1.en.due.html holds0.due.html holds0.en.due.html holds2.due.html holds2.en.due.html
test vufind searchList searchList.datteln.html searchList.hhu.html searchList.hmtleipzig.html
test vufind searchDetails searchDetails.hhu.html searchDetails.marl.html searchDetails.glad.html searchDetails.hmtleipzig.html searchDetails.hmtleipzig2.html

#=============DIGIBIB==============
mkdir -p $OUTPATH/digibib
testCrossDir ../search/templates/digibib search  digibib search.html search2.html
testCrossDir ../search/templates/digibib details digibib details.html details2.html details3.html details4.html details5.html details6new.html details7new.fhmuenster.html details7holdings.fhmuenster.html details7noholdings.fhmuenster.html  detailsNRW.html
test digibib list list.html list.empty.html 

#=============SRU=================
mkdir -p $OUTPATH/sru
test sru sruconnect explain.xml
test sru list opac-de-gl2.xml gvk.xml

#=============KOHA==============
mkdir -p $OUTPATH/koha
test koha user user.2.en.pf.htm user.2.pf.htm user.adminkuhn.htm user.holds.en.pf.htm user.0.htm user.0.adminkuhn.htm user.adminkuhn2.htm
test koha searchDetails searchDetails.ebook.pf.htm searchDetails.2.pf.htm
#test koha 
#test koha reserve'{holding:={"id":123}}' reserve.pf.htm