#!/bin/bash


TEMPLATEPARSER="../../../../../components/pascal/data/examples/htmlparserExample --no-header --immediate-vars-changelog --template="
TEMPLATEPATH=../../data/libraries/templates
INPATH=./
OUTPATH=/tmp/

if [ "$1" = "--generate" ]; then
echo "GENERATING"
OUTPATH=./
fi


#=============WAS==============
mkdir -p $OUTPATH/wasnrw
TEMPLATES=(wasnrw/start wasnrw/KontoServlet wasnrw/KontoServlet wasnrw/KontoServlet)
PAGES=(wasnrw/start.html wasnrw/BenutzerkontoServlet_books.html wasnrw/BenutzerkontoServlet_books2.html wasnrw/BenutzerkontoServlet_empty.html)

#=============ALEPH ULBD==============
mkdir -p $OUTPATH/ulbdue
TEMPLATES=(${TEMPLATES[@]} ulbdue/start ulbdue/login aleph_base/loggedIn2005 aleph_base/details ulbdue/update ulbdue/update)
PAGES=(${PAGES[@]} ulbdue/start.html ulbdue/login.html ulbdue/loggedIn.html ulbdue/details_1.html ulbdue/update_singlebook.html ulbdue/update_empty.html)

#=============ALEPH TU/UdK Berlin========
mkdir -p $OUTPATH/aleph_ubTUb
TEMPLATES=(${TEMPLATES[@]} aleph_ubTUb/loggedIn aleph_ubTUb/update aleph_base/details)
PAGES=(${PAGES[@]} aleph_ubTUb/loggedIn.html aleph_ubTUb/update.html aleph_ubTUb/details.html)

#=============ALEPH HU Berlin============
mkdir -p $OUTPATH/aleph_ubHUb
TEMPLATES=(${TEMPLATES[@]} aleph_base/loggedIn2005 aleph_base/details aleph_base/details aleph_base/details aleph_ubHUb/update)
PAGES=(${PAGES[@]} aleph_ubHUb/loggedIn.html aleph_ubHUb/details1ex.html aleph_ubHUb/details2ex.html aleph_ubHUb/details3ex.html aleph_ubHUb/update-3books.html)


#=============LIBERO==============
mkdir -p $OUTPATH/libero54

TEMPLATES=(${TEMPLATES[@]} libero54/start libero54/update libero54/update libero54/update libero54/update)
PAGES=(${PAGES[@]} libero54/start.html libero54/update_empty.html libero54/update55sp73_empty.html libero54/update55sp4_2books.html libero54/update55sp73_books.html)

#=============SISIS==============
mkdir -p $OUTPATH/sisis

TEMPLATES=(${TEMPLATES[@]} sisis/start sisis/loggedIn sisis/loggedIn sisis/loggedIn)
PAGES=(${PAGES[@]} sisis/start.do.html sisis/userAccount.do_empty.html sisis/userAccount.do_singlebook.html sisis/userAccount.do_singlebook2.html)

#=============DIGIBIB==============
mkdir -p $OUTPATH/digibib

DISE=../search/templates/digibib/search
DIDE=../search/templates/digibib/details
TEMPLATES=(${TEMPLATES[@]} $DISE $DISE $DIDE $DIDE $DIDE $DIDE $DIDE)
PAGES=(${PAGES[@]} digibib/search.html digibib/search2.html digibib/details.html digibib/details2.html digibib/details3.html digibib/details4.html digibib/details5.html)  

for ((i=0;i<${#TEMPLATES[@]};i++)); do
  echo -e "Testing: ${TEMPLATES[i]} \t\t\twith\t\t $INPATH/${PAGES[i]}"
  $TEMPLATEPARSER$TEMPLATEPATH/${TEMPLATES[i]} $INPATH/${PAGES[i]} > $OUTPATH/${PAGES[i]}.result
  diff $INPATH/${PAGES[i]}.result $OUTPATH/${PAGES[i]}.result
done;

#$TEMPLATEPARSER$TEMPLATES/wasnrw/start $INPAGES/was/start.html > $OUTPAGES/was/start
#$TEMPLATEPARSER$TEMPLATES/wasnrw/KontoServlet $INPAGES/wasnrw/BenutzerkontoServlet_books.html $OUTPAGES/was/BenutzerkontoServlet_books.ht


