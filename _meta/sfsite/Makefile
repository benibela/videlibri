

all: buildtemp/tardis buildtemp/global.d phony upload

buildtemp/global.d: global.xml
	xidel global.xml --extract-include xxx --extract-kind xquery -e 'output-dir := "_publish/",source:=()'  --extract-file common.xq -e 'local:deps()' > buildtemp/global.d


include buildtemp/global.d

.PHONY := phony clean all clean-html clean-img pre-upload do-upload
phony: $(PAGES)
#$(DEPS) $(PAGES)

#include $(DEPS)

#_publish/%.html: %.xml common.xq style.xq.html global.xml
#	xidel --input-format xml-strict --extract-include=xxx -e 'source := "$*", output-dir := "_publish/"' --extract-kind xquery  --extract-file common.xq -e 'local:doit()'
#	

clean: clean-img clean-html
	
clean-html:
	rm publish/*.html


buildtemp/tardis:
	test -e buildtemp/tardis || touch buildtemp/tardis
  

upload:
#Write all files that were changed to buildtemp/changed
	find _publish/ -type f -newer buildtemp/tardis > buildtemp/changed
	#xargs alone calls rsync for every file separately, which is slow. tr+xargs calls rsync with one arg for all files, which does not work
	cd _publish; sed -e 's:^_publish:.:' ../buildtemp/changed | tr '\n' ' ' | xargs -I XXXX sh -c 'rsync -av -e ssh XXXX  "benibela,videlibri@web.sourceforge.net:/home/project-web/videlibri/htdocs/"; echo'
	touch buildtemp/tardis
	
