PROG_PREV = ./dist/build/snap-web/snap-web
PROG_NAME = ./snap-web
#EXTRA_TEST_LIB = ./dist/build/snap-web/snap-web-tmp
STYLE=stylish-haskell

DIST=dist
SITE=_site

CBD=cabal-dev

default: build-dev

clean:
	rm -rf $(DIST)

hlint:
	hlint src/ tests/ --report=$(DIST)/hlint.html
	$(STYLE) -i src/**/*.hs

###########################
## DEVELOPMENT
## 
###########################
init:
	cabal update
	$(CBD) install

conf-dev:
	$(CBD) --flags="development" --enable-tests configure

build-dev: conf-dev
	$(CBD) build

test: build-dev
	$(CBD) test

p:
	$(PROG_PREV) -p 9900

bp: build-dev p
rp: clean build-dev p

###########################
## PRODUCTION
##
###########################

build: 
	$(CBD) configure
	$(CBD) build 1>./log/build.log 2>&1

rebuild: clean build

##
##       1. create new dir _sites
##       2. compress HTML
##       3. combine & compress JS; replace related links in templates.
##       4. generate main.css via lessc; replace related links in templates.
##       5. [ ] md5sum
## 

markdownJS=Markdown.Converter.js Markdown.Sanitizer.js Markdown.Editor.js markdown.js
markdownMergeJS=markdown.min.js

create-site: rebuild
	rm -rf $(SITE)
	mkdir -p $(SITE)/log
	mkdir -p $(SITE)/static/css
	cp devel.cfg $(SITE)/prod.cfg
	cp -r snaplets data $(SITE)
	cp -r static/img $(SITE)/static/img
	cp -r static/js $(SITE)/static/js
	## Uglify JavaScripts
	cd $(SITE)/static/js && for x in *.js ; do \
		uglifyjs $$x > $$x.min.js ; \
		mv -f $$x.min.js $$x ; \
	done
	## Merge Markdown JS
	cd $(SITE)/static/js && for x in $(markdownJS) ; do \
		cat $$x >> $(markdownMergeJS); \
		echo >> $(markdownMergeJS); \
		rm $$x ; \
	done
	## Generate CSS from LESS files
	lessc --compress static/less/bootstrap.less > $(SITE)/static/css/main.css
	## compress TPL files
	for x in `find $(SITE)/ -name '*.tpl' ` ; do \
		perl -i -p -e  's/[\r\n]+|[ ]{2}|<!--(.|\s)*?--.*>//gs' $$x ; \
		perl -i -p -e  's/<!--(.|\s)*?-->//gs' $$x ; \
	done

	cp $(PROG_PREV) $(SITE)


prod:
	cd $(SITE) && $(PROG_NAME) -p 9900 @prod

####################### Doc

doc:
	cabal haddock --executable
