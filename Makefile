CBD=cabal
STYLE=stylish-haskell

PROG_PREV = ./dist/build/snap-web/snap-web
PROD_PROD = .cabal-sandbox/bin/snap-web
PROG_NAME = ./snap-web

DIST=dist
SITE=_site

default: build

clean:
	cabal clean

hlint:
	$(STYLE) -i src/**/*.hs
	hlint src/ tests/ --report=$(DIST)/hlint.html

doc:
	cabal haddock --executable


###########################
## DEVELOPMENT
##
###########################

## need manual config pandoc because of https://github.com/jgm/pandoc/issues/1526
##
init:
	test -e cabal.sandbox.config || $(CBD) sandbox init
	cabal install -fthree transformers-compat
	cabal install scientific-0.3.2.1 -f -bytestring-builder
	mkdir data log
	$(CBD) install --only-dependencies --enable-tests --job=2

conf:
	$(CBD) --flags="development" configure

build: conf
	$(CBD) build

install: conf
	$(CBD) install

test:
	$(CBD) --flags="development" --enable-tests configure
	$(CBD) build
	$(CBD) test

p:
	$(PROG_PREV) -p 9900

cb: clean build

bp: build p

rp: clean build p

###########################
## PRODUCTION
##
###########################

LOG_FILE=./log/build.log

build-prod: clean
	echo "Start building" >$(LOG_FILE)
	date >>$(LOG_FILE)
	$(CBD) configure
	$(CBD) install 1>>$(LOG_FILE) 2>&1
	date >>$(LOG_FILE)
	echo "End building" >>$(LOG_FILE)

rebuild: clean build-prod

##
##       1. create new dir _sites
##       2. compress HTML
##       3. combine & compress JS; replace related links in templates.
##       4. generate main.css via lessc; replace related links in templates.
##       5. [ ] md5sum
##

JS_FILES=jquery.js bootstrap-collapse.js bootstrap-button.js

create-site:
	rm -rf $(SITE)
	mkdir -p $(SITE)/log
	mkdir -p $(SITE)/static/css
	cp Makefile $(SITE)/
	cp devel.cfg $(SITE)/prod.cfg
	cp robots.txt $(SITE)/static/
	cp -r snaplets data $(SITE)
	cp -r static/img $(SITE)/static/img
	cp -r static/js $(SITE)/static/js

	cd $(SITE)/static/js && for x in *.js ; do \
		uglifyjs $$x > $$x.min.js ; \
		mv -f $$x.min.js $$x ; \
	done

	# uglifyjs $$x > $$x.min.js
	# mv -f $$x.min.js $$x

	cd $(SITE)/static/js/libs && for x in $(JS_FILES) ; do \
		uglifyjs $$x >> m.js ; \
		rm -f $$x ;\
	done

	mv -f $(SITE)/snaplets/heist/templates/_layout-js-prod.tpl $(SITE)/snaplets/heist/templates/_layout-js.tpl

	lessc --compress static/bootstrap/bootstrap.less > $(SITE)/static/css/main.css
	mv -f $(SITE)/snaplets/heist/templates/_layout-css-prod.tpl $(SITE)/snaplets/heist/templates/_layout-css.tpl

	for x in `find $(SITE)/ -name '*.tpl' ` ; do \
		perl -i -p -e  's/[\r\n]+|[ ]{2}|<!--(.|\s)*?--.*>//gs' $$x ; \
		perl -i -p -e  's/<!--(.|\s)*?-->//gs' $$x ; \
	done

	cp $(PROD_PROD) $(SITE)


prod:
	cd $(SITE) && $(PROG_NAME) -p 9900 -e prod

## ?? TODO: use --address=a.haskellcn.org
a.hcn:
	$(PROG_NAME) -p 9900 -e prod --no-access-log


#######################
