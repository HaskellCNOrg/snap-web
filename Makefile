CBD=cabal-dev
STYLE=stylish-haskell

PROG_PREV = ./dist/build/snap-web/snap-web
PROG_NAME = ./snap-web

DIST=dist
SITE=_site

default: build-dev

clean:
	rm -rf $(DIST)

hlint:
	$(STYLE) -i src/**/*.hs
	hlint src/ tests/ --report=$(DIST)/hlint.html

doc:
	cabal haddock --executable


###########################
## DEVELOPMENT
##
###########################

dryrun:
	$(CBD) install --only-dependencies --dry-run

init:
	cabal update
	$(CBD) install --only-dependencies

build-dev:
	$(CBD) --flags="development" configure
	$(CBD) build

install-dev: build-dev
	$(CBD) install

test:
	$(CBD) --flags="development" --enable-tests configure
	$(CBD) build
	$(CBD) test

p:
	$(PROG_PREV) -p 9900

cb: clean build-dev

bp: build-dev p

rp: clean build-dev p

###########################
## PRODUCTION
##
###########################

LOG_FILE=./log/build.log

build:
	echo "Start building" >$(LOG_FILE)
	date >>$(LOG_FILE)
	$(CBD) configure
	$(CBD) build 1>>$(LOG_FILE) 2>&1
	date >>$(LOG_FILE)
	echo "End building" >>$(LOG_FILE)

rebuild: clean build

##
##       1. create new dir _sites
##       2. compress HTML
##       3. combine & compress JS; replace related links in templates.
##       4. generate main.css via lessc; replace related links in templates.
##       5. [ ] md5sum
##

create-site:
	rm -rf $(SITE)
	mkdir -p $(SITE)/log
	mkdir -p $(SITE)/static/css
	cp Makefile $(SITE)/
	cp devel.cfg $(SITE)/prod.cfg
	cp -r snaplets data $(SITE)
	cp -r static/img $(SITE)/static/img
	cp -r static/js $(SITE)/static/js

	cd $(SITE)/static/js && for x in *.js ; do \
		uglifyjs $$x > $$x.min.js ; \
		mv -f $$x.min.js $$x ; \
	done
	cd $(SITE)/static/js/libs && for x in *.js ; do \
		uglifyjs $$x > $$x.min.js ; \
		mv -f $$x.min.js $$x ; \
	done

	lessc --compress static/less/bootstrap.less > $(SITE)/static/css/main.css
	lessc --compress static/less/responsive.less > $(SITE)/static/css/responsive.css
	cp -f $(SITE)/snaplets/heist/templates/_layout-css-prod.tpl $(SITE)/snaplets/heist/templates/_layout-css.tpl

	for x in `find $(SITE)/ -name '*.tpl' ` ; do \
		perl -i -p -e  's/[\r\n]+|[ ]{2}|<!--(.|\s)*?--.*>//gs' $$x ; \
		perl -i -p -e  's/<!--(.|\s)*?-->//gs' $$x ; \
	done

	cp $(PROG_PREV) $(SITE)


prod:
	cd $(SITE) && $(PROG_NAME) -p 9900 -e prod

## ?? TODO: use --address=a.haskellcn.org
a.hcn:
	$(PROG_NAME) -p 9900 -e prod --no-access-log


#######################
