
PROG_PREV = ./dist/build/ahaskellcnorg/ahaskellcnorg
DIST=dist

default: build-dev

clean:
	rm -rf $(DIST)

hlint:
	hlint src/ --report

###########################
## DEVELOPMENT
## 
###########################
conf-dev:
	cabal --flags="development" configure

build-dev: conf-dev
	cabal build
	$(hlint)

bp: build-dev preview
rp: clean build-dev preview

###########################
## PRODUCTION
## Maybe TODO: (third party or all in Haskell?)
##       0. Maybe enhance Heist in order to compress HTML. it is due to parseHTML in xmlhtml.
##       1. create new dir _sites
##       2. combine & compress JS; replace related links in templates.
##       3. generate main.css via lessc; replace related links in templates.
##
###########################

build:
	cabal configure
	cabal build
	$(hlint)

rebuild: clean build

####################### preview
preview:
	$(PROG_PREV) -p 9900

p: preview

####################### Doc

doc:
	cabal haddock --executable
