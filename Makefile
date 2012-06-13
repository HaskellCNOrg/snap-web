
PROG_PREV = ./dist/build/ahaskellcnorg/ahaskellcnorg
EXTRA_TEST_LIB = ./dist/build/ahaskellcnorg/ahaskellcnorg-tmp
DIST=dist

default: build-dev

clean:
	rm -rf $(DIST)

hlint:
	hlint src/ tests/ --report

###########################
## DEVELOPMENT
## 
###########################
conf-dev:
	cabal --flags="development" configure

build-dev: conf-dev
	cabal -v build

test:
	cabal --enable-tests configure
	cabal build
	cabal test

p:
	$(PROG_PREV) -p 9900 @dev

bp: build-dev p
rp: clean build-dev p

###########################
## PRODUCTION
## Maybe TODO: (third party or all in Haskell?)
##       0. Maybe enhance Heist in order to compress HTML. it is due to parseHTML in xmlhtml.
##       1. create new dir _sites
##       2. combine & compress JS; replace related links in templates.
##       3. generate main.css via lessc; replace related links in templates.
##
###########################

build: hlint
	cabal configure
	cabal build

rebuild: clean build

preview:
	$(PROG_PREV) -p 9900 @prod

####################### Doc

doc:
	cabal haddock --executable
