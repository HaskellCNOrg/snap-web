
PROG_PREV = ./dist/build/ahaskellcnorg/ahaskellcnorg
DIST=dist

default: build-dev

clean:
	rm -rf $(DIST)

hlint:
	hlint src/ --report

###########################
## Enable Development mode
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
## production build
## Maybe TODO:
##       0. Maybe enhance Heist in order to compress HTML
##       1. create new dir _sites
##       2. cp files and compress JS/CSS, third party or Haskell?
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
