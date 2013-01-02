
CBD=cabal-dev
sandbox=--sandbox=../../cabal-dev

default: build

clean:
	rm -rf dist
	rm -rf cabal-dev

build:
	$(CBD) $(sandbox) configure
	$(CBD) $(sandbox) build

migrate:
	./dist/build/migrate/migrate
