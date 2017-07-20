all: build

build:
	pandoc help.md -o help.html

doc:
	esdoc

deploy:
	cd .. && $(MAKE) deploy-gh-pages

.PHONY: doc deploy
