all: lint

deploy: lint
	cd .. && $(MAKE) deploy-gh-pages

lint:
	ln -f script.js script.ts
	tsc -t es2016 --noEmit script.ts libs.d.ts
	rm script.ts
