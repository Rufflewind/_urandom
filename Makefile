all: colorpicker/nobuild.tmp

deploy: dist/.git/config all

	mkdir -p $(<D)/../antialias-area
	(cd antialias-area && pandoc -s --mathjax -M css=style.css -o index.html notes.md)
	ln -f antialias-area/index.html antialias-area/style.css $(<D)/../antialias-area

	mkdir -p $(<D)/../colorpicker
	ln -f colorpicker/index.html colorpicker/script.js colorpicker/style.css $(<D)/../colorpicker

	mkdir -p $(<D)/../ctype
	ln -f ctype/index.html ctype/main.js $(<D)/../ctype

	mkdir -p $(<D)/../loopy-game
	ln -f loopy-game/index.html loopy-game/script.js loopy-game/style.css $(<D)/../loopy-game

	mkdir -p $(<D)/../moon-calendar
	ln -f moon-calendar/index.html $(<D)/../moon-calendar

	mkdir -p $(<D)/../tunic-encoder
	ln -f tunic-encoder/index.html tunic-encoder/script.js tunic-encoder/style.css $(<D)/../tunic-encoder

	mkdir -p $(<D)/../angular-momentum-diagrams
	echo '<meta http-equiv="refresh" content="0;url=/jucys"/>' >$(<D)/../angular-momentum-diagrams/index.html

	cd $(<D)/.. && \
	git add -A && \
	git commit --amend -q -m Autogenerated && \
	git push -f origin HEAD:gh-pages

colorpicker/nobuild.tmp:
	(cd colorpicker && npm run-script build)

dist/.git/config:
	mkdir -p $(@D)
	url=`git remote -v | grep origin | awk '{ printf "%s", $$2; exit }'` && \
	cd $(@D)/.. && \
	git init && \
	git config user.name Bot && \
	git config user.email "<>" && \
	git commit -m _ --allow-empty && \
	git remote add origin "$$url"
