all:

deploy-gh-pages: dist/.git/config all
	mkdir -p $(<D)/../colorpicker
	ln -f colorpicker.html $(<D)/../colorpicker/index.html
	cd $(<D)/.. && \
	git add -A && \
	git commit --amend -q -m Autogenerated && \
	git push -f origin master:gh-pages

dist/.git/config:
	mkdir -p $(@D)
	url=`git remote -v | grep origin | awk '{ printf "%s", $$2; exit }'` && \
	cd $(@D)/.. && \
	git init && \
	git config user.name Bot && \
	git config user.email "<>" && \
	git commit -m _ --allow-empty && \
	git remote add origin "$$url"