current_dir := $(shell pwd)
name := sy-datascript

.PHONY: all

update-node-deep-deps:
	rm -rf node_modules
	rm -f package-lock.json
	npm install

outdated:
	clojure -Msy-datascript:outdated --every
update-cljs:
	clojure -Msy-datascript:outdated --every --write

watch:
	rm -f sy-datascript.js
	rm -f sy-datascript.js.map
	./node_modules/.bin/shadow-cljs watch sy-datascript

repl:
	node ./sy-datascript.js
