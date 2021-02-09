current_dir := $(shell pwd)
name := sy-datascript

.PHONY: all

update-node-deep-deps:
	rm -rf node_modules
	rm -f package-lock.json
	npm install

outdated:
	clojure -Aoutdated --aliases outdated --aliases sy-datascript
update-cljs:
	clojure -Aoutdated --aliases outdated --aliases sy-datascript --write

watch:
	rm -f sy-datascript.js
	rm -f sy-datascript.js.map
	./node_modules/.bin/shadow-cljs watch sy-datascript

repl:
	node ./sy-datascript.js
