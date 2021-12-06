current_dir := $(shell pwd)
name := sy-datascript

.PHONY: all

upgrade-node-deep-deps:
	rm -rf node_modules
	rm -f package-lock.json
	npm install

upgrade-cljs-deps:
	clojure -M:sy-datascript:outdated --upgrade

watch:
	rm -f sy-datascript.js
	rm -f sy-datascript.js.map
	./node_modules/.bin/shadow-cljs watch sy-datascript

repl:
	node ./sy-datascript.js
