# repository guidelines

## project structure & module organization
keep clojure sources in `src/sy_datascript` with `core.cljc` hosting the public apis. shared cljs entry code lives in `src/main.cljs`. node-focused tests sit under `src/test` (`core.cljc`, `series.cljc`, `parented_series.cljc`) and mirror the transactional helpers. build artifacts (`sy-datascript.js` and map files) land in the repo root; never commit them.

## build, test, and development commands
run `npm install` once to pull `shadow-cljs`. use `make watch` to compile the node library with the `:local-development` profile and keep outputs current during feature work. `make repl` executes the generated bundle with node for quick smoke checks. upgrade npm dependencies with `make upgrade-node-deep-deps`; refresh clojure deps via `make upgrade-cljs-deps`.

## coding style & naming conventions
prefer two-space indentation and hyphenated var names (`compare-and-swap`). namespaces stay dashed (`sy-datascript.core`) while test namespaces use the `test.*` prefix. when adding docstrings, follow `(fn-name) ;; single-line summary under 120 chars`. run `clojure -M:sy-datascript -m cljfmt.main fix` before committing to keep formatting consistent. keep public apis pure functions returning transactions; use inline comments sparingly and in lowercase.

## testing guidelines
all tests use `clojure.test` with datascript in-memory connections. execute the full suite via `clojure -M:sy-datascript -m clojure.test test.core test.series test.parented-series`. aim to cover new transactional branches and guard clauses; mirror existing naming (`*-test`) for new cases. prefer deterministic data fixtures so node and jvm runs align.

## commit & pull request guidelines
commits follow conventional commits in lowercase, e.g., `feat: add series reorder guard` with a multi-line body describing rationale, risks, and validation. group related changes per commit; avoid bundling formatting-only edits with behavior shifts. prs should explain intent, list manual or automated test commands, and link to any tracking issues. attach cli output or screenshots when behavior affects downstream consumers so reviewers can verify quickly.

## tooling notes for agents
shadow-cljs reads config from `shadow-cljs.edn`; it targets `:sy-datascript` as a node library exporting `main/main`. prefer `clojure -M:sy-datascript` aliases to keep resolver settings uniform, and reuse the provided make targets instead of bespoke scripts to stay consistent across agents.
