SHELL       := /bin/sh
BUILD       := bin/build
DEPLOY      := bin/deploy
DOCS        := bin/docs
UPREADME    := bin/update-readme

VERSION     ?= 2.2.0
GROUP       ?= io.randomseed
APPNAME     ?= bankster
DESCRIPTION ?= Money as data, done right.
URL         ?= https://randomseed.io/software/$(APPNAME)/
SCM         ?= github.com/randomseed-io/$(APPNAME)
AOTNS       ?= '[io.randomseed.bankster]'

POMFILE     := pom.xml
JARNAME     := $(APPNAME)-$(VERSION).jar
JARFILE     := target/$(APPNAME)-$(VERSION).jar
DOCPREFIX   := $(GROUP)/$(APPNAME)

.PHONY: default lint doc docs push-docs readme readers
.PHONY: test test-full test-clj
.PHONY: sync-pom pom jar
.PHONY: deploy sig tag clean

default: docs

lint:
	@bin/lint

readme:
	@echo "[readme]   -> README.md"
	@$(UPREADME) "$(DOCPREFIX)" "$(VERSION)" README.md

docs: readme
	@echo "[doc]      -> docs/"
	@echo "# Introduction" > doc/10_introduction.md
	@tail -n +2 README.md >> doc/10_introduction.md
	@perl -pi -e 's/\[([^\]]+)\]\(API\.md\)/[$$1](12_api.md)/g; s/\[([^\]]+)\]\(CONTRACTS\.md\)/[$$1](15_contracts.md)/g' doc/10_introduction.md
	@cat     CONTRACTS.md  > doc/15_contracts.md
	@cat           API.md  > doc/12_api.md
	@cat          LICENSE  > doc/LICENSE
	@$(DOCS) :version '"$(VERSION)"' :description '"$(DESCRIPTION)"'

doc: docs

readers:
	@echo "[readers]  -> src/"
	@bin/readers

push-docs:
	git subtree push --prefix=docs docs main

test:
	@rm -rf .cpcache || true
	@bin/test --no-profiling

test-full:
	@rm -rf .cpcache || true
	@bin/test-full

test-clj: test

sync-pom:
	@echo "[pom]      -> $(POMFILE)"
	@$(BUILD) sync-pom                  \
	  :group       "\"$(GROUP)\""       \
	  :name        "\"$(APPNAME)\""     \
	  :version     "\"$(VERSION)\""     \
	  :description "\"$(DESCRIPTION)\"" \
	  :scm         "\"$(SCM)\""         \
	  :url         "\"$(URL)\""

pom: clean sync-pom

jar: readers pom
	@echo "[jar]      -> $(JARNAME)"
	@rm -rf target/classes .cpcache || true
	@rm -f $(JARFILE) || true
	@$(BUILD) jar               \
	  :group   "\"$(GROUP)\""   \
	  :name    "\"$(APPNAME)\"" \
	  :version "\"$(VERSION)\"" \
	  :aot-ns  $(AOTNS)

sig:
	@echo "[sig]      -> $(POMFILE).asc"
	@rm -f "$(POMFILE).asc" || true
	@gpg2 --armor --detach-sig "$(POMFILE)"

release: test clean readers docs jar

deploy: clean readers pom jar
	@echo "[deploy]   -> $(GROUP)/$(APPNAME)-$(VERSION)"
	@test -f "$(JARFILE)" || (echo "Missing $(JARFILE)"; exit 1)
	@test -f "$(POMFILE)" || (echo "Missing $(POMFILE)"; exit 1)
	@$(DEPLOY) deploy :pom-file "\"$(POMFILE)\"" :artifact "\"$(JARFILE)\""
	@test -f "$(APPNAME)-$(VERSION).pom.asc" && mv -f "$(APPNAME)-$(VERSION).pom.asc" "sigs/" || true
	@test -f "target/$(APPNAME)-$(VERSION).pom.asc" && mv -f "target/$(APPNAME)-$(VERSION).pom.asc" "sigs/" || true
	@test -f "target/$(APPNAME)-$(VERSION).jar.asc" && mv -f "target/$(APPNAME)-$(VERSION).jar.asc" "sigs/" || true

tag:
	git tag -s "$(VERSION)" -m "Release $(VERSION)"

clean:
	@rm -f target/*.jar "$(POMFILE).asc" || true
	@rm -rf target/classes target/tmp .cpcache || true
	@find . -name .DS_Store -print0 | xargs -0 rm -f
