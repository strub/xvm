# --------------------------------------------------------------------
BIN   := xvmrun xvmlib
JSPKG := jquery\#^2.0.0 bootstrap\#^3.0.0 ace-builds\#^1.2.0

# --------------------------------------------------------------------
UNAMES := $(shell uname -s)
OPEN   := /bin/false

ifeq ($(UNAMES),Linux)
OPEN := xdg-open
else ifeq ($(UNAMES),Darwin)
OPEN := open
endif

# --------------------------------------------------------------------
.PHONY: __force__ default build install uninstall clean mkproper

DUNEOPTS := --display=short

# --------------------------------------------------------------------
default: build

build:
	dune build $(DUNEOPTS)

install:
	dune install $(DUNEOPTS)

uninstall:
	dune uninstall $(DUNEOPTS)

clean:
	dune clean $(DUNEOPTS)

mrproper: clean
	git clean -dfXq

# --------------------------------------------------------------------
.PHONY: webui webui-3rdparty webui-open

html/xvmlib.js: __force__
	rm -f $@ $(@:.js=.map) && dune build $(DUNEOPTS) src/xvmlib.js
	@for i in js map; do [ ! -f _build/default/src/xvmlib.$$i ] || \
	  cp -v _build/default/src/xvmlib.$$i html/; \
	done

webui: html/xvmlib.js
	@true

webui-3rdparty:
	cd html && bower install $(JSPKG)

webui-open:
	cd html && $(OPEN) index.html
