ERL  ?= erl
APP  := eplot
LINK := ln -s -f

.PHONY: deps test

all: build
	@./rebar escriptize

build: deps
	@./rebar compile

deps: Makefile
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

test:
	@bin/eplot_test rendertest

#INSTALL      = /usr/bin/install -c
#INSTALL_DIR  = /usr/bin/install -c -d
#INSTALL_DATA = /usr/bin/install -m 644
#prefix       = /usr/local
#
#ifeq ($(DESTDIR),)
#	RELEASE_DIR = $(prefix)
#else
#	RELEASE_DIR = $(DESTDIR)/$(prefix)
#endif
#
#
#RELEASE_LIB_DIR = $(RELEASE_DIR)/lib/erlang/lib/eplot-$(VSN)
#
#TARGETS = $(MODULES:%=$(EBIN)/%.beam) vsn.mk
#
#
#install : build $(EXAMPLE)
#	$(INSTALL_DIR)  $(RELEASE_LIB_DIR)/ebin
#	$(INSTALL)      $(TARGETS) $(RELEASE_LIB_DIR)/ebin
#	$(INSTALL_DIR)  $(RELEASE_LIB_DIR)/example
#	$(INSTALL_DATA) $(DATAFILES) $(RELEASE_LIB_DIR)/example
#	$(INSTALL_DIR)  $(RELEASE_LIB_DIR)/bin
#	$(INSTALL)      bin/eplot $(RELEASE_LIB_DIR)/bin
#	$(INSTALL_DIR)  $(RELEASE_DIR)/bin
#	$(LINK)         $(RELEASE_LIB_DIR)/bin/eplot $(RELEASE_DIR)/bin/eplot
