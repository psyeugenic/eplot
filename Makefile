ERL  ?= erl
APP  := eplot

.PHONY: deps test install

all: escript
	
escript: build
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

INSTALL      = /usr/bin/install -c
INSTALL_DIR  = /usr/bin/install -c -d
INSTALL_DATA = /usr/bin/install -m 644
prefix       = /usr/local

ifeq ($(DESTDIR),)
	RELEASE_DIR = $(prefix)
else
	RELEASE_DIR = $(DESTDIR)/$(prefix)
endif

install: install_escript

install_escript: escript
	$(INSTALL_DIR)  $(RELEASE_DIR)/bin
	$(INSTALL)      bin/eplot $(RELEASE_DIR)/bin

#RELEASE_LIB_DIR = $(RELEASE_DIR)/lib/erlang/lib/eplot-$(VSN)
#TARGETS = $(shell echo ebin/*.beam)
#
#install_app: escript
#	$(INSTALL_DIR)  $(RELEASE_LIB_DIR)/ebin
#	$(INSTALL)      $(TARGETS) $(RELEASE_LIB_DIR)/ebin
#	$(INSTALL_DIR)  $(RELEASE_LIB_DIR)/bin
#	$(INSTALL)      bin/eplot $(RELEASE_LIB_DIR)/bin

