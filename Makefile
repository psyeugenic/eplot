
ERLC   = erlc
ESRC   = src
EBIN   = ebin
BIN    = bin
LINK   = ln -s -f
EFLAGS = +debug_info

MODULES = \
	  egd_colorscheme \
	  egd_chart \
	  eplot_main \
	  eview

DATAFILES = \
	  example/data1.dat \
	  example/data2.dat \
	  example/test1.png

INSTALL      = /usr/bin/install -c
INSTALL_DIR  = /usr/bin/install -c -d
INSTALL_DATA = /usr/bin/install -m 644
prefix       = /usr/local

ifeq ($(DESTDIR),)
	RELEASE_DIR = $(prefix)
else
	RELEASE_DIR = $(DESTDIR)/$(prefix)
endif

all: build

vsn.mk:
	@/bin/sh ./VERSION-GEN
-include vsn.mk

RELEASE_LIB_DIR = $(RELEASE_DIR)/lib/erlang/lib/eplot-$(VSN)

TARGETS = $(MODULES:%=$(EBIN)/%.beam) vsn.mk


build: Makefile $(TARGETS)

$(EBIN)/%.beam : $(ESRC)/%.erl
	$(ERLC) $(EFLAGS) -o $(EBIN) $<

install : build $(EXAMPLE)
	$(INSTALL_DIR)  $(RELEASE_LIB_DIR)/ebin
	$(INSTALL)      $(TARGETS) $(RELEASE_LIB_DIR)/ebin
	$(INSTALL_DIR)  $(RELEASE_LIB_DIR)/example
	$(INSTALL_DATA) $(DATAFILES) $(RELEASE_LIB_DIR)/example
	$(INSTALL_DIR)  $(RELEASE_LIB_DIR)/bin
	$(INSTALL)      bin/eplot $(RELEASE_LIB_DIR)/bin
	$(INSTALL_DIR)  $(RELEASE_DIR)/bin
	$(LINK)         $(RELEASE_LIB_DIR)/bin/eplot $(RELEASE_DIR)/bin/eplot

test: build
	bin/eplot_test rendertest

examples: build
	bin/eplot_test examples

info: 
	@echo "$(TARGETS)"
clean:
	rm -f $(TARGETS)


.PHONY: install clean info test examples
