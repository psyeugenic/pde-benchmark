ERL  ?= erl
APP  := pde_benchmark
REBAR = rebar

.PHONY: deps test install

all: escript
	
escript: build
	@./rebar escriptize

build: deps
	@./rebar compile

deps: $(REBAR) Makefile
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test:
	@./rebar ct

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

$(REBAR):
	@(wget --output-document=$(REBAR) http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x $(REBAR))

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

