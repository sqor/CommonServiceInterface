.PHONY: all init compile rel apps clean distclean rel run

all: init compile

CURR_DIR = $(shell pwd)
DEPS_DIR ?= $(CURR_DIR)/deps
DEPS = $(DEPS_DIR)
$(DEPS_DIR):
	mkdir $(DEPS_DIR)

APPS = 

# All the static upfront dependencies we need for the project
init: $(DEPS) \
	$(DEPS)/lager


# include the proper deps, per BUILD_ENV
BUILD_ENV ?= development
ENV = $(BUILD_ENV)
ifneq ("$(wildcard build_deps_$(BUILD_ENV).mk)","")
	include build_deps_$(BUILD_ENV).mk
else
	include build_deps_development.mk
endif


ebin:
	mkdir ebin

APPSRC_FILES = $(shell find src -name "*.app.src")
APP_FILES = $(subst src/,ebin/,$(subst .src,,$(APPSRC_FILES)))

$(APP_FILES): $(APPSRC_FILES)
	cp $(subst ebin/,src/,$@).src $@

compile: init Emakefile ebin $(APP_FILES)
	erl -noinput -pa deps/*/ebin -eval '$(ERL_MAKE)'

ERL_SOURCES=$(shell ls src/*.?rl)

Emakefile: $(ERL_SOURCES)
	echo "{[\"src/*\"], [{outdir, \"ebin\"}]}." > Emakefile

ERL_MAKE=case make:all([ $(ERL_MAKE_OPTS) ]) of up_to_date -> halt(0); error -> halt(1) end.

ERL_MAKE_OPTS=debug_info, report, {i, "deps"}, {parse_transform, lager_transform}

distclean: clean
	-rm -rf $(DEPS)

clean:
	-rm -rf ebin
	-rm -rf scripts
	-rm Emakefile
	-rm -rf logs
	-rm `find . -name '*.beam' | grep -v deps`
	-rm -rf _rel

rel: scripts \
	scripts/relx \
	_rel

scripts:
	mkdir scripts

scripts/relx: scripts
	@# Must follow redirects (-L), since github returns those
	curl -L -o $@ https://github.com/erlware/relx/releases/download/v3.4.0/relx
	chmod +x $@

_rel: compile relx.config
	scripts/relx -o _rel

test: compile
	-mkdir -p logs
	PWD=`pwd` ct_run \
		-pa deps/*/ebin ebin \
		-include $$PWD/deps/ \
		-dir test \
		-logdir logs \
		-ct_hooks cth_surefire "[{path,\"$$PWD/test/logs/report.xml\"}]" \
		-erl_args -noshell \
			-setcookie test/cookie


APPNAME = $(shell erl -noinput -eval 'begin {ok, List} = file:consult("relx.config"), {_, {AppName, Version},_} = proplists:lookup(release, List), io:format("~p~n", [AppName]), halt(0) end.')
VERSION = $(shell erl -noinput -eval 'begin {ok, List} = file:consult("relx.config"), {_, {AppName, Version},_} = proplists:lookup(release, List), io:format("~s~n", [Version]), halt(0) end.')

run: rel
	_rel/$(APPNAME)/bin/$(APPNAME) console




PROJECT = csi
#DEPS = lager

#include erlang.mk

# Compile flags
# ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}' -Dlager -Ddebug +debug_info
#ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}' -Dlager +debug_info

# Use the same settings for compiling releases as well as for testing
#ERLC_OPTS= $(ERLC_COMPILE_OPTS)
#TEST_ERLC_OPTS= $(ERLC_COMPILE_OPTS)
