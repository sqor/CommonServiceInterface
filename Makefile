
PROJECT = csi
PROJECT_DESCRIPTION = Common Service Interface

SQOR_DEPS =
DEPS = lager ${SQOR_DEPS}

BUILD_ENV ?= dev

APPNAME = $(shell erl -noinput -eval 'begin {ok, List} = file:consult("relx.config"), {_, {AppName, Version},_} = proplists:lookup(release, List), io:format("~p~n", [AppName]), halt(0) end.')
VERSION = $(shell erl -noinput -eval 'begin {ok, List} = file:consult("relx.config"), {_, {AppName, Version},_} = proplists:lookup(release, List), io:format("~s~n", [Version]), halt(0) end.')

rel::
	$(gen_verbose) config/generate-config.sh \
		--build-env ${BUILD_ENV}

include erlang.mk


