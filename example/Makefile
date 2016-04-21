
PROJECT = em
PROJECT_DESCRIPTION = Example module using CSI service

SQOR_DEPS = csi
DEPS = lager ${SQOR_DEPS}

dep_csi = git git@github.com:esl/CommonServiceInterface.git  

BUILD_ENV ?= dev

APPNAME = $(shell erl -noinput -eval 'begin {ok, List} = file:consult("relx.config"), {_, {AppName, Version},_} = proplists:lookup(release, List), io:format("~p~n", [AppName]), halt(0) end.')
VERSION = $(shell erl -noinput -eval 'begin {ok, List} = file:consult("relx.config"), {_, {AppName, Version},_} = proplists:lookup(release, List), io:format("~s~n", [Version]), halt(0) end.')

rel::
	$(gen_verbose) config/generate-config.sh \
		--build-env ${BUILD_ENV} \
		--deps csi

include erlang.mk

