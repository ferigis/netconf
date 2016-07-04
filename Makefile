REBAR = $(shell which rebar3 || echo ./rebar3)

ifdef REBAR_PROFILE
PROFILE = $(REBAR_PROFILE)
else
PROFILE = default
endif

BUILD_ROOT = ./_build/$(PROFILE)/lib
BUILD_PATH = $(BUILD_ROOT)/*/ebin

CONFIG = config/sys.config
CT_CONFIG ?= test/test.config

CT_OPTS = -cover test/cover.spec -erl_args -config ${CT_CONFIG}
CT_SUITES = ssh_common_SUITE

.PHONY: all compile clean distclean dialyze tests shell doc

all: compile

compile:
	$(REBAR) compile

dist:
	REBAR_PROFILE=dist $(REBAR) compile

clean:
	rm -rf ebin/* test/*.beam logs log
	$(REBAR) clean

distclean: clean
	$(REBAR) clean --all
	rm -rf _build logs log edoc *.dump c_src/*.o priv/*.so

dialyze:
	$(REBAR) dialyzer

tests: compile
	mkdir -p logs
	ct_run -dir test -suite $(CT_SUITES) -pa $(BUILD_PATH) -logdir logs $(CT_OPTS)
	rm -rf test/*.beam

shell: compile
	erl -pa $(BUILD_PATH) -config $(CONFIG) -s netconf

edoc:
	$(REBAR) edoc
