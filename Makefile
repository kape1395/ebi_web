REBAR=/opt/rebar/bin/rebar
APP=ebi_web

all: compile

deps-get:
	$(REBAR) get-deps
deps:
	$(REBAR) update-deps

deps-local:
	 (cd deps/ebi_core; git pull ../../../ebi_core master )

compile:
	$(REBAR) compile

check: test itest

test: compile
	$(REBAR) eunit apps=$(APP) verbose=1 

itest: compile
	$(REBAR) ct apps=$(APP)

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean apps=ebi_web
	rm -f doc/*.html doc/edoc-info

clean-all:
	$(REBAR) clean
	rm -f doc/*.html doc/edoc-info

.PHONY: all deps deps-get compile check test itest doc clean

