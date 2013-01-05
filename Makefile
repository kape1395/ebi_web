REBAR=/opt/rebar/bin/rebar
APP=ebi_web

all: compile

deps-get:
	$(REBAR) get-deps
deps:
	$(REBAR) update-deps

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
	$(REBAR) clean
	rm -f doc/*.html doc/edoc-info

.PHONY: all deps deps-get compile check test itest doc clean

