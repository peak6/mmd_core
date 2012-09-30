EBIN_DIR = ebin
EBIN_DIRS = $(EBIN_DIR) $(wildcard deps/*/ebin) $(wildcard apps/*/ebin)

EPA=-pa $(shell echo $(EBIN_DIRS) | sed 's/ / -pa /')

ERL=erl $(EPA) -boot start_sasl -sasl errlog_type error

REBAR=./rebar


all: compile

shell: compile
	$(ERL) -sname shell

mmd: compile
	deps/p6core/priv/scontrol -e dev -a mmd_core -c example/mmd.cfg  debug

mmd2: compile
	deps/p6core/priv/scontrol -e dev -a mmd_core -n mmd2_core -c example/mmd2.cfg  debug

compile:
	$(REBAR) compile

.PHONY: deps test

deps:
	$(REBAR) get-deps
	$(REBAR) update-deps

test: compile
	$(REBAR) eunit

xref: compile
	./rebar xref

doc: compile
	./rebar skip_deps=true doc

clean:
	./rebar clean
	rm -rf release
	find . -name '*~' -exec rm {} \;
	find . -name erl_crash.dump -exec rm {} \;

