PROJECT = anvil

# dependencies

DEPS = fast_key jsx gun
dep_jsx = pkg://jsx master
dep_gun = pkg://gun master
dep_fast_key = https://github.com/camshaft/fast_key.git master

include erlang.mk

repl: all bin/start
	@bin/start anvil

bin/start:
	@mkdir -p bin
	@curl https://gist.githubusercontent.com/camshaft/372cc332241ac95ae335/raw/start -o $@
	@chmod a+x $@

.PHONY: repl
