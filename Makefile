SH = /bin/sh

TESTGUILE = ./run-single-test
PROVE = prove --merge --verbose --color -e '$(TESTGUILE)'

all:
	@printf 'Use either "make doc" or "make test".\n'

compile:
	(cd scheme/test && guild compile -o tap.go tap.scm;)

doc:
	@(cd doc && $(MAKE) all;)

test:
	$(PROVE) test/*.scm

failures:
	$(PROVE) examples/*.scm || true

install:
	@$(SH) ./install

clean:
	@(cd doc && $(MAKE) clean;)

.PHONY: all clean doc failures install test
