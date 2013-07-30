SH = /bin/sh

all:
	@printf 'Use either "make doc" or "make test".\n'

compile:
	(cd scheme/test && guild compile -o tap.go tap.scm;)

doc:
	@(cd doc && $(MAKE) all;)

test:
	@(cd test && $(MAKE) all;)

install:
	@$(SH) ./install

clean:
	@(cd doc && $(MAKE) clean;)

.PHONY: all clean doc install test
