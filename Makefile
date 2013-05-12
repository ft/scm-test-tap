SH = /bin/sh

all:
	@printf 'Use either "make doc" or "make test".\n'

compile:
	(cd scm && guild compile -o taptest.go taptest.scm;)

doc:
	@(cd doc && $(MAKE) all;)

test:
	@(cd test && $(MAKE) all;)

install:
	@$(SH) ./install

clean:
	@(cd doc && $(MAKE) clean;)

.PHONY: all clean doc install test
