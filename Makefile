all:
	@printf 'Use either "make doc" or "make test".\n'

doc:
	@(cd doc && $(MAKE) all;)

test:
	@(cd test && $(MAKE) all;)

clean:
	@(cd doc && $(MAKE) clean;)

.PHONY: all clean doc test
