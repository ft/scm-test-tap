TOPDIR = .

LOAD_PATH = $(TOPDIR)/scheme
TEST_PATH = $(TOPDIR)/test

GUILE_BINARY ?= guile
GUILE_CALL = $(GUILE_BINARY) -L $(LOAD_PATH) -C $(LOAD_PATH) --no-auto-compile
GUILD_BINARY ?= guild

CFLAGS = -Wunsupported-warning -Wunused-variable # -Wunused-toplevel
CFLAGS += -Wunbound-variable -Warity-mismatch -Wduplicate-case-datum
CFLAGS += -Wbad-case-datum -Wformat -L$(LOAD_PATH)

COMPILE = $(GUILD_BINARY) compile $(CFLAGS)

TESTGUILE = ./run-single-test
PROVE = prove --merge --verbose --color -e '$(TESTGUILE)'

MODULES_CORE =  $(TOPDIR)/scheme/test/tap.scm
MODULES_CORE += $(TOPDIR)/scheme/test/tap-harness.scm
MODULES = $(MODULES_CORE)
OBJECTS = ${MODULES:.scm=.go}

.SUFFIXES: .scm .go

all: $(OBJECTS)

.scm.go:
	$(COMPILE) -o $@ $<

doc:
	@(cd doc && $(MAKE) all;)

test:
	$(PROVE) test/*.scm

failures:
	$(PROVE) examples/*.scm || true

install:
	./install

clean:
	find . -name "*.go" -exec rm -f '{}' +
	find . -name "*~" -exec rm -f '{}' +
	(cd doc && $(MAKE) clean;)

.PHONY: all clean doc failures install test
