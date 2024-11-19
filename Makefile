EXTENSION    = $(shell grep -m 1 '"name":' META.json | \
               sed -e 's/[[:space:]]*"name":[[:space:]]*"\([^"]*\)",/\1/')
EXTVERSION   = $(shell grep -m 1 '[[:space:]]\{8\}"version":' META.json | \
               sed -e 's/[[:space:]]*"version":[[:space:]]*"\([^"]*\)",\{0,1\}/\1/')
DISTVERSION  = $(shell grep -m 1 '[[:space:]]\{3\}"version":' META.json | \
               sed -e 's/[[:space:]]*"version":[[:space:]]*"\([^"]*\)",\{0,1\}/\1/')

MODULEDIR    = $(EXTENSION)
DESCRIPTION  = A Postgres data type for the Semantic Version format with support for btree and hash indexing.
REPO_URL     = https://github.com/theory/pg-semver
DATA         = $(wildcard sql/*--*.sql)
DOCS         = $(wildcard doc/*.mmd)
TESTS        = $(wildcard test/sql/*.sql)
REGRESS      = $(patsubst test/sql/%.sql,%,$(TESTS))
REGRESS_OPTS = --inputdir=test
MODULES      = $(patsubst %.c,%,$(wildcard src/*.c))
PG_CONFIG   ?= pg_config
EXTRA_CLEAN  = sql/$(EXTENSION)--$(EXTVERSION).sql
PG92         = $(shell $(PG_CONFIG) --version | grep -qE " 8\.| 9\.0| 9\.1" && echo no || echo yes)

ifeq ($(PG92),no)
$(error $(EXTENSION) requires PostgreSQL 9.2 or higher)
endif

PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
include ./trunk.mk

all: sql/$(EXTENSION)--$(EXTVERSION).sql

sql/$(EXTENSION)--$(EXTVERSION).sql: sql/$(EXTENSION).sql
	cp $< $@

.PHONY: results
results:
	rsync -avP --delete results/ test/expected

dist:
	git archive --format zip --prefix=$(EXTENSION)-$(DISTVERSION)/ -o $(EXTENSION)-$(DISTVERSION).zip HEAD

latest-changes.md: Changes
	perl -e 'while (<>) {last if /^(v?\Q${DISTVERSION}\E)/; } print "Changes for v${DISTVERSION}:\n"; while (<>) { last if /^\s*$$/; s/^\s+//; print }' Changes > $@

# Temporary fix for PostgreSQL compilation chain / llvm bug, see
# https://github.com/rdkit/rdkit/issues/2192
COMPILE.cxx.bc = $(CLANG) -xc++ -Wno-ignored-attributes $(BITCODE_CPPFLAGS) $(CPPFLAGS) -emit-llvm -c
%.bc : %.cpp
	$(COMPILE.cxx.bc) -o $@ $<
	$(LLVM_BINPATH)/opt -module-summary -f $@ -o $@
