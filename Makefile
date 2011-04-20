EXTENSION=semver
EXTVERSION=0.1.2

DATA = $(filter-out $(wildcard sql/*--*.sql),$(wildcard sql/*.sql))
DOCS = $(wildcard doc/*.txt)
TESTS = $(wildcard test/sql/*.sql)
REGRESS = $(patsubst test/sql/%.sql,%,$(TESTS))
REGRESS_OPTS = --inputdir=test --load-language=plpgsql
MODULES = $(patsubst %.c,%,$(wildcard src/*.c))

PG_CONFIG = pg_config

VERSION     = $(shell $(PG_CONFIG) --version | awk '{print $$2}')
PGVER_MAJOR = $(shell echo $(VERSION) | awk -F. '{ print ($$1 + 0) }')
PGVER_MINOR = $(shell echo $(VERSION) | awk -F. '{ print ($$2 + 0) }')

ifeq ($(PGVER_MAJOR), 9)
ifneq ($(PGVER_MINOR), 0)
all: sql/$(EXTENSION)--$(EXTVERSION).sql

sql/$(EXTENSION)--$(EXTVERSION).sql: sql/$(EXTENSION).sql
	cp $< $@

DATA = $(wildcard sql/*--*.sql) sql/$(EXTENSION)--$(EXTVERSION).sql
EXTRA_CLEAN = sql/$(EXTENSION)--$(EXTVERSION).sql
endif
endif

PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
