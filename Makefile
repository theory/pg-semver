DATA = $(wildcard sql/*.sql)
DOCS = $(wildcard doc/*.txt)
TESTS = $(wildcard test/sql/*.sql)
REGRESS = $(patsubst test/sql/%.sql,%,$(TESTS))
REGRESS_OPTS = --inputdir=test --load-language=plpgsql
MODULES = $(patsubst %.c,%,$(wildcard src/*.c))

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)

