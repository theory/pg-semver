# Trunk packaging. To use, create a standard PGXS Makefile for your extension.
# The only extra variables are DISTVERSION, LICENSE, and LANGUAGE.
#
# ``` make
# DISTVERSION  = 1.2.1
# LICENSE      = mit
# LANGUAGE     = c
#
# EXTENSION    = bike
# MODULEDIR    = $(EXTENSION)
# DATA         = $(wildcard sql/*.sql)
# DOCS         = $(wildcard doc/*.mmd)
# MODULES      = $(patsubst %.c,%,$(wildcard src/*.c))
# PG_CONFIG   ?= pg_config
#
# PGXS := $(shell $(PG_CONFIG) --pgxs)
# include $(PGXS)
# include ./trunk.mk
# ```
#
# Then build it:
# 
# ``` sh
# make trunk
# ```
#
# This will create a file with a name similar to
#
# bike-1.2.1+pg16-darwin-23.5.0-arm64
#
# Requires:
#
# * pg_config
# * PGXS (from `pg_config --pgxs`)
# * tar
# * shasum
# * jq

LAUNGUAGE      ?= c
LICENSE        ?= PostgreSQL

pkg_arch       := $(shell uname -m)
ifeq ($(pkg_arch),x86_64)
pkg_arch       := amd64
endif
ifeq ($(PORTNAME),darwin)
pkg_os_ver     := $(shell uname -r)
pkg            := $(EXTENSION)-$(DISTVERSION)+pg$(MAJORVERSION)-$(PORTNAME)-$(pkg_os_ver)-$(pkg_arch)
else
pkg            := $(EXTENSION)-$(DISTVERSION)+pg$(MAJORVERSION)-$(PORTNAME)-$(pkg_arch)
endif
pkg_dir 	   := $(pkg)
pkg_installdir := $(pkg_dir)/pgsql
pkg_sharedir   := $(pkg_installdir)/share
pkg_libdir     := $(pkg_installdir)/lib
pkg_pkglibdir  := $(pkg_installdir)/pkglib
pkg_docdir     := $(pkg_installdir)/doc/$(docmoduledir)
pkg_bindir     := $(pkg_installdir)/bin
pkg_incdir     := $(pkg_installdir)/include/server/$(docmoduledir)
ifdef SO_MAJOR_VERSION
pkg_pkgconfigdir = $(pkg_libdir)/pkgconfig
endif
pkg_info_files ?= $(wildcard README* readme* Readme* LICENSE* license* License* CHANGE* change* Change*)
EXTRA_CLEAN    += $(EXTENSION)-$(DISTVERSION)+*/

# Phony target to create the trunk and OCI JSON files.
trunk: $(pkg).trunk

$(pkg).trunk: package
	tar zcvf $@ $(pkg_dir)

# Use jq to build trunk.json. XXX Add dependencies
$(pkg_dir)/trunk.json: pkg_dirs
	@pg=$$(jq -n \
		--arg version "$(VERSION)" \
		--arg major "$(MAJORVERSION)" \
		--argjson number "$(VERSION_NUM)" \
		--arg libs "$(LIBS)" \
		--arg cppflags "$(CPPFLAGS)" \
		--arg cflags "$(CFLAGS)" \
		--arg ldflags "$(LDFLAGS)" \
		'$$ARGS.named' \
	) && pkg=$$(jq -n \
		--arg name "$(EXTENSION)" \
		--arg version $(DISTVERSION) \
		--arg language "$(LAUNGUAGE)" \
		--arg license "$(LICENSE)" \
		'$$ARGS.named' \
	) && plat=$$(jq -n \
		--arg os "$(PORTNAME)" \
		--arg version "$(pkg_os_ver)" \
		--arg arch $(pkg_arch) \
		'$$ARGS.named | with_entries(select(.value |. !=null and . != ""))' \
	) && jq -n \
		--arg trunk 0.1.0 \
		--argjson package "$$pkg" \
		--argjson postgres "$$pg" \
		--argjson platform "$$plat" \
		'$$ARGS.named' > $@


# Based on install in https://github.com/postgres/postgres/blob/REL_17_BETA1/src/makefiles/pgxs.mk#L237C1-L276
package: all pkg_dirs $(pkg_dir)/trunk.json
ifneq (,$(EXTENSION))
	$(INSTALL_DATA) $(addprefix $(srcdir)/, $(addsuffix .control, $(EXTENSION))) '$(pkg_sharedir)/extension/'
endif # EXTENSION
ifneq (,$(DATA)$(DATA_built))
	$(INSTALL_DATA) $(addprefix $(srcdir)/, $(DATA)) $(DATA_built) '$(pkg_sharedir)/$(datamoduledir)/'
endif # DATA
ifneq (,$(DATA_TSEARCH))
	$(INSTALL_DATA) $(addprefix $(srcdir)/, $(DATA_TSEARCH)) '$(pkg_sharedir)/tsearch_data/'
endif # DATA_TSEARCH
ifdef MODULES
	$(INSTALL_SHLIB) $(addsuffix $(DLSUFFIX), $(MODULES)) '$(pkg_pkglibdir)/'
ifeq ($(with_llvm), yes)
	$(foreach mod, $(MODULES), $(call package_llvm_module,$(mod),$(mod).bc))
endif # with_llvm
endif # MODULES
ifdef DOCS
ifdef docdir
	$(INSTALL_DATA) $(addprefix $(srcdir)/, $(DOCS)) '$(pkg_docdir)/'
endif # docdir
endif # DOCS
ifdef PROGRAM
	$(INSTALL_PROGRAM) $(PROGRAM)$(X) '$(pkg_bindir)'
endif # PROGRAM
ifdef SCRIPTS
	$(INSTALL_SCRIPT) $(addprefix $(srcdir)/, $(SCRIPTS)) '$(pkg_bindir)/'
endif # SCRIPTS
ifdef SCRIPTS_built
	$(INSTALL_SCRIPT) $(SCRIPTS_built) '$(pkg_bindir)/'
endif # SCRIPTS_built
ifneq (,$(strip $(HEADER_dirs)))
	$(foreach dir,$(HEADER_dirs),$(call package_headers,$(dir),$(HEADER_files_$(dir))))
endif # HEADERS
ifdef MODULE_big
ifeq ($(with_llvm), yes)
	$(call package_llvm_module,$(MODULE_big),$(OBJS))
endif # with_llvm
package: package-lib
endif # MODULE_big
ifneq (,$(pkg_info_files))
	$(INSTALL_DATA) $(addprefix $(srcdir)/, $(pkg_info_files)) '$(pkg_dir)/'
endif
	rm -f "$(pkg_dir)/digests"
	cd "$(pkg_dir)/" && find * -type f | xargs shasum --tag -ba 256 > digests

# Based on installdirs in https://github.com/postgres/postgres/blob/REL_17_BETA1/src/makefiles/pgxs.mk#L279C1-L303
pkg_dirs:
ifneq (,$(EXTENSION))
	$(MKDIR_P) '$(pkg_sharedir)/extension'
endif
ifneq (,$(DATA)$(DATA_built))
	$(MKDIR_P) '$(pkg_sharedir)/$(datamoduledir)'
endif
ifneq (,$(DATA_TSEARCH))
	$(MKDIR_P) '$(pkg_sharedir)/tsearch_data'
endif
ifneq (,$(MODULES))
	$(MKDIR_P) '$(pkg_pkglibdir)'
endif
ifdef DOCS
	$(MKDIR_P) '$(pkg_docdir)'
endif
ifneq (,$(PROGRAM)$(SCRIPTS)$(SCRIPTS_built))
	$(MKDIR_P) '$(pkg_bindir)'
endif
ifdef MODULE_big
pkg_dirs: pkg_dirs-lib
endif # MODULE_big


# Based on https://github.com/postgres/postgres/blob/REL_17_BETA1/src/Makefile.shlib#L364-L399.
.PHONY: package-lib package-lib-static package-lib-shared installdirs-lib
package-lib: package-lib-shared
ifdef soname
package-lib: package-lib-static
package-lib: package-lib-pc
endif

package-lib-pc: lib$(NAME).pc installdirs-lib
	$(INSTALL_DATA) $< '$(pkg_pkgconfigdir)/lib$(NAME).pc'

package-lib-static: $(stlib) installdirs-lib
	$(INSTALL_STLIB) $< '$(pkg_libdir)/$(stlib)'

package-lib-shared: $(shlib) installdirs-lib
ifdef soname
# we don't install $(shlib) on AIX
# (see http://archives.postgresql.org/message-id/52EF20B2E3209443BC37736D00C3C1380A6E79FE@EXADV1.host.magwien.gv.at)
ifneq ($(PORTNAME), aix)
	$(INSTALL_SHLIB) $< '$(pkg_libdir)/$(shlib)'
ifneq ($(PORTNAME), cygwin)
ifneq ($(PORTNAME), win32)
ifneq ($(shlib), $(shlib_major))
	cd '$(pkg_libdir)' && \
	rm -f $(shlib_major) && \
	$(LN_S) $(shlib) $(shlib_major)
endif
ifneq ($(shlib), $(shlib_bare))
	cd '$(pkg_libdir)' && \
	rm -f $(shlib_bare) && \
	$(LN_S) $(shlib) $(shlib_bare)
endif
endif # not win32
endif # not cygwin
endif # not aix
ifneq (,$(findstring $(PORTNAME),win32 cygwin))
	$(INSTALL_SHLIB) $< '$(DESTDIR)$(bindir)/$(shlib)'
endif
else # no soname
	$(INSTALL_SHLIB) $< '$(DESTDIR)$(pkglibdir)/$(shlib)'
endif


# Based on installdirs-lib in https://github.com/postgres/postgres/blob/REL_17_BETA1/src/Makefile.shlib#L402-L407.
pkg_dirs-lib:
ifdef soname
	$(MKDIR_P) '$(pkg_libdir)' '$(pkg_pkgconfigdir)' $(if $(findstring $(PORTNAME),win32 cygwin),'$(pkg_bindir)')
else
	$(MKDIR_P) '$(pkg_pkgconfigdir)'
endif


# Based on install_llvm_module in https://github.com/postgres/postgres/blob/REL_17_BETA1/src/Makefile.global.in#L1090-L1107.
define package_llvm_module
$(MKDIR_P) '$(DESTDIR)${bitcodedir}/$(1)'
$(MKDIR_P) $(sort $(dir $(addprefix '$(pkg_pkglibdir)/bitcode'/$(1)/, $(2))))
$(foreach obj, ${2}, $(INSTALL_DATA) $(patsubst %.o,%.bc, $(obj)) '$(pkg_pkglibdir)/bitcode'/$(1)/$(dir $(obj))
)
cd '$(pkg_pkglibdir)/bitcode' && $(LLVM_BINPATH)/llvm-lto -thinlto -thinlto-action=thinlink -o $(1).index.bc $(addprefix $(1)/,$(patsubst %.o,%.bc, $(2)))
endef


# Based on install_headers in https://github.com/postgres/postgres/blob/REL_17_BETA1/src/makefiles/pgxs.mk#L202-L207
define package_headers
$(MKDIR_P) '$(pkg_incdir)/$(1)/'
$(INSTALL_DATA) $(2) '$(pkg_incdir)/$(1)/'
endef
