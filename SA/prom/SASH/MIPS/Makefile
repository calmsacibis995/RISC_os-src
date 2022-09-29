#ident "$Header: Makefile,v 1.5 90/10/10 15:07:39 huang Exp $"
#
# ..../prom/SASH/MIPS/Makefile -- Makefile to build the SASH MIPS
#

SHELL=/bin/sh
MAKE=/bin/make

include ../../../Mkinc.release
include ../../../Mkinc.common_defines

default: all

all remake sash clean clobber depend install:
	@for i in $(BINTYPES); \
	do \
		echo XXXXXX Make $@ for prom/SASH/MIPS/SASH_$$i XXXXXX; \
		if [ -d SASH_$$i ]; \
		then \
			[ -r SASH_$$i/Makedepend ] || touch SASH_$$i/Makedepend; \
			(cd SASH_$$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING: $$i does not exist: no actions taken; \
		fi \
	done
	@echo prom/SASH/MIPS make $@ done


install_umips:
	@for i in $(UMIPS_BINTYPES); \
	do \
		echo XXXXXX Make $@ for prom/SASH/MIPS/SASH_$$i XXXXXX; \
		if [ -d SASH_$$i ]; \
		then \
			(cd SASH_$$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING: $$i does not exist: no actions taken; \
		fi \
	done
	@echo prom/SASH/MIPS make $@ done

clean_umips:
	@for i in $(UMIPS_BINTYPES); \
	do \
		echo XXXXXX Make $@ for prom/SASH/MIPS/SASH_$$i XXXXXX; \
		if [ -d SASH_$$i ]; \
		then \
			(cd SASH_$$i; $(MAKE) clean DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING: $$i does not exist: no actions taken; \
		fi \
	done
	@echo prom/SASH/MIPS make $@ done

clobber_umips:
	@for i in $(UMIPS_BINTYPES); \
	do \
		echo XXXXXX Make $@ for prom/SASH/MIPS/SASH_$$i XXXXXX; \
		if [ -d SASH_$$i ]; \
		then \
			(cd SASH_$$i; $(MAKE) clobber DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING: $$i does not exist: no actions taken; \
		fi \
	done
	@echo prom/SASH/MIPS make $@ done
