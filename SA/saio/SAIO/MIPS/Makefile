#ident "$Header: Makefile,v 1.5 90/10/10 16:14:14 huang Exp $"
#
# ..../saio/SAIO/MIPS/Makefile -- Makefile to build the SAIO/MIPS saio
#

SHELL=/bin/sh
MAKE=/bin/make

include ../../../Mkinc.release
include ../../../Mkinc.common_defines

default: all

all remake clean clobber depend install:
	@for i in $(BINTYPES); \
	do \
		echo XXXXXX Make $@ for saio/SAIO/MIPS/SAIO_$$i XXXXXX; \
		if [ -d SAIO_$$i ]; \
		then \
			[ -r SAIO_$$i/Makedepend ] || touch SAIO_$$i/Makedepend; \
			(cd SAIO_$$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no actions taken; \
		fi \
	done
	@echo saio/SAIO/MIPS make $@ done

install_umips:
	@for i in $(UMIPS_BINTYPES); \
	do \
		echo XXXXXX Make $@ for saio/SAIO/MIPS/SAIO_$$i XXXXXX; \
		if [ -d SAIO_$$i ]; \
		then \
			(cd SAIO_$$i; $(MAKE) install_umips DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no actions taken; \
		fi \
	done

clean_umips:
	@for i in $(UMIPS_BINTYPES); \
	do \
		echo XXXXXX Make $@ for saio/SAIO/MIPS/SAIO_$$i XXXXXX; \
		if [ -d SAIO_$$i ]; \
		then \
			(cd SAIO_$$i; $(MAKE) clean DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no actions taken; \
		fi \
	done

clobber_umips:
	@for i in $(UMIPS_BINTYPES); \
	do \
		echo XXXXXX Make $@ for saio/SAIO/MIPS/SAIO_$$i XXXXXX; \
		if [ -d SAIO_$$i ]; \
		then \
			(cd SAIO_$$i; $(MAKE) clobber DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no actions taken; \
		fi \
	done


