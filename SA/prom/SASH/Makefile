#ident "$Header: Makefile,v 1.4 90/10/10 15:07:02 huang Exp $"
#
# ..../prom/SASH/Makefile -- Makefile to build the SASH
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake sash clean clobber depend install:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for prom/SASH/$$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo prom/SASH make $@ done

install_umips clean_umips clobber_umips:
	@for i in $(UMIPS_VERSIONS); \
	do \
		echo XXXXXX Make $@ for prom/SASH/$$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo prom/SASH make $@ done
