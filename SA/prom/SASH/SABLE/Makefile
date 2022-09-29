#ident "$Header: Makefile,v 1.4 90/10/10 15:13:54 huang Exp $"
#
# ..../prom/SASH/SABLE/Makefile -- Makefile to build the SASH SABLE
#

SHELL=/bin/sh
MAKE=/bin/make

include ../../../Mkinc.release
include ../../../Mkinc.common_defines

default: all
all remake sash clean clobber depend install install_umips:
	@for i in $(BINTYPES); \
	do \
		echo XXXXXX Make $@ for prom/SASH/SABLE/SASH_$$i XXXXXX;\
		if [ -d SASH_$$i ]; \
		then \
			[ -r SASH_$$i/Makedepend ] || touch SASH_$$i/Makedepend; \
			(cd SASH_$$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no actions taken; \
		fi \
	done
	@echo prom/SASH/SABLE make $@ done
