#ident "$Header: Makefile,v 1.1.2.1 90/07/18 16:31:30 huang Exp $"
#
# ..../saio/RB3125/SABLE/Makefile -- Makefile to build the RB3125 saio
#

SHELL=/bin/sh
MAKE=/bin/make

include ../../../Mkinc.release
include ../../../Mkinc.common_defines

default: all

all remake clean clobber depend:
	@for i in $(BINTYPES); \
	do \
		echo XXXXXX Make $@ for saio/RB3125/SABLE/PSAIO_$$i XXXXXX; \
		if [ -d PSAIO_$$i ]; \
		then \
			[ -r PSAIO_$$i/Makedepend ] || touch PSAIO_$$i/Makedepend; \
			(cd PSAIO_$$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo XXXXXX WARNING $$i not supported: no actions taken XXXXXX; \
		fi \
	done
	@echo saio/RB3125/SABLE make $@ done


