#ident "$Header: Makefile,v 1.2.2.1 90/07/18 15:50:47 huang Exp $"
#
# ..../prom/M2000/MIPS/Makefile -- Makefile to build the M2000_MIPS proms
#

SHELL=/bin/sh
MAKE=/bin/make

include ../../../Mkinc.release
include ../../../Mkinc.common_defines

default: all

all remake prom clean clobber depend:
	@for i in $(BINTYPES); \
	do \
		echo XXXXXX Make $@ for prom/M2000/MIPS/PROM_$$i XXXXXX; \
		if [ -d PROM_$$i ]; \
		then \
			[ -r PROM_$$i/Makedepend ] || touch PROM_$$i/Makedepend; \
			(cd PROM_$$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo XXXXXX WARNING $$i not supported: no actions taken XXXXXX; \
		fi \
	done
	@echo prom/M2000/MIPS make $@ done
