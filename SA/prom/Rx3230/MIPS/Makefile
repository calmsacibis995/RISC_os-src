#ident "$Header: Makefile,v 1.3.2.1 90/07/18 15:48:54 huang Exp $"
#
# ..../prom/Rx3230/MIPS/Makefile -- Makefile to build the Rx3230 proms
#

SHELL=/bin/sh
MAKE=/bin/make

include ../../../Mkinc.release
include ../../../Mkinc.common_defines

default: all

all remake prom clean clobber depend:
	@for i in $(BINTYPES); \
	do \
		echo XXXXXX Make $@ for prom/Rx3230/MIPS/PROM_$$i XXXXXX; \
		if [ -d PROM_$$i ]; \
		then \
                        [ -r PROM_$$i/Makedepend ] || touch PROM_$$i/Makedepend; \
			(cd PROM_$$i; $(MAKE) $@ $(MAKEARGS)); \
		else \
			echo XXXXXX WARNING $$i not supported: no actions taken XXXXXX; \
		fi \
	done
	@echo prom/Rx3230/MIPS make $@ done
