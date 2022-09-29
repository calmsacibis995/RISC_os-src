#ident "$Header: Makefile,v 1.1.8.1 90/07/18 15:45:09 huang Exp $"
#
# ..../prom/M1000/Makefile -- Makefile to build the M1000 proms
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake prom clean clobber depend:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for prom/M1000/$$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done
	@echo prom/M1000 make $@ done
