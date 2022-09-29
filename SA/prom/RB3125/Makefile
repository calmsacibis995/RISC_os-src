#ident "$Header: Makefile,v 1.1.2.1 90/07/18 15:55:40 huang Exp $"
#
# ..../prom/RB3125/Makefile -- Makefile to build the RB3125 proms
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake prom clean clobber depend:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for prom/RB3125/$$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done
	@echo prom/RB3125 make $@ done
