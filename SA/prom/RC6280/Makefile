#ident "$Header: Makefile,v 1.2.8.1 90/07/18 15:54:05 huang Exp $"
#
# ..../prom/RC6280/Makefile -- Makefile to build the RC6280 proms
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake prom clean clobber depend:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for prom/RC6280/$$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done
	@echo prom/RC6280 make $@ done
