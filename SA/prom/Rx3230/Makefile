#ident "$Header: Makefile,v 1.2.8.1 90/07/18 15:48:43 huang Exp $"
#
# ..../prom/Rx3230/Makefile -- Makefile to build the Rx3230 proms
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake prom clean clobber depend:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for prom/Rx3230/$$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done
	@echo prom/Rx3230 make $@ done
