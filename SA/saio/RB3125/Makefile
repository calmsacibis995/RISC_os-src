#ident "$Header: Makefile,v 1.1.2.1 90/07/18 16:31:00 huang Exp $"
#
# ..../saio/RB3125/Makefile -- Makefile to build the RB3125 saio
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake clean clobber depend:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for saio/RB3125/$$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done
	@echo saio/RB3125 make $@ done

