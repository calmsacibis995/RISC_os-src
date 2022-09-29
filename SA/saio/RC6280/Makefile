#ident "$Header: Makefile,v 1.2.8.1 90/07/18 16:29:29 huang Exp $"
#
# ..../saio/RC6280/Makefile -- Makefile to build the RC6280 saio
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake clean clobber depend:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for saio/RC6280/$$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done
	@echo saio/RC6280 make $@ done

