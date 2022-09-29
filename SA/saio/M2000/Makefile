#ident "$Header: Makefile,v 1.1.8.1 90/07/18 16:22:23 huang Exp $"
#
# ..../saio/M2000/Makefile -- Makefile to build the M2000 saio
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake clean clobber depend:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for saio/M2000/$$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done
	@echo saio/M2000 make $@ done

