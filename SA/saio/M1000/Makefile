#ident "$Header: Makefile,v 1.1.8.1 90/07/18 16:19:14 huang Exp $"
#
# ..../saio/M1000/Makefile -- Makefile to build the M1000 saio
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake clean clobber depend:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for saio/M1000/$$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done
	@echo saio/M1000 make $@ done

