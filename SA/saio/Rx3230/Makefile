#ident "$Header: Makefile,v 1.2.8.1 90/07/18 16:23:56 huang Exp $"
#
# ..../saio/Rx3230/Makefile -- Makefile to build the Rx3230 saio
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake clean clobber depend:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for saio/Rx3230/$$i XXXXXX; \
		(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
	done
	@echo saio/Rx3230 make $@ done

