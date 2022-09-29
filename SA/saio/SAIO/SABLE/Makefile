#ident "$Header: Makefile,v 1.4 90/10/10 16:14:49 huang Exp $"
#
# ..../saio/SAIO/SABLE/Makefile -- Makefile to build the SAIO saio
#

SHELL=/bin/sh
MAKE=/bin/make

include ../../../Mkinc.release
include ../../../Mkinc.common_defines

default: all

all remake clean clobber depend install:
	@for i in $(BINTYPES); \
	do \
		echo XXXXXX Make $@ for saio/SAIO/SABLE/SAIO_$$i XXXXXX;\
		if [ -d SAIO_$$i ]; \
		then \
			[ -r SAIO_$$i/Makedepend ] || touch SAIO_$$i/Makedepend; \
			(cd SAIO_$$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no actions taken; \
		fi \
	done
	@echo saio/SAIO/SABLE make $@ done


