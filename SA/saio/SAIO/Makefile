#ident "$Header: Makefile,v 1.4 90/10/10 16:13:24 huang Exp $"
#
# ..../saio/SAIO/Makefile -- Makefile to build the SAIO saio
#


SHELL=/bin/sh
MAKE=/bin/make

include ../../Mkinc.release
include ../../Mkinc.common_defines

default:	all

all remake clean clobber depend install:
	@for i in $(VERSIONS); \
	do \
		echo XXXXXX Make $@ for saio/SAIO/$$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo saio/SAIO make $@ done

install_umips clobber_umips clean_umips:
	@for i in $(UMIPS_VERSIONS); \
	do \
		echo XXXXXX Make $@ for saio/SAIO/$$i XXXXXX; \
		if [ -d $$i ]; \
		then \
			(cd $$i; $(MAKE) $@ DESTDIR=$(DESTDIR) $(MAKEARGS)); \
		else \
			echo !!!!!! WARNING $$i does not exist: no action taken; \
		fi \
	done
	@echo saio/SAIO make $@ done

