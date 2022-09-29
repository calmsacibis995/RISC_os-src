#
# $Header: Makefile,v 1.1.2.1 89/11/28 08:56:33 wje Exp $
#
# ---------------------------------------------------
# | Copyright (c) 1988 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#

SHELL	=/bin/sh

all:
	@cd ../uts ;\
	echo "installing kernel header files..." ;\
	$(MAKE) DESTROOT=$(DESTROOT) install-headers

install: all

clobber:
	@echo "nothing to do"


