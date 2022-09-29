#
# $Header: Makefile,v 1.5.1.3.1.1.1.2 90/11/29 16:11:41 beacker Exp $
# ---------------------------------------------------
# | Copyright (c) 1987 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#

SHELL	=/bin/sh

SUBDIRS	=nsl libmips libPW libbsd libc libcrypt libcurses libdbm libgen libl \
	libmalloc libmalloccheck \
	libsec \
	librpcsvc libsec libsun liby terminfo

SPECLIST=libbsd libc
SPECARGS= LIBSUFFIX=_G0 GNUM="-G 0"

all clobber clean generic:
	@for dir in $(SUBDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir XXXXXXX" ;\
			( cd $$dir ; $(MAKE) $(MAKEARGS) $@ ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}

install:
	@for dir in $(SUBDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir XXXXXXX" ;\
			( cd $$dir ; $(MAKE) $(MAKEARGS) install ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}
	@for dir in $(SPECLIST) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir (special) XXXXXXX" ;\
			( cd $$dir ; $(MAKE) clobber ;\
			  $(MAKE) $(MAKEARGS) $(SPECARGS) install ;\
			  $(MAKE) clobber ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}
