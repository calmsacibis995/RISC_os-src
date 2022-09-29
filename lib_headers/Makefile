#
# |-----------------------------------------------------------|
# | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restricted Rights Legend                         |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.3.1.3 90/05/10 02:54:21 wje Exp $
#
SHELL	=/bin/sh
SUBDIRS =../bsd_lib/libc/rpc \
	 ../bsd_lib/librpcsvc\
	 ../bsd_lib/libc/des\
	 ../bsd_lib/libc/yp \
	 ../bsd_lib/libcurses \
	 ../lib/libcurses/setup

all:
	@for dir in $(SUBDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
		    echo "installing header files from $$dir ..." ;\
		    (cd $$dir;$(MAKE) DESTROOT=$(DESTROOT) install-headers) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}

install: all

clobber:
	@echo "nothing to do"


