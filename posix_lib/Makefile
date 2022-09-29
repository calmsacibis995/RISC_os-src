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
# $Header: Makefile,v 1.1.1.3 90/05/10 04:11:00 wje Exp $
#
SHELL	=/bin/sh

SUBDIRS	=libc locale

SPECLIST=libc
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
