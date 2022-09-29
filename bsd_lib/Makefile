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
# $Header: Makefile,v 1.9.1.3.1.2.1.2 90/10/16 11:45:21 beacker Exp $
#
SHELL	=/bin/sh

SUBDIRS	=dict.d eign explain.d \
	lib300 lib300s lib4013 lib4014 lib450 libc libcrypt libcurses \
	libtermlib \
	libdbm libf77plot libl libmp libplot libplot2648 libplot7221 \
	libplotaed libplotbg libplotdumb libplotgigi libplotgrn libplotimagen \
	libsec \
	librpcsvc libvt0 liby

SPECLIST= libc
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
