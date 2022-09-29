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
# $Header: Makefile,v 1.3.2.2 90/05/10 00:33:58 wje Exp $
#
SHELL	=/bin/sh

SUBDIRS	=include a_uucp Uutry ct cu rmt.unknown uucheck uucico uucleanup \
	uucp uucpd uudemon.adm uudemon.clnu uudemon.hour uudemon.poll \
	uugetty uulog uuname uupick uupoll uusched uustat uuto uux uuxqt \
	uuencode uudecode

all install clobber clean generic:
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
