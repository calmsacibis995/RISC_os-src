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
# $Header: Makefile,v 1.2.2.2 90/05/10 03:09:58 wje Exp $
SHELL=/bin/sh
MAKE=/bin/make
SUBDIRS = \
	include \
	a_pathname \
	hunt \
	hunt.driver 


all install generic clean clobber:
	 for i in $(SUBDIRS) ;\
	 {\
		if [ ! -d "$$i" ] ;\
		then \
			continue ;\
		fi ;\
		if [ -f "$$i/SRCONLY" -a " $$SRCONLY" != " yes" ] ;\
		then \
			echo "NOTE: $$i not rebuilt - Source release only" ;\
			continue ;\
		fi ;\
		echo "XXXXXX $$i XXXXXX" ;\
		( cd $$i ; $(MAKE) $(MAKEARGS) $@ ) ; \
	 }
