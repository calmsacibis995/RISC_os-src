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
# $Header: Makefile,v 1.6.1.2 90/05/09 17:34:31 wje Exp $
#
BASE =	..

include $(BASE)/commondefs

CSOURCES = snmpd.c

OBJECTS = $(CSOURCES:.c=.o)

WHOAMI = snmpd

WHEREIGO = $(DESTROOT)/usr/etc

INLINK = -l $(DESTROOT)/etc/$(WHOAMI)

all: $(WHOAMI)

$(WHOAMI): $(OBJECTS) $(LIBRARIES)
	$(CC) -o $(WHOAMI) $(SYSTYPE) $(OBJECTS) $(LIBRARIES)

include $(BASE)/commonmacros

include Makedepend

FRC:
