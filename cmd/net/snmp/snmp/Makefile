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
# $Header: Makefile,v 1.6.1.2 90/05/09 17:24:02 wje Exp $
#
BASE =	..

include $(BASE)/commondefs

CSOURCES =	aps.c ap0.c asl.c asn.c asx.c at.c avl.c egp.c gated.c \
		host.c icmp.c interfaces.c ip.c mis.c miv0.c miv1.c miv2.c \
		miv3.c miv4.c miv5.c miv6.c miv7.c mix.c oid.c rdx.c smp.c \
		smx.c stubs.c systm.c tcp.c udp.c udp_request.c utils.c vec.c

OBJECTS = $(CSOURCES:.c=.o)

GATED_PATH = $(BASE)/../gated
GATED_INCLUDE = snmp.h

WHOAMI = libsnmp.a

WHEREIGO = $(DESTROOT)/usr/lib

all: $(WHOAMI)

$(WHOAMI): $(GATED_INCLUDE) $(OBJECTS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJECTS)

$(GATED_INCLUDE): $(GATED_PATH)/$@
	[ ! -r $@ ] || rm -f $@
	ln -s $(GATED_PATH)/$@ $@

include $(BASE)/commonmacros

include Makedepend

FRC:
