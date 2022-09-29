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
# $Header: Makefile,v 1.2.2.3 90/05/10 03:07:05 wje Exp $
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTROOT	The root of the destination directory.
#

DESTROOT=
LOCAL_CC=cc
LOCAL_INCLUDE=-I../include

DEF_CFLAGS=-O2
CFLAGS	= -systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE)


ICOPY=-c
ISTRIP=-s
INSTALL=/etc/mipsinstall
WHEREIGO=$(DESTROOT)/usr/games/lib

SRCS	=strfile.c rnd.c scene obscene README

WHOAMI	=fortunes.dat

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(ICOPY) -m 600 -o daemon $(WHOAMI) $(WHEREIGO)

$(WHOAMI): strfile fortunes
	./strfile -r fortunes

clean:
	-rm -f strfile.o rnd.o strfile fortunes

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -p $(WHOAMI) $(INCLUDE)

strfile: strfile.o rnd.o
	$(LOCAL_CC) $(CFLAGS) -o strfile strfile.o rnd.o

strfile.o: strfile.c ../include/strfile.h
	$(LOCAL_CC) $(CFLAGS) -c $(LOCAL_INCLUDE) strfile.c

rnd.o: rnd.c
	$(LOCAL_CC) $(CFLAGS) -c rnd.c

fortunes: scene obscene
	echo "%-" | cat scene - obscene > fortunes

clobber:	clean
	-rm -f $(WHOAMI)
