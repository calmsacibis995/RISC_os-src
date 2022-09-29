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
# $Header: Makefile,v 1.2.2.3 90/05/10 00:34:16 wje Exp $
#
SHELL	=/bin/sh

MAXOPT	=-O1
INCLUDE	=-I../include
CFLAGS	=$(MAXOPT) -DRISCOS $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

ARFLAGS	=crs

OBJS	=callers.o chremdir.o conn.o cpmv.o expfile.o gename.o getargs.o \
	getprm.o getpwinfo.o gnamef.o gtcfile.o gwd.o interface.o line.o \
	logent.o mailst.o permission.o strecpy.o strsave.o sysfiles.o \
	systat.o ulockf.o utility.o uucpdefs.o uucpname.o versys.o xqt.o

SRCS	=callers.c chremdir.c conn.c cpmv.c expfile.c gename.c getargs.c \
	getprm.c getpwinfo.c gnamef.c gtcfile.c gwd.c interface.c line.c \
	logent.c mailst.c permission.c strecpy.c strsave.c sysfiles.c \
	systat.c ulockf.c utility.c uucpdefs.c uucpname.c versys.c xqt.c

HDRS	=

WHOAMI	=libuucp.a

all: $(WHOAMI)

install: $(WHOAMI)

$(WHOAMI): $(OBJS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)
