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
# $Header: Makefile,v 1.2.2.2 90/05/09 18:39:38 wje Exp $
#
SHELL	=/bin/sh

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

ARFLAGS	=crs

OBJS	=auxf.o chkid.o chksid.o date_ab.o date_ba.o del_ab.o del_ba.o \
	dodelt.o dofile.o dohist.o doie.o dolist.o eqsid.o flushto.o \
	fmterr.o getline.o logname.o newstats.o permiss.o pf_ab.o putline.o \
	rdmod.o setup.o sid_ab.o sid_ba.o sidtoser.o sinit.o stats_ab.o

SRCS	=auxf.c chkid.c chksid.c date_ab.c date_ba.c del_ab.c del_ba.c \
	dodelt.c dofile.c dohist.c doie.c dolist.c eqsid.c flushto.c \
	fmterr.c getline.c logname.c newstats.c permiss.c pf_ab.c putline.c \
	rdmod.c setup.c sid_ab.c sid_ba.c sidtoser.c sinit.c stats_ab.c

HDRS	=

WHOAMI	=libcomobj.a

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
