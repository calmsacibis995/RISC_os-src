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
# $Header: Makefile,v 1.2.2.2 90/05/09 16:25:21 wje Exp $
#
SHELL	=/bin/sh

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

ARFLAGS	=crs

OBJS	=date.o dest.o destlist.o eaccess.o enter.o fatal.o fifo.o \
	findtty.o fullpath.o getname.o gwd.o isclass.o isdest.o isprinter.o \
	isrequest.o lock.o outlist.o outputq.o pstatus.o qstatus.o \
	request.o sendmail.o trim.o wrtmsg.o

SRCS	=date.c dest.c destlist.c eaccess.c enter.c fatal.c fifo.c \
	findtty.c fullpath.c getname.c gwd.c isclass.c isdest.c isprinter.c \
	isrequest.c lock.c outlist.c outputq.c pstatus.c qstatus.c \
	request.c sendmail.c trim.c wrtmsg.c

HDRS	=

WHOAMI	=liblp.a

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
