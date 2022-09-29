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
# $Header: Makefile,v 1.3.1.2 90/05/07 21:04:49 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=clrerr.o doprnt.o doscan.o exit.o fdopen.o fgetc.o fgets.o \
	filbuf.o findiop.o flsbuf.o fopen.o fprintf.o fputc.o \
	fputs.o fread.o freopen.o fseek.o ftell.o fwrite.o \
	getchar.o gets.o getw.o printf.o putchar.o puts.o putw.o \
	rew.o scanf.o setbuf.o setbuffer.o sibuf.o sobuf.o \
	sprintf.o strout.o ungetc.o vfprintf.o vprintf.o vsprintf.o

SRCS	=clrerr.c doprnt.c doscan.c exit.c fdopen.c fgetc.c fgets.c \
	filbuf.c findiop.c flsbuf.c fopen.c fprintf.c fputc.c \
	fputs.c fread.c freopen.c fseek.c ftell.c fwrite.c \
	getchar.c gets.c getw.c printf.c putchar.c puts.c putw.c \
	rew.c scanf.c setbuf.c setbuffer.c sibuf.c sobuf.c \
	sprintf.c strout.c ungetc.c vfprintf.c vprintf.c vsprintf.c

HDRS	=print.h

WHOAMI	=libc_stdio.port.a
INTEGDIR=../libc

all install: $(INTEGDIR)/$(WHOAMI)

$(INTEGDIR)/$(WHOAMI): $(WHOAMI)
	-rm -f $(INTEGDIR)/$(WHOAMI)
	cp $(WHOAMI) $(INTEGDIR)

$(WHOAMI): $(OBJS)
	-rm -f $(WHOAMI)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)

doprnt.o: doprnt.c print.h
