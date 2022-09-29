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
# $Header: Makefile,v 1.8.1.3 90/05/10 01:43:50 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=$(GNUM) -systype sysv $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(GNUM) -systype sysv $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=clrerr.o ctermid.o cuserid.o data.o doscan.o fdopen.o feof.o \
	ferror.o fgetc.o \
	fgets.o filbuf.o findiop.o flsbuf.o fopen.o fputc.o fputs.o fread.o \
	fseek.o ftell.o fwrite.o getchar.o getpass.o gets.o getw.o popen.o \
	putchar.o puts.o putw.o remove.o rew.o scanf.o setbuf.o setvbuf.o \
	stdio_def.o system.o tempnam.o tmpfile.o tmpnam.o ungetc.o

SRCS	=clrerr.c ctermid.c cuserid.c data.c doscan.c fdopen.c feof.c \
	ferror.c fgetc.c \
	fgets.c filbuf.c findiop.c flsbuf.c fopen.c fputc.c fputs.c fread.c \
	fseek.c ftell.c fwrite.c getchar.c getpass.c gets.c getw.c popen.c \
	putchar.c puts.c putw.c remove.c rew.c scanf.c setbuf.c setvbuf.c \
	stdio_def.c system.c tempnam.c tmpfile.c tmpnam.c ungetc.c

HDRS	=stdiom.h

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

#
# We make data.o with -G 2000 to make sure that old objects link
# with it.  (2000 is the size of _iob[])
#

data.o: data.c
	$(CC) $(CFLAGS) -G 2000 -c $<
