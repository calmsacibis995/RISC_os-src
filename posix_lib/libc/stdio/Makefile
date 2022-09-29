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
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.4.1.3.1.1.1.2 90/10/05 10:12:07 beacker Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=-systype posix $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype posix $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

LOCALSRCS= getc.c fileno.c putc.c

LOCALOBJS =$(LOCALSRCS:.c=.o)

SPECIALSRCS = data.c

SPECIALOBJS =$(SPECIALSRCS:.c=.o)

SYSVSTDIOHDRS=stdiom.h

SYSVSTDIOSRCS=clrerr.c ctermid.c cuserid.c doscan.c fdopen.c feof.c \
	ferror.c fgetc.c \
	fgets.c filbuf.c findiop.c flsbuf.c fopen.c fputc.c fputs.c fread.c \
	fseek.c ftell.c fwrite.c getchar.c gets.c  \
	putchar.c puts.c putw.c remove.c rew.c scanf.c setbuf.c system.c \
	tempnam.c tmpfile.c tmpnam.c ungetc.c

SYSVSTDIOOBJS =$(SYSVSTDIOSRCS:.c=.o)

REMOTESRCS=$(SYSVSTDIOSRCS) $(SPECIALSRCS)

REMOTEOBJS=$(SYSVSTDIOOBJS) $(SPECIALOBJS)

SRCS= $(LOCALSRCS) $(REMOTESRCS)

OBJS= $(LOCALOBJS) $(REMOTEOBJS)

HDRS	= $(SYSVSTDIOHDRS)

NAMEMAPPING=../include/name_mapping.h

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
	-rm -f $(REMOTESRCS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)

$(SYSVSTDIOHDRS): ../../../lib/libc/stdio/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../../lib/libc/stdio/$@ $@

$(SYSVSTDIOSRCS) $(SPECIALSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../lib/libc/stdio/$@"' >> $@

$(REMOTESRCS): Makefile

$(REMOTEOBJS): $(NAMEMAPPING)

$(SYSVSTDIOOBJS):
	$(CC) $(CFLAGS) -c $<

#
# We make data.o with -G 2000 to make sure that old objects link
# with it.  (2000 is the size of _iob[])
#

$(SPECIALOBJS):
	$(CC) $(CFLAGS) -G 2000 -c $<
