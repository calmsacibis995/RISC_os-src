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
# $Header: Makefile,v 1.2.2.2 90/05/09 18:29:41 wje Exp $
#
SHELL	=/bin/sh

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

ARFLAGS	=crs

OBJS	=Chdir.o Execvp.o Fopen.o Fread.o Fstat.o Malloc.o Open.o Read.o \
	Write.o argfopen.o bufsplit.o errafter.o errbefore.o errtext.o \
	filename.o fopenck.o num.o numd.o p2open.o pgmname.o stddef.o

SRCS	=Chdir.c Execvp.c Fopen.c Fread.c Fstat.c Malloc.c Open.c Read.c \
	Write.c argfopen.c bufsplit.c errafter.c errbefore.c errtext.c \
	filename.c fopenck.c num.c numd.c p2open.c pgmname.c stddef.c

HDRS	=

WHOAMI	=libcsub.a

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
