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
# $Header: Makefile,v 1.7.1.4 90/05/10 03:40:20 wje Exp $
#
#
# Makefile for "top", a top 10 process display for Unix
#
# This makefile is for top, version 2.1
#
# Written by William LeFebvre, Rice University graduate student

# installation information:
#	OWNER	- name (or uid) for the installed executable's owner
#	GROUP	- group name (or gid) for the installed executable's group
#	MODE	- mode for the installed executable (should start with a 0)
#	BINDIR	- directory where the executable should live
#	MANDIR	- directory where the manual page should live
#	MAN	- troff macros for manual pages
#	TROFF	- most appropriate troff command

SHELL=/bin/sh
DESTROOT =
OWNER  = bin
GROUP  = sys
MODE   = 2555
CATDIR = /usr/man/catman/u_man/man1
MANDIR = /usr/man/u_man/man1
WHEREIGO = $(DESTROOT)/usr/new
DESTCATDIR = $(DESTROOT)$(CATDIR)
DESTMANDIR = $(DESTROOT)$(MANDIR)
TMACDIR =/usr/lib/tmac
TMACFILE=$(TMACDIR)/tmac.an
DESTTMACFILE=$(DESTROOT)$(TMACFILE)

NROFF	=cat $(DESTTMACFILE) - | tbl | nroff | col
TROFF	=cat $(DESTTMACFILE) - | tbl | troff -Ti10 | lpr -n

ETCDIR = /etc
INSTALL = $(ETCDIR)/mipsinstall
ISTRIP = -s
ICOPY = -c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)
MANINSTOPTS=$(ICOPY) -m 444 -o $(OWNER) -g bin

WHOAMI = top

# Values for the two defaults in "top":
#	TOPN	- default number of processes to display
#	DELAY	- default delay between updates
#
# set TOPN to -1 to indicate infinity (so that top will display as many
# as the screen will hold).

TOPN = -1
DELAY = 5

TABLE =
# Top maintains an internal hash table for translating uid to username.
# This hash table must be big enough to hold every name in /etc/passwd.
# It is possible, but not necessary, to specify the hash table size in
# this Makefile.  Just uncomment the following line and provide a number.
#TABLE = -DTable_size=

TARFILES = README Changes Makefile top.c commands.c display.c kernel.c \
	   screen.c getopt.c \
	   boolean.h layout.h screen.h top.h top.local.h bzero.c \
	   sigconv.awk top.man
OBJS = top.o commands.o display.o kernel.o screen.o getopt.o

COPTS=-g3
MAXOPT=-O2 $(COPTS)
INCLUDE=-I$(DESTROOT)/usr/include/bsd
LIBRARIES=-ltermlib -lm -lmld -lsun -lbsd
LOCAL_LIBS=../../lib/libmips/sysv/libmips.a
SYSTYPE=-systype sysv
CFLAGS = -DRISCOS -DNUMIPS $(SYSTYPE) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-DRISCOS -DNUMIPS $(SYSTYPE) $(EASFLAGS)
LFLAGS	=$(SYSTYPE) $(ELFLAGS)
YFLAGS	=$(EYFLAGS)

SIGNALFILE = $(DESTROOT)/usr/include/sys/signal.h

all: top top.1

$(WHOAMI): $(OBJS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) ${LIBRARIES}

top.o: top.c Makefile
	$(CC) $(CFLAGS) -c $(TABLE) -DDefault_TOPN=$(TOPN) -DDefault_DELAY=$(DELAY) top.c

# include file dependencies
top.o: boolean.h layout.h screen.h top.h top.local.h
commands.o: sigdesc.h
display.o: boolean.h layout.h screen.h top.h
kernel.o: top.local.h
screen.o: boolean.h screen.h

# automatically built include file
sigdesc.h: sigconv.awk /usr/include/signal.h
	awk -f sigconv.awk ${SIGNALFILE} >sigdesc.h

# top.1 is built by combining the actual text with the default information
top.1: top.man Makefile
	echo '.nr N' $(TOPN) > top.1
	echo '.nr D' $(DELAY) >>top.1
	cat top.man >>top.1

top.cat: top.1
	cat top.1 | $(NROFF) >top.cat

troff: top.1
	cat top.1 | $(TROFF) 

tar:
	rm -f top.tar
	tar cvf top.tar $(TARFILES)

clean:
	rm -f *.o core

clobber: clean
	rm -f top top.1 top.cat top.tar

install: $(WHOAMI) top.1 top.cat
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)
	if [ -d $(DESTCATDIR)/. ] ; \
	then \
		$(INSTALL) $(MANINSTOPTS) -f top.cat $(DESTCATDIR)/top.1 ;\
	fi
	if [ -d $(DESTMANDIR)/. ] ; \
	then \
	    if [ ! -d $(DESTCATDIR)/. ] || \
	       [ `(cd $(DESTCATDIR) ; /bin/pwd )` != `(cd $(DESTMANDIR)/. ; /bin/pwd )` ] ; \
	    then \
		$(INSTALL) $(MANINSTOPTS) top.1 $(DESTMANDIR) ;\
	    fi ; \
	fi
