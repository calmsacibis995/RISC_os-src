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
# $Header: Makefile,v 1.7.1.2 90/05/10 02:51:27 wje Exp $
#
# NOTE: there aren't any real source files in this dir.  All are
#       symlinks to bsd_lib/librpcsvc, where the sources have been
#       tweaked to compile either systype bsd43 or systype sysv

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

DEBUG	=
MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(DEBUG) $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS
ASFLAGS =$(DEBUG) $(GNUM) $(EASFLAGS)
#CFLAGS	=$(DEBUG) -DKERNEL -DRISCOS $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
#ASFLAGS =$(DEBUG) -DKERNEL -DRISCOS $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/usr/lib

OBJS	=bootparam_xdr.o etherxdr.o klm_prot.o mountxdr.o nlm_prot.o publickey.o \
	rex_xdr.o rquotaxdr.o \
	rstatxdr.o rusersxdr.o rwallxdr.o sm_inter.o sprayxdr.o util.o \
	xcrypt.o yppasswdxdr.o

SRCS	=bootparam_xdr.c etherxdr.c klm_prot.c mountxdr.c nlm_prot.c \
	publickey.c rex_xdr.c rquotaxdr.c \
	rstatxdr.c rusersxdr.c rwallxdr.c sm_inter.c sprayxdr.c util.c \
	xcrypt.c yppasswdxdr.c

HDRS	=bootparam.h ether.h klm_prot.h mount.h nlm_prot.h pwdnm.h \
	rex.h rquota.h rstat.h \
	rusers.h rwall.h sm_inter.h spray.h yppasswd.h

BSD43STUFF = $(SRCS)

LIBSUFFIX=
WHOAMI	=librpcsvc$(LIBSUFFIX).a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)


rex_xdr.o:  rex_xdr.c rex.h

nlm_prot.o: nlm_prot.c nlm_prot.h
$(BSD43STUFF): ../../bsd_lib/librpcsvc/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../bsd_lib/librpcsvc/$@ $@

nlm_prot.h: ../../bsd_lib/librpcsvc/nlm_prot.h
	[ ! -r nlm_prot.h ] || rm -f nlm_prot.h
	ln -s ../../bsd_lib/librpcsvc/nlm_prot.h nlm_prot.h

rex.h: ../../bsd_lib/librpcsvc/rex.h
	[ ! -r rex.h ] || rm -f rex.h
	ln -s ../../bsd_lib/librpcsvc/rex.h rex.h
