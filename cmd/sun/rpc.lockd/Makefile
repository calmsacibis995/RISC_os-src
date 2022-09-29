#
# $Header: Makefile,v 1.1.1.2.1.2.1.2 90/11/17 11:50:28 beacker Exp $
# ---------------------------------------------------
# | Copyright (c) 1987 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

#MAXOPT	=-O2

MAXOPT	=-O0 -g3
INCLUDE	=-I.
CFLAGS	=-systype bsd43 -DRISCOS $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lrpcsvc
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/etc

OBJS	=flk_filock.o flk_reclox.o hash.o \
prot_alloc.o prot_free.o prot_libr.o prot_lock.o prot_main.o prot_msg.o \
prot_pklm.o prot_pnlm.o prot_priv.o prot_proc.o prot_share.o \
sm_monitor.o svc_udp.o tcp.o udp.o xdr_klm.o xdr_nlm.o xdr_sm.o

SRCS	=flk_filock.c flk_reclox.c hash.c \
prot_alloc.c prot_free.c prot_libr.c prot_lock.c prot_main.c prot_msg.c \
prot_pklm.c prot_pnlm.c prot_priv.c prot_proc.c prot_share.c \
sm_monitor.c svc_udp.c tcp.c udp.c xdr_klm.c xdr_nlm.c xdr_sm.c

HDRS	=flock.h klm_prot.h lockf.h nlm_prot.h \
priv_prot.h prot_lock.h prot_time.h sm_inter.h sm_res.h

WHOAMI	=rpc.lockd

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(OBJS):	$(HDRS)
