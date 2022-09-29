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
# $Header: Makefile,v 1.1.1.1 90/10/02 18:01:40 beacker Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(GNUM) $(MAXOPT) $(INCLUDE) -DRISCOS $(ECFLAGS)
ASFLAGS =$(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/usr/lib

OBJS	= t_accept.o t_bind.o t_connect.o t_error.o t_close.o t_getinfo.o\
	 t_getstate.o t_listen.o t_look.o t_rcv.o t_rcvconnect.o t_rcvdis.o\
	 t_snd.o t_snddis.o t_unbind.o t_optmgmt.o t_rcvudata.o t_rcvuderr.o\
	 t_sndudata.o t_sndrel.o t_rcvrel.o\
	 t_alloc.o t_free.o t_open.o t_sync.o\
	 _dummy.o _errlst.o _data.o _data2.o _conn_util.o _utility.o

#	 __free_def.o __calloc_def.o __perror_def.o __strlen_def.o\
#	 __write_def.o __ioctl_def.o __putmsg_def.o __getmsg_def.o\
#	 __errno_def.o __memcpy_def.o __fcntl_def.o __sigset_def.o\
#	 __open_def.o __close_def.o __ulimit_def.o

SRCS	= t_accept.c t_bind.c t_connect.c t_error.c t_close.c t_getinfo.c\
	 t_getstate.c t_listen.c t_look.c t_rcv.c t_rcvconnect.c t_rcvdis.c\
	 t_snd.c t_snddis.c t_unbind.c t_optmgmt.c t_rcvudata.c t_rcvuderr.c\
	 t_sndudata.c t_sndrel.c t_rcvrel.c\
	 t_alloc.c t_free.c t_open.c t_sync.c\
	 _dummy.c _errlst.c _data.c _data2.c _conn_util.c _utility.c

#	 __free_def.c __calloc_def.c __perror_def.c __strlen_def.c
#	 __write_def.c __ioctl_def.c __putmsg_def.c __getmsg_def.c
#	 __errno_def.c __memcpy_def.c __fcntl_def.c __sigset_def.c
#	 __open_def.c __close_def.c __ulimit_def.c

HDRS	= _import.h

LIBSUFFIX=
WHOAMI	=libnsl$(LIBSUFFIX).a

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

$(OBJS):	$(HDRS)
