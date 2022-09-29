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
# $Header: Makefile,v 1.4.2.2 90/05/09 17:42:26 wje Exp $
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTROOT	The root of the destination directory.
#
DESTROOT=
ETCDIR=/etc

MAXOPT	=-O2
DEBUG	=
INCLUDE	=
CFLAGS	=-systype bsd43 -DRISCOS $(DEBUG) $(MAXOPT) $(INCLUDE)
ASFLAGS =-systype bsd43 -DRISCOS $(DEBUG)

LIBRARIES=

WHEREIGO=$(DESTROOT)/usr/etc

LIBC=	/lib/libc.a

SRCS =	acksend.c candidate.c correct.c master.c networkdelta.c readmsg.c \
	slave.c timed.c cmds.c cmdtab.c timedc.c byteorder.c measure.c \
	cksum.c cksupport.s

HDRS =	globals.h timedc.h

SRCDS=	acksend.c candidate.c correct.c master.c networkdelta.c readmsg.c \
	slave.c timed.c
OBJDS=	acksend.o candidate.o correct.o master.o networkdelta.o readmsg.o \
	slave.o timed.o
SRCCS=	cmds.c cmdtab.c timedc.c
OBJCS=	cmds.o cmdtab.o timedc.o
COMMS=	byteorder.c measure.c cksum.c cksupport.s
COMMO=	byteorder.o measure.o cksum.o cksupport.o

#COMMS=	byteorder.c measure.c cksum.${MACHINE}.c
#COMMO=	byteorder.o measure.o cksum.${MACHINE}.o

WHOAMI	=timed

INSTALL	=$(ETCDIR)/mipsinstall
MODED	=555
MODEDC	=4555
OWNERD	=bin
OWNERDC	=root
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c

LINKD	=-l $(DESTROOT)/etc/timed
LINKDC	=-l $(DESTROOT)/etc/timedc
INSTOPTSD=$(ICOPY) $(ISTRIP) -o $(OWNERD) -g $(GROUP) -m $(MODED) $(LINKD)
INSTOPTSDC=$(ICOPY) $(ISTRIP) -o $(OWNERDC) -g $(GROUP) -m $(MODEDC) $(LINKDC)

all: timed timedc

timed: 	${OBJDS} ${COMMO} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJDS} ${COMMO} ${LIBRARIES}

timedc: ${OBJCS} ${COMMO} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJCS} ${COMMO} ${LIBRARIES}

clobber: clean
	-rm -f timed timedc
clean:
	rm -f ${OBJDS} ${OBJCS} ${COMMO}

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

cksum.o:
	${CC} ${CFLAGS} -c $*.c

cksupport.o: cksupport.s
	$(AS) $(ASFLAGS) -o $@ $<

install: timed timedc
	$(INSTALL) $(INSTOPTSD) timed ${WHEREIGO}
	$(INSTALL) $(INSTOPTSDC) timedc ${WHEREIGO}

lint:
	lint ${CFLAGS} ${SRCDS} ${COMMS}
	lint ${CFLAGS} ${SRCCS} ${COMMS}

tags:	ctags ${SRCDS} ${SRCCS} ${COMMS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.
