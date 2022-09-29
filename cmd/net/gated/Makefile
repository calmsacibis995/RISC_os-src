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
# $Header: Makefile,v 1.4.1.3 90/05/09 16:58:16 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

AWK	=/bin/awk
FGREP	=/usr/bin/fgrep

DESTROOT=

MAXOPT	=-O2
MAXOPT = -g
INCLUDE	= \
	 -I../../../lib/libmips
#
#	various compile time options....
#
#		GATEWAY 	- if this is defined in your config file this
#			  	  MUST be defined here.  This gives you bigger
#			  	  routing tables.
#
#		LOG_FACILITY	- if you don't like gated's syslog
#				  output in the LOG_DAEMON class, use
#				  this define to redirect it, for
#				  example:
#					-DLOG_FACILITY=LOG_LOCAL7
#
#		AGENT_SGMP	- define this when gated is being used
#				  with NYSERnet's SGMP code.  If you are
#				  using both SGMP and SNMP, make sure
#				  that SGMP_INCLUDE specifies the
#				  directory containing the snmp.h file.
#
#		AGENT_SNMP	- define this when gated is being used
#				  with NYSERnet's SNMP code.  If you are
#				  using both SGMP and SNMP, make sure
#				  that SNMP_INCLUDE specifies the
#				  directory containing the snmp.h file.
#
#	The following defines are for testing purposes only, i.e. use at
#		your own risk!
#
#
#		EGP_UPDATE_FORMAT - if you want the EGP network update
#				  packets to be formated.  Used for
#				  debugging EGP code.  This code does
#				  not do adequate checking for invalid
#				  packets so it should only be used when
#				  gated is being watched closely.
#
#		EGP_SKIP_SHARED - if it is not desired to include the
#				  shared network in EGP updates.  If
#				  not specified, the shared network will
#				  be included in EGP updates.
#
#		NO_HOLD_DOWN -	  disables the hold down of routes that
#				  have timed out or become unreachable.
#
SYSTYPE = -systype bsd43
DFLAGS = -DRISCOS -DBSD43 -DAGENT_SNMP
CFLAGS	=$(MAXOPT) $(INCLUDE) $(DFLAGS) $(SYSTYPE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lmld
LOCAL_LIBS=../../../lib/libmips/bsd43/libmips.a

WHEREIGO=$(DESTROOT)/usr/etc

OBJS =	af.o egp.o ext.o hello.o if.o icmp.o inet.o init.o main.o rip.o \
	rt_control.o rt_egp.o rt_if.o rt_init.o rt_nss.o rt_table.o sgmp.o \
	snmp.o str.o trace.o version.o

SRCS =	af.c egp.c ext.c hello.c if.c icmp.c inet.c init.c main.c rip.c \
	rt_control.c rt_egp.c rt_if.c rt_init.c rt_nss.c rt_table.c sgmp.c \
	snmp.c str.c trace.c version.c

HDRS =	af.h defs.h egp.h egp_param.h hello.h if.h include.h rip.h routed.h \
	rt_control.h rt_table.h snmp.h trace.h

WHOAMI	=gated

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK	=-l $(DESTROOT)/etc/$(WHOAMI)
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

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
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) \
		-l /etc/$(WHOAMI)

$(OBJS):	$(HDRS)
