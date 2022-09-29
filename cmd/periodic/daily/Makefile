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
# $Header: Makefile,v 1.7.2.3 90/05/09 18:16:38 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/adm/periodic/daily

SRCS	=10.clean.system 10.makehosts.system 10.rwho.system \
	10.syslogd.system 50.df.system \
	16.sendmail.freeze.system 17.sendmail.restart.system \
	10.cronlog.system 50.calendar.system 50.checkdev.system

WHOAREWE=10.clean.system 10.makehosts.system 10.rwho.system \
	10.syslogd.system 50.df.system \
	16.sendmail.freeze.system 17.sendmail.restart.system \
	10.cronlog.system 50.calendar.system 50.checkdev.system

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAREWE)

install: $(WHOAREWE)
	for name in $(WHOAREWE) ;\
	{\
		$(INSTALL) $(INSTOPTS) "$$name" $(WHEREIGO) ;\
	}

clean:

clobber:

generic:
	$(GENERIC) -s $(SRCS) -d $(WHOAREWE) \
		-R$(DESTROOT) -D$(WHEREIGO)
