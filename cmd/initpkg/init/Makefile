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
# $Header: Makefile,v 1.10.2.5 90/05/09 17:23:03 wje Locked $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/etc/init.d

SRCS	=ANNOUNCE MOUNTFSYS README RMTMPFILES autoconfig cron disks \
	firstcheck nfs sysetup tcp netdaemons syslog uucp lockfix \
	savecore addswap smtp tzsetup \
	acct bsd43_accton bsd43_lpd lpsched noacct set_kopts

WHOAREWE=ANNOUNCE MOUNTFSYS README RMTMPFILES autoconfig cron disks \
	firstcheck nfs sysetup tcp netdaemons syslog uucp lockfix \
	savecore addswap smtp tzsetup \
	acct bsd43_accton bsd43_lpd lpsched noacct set_kopts

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=544
OWNER	=root
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
