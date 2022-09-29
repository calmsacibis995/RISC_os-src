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
# $Header: Makefile,v 1.11.1.2.1.2 90/07/19 12:54:03 hawkes Exp $
#
SHELL	=/bin/sh

SUBDIRS	=	arp finger fingerd ftp ftpd gated gettable havetcp \
		hostid hostname htable ifconfig inetd named netstat \
		ping rcp rexecd rlogin rlogind rmt route routed rsh rshd \
		ruptime rwho rwhod slattach slconfig snmp syslogd talkd \
		telnet telnetd tftp tftpd timed trpt

all install clobber clean generic:
	@for dir in $(SUBDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir XXXXXXX" ;\
			( cd $$dir ; $(MAKE) $(MAKEARGS) $@ ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}
