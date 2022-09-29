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
# $Header: Makefile,v 1.4.2.9.1.1.1.2 90/10/23 13:42:17 beacker Exp $
#
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

SHELL	=/bin/sh
ETCDIR	=/etc

TESTDIR =.
INSDIR	=$(DESTROOT)/usr/lib/acct
ADMDIR	=$(DESTROOT)/usr/adm
ROOTBIN =$(DESTROOT)/bin
UBIN    =$(DESTROOT)/usr/bin
WKDIR	=$(ADMDIR) $(ADMDIR)/acct $(ADMDIR)/acct/nite $(ADMDIR)/acct/fiscal $(ADMDIR)/acct/sum 

FRC	=
ARGS	=
MAXOPT	=-O2
INCLUDE	=
DEFS	=
CFLAGS	=$(MAXOPT) $(INCLUDE) $(DEFS) $(ECFLAGS)
ASFLAGS	=$(DEFS) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)

LIBRARIES=
LOCAL_LIBS=./lib/a.a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all:	library acctcms acctcom acctcon1\
	acctcon2 acctdisk acctdusg acctmerg accton\
	acctprc1 acctprc2 acctwtmp\
        diskusg fwtmp wtmpfix closewtmp utmp2wtmp \
	chargefee ckpacct dodisk lastlogin\
	monacct nulladm prctmp prdaily\
	prtacct remove runacct\
	shutacct startup turnacct holtable \
	awkecms awkelus

library:
		cd lib; make 

acctcms:	$(LOCAL_LIBS) acctcms.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			acctcms.c $(LOCAL_LIBS) -o $(TESTDIR)/acctcms

acctcom:	$(LOCAL_LIBS) acctcom.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			acctcom.c $(LOCAL_LIBS) -o $(TESTDIR)/acctcom

acctcon1:	$(LOCAL_LIBS) acctcon1.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			acctcon1.c $(LOCAL_LIBS) -o $(TESTDIR)/acctcon1

acctcon2:	acctcon2.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			acctcon2.c -o $(TESTDIR)/acctcon2

acctdisk:	acctdisk.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			acctdisk.c -o $(TESTDIR)/acctdisk

acctdusg:	acctdusg.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG)  -DRISCOS \
			acctdusg.c -o $(TESTDIR)/acctdusg

acctmerg:	$(LOCAL_LIBS) acctmerg.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			acctmerg.c $(LOCAL_LIBS) -o $(TESTDIR)/acctmerg

accton:		accton.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			accton.c -o $(TESTDIR)/accton

acctprc1:	$(LOCAL_LIBS) acctprc1.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			acctprc1.c $(LOCAL_LIBS) -o $(TESTDIR)/acctprc1

acctprc2:	acctprc2.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			acctprc2.c -o $(TESTDIR)/acctprc2

acctwtmp:	acctwtmp.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			acctwtmp.c -o $(TESTDIR)/acctwtmp

diskusg:	diskusg.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) -DRISCOS \
			diskusg.c -o $(TESTDIR)/diskusg

fwtmp:		fwtmp.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			fwtmp.c -o $(TESTDIR)/fwtmp

wtmpfix:	wtmpfix.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
			wtmpfix.c -o $(TESTDIR)/wtmpfix

closewtmp:      closewtmp.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
                        closewtmp.c -o $(TESTDIR)/closewtmp

utmp2wtmp:      utmp2wtmp.c $(FRC)
		$(CC) $(CFLAGS) $(LFLAGS) $(FFLAG) \
                        utmp2wtmp.c -o $(TESTDIR)/utmp2wtmp

chargefee:	chargefee.sh $(FRC)
		cp chargefee.sh $(TESTDIR)/chargefee

ckpacct:	ckpacct.sh $(FRC)
		cp ckpacct.sh $(TESTDIR)/ckpacct

dodisk:		dodisk.sh $(FRC)
		cp dodisk.sh $(TESTDIR)/dodisk

monacct:	monacct.sh $(FRC)
		cp monacct.sh $(TESTDIR)/monacct

lastlogin:	lastlogin.sh $(FRC)
		cp lastlogin.sh $(TESTDIR)/lastlogin

nulladm:	nulladm.sh $(FRC)
		cp nulladm.sh $(TESTDIR)/nulladm

prctmp:		prctmp.sh $(FRC)
		cp prctmp.sh $(TESTDIR)/prctmp

prdaily:	prdaily.sh $(FRC)
		cp prdaily.sh $(TESTDIR)/prdaily

prtacct:	prtacct.sh $(FRC)
		cp prtacct.sh $(TESTDIR)/prtacct

remove:		remove.sh $(FRC)
		cp remove.sh $(TESTDIR)/remove

runacct:	runacct.sh $(FRC)
		cp runacct.sh $(TESTDIR)/runacct

shutacct:	shutacct.sh $(FRC)
		cp shutacct.sh $(TESTDIR)/shutacct

startup:	startup.sh $(FRC)
		cp startup.sh $(TESTDIR)/startup

turnacct:	turnacct.sh $(FRC)
		cp turnacct.sh $(TESTDIR)/turnacct

holtable:	holidays $(FRC)

awkecms:	ptecms.awk $(FRC)

awkelus:	ptelus.awk $(FRC)

install-dirs:
		ID=`id +%U` ;\
		for dir in $(WKDIR) ;\
		{\
		    [ -d $$dir ] || \
			((mkdir -p "$$dir" || exit 0) ; \
			[ $$ID != 0 ] || \
			(chmod 755 "$$dir" ;\
			chown adm "$$dir" ;\
			chgrp adm "$$dir")) ;\
		}

install:	all install-dirs
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctcms $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctcom $(UBIN)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctcon1 $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctcon2 $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctdisk $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctdusg $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctmerg $(INSDIR)
		$(INSTALL) -s -o root -g adm -m 4755 $(TESTDIR)/accton $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctprc1 $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctprc2 $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/acctwtmp $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/fwtmp $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/diskusg $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/wtmpfix $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/closewtmp $(INSDIR)
		$(INSTALL) $(INSTOPTS) -s $(TESTDIR)/utmp2wtmp $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/chargefee $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/ckpacct $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/dodisk $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/monacct $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/lastlogin $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/nulladm $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/prctmp $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/prdaily $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/prtacct $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/remove $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/runacct $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/shutacct $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/startup $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/turnacct $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/holidays $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/ptecms.awk $(INSDIR)
		$(INSTALL) $(INSTOPTS) $(TESTDIR)/ptelus.awk $(INSDIR)
		$(INSTALL) $(INSTOPTS) crontab.acct $(INSDIR)
		$(INSTALL) $(INSTOPTS) install_acct $(INSDIR)

clean:
		-rm -f *.o
		cd lib; make clean

clobber:	clean
		-rm -f					\
			$(TESTDIR)/acctcms		\
			$(TESTDIR)/acctcom		\
			$(TESTDIR)/acctcon1		\
			$(TESTDIR)/acctcon2		\
			$(TESTDIR)/acctdisk		\
			$(TESTDIR)/diskusg		\
			$(TESTDIR)/acctdusg		\
			$(TESTDIR)/acctmerg		\
			$(TESTDIR)/accton		\
			$(TESTDIR)/acctprc1		\
			$(TESTDIR)/acctprc2		\
			$(TESTDIR)/acctwtmp		\
			$(TESTDIR)/fwtmp		\
                        $(TESTDIR)/wtmpfix              \
                        $(TESTDIR)/closewtmp            \
                        $(TESTDIR)/utmp2wtmp
		-rm -f					\
			$(TESTDIR)/chargefee		\
			$(TESTDIR)/ckpacct		\
			$(TESTDIR)/dodisk		\
			$(TESTDIR)/lastlogin		\
			$(TESTDIR)/nulladm		\
			$(TESTDIR)/monacct		\
			$(TESTDIR)/prctmp		\
			$(TESTDIR)/prdaily		\
			$(TESTDIR)/prtacct		\
			$(TESTDIR)/remove		\
			$(TESTDIR)/runacct		\
			$(TESTDIR)/shutacct		\
			$(TESTDIR)/startup		\
			$(TESTDIR)/turnacct
		cd lib; make clobber

FRC:
