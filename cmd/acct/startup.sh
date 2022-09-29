#! /bin/sh
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
# $Header: startup.sh,v 1.3.1.3 90/05/09 15:10:23 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.


#	"startup (acct) - should be called from /etc/rc"
#	"whenever system is brought up"
PATH=/usr/lib/acct:/bin:/usr/bin:/etc
acctwtmp "acctg on" >>/etc/wtmp
turnacct on
#	"clean up yesterdays accounting files"
remove
# marker file indicating that system V acct is active.  This
# is used by /usr/adm/periodic/monthly/10.wtmp.system
touch /usr/adm/SVACCTG_ON
