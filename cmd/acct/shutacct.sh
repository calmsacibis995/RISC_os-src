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
# $Header: shutacct.sh,v 1.3.1.3 90/05/09 15:10:10 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.


#	"shutacct [arg] - shuts down acct, called from /etc/shutdown"
#	"whenever system taken down"
#	"arg	added to /etc/wtmp to record reason, defaults to shutdown"
PATH=/usr/lib/acct:/bin:/usr/bin:/etc
_reason=${1-"acctg off"}
acctwtmp  "${_reason}"  >>/etc/wtmp
turnacct off
# remove marker file indicating that system V acct is active.  This
# is used by /usr/adm/periodic/monthly/10.wtmp.system
rm /usr/adm/SVACCTG_ON
