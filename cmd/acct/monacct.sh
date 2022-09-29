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
# $Header: monacct.sh,v 1.3.1.2 90/05/09 15:08:10 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.


#	"this procedure should be run periodically ( by month or fiscal )"
_adm=/usr/adm
_sum=${_adm}/acct/sum
_fiscal=${_adm}/acct/fiscal
PATH=:/usr/lib/acct:/bin:/usr/bin:/etc
export PATH


#if test $# -ne 1; then
#	echo "usage: monacct fiscal-number"
#	exit
#fi

_period=${1-`date +%m`}

cd ${_adm}

#	"move summary tacct file to fiscal directory"
mv ${_sum}/tacct ${_fiscal}/tacct${_period}

#	"delete the daily tacct files"
rm -f ${_sum}/tacct????

#	"restart summary tacct file"
nulladm ${_sum}/tacct

#	"move summary cms file to fiscal directory
mv ${_sum}/cms ${_fiscal}/cms${_period}

#	"restart summary cms file"
nulladm ${_sum}/cms

#	"remove old prdaily reports"
rm -f ${_sum}/rprt*

#	"produce monthly reports"
prtacct ${_fiscal}/tacct${_period} > ${_fiscal}/fiscrpt${_period}
acctcms -a -s ${_fiscal}/cms${_period} |  \
pr -h "TOTAL COMMAND SUMMARY FOR FISCAL ${_period}" >> ${_fiscal}/fiscrpt${_period}
pr -h "LAST LOGIN" -3 ${_sum}/loginlog >> ${_fiscal}/fiscrpt${_period}

#	"add commands here to do any charging of fees, etc"
exit
