#!/bin/sh
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
# $Header: uudemon.adm.sh,v 1.4.2.3 90/05/10 00:43:01 wje Exp $
# #ident	"@(#)uucp:uudemon.admin	2.2"
#
#	This shell sends uucp status information to an administrator.
#	It should started by a line in /usr/lib/crontab.
#	e.g.
#
# 48 8,12,16 * * * /bin/su uucp -c "/usr/lib/uucp/uudemon.adm" > /dev/null
#
set +e

export PATH
PATH=/bin:/usr/bin
MAILTO=uucp
LOGDIR=/usr/spool/uucp/.Log
ULOG=$LOGDIR/uucico
TMP=/tmp/uu$$

(uustat -p; uustat -q) > $TMP
if [ -s $TMP ]
then
	(echo "Subject: uu-status"; cat $TMP) | mail $MAILTO
fi
num=`echo $ULOG/* | wc -w`
if [ ${num} -gt 1 ]
then
	grep passwd $ULOG/* > $TMP
	if [ -s $TMP ]
	then
		(echo "Subject: passwd check"; cat $TMP) | mail $MAILTO
	fi
fi
rm $TMP

