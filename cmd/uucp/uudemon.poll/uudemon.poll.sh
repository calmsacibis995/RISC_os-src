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
# $Header: uudemon.poll.sh,v 1.3.2.2 90/05/10 00:43:49 wje Exp $
# #ident	"@(#)uucp:uudemon.poll	2.2"

# This shell should be run out of crontab every hour,
#  a little before  uudemon.hour since this one
#  does not start the scheduler.

PATH=/bin:/usr/bin:/etc:/usr/lib/uucp
SPOOL=/usr/spool/uucp

POLLFILE=/usr/lib/uucp/Poll
# POLLFILE is a list of "system <tab> hour1 hour2 hour3 ..." for polling
# For example 
#	raven	2  6  10
# without the # at the beginning.  Lines starting with # are ignored.
# NOTE a tab must follow the machine name

umask 022
set +e

HOUR="`date '+%H'`"
# HOUR="`date | sed -e 's/:.*//' -e 's/^.*\(..\)$/\1/'"
HOUR=`expr $HOUR + 0`

cat $POLLFILE  |
sed -n -e "/^[^#].*[ 	]$HOUR[ 	]/s/	.*//p" -e "/^[^#].*[ 	]$HOUR\$/s/	.*//p" |
while read site
do
	if test ! -d $SPOOL/$site
	then
		mkdir $SPOOL/$site
		chown uucp $SPOOL/$site
	fi

	j=`expr $site : '\(.\{1,7\}\)'`
	touch $SPOOL/$site/C.${j}n0000
	chown uucp $SPOOL/$site/C.${j}n0000
done
