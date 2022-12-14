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
# $Header: Uutry.sh,v 1.3.2.2 90/05/10 00:34:10 wje Exp $
# #ident	"@(#)uucp:Uutry	2.3"

#	This shell will start a uucico for the system given.
#	Options:
#	  -xN the debugging level for uucico (-x5 default)
#	  -r  force the removal of the status file
#	The output is put in /tmp/Name where Name is the name
#	of the system name.  A tail -f is performed after uucico is started.

STATUS=/usr/spool/uucp/.Status

if [ -x "./uucico" ]; then
	UUCICO=./uucico
else
	UUCICO=/usr/lib/uucp/uucico
fi

REMOVE=""
X="-x5"
SYS=
while [ $# -gt 0 ]
do
	case $1 in
	-x)  shift; X="-x$1"; shift;;
	-x*) X=$1; shift;;
	-r) REMOVE="y"; shift;;
	*) SYS="$1"; shift;;
	esac
done

if [ -z "$SYS" ]
then
	echo "$0:  system name required"
	exit 1
fi

#  check for existence in Systems file
#  only accept match of full name
#  (important because some names may be prefixes of others!)
XX=
XX=`uuname | grep "^${SYS}$" `
if [ -z "$XX" ]
then
	echo "Invalid system name \"$SYS\""
	exit
fi

STMP=/tmp/$SYS
rm -f $STMP
> $STMP
chmod 660 $STMP
#  remove old status file
if [ -n "$REMOVE" ]; then
    rm -f $STATUS/${SYS}
fi

echo "$UUCICO -r1 -s$SYS  $X >$STMP 2>&1&"
$UUCICO  -r1 -s$SYS  $X >$STMP 2>&1&

echo "tmp=$STMP"
#	on heavily loaded systems, may take a moment for uucico
#	to create debug file.
if [ ! -f $STMP ]
then
	sleep 5
fi
tail -f $STMP
