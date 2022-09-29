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
# $Header: fsstat.sh,v 1.2.2.2 90/05/09 16:03:48 wje Exp $

#
# Wrapper to determine the status of any type of disk file system.
#

if [ $# != 1 ]
then
	echo "usage: fsstat special" 1>&2;
	exit 3
fi

FSTYP=`/etc/fstyp $1 2>&1` || { echo "fsstat: $FSTYP" 1>&2; exit 3; }

/etc/fsstat.$FSTYP "$1"
