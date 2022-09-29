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
# $Header: fstyp.sh,v 1.7.2.2 90/05/09 16:04:00 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.


#	Determine the fs identifier of a file system.

if [ $# -ne 1 ]
then
	echo "Usage: fstyp special" 1>&2
	exit 2
fi

if [ ! -r $1 ]
then
	echo "fstyp: cannot read $1" 1>&2
	exit 1
fi

#
#	Execute all heuristic functions in /etc/fstyp.d and
#	return the fs identifier of the specified file system.
#

for f in /etc/fstyp.d/*
do
	msg=`$f $1 2>&1`
	if [ $? -eq 0 ]
	then
		echo ${msg}
		exit 0
	fi
done
echo "Unknown_fstyp"
exit 1
