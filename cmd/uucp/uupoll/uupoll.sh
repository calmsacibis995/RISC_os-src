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
# $Header: uupoll.sh,v 1.1.2.2 90/05/10 00:45:25 wje Exp $
#
# uupoll - start a uucp job for the named system
#

Myname=`basename "$0"`
PATH=/bin:/usr/bin

case $# in
	0)
		echo "$Myname: usage: $Myname system..."
		exit 1
		;;
esac

for sys
{
	case "$sys" in
		-*)
			echo "$Myname: Invalid system name $sys"
			exit 1
			;;
		
		*)
			/usr/lib/uucp/uucico -r1 -s"$sys" &
			;;
	esac
}
exit 0
