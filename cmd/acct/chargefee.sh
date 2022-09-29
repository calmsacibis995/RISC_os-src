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
# $Header: chargefee.sh,v 1.2.1.2 90/05/09 15:04:03 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

#/* #ident	"@(#)acct:chargefee.sh	1.3" */
#/* $Header: chargefee.sh,v 1.2.1.2 90/05/09 15:04:03 wje Exp $ */

#	"chargefee login-name number"
#	"emits tacct.h/ascii record to charge name $number"
cd /usr/adm
PATH=/usr/lib/acct:/bin:/usr/bin:/etc
if test $# -lt 2; then
	echo "usage: chargefee name number"
	exit
fi
_entry="`grep \^$1: /etc/passwd`"
if test -z "${_entry}"; then
	echo "can't find login name $1"
	exit
fi
case "$2"  in
-[0-9]*|[0-9]*);;
*)
	echo "charge invalid: $2"
	exit
esac

if test ! -r fee; then
	nulladm fee
fi
_userid=`echo "${_entry}" | cut -d: -f3`  # get the UID
echo  "${_userid} $1 0 0 0 0 0 0 0 0 0 0 $2"  >>fee
