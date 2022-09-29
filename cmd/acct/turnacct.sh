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
# $Header: turnacct.sh,v 1.3.1.2 90/05/09 15:10:41 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.


#	"control process accounting (must be root)"
#	"turnacct on	makes sure it's on"
#	"turnacct off	turns it off"
#	"turnacct switch	switches pacct to pacct?, starts fresh one"
#	"/usr/adm/pacct is always the current pacct file"
PATH=/usr/lib/acct:/bin:/usr/bin:/etc
cd /usr/adm
case "$1"  in
on)
	if test ! -r pacct
	then
		nulladm pacct
	fi
	accton pacct
	rc=$?
	;;
off)
	accton
	rc=$?
	;;
switch)
	if test -r pacct
	then
		_i=1
		while test -r pacct${_i}
		do
			_i="`expr ${_i} + 1`"
		done
		mv pacct pacct${_i}
	fi
	nulladm pacct
	accton
	accton pacct
	_rc=$?
	if test ${_rc} -ne 0; then
		echo "accton failed"
		rm pacct
		mv pacct${_i} pacct
		exit ${_rc}
	fi
	;;
*)
	echo "Usage: turnacct on|off|switch"
	_rc=1
	;;
esac
exit ${_rc}
