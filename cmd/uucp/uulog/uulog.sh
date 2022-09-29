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
# $Header: uulog.sh,v 1.3.2.2 90/05/10 00:44:48 wje Exp $
# #ident	"@(#)uucp:uulog	2.3"

#From: ulysses!smb (Steven Bellovin)
#
# usage:
# 	uulog
# or	uulog foo
# or	uulog -sfoo
# or	uulog -s foo
# or	uulog -ffoo
# or	uulog -f foo
#
#	-x means check the execute file
#	-nnn where 'nnn' is a number will do tail -nnn
#
LOGDIR=/usr/spool/uucp/.Log
type=uucico
n=""

cd $LOGDIR

while [ $# -gt 0 ]
do
	case $1 in
	-x)	type=uuxqt
		shift
		;;

	-[0-9]*)n=`echo $1|cut -c2-`
		shift
		;;

	-f)	fflag=1
		shift
		if [ $# -eq 0 ]
		then
			echo "System name must follow -f flag" 1>&2
			exit 1
		else
			case "$1" in
			-*)
				echo "System name must follow -f flag" 1>&2
				exit 1
				;;
			esac
		fi
		;;

	-f*)	x=`echo $1|cut -c3-`
		shift
		set - $x $*
		fflag=1
		;;

	-s)	shift
		if [ $# -eq 0 ]
		then
			echo "System name must follow -s flag" 1>&2
			exit 1
		else
			case "$1" in
			-*)
				echo "System name must follow -s flag" 1>&2
				exit 1
				;;
			esac
		fi
		;;

	-s*)	x=`echo $1|cut -c3-`
		shift
		set - $x $*
		;;

	-*)	echo "Invalid flag $1" 1>&2
		exit 1
		;;

	*)	sys="$sys $1"
		shift
		;;

	esac
done

set - $sys
if [ x$fflag = x ]; then
	if [ $# = 0 ]; then
		set - `ls $type`
	fi
	for i
	do
		if [ x$n = x ]; then
			cat $type/$i
		else
			tail -$n $type/$i
		fi
	done
else
	if [ $# != 1 ]; then
		echo "Exactly one system with -f" 1>&2
		exit 2
	fi
	exec tail -${n}f $type/$1
fi
