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
# $Header: uuto.sh,v 1.5.2.2 90/05/10 00:46:35 wje Exp $
# #ident	"@(#)uucp:uuto	2.3"

a=""
f=""
d=""
sub=""
export UUP
mysys=`uuname -l`
mesg="Usage: $0 [-m -p] files remote!user"
if test  $# -lt 1
	then
		echo "$mesg" 1>&2
		exit
fi
trap "trap '' 0; exit" 0 1 2 13 15
copy=0
#	get options
while true
do
	case $1 in
	 -m)	a="$a $1" sub="$sub -m" ;;
	 -p)	copy=1 sub="$sub -p" ;;
	 -*)	echo "$mesg" 1>&2; exit;;
	 *)	break ;;
	esac
	shift
done
#	be sure have both files and destination
if test $# -eq 1
then
	echo "$mesg" 1>&2
	exit 2
fi
#	get file names
while test $#  -gt 1
do
	if test -d "$1" -a {"$1" = "." -o "$1" = ".."}
		then shift; continue
	elif test -r "$1" -a -f "$1"
		then f="$f $1"
	elif test -r "$1" -a -d "$1"
		then d="$d $1"
	elif test "$UUP" = ""
		then echo "$1: file/directory not found" 1>&2; exit
	fi
	shift
done
#	the recipient arg: remote!user
#	remote may be omitted (default is this machine)
#	must have at least !user
remote=`expr $1 : '\(.*\)!'`
user=`expr $1 : '.*!\(.*\)'`
if test -z "$user"
then
	echo "$0: incomplete destination -- must specify user" 1>&2
	echo "$mesg" 1>&2
	exit 2
fi
if test 1 -eq "$copy"
	then a="$a -C"
fi
	a="$a -d -n$user"
error=1
if test -n "$d" -a -n "$user"
then
	for i in $d
	do
		( cd $i; UUP="$UUP/$i"
		for j in `ls -a`
		do
			if test "$j" = "." -o "$j" = ".."; then continue;
			else
			FILES="$FILES $j"
			fi
		done
		if test "$FILES"; then
		uuto $sub $FILES $1;fi)
	error=0
	done
fi
if test -n "$f" -a -n "$user"
then
	uucp $a $f $remote!~/receive/$user/$mysys$UUP/
	error=0
fi
if test $error = 1
then 
	echo "$mesg" 1>&2
	exit 2
fi
