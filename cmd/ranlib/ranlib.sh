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
# $Header: ranlib.sh,v 1.3.2.2 90/05/09 18:23:08 wje Exp $
#
#  simulate "ranlib" with mips ar ts
#  (mainly for makefile compatibility)
#
PATH=/bin:/usr/bin
Myname=`basename "$0"`
while :
do
	case "$1" in
		-*)
			echo "$Myname: usage : $Myname filename..." 1>&2
			exit 2
			;;
		*)
			break
			;;
	esac
done

case "$#" in
	0)
		echo "$Myname: usage : $Myname filename..." 1>&2
		exit 2
		;;
esac

for afile
{
    ar ts "$afile" >/dev/null
}
exit 0
