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
# $Header: calendar.sh,v 1.9.2.3 90/05/09 15:17:44 wje Exp $
#
PATH=/bin:/usr/bin:
Cpp=/lib/cpp
if [ ! -f "$Cpp" ]
then
	Cpp=/usr/lib/cpp
fi

#
# The function isok() makes sure the named file exists, is readable,
# and resides on a non-NFS filesystem. The final check is used to make
# sure that users sharing home directories only get one message.
#

isok()
{
	if [ ! -r "$1" ]
	then
		return 1
	fi

	found=`find "$1" -local -print 2>/dev/null`
	case "$found" in
		"")
			return 1
			;;
		*)
			return 0
			;;
	esac
}

	
tmp=/tmp/cal0$$
trap "rm -f $tmp /tmp/cal1$$ /tmp/cal2$$"
trap exit 1 2 13 15
/usr/lib/calprog >$tmp
case $# in
0)
	trap 'rm -f $tmp ; exit 1' 0 1 2 13 15
	if [ ! -r calendar ]
	then
		echo "Warning: No calendar file"
		exit 1
	fi
	($Cpp calendar | egrep -f $tmp);;
*)
	trap "rm -f $tmp /tmp/cal1$$ /tmp/cal2$$; exit" 0 1 2 13 15
	echo "Subject: Calendar for \c" > /tmp/cal1$$
	date | sed -e "s/ [0-9]*:.*//" >> /tmp/cal1$$
	echo "" >> /tmp/cal1$$
	sed '
		s/\([^:]*\):.*:\(.*\):[^:]*$/y=\2 z=\1/
	' /etc/passwd \
	| while read x
	do
		eval $x
		if isok $y/calendar
		then
			($Cpp $y/calendar | egrep -f $tmp) 2>/dev/null > /tmp/cal2$$
			if test -s /tmp/cal2$$
			then
				( echo "To: $z" ;\
				  cat /tmp/cal1$$ /tmp/cal2$$ ) | mail "$z"
			fi
		fi
	done
esac
rm -f $tmp /tmp/cal1$$ /tmp/cal2$$
trap "" 0
exit 0
