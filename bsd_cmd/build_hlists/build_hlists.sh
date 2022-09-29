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
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# build_hlists - build spell hash lists in /usr/dict
#
# $Header: build_hlists.sh,v 1.3.1.3 90/08/08 17:58:58 hawkes Exp $
#
# This command checks to make sure that the hash lists are up to
# date and non-empty. If not, they are rebuilt.
#

PATH=/bin:/usr/bin:/bsd43/bin
export PATH
cd $DESTROOT/usr/dict

#
# Make sure all source files exist
#

rm -f build_errors
if [ ! -f american -o ! -f british -o ! -f local -o ! -f stop -o ! -f words ]
then
	echo "$0 : Missing source file" > build_errors
	exit 1
fi

#
# Start with hlist - made from words
#

Rebuild="no"
while :
do
	if [ ! -s hlist ]
	then
		Rebuild="yes"
		break
	fi

	Out_of_date=`find words -newer hlist -print 2>&1`
	case "$Out_of_date" in
		"")
			;;

		*)
			Rebuild="yes"
			;;
	esac
	break
done

case "$Rebuild" in
	"yes")
		spellin < words > hlist
		/bin/chown bin hlist
		/bin/chgrp bin hlist
		/bin/chmod 444 hlist
		;;
esac

#
# Now hlista - made from american and local
#

Rebuild="no"
while :
do
	if [ ! -s hlista ]
	then
		Rebuild="yes"
		break
	fi

	Out_of_date=`find american local -newer hlista -print 2>&1`
	case "$Out_of_date" in
		"")
			;;

		*)
			Rebuild="yes"
			;;
	esac
	break
done

case "$Rebuild" in
	"yes")
		cat american local | spellin hlist > hlista
		/bin/chown bin hlista
		/bin/chgrp bin hlista
		/bin/chmod 444 hlista
		;;
esac

#
# Now hlistb - made from british and local
#

Rebuild="no"
while :
do
	if [ ! -s hlistb ]
	then
		Rebuild="yes"
		break
	fi

	Out_of_date=`find british local -newer hlistb -print 2>&1`
	case "$Out_of_date" in
		"")
			;;

		*)
			Rebuild="yes"
			;;
	esac
	break
done

case "$Rebuild" in
	"yes")
		cat british local | spellin hlist > hlistb
		/bin/chown bin hlistb
		/bin/chgrp bin hlistb
		/bin/chmod 444 hlistb
		;;
esac

#
# Now hstop - made from stop
#

Rebuild="no"
while :
do
	if [ ! -s hstop ]
	then
		Rebuild="yes"
		break
	fi

	Out_of_date=`find stop -newer hstop -print 2>&1`
	case "$Out_of_date" in
		"")
			;;

		*)
			Rebuild="yes"
			;;
	esac
	break
done

case "$Rebuild" in
	"yes")
		spellin < stop > hstop
		/bin/chown bin hstop
		/bin/chgrp bin hstop
		/bin/chmod 444 hstop
		;;
esac

exit 0
