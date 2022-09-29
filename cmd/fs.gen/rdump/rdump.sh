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
# $Header: rdump.sh,v 1.6.2.2 90/05/09 16:05:19 wje Exp $
#
# Smart front-end for rdump.  First, the key is parsed to eat the
# extra arguments.  The 'f' key is required.  Then, all filesystems
# listed are checked to make sure that they are ffs filesystems.  If
# none are listed, and the options w and W have not been given, /dev/usr
# is checked, since it is the default.

PATH=/bin:/usr/bin

Myname=`basename "$0"`

main()
{

	if check_type ${1+"$@"}
	then
		exec /etc/rdump.ffs ${1+"$@"}
	fi
	exit 2
}

#
# check_type() parses the command line
#
check_type()
{
	key="$1"

	case "$#" in
		0)
			error "No key argument"
			return 1
			;;
	esac
	shift

	case "$key" in
		*[Ww]*)
			return 0
			;;
	esac

	case "$key" in
		*f*)
			case "$#" in
				0)
					error "Not enough arguments"
					return 1
					;;
			esac
			shift
			;;
		*)
			error "No 'f' key given"
			return 1
			;;
	esac

	case "$key" in
		*s*)
			case "$#" in
				0)
					error "Not enough arguments"
					return 1
					;;
			esac
			shift
			;;
	esac

	case "$key" in
		*d*)
			case "$#" in
				0)
					error "Not enough arguments"
					return 1
					;;
			esac
			shift
			;;
	esac

	case "$#" in
		0)
			set "/dev/usr"
			;;
	esac

	for i
	{
		type=`get_fsinfo "$i"`
		case "$type" in
			ffs)
				;;
			"")
				error "$i: Unknown filesystem type"
				return 1
				;;
			*)
				error "$i: Can not dump filesystem type $type"
				return 1
				;;
		esac
	}
	return 0
}

#
# get_fsinfo() prints the type of the named filesystem.  This is done by
# executing df on the named item and extracting the name.
#

get_fsinfo()
{
	df "$1" 2>&1 | sed -n 's/^[^ ][^ ]*  *\([^ ]*\)  *[0-9].*$/\1/p'
}

#
# error() prints an error message
#

error()
{
	echo "$Myname: $*" 1>&2
}

main ${1+"$@"}
exit 0
