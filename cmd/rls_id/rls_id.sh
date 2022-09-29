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
# $Header: rls_id.sh,v 1.2.2.2 90/05/09 18:24:18 wje Exp $
#
# rls_id - generate a release ID file
#
# Usage: rls_id [-c] message [output-file]
#
# The "message" is placed in static strings in the output-file (default
# is $RLS_ID_OBJECT) such that ident(1) and what(1) will print the
# message. See the man page for typical use.
#
# If -c is given, the C source is printed on the standard output
# instead of creating an object file.
#
PATH=/bin:/usr/bin
Myname=`basename "$0"`

Rid_src=rid.$$.c
Rid_obj=rid.$$.o
Rid_dir=/tmp

main()
{

	compile="yes"
	while :
	do
		case "$1" in
			-c)
				compile="no"
				;;
			-*)
				usage
				exit 1
				;;
			*)
				break
				;;
		esac
		shift
	done

	output="$RLS_ID_OBJECT"
	case "$#" in
		1)
			message="$1"
			;;
		2)
			message="$1"
			output="$2"
			;;
		*)
			usage
			exit 1
			;;
	esac

	if bad_message "$message"
	then
		exit 1
	fi

	case "$compile" in
		"no")
			rid_src "$message"
			exit 0
			;;
	esac

	case "$output" in
		"")
			err "RLS_ID_OBJECT not set -- output filename required"
			exit 1
			;;
	esac

	if cp /dev/null "$output" 2>/dev/null
	then
		:
	else
		err "Can not create $output"
	fi

	make_rid "$message" "$output"
}

#
# The function err() prints an error message, prefacing it with the
# program name, and putting the message on standard error.
# 

err()
{
	echo "$Myname: $1" 1>&2
}

#
#  The function usage() prints a usage message.
#

usage()
{
	err "usage: $Myname [-c] message [output-file]"
}

#
# The function bad_message() makes sure that the message contains
# only characters that can be in a message. If there are any other
# characters, the return value to 0. Otherwise, 1 is returned.
#

bad_message()
{
	ok="-'"'[A-Z][a-z][0-9]!@#%^&*()_+={}\[\]\`~:;<,.?/\\| \t'
	bad=`echo "$1" | tr -d "$ok"`
	case "$bad" in
		"")
			return 1
			;;
		*)
			err "Message contains unuseable characters: $bad"
			return 0
			;;
	esac
}

#
# The function make_rid() puts the release ID information in a file
# compiles the file, and places the result in the output file.
#

make_rid()
{
	here=`pwd`
	message="$1"
	output="$2"
	cd "$Rid_dir"
	rid_src "$message" > "$Rid_src"
	cc -c "$Rid_src"
	rm -f "$Rid_src"
	cd "$here"
	mv "$Rid_dir/$Rid_obj" "$output"
}

#
# The function rid_src() prints the C code containing the
# message strings.
#

rid_src()
{
	rmsg="\$Revision"
	echo "static char *_RIO_RCSID = \"$rmsg: $1\$\";"
	echo "static char *_RIO_SCCSID = \"@(#)$1\";"
}

main ${1+"$@"}
exit 0
