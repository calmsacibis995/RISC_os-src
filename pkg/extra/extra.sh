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
# $Header: extra.sh,v 1.4.2.2 90/05/10 03:46:53 wje Exp $
# extra - Generate a list of files not listed in a set of BOM files.
#
#
# Usage: extra directory filename...
#
# This command takes a "root" directory name and a set of BOM files (or
# any files whose first 'awk' field is a set of filenames, and produces
# a list of files that exist in the directory that are not in the BOMs.
#
# The exit code is 0 unless a problem was found ("problem" does not
# mean extra files).
#

PATH=/bin:/usr/bin
export PATH

#
# Make sure that the root directory exists.
#

Root="$1"
if [ " $Root" = " " -o ! -d "$Root" ]
then
	echo "$0: Invalid directory name - $Root" 1>&2
	exit 1
fi
shift

#
# Make sure that all of the BOM files exist, and build the master list.
#

Master=/tmp/comply.master.$$
for file in "$@"
{
	if [ ! -f "$file" ]
	then
		if [ " $*" = " " ]
		then
			echo "$0: No BOM files given" 1>&2
			exit 1
		fi
		echo "$0: Nonexistent BOM file - $file" 1>&2
		rm -f "$Master"
		exit 1
	fi
	cat "$file"
} | awk '{print $1}' | sort -u > "$Master"

#
# Go over to the root directory and get a list of all files.
#

cd "$Root"
Exist=/tmp/comply.exist.$$
find . -print 2>/dev/null | sed 's/^\.\///' | sort > "$Exist"

#
# Generate the list of files not in Master.
#

comm -23 "$Exist" "$Master"

rm -f "$Exist" "$Master"

exit 0
