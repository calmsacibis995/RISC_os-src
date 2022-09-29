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
# $Header: stamp_links.sh,v 1.3.2.2 90/05/09 19:06:01 wje Exp $
#
# stamp_links - Set up symbolic links to commands whose names end in
#		the given stamp, without the stamp. This is done in
#		/usr/bin, /usr/lib, and /usr.
#
# Usage: stamp_links [ -f ] stamp
#
#  where stamp is of the form {digits}.{digit}{anything} unless -f
#  is given.

PATH=/usr/ucb:/bin:/usr/bin

Myname=`basename "$0"`
Force="no"

case "$1" in
	-f)
		Force="yes"
		shift
		;;
esac

case "$#" in
	1)
		;;
	*)
		echo "$Myname: usage: $Myname [-f] stamp"
		exit 1
		;;
esac

Stamp="$1"

#
# Check the stamp for validity
#

case "$Force" in
	"no")
		Stampmatch=`expr "$Stamp" : '^\([0-9][0-9]*\.[0-9].*\)$'`
		case "$Stampmatch" in
			"$Stamp")
				;;

			*)
				echo "$Myname: Invalid stamp"
				exit 1
				;;
		esac
		;;
esac

#
# The function do_include() copies all of the files in the include
# directory (which is the current directory when this function is
# executing) to /usr/include.
#

do_include()
{
	for file in *
	{
		rm -rf "/usr/include/$file"
		find "$file" -type f -print |\
			cpio -pdmul "/usr/include" 2>&1 |\
			grep -v 'blocks'
	}
}

#
# Look for names ending in the stamp in the various directories, and
# set up links to those files.
#

for Dir in /bin /lib /usr/bin /usr/lib /usr/new /usr/new/lib /usr
{
	if [ ! -d "$Dir" ]
	then
		continue
	fi

	cd "$Dir"
	for File in *"$Stamp"
	{
		case "$File" in
			"*$Stamp")
				break
				;;
			"include$Stamp")
				Here=`pwd`
				cd "$File"
				do_include
				cd "$Here"
				continue
				;;
		esac

		Linkname=`basename "$File" "$Stamp"`
		rm -f "$Linkname"
		ln -s "$File" "$Linkname"
	}
}

exit 0
