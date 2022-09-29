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
# $Header: driver.sh,v 1.6.2.2 90/05/09 18:16:50 wje Exp $
#
#
# Execute periodic task files
#
# Usage: /usr/adm/periodic/driver period
#
# period is either "hourly", "daily", "weekly", or "monthly".
#
# The basic idea is to execute files in the subdirectory of
# /usr/adm/periodic named by the period argument.  The files
# executed must have a name beginning with a digit, and end with
# a suffix that is composed of a . followed by the word "system",
# the word "local", or the current hostname.  In addition, if there
# is a file with the same name plus the extra suffix ".stop", execution
# is held and a message and the contents of the stop file are printed.
#
# Note that this program should only be run by cron.
#

PATH=/usr/net:/usr/bin:/bin:/usr/ucb:/usr/new:/etc

Hostname=`hostname`

#
# main() gets the list of files and executes the ones that aren't
# held.
#

main()
{

	case "$1" in
		hourly)
			echo "Subject: Hourly run output for host $Hostname\n"
			;;
		daily)
			echo "Subject: Daily run output for host $Hostname\n"
			;;
		weekly)
			echo "Subject: Weekly run output for host $Hostname\n"
			;;
		monthly)
			echo "Subject: Monthly run output for host $Hostname\n"
			;;
		*)
			echo "Subject: ERROR in periodic run\n"
			echo "Unknown period $1"
			exit 1
			;;
	esac

	getnames "$1" | sort | while read name
	do
		if [ -f "$name.stop" ]
		then
			echo "Execution of $name stopped"
			cat "$name.stop"
			continue
		fi
		eval nice -15 "$name" </dev/null
	done

}

#
# getnames() generates a list of names of files found in the requested
# periodic directory.  It is careful to only print the names of existing
# files and avoid error messages.
#

getnames()
{
	for i in system local "$Hostname"
	{
		list="`echo /usr/adm/periodic/$1/[0-9]*.$i`"
		case "$list" in
			'/usr/adm/periodic/'"$1"'/[0-9]*.'"$i")
				continue
				;;
		esac
		ls /usr/adm/periodic/$1/[0-9]*.$i
	}
}

main ${1+"$@"}
exit 0
