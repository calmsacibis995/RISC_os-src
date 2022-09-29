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
# $Header: bomerge.sh,v 1.1.2.2 90/05/10 03:43:24 wje Exp $
#
#   usage: bomerge bomfile ...
#

trap "/bin/rm -f /tmp/bomerge.$$ /tmp/bomdup.$$; exit 1" 1 2 3 15

if [ $# -lt 1 ]
  then
    echo "`basename $0`: usage: `basename $0` bomfile ..."
    exit 1
  fi

#
#  round 'em up and shmoosh `em out...
#
#  Notes:
#   1. cat *		combines divided boms
#   2. sed		insures newlines between adjacent files
#   3. sort		sorts 'em (important since dir entries must
#			precede files in those directories in the
#			boms -- comply now makes directories)
#   4. tee		stashes a copy of the results...
#   5. awk		checks for conflicting duplicate entries
#   6. uniq 		removed redundant entries
#

#
# First, make sure all of the files named in the argument list exist
#

Exit=0

for File in $@
  do
    if [ ! -f $File ]
      then
        echo "$0: cannot open $File"
	Exit=1
      fi
  done

case $Exit in
  0) ;;
  *) exit $Exit
  esac

cat $@ | sed 'p' | sort | tee /tmp/bomerge.$$ |
          awk ' BEGIN      { last1 = ""; last = "" }
		$0 !~ /^#/ { if ($1 == last1 && $0 != last)
			       printf "%s\n%s\n----------\n", last, $0
			     last1 = $1; last = $0 } ' >/tmp/bomdup.$$ 2>&1

if [ -s /tmp/bomdup.$$ ]
  then
    cat /tmp/bomdup.$$
    /bin/rm -f /tmp/bomerge.$$ /tmp/bomdup.$$
    exit 1
  else
    cat /tmp/bomerge.$$ | uniq
    /bin/rm -f /tmp/bomerge.$$ /tmp/bomdup.$$
    exit 0
  fi


