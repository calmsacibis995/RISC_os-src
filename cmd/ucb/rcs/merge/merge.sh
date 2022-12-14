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
# $Header: merge.sh,v 1.4.2.2 90/05/10 00:27:35 wje Exp $
# Three-way file merge. 
# use: merge [-p] file1 file2 file3 [mark1 mark3]
# Effect: incorporates all changes that lead from file2 to file3 into file1.
# Result goes to std. output if -p is present, into file1 otherwise.
# Overlapping changes are delimited as follows:
# <<<<<<< file1
# lines in file1
# =======
# lines in file3
# >>>>>>> file3
# If mark1 and mark3 are given the delimiting lines <<<<.. and >>>>...
# contain mark1 and mark3 instead of the names of file1 and file3.
# A warning is printed if there are overlaps.

PATH=/bin:/usr/bin:.
DIFF3=/usr/lib/rdiff3
p=0

usage()
{
	echo "usage: merge [-p] file1 file2 file3 [mark1] [mark2]" 1>&2
}

while :
do
	case "$1" in
	-p)
		p=-p
		shift
		;;
	-*)
		usage
		exit 2
		;;
	*)
		break
		;;
	esac
done


if [ $# -ge 3 ]
then
        if [ -f "$1" -a -f "$2" -a -f "$3" ]
        then
                trap "rm -f /tmp/d3[abc]$$" 0 1 2 13 15
                diff "$1" "$3" >/tmp/d3a$$
                diff "$2" "$3" >/tmp/d3b$$
                $DIFF3 -E /tmp/d3[ab]$$ "$1" "$2" "$3" $4 $5 > /tmp/d3c$$
                r=$?
                if [ $r != 0 ]
                then
                        echo Warning: $r overlaps during merge. 1>&2
                fi
                if [ $p != 0 ]
                then
                        (cat /tmp/d3c$$; echo '1,$p') | ed - "$1"
                else
                        (cat /tmp/d3c$$; echo w) | ed - "$1"
                fi
                exit 0
        else
		for i in "$1" "$2" "$3"
		{
			if [ ! -f "$i" ]
			then
				echo "Cannot read $i" 1>&2
				exit 1
			fi
		}
		echo "Cannot open $1, $2, or $3" 1>&2
		exit 1
        fi
fi
usage
exit 2
