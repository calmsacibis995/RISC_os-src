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
# $Header: dircmp.sh,v 1.6.2.2 90/05/09 15:46:12 wje Exp $

#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

# #ident	"@(#)dircmp:dircmp.sh	1.12"
PATH=/bin:/usr/bin
USAGE="dircmp: usage: dircmp -s -d -wn directory directory"
trap "rm -f /usr/tmp/dc$$*;exit" 1 2 3 15
Sflag=""
Dflag=""
width=72
if type getopts | grep 'not found' > /dev/null
then
	eval set -- "`getopt dsw: "$@"`"
	if [ $? != 0 ]
	then
		echo "$USAGE" 1>&2
		exit 2
	fi
	for i in $*
	do
		case $i in
		-d)	Dflag="yes"; shift;;
		-s)	Sflag="yes"; shift;;
		-w)	width=`expr $2 + 0 2>/dev/null`
			if [ $? = 2 -o "$width" -lt 1 ]
			then echo "dircmp: positive numeric argument required" 1>&2
				exit 2
			fi
			shift 2
			;;
		--)	shift; break;;
		esac
	done
else
	while getopts dsw: i
	do
		case $i in
		d)	Dflag="yes";; 
		s)	Sflag="yes";; 
		w)	width=`expr $OPTARG + 0 2>/dev/null`
			if [ $? = 2 -o "$width" -lt 1 ]
			then echo "dircmp: positive numeric argument required" 1>&2
				exit 2
			fi
			;;
		\?)	echo "$USAGE" 1>&2
			exit 2;;
		esac
	done
	shift `expr $OPTIND - 1`
fi
D0=`pwd`
D1=$1
D2=$2
if [ $# -lt 2 ]
then echo "$USAGE" 1>&2
     exit 1
elif [ ! -d "$D1" ]
then echo "$D1 not a directory!" 1>&2
     exit 2
elif [ ! -d "$D2" ]
then echo "$D2 not a directory!" 1>&2
     exit 2
fi
cd $D1
find . -print | sort > /usr/tmp/dc$$a
cd $D0
cd $D2
find . -print | sort > /usr/tmp/dc$$b
comm /usr/tmp/dc$$a /usr/tmp/dc$$b | sed -n \
	-e "/^		/w /usr/tmp/dc$$c" \
	-e "/^	[^	]/w /usr/tmp/dc$$d" \
	-e "/^[^	]/w /usr/tmp/dc$$e"
rm -f /usr/tmp/dc$$a /usr/tmp/dc$$b
pr -w${width} -h "$D1 only and $D2 only" -m /usr/tmp/dc$$e /usr/tmp/dc$$d
rm -f /usr/tmp/dc$$e /usr/tmp/dc$$d
sed -e s/..// < /usr/tmp/dc$$c > /usr/tmp/dc$$f
rm -f /usr/tmp/dc$$c
cd $D0
> /usr/tmp/dc$$g
while read a
do
	if [ -d $D1/"$a" ]
	then if [ "$Sflag" != "yes" ]
	     then echo "directory	$a"
	     fi
	elif [ -f $D1/"$a" ]
	then cmp -s $D1/"$a" $D2/"$a"
	     if [ $? = 0 ]
	     then if [ "$Sflag" != "yes" ]
		  then echo "same     	$a"
		  fi
	     else echo "different	$a"
		  if [ "$Dflag" = "yes" ]
		  then
			type=`file $D1/"$a"`
			case "$type" in
				*text)	;;
				*empty)	echo $D1/`basename "$a"` is an empty file |
					 pr -h "diff of $a in $D1 and $D2" >> /usr/tmp/dc$$g
					continue
				;;
				*)	echo $D1/`basename "$a"` is an object file |
					 pr -h "diff of $a in $D1 and $D2" >> /usr/tmp/dc$$g
					continue
				;;
			esac
			type=`file $D2/"$a"`
			case "$type" in
				*text)	;;
				*empty)	echo $D2/`basename "$a"` is an empty file |
					 pr -h "diff of $a in $D1 and $D2" >> /usr/tmp/dc$$g
					continue
				;;
				*)	echo $D2/`basename "$a"` is an object file |
					 pr -h "diff of $a in $D1 and $D2" >> /usr/tmp/dc$$g
					continue
				;;
			esac
			diff $D1/"$a" $D2/"$a" | pr -h "diff of $a in $D1 and $D2" >> /usr/tmp/dc$$g
		  fi
	     fi
	elif [ "$Sflag" != "yes" ]
	then echo "special  	$a"
	fi
done < /usr/tmp/dc$$f | pr -r -h "Comparison of $D1 $D2"
if [ "$Dflag" = "yes" ]
then cat /usr/tmp/dc$$g
fi
rm -f /usr/tmp/dc$$*
