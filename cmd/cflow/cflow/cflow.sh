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
# $Header: cflow.sh,v 1.10.2.5 90/05/22 18:23:07 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

INVFLG=
DFLAG=
IFLAG=
DIR=/usr/lib
CC=/usr/bin/cc
# with changes to lpfx, we can use MIPS std lint1.  This is where it lives
# with 2.11 Compilers
LINT1=/usr/lib/cmplrs/cc/lint1
TMP=/usr/tmp/cf.$$
TMPG=$TMP.g
trap "rm -f $TMP.?; kill $$" 1 2 3
echo "" >$TMP.g
Usage="Usage: cflow [-r] [-ix] [-i_] [-d num] [-g file] [-f file] file..."

case "$#" in
	0)
		echo "$Usage" 1>&2
		exit 2
		;;
esac
	
while [ "$1" != "" ]
do
	case "$1" in
	-r)
		INVFLG=1
		;;
	-d*)
		DFLAG=$1
		;;
	-i*)
		IFLAG="$IFLAG $1"
		;;
	-f)
		cat $2 </dev/null >>$TMPG
		shift
		;;
	-g)
		TMPG=$2
		if [ "$TMPG" = "" ]
		then
			TMPG=$TMP.g
		fi
		shift
		;;
	-[IDU]*)
		o="$o $1"
		;;
	-*)
		echo "$Usage" 1>&2
		exit 2
		;;
	*.y)
		yacc $1
		sed -e "/^# line/d" y.tab.c > $1.c
		$CC -E $o $1.c | $LINT1 -H$TMP.j 2>/dev/null $1.c\
			| $DIR/lpfx $IFLAG >>$TMPG
		rm y.tab.c $1.c
		;;
	*.l)
		lex $1
		sed -e "/^# line/d" lex.yy.c > $1.c
		$CC -E $o $1.c | $LINT1 -H$TMP.j 2>/dev/null $1.c\
			| $DIR/lpfx $IFLAG >>$TMPG
		rm lex.yy.c $1.c
		;;
	*.c)
		$CC -E $o $1 | $LINT1 -H$TMP.j 2>/dev/null $1\
			| $DIR/lpfx $IFLAG >>$TMPG
		;;
	*.i)
		name=`basename $1 .c`
		$LINT1 -H$TMP.j 2>/dev/null <$1 | $DIR/lpfx >>$TMPG $name.c
		;;
	*.s)
		a=`basename $1 .s`
		as -o $TMP.o $1
		nm -he $TMP.o | sort -t'|' -n +1 -2 | $DIR/nmf $a ${a}.s >>$TMPG
		;;
	*.o)
		a=`basename $1 .o`
		nm -he $1 | sort -t'|' -n +1 -2 | $DIR/nmf $a ${a}.o >>$TMPG
		;;
	*)
		echo $1 "-- cflow can't process - file skipped"
		;;
	esac
	shift
done
if [ "$INVFLG" != "" ]
then
	grep "=" $TMPG >$TMP.q
	grep ":" $TMPG | $DIR/flip >>$TMP.q
	sort <$TMP.q >$TMPG
	rm $TMP.q
fi
$DIR/dag $DFLAG <$TMPG
ret=$?
rm -f $TMP.?
exit $ret
