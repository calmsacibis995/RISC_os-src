#!/bin/sh -
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
# $Header: makewhatis.sh,v 1.3.2.2 90/05/07 18:51:55 wje Exp $
#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)makewhatis.sh	5.3 (Berkeley) 3/29/86
#

trap "rm -f /tmp/whatisx.$$ /tmp/whatis$$; exit 1" 1 2 13 15
USAGE='makewhatis [-GETNAME getNAME] [-EXPAND expand] [-UNEXPAND unexpand] mandir'
MANDIR=/usr/man
GETNAME=/usr/lib/getNAME
if test -x /usr/ucb/expand
then
	EXPAND=/usr/ucb/expand ;
	UNEXPAND=/usr/ucb/unexpand ;
else
	EXPAND=/bsd43/bin/expand
	UNEXPAND=/bsd43/bin/unexpand
fi
while test "$#" -gt 0; do
	case "$1" in
	-GETNAME)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    GETNAME=$2
	    shift; shift
	    ;;
	-EXPAND)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    EXPAND=$2
	    shift; shift
	    ;;
	-UNEXPAND)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    UNEXPAND=$2
	    shift; shift
	    ;;
	-*)
	    echo "$USAGE"; exit 1
	    ;;
	*)
	    if test "$#" -gt 1; then echo "$USAGE"; exit 1; fi
	    MANDIR=$1
	    shift
	    ;;
	esac
done
	
rm -f /tmp/whatisx.$$ /tmp/whatis$$
if test ! -d $MANDIR ; then exit 0 ; fi
cd $MANDIR
top=`pwd`
for j in u_man p_man a_man x_man l_man . 
do
 if [ -d $j ] ; then
    for k in "" bsd_ 
      do
    	for i in man1 man2 man3 man4 man5 man6 man7 man8 mann \
		 manl manp mano mans
	    do
		if [ -d $j/$k$i ] ; then
			cd $j/$k$i
		 	if test "`echo *.*`" != "*.*" ; then
				$GETNAME *.*
			fi
			cd $top
		fi
	    done
       done
  fi
done >/tmp/whatisx.$$
sed  </tmp/whatisx.$$ >/tmp/whatis$$ \
	-e 's/\\-/-/' \
	-e 's/\\\*-/-/' \
	-e 's/ VAX-11//' \
	-e 's/\\f[PRIB0123]//g' \
	-e 's/\\s[-+0-9]*//g' \
	-e 's/.TH [^ ]* \([^ 	]*\).*	\([^-]*\)/\2(\1)	/' \
	-e 's/	 /	/g'
$EXPAND -24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100 \
	/tmp/whatis$$ | sort | uniq | $UNEXPAND -a > whatis
chmod 664 whatis >/dev/null 2>&1
rm -f /tmp/whatisx.$$ /tmp/whatis$$
exit 0
