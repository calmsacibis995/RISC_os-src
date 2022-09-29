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
# $Header: diction.sh,v 1.2.1.2 90/05/07 18:20:11 wje Exp $
#
#	@(#)diction.sh	4.5	(Berkeley)	82/11/06
#
D=/bsd43/bin
B=/bsd43/usr/lib
echo $*
rest=
flag=
nflag=
mflag=-me
lflag=-ml
kflag=
file=
for i
do case $i in
 -f) flag=-f;shift; file=$1; shift; continue;;
-n) nflag=-n;shift; continue;;
-k) kflag=-k;shift; continue;;
 -mm) mflag=$1; shift; continue;;
-ms) mflag=$1;shift;continue;;
-me) mflag=$1;shift;continue;;
-ma) mflag=$1;shift;continue;;
-ml) lflag=$1;shift;continue;;
*) rest=$*; break;;
esac
done
 $D/deroff $kflag $lflag $mflag $rest^$B/dprog -d $nflag $flag $file
