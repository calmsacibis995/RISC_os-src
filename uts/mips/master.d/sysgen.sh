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
# $Header: sysgen.sh,v 1.2.6.2 90/05/10 05:40:16 wje Exp $

# construct a 'system' file with which to generate a kernel, using lboot

# usage:
USAGE='sysgen [-v] [-i input] [-noprobe | -f dev1 [-f dev2] ...]  \
	[-l lboot] [-s system] [-m masterd] [-b bootarea] [-u name] \
	[options]'
# '-f dev' directs that any device module name 'dev' should be included, but
#	not probed for.
# '-noprobe' says that all devices should be assumed to be present.

# The input system schema contains lines prefixed with '#opt#' or
#	'#opt1,opt2,...,optn#' and lines without such prefixes.  An
#	'opt' should be a string of alphanumeric characters with an
#	optional leading '!'.  The exclamation point says the line should
#	be delete if the option is requested. 


NC='[a-zA-Z0-9_,!]*'
WC='[a-zA-Z0-9_]'
NWC='[^a-zA-Z0-9_]'

DEVS=
OPTS=

while test "$#" -gt 0; do
	case "$1" in
	-v)
	    BOPTS="${BOPTS-} -v"
	    shift
	    ;;

	-i)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    IN=$2
	    shift; shift
	    ;;

	-noprobe)
	    ALLDEVS="-e /^VECTOR\:/s/probe[_size]*=$WC*//g"
	    shift
	    ;;

	-f)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    DEVS="$DEVS \
		    -e /^VECTOR:.*module=$2$NWC/s/probe[_size]*=$WC*//g"
	    shift; shift
	    ;;

	-l)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    LBOOT=$2
	    shift; shift
	    ;;

	-m)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    MASTERD=$2
	    shift; shift
	    ;;

	-s)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    OUT=$2
	    shift; shift
	    ;;

	-b)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    BOOTA=$2
	    shift; shift
	    ;;
	
	-u)
	    if test "$#" -lt 2; then echo "$USAGE"; exit 1; fi
	    NAME=$2
	    shift; shift
	    ;;
	
	-*) 
	    echo "$USAGE"; exit 1
	    ;;

	*)
	    OPTS="$OPTS \
		-e /^#!$1[,#]/s/^#$NC#INCLUDE/EXCLUDE/ \
		-e /^#$NC,!$1[,#]/s/^#$NC#INCLUDE/EXCLUDE/  \
		-e /^#!$1[,#]/d \
		-e /^#$NC,!$1[,#]/d  \
		-e /^#$1[,#]/s/^#$NC#// \
		-e /^#$NC,$1[,#]/s/^#$NC#//"
	    shift
	    ;;
	esac
done

if test "${OUT=sysgen.tmp$$}" = "${IN=${MASTERD=/master.d}/system}"; then
	echo "$IN" and "$OUT" are the same.
	exit 1
fi
if test ! -f $IN; then
	echo "$IN" does not exist.
	exit 1
fi

rm -f sysgen.unix* sysgen.tmp*
set -e
sed $OPTS -e "s/^#$NC!$NC#//" -e "s/^#$NC#INCLUDE/EXCLUDE/" -e '/^#/d' \
	$DEVS ${ALLDEVS-} \
	-e "/^LDOPTS:/s/-o[	 ]*[^	 ]*/-o ${NAME=unix}/g" \
	$IN > $OUT

${LBOOT=lboot} ${BOPTS=} -m $MASTERD -b ${BOOTA=/boot} \
	-s $OUT
rm -f sysgen.tmp$$
