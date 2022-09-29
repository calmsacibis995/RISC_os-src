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
# $Header: updatedb.sh,v 1.2.1.2 90/05/07 18:33:03 wje Exp $
#
# Rewritten as a /bin/sh script from a 4.3BSD csh script
#
SRCHPATHS=/
LIBDIR=/usr/lib/find
ERRORSTO=root
FCODES=/usr/lib/find/find.codes

PATH=$LIBDIR:/bsd43/bin:/usr/ucb:/bin:/usr/bin
Bigrams=/tmp/f.bigrams.$$
Filelist=/tmp/f.list$$
Errs=/tmp/f.errs$$

# Make a file list and compute common bigrams.
# Alphabetize '/' before any other char with 'tr'.
# If the system is very short of sort space, 'bigram' can be made
# smarter to accumulate common bigrams directly without sorting
# ('awk', with its associative memory capacity, can do this in several
# lines, but is too slow, and runs out of string space on small machines).

/bsd43/bin/find ${SRCHPATHS} -fstype nfs -prune -o -print | tr '/' '\001' | \
   (sort -f; echo $? > "$errs") | \
   tr '\001' '/' > "$Filelist"
$LIBDIR/bigram < "$Filelist" | \
   (sort; echo $? >> "$Errs") | uniq -c | sort -nr | \
   awk '{ if (NR <= 128) print $2 }' | tr -d '\012' > "$Bigrams"

# code the file list

if grep -s -v 0 "$Errs"
then
	echo 'squeeze error: out of sort space' | mail $ERRORSTO
else
	$LIBDIR/code "$Bigrams" < "$Filelist" > "$FCODES"
	chmod 644 "$FCODES"
	rm -f "$Bigrams" "$Filelist" "$Errs"
fi
