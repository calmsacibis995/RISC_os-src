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
# $Header: keysrch.sh,v 1.5.2.2 90/05/09 16:12:54 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

# #ident	"@(#)help:keysrch.sh	1.3"

echo "\nCommands found using \\S${*}\\S:\n"
for word
do
	line="${line}`grep \"^key='${word}'	\" ${KEYWORDS_TABLE}`"
	if [ $? -eq 0 ]
	then
		line="${line};"
	fi
done
if [ -z "${line}" ]
then
	echo "\n\n\n               No commands found using \\S ${*} \\S\n"
else
	eval "${line}" | sort -u
fi
