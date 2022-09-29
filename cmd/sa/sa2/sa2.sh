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
# $Header: sa2.sh,v 1.6.2.2 90/05/09 18:26:15 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

# #ident	"@(#)sa:sa2.sh	1.3"
#	sa2.sh 1.3 of 5/13/85
DATE=`date +%d`
RPT=/usr/adm/sa/sar$DATE
DFILE=/usr/adm/sa/sa$DATE
ENDIR=/usr/bin
cd $ENDIR
$ENDIR/sar $* -f $DFILE > $RPT
find /usr/adm/sa \( -name 'sar*' -o -name 'sa*' \) -type f -mtime +7 -exec rm {} \;
