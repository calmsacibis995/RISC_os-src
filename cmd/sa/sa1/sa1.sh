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
# $Header: sa1.sh,v 1.5.2.2 90/05/09 18:26:01 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

# #ident	"@(#)sa:sa1.sh	1.4"
#	sa1.sh 1.4 of 5/13/85
DATE=`date +%d`
ENDIR=/usr/lib/sa
DFILE=/usr/adm/sa/sa$DATE
cd $ENDIR
if [ $# = 0 ]
then
	exec $ENDIR/sadc 1 1 $DFILE
else
	exec $ENDIR/sadc $* $DFILE
fi
