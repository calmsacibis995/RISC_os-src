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
# $Header: prctmp.sh,v 1.3.1.2 90/05/09 15:08:28 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.


#	"print session record file (ctmp.h/ascii) with headings"
#	"prctmp file [heading]"
PATH=/usr/lib/acct:/bin:/usr/bin:/etc
(cat <<!; cat $*) | pr -h "SESSIONS, SORTED BY ENDING TIME"

MAJ/MIN			CONNECT SECONDS	START TIME	SESSION START
DEVICE	UID	LOGIN	PRIME	NPRIME	(NUMERIC)	DATE	TIME


!
