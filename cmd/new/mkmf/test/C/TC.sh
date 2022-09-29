#!/bin/csh -f
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
# $Header: TC.sh,v 1.1.1.2 90/05/09 18:05:50 wje Exp $
mkdir h proc
mv convert.l error.h globs.h parser.y prog.c scanner.l tab.h proc
mv cons.h h
(cd proc; mkmf; cat Makefile) |& diff - OC
set diffstatus = $status
/bin/rm -rf h proc
exit($diffstatus)
