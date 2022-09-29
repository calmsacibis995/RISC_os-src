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
# $Header: TPascal.sh,v 1.1.1.2 90/05/09 18:07:10 wje Exp $
(mkmf LINKER=pc; cat Makefile) |& diff - OPascal
set diffstatus = $status
/bin/rm -f Makefile main.p a.p b.h defs
exit($diffstatus)
