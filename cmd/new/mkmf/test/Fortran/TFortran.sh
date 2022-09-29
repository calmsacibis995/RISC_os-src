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
# $Header: TFortran.sh,v 1.1.1.2 90/05/09 18:06:58 wje Exp $
(mkmf LINKER=fc; cat Makefile) |& diff - OFortran
set diffstatus = $status
/bin/rm -f Makefile program.f a.f b.h defs
exit($diffstatus)
