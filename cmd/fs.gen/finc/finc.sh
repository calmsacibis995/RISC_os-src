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
# $Header: finc.sh,v 1.3.2.2 90/05/09 16:02:30 wje Exp $

#
# Error printing wrapper for fs commands that don't have a useful
# wrapper.
#
# For some fs commands, it may be possible to provide an intelligent 
# wrapper that will switch to the correct function based on file 
# system type.
#
# For other fs commands, this is difficult because the arguments are
# different and cannot be easily mapped from a common argument set.
#

CMD=`basename $0`

echo "$CMD: Use $CMD.ffs or $CMD.S51K according to file system type." 1>&2
exit 1
