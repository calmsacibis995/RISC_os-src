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
# $Header: basename.sh,v 1.5.2.2 90/05/09 15:14:27 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

if [ $# -gt 2 ]
then
	echo >&2 "Usage:  basename [ path [ suffix-pattern ] ]"
	exit 1
fi
#	If no first argument or first argument is null, make first argument
#	"."  Add beginning slash, then remove trailing slashes, then remove 
#	everything up through last slash, then remove suffix pattern if 
#	second argument is present.
#	If nothing is left, first argument must be of form //*, in which
# 	case the basename is /.
exec /bin/expr \
	"/${1:-.}" : '\(.*[^/]\)/*$' : '.*/\(..*\)' : "\\(.*\\)$2\$"  \|  \
	"/${1:-.}" : '\(.*[^/]\)/*$' : '.*/\(..*\)'    \|  \
	"/${1:-.}" : '.*/\(..*\)' 
