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
# $Header: dirname.sh,v 1.5.2.2 90/05/09 15:46:24 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

if [ $# -gt 1 ]
then
	echo >&2 "Usage: dirname [ path ]"
	exit 1
fi
#	First check for pathnames of form //*non-slash*/* in which case the 
#	dirname is /.
#	Otherwise, remove the last component in the pathname and slashes 
#	that come before it.
#	If nothing is left, dirname is "."
exec /bin/expr \
	"${1:-.}/" : '\(/\)/*[^/]*//*$'  \| \
	"${1:-.}/" : '\(.*[^/]\)//*[^/][^/]*//*$' \| \
	.
