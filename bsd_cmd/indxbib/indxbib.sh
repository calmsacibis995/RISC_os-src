#! /bin/sh
#
#|-----------------------------------------------------------|
#| Copyright (c) 1990 MIPS Computer Systems, Inc.            |
#| All Rights Reserved                                       |
#|-----------------------------------------------------------|
#|          Restricted Rights Legend                         |
#| Use, duplication, or disclosure by the Government is      |
#| subject to restrictions as set forth in                   |
#| subparagraph (c)(1)(ii) of the Rights in Technical        |
#| Data and Computer Software Clause of DFARS 52.227-7013.   |
#|         MIPS Computer Systems, Inc.                       |
#|         950 DeGuigne Drive                                |
#|         Sunnyvale, CA 94086                               |
#|-----------------------------------------------------------|
#   $Header: indxbib.sh,v 1.1.2.2.1.2 90/08/06 14:46:23 hawkes Exp $
#	@(#)indxbib.sh	4.1	(Berkeley)	83/05/08
#
#	indxbib sh script
#
case "$#" in
	0)
		echo 'Usage:  indxbib database [ ... ]
		first argument is the basename for indexes
		indexes will be called database.{ia,ib,ic}'
		;;

	*)
		/bsd43/usr/lib/refer/mkey "$@" | /bsd43/usr/lib/refer/inv "_$1"
		mv _$1.ia $1.ia
		mv _$1.ib $1.ib
		mv _$1.ic $1.ic
		;;
esac
