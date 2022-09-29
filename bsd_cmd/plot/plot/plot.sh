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
# $Header: plot.sh,v 1.2.1.3 90/05/07 19:05:07 wje Exp $
#
#	@(#)plot.sh	4.5	(Berkeley)	8/29/86
#
# plot - plot file on specified device
#
# usage : plot [ -Tterm ] [ -rresolution ] [ files... ]
#
# Warning: -Tterm MUST be the first argument if used
#
PATH=/bsd43/bin:/bin:/usr/bin:/usr/ucb
case $1 in
-T*)	t=$1
	shift ;;
*)	t=-T$TERM
esac
case $t in
-T450)			exec t450 $*;;
-T300)			exec t300 $*;;
-T300S|-T300s)		exec t300s $*;;
-Tver)			exec lpr -Pversatec -g $*;;
-Tvar)			exec lpr -Pvarian -g $*;;
-Ttek|-Ttek4014|-T4014|-T)
			exec tek $* ;;
-T4013|-Ttek4013)	exec t4013 $* ;;
-Tbitgraph|-Tbg)	exec bgplot $*;;
-Tgigi|-Tvt125)		exec gigiplot $*;;
-Taed)			exec aedplot $*;;
-Thp7221|-Thp7|-Th7)	exec hp7221plot $*;;
-Thp|-T2648|-T2648a|-Thp2648|-Thp2648a|h8)
			exec hpplot $*;;
-Tip|-Timagen)		exec implot $*;;
-Tgrn)			exec grnplot $*;;
-Tcrt)			exec crtplot $*;;
-Tdumb|un|unknown)	exec dumbplot $*;;
*)  			exec crtplot $*;;
esac
