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
# $Header: lorder.sh,v 1.11.2.2 90/05/09 16:24:29 wje Exp $
case $# in
0)	echo usage: lorder file ... 1>&2
	exit 2;;
1)	case "$1" in
	*.o)	set "$1" "$1"
	esac
esac
trap "rm -f $$sym?ef; exit" 0 1 2 13 15
nm -Bg "$@" | sed '
	/^$/d
	/:$/{
		/\.o:/!d
		s/://
		h
		s/.*/& &/
		p
		d
	}
	/[TD] /{
		s/.* //
		G
		s/\n/ /
		w '$$symdef'
		d
	}
	s/.* //
	G
	s/\n/ /
	w '$$symref'
	d
'
sort $$symdef -o $$symdef
sort $$symref -o $$symref
join $$symref $$symdef | sed 's/[^ ]* *//'
