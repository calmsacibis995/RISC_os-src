#! /bin/csh -f
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
# $Header: vgrind.sh,v 1.1.1.2 90/05/07 19:48:42 wje Exp $
#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)vgrind.sh	5.3 (Berkeley) 11/13/85
#
# vgrind
#
set b=/bsd43/usr/lib
set m=/usr/lib
set voptions=
set options=
set files=
set f=''
set head=""
top:
if ($#argv > 0) then
    switch ($1:q)

    case -f:
	set f='filter'
	set options = "$options $1:q"
	shift
	goto top

    case -t:
	set voptions = "$voptions -t"
	shift
	goto top

    case -o*:
	set voptions="$voptions $1:q"
	shift
	goto top

#   case -W:
#	set voptions = "$voptions -W"
#	shift
#	goto top

    case -d:
	if ($#argv < 2) then
	    echo "vgrind: $1:q option must have argument"
	    goto done
	else
	    set options = ($options $1:q $2)
	    shift
	    shift
	    goto top
	endif
			
    case -h:
	if ($#argv < 2) then
	    echo "vgrind: $1:q option must have argument"
	    goto done
	else
	    set head="$2"
	    shift
	    shift
	    goto top
	endif
			
    case -*:
	set options = "$options $1:q"
	shift
	goto top

    default:
	set files = "$files $1:q"
	shift
	goto top
    endsw
endif
if (-r index) then
    echo > nindex
    foreach i ($files)
	#	make up a sed delete command for filenames
	#	being careful about slashes.
	echo "? $i ?d" | sed -e "s:/:\\/:g" -e "s:?:/:g" >> nindex
    end
    sed -f nindex index >xindex
    if ($f == 'filter') then
	if ("$head" != "") then
	    $b/vfontedpr $options -h "$head" $files | cat $m/tmac/tmac.vgrind -
	else
	    $b/vfontedpr $options $files | cat $m/tmac/tmac.vgrind -
	endif
    else
	if ("$head" != "") then
	    $b/vfontedpr $options -h "$head" $files | \
		/bin/sh -c "itroff -rx1 $voptions -i -mvgrind 2>> xindex"
	else
	    $b/vfontedpr $options $files | \
		/bin/sh -c "itroff -rx1 $voptions -i -mvgrind 2>> xindex"
	endif
    endif
    sort -df +0 -2 xindex >index
    rm nindex xindex
else
    if ($f == 'filter') then
	if ("$head" != "") then
	    $b/vfontedpr $options -h "$head" $files | cat $m/tmac/tmac.vgrind -
	else
	    $b/vfontedpr $options $files | cat $m/tmac/tmac.vgrind -
	endif
    else
	if ("$head" != "") then
	    $b/vfontedpr $options -h "$head" $files \
		| itroff -i $voptions -mvgrind
	else
	    $b/vfontedpr $options $files \
		| itroff -i $voptions -mvgrind
	endif
    endif
endif

done:
