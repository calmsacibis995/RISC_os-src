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
# $Header: spell.sh,v 1.8.1.2 90/05/09 19:04:34 wje Exp $
#
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.
# #ident	"@(#)spell:spell.sh	1.8"


#
#	spell program
#

SPELL_LIB=/usr/lib/spell/	# Home of spell programs.

# B_SPELL - flags 
# F_SPELL - input files from arg list.
# D_SPELL - dictionary. 
# H_SPELL - history.
# S_SPELL - stop. (List of misspelled words.)
# V_SPELL - data for -v.
# I_SPELL - -i option to deroff.
# L_SPELL - sed script to strip troff commands.

B_SPELL=""
F_SPELL=""
V_SPELL=/dev/null

I_SPELL=""

#
# Remove troff commands.
# (But keep .so and .nx file-include requests.)
# ((But not if included file is in /usr/lib.))
#
L_SPELL="sed -e \"/^[.'].*[.'][ 	]*nx[ 	]*\/usr\/lib/d\" 
	     -e \"/^[.'].*[.'][ 	]*so[ 	]*\/usr\/lib/d\" 
	     -e \"/^[.'][ 	]*so[ 	]*\/usr\/lib/d\" 
	     -e \"/^[.'][ 	]*nx[ 	]*\/usr\/lib/d\" "

#
# Clean up.
#
trap "rm -f /tmp/spell.$$; exit" 0 1 2 13 15

#
# Parse arguments
#
for A in $*
do
    case $A in

    -v)	if /bin/pdp11
	then	
	    echo -v option not supported on pdp11 >&2
	    EXIT_SPELL=exit
	else	
	    B_SPELL="$B_SPELL -v"
	    V_SPELL=/tmp/spell.$$
	fi 
	;;

    -a)	: ;;

    -b) D_SPELL=${D_SPELL-/usr/lib/spell/hlistb}
	B_SPELL="$B_SPELL -b" 
	;;

    -x)	B_SPELL="$B_SPELL -x" ;;

    -l)	L_SPELL="cat" ;;

    +*)	if [ "$FIRSTPLUS" = "+" ]
	then	
	    echo "multiple + options in spell, all but the last are ignored" >&2
	fi;
	FIRSTPLUS="$FIRSTPLUS"+
	if  LOCAL=`expr $A : '+\(.*\)' 2>/dev/null`;
	then 
	    if test ! -r $LOCAL;
	    then 
		echo "spell cannot read $LOCAL" >&2;
		EXIT_SPELL=exit;
	    fi
	else 
	    echo "spell cannot identify local spell file" >&2;
	    EXIT_SPELL=exit;
	fi 
	;;

    -i)	I_SPELL="-i" ;;

    *)	F_SPELL="$F_SPELL $A" ;;

    esac
done

D_SPELL=${D_SPELL-/usr/lib/spell/hlista}
H_SPELL=${H_SPELL-/usr/lib/spell/spellhist}
S_SPELL=${S_SPELL-/usr/lib/spell/hstop}

#
# Quit if found an error during arg parsing.
#
${EXIT_SPELL-:}

# 
# make sure we can write to the history file.  If we 
# cannot, then send history info to /dev/null
#
if [ ! -w $H_SPELL ]
then
    H_SPELL="/dev/null"
fi

#
# Concatenate, clear troff text, cut to word size, sort ...
#
cat $F_SPELL |\
    eval $L_SPELL |\
    deroff -w $I_SPELL |\
    sort -u +0 |\
    $SPELL_LIB/spellprog $S_SPELL 1 |\
    $SPELL_LIB/spellprog $D_SPELL $V_SPELL $B_SPELL |\
    comm -23 - ${LOCAL-/dev/null} |\
    tee -a $H_SPELL

who am i >>$H_SPELL 2>/dev/null

case $V_SPELL in
/dev/null)	exit ;;
esac

sed '/^\./d' $V_SPELL | sort -u +1f +0
