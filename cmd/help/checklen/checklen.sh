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
# $Header: checklen.sh,v 1.5.2.2 90/05/09 16:09:09 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

# #ident	"@(#)help:checklen.sh	1.3"

while [ "${LEN}" = "bad" ]
do
	${EDITOR} ${EDFILE}
	if [ `wc -l <${EDFILE}` -lt 1 ]
	then
		echo "This help screen may not be empty.  You are being put back into the editor to   enter some data."
	elif [ `wc -l <${EDFILE}` -le ${SCRNLEN} ]
	then
		ANS=~
		while [ \( "${ANS}" != "y" \) -a \( "${ANS}" != "n" \) ]
		do
			echo "Are you satisfied with this screen (y or n)? > \c"
			read ANS
		done
		if [ "${ANS}" = "y" ]
		then
			LEN=good
		else
			echo "You may now re-edit this help screen."
		fi
	else
		CHOICE=~
		while [ \( "${CHOICE}" != "r" \) -a \( "${CHOICE}" != "e" \) ]
		do
			echo "This screen is more than ${SCRNLEN} lines long.  Do you want to"
			echo "re-edit it, or have it expanded to a multi-page screen?"
			echo "Enter choice (r for re-edit, e for expand) > \c"
			read CHOICE
		done
		if [ "${CHOICE}" = "e" ]
		then
			LEN=good
		else
			echo "You may now re-edit this help screen."
		fi
	fi
done
