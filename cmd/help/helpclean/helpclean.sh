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
# $Header: helpclean.sh,v 1.5.2.2 90/05/09 16:12:06 wje Exp $
# #ident	"@(#)help:helpclean	1.4"
ROOT=
HELP="${ROOT}/usr/lib/help"

rm -f ${HELP}/oHELPLOG
cp ${HELP}/HELPLOG ${HELP}/oHELPLOG
${HELP}/HELPLOG
