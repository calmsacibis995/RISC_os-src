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
# $Header: ypxfr_2perday.sh,v 1.1.1.2 90/05/09 19:31:24 wje Exp $
#
#    @(#)ypxfr_2perday.sh	1.1 88/03/07 4.0NFSSRC SMI
#
# ypxfr_2perday.sh - Do twice-daily yp map check/updates
#

PATH=/bin:/usr/bin:/usr/etc:/usr/etc/yp:$PATH
export PATH

# set -xv
ypxfr hosts.byname
ypxfr hosts.byaddr
ypxfr ethers.byaddr
ypxfr ethers.byname
ypxfr netgroup
ypxfr netgroup.byuser
ypxfr netgroup.byhost
ypxfr mail.aliases 
