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
# $Header: inst_bsd_sysv.sh,v 1.3.2.2 90/05/10 03:49:36 wje Exp $
#
trap "" 0
trap "" 1 2 3 15

case "$Instenv" in
  "") . inst_env ;;
esac

# check if we have an inittab file - if present we must be SYS V already
if [ -f /mnt/etc/inittab ]
then
	echo ""
else
# no inittab file and a ttys file means BSD
    if [ -f /mnt/etc/ttys ]
    then
    # we have a ttys file so convert to inittab and ttytype files
	section "converting BSD ttys to SYS V inittab"
	echo ""
	echo "Creating /etc/inittab and /etc/ttytype from /etc/ttys..."
	cat >/mnt/etc/inittab.conf <<===EOF===
#
# The following lines from your old /etc/ttys file were converted and added
# Be sure to remove any duplications that are found in the above lines
#
===EOF===
    convttys < /mnt/etc/ttys >> /mnt/etc/inittab.conf 2>> /mnt/etc/ttytype.conf
    echo ""
    fi
fi

# check if we have an old style BSD fstab
grep " 4\.3 " /mnt/etc/fstab > /dev/null 2> /dev/null
OLDSTYLE=$?

if [ `expr $OLDSTYLE` -eq 0 ]
then
# we have the old type so convert it then
	section "converting BSD fstab to SYS V fstab"
	touch /etc/fstab.conf
	sed -e 's/ 4\.3 / ffs /g' < /mnt/etc/fstab | filtfstab >> /etc/fstab.conf
fi

