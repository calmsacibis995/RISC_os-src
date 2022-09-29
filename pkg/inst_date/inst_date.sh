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
# $Header: inst_date.sh,v 1.2.2.3 90/05/10 03:51:01 wje Exp $
#
case "$Instenv" in
  "") . inst_env ;;
  esac

section "setting system clock/calendar"

#
# format for setting date varies between BSD and V
#
case "$Hostsys" in
      *BSD) Datestring="\"yymmddhhmm[.ss]\"" ;;
      *) Datestring="\"mmddhhmm[yy]\"" ;;
  esac

Ans=n
while [ "$Ans" != "y" ]
  do
    echo "The current value of the clock is: `date`"
    ask "Is the clock correct" y y n
    case "$Ans" in
      y) ;;
      *) echo $Dn "Enter the correct time and date as ${Datestring}: $Sc"
	 read Date
	 date $Date 2>$Verbose
	 ;;
      esac
  done
