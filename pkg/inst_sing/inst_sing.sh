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
# $Header: inst_sing.sh,v 1.2.2.3 90/05/10 03:54:29 wje Exp $
#
case "$Instenv" in
  "") . inst_env ;;
  esac

section "verifying single-user mode"

case "$Hostsys" in
      *BSD) if ps -gax | awk '{ print $5 }' | grep /etc/update >/dev/null
	      then
	        ask "\
This system is not presently in a single-user run level.  Installation
of a package can fail if performed in multi-user mode.  We recommend
that the system be brought to a single user run level (using 
\"/etc/shutdown\") prior to performing the installation.

Are you absolutely sure you wish to continue" n y n
                case "$Ans" in
	          n) exit 1 ;;
	          esac
	      else
	        echo "The system is in single-user mode."
	      fi
            ;;
         *) if [ -f /.miniroot ]
              then
	        set one two 1
              else
	        set `who -r`
	      fi
	
	    case $3 in
	      s|S|1) echo "The system is in a single-user run level." ;;
	          *) ask "\
This system is not presently in a single-user run level.  Installation
of a package can fail if performed at this run level.  We recommend
that the system be brought to a single user run level (using \"init
S\") prior to performing the installation.

Are you absolutely sure you wish to continue" n y n
		     case "$Ans" in
		       n) exit 1 ;;
		       esac ;;
	      esac ;;
  esac





