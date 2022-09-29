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
# $Header: inst_compl.sh,v 1.3.2.2 90/05/10 03:50:00 wje Exp $
#
case "$Instenv" in
  "") . inst_env ;;
  esac

section "running comply"

Complyout=${Complyout=$Pkg/lib/comply.out}

Boms=""

#
#  need 0 umask so comply will create directories with the permissions
#  specified in the boms - should be done in comply in a future release
#
umask 0

for Subpkg in $Subpkgs
  do
    Boms="$Boms $Pkg/boms/`pkginfo bomname $Subpkg`"
  done

cd $Pkgroot

echo $Dn "running first comply pass... $Sc"

comply -f $Boms >/dev/null 2>&1
sync

echo $Dn "\nrunning second comply pass... $Sc"

comply -f $Boms >$Complyout 2>&1

case $? in
  0) echo "\nThere were no comply messages from the second pass." ;;
  *) echo "\nThere are comply messages from the second pass in:
   $Complyout"
     case "$Onmini" in
       y) echo "(A copy will be placed on the installed system as:)
   /usr/pkg/lib$Complyout"
       esac
     ask "
Normally, there should be no second-pass comply messages. The presence
of these messages may indicate problems with the installation.

View the messages now" y y n
     case $Ans in
       y) Pager=""
	  if [ -x /bin/more ]
	    then
	      Pager=/bin/more
	    else if [ -x /usr/ucb/more ]
		   then
		     Pager=/usr/ucb/more
		   else if [ -x /mnt/bin/more ]
			  then
			    Pager=/mnt/bin/more
			  else if [ -x /mnt/usr/ucb/more ]
			         then
				   Pager=/mnt/usr/ucb/more
				 fi
			  fi
		   fi
	    fi
	  if [ "$Pager" = "" ]
	    then
	      Pager=/bin/cat
	      echo $Dn "
I can't find a pager for you, so be ready to use control-s/control-q in
case there are more than a screen's worth of comply messages.

(Press return to see comply messages...) $Sc"
	      read Ans
	    fi
	  $Pager $Complyout
	  echo $Dn "
(Press return to continue...) $Sc"
	  read Ans
	  ;;
       n) echo $Dn "
Please be sure to examine the comply messages after the installation
has completed to determine the exact nature of the problem.

(Press return to continue...) $Sc"
	  read Ans
	  ;;
       esac
     ;;
  esac

  





