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
# $Header: ask.sh,v 1.2.2.2 90/05/10 03:43:00 wje Exp $
#
# This function provides a standard way of prompting the user for
# a choice, either from a fixed set of valid responses or not.
#

ask ()
{

if [ $# -lt 2 ]
  then
    echo "$0: usage: $0 prompt default choice ..."
    exit 1
  fi

case $# in

  2) Prompt=$1
     Default=$2
     echo $Dn "$Prompt [$Default]? $Sc"
     set +x
     read Ans </dev/tty
     case "$Ans" in
       "") Ans=$Default ;;
       esac
     ;;

  *) Prompt=$1
     shift
     Default=$1
     shift
     Choices=$*
     
     Asking=Y
     
     while [ "$Asking" = "Y" ]
       do
     
         echo $Dn "$Prompt ($Choices) [$Default]? $Sc"
         set +x
         read Ans </dev/tty
     
         case $Ans in
           "") Ans=$Default ;;
           esac
         
         Ans=`echo $Ans | tr [A-Z] [a-z]`
         
         for C in $*
           do
     	     if [ "$Ans" = "$C" ]
               then
     	         Asking=N
     	       fi
           done
         
         if [ "$Asking" = "Y" ]
           then
     	     echo $Dn "What? $Sc"
           fi
     
       done
     
     ;;
  esac           
     
}
