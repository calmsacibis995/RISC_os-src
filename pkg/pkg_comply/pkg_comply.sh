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
# $Header: pkg_comply.sh,v 2.0.1.5 90/05/10 03:59:48 wje Exp $
#
if [ "$Pkg" = "" ]
  then
    echo "\$Pkg undefined"
    exit 1
  fi

#
# This is the path we will use when trying to find pkg_env. When pkg_env
# is sourced, it will set up the rest of the path to other commands.
#

. pkg_env

section "running standard two-pass comply for `pkginfo pkgname`"

Complyout=$Pkg/lib/comply.out
rm -f $Complyout

Boms=""

echo "Boms are:\n" | tee -a $Complyout


for Subpkg in `pkginfo subpkgnames`
  do
    Bom="$Pkg/boms/`pkginfo bomname $Subpkg`"
    echo "  $Bom" | tee -a $Complyout
    Boms="$Boms $Bom"
  done

cd $Pkgroot

#
# Apologies for the $Complyout.pass1 rigamarole below; It's necessary
# because 1) We want to see comply messages as they happen, so the
# tee to /dev/tty is necessary- therefore we can't directly test the
# comply exit status and 2) Need to determine whether there were any
# comply messages, so we can avoid pass2 if not.
#

echo "\nPass one:\n" 2>&1 | tee -a $Complyout
$Dbgecho comply -x -f -t `pkginfo timestamp` $Boms > ${Complyout}.pass1 2>&1

cat $Complyout $Complyout.pass1 >$Complyout.tmp
mv $Complyout.tmp $Complyout

if [ -s $Complyout.pass1 ]
  then
    echo "\nPass two:\n" 2>&1 | tee -a $Complyout
    $Dbgecho comply -x $Boms 2>&1 | tee -a $Complyout
  else
    echo "\nPass two:\n
Since there were no messages from the first pass, the second pass
will not be performed." | tee -a $Complyout
  fi

rm ${Complyout}.pass1



