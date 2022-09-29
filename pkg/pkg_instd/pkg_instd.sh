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
# $Header: pkg_instd.sh,v 2.0.1.2 90/05/10 04:01:01 wje Exp $
#
if [ "$Pkg" = "" ]
  then
    echo "\$Pkg undefined"
    exit 1
  fi

. pkg_env

section "creating installation directory archive for `pkginfo pkgname`"

Instdtmp=${Instdtmp=$Tmp/instd.$$}
Packagename=`pkginfo pkgname`
Pkgbase=$Packagename`pkginfo version $Packagename`

trap "echo \"\n***Interrupt; cleaning up...\"; \
        rm -rf $Instdtmp ; exit 1" 1 2 3 15

mkdir $Instdtmp
mkdir $Instdtmp/$Pkgbase
cd $Instdtmp/$Pkgbase

echo $Dn "copying files into $Instdtmp... $Sc"
echo "" >$Verbose
#
# pkg_cpinstd does the work; it's a seperate script since pkg_miniroot
# must do the same thing, and we'd prefer to only update it in one
# place.
#
. pkg_cpinstd

echo""

#
# Make the archive
#
cd $Instdtmp

echo $Dn "writing $Pkg/lib/instd.ls... $Sc"
ls -lRFia >$Pkg/lib/instd.ls
echo ""

echo $Dn "archiving the installation directory... $Sc"
echo "" >$Verbose
tar cvf $Pkg/lib/instd $Pkgbase >$Verbose
echo ""

#
# Clean up
#

echo $Dn "cleaning up... $Sc"
cd $Pkg
rm -rf $Instdtmp
echo "\n"
