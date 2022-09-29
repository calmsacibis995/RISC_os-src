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
# $Header: pkg_sizes.sh,v 2.0.1.2 90/05/10 04:01:32 wje Exp $
#
if [ "$Pkg" = "" ]
  then
    echo "\$Pkg undefined"
    exit 1
  fi

. pkg_env

section "making size files for `pkginfo pkgname`"

cd $Pkgroot

for Subpkg in `pkginfo subpkgnames`
  do
    Bom=$Pkg/boms/`pkginfo bomname $Subpkg`
    Sizes=$Bom.sizes
    Tsizes=$Bom.total
    echo $Dn "subpackage $Subpkg... $Sc"
    echo "" >$Verbose
    mksizes 2>$Tsizes $Bom | tee $Verbose >$Sizes
    cat $Tsizes
  done

echo ""
