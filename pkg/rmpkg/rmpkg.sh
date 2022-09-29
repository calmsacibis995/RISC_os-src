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
# $Header: rmpkg.sh,v 1.2.2.2 90/05/10 04:04:28 wje Exp $
#
set -a

Pkgroot=${Pkgroot=""}

PATH=/bin:/usr/bin:/usr/pkg/bin

. ask

if [ $# -ne 1 ]
  then
    echo "$0: usage: rmpkg <pkgname><ver>"
    exit 1
  fi

if [ ! -d $Pkgroot/usr/pkg/lib/$1 ]
  then
    echo "$0: directory \"$Pkgroot/usr/pkg/lib/$1\" not found"
    exit 1
  fi

Pkg=$Pkgroot/usr/pkg/lib/$1

if [ ! -f $Pkg/pkginfo ]
  then
    echo "$0: can't find pkginfo file \"$Pkg/pkginfo\""
    exit 1
  fi

Pkgname=`pkginfo pkgname`
Version=`pkginfo version $Pkgname`
Subpkgs=`pkginfo subpkgnames`


if [ "$Pkgroot" = "" ]
  then
    Pkgrootprint=/
  else
    Pkgrootprint=$Pkgroot
  fi

echo "\nPkgroot=$Pkgrootprint, Pkg=$Pkg (package $Pkgname $Version)"

Ok=n

while [ $Ok != "y" ]
  do
    Rmsubpkgs=""
    echo ""
    for Subpkg in $Subpkgs
      do
        ask "Remove $Pkgname.$Subpkg `pkginfo version $Subpkg`" n y n
        case $Ans in
          y) Rmsubpkgs="$Rmsubpkgs $Subpkg" ;;
          esac
      done
 
    echo "\nThe following subpackages have been selected for removal:"
    for Subpkg in $Rmsubpkgs
      do
	echo "  $Subpkg"
      done

    ask "\nOk" y y n
    Ok=$Ans
  done

echo ""

cd $Pkgrootprint

export Subpkg
for Subpkg in $Rmsubpkgs
  do
    echo "removing $Pkgname.$Subpkg `pkginfo version $Subpkg`..."
    Bom=`pkginfo bomname $Subpkg`
    $Dbgecho stripln -t `pkginfo timestamp` $Pkg/boms/$Bom
    rm $Pkg/boms/$Bom
  done

Allbomsrmed=y

export Subpkg
for Subpkg in `pkginfo subpkgnames`
  do
    if [ -f $Pkg/boms/`pkginfo bomname $Subpkg` ]
      then
	Allbomsrmed=n
      fi
  done

if [ "$Allbomsrmed" = "y" ]
  then
    ask "\
All of the boms for subpackages in this package have been removed. Remove
any files remaining in the packaging informatin tree" y y n
    if [ "$Ans" = "y" ]
      then
        rm -rf $Pkg
      fi    
  else
    echo "\
Some of the boms for subpackages in this package remain in the packaging
information tree, so the packaging information tree will not be removed."
  fi
    
