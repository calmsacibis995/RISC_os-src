#!/bin/sh
#
# $Header: pkg_tapevol.sh,v 2.0.1.2.1.2 90/07/11 18:25:02 hawkes Exp $
#
# ---------------------------------------------------
# | Copyright (c) 1986 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#

if [ "$Pkg" = "" ]
  then
    echo "\$Pkg undefined"
    exit 1
  fi

. pkg_env


if [ "$1" != "" ]
then
  section "making tape volume header for $1"

  if file $Pkgroot/stand/sash.$1 | grep "not stripped" >/dev/null 2>&1
  then
    echo "--- Stripping $Pkgroot/stand/sash.$1"
    strip $Pkgroot/stand/sash.$1
  fi 
  if file $Pkgroot/stand/format.$1 | grep "not stripped" >/dev/null 2>&1
  then
    echo "--- Stripping $Pkgroot/stand/format.$1"
    strip $Pkgroot/stand/format.$1
  fi 

  echo "mkboottape -f $Pkg/lib/tapevol.$1 $Pkgroot/stand/sash.$1 $Pkgroot/stand/format.$1"
  mkboottape -f $Pkg/lib/tapevol.$1 $Pkgroot/stand/sash.$1 $Pkgroot/stand/format.$1
else
  section "making tape volume header for `pkginfo pkgname`"

  if file $Pkgroot/stand/sash | grep "not stripped" >/dev/null 2>&1
  then
    echo "--- Stripping $Pkgroot/stand/sash"
    strip $Pkgroot/stand/sash
  fi 
  if file $Pkgroot/stand/format | grep "not stripped" >/dev/null 2>&1
  then
    echo "--- Stripping $Pkgroot/stand/format"
    strip $Pkgroot/stand/format
  fi 

  echo "mkboottape -f $Pkg/lib/tapevol $Pkgroot/stand/sash $Pkgroot/stand/format"
  mkboottape -f $Pkg/lib/tapevol $Pkgroot/stand/sash $Pkgroot/stand/format
fi

echo ""
