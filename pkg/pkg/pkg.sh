#!/bin/sh
#
# $Header: pkg.sh,v 2.1.1.7.1.2 90/07/11 18:24:32 hawkes Exp $
#
# ---------------------------------------------------
# | Copyright (c) 1986 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#
#set -x
if [ "$Pkg" = "" ]
  then
    echo "\$Pkg undefined"
    exit 1
  fi

#
# This is the path we will use when trying to find pkg_env. When pkg_env
# is sourced, it will set up the rest of the path to other commands.
#
PATH=$Pkgroot/crossenv/usr/pkg/bin:$Pkg/bin:$Pkgroot/usr/pkg/bin:/usr/pkg/bin

. pkg_env

newer()
{
  F1="$1"
  F2="$2"
 
  if [ ! -f $F1 ]
    then
      echo "**** ERROR - split bom file: $F1 does not exist."
      return 0
    fi

  if [ ! -f $F2 ]
    then
      return 0
    fi

  set `dates -f $F1`
  T1=$2
  set `dates -f $F2`
  T2=$2
  
  if [ $T1 -gt $T2 ]
    then
      return 0
    else
      return 1
    fi
}

section "packaging `pkginfo pkgname`"

echo $Dn "\
Pkgroot = $Pkgroot
Pkg     = $Pkg

This command is a front-end for the several pkg_* commands used in the
course of packaging MIPS software.

You will be presented with the opportunity to execute each command, in
the proper order. For steps in which the dependencies are simple and
easy to check, dependency checking is done, and if it is thereby
determined that a step need not be performed, you will be so informed.

(Press return to continue...) $Sc"
read Ans
echo ""

#
# See if we need to do pkg_boms
#

section "pkg_boms"

Doboms=n

cd $Pkg/boms

echo "pkg_boms should be executed whenever the contents of one of the
split boms has changed.
"

for Subpkg in `pkginfo subpkgnames`
  do
    Bom="$Pkg/boms/`pkginfo bomname $Subpkg`"
    Sboms=`pkginfo splitboms $Subpkg`
    for Sbom in $Sboms
      do
	if newer $Sbom $Bom
	  then
	    Doboms=y
	    echo "`basename $Sbom` is newer than `basename $Bom`"
	  fi
      done
  done

case $Doboms in
  n) echo "All split bom files are older than the merged bom files.\n"
     ;;
  y) echo ""
     ;;
  esac

Ans=""
while [ "$Ans" != "y" -a "$Ans" != "n" ]
  do
    if [ "$Doboms" = "y" ]
      then
        echo \
  "Based on dependency checking, it is suggested the pkg_boms be run"
      else
	echo \
  "Based on dependency checking it appears that pkg_boms need not be run"  
      fi
    echo $Dn "Execute pkg_boms? (y n) $Sc"
    read Ans
  done

case $Ans in
  y) pkg_boms
     ## We now run pkg_sizes AFTER pkg_comply
     ## pkg_sizes 
     ;;
  esac

#
# See if we need to do pkg_comply
#

section "pkg_comply"

Complyout=${Complyout=$Pkg/lib/comply.out}
Docomply=n

echo "\
pkg_comply should be re-run whenever one of the boms has been changed,
or when files in the release tree have changed.
"

for Subpkg in `pkginfo subpkgnames`
  do
    Bom=`pkginfo bomname $Subpkg`
    if newer $Bom $Complyout
      then
	Docomply=y
	echo "`basename $Bom` is newer than `basename $Complyout`"
      fi
  done

case $Docomply in
  n) echo "All bom files are older than the existing comply.out file.\n" ;;
  y) echo "" ;;
  esac

Ans=""
while [ "$Ans" != "y" -a "$Ans" != "n" ]
  do
    if [ "$Doboms" = "y" -o "$Docomply" = "y" ]
      then
        echo \
  "Based on dependency checking, it is suggested the pkg_comply be run"
      else
	echo \
  "Based on dependency checking it appears that pkg_comply need not be run"
      fi
    echo $Dn "Execute pkg_comply? (y n) $Sc"
    read Ans
  done

case $Ans in
  y) pkg_comply
     pkg_sizes 
     ;;
  esac

#
# See if we need to do pkg_tapevol
#

if `pkginfo os`
  then
    section "pkg_tapevol"
    echo "\
pkg_tapevol should be executed whenever format or sash have changed.
Since it is so quick to do this, it is recommended that this always be
done.
"
    Ans=""
    while [ "$Ans" != "y" -a "$Ans" != "n" ]
      do
        echo $Dn "Execute pkg_tapevol? (y n) $Sc"
        read Ans
      done

    case $Ans in
      y) if [ "$KernelOnMiniroot" != "y" ]
         then
           pkg_tapevol std
           pkg_tapevol 2030
           echo " "
           echo "Making tape bootable image of unix.r2300_std... "
           mkboottape -f $Pkgroot/unix.r2300_std.boot $Pkgroot/unix.r2300_std
           echo "Making tape bootable image of unix.r2400_std... "
           mkboottape -f $Pkgroot/unix.r2400_std.boot $Pkgroot/unix.r2400_std
           echo "Making tape bootable image of unix.r3200_std... "
           mkboottape -f $Pkgroot/unix.r3200_std.boot $Pkgroot/unix.r3200_std
           echo "Making tape bootable image of unix.r3200_ijc... "
           mkboottape -f $Pkgroot/unix.r3200_ijc.boot $Pkgroot/unix.r3200_ijc
           echo "Making tape bootable image of unix.i2000_std... "
           mkboottape -f $Pkgroot/unix.i2000_std.boot $Pkgroot/unix.i2000_std
           echo "Making tape bootable image of unix.r6000_std... "
           mkboottape -f $Pkgroot/unix.r6000_std.boot $Pkgroot/unix.r6000_std
           echo "Making tape bootable image of unix.r3030_std... "
           mkboottape -f $Pkgroot/unix.r3030_std.boot $Pkgroot/unix.r3030_std
           echo "Making tape bootable image of unix.rb3125_std... "
           mkboottape -f $Pkgroot/unix.rb3125_std.boot $Pkgroot/unix.rb3125_std
           echo "Done. "
           echo " "
         else
           pkg_tapevol 
         fi ;;
      esac
  fi


#
# See if we need to do pkg_instd
#

section "pkg_instd"

echo "\
pkg_instd should be executed whenever any of the installation tools or
packaging information have changed. This includes the pkginfo file,
bomfile, sizefiles, and the installation scripts themselves. If you
have any doubt about whether this is necessary, you should execute
pkg_instd. It doesn't take very long.
"

Ans=""
while [ "$Ans" != "y" -a "$Ans" != "n" ]
  do
    echo $Dn "Execute pkg_instd? (y n) $Sc"
    read Ans
  done

case $Ans in
  y) pkg_instd ;;
  esac

#
# See if we need to do pkg_miniroot
#

if `pkginfo os`
  then
    section "pkg_miniroot"
    echo "\
pkg_miniroot should be executed whenever any of the packaging
information or tools (as for pkg_instd, above) have changed, or when
any of the binaries which will be included on the miniroot have
changed. It is a relatively time consuming step (three to five
minutes), but it is advisable to perform it if there is any doubt.
"

    Ans=""
    while [ "$Ans" != "y" -a "$Ans" != "n" ]
      do
        echo $Dn "Execute pkg_miniroot? (y n) $Sc"
        read Ans
      done

    case $Ans in
      y) pkg_miniroot
      esac
  fi


#
# See if we need to do pkg_tapes
#

section "pkg_tapes"

Ans=""
while [ "$Ans" != "y" -a "$Ans" != "n" ]
  do
    echo $Dn "Execute pkg_tapes? (y n) $Sc"
    read Ans
  done

case $Ans in
  y) pkg_tapes ;;
  esac

#
# That's all, folks!
#

section "packaging complete"
