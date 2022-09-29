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
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: pkg_cpinstd.sh,v 2.0.1.3.1.1.1.2 90/11/15 13:46:16 beacker Exp $
#
# Currently used by pkg_instd and pkg_miniroot
#

#
# This function is like /bin/cp, except that it first tries to find
# the source file under $Pkgroot. If none is found there, it uses /, and
# issues a warning.
#
pcp ()
{
  if [ -f $Pkgroot/$1 ]
    then
      echo "cp $Pkgroot/$1 $2" >$Verbose 2>&1
      cp $Pkgroot/$1 $2
    else
      if [ -f /$1 ]
      then
        echo "warning: using host version \"/$1\""
        echo "cp /$1 $2" >$Verbose 2>&1
        cp /$1 $2
      else
        echo " "
        echo "ERROR!  File $1 not found.  Can't copy."
        echo " "
        echo "  The instd file is NOT complete. "
        echo "  Please locate the missing file(s) and repackage instd"
        echo " "
        echo $Dn "Press [RETURN] after reading the error message above: $Sc"
        read Ans
        echo " "
        exit 1
      fi
    fi
}


#
# This function is just a cp with an echo to $Verbose, since we do
# it when copying files with pcp in pkg_miniroot and in ipcp for
# install tool files below.
#  
ecp()
{
  echo "cp $@" >$Verbose 2>&1
  cp $@
}
 
#
# This function is like /bin/cp, except that it first tries to find
# the source file under $Pkg/bin. If none is found there, it tries
# $Pkgroot/usr/pkg/bin. If the file is not found there, it uses
# /usr/pkg/bin, and issues a warning.
#
ipcp ()
{
  if [ -f $Pkg/bin/$1 ]
    then
      echo "cp $Pkg/bin/$1 $2" >$Verbose 2>&1
      cp $Pkg/bin/$1 $2
    else
      if [ -f $Pkgroot/usr/pkg/bin/$1 ]
	then
          echo "cp $Pkgroot/usr/pkg/bin/$1 $2" >$Verbose 2>&1
          cp $Pkgroot/usr/pkg/bin/$1 $2
	else
          if [ -f /usr/pkg/bin/$1 ]
	  then
            echo "warning: using host version \"/$1\""
            echo "cp /usr/pkg/bin/$1 $2" >$Verbose 2>&1
            cp /usr/pkg/bin/$1 $2
          else
            echo " "
            echo "ERROR!  File $1 not found.  Can't copy."
            echo " "
            echo "  The instd file is NOT complete. "
            echo "  Please locate the missing file(s) and repackage instd"
            echo " "
            echo $Dn "Press [RETURN] after reading the error message above: $Sc"
            read Ans
            echo " "
            exit 1
          fi
        fi
    fi
}


#
# This function adds it's argumeent to the reserve list, a .sizes file
# used to account for space for items that will occupy space on the installed
# system which are not accounted for in the boms. This is important for
# installs from the miniroot, wherein the packaging information will get
# copied to the real filesystem after the install is done
#
reserve()
{
  Fname=$1
  case $Hostsys in
        *BSD) Fsize=`ls -l $1 | awk ' { print $4 }` ;;
        *) Fsize=`ls -l $1 | awk ' { print $5 }` ;;
    esac
  echo "$Pkgpath/`basename $Fname` $Fsize" >>boms/reserve.sizes
}

#
# End of shell functions
#

#
# This variable is used to make the names for the reserve.size file.
# It makes the assumtion that $Pkg will be found in $Pkgroot/usr/pkg/lib
# on the target system. Since the inst_clnup stuff already makes this
# assumption, I don't see a problem with it. - rmg
#
Pkgpath="usr/pkg/lib/`basename $Pkg`"

mkdir bin
mkdir lib
mkdir boms
mkdir conv

#
# pkginfo
#
ecp $Pkg/pkginfo .

#
# Install tools (bin)
#

for file in \
  ask \
  cleannew \
  comply \
  convttys \
  dates \
  devstat \
  diskmap \
  extra \
  findmods \
  filtfstab \
  getmem \
  inst \
  inst_abort \
  inst_bsd_sysv \
  inst_clnup \
  inst_compl \
  inst_conv \
  inst_date \
  inst_env \
  inst_mkdev \
  inst_mkstuff \
  inst_mount \
  inst_newfs \
  inst_presv \
  inst_restr \
  inst_rmold \
  inst_sing \
  inst_space \
  inst_start \
  inst_stpln \
  inst_subsl \
  inst_swprq \
  inst_xtr \
  mkdevcmd \
  pkginfo \
  preserve \
  space \
  stripln
    do
      ipcp $file bin
    done

#
# These two files are a special case, and need to be included in $Pkg/bin
# because they cannot overwrite themselves when an update install is being
# performed, since a "text file busy" condition would result.
#
pcp bin/tar bin
pcp usr/ucb/rsh bin

rm -f boms/reserve.sizes
reserve pkginfo

#
# boms
#
for Subpkg in `pkginfo subpkgnames`
  do
    Bom=$Pkg/boms/`pkginfo bomname $Subpkg`
    ecp $Bom boms
    reserve boms/`basename $Bom`
    ecp $Bom.sizes boms
    reserve boms/`basename $Bom.sizes`
    ecp $Bom.total boms
    reserve boms/`basename $Bom.total`
  done

#
# lib
#
for Subpkg in `pkginfo subpkgnames`
  do
    if [ -f $Pkg/lib/${Subpkg}.preserves ]
      then
        ecp $Pkg/lib/${Subpkg}.preserves lib
        reserve lib/${Subpkg}.preserves
      fi
  done

#
# conv
#
for File in $Pkg/conv/*
  do
    if [ -f $File ]
      then
        ecp $File conv
	reserve conv/`basename $File`
      fi
  done

#
# something like xtrdir_insert goes here someday if we need it
#
