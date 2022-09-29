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
# $Header: pkg_env.sh,v 2.0.1.3 90/05/10 04:00:18 wje Exp $
#
# pkg_env
#
# This file must be "sourced" rather than run in a subshell
# 
# It sets up values for the environment variables required by the
# packaging tools.  An attempt is made to provide correct defaults
# generically for all packages. In most cases, existing values in the
# environment are taken as overrides of the defaults provided herein.
#

set -a

if [ "$Pkgshflags" != "" ]
  then
    set $Pkgshflags
  fi

if [ "$Pkgroot" = "" ]
  then
    echo "\$Pkgroot undefined"
    exit 1
  else
    #
    # Insure ablsolute pathnames
    #
    Curdir=`/bin/pwd`
    cd $Pkgroot; Pkgroot=`/bin/pwd`
    cd $Pkg; Pkg=`/bin/pwd`
    cd $Curdir
  fi

#
# Define/Pick-up shell functions
#

. ask

section ()
{
  echo "
========== $1 ==========
"
}

#
# Use the commands and packaging/installation tools from the release if
# they exist. Directories should be added to Pkgpath and Stdpath in pairs.
#
Pkgpath=$Pkg/bin:$Pkgroot/bin:$Pkgroot/usr/bin:$Pkgroot/etc:$Pkgroot/usr/ucb
Stdpath=/bin:/usr/bin:/etc:/usr/ucb
PATH=$Pkgpath:$Stdpath:$PATH

#
# Hostsys will always tell us whether we're working on a BSD or a V
# system.
#
Hostsys=`uname -v`

#
# Things for newlineless echo. Ugh.
#
case $Hostsys in
      *BSD) Dn="-n"; Sc="";;
      *) Dn=""; Sc="\c";;
  esac

#
# Verbosity control. Usually set to /dev/tty for verbososity.
#
Verbose=${Verbose=/dev/null}

#
# Tmp directory for packaging tools
#
Tmp=${Tmp=/usr/tmp}

#
# NOTE: took this out as we now set media to by doing a pkginfo media 
#	query. let all who need it do that instead of depending on this.
#	The reason we did it was because you had to set the To environment
#	variable if you didn't want Q24 even though you speced the media
#	in the pkginfo file.
#
# Media/format selection for tape making tools.
# $To selects format.
#
#case $Hostsys in
#      *BSD) To=${To=pt}
#	    Taperw=${Taperw="/dev/r${To}8"}
#	    Tapenrw=${Tapenrw="/dev/r${To}12"} ;;
#      *) To=${To=Q24}
#	    Taperw=${Taperw="/dev/rmt/${To}-0"}
#	    Tapenrw=${Tapenrw="/dev/rmt/${To}n-0"} ;;
#  esac

#
# Note: this was only in here for the bizzare reason that pkg_miniroot
# checked what kind of controller was in the system being packaged on,
# then conditionallly made an fstab with the right /dev/dsk/{ipc,int}0s0d1
# name for that type. I've changed pkg_miniroot to just make an fstab
# entry for /dev/swap, which should work generically, but I'm going to leave
# this in (commented out) until we know /dev/swap works OK.
#
#
# What kind of disk controller is drive 0?
#
# reserved values:
#   ip3200	interphase 3200
#   ip4200	interphase 4200
#   in300	introl 300
#
#Diskcont=${Diskcont=ip3200}
