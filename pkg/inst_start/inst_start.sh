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
# $Header: inst_start.sh,v 2.2.1.4 90/05/28 17:58:13 wje Exp $
#
#   usage: inst
#
#   various switches are taken from the environment
#

set -a

if [ "$Pkg" = "" ]
  then
    echo "\$Pkg undefined"
    exit 1
  fi

#
# This is the path we will use when trying to find pkg_env. When pkg_env
# is sourced, it will set up the rest of the path to other commands.
#
PATH=$Pkg/bin:$Pkgroot/usr/pkg/bin:/usr/pkg/bin

. inst_env

trap ". inst_abort" 1 2 3 15
trap ". inst_clnup" 0

#  Remove the extra unix images to make space
#  touch something to avoid error message
##
## There's no reason to do this because unix and unix.r* are hard linked.
## That means that we don't save any space by deleting the unix.r* file.
##
##touch /unix.rXXX
##rm /unix.r*

#
#  Now source sub-scripts to do things based on the environment we've
#  set up. They communicate with each other via the environment, so
#  they are sourced rather than run in subshells.
#
. inst_subsl	#  subpackage selection
. inst_date	#  set up system clock
. inst_sing	#  insist on single-user mode for installs
. inst_newfs	#  make new file systems for scratch os installs
. inst_mount	#  mount filesystems
. inst_swprq 	#  check swap prerequisites
if [ "$inst1by1" != "y" ]
then
. inst_presv 	#  run preserve -s
. inst_space 	#  check disk space availablility
. inst_stpln 	#  run stripln
. inst_xtr	#  extract the archives
. inst_mkdev	#  make device files
. inst_compl  	#  run comply
. inst_rmold 	#  remove previous version of software
. inst_restr	#  run preserve -r
else
TempSubpkgs1=$Subpkgs
section "one by one package installation selected"
#inst_xtr calls the other inst scripts for 1 by 1 install
. inst_xtr	#  extract the archives
Subpkgs=$TempSubpkgs1
export Subpkgs
fi
. inst_conv	#  run conversion scripts
. inst_mkstuff	#  make some assundry directories and files
. inst_bsd_sysv	#  convert BSD files to SYSV files
. inst_clnup    #  clean up left over stuff from the install

section "installation complete"

exit 0
