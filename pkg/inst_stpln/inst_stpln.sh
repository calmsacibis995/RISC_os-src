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
# $Header: inst_stpln.sh,v 1.1.2.2 90/05/10 03:55:07 wje Exp $
#
# Run stripsym to remove any pathnames in the package being installed
# that are presently symlinks on the target system.  This prevents
# problems with files that were symlinks in a previous version of the
# package, but which have become something else in the current version
#

case "$Instenv" in
  "") . inst_env ;;
  esac

if [ "$Install" = "update" ]
  then

     section "stripping old links"

     cd $Pkgroot

     for Subpkg in $Subpkgs
       do
         echo $Dn "Stripping links for subpackage $Subpkg... $Sc"
         echo "" >$Verbose
         $Dbgecho stripln $Pkg/boms/`pkginfo bomname $Subpkg` >$Verbose
         echo ""
       done

  fi

 
