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
# $Header: netfeed.sh,v 1.1.2.2 90/05/10 03:58:36 wje Exp $
#
set -a

Pkg="$1"
Subpkg="$2"
Pkgroot="$3"

#
# This script is used to feed an archive across the net from a server
# to a machine installing over the net. Usage is (via rsh):
#
#   netfeed <pkg> <subpkgname> <pkgroot>
#
# where <pkg> is the "packaging information tree" tree for the desired
# package (see pkgins man page), and <subpkgname> is the name of the
# subpackage to be served. <pkgroot> gives the location of the release tree
# to use when creating the archive.
#

PATH=$Pkg/bin:/usr/pkg/bin:/bin:/usr/bin

cd $Pkgroot
$Dbgecho cat $Pkg/boms/`pkginfo bomname $Subpkg` | $Dbgecho bom_to_tar | \
  $Dbgecho tar cnf - -

