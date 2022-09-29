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
# $Header: pkg_boms.sh,v 2.0.1.4 90/05/10 03:59:35 wje Exp $
#
if [ "$Pkg" = "" ]
  then
    echo "\$Pkg undefined"
    exit 1
  fi

. pkg_env

section "merging boms for `pkginfo pkgname`"

Exit=0

cd $Pkg/boms

for Subpkg in `pkginfo subpkgnames`
  do
    echo $Dn "subpackage $Subpkg... $Sc"
    Bom=$Pkg/boms/`pkginfo bomname $Subpkg`

    split_boms="`pkginfo splitboms $Subpkg`"
    if [ "${split_boms}" = "" ]
    then
      echo "Error.  No split boms specified for subpkg: $Subpkg"
      echo "Edit the pkginfo file and try again."
      exit 1
    else
      bomerge ${split_boms} >$Bom
    fi
    if [ "$?" = "0" ]
      then
        ## get rid of any blank lines
        grep \. $Bom > $Bom.$$
        mv $Bom.$$ $Bom
        echo "merged OK"
      else
	Exit=1
        echo "merge errors, messages in \$Pkg/boms/`basename $Bom`"
      fi
  done

echo ""

exit $Exit
