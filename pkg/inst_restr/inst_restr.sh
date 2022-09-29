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
# $Header: inst_restr.sh,v 1.1.2.2 90/05/10 03:53:30 wje Exp $
#
case "$Instenv" in
  "") . inst_env ;;
  esac

dorestore()
{

  Preslog=${Preslog=$Pkg/lib/preserve.log}
  Instlog=${Instlog=$Pkgroot/etc/installlog}
  
  cd $Pkgroot
  
  for Subpkg in $Subpkgs
    do

      Findmodsout=$Pkg/lib/${Subpkg}.findmods
      Preserves=$Pkg/lib/${Subpkg}.preserves
      Pkgsubpkg=`pkginfo pkgname`.$Subpkg
      Subpkgver=`pkginfo version $Subpkg`
      Presfiles="";

      if [ -f $Findmodsout ]
	then
	  Presfiles="$Presfiles $Findmodsout"
	fi

      if [ -f $Preserves ]
	then
	  Presfiles="$Presfiles $Preserves"
	fi
      
      case $Presfiles in
	"") echo "\
No preserve list or findmods list for $Subpkg- no files restored."
	    ;;
	 *) echo $Dn "Running preserve -r for subpackage $Subpkg... $Sc"
	    echo "" >$Verbose
            echo "========== subpackage $Subpkg preserve -r `date`" >>$Preslog

	    #
	    # If Restall is set, set -k flags for all files in the preserve
	    # list, i.e., unconditionally restore all mentioned files. This
	    # is for use in cases, such as finding insufficient file space,
	    # where the install procedure is terminated after preserve has been
	    # run, but before the package has been extracted.
	    #
	    case $Restrall in
	      y) cat $Presfiles |\
	           awk ' { if ($0 ~ /^#/) print $0; else print $1 " -k" } ' |\
                     preserve -r $Subpkgver 2>&1 |\
                       tee $Verbose >>$Preslog
		 ;;
              *) cat $Presfiles |\
                    preserve -r $Subpkgver 2>&1 |\
                      tee $Verbose >>$Preslog
		 ;;
	      esac
	    echo ""
	esac
	
      sync

   done

}
    
case $Install in

  update) \
    section "restoring preserved user files"
    trap "echo \"\n*** Cannot interrupt during file restoration ***\n\"" \
      1 2 3 15
    dorestore
    Presrest=n
    trap ". inst_abort" 1 2 3 15
    ;;
  esac



