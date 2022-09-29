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
# $Header: inst_presv.sh,v 1.3.2.2 90/05/10 03:53:18 wje Exp $
#
case "$Instenv" in
  "") . inst_env ;;
  esac

dopreserve()
{
  Preslog=${Preslog=$Pkg/lib/preserve.log}
  Findmods=${Findmods=n}
  Instlog=${Instlog=$Pkgroot/etc/installlog}
  
  case $Findmods in
    n) ;;
    y) if [ ! -f $Instlog ]
         then
           ask "\
The install log \"$Instlog\" is not present, so findmods cannot be
used.
  
Proceed with the installation" y y n
          case "$Ans" in
            y) Findmods=n ;;
            *) exit 1
               ;;
            esac
         fi
       ;;
    esac

  cd $Pkgroot
  
  for Subpkg in $Subpkgs
    do

      Findmodsout=$Pkg/lib/${Subpkg}.findmods
      Preserves=$Pkg/lib/${Subpkg}.preserves
      Pkgsubpkg=`pkginfo pkgname`.$Subpkg
      Subpkgver=`pkginfo version $Subpkg`
      Prestmp=$Pkg/lib/preserve.tmp
      Presfiles="";

      case $Findmods in
	y) echo $Dn "Running findmods for subpackage $Subpkg... $Sc"
           findmods `pkginfo pkgname` $Pkg/boms/`pkginfo bomname $Subpkg` \
	     >$Findmodsout
	   case $? in
	     0) echo "(found modified files)"
   	        Presfiles="$Presfiles $Findmodsout"
		;;
	     1) echo "(none found)"
	        rm $Findmodsout
	        ;;
 	     esac
	   ;;
        esac

      if [ -f $Preserves ]
	then
	  Presfiles="$Presfiles $Preserves"
	fi

      case $Presfiles in
	"") echo "\
No preserve list or findmods list for $Subpkg- preserve not executed."
	    ;;
	 *) echo $Dn "Running preserve -s for subpackage $Subpkg... $Sc"
	    echo "" >$Verbose
            echo "========== subpackage $Subpkg preserve -s `date`" >>$Preslog
            cat $Presfiles |\
              preserve -s $Subpkgver 2>&1 |\
                tee $Verbose | tee $Prestmp >>$Preslog
            set `grep '^preserve' $Prestmp | wc` "insure at least one set arg"
	    rm -f $Prestmp
	    case $Verbose in
	      /dev/null) echo "$1 files preserved." ;;
	      esac
	esac
      sync

   done

}
    
case $Install in

  update) \
    section "preserving local files"
    trap "echo \"\n*** Cannot interrupt during file preservation ***\n\"" \
      1 2 3 15
    dopreserve
    Presrest=y
    trap ". inst_abort" 1 2 3 15
    ;;    
  esac

