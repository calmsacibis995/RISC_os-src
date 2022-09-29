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
# $Header: inst_rmold.sh,v 1.4.2.4 90/05/22 18:27:35 wje Exp $
#
case "$Instenv" in
  "") . inst_env ;;
  esac

#
# Check to see if there are any boms left for this package; if not,
# offer to remove the entire packaging information tree, which is probably
# obsolete.
#
chkallrmed()
{
  Allrmed=y
  for Rmsubpkg in `pkginfo subpkgnames`
    do
      Bom=`pkginfo bomname $Rmsubpkg`
      if [ -f $Pkg/boms/$Bom ]
	then
	  Allrmed=n
        fi
    done
  case $Allrmed in
    y) Rmpkgname=`pkginfo pkgname`
       Version=`pkginfo version $Rmpkgname`
       ask "
There are no longer any boms present for any subpackages from 
$Pkg version $Version. 
This probably indicates that no subpackages installed from this 
package are still present on this system.

Remove the packaging information tree for this package" y y n
       case $Ans in
	 y) cd /
	    rm -rf $Pkg
	    ;;
	 esac
       ;;
    esac

}


#
# This package has the subpackage we're interested in. If the bom
# for it is still there, offer to clean it up.
#
rmsubpkg()
{
  Bom=`pkginfo bomname $Subpkg`
  if [ -f $Pkg/boms/$Bom ]
    then
      Rmpkgname=`pkginfo pkgname`
      Subpkgver=`pkginfo version $Subpkg`
      Pkgtmp=$Pkg
      Pkg=$Pkgsave
      export Pkg
      Nsubpkgver=`pkginfo version $Subpkg`
      Pkg=$Pkgtmp
      export Pkg
      if [ "$Subpkgver" != "$Nsubpkgver" ]
        then
          ask "Clean up $Pkgname.$Subpkg $Subpkgver" n y n
          case $Ans in
	    y) cd $Pkgroot
	       Version=`pkginfo version $Subpkg`
               echo \
               "Removing leftover files from $Rmpkgname.$Subpkg $Version..."
               $Dbgecho stripln -t `pkginfo timestamp` $Pkg/boms/$Bom
	       rm $Pkg/boms/$Bom
               ;;
            esac
        fi
    fi
}


rmolds()
{
  section "cleaning up old versions"

  Pkgname=`pkginfo pkgname`
  Pkgsave=$Pkg

  echo "\
An attempt will now be made to clean up any files left over from previous
versions of the software which has just been installed.

Searching for old versions to remove..."

  for Subpkg in $Subpkgs
    do
      for Pkg in $Pkgroot/usr/pkg/lib/*
        do
	  if [ -f $Pkg/pkginfo ]
	    then
  	      export Pkg
	      if [ `basename $Pkg` != `basename $Pkgsave` ]
	        then
     	          for Osubpkg in `pkginfo subpkgalias`
	            do
	              if [ $Osubpkg = $Subpkg ]
		        then
		          rmsubpkg
		        fi
		    done
	        fi
	      chkallrmed
	    fi
	done
    done

  Pkg=$Pkgsave
  export Pkg

}

if [ $Os = "y" -a $Install = "scratch" ]
  then
    : # well, then we won't do anything...
  else
    rmolds
  fi
  
