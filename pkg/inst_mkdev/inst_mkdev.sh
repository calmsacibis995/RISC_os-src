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
# $Header: inst_mkdev.sh,v 2.2.1.10.1.3 90/07/26 10:03:42 alexp Exp $
#
case "$Instenv" in
  "") . inst_env ;;
  esac

if [ "$Onmini" = "y" ]
  then
    section "making device special files"
    case "$Hostsys" in
*BSD) if [ ! -d /mnt/dev ]
      then
        mkdir /mnt/dev
      fi
      Cdsav=`/bin/pwd`
      cd /mnt/dev
      ./MAKEDEV \
        std ip0 PT0 qt0 st0 cp0 pty0 pty1 pty2 pty3 par0 par1;
      cd $Cdsav
      ;;

   *) ############# Non-BSD section #################
      cd $Pkgroot/dev
      dsk3=$TGdsk3
      bfile=$TGbomfile
      ## devdbfile is the DEV_DB file to edit
      if [ "$TGvariableRoot" = "y" ]
      then
        devdbfile=$TGdevdbfile.$dsk3
      else
        devdbfile=$TGdevdbfile.system
      fi
      mv DEV_DB/$devdbfile DEV_DB/$devdbfile.old
      cat DEV_DB/$devdbfile.old |\
        sed 's/${dsk3}0d0s.*,.*\ rusr/$TGusrDisk, rusr/' | \
        sed 's/${dsk3}0d0s.*,.*\ usr/$TGusrDisk, usr/' \
        > DEV_DB/$devdbfile
      #     rm -f DEV_DB/$devdbfile.old

      OLDusrDisk=`grep "dev\/usr" $Pkg/boms/$bfile | sed "s/.*\///"`
      ##  
      ## If they changed the /usr disk, then we have to fix the bom file
      ##
      if [ "$OLDusrDisk" != "$TGusrDisk" ]
      then
        ## Get the Major, Minor, and Link Count of the OLD usr disk
        set `grep dev/usr $Pkg/boms/$bfile`
        OLDmajor=$5
        OLDminor=$6
        Tmp_link_old=$7

        ## Get the Major, Minor, and Link Count of the NEW usr disk
        set `grep \^dev/dsk/${TGusrDisk} $Pkg/boms/$bfile`
        NEWmajor=$5
        NEWminor=$6
        Tmp_link_new=$7

        echo "modifying bom file $bfile for new location of /dev/usr..."  

        ## OLD link count goes down 1, NEW link count goes up 1
        T_l_o=`expr $Tmp_link_old - 1`
        T_l_n=`expr $Tmp_link_new + 1`
        mv $Pkg/boms/$bfile $Pkg/boms/$bfile.old

        ## Start by putting all the non-changed stuff in $bfile
        grep -v $TGusrDisk $Pkg/boms/$bfile.old | grep -v $OLDusrDisk \
        > $Pkg/boms/$bfile

        ## Next fix the link count on the OLD usr disk
        grep $OLDusrDisk $Pkg/boms/$bfile.old | grep -v usr | \
        sed "s/$OLDmajor $OLDminor $Tmp_link_old/$OLDmajor $OLDminor $T_l_o/" \
        >> $Pkg/boms/$bfile

        ## Next fix the link count on the NEW usr disk
        grep $TGusrDisk $Pkg/boms/$bfile.old | \
        sed "s/$NEWmajor $NEWminor $Tmp_link_new/$NEWmajor $NEWminor $T_l_n/" \
        >> $Pkg/boms/$bfile

        ## Finally, add the "usr" and "rusr" entries
        set `grep dev/usr $Pkg/boms/$bfile.old`
        echo "$1 $2 $3 $4 $NEWmajor $NEWminor $T_l_n dev/dsk/$TGusrDisk" \
        >> $Pkg/boms/$bfile
        set `grep dev/rusr $Pkg/boms/$bfile.old`
        echo "$1 $2 $3 $4 $NEWmajor $NEWminor $T_l_n dev/rdsk/$TGusrDisk" \
        >> $Pkg/boms/$bfile
        echo "done."
      fi
    
#
# Make sure we use the device database we just hacked.
#
      echo "running MKDEV..."  
      if [ -x /dev/mkdevcmd ]
      then
        ./MKDEV -m $TGdevdbfile -d DEV_DB -p -n |\
          /dev/mkdevcmd >/dev/null 2>&1
      else
        ./MKDEV -m $TGdevdbfile -d DEV_DB
      fi
      echo "done."
      ;;
  esac
fi

 
