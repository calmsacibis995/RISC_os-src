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
# $Header: inst_clnup.sh,v 2.0.1.3.1.2.1.2 90/11/06 17:36:15 beacker Exp $
#
trap "" 0
trap "" 1 2 3 15

# ensure that shell verbose is turned off
set +x

case "$Instenv" in
  "") . inst_env ;;
  esac

section "cleaning up"

#
# Restore preserved files if necessary
#
case "$Presrest" in
  y) Restall=y
     . inst_restr
     ;;
  esac

case $Onmini in
  y) # On miniroot
     #
     # Cpypkginfo is set to y in inst_mount when filesystems have been
     # mounted so that the packaging information tree can be copied to the
     # target filesystem
     #
     case "$Cpypkginfo" in
       y) #
          # copy instd directory stuff to real /usr/filesystem, for posterity
          #
          cd $Pkg
          Pkgdname=`/bin/pwd`
          Pkgdname=`basename $Pkgdname`
          Upkg=$Pkgroot/usr/pkg/lib/$Pkgdname
          echo $Dn "Copying packaging information directory to $Upkg... $Sc"
          mkdirp $Upkg/lib >/dev/null 2>&1 
          mkdirp $Upkg/boms >/dev/null 2>&1
          mkdirp $Upkg/conv >/dev/null 2>&1
          cp $Pkg/pkginfo $Upkg
	  if [ -f $Pkg/lib/comply.out ]
	    then
              cp $Pkg/lib/comply.out $Upkg/lib
	    fi
	  if [ -f $Pkg/lib/newfs.out ]
	    then
              cp $Pkg/lib/newfs.out $Upkg/lib
	    fi
	  if [ -f $Pkg/lib/preserve.log ]
	    then
              cat $Pkg/lib/preserve.log >>$Upkg/lib/preserve.log
              CleanVer=`pkginfo version umips`
              echo "\n"
              cleannew $Pkg/lib/preserve.log $CleanVer /mnt
	    fi
	  #
	  # The looping rigamarole below is just in case nothing matches the
	  # wild card. What a pain.
	  # 
          for Cpfile in $Pkg/boms/*
	    do
	      if [ -f $Cpfile ]
		then
		  cp  $Cpfile $Upkg/boms
		fi
	    done
	  for Cpfile in $Pkg/conv/*
	    do
	      if [ -f $Cpfile ]
		then
	          cp $Cpfile $Upkg/boms
		fi
	    done
	  echo ""
          #
          # Also copy in the miniroot fstab, which may have been editted to
          # set up the disk local configuration.
          #
          if [ $Os = "y" -a $Install = "scratch" ]
            then
              mkdir /mnt/etc >/dev/null 2>&1
              echo $Dn "Copying miniroot fstab to installed system... $Sc"
              cp /etc/fstab.conf /mnt/etc/fstab
              echo ""
          else
	  # check if we have an old style BSD fstab
	  grep " 4\.3 " /mnt/etc/fstab > /dev/null 2> /dev/null
	  OLDSTYLE=$?

	  # if yes save the old fstab as fstab BSD
	  # and use the new SYSV fstab file instead
	  if [ `expr $OLDSTYLE` -eq 0 ]
	  then
		echo "Renaming old /etc/fstab to /etc/fstab.BSD"
		mv /mnt/etc/fstab /mnt/etc/fstab.BSD
		cp /etc/fstab.conf /mnt/etc/fstab
	  fi

	if [ -f /mnt/etc/inittab ]
	then
          ## inittab already exists - don't mess with it
          echo " "
        else
          ## inittab does not already exists - mess with it
	  if [ -f /mnt/etc/inittab.conf ]
          then
	    cat < /mnt/etc/inittab.conf  >> /mnt/etc/inittab
	    echo ""
	  fi
        fi

	if [ -f /mnt/etc/ttytype ]
	then
          ## ttytype already exists - don't mess with it
          echo " "
        else
          ## ttytype does not already exists - mess with it
	  if [ -f /mnt/etc/ttytype.conf ]
	  then
	    cat < /mnt/etc/ttytype.conf  >> /mnt/etc/ttytype
	    echo ""
	  fi
        fi

          # check if user created more swap disks to add
		if [ -f ADDSWAP ]
		then
		  echo $Dn "Adding swap disks to installed system fstab... $Sc"
		  cat < ADDSWAP  >> /mnt/etc/fstab
		  echo ""
		fi
          fi
	  ;;
       esac

# check if BSD system kernel laying around
	if [ -f /mnt/vmunix ]
	then
		echo "Removing old BSD kernel /vmunix"
		rm /mnt/vmunix*
	fi

# check if user changed usr and make the same as miniroot
#      if [ "$Onmini" = "y" -a -b /mnt/dev/usr -a -b /dev/usr ]
#      then
#	set `ls -l /dev/usr`
#	Mini_Usr_part=`expr $6 % 16`
#	Mini_Usr_major=`expr $5 : '\(.*\),'`
#	set `ls -l /mnt/dev/usr`
#	Usr_part=`expr $6 % 16`
#	Usr_major=`expr $5 : '\(.*\),'`
#	if [ "$Mini_Usr_part" != "$Usr_part" ]
#	then
#          rm -f /mnt/dev/usr
#          ln /mnt/dev/dsk/$TGdsk3$TGusrCont${TGusrDrive}s$Mini_Usr_part /mnt/dev/usr
#	fi
#     fi

     #
     # unmount filesystems
     #
     cd /
     echo "Unmounting filesystems..."
     case "$Hostsys" in
           *BSD) umount -av -t 4.3 ;;
           *) umount -av -t ffs ;;
       esac
     ;;

  n) # Not on Miniroot
     ask "Remove install tools" n y n
     case $Ans in
       y) cd /; $Dbgecho rm -rf $Pkg/bin/* ;;
       esac
     ;;
  esac
  
