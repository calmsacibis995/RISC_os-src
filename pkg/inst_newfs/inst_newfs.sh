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
# $Header: inst_newfs.sh,v 2.2.1.12.1.2.1.2 90/11/15 13:45:46 beacker Exp $
#
# This section is used in scratch installs to make new filesystems.
# In the first cut, for scratch installs, we will assume the standard
# partitioning reflected by the fstab included with the release.
# At some future time, more flexibility may be added, probably including
# a section to allow automated editing on /etc/fstab prior to making
# filesystems.
#

case "$Instenv" in
  "") . inst_env ;;
  esac

#
# This function is used to divine the disktypes supported in /etc/disktab
#
disktypes()
{
  Dtypes=""	
  Ntypes=0
  while read Type
    do
      echo $Dn " $Type $Sc"
      Ntypes=`expr $Ntypes + 1`
      if [ `expr $Ntypes % 6` -eq 0 ]
        then
          echo ""
        fi
    done << ----EOF----
 `grep '^[^ 	#]' </etc/disktab | sed -e 's/|.*//'`
----EOF----
}

	
donewfsbsd()
{
  for Partinfo in $Partitions
    do
      set `echo $Partinfo | sed -e 's/=/ /g'`
      Partition=$1
      Disktype=$2
      Partsz=$3

      echo "\nInitializing the filesystem on $Partition..."

      ## Special Case for 2 disk 3230
      case "$Machclass$TWODISK$Partition" in
        *3230*y*/dev/usr*) $Dbgecho newfs.ffs -m 4 -s $Partsz -c 32 -i 12288 $Partition $Disktype 2>&1 | tee -a $Pkg/lib/newfs.out ;;
      *) $Dbgecho newfs.ffs -s $Partsz $Partition $Disktype 2>&1 |\
        tee -a $Pkg/lib/newfs.out ;;
      esac
      sync

      if [ "$Diskcont" = "ip3200" ]
        then
          echo "\nTuning filesystem for rot 7..."
          $Dbgecho /etc/tunefs.ffs -d 7 $Partition
        fi

      echo "\nChecking the filesystem on $Partition..."
      $Dbgecho fsck -p $Partition
    done
}  

newfsbsd ()
{
  umount -a # Just in case something was mounted- newfs is oblivious

  #
  # Presently only make 4.3 filesystems.
  #

  Partitions=""
  
  rm -f $Pkg/lib/newfs.out

  exec 0</etc/fstab.conf
  while read Mntent
    do
      set $Mntent
      case "$3" in
        4.3) Partition=$1
             set `ls -l $Partition`
             #
             # Note: there are assumptions about minor device numbering here
             # which may need to change in the future...
             #
             Drive=`expr $5 / 16`
             Lastdrive=${Lastdrive=""}
             Defdisk=${Defdisk=2333-64}
             if [ "$Drive" != "$Lastdrive" ]
	       then
                 Disktype="noneiveeverheardof"
                 until echo "$Dtypes" | grep " $Disktype " >/dev/null
                   do
                     ask "Disk type for drive $Drive" $Defdisk
	             Disktype=$Ans
	             if echo "$Dtypes" | grep " $Disktype " >/dev/null
	               then
	                 Lastdrive=$Drive
		         Defdisk=$Disktype
	               else
		         echo "Valid disk types are:"
			 for Type in $Dtypes
			   do
			     echo "  $Type"
			   done
		       fi
	           done
		fi
	     ask "Initialize filesystem on $Partition" y y n
	     case $Ans in
               y) Partitions="$Partitions ${Partition}=${Disktype} " ;;
	       esac
	     ;;
        esac
    done
  exec 0</dev/tty

  donewfsbsd
}

	
donewfsv()
{
  for Partinfo in $Partitions
    do
      set `echo $Partinfo | sed -e 's/=/ /g'`

      Partition=$1
      Disktype=$2
      Partsz=$3

      echo "\nInitializing the filesystem on $Partition..."

      ## Special Case for 2 disk 3230
      case "$Machclass$TWODISK$Partition" in
        *3230*y*/dev/usr*) $Dbgecho newfs.ffs -m 4 -s $Partsz -c 32 -i 12288 $Partition $Disktype 2>&1 | tee -a $Pkg/lib/newfs.out ;;
      *) $Dbgecho newfs.ffs -s $Partsz $Partition $Disktype 2>&1 |\
        tee -a $Pkg/lib/newfs.out ;;
      esac
      sync

      if [ "$Diskcont" = "ip3200" ]
        then
          echo "\nTuning filesystem for rot 7..."
          $Dbgecho /etc/tunefs.ffs -d 7 $Partition
        fi

      echo "\nChecking the filesystem on $Partition..."
      $Dbgecho fsck.ffs -y $Partition
    done
}  

newfsv ()
{
  umount -a # Just in case something was mounted- newfs is oblivious

  #
  # Presently only make ffs filesystems.
  #

  Partitions=""

  rm -f $Pkg/lib/newfs.out

  exec 0</etc/fstab.conf
  while read Mntent
    do
      #
      # Treat # signs in fstab as comments
      #
      set $Mntent
      case "$1" in
	\#*) ;;
	  *) case "$3" in
	        ffs) Partition=$1
	             set `ls -l $Partition`
		     #
		     # After the case statement below has executed, $Vh will be
		     # the path to the volume header device entry corresponding
		     # to the drive containing the partition were are going to
		     # newfs, $Partnum will be the partition number within the
		     # drive, $Controller will be the controller number within
		     # the system, and $Drive will be the drive number...
		     #
		     case "$Diskcont" in
		       ip*|in*|ij*)
	                 #
	                 # Note: there are assumptions about minor device
			 # numbering here which may need to change in the
			 # future...
	                 #
	                 Controller=`expr $6 / 32`
	                 Minmod32=`expr $6 % 32`
	                 Partnum=`expr $6 % 16`
	                 Drive=`expr $Minmod32 / 16`
		         case "$Diskcont" in
		           ip*) Vh=/dev/rdsk/ipc${Controller}d${Drive}vh ;;
		           in*) Vh=/dev/rdsk/int${Controller}d${Drive}vh ;;
		           ij*) Vh=/dev/rdsk/ijc${Controller}d${Drive}vh ;;
			   esac
			 ;;
		       sdc*|isc*)
			 Controller=0 # This is good for the first six drives
		         Partnum=`expr $6 % 16`
			 Drive=`expr $6 / 16`
		         case "$Diskcont" in
			     isc*) Vh=/dev/rdsk/isc${Controller}d${Drive}vh ;;
			     sdc*) Vh=/dev/rdsk/sdc${Controller}d${Drive}vh ;;
			 esac
		         ;;
		       esac
	
	             Fssz=`/etc/prtvtoc $Vh |\
	               awk "/^[ 	]*$Partnum[ 	]/ { print \\$5 }"`
	
	             # Hack to workaround the "last 2 sectors" bug (MIPS bug
	             # 1328).  This should be removed in the first release
	             # where this is fixed.
		     # Fssz=`expr $Fssz - 100`
		     # (This is supposed to be fixed in RISC/os 3.0, but we're
		     # going to leave this in (commented out) until we're sure)
	
	             Lastvh=${Lastvh=""}
	             if [ "$Vh" != "$Lastvh" ]
		       then
	                 Disktype="noneiveeverheardof"
	                 until echo "$Dtypes" | grep " $Disktype " >/dev/null
	                   do
	                     ask "Disk type for controller $Controller drive $Drive" \
		  	       $Defdisk
		             Disktype=$Ans
		             if echo "$Dtypes" | grep " $Disktype " >/dev/null
		               then
		                 Lastvh=$Vh
			         Defdisk=$Disktype
		               else
	  		         echo "Valid disk types are:"
			         for Type in $Dtypes
			           do
			             echo "  $Type"
			           done
			       fi
			  done
	  	       fi
		     ask "Initialize filesystem on $Partition" y y n
		     case $Ans in
	               y) Partitions="$Partitions ${Partition}=${Disktype}=${Fssz} "
		     esac
		     ;;
	        esac
		;;
	esac
    done
  exec 0</dev/tty

  donewfsv
}

#
# End of shell functions
#
if [ "$Os" = "y" -a \
     "$Onmini" = "y" ]
  then
echo " "
echo "Please answer \"y\" to the following question unless you really understand"
echo "the consequences."
echo " "
ask "Do you want to install sash to the volume header" y y n
fi

if [ "$Os" = "y" -a \
     "$Onmini" = "y" -a \
     "$Ans" = "y" ]
  then
    section "installing sash to the volume header"

    case "$Hostsys" in
          *BSD) case $Diskcont in
                  ip*) Dvhvol=/dev/rip0vh ;;
                  in*) Dvhvol=/dev/rin0vh ;;
                  esac

                if [ "$Os2" = "y" ]
                then
                  case $Machname in
                    *2030*) $Dbgecho dvhtool -f $Dvhvol -v creat /stand/sash.2030 sash ;;
                    *) $Dbgecho dvhtool -f $Dvhvol -v creat /stand/sash.std sash ;;
                  esac
                else
                  $Dbgecho dvhtool -f $Dvhvol -v creat /stand/sash sash
                fi
                $Dbgecho dvhtool -f $Dvhvol -v bootfile /vmunix
                ;;

             *) Dvhvol=/dev/dsk/${TGdsk3}${TGrootCont}${TGrootDrive}vh 

                if [ "$Os2" = "y" ]
                then
                  case $Machname in
                    *2030*) $Dbgecho dvhtool -f $Dvhvol -v creat /stand/sash.2030 sash ;;
                    *) $Dbgecho dvhtool -f $Dvhvol -v creat /stand/sash.std sash ;;
                  esac
                else
                  $Dbgecho dvhtool -f $Dvhvol -v creat /stand/sash sash
                fi
                $Dbgecho dvhtool -f $Dvhvol -v bootfile /unix
    	        ;;
      esac
  fi

if [ "$Install" = "scratch" -a \
     "$Os" = "y" -a \
     "$Onmini" = "y" ]
  then
	section "determining /usr partition"

	#
	# Here is where we ask the user which partition he would like for
	# /usr.  Currently if he does not pick 6 or 4, we give him a
	# warning, and proceed.
	#

    Verd=n
    ## On a 2030, recommend partition 3 for 172MB drives
    ## On a 2 disk 3230, don't ask, force it to partition 2
    case $Machclass in
      2030) echo " "
              echo "If you are installing on a 172 MB drive, we recommend that you select"
              echo "partition 3 for /usr." 
              echo " " ;;
      3230) if [ "${TWODISK}" = "y" ]
              then
                Verd=y
              fi ;;
    esac
    if [ "$TGusrDisk" != "${TGdsk3}0d0s6" ]
    then
      ## If they already changed /usr, don't ask again
      Verd=y
    fi
    while [ "$Verd" != "y" ]
    do
      # display the possible partitions available

      diskmap -i -s0 -s1 -s2 -s7 -s11 -s12 -s13 -s14 -s15 /dev/rdsk/$TGusrDisk

	echo "\nPossible partitions to use are marked by -**** Available Partition ****-"
	echo "         select either partition 3, 4, 5 or 6"

	ask "\
Which partition should /usr be installed on" $USRpart

        USRpart=$Ans
        USRminor=$Ans
	case "$Ans" in
	  3|4|5) echo "/usr partition will be installed on partition $Ans"
          TGusrDisk="${TGdsk3}0d0s$Ans"
	  Verd=y ;;
	  6) echo "/usr partition will be installed on partition $Ans"
	  Verd=y ;;
	  *) echo "WARNING: partition $Ans is a partition not recommended"
	   echo "         select either partition 3, 4, 5 or 6"
	         ;;
	esac
    done

	set `ls -l /dev/usr`
	Tmp_minor=$6
	Tmp_part=`expr $6 % 16`
	Tmp_major=`expr $5 : '\(.*\),'`
	if [ "$USRminor" != "$Tmp_minor" ]
	then
                Tmp_usr=/dev/dsk/${TGdsk3}
		echo "Changing /dev/usr link from ${Tmp_usr}0d0s$Tmp_part to $Tmp_usr${TGusrCont}${TGusrDrive}s$USRpart"
		rm -f /dev/usr
		ln $Tmp_usr${TGusrCont}${TGusrDrive}s$USRpart /dev/usr
	fi

    section "initializing filesystems"

    #
    # Note: Contents of the Dtypes variable:
    # 1. Must list all valid disk types
    # 2. Every type (including the first) must be bracketed with a ' '
    # 3. Please add newlines as appropriate, since the value of this
    # variable will be used for prompting after a bogus choice
    # [NOTE: this is now set up automatically by examining
    # /etc/disktab
    #
    Dtypes=`disktypes`

    ask "\
A scratch install of an operating system package is being performed
from the miniroot. Normally in this case the filesystems are
initialized.  When a filesystem is initialized, any existing data will
be lost.  You will be given a chance to override initialization of
each individual filesystem below.

Initialize filesystems" y y n
    case "$Ans" in
      y) case "$Hostsys" in
               *BSD) newfsbsd ;;
	       *) newfsv ;;
           esac
	 ;;
      esac
  fi
