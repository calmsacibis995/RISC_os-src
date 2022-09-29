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
# $Header: inst_mount.sh,v 2.1.1.7.1.2.1.3 90/11/15 13:45:22 beacker Exp $
#
# For installs from the miniroot, this section mounts the user filesystems.
# It mounts all nonnfs (and non S51K, for SYS-V) filesystems that it
# finds in /etc/fstab, relative to /mnt on the miniroot.
#

case "$Instenv" in
  "") . inst_env ;;
  esac

mountbsd()
{
  if [ "$Os" != "y" -o "$Install" != "scratch" ]
    then
      #
      # We will be using the fstab from the user system
      #
      case $Diskcont in
	ip*) Rootpart=${Rootpart=ip0a} ;;
	in*) Rootpart=${Rootpart=in0a} ;;
	esac
      #
      # The error checking is weird, but mount doesn't seem to return a
      # decent status.
      #
      $Dbgecho mount /dev/$Rootpart /mnt >/tmp/mntmsg 2>&1
      if [ -s /tmp/mntmsg ]
        then
	  echo "Couldn't mount /dev/$Rootpart:"
	  cat /tmp/mntmsg
	  exit 1
	fi
      rm -f /tmp/mntmsg 
      $Dbgecho cp /mnt/etc/fstab /etc/fstab.conf
      $Dbgecho umount /dev/$Rootpart
    fi

  echo "/dev/ip0b / 4.3 rw 0 0" >/etc/fstab

  grep -v "^$" /etc/fstab.conf | \
    awk '   { if ( $2 == "/" )
		mntpt = "/mnt"
	      else
		mntpt = "/mnt"$2
		printf "%s %s %s %s %s %s\n", $1, mntpt, $3, $4, $5, $6
            } ' >>/etc/fstab

  exec 0</etc/fstab
  while read Fsent
    do
      set $Fsent dummy
      #
      # Treat pound sign lines in fstab as comments
      #
      case "$1" in
        \#*) ;;
    "dummy") ;;
 	  *) Mntdev=$1
             Mntpt=$2
             Mnttyp=$3
             #
             # Honor the "noauto" option
             #
             shift; shift; shift
             if echo $* | grep 'noauto' >/dev/null 2>&1
    	       then
	         Mnttype=ignore
	       fi
             case $Mnttyp in
               4.3) if [ $Mntpt != "/" ]
	              then
	                if [ ! -d $Mntpt ]
	                  then
	                    mkdirp $Mntpt
	                  fi
                        $Dbgecho mount $Mntdev $Mntpt >/tmp/mntmsg 2>&1
                        if [ -s /tmp/mntmsg ]
                          then
                            if [ $Mntpt = "/mnt" -o $Mntpt = "/mnt/usr" ]
                            then
	                      echo "Error:  Couldn't mount $Mntdev:"
	                      cat /tmp/mntmsg
	                      exit 1
                            else
	                      echo "Warning:  Couldn't mount $Mntdev:"
	                      cat /tmp/mntmsg
	                    fi
	                  else
	                    echo "$Mntdev mounted on $Mntpt"
	                  fi
                        rm -f /tmp/mntmsg 
	              fi
	            ;;
               esac
	esac
    done
    exec 0</dev/tty

}


fstabdevs()
{
#
#  This function examines the fstab.conf file. For each entry the
#  following steps is done:
#
#  Examine the named special file in the user /dev directory. If the
#  named device is not present under the miniroot /dev, make the device
#  on the miniroot /dev; If the named device is present on the miniroot
#  /dev, but the device has different major and minor device numbers,
#  re-mknod the device on the miniroot to agree with the device on
#  the user system. Otherwise (when the device *is* present on the
#  miniroot and matches the user system device), take no action.
#
exec 0</etc/fstab.conf
while read Fsent
  do
    set $Fsent dummy
    #
    # Treat pound sign lines in fstab as comments
    #
    case "$1" in
      \#*) ;;
  "dummy") ;;
        *) Mntdev=$1
           Mntpt=$2
           Mnttyp=$3
           #
           # isolate the cases we are interested in, i.e., ffs filesystems.
           #
           shift; shift; shift
           if echo $* | grep 'noauto' >/dev/null 2>&1
             then
               Mnttype=ignore
             fi
           case $Mnttyp in
             ffs) set `devstat /mnt/$Mntdev 2>/dev/null` dummy
	 	  if [ "$?" != "0" ]
		    then
             	      set `devstat $Mntdev 2>/dev/null` dummy
	 	      if [ "$?" != "0" ]
		        then
		        echo "fstab names a non-device special file: $Mntdev"
 		        exit 1
		      fi
		    fi
		  Sp_type=$1; Sp_maj=$2; Sp_min=$3
		  set `devstat /$Mntdev 2>/dev/null` dummy
		  if [ "$?" != "0" -o "$1" != "$Sp_type" \
                       -o "$2" != "$Sp_maj" -o "$3" != "$Sp_min" ]
                    then
		      echo "Making $Mntdev"
     		      rm -f $Mntdev
    		      mknod $Mntdev $Sp_type $Sp_maj $Sp_min
	            fi
		 ;;
             S51K) echo " "
echo "Error.  S51K file system type is no longer supported. "
echo "Please examine /etc/fstab and comment out, change, or delete all "
echo "references to S51K file systems before retrying the installation. "
echo " "
             exit 1 ;;
             esac
          ;;
       esac 	     
  done
exec 0</dev/tty
}


mountv()
{
  if [ "$Os" != "y" -o "$Install" != "scratch" ]
    then
      #
      # We will be using the fstab from the user system
      #
      Rootpart=${Rootpart=${TGrootDisk}}
      #
      # The error checking is weird, but mount doesn't seem to return a
      # decent status.
      #
      $Dbgecho mount -t ffs /dev/dsk/$Rootpart /mnt >/tmp/mntmsg 2>&1
      if [ -s /tmp/mntmsg ]
        then
	  echo "Couldn't mount /dev/dsk/$Rootpart:"
	  cat /tmp/mntmsg
	  exit 1
	fi
	rm -f /tmp/mntmsg 

# check if we have an old style BSD fstab
	grep " 4\.3 " /mnt/etc/fstab > /dev/null 2> /dev/null
	OLDSTYLE=$?

	if [ `expr $OLDSTYLE` -eq 0 ]
	then
# clean bsd mount fstab
	   sed -e 's/ 4\.3 / ffs /g' < /mnt/etc/fstab | filtfstab > /etc/fstab.conf
	else
# nope
	   cp /mnt/etc/fstab /etc/fstab.conf
	fi
      fstabdevs
      $Dbgecho umount /dev/dsk/$Rootpart
    fi

  echo "/dev/dsk/${TGdsk3}0d0s1 / ffs rw 0 0" >/etc/fstab

  grep -v "^$" /etc/fstab.conf | \
    awk '  { if ( $2 == "/" )
		mntpt = "/mnt"
	      else
		mntpt = "/mnt"$2
		printf "%s %s %s %s %s %s\n", $1, mntpt, $3, $4, $5, $6
            } ' >>/etc/fstab

# check if we have an old style BSD fstab
	grep " 4\.3 " /etc/fstab > /dev/null 2> /dev/null
	OLDSTYLE=$?

	if [ `expr $OLDSTYLE` -eq 0 ]
	then
# clean bsd mount fstab
	   sed -e 's/ 4\.3 / ffs /g' < /etc/fstab | filtfstab > /etc/fstabX
	   rm /etc/fstab
	   mv /etc/fstabX /etc/fstab
	fi

  exec 0</etc/fstab
  while read Fsent
    do
      set $Fsent dummy
      #
      # Treat pound sign lines in fstab as comments
      #
      case "$1" in
	\#*) ;;
    "dummy") ;;
	  *) Mntdev=$1
             Mntpt=$2
             Mnttyp=$3
             #
             # Honor the "noauto" option
             #
             shift; shift; shift
             if echo $* | grep 'noauto' >/dev/null 2>&1
	       then
	         Mnttype=ignore
	       fi
             case $Mnttyp in
               ffs) if [ $Mntpt != "/" ]
	              then
	                if [ ! -d $Mntpt ]
	                  then
	                    mkdirp $Mntpt
	                  fi
                        $Dbgecho mount -t $Mnttyp $Mntdev $Mntpt \
	 	          >/tmp/mntmsg 2>&1
                        if [ -s /tmp/mntmsg ]
                          then
                            if [ $Mntpt = "/mnt" -o $Mntpt = "/mnt/usr" ]
                            then
	                      echo "Error:  Couldn't mount $Mntdev:"
	                      cat /tmp/mntmsg
	                      exit 1
                            else
	                      echo "Warning:  Couldn't mount $Mntdev:"
	                      cat /tmp/mntmsg
	                    fi
	                  else
	                    echo "$Mntdev mounted on $Mntpt"
	                  fi
                        rm -f /tmp/mntmsg 
	              fi
	            ;;
               S51K) echo " "
echo "Error.  S51K file system type is no longer supported. "
echo "Please examine /etc/fstab and comment out, change, or delete all "
echo "references to S51K file systems before retrying the installation. "
echo " "
               exit 1 ;;
               esac
        esac
    done
  exec 0</dev/tty

}

if [ "$Onmini" = "y" ]
  then
    section "mounting filesystems"

    case "$Hostsys" in
          *BSD) mountbsd ;;
          *) mountv ;;
      esac

    echo "\n"

  if [ "$Install" = "update" -a "$TGusrDiskNotSet" = "y" ]
  then
    ## must determine the current /usr disk
    set b `ls -l /mnt/dev/usr`
    ## $c is the major number, $d is the minor number
    c=$6
    d=$7
    a=`ls -l /mnt/dev/usr | sed "s/.*$c/StAr/" | sed "s/$d.*/$d/" | sed "s/StAr/$c/"`
    dsk=`ls -l /mnt/dev/dsk | grep "$a" | sed '2,$d'`
    TGusrDisk=`echo $dsk | sed '2,$d' | sed "s/.* //"`
    case $Machclass$TWODISK in
  *3230*y) TGusrDisk=${TGdsk3}0d1s2
           ;;
     esac
     TGusrCont=`echo $TGusrDisk | sed "s/...//" | sed "s/s.*//"`
     TGusrDrive=`echo $TGusrCont | sed "s/.*d/d/"`
     TGusrCont=`echo $TGusrCont | sed "s/d.*//"`
   fi

# show current /etc/fstab file systems and partitions
  diskmap -s11 -s12 -s13 -s14 -s15 /dev/rdsk/$TGrootDisk
  if [ "$TGrootCont$TGrootDrive" != "$TGusrCont$TGusrDrive" ]
  then
    diskmap -s11 -s12 -s13 -s14 -s15 /dev/rdsk/$TGusrDisk
  fi
  #
  # This flags tells inst_clnup to attempt to copy the packaging information
  # tree from the miniroot to the real filesystem as part of the cleanup.
  #
  Cpypkginfo=y
  fi
