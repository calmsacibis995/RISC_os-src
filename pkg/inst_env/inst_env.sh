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
# $Header: inst_env.sh,v 2.2.1.15.1.4.1.5 90/12/20 19:20:24 beacker Exp $
#
# inst_env
#
# This file must be "sourced" rather than run in a subshell. 
#
# It sets up values for the environment variables required by the
# installation tools.  An attempt is made to provide correct defaults
# generically for all packages. In most cases, existing values in the
# environment are taken as overrides of the defaults provided herein.
#

set -a

if [ "$Pkgshflags" != "" ]
  then
    set $Pkgshflags
  fi

if [ "$Pkgroot" = "" ]
  then
    echo "\$Pkgroot undefined"
    exit 1
  fi

#
# Define/Pick-up shell functions
#

. ask

mkdirp()
{
  Fullname="$1"

  set `echo "$Fullname" | tr '/' ' '`

  case "$Fullname" in
    /*) Dir="/$1" ;;
     *)	Dir="$1" ;;
    esac
  shift

  if [ ! -d "$Dir" ]
    then
      mkdir "$Dir"
      chmod 777 "$Dir"
    fi

  for sdir in "$@"
    {
      Dir="$Dir/$sdir"
      if [ ! -d "$Dir" ]
	then
	  mkdir "$Dir"
	  chmod 777 "$Dir"
	fi
    }
}  


section ()
{
  echo "
========== $1 ==========
"
}


#
# Use the commands and packaging/installation tools from the release if
# they exist.
#
Stdpath=/bin:/usr/bin:/etc:/usr/ucb
PATH=$PATH:$Stdpath

#
#  Onmini set to y if running on the miniroot else n
#
if [ -f /.miniroot ]
  then
    Onmini=y
    Pkgroot=/mnt
    Pkg="/`cat /Pkgname`"
  else
    Onmini=n
  fi

#
#  Insure that $Pkg/lib exists
#
if [ ! -d $Pkg/lib ]
  then
    rm -rf $Pkg/lib
    mkdir $Pkg/lib
  fi

#
#  Os set to y if this is an os package, else n
#
#  Os2 set to y if this is an os2 package (kernels after miniroot), else n
#
if pkginfo os
  then
    Os=y
    if [ "$KernelOnMiniroot" != "y" ]
    then
      Os2=y
    else
      Os2=n
    fi
  else
    Os=n
  fi

if [ "$Os" = "y" ]
then
  ## Set Netuser to root if a net install
  if [ "$From" = "en" -o "$From" = "net" ]
  then
    Netuser="root"
  fi
  ## check /dev/usr
  if ls -l /dev/usr >/dev/null 2>&1
  then
    :
  else
    echo " "
    echo "ERROR!  /dev/usr does not exist."
    echo "Please create the device /dev/usr and start the installation over."
    echo " "
    echo "Exiting... "
    exit 1
  fi
fi

#
#  Default Install type to scratch only for os packages being
#  installed from the miniroot...
#

if [ "$Os" = "y" -a "$Onmini" = "y" ]
  then
    Install=${Install=scratch}
  else
    Install=${Install=update}
  fi

#
# Hostsys will always tell us whether we're working on a BSD or a V
# system.
#
# TapeHostsys tells us whether the tape is on a BSD or a V system.
#
Hostsys=`uname -v`
TapeHostsys=$Hostsys
if [ "$EnTapehost" != "" ]
then
  Netuser=${Netuser="bin"}
  if su -c $Netuser /usr/ucb/rsh "$EnTapehost" < /dev/null date >/dev/null 2>&1
  then
    :
  else
              echo "\
Can't raise server $EnTapehost. The system must be able to access the
server before the installation can proceed. Please initialize the
network and retry the installation.\n"
              exit 1
  fi
  TapeHostsys=`su -c $Netuser /usr/ucb/rsh $EnTapehost < /dev/null uname -v 2> /dev/null`
fi

#
# Things for newlineless echo. Ugh.
#
case $Hostsys in
      *BSD) Dn="-n"; Sc="";;
      *) Dn=""; Sc="\c";;
  esac

#
# Verbosity control. Usually set to /dev/tty for verbososity.
#
Verbose=${Verbose=/dev/null}


case $Install in
   update) Findmods=${Findmods:=${findmods=n}}
	   ;;
  scratch) ;;
        *) echo "Unknown install type: \"$Install\"; exiting..."
	   exit 1
	   ;;
  esac

#
# Media/format selection for tape install tools.
# $To selects format.
#
#
# For network installs
# 
case $From in

  en|net) Rpkg=${Rpkg=/rel}
      Rpkgroot=${Rpkgroot=/relroot}
      Tapenrw=""; Taperw=""
      ;;

   *) case $TapeHostsys in
            *BSD) From=${From=pt}
	          Taperw=${Taperw="/dev/r${From}8"}
	          Tapenrw=${Tapenrw="/dev/r${From}12"} ;;
               *) From=${From=Q24}
	          Taperw=${Taperw="/dev/rmt/${From}-0"}
	          Tapenrw=${Tapenrw="/dev/rmt/${From}n-0"} ;;
	esac

      rewind ()
      {
        echo $Dn "rewinding the tape... $Sc"
        if [ "$EnTapehost" != "" ]
        then
su -c $Netuser /usr/ucb/rsh "$EnTapehost" < /dev/null mt -f $Taperw rewind
          ##/usr/ucb/rsh $EnTapehost "mt -f $Taperw rewind"
        else
          mt -f $Taperw rewind
        fi
        echo ""
      }

      fsf ()
      {
	echo $Dn "Forward spacing the tape... $Sc"
        if [ "$EnTapehost" != "" ]
        then
su -c $Netuser /usr/ucb/rsh "$EnTapehost" < /dev/null mt -f $Tapenrw fsf $1
          ##/usr/ucb/rsh $EnTapehost "mt -f $Tapenrw fsf $1"
        else
          mt -f $Tapenrw fsf $1
        fi
        echo ""
      }
      
      ;;

  esac

#
# What is this beastie we're running on, anyway?
#
Machname=`uname -t`

#
# find machine class
#
case $Machname in
       m1000|m800|m500) Machclass=mbox ;;
                *2030*) Machclass=2030 ;;
                *3230*) Machclass=3230 ;;
                *3330*) Machclass=3230 ;;
                 m120*) Machclass=120 ;;
                *3240*) Machclass=120 ;;
              m2000-25) Machclass=RB3125 ;;
              m2000-33) Machclass=RB3133 ;;
                m2000*) Machclass=2000 ;;
                *3260*) Machclass=2000 ;;
                 RC62*) Machclass=6000 ;;
                 rc62*) Machclass=6000 ;;
                 RS62*) Machclass=6000 ;;
                 rs62*) Machclass=6000 ;;
esac
#
# What kind of disk controller is drive 0?
#
# reserved values:
#   ip3200	interphase 3200
#   ip4200	interphase 4200
#   in300	introl 300
#   isc120	m120 scsi
#   sdc2030	rc2030 scsi
#   sdc3230	rc3230 scsi
#   ij4210      interphase 4210
#
TGMachname=$Machname
case $Machclass in
  mbox) Diskcont=${Diskcont=ip3200} 
        TGdsk3=ipc
        TGunix=unix.r2300_std
        TGfsfnumber=5
        TGdevdbfile=m1000
        TGbomfile=r2300
        Defdisk=${Defdisk=2333-64} ;;
  2000|6000|RB3125|RB3133) TGvariableRoot=y
        if [ "$RootOnSCSI" = "" -o "$TGdsk3" = "" ] 
        then
          Diskmaj=`/etc/kopt get rootdev 2>&1 | sed -e 's#^.*0x##'`
          Diskmaj=`echo $Diskmaj | sed -e 's#......$##'`

          case "$Diskmaj" in
            4|9)                            TGdsk3=ipc ;;
            16|17|18|19|1a|1b|1c|1d)        TGdsk3=ijc ;;
            21)                             TGdsk3=sdc ;;
            *) # kopt didn't tell us, better ask
               if [ "$Os" = "y" ]
               then
                 case $Machclass in
                   2000|6000) choices="ipc ipc ijc" ;;
                   RB3125|RB3133) choices="ipc ipc ijc sdc" ;;
                 esac
                 ask "Which controller type did you boot off" $choices
                 TGdsk3=$Ans
               fi
               ;;
          esac

          if [ "$TGdsk3" = "ipc" ]
          then
            RootOnSCSI=n
            Diskcont=${Diskcont=ip4200}
            Defdisk=${Defdisk=2333-64} 
          else
            RootOnSCSI=y
            if [ "$TGdsk3" = "sdc" ]
            then
              RootOnSDC=y
              Diskcont=${Diskcont=sdc3230}
              Defdisk=${Defdisk=94191}
            else
              Diskcont=${Diskcont=ij4210}
              Defdisk=${Defdisk=94171}
            fi 
          fi
	fi
        ;;
  120)  Diskcont=${Diskcont=isc120} 
        TGdsk3=isc
        TGunix=unix.r2400_std
        TGfsfnumber=6
        TGdevdbfile=m120
        TGbomfile=r2400
        Defdisk=${Defdisk=94171} ;;
  2030) Diskcont=${Diskcont=sdc2030} 
        TGdsk3=sdc
        TGunix=unix.r2030_std
        TGfsfnumber=8
        TGdevdbfile=rc2030
        TGbomfile=r2030
        Defdisk=${Defdisk=94351} ;;
  3230) Diskcont=${Diskcont=sdc3230} 
        TGdsk3=sdc
        TGunix=unix.r3030_std
        TGfsfnumber=11
        TGdevdbfile=RC3230
        TGbomfile=r3030
        Defdisk=${Defdisk=LXT-200S} ;;
     *) ## Unknown machine
        echo " "
        echo "Warning, this is not a recognized machine: $Machname "
        echo " "
        if [ "$Os" = "y" ]
        then
          echo "Please answer the following questions: "
          echo " "
          echo "Known controllers: ipc, ijc, sdc, isc."
          ask "Which controller type did you boot off" ""
          TGdsk3=$Ans
          Diskcont=${Diskcont=$TGdsk3}
          echo " "
          echo "Examples of BOM file names: r2300, r2400, r3200, etc. "
          ask "What is the name of the BOM file for this machine" ""
          TGbomfile=$Ans
          echo " "
          echo "Examples of DEV_DB names: m1000, m2000, rc2030, etc. "
          ask "What is the name of the DEV_DB file for this machine" ""
          TGdevdbfile=$Ans
        fi
        ;;
esac
case $Machclass in
   2000) if [ "$RootOnSCSI" != "y" ] 
         then
           ## SMD root
           TGdevdbfile=m2000
           TGbomfile=r3200
           TGunix=unix.r3200_std
           TGfsfnumber=7
           TGMachname=m2000
         else
           ## SCSI root
           TGdevdbfile=m2000
           TGbomfile=r3260
           TGunix=unix.r3200_ijc
           TGfsfnumber=9
           TGMachname=rc3260
         fi ;;
   6000) TGunix=unix.r6000_std
         TGfsfnumber=10
         if [ "$RootOnSCSI" != "y" ] 
         then
           ## SMD root
           TGdevdbfile=RC6280
           TGbomfile=r6280
           TGMachname=RC6280
         else
           ## SCSI root
           TGdevdbfile=RC6280
           TGbomfile=r6260
           TGMachname=RC6260
         fi ;;
RB3125) TGunix=unix.rb3125_std
         TGfsfnumber=12
         TGdevdbfile=m2000-25
         TGbomfile=rb3125$TGdsk3
         TGMachname=m2000-25$TGdsk3
         ;;
RB3133) TGunix=unix.rb3125_std
         TGfsfnumber=12
         TGdevdbfile=m2000-25
         TGbomfile=rb3125$TGdsk3
         TGMachname=m2000-33$TGdsk3
         ;;
esac
          
#
# Default usr parition.  May be overridden by query
#
if [ "$TGusrDisk" = "" ]
then
  TGusrDiskNotSet=y
fi
case $Machclass$TWODISK in
  *3230*y) USRpart=${USRpart=2}
           USRdrive=${USRdrive=1}
           USRminor=${USRminor=18} 
           TGusrDisk=${TGusrDisk=${TGdsk3}0d1s2}
           if [ -f /etc/fstab.conf ]
           then
             linesinfstab=`cat /etc/fstab.conf | wc -l`
             if [ $linesinfstab = 2 ]
             then
## On a two disk 3230 with a 2 line /etc/fstab.conf
## add the two lines for /usr1 and /usr2 in the middle of the file
##
ed - /etc/fstab.conf << !EOF
1a
/dev/dsk/sdc0d0s6	/usr1	ffs rw 0 2
/dev/dsk/sdc0d0s7	/usr2	ffs rw 0 2
.
w
q
!EOF
             fi
           fi
           ;;
        *)
           USRpart=${USRpart=6}
           USRdrive=${USRdrive=0}
           USRminor=${USRminor=6} ;;
esac

if [ "$Os" = "y" ]
then
  ## Only need all this stuff on an OS install
  TGusrDisk=${TGusrDisk=${TGdsk3}0d0s6}
  TGrootDisk=${TGrootDisk=${TGdsk3}0d0s0}
  TGswapDisk=${TGswapDisk=${TGdsk3}0d0s1}
  case $TGrootDisk in
    ${TGdsk3}*d*s0) ;;
                 *) echo "Warning!  TGrootDisk=$TGrootDisk does not match the expected form: ${TGdsk3}#d#s0"
                    ask "Continue at your own risk or Abort" a a c
                    case $Ans in
                      c) ;;
                      a) echo "Please set the environment variable TGrootDisk and try again."
                         echo "Exiting..."
                         exit ;;
                    esac
  esac
  case $TGusrDisk in
    ${TGdsk3}*d*s*) ;;
                 *) echo "Warning!  TGusrDisk=$TGusrDisk does not match the expected form: ${TGdsk3}#d#s#"
                    ask "Continue at your own risk or Abort" a a c
                    case $Ans in
                      c) ;;
                      a) echo "Please set the environment variable TGusrDisk and try again."
                         echo "Exiting..."
                         exit ;;
                    esac
  esac
  case $TGswapDisk in
    ${TGdsk3}*d*s*) ;;
                 *) echo "Warning!  TGswapDisk=$TGswapDisk does not match the expected form: ${TGdsk3}#d#s#"
                    ask "Continue at your own risk or Abort" a a c
                    case $Ans in
                      c) ;;
                      a) echo "Please set the environment variable TGswapDisk and try again."
                         echo "Exiting..."
                         exit ;;
                    esac
  esac
  TGusrCont=`echo $TGusrDisk | sed "s/...//" | sed "s/s.*//"`
  TGusrDrive=`echo $TGusrCont | sed "s/.*d/d/"`
  TGusrCont=`echo $TGusrCont | sed "s/d.*//"`
  TGrootCont=`echo $TGrootDisk | sed "s/...//" | sed "s/s.*//"`
  TGrootDrive=`echo $TGrootCont | sed "s/.*d/d/"`
  TGrootCont=`echo $TGrootCont | sed "s/d.*//"`
  TGswapCont=`echo $TGswapDisk | sed "s/...//" | sed "s/s.*//"`
  TGswapDrive=`echo $TGswapCont | sed "s/.*d/d/"`
  TGswapCont=`echo $TGswapCont | sed "s/d.*//"`
fi

if [ "$InstDiskless" != "y" ]
then
#
# Ask if we should keep going
#
echo " "
echo "Installation Information:"
echo " "
if [ $Install = scratch ]
then
  echo "This is a SCRATCH install.  Data on the root  and /usr disks will be lost. "
else
  if [ "$Os" = "y" -a "$Onmini" = "y" ]
  then
    echo "This is an update install."
  fi
fi
if [ "$From" = "en" -o "$From" = "net" ]
then
  echo "Packages will be read in over the Ethernet. "
elif [ "$EnTapehost" != "" ]
then
  echo "Packages will be read in from the tape drive on $EnTapehost."
else
  pkginfo position $From xxx >/tmp/pkgerr$$ 2>&1
  if grep "requested media type not found" /tmp/pkgerr$$ >/dev/null 
  then
    OLDFrom=$From
    From=`pkginfo media` > /dev/null 2>&1
    LocalFrom=$From
    echo " "
    echo "The tape in the drive was written in $From format."
    echo "Resetting the tape device variable from $OLDFrom to $From."
    echo " "
  fi
  rm -f /tmp/pkgerr$$
  echo "Packages will be read in from the local $From tape device."
fi
  
echo "Machine type: $Machname "
case $Machclass$TWODISK in
  *3230*y) echo "The usr partition will be on drive 1."
esac
if [ "$RootOnSCSI" = "y" ]
then
  if [ "$Machclass" = "RB3125" -o "$Machclass" = "RB3133" ]
  then
    if [ "$RootOnSDC" = "y" ]
    then
      echo "Root disk type: SCSI (sdc)"
    else
      echo "Root disk type: SCSI (ijc)"
    fi
  else
    echo "Root disk type: SCSI"
  fi
elif [ "$RootOnSCSI" = "n" ]
then
  echo "Root disk type: SMD "
fi
echo " "
sleep 1
            
ask "Is the information above correct?" y y n
case $Ans in
  n) echo " "
     echo "Please check the Release Notes and run inst again.  Exiting..."
     exit 1
     ;;
esac
fi

#
# Instenv in the environment allows subshells to know if the environment
# has been set up. This is mainly so they can source this file if they
# are being executed directly from the interactive shell during
# debugging.
#
Instenv=y
