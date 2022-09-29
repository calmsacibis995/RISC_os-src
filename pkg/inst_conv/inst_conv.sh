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
# $Header: inst_conv.sh,v 2.0.1.2.1.1.1.2 90/11/06 17:36:45 beacker Exp $
#
case "$Instenv" in
  "") . inst_env ;;
  esac

#
# Might as well set up some network config stuff for OS scratch installs
# from the miniroot...
#

if [ $Install = "scratch" -a $From = "en" -a $Os = "y" ]
  then
    section "configuring network on newly installed system"
    ask "Set up /etc/local_hostname and /etc/hosts for installed system" \
      y y n
    case $Ans in
      y) cat >/mnt/etc/local_hostname <<===EOF===
$Hostname
netmask $Netmask broadcast $Broadcast
===EOF===
         cp /etc/hosts /mnt/etc/hosts
	 ask "Local domainname" mips
	 echo $Ans >/mnt/etc/local_domainname
	 rcp $Server:/etc/hosts.equiv /mnt/etc
	 rcp $Server:/.rhosts /mnt
         ;;
      esac
  fi     


if [ $Install = "update" ]
  then

    First=y
    Convout=${Convout=$Pkg/lib/convert.out}

    cd $Pkg/conv

    for Subpkg in $Subpkgs
      do
        for File in `ls $Subpkg.* 2>/dev/null`
          do
            if [ -x $File ]
               then
                 case $First in
                   y) section "running conversion scripts"
	              First=n
	              ;;
                   esac
	         echo "========== $File   `date` ==========" 2>&1
                 ./$File 2>&1
               fi
          done
      done
  fi

# Special case for rb3133
if [ "$Machclass" = "RB3133" ]
then
  if [ -f $Pkgroot/unix.rb3125_std ]
  then
    mv $Pkgroot/unix.rb3125_std $Pkgroot/unix.rb3133_std
    rm -f $Pkgroot/unix
    ln $Pkgroot/unix.rb3133_std $Pkgroot/unix
  fi
fi
