#!/bin/sh
#
# $Header: inst_mkstuff.sh,v 2.1.1.2.1.2 90/07/11 18:23:00 hawkes Exp $
#
# ---------------------------------------------------
# | Copyright (c) 1989 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------

# Ensure that the environment has been set

case "$Instenv" in
  "") . inst_env ;;
esac

# check if install scratch

if [ "$Install" = "scratch" -a "$Os" = "y" ]
then

# See if we should make the /usr/adm/crash crash directory

if [ -d /mnt/usr/adm/crash ]
then
    echo ""
else
    section "making special directories"

    echo "Default crash directory is /usr/adm/crash"
    Defcrash=${Defcrash=/mnt/usr/adm/crash}
    ask  "Set name of crash directory" "$Defcrash"
    Defcrash=$Ans
    ask  "Should we create this directory for kernel core dumps" y y n
    case $Ans in
      y) mkdir $Defcrash ;;
      *) echo "Not created" ;;
    esac
    echo "\n"
fi     

# ask user if they wish to change swap partition configuration

echo ""

ask "Do you wish to configure the network" n y n

if [ "$Ans" != "n" ]
then

if [ -f /mnt/etc/local_hostname -a -f /mnt/etc/local_domainname ]
then
    echo ""
else
	section "making special network files"
fi

# See if we should make the local_hostname file

if [ -f /mnt/etc/local_hostname ]
then
# determine if it has junk in it
	grep "no_hostname_set" /mnt/etc/local_hostname > /dev/null 2> /dev/null
	HOSTNAME=$?

	if [ `expr $HOSTNAME` -eq 0 ]
	then
# remove it if it does and allow for a good one to be setup
		touch /mnt/etc/local_hostname
		rm /mnt/etc/local_hostname
	fi
fi

# now build a good local_hostname if it does not exist
if [ -f /mnt/etc/local_hostname ]
then
    echo ""
else
    if [ "$Hostname" -eq "" ]
    then
        Hostname=${Hostname=no_hostname}
    fi
    ask  "Set hostname" "$Hostname"
    Hostname=$Ans

    if [ "$Netmask" -eq "" ]
    then
        Netmask=${Netmask=0xffff0000}
    fi
    ask  "Set netmask" "$Netmask"
    Netmask=$Ans

    if [ "$Broadcast" -eq "" ]
    then
        Broadcast=${Broadcast=255.255.255.0}
    fi
    ask  "Set broadcast address" "$Broadcast"
    Broadcast=$Ans

    if [ "$NetAddr" -eq "" ]
    then
        NetAddr=${NetAddr=127.1.0.0}
    fi
    ask  "Set net address" "$NetAddr"
    NetAddr=$Ans

    ask  "Should we create the /etc/local_hostname file" y y n
    case $Ans in
      y) cat >/mnt/etc/local_hostname <<===EOF===
$Hostname
netmask $Netmask broadcast $Broadcast
===EOF===
         ;;
      *) echo "Not Created" ;;
    esac
    echo ""

    echo "$Hostname $NetAddr"
    ask  "Should we add the above entry to the /etc/hosts file" y y n
    case $Ans in
      y) cat >>/mnt/etc/hosts <<===EOF===
$NetAddr $Hostname
===EOF===
         ;;
      *) echo "Not Added" ;;
    esac
    echo "\n"
fi     

# See if we should make the local_domainname file

if [ -f /mnt/etc/local_domainname ]
then
    echo ""
else
    Domainname=${Domainname=mips.com}
    ask  "Set domain name" "$Domainname"
    Domainname=$Ans

    ask  "Should we create the /etc/local_domainname file" y y n

    case $Ans in
      y) cat >/mnt/etc/local_domainname <<===EOF===
$Domainname
===EOF===
         ;;
      *) echo "Not created" ;;
    esac
    echo "\n"
fi

# ensure that miniroot /dev/usr is same as target /mnt/dev/usr

# get current target dev usr
set `ls -l /mnt/dev/usr`
USRpart=`expr $6 % 16`
USRmajor=`expr $5 : '\(.*\),'`

# get current minroot dev usr
set `ls -l /dev/usr`
Tmp_part=`expr $6 % 16`
Tmp_major=`expr $5 : '\(.*\),'`

# determine if scratch or update
if [ "$Install" = "scratch" -a "$Os" = "y" ]
then

# handle as scratch
# check if same and correct if need be
	if [ "$USRpart" != "$Tmp_part" ]
	then
          rm -f /mnt/dev/usr
          ln /mnt/dev/dsk/$TGdsk3$TGusrCont${TGusrDrive}s$Tmp_part /mnt/dev/usr
	fi
  fi
fi
fi
