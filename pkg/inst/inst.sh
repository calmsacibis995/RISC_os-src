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
# $Header: inst.sh,v 2.1.1.11.1.2.1.3 90/11/15 13:44:43 beacker Exp $
#
# This is the top-level command for starting the installation stuff;
# it has just enough smarts to extract the packaging information
# tree (unless running on the miniroot), and then kick off inst_start.
#

set -a
 
#### Uncomment and Modify the following lines for Network Installations ####
#### PLEASE READ THE RELEASE NOTES BEFORE ATTEMPTING A NETWORK INSTALL  ####
#
# From=en
# Server=MACHINE_NAME
# Rpkgroot=${Rpkgroot=/ROOT}
# Defrpkg=${Defrpkg=/ROOT/usr/pkg/lib/PKG_NAME}

#%askgoeshere%

Hostsys=`uname -v`
Machname=`uname -t`
Netuser=${Netuser="bin"}

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
# find enet device
#
case $Machclass in
  2030|3230|120|RB3125|RB3133) Enetdev=${Enetdev=la0} ;;
          2000|6000) if (ifconfig "egl0" > /dev/null 2>&1)
                     then
                       Enetdev=${Enetdev=egl0}
                     else
                       Enetdev=${Enetdev=enp0}
                     fi ;;
               mbox) case $Hostsys in
			            *BSD) Enetdev=${Enetdev=en0} ;;
			            *) Enetdev=${Enetdev=enp0} ;;
                     esac ;;
                  *) ## unknown machine, let's try em all
                     if (ifconfig "$Enetdev" > /dev/null 2>&1)
                     then
                       :
                     else
                       if (ifconfig "la0" > /dev/null 2>&1)
                       then
                         Enetdev=la0
                       else
                         if (ifconfig "egl0" > /dev/null 2>&1)
                         then
                           Enetdev=egl0
                         else
                           if (ifconfig "enp0" > /dev/null 2>&1)
                           then
                             Enetdev=enp0
                           else
                             echo "Can't determine the Ethernet interface (known types: enp0, la0, and egl0. "
                             echo "Please set the environment variable Enetdev and then run inst again. "
                             echo "Exiting... "
                             exit
                           fi
                         fi 
                       fi
                     fi 
                     ;;
esac
#
#  Onmini set to y if running on the miniroot else n
#
if [ -f /.miniroot ]
then
  Onmini=y
else
  Onmini=n
fi

#
# Things for newlineless echo. Ugh.
#
case $Hostsys in
      *BSD) Dn="-n"; Sc="";;
      *) Dn=""; Sc="\c";;
  esac

trap 'echo "\n*** Interrupt ***\n"; rm -rf $Pkgroot/usr/pkg/lib/inst_tar.out ;\
       exit 1' \
  1 2 3 15

echo "\nSoftware package installation\n"

if [ "$EnTapehost" != "" ]
then
    echo "Remote Tape Installation selected."
    echo "The tape must be mounted on the machine: $EnTapehost"
    echo " "
    Server=$EnTapehost
fi
#
# Just enough environment to get going here
#
## We have to get the net up earlier for remote tape or en
## The following take the place of inst_netup
if [ "$From" = "en" -o "$EnTapehost" != "" ]
then
  if [ "$Onmini" = "y" ]
  then
    echo " "
    echo "=====  initializing the network  ====="
    echo " "
    Ans=n
    while [ "$Ans" != "y" ]
    do
        #
        # set hostname
        #
        if [ "$Hostname" = "" ]
        then
          Hostname=`hostname`
        fi
        Hostname=${Hostname=""}
        until 
          ask "Enter the hostname of this machine" "$Hostname" ;
          Hostname=$Ans ;
	  [ "$Hostname" != "" ]
        do
          : 
        done
 
        #
        # set server
        #
        if [ "$EnTapehost" != "" ]
        then
          Server=$EnTapehost
        fi
        case "$Server" in
          "") case $Hostsys in
                *BSD) Server=quacky ;;
                *) Server=dunkshot ;;
              esac
              if su -c $Netuser /usr/ucb/rsh "$Server" < /dev/null date >/dev/null 2>&1
              then
                :
              else
                Server=""
                if [ "$EnTapehost" != "" ]
                then
                  Server=$EnTapehost
                fi
              fi
              ask "Enter the hostname of the install server" "$Server"
              Server=$Ans
	      ;;
        esac
        
## Let's try to get the old values
Netmask=`ifconfig $Enetdev | sed 1d | sed "s/ broadcast.*//" | sed "s/.* /0x/"`
Broadcast=`ifconfig $Enetdev | sed 1d | sed "s/.*broadcast //"`
        #
        # set netmask
        #
        Netmask=${Netmask=0xffff0000}
        ask "Enter the netmask" "$Netmask"
        Netmask=$Ans
           
        if [ "$Netmask" != "" ]
        then
          Netarg="netmask $Netmask"
        else
          Netarg=""
        fi
         
        #
        # set broadcast address
        #
        Broadcast=${Broadcast=97.0.0.0}
        ask "Enter the broadcast address" "$Broadcast"
        Broadcast=$Ans
         
        if [ "$Broadcast" != "" ]
        then
          Broadarg="broadcast $Broadcast"
        else
          Broadarg=""
        fi
        
        if grep $Hostname /etc/hosts >/dev/null
        then
          :
        else
          InetAddr=${InetAddr=97.0.0.0}
          ask "Enter the inet address for $Hostname " "$InetAddr"
          echo "$Ans 	$Hostname" >> /etc/hosts
        fi
  
        if grep $Server /etc/hosts >/dev/null
        then
          :
        else
          ServAddr=${ServAddr=97.0.0.0}
          ask "Enter the inet address for $Server " "$ServAddr"
          echo "$Ans 	$Server" >> /etc/hosts
        fi
               
      ask "
hostname:  $Hostname
server:    $Server
netmask:   $Netmask
broadcast: $Broadcast

Ok" y y n
               echo
             done

        #
        # configure the net
        #
           $Dbgecho hostname "$Hostname"
	   $Dbgecho ifconfig $Enetdev "$Hostname" $Netarg $Broadarg
	   $Dbgecho ifconfig lo0 localhost
	   case $Hostsys in
	         *BSD) Psopts=-ax ;;
	         *) Psopts=-e ;;
	     esac
## We don't have a kernel yet, so we can't do a ps
#	   if ps $Psopts | grep -v grep | grep routed >/dev/null
#	     then
#	       : # There's already one running, don't start a new one
#	     else
## Just start it
               $Dbgecho routed & >/dev/null 2>&1
               $Dbgecho route add "$Hostname" localhost 0 >/dev/null 2>&1
#	     fi
           if [ "$Gateway" != "" ]
             then
               $Dbgecho route add $Server $Gateway 1
             fi
           $Dbgecho hostid "$Hostname"
## We don't have a kernel yet, so we can't do a ps
#	   if ps $Psopts | grep -v grep | grep inetd >/dev/null
#	     then
#	       : # There's already one running, don't start a new one
#	     else
## Just start it
               $Dbgecho inetd & >/dev/null 2>&1
#	     fi
       fi

### This test is performed below.  No reason to do it twice.
#  if [ "$From" = "en" -o "$EnTapehost" != "" ]
#  then
#    echo " "
#    echo "=====  checking server access  ====="
#    echo " "
#    #
#    # See if we can raise the net...
#    #
#    echo /usr/ucb/rsh "$Server" date
#    if /usr/ucb/rsh "$Server" date
#    then
#      echo "Talking to server $Server OK..."
#    else
#      echo "Can't raise server $Server, exiting."
#      exit 1
#    fi
# fi

fi

# END NET UP
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
  if su -c $Netuser /usr/ucb/rsh "$EnTapehost" < /dev/null date >/dev/null 2>&1
  then
    :
  else
              echo "\
Can't raise server $Server. The system must be able to access the
server before the installation can proceed. Please initialize the
network and retry the installation.\n"
              exit 1
  fi
  TapeHostsys=`su -c $Netuser /usr/ucb/rsh $EnTapehost < /dev/null uname -v 2> /dev/null`
fi

Verbose=${Verbose=/dev/null}


#
# For network installs
# 
case "$From" in
  en) case $Hostsys in
	    *BSD) Defserver=${Server=quacky}
	          ;;
	    *) Defserver=${Server=dunkshot}
	          ;;
        esac
      Defrpkgroot=${Rpkgroot=/relroot}
      Defrpkg=${Defrpkg=/rel}

      echo "Network installation selected.\n"
      if su -c $Netuser /usr/ucb/rsh "$Server" < /dev/null date >/dev/null 2>&1
      then
        :
      else
        Defserver="none"
      fi
      # echo "Defserver is $Defserver"
      ask "Server" $Defserver
      Server=$Ans
      if su -c $Netuser /usr/ucb/rsh "$Server" < /dev/null date >/dev/null 2>&1
      then
        :
      else
              echo "\
Can't raise server $Server. The system must be able to access the
server before the installation can proceed. Please initialize the
network and retry the installation.\n"
              exit 1
      fi
      ask "Remote package root" $Defrpkgroot
      Rpkgroot=$Ans
      ask "Remote packaging information tree" $Defrpkg
      Rpkg=$Ans
      Tapenrw=""; Taperw=""

      if [ ! -f /.miniroot ]
        then
          #
          #  see if we can raise the net...
          #
          echo "\nsu -c $Netuser /usr/ucb/rsh $Server date"
          if su -c $Netuser /usr/ucb/rsh "$Server" < /dev/null date 2> /dev/null
            then
              echo "Talking to server $Server OK...\n"
            else
              echo "\
Can't raise server $Server. The system must be able to access the
server before the installation can proceed. Please initialize the
network and retry the installation.\n"
              exit 1
            fi
	fi
      ;;
      
   *) case $TapeHostsys in
            *BSD) From=${From=pt}
	          Taperw=${Taperw="/dev/r${From}8"}
	          Tapenrw=${Tapenrw="/dev/r${From}12"} ;;
            *) From=${From=Q24}
	          Taperw=${Taperw="/dev/rmt/${From}-0"}
	          Tapenrw=${Tapenrw="/dev/rmt/${From}n-0"} ;;
	esac 
      ## LocalFrom is used by pkginfo to determine where a
      ## package is on a tape.  We just need a legal device.
      case $Hostsys in
            *BSD) LocalFrom=${LocalFrom=$From} ;;
            *) LocalFrom=${LocalFrom=$From} ;;
	esac ;;
  esac

rewind ()
{
  echo $Dn "Rewinding the tape... $Sc"
  if [ "$EnTapehost" != "" ]
    then
      su -c $Netuser /usr/ucb/rsh $EnTapehost < /dev/null "mt -f $Taperw rewind"
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
      su -c $Netuser /usr/ucb/rsh $EnTapehost < /dev/null "mt -f $Tapenrw fsf $1"
    else
      mt -f $Tapenrw fsf $1
    fi
  echo ""
}

#
# If we're running on the miniroot then we already have the packaging
# information tree for the package we're going to install; otherwise
# we need to get it from tape or the net.
#
if [ -f /.miniroot ]
  then

    case $Hostsys in
      *BSD) #
            # update /etc/psdatabase
            #
            ps -U
	    ;;
      esac

    Pkgroot=/mnt
    Pkg="/`cat /Pkgname`"

  else

    #
    # Better ask about this, better safe than sorry...
    #
    Pkgroot=${Pkgroot=/}
    ask "Install package relative to where" $Pkgroot
    Pkgroot=$Ans
    #
    # guarentee $Pkgroot/usr/pkg/lib exists, the kludgy way
    #
    mkdir $Pkgroot $Pkgroot/usr $Pkgroot/usr/pkg $Pkgroot/usr/pkg/lib \
      >/dev/null 2>&1

    cd $Pkgroot/usr/pkg/lib

    case $From in
      en) echo $Dn "\nExtracting packaging information tree... $Sc"
	  echo "" >$Verbose
      su -c $Netuser /usr/ucb/rsh $Server < /dev/null cat $Rpkg/lib/instd | tar xvf - | \
	   tee inst_tar.out >$Verbose
          #### tar xvNf $Server $Rpkg/lib/instd | tee inst_tar.out >$Verbose
	  ;;
       *) Verified=n
          while [ $Verified != y ]
            do
              echo $Dn "
Please mount the (first, if multiple tapes) distribution
tape, then press return... $Sc"
	      read Ans
              $Dbgecho rewind
              echo $Dn "Verifying tape id... $Sc"
	      case "$From" in
	        st|Q11|HC|hc) Ibs="ibs=16k" ;;
                     *) Ibs="" ;;
	        esac

              if [ "$EnTapehost" != "" ]
              then
                Id="`su -c $Netuser /usr/ucb/rsh $EnTapehost < /dev/null dd if=$Tapenrw $Ibs 2>/dev/null | sed -e '2,$d'`"
              else
                Id="`dd if=$Tapenrw $Ibs 2>/dev/null | sed -e '2,$d'`"
              fi
	      case "$Id" in
		"") Id=bogusid ;;
		esac
	      set $Id
              case $# in
        	3) if [ "$3" != "1" ]
                     then
         	       echo "\n
The tape id indicates this is tape number $3; expected tape number 1."
		     $Dbgecho rewind
    	             else
	               Verified=y
	               echo "ok"
	             fi
		   ;;
                *) echo "\nBad tape id file"
		   ;;
                esac
            done
	  echo $Dn "\nExtracting packaging information tree... $Sc"
          if [ "$EnTapehost" != "" ]
          then
	    case $From in
              st|Q11|HC|hc) tar xvNfb $EnTapehost $Taperw 32 | tee inst_tar.out >$Verbose ;;
                   *) tar xvNf $EnTapehost $Taperw | tee inst_tar.out >$Verbose ;;
	    esac
          else
	    case $From in
              st|Q11|HC|hc) tar xvfb $Taperw 32 | tee inst_tar.out >$Verbose ;;
                   *) tar xvf $Taperw | tee inst_tar.out >$Verbose ;;
	    esac
          fi
	  ;;
      esac
    Pkgdir=`cat inst_tar.out | sed -e '2,$d' |\
      sed -e 's%^x \([^/]*\)/.*$%\1%'`
    echo "$Pkgdir"
    rm -f inst_tar.out

    Pkg=$Pkgroot/usr/pkg/lib/$Pkgdir
  fi

mkdir $Pkg/conv >/dev/null 2>&1

Tapemnted=1
exec $Pkg/bin/inst_start
