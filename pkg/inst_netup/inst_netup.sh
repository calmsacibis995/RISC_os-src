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
# $Header: inst_netup.sh,v 1.4.2.4 90/05/10 03:52:45 wje Exp $
#
case "$Instenv" in
  "") . inst_env ;;
  esac

case $From in
  en) case $Onmini in
	y) section "initializing the network"
           Ans=n
           while [ "$Ans" != "y" ]
             do
               #
               # set hostname
               #
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
               case "$Server" in
                 "") case $Hostsys in
		           *BSD) Server=quacky ;;
		           *) Server=dunkshot ;;
	               esac
                     ask "Enter the hostname of the install server" "$Server"
                     Server=$Ans
	             ;;
	         esac
        
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
           case $Machname in
                       m120*) Enetdev=${Enetdev=la0} ;;
                      RC6280) Enetdev=${Enetdev=enp0} ;;
                      m2000*) Enetdev=${Enetdev=enp0} ;;
             m1000|m800|m500) case $Hostsys in
			            *BSD) Enetdev=${Enetdev=en0} ;;
			            *) Enetdev=${Enetdev=enp0} ;;
			        esac
             esac
           $Dbgecho hostname "$Hostname"
	   $Dbgecho ifconfig $Enetdev "$Hostname" $Netarg $Broadarg
	   $Dbgecho ifconfig lo0 localhost
	   case $Hostsys in
	         *BSD) Psopts=-ax ;;
	         *) Psopts=-e ;;
	     esac
	   if ps $Psopts | grep -v grep | grep routed >/dev/null
	     then
	       : # There's already one running, don't start a new one
	     else
               $Dbgecho routed &
               $Dbgecho route add "$Hostname" localhost 0
	     fi
           if [ "$Gateway" != "" ]
             then
               $Dbgecho route add $Server $Gateway 1
             fi
           $Dbgecho hostid "$Hostname"
	   if ps $Psopts | grep -v grep | grep inetd >/dev/null
	     then
	       : # There's already one running, don't start a new one
	     else
               $Dbgecho inetd &
	     fi
	   ;;      
	*) section "checking server access"
        esac
      #
      # See if we can raise the net...
      #
      echo /usr/ucb/rsh "$Server" date
      if /usr/ucb/rsh "$Server" date
        then
          echo "Talking to server $Server OK..."
        else
          echo "Can't raise server $Server, exiting."
          exit 1
        fi
      ;;
  esac

