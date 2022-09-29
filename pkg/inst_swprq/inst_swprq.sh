#!/bin/sh
#
# $Header: inst_swprq.sh,v 1.17.3.5.1.2 90/07/11 18:24:04 hawkes Exp $
#
# ---------------------------------------------------
# | Copyright (c) 1989 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------

set +x

# Ensure that the environment has been set

case "$Instenv" in
  "") . inst_env ;;
esac

# only do this if on miniroot
if [ "$Onmini" = "y" ]
then

# ask user if they wish to change swap partition configuration
echo ""
ask "Do you wish to change swap partition configuration" n y n

if [ "$Ans" != "n" ]
then

 section "configuring swap space"

 MEMORY0=16
# determine how much physical memory in the system
if [ -f /unix ]
then
 getmem /unix physmem
 MEMORY1=$?
 echo "System has $MEMORY1 megabytes of main memory\n"
else
 ## unix isn't here, ask the user what he has
 ask "How many of megabytes of main memory does this machine have" "16"
 MEMORY1=$Ans
fi

 echo "The following is a listing of available disk partitions and sizes."
 echo "NOTE: the miniroot installation system is mounted as / on /dev/swap\n"
 Swapdisk=/dev/rdsk/$TGdsk3$TGswapCont${TGswapDrive}s1
 diskmap -s0 -s2 -s3 -s4 -s5 -s6 -s11 -s12 -s13 -s14 -s15 $Swapdisk

# determine if any alternate swaps have been configured
 grep "swap" /etc/fstab | grep "dev" > /dev/null 2> /dev/null
 ALTSWAP=$?

 if [ `expr $ALTSWAP` -eq 0 ]
 then
	MEMORY0=32
 fi

 echo "(Press return to continue...)"
 read Ans

# determine if we should tell the user to define more swap partitions
 if [ `expr $MEMORY1` -gt `expr $MEMORY0` ]
 then
	echo ""
	echo "It IS recommended that you select an additional swap partition"
	echo "Here are partitions you may select from for additional swap space"
 else
	echo ""
	echo "It IS NOT recommended that you add an additional swap partition"
	echo "But you may add more swap space if desired."
	echo ""
 fi

# only display those that are still available
 diskmap -a $Swapdisk
 NUMSWAP=$?

 echo "Select from the above displayed available partitions as in"
 echo "-----------------------------------------------------------------------"
 echo "                                 -**** Available Partition ****-"
 echo "-----------------------------------------------------------------------"
 echo "If no -**** Available Partition ****-  partitions are displayed"
 echo "then you will need to add an ADDITIONAL DISK if you wish to provide"
 echo "more swap space. Note that the size of the partition is in megabytes."
 echo "It is recommended that the system be configured with 2 or 3 times the"
 echo "swap space as there is system memory. For example a 16 megabyte memory"
 echo "system should have 32 megabytes of swap disk space available."

# Ask if there are any partitons to use
 if [ `expr $NUMSWAP` -gt 0 ]
 then
	ask "Do you wish to add any swap partitions" n y n
 else
	echo "No additional swap partitions available"
	Ans=n
 fi

 echo "\n(Press return to continue...)"
 read NUMSWAP

 if [ "$Ans" != "n" ]
 then

# determine disk naming conventions for this machine

 SWAPpart=/dev/dsk/$TGdsk3$TGswapCont${TGswapDrive}s

    # preset some stuff here
    Ans=q
    touch ADDSWAP
    rm ADDSWAP
    touch ADDSWAP
    Verd=n

    # query user for additional swap partitions
    while [ "$Verd" != "y" ]
    do
    Ans=q
    ask "Which partition should be added for swap [# or q to Quit Adding]" $Ans

	case "$Ans" in

		4|5|7)
		if grep $SWAPpart$Ans ADDSWAP >/dev/null 2>&1
		then
                  :
                else
		  echo "$SWAPpart$Ans none swap rw,noauto 0 0" >> ADDSWAP
		fi
		echo "\nCurrent alternate swap"
		cat ADDSWAP
		Verd=n ;;

		Q|q) Verd=y ;;

		*)
		echo "This is not a Recommend Partition"
		echo "Please select partition 4, 5, or 7 for added swap"
		Verd=n ;;
	esac
    done

    # determine if user added any entries to the swap add file
    if [ -s ADDSWAP ]
    then
	ask "Add additional swap partitions to system /etc/fstab" n y n
	case "$Ans" in
           y) cat ADDSWAP >> /etc/fstab.conf;;
           *) ;;
	esac
    fi

    # cleanup left over file
    rm ADDSWAP
 fi
fi
fi
