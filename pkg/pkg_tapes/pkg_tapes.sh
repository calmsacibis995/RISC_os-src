#!/bin/sh
#
# $Header: pkg_tapes.sh,v 2.0.1.10.1.2 90/07/11 18:24:48 hawkes Exp $
#
# ---------------------------------------------------
# | Copyright (c) 1986 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#

if [ "$Pkg" = "" ]
  then
    echo "\$Pkg undefined"
    exit 1
  fi

. pkg_env

#
# set To media
#
To=${To=`pkginfo media`}
Taperw=${Taperw="/dev/rmt/${To}-0"}
Tapenrw=${Tapenrw="/dev/rmt/${To}n-0"}

section "making tapes for `pkginfo pkgname`"

#
# Tapehost tells us which host's tape drive to use. It defaults to the host
# we're running on.
#
Tapehost=${Tapehost=""}

if [ "$Tapehost" != "" ]
  then
    echo "checking tape host ($Tapehost) access..."
    if /usr/ucb/rsh "$Tapehost" date >/dev/null
      then
        echo "talking to tape host $Tapehost OK..."
      else
        echo "can't raise tape host $Tapehost, exiting."
        exit 1
      fi
  fi

#
# Shell functions
#

gettape ()
{
  Ans=""
  while [ "$Ans" != "go" ]
    do
      skip=0 
      echo $Dn "mount a blank, write enabled tape and enter \"go\": $Sc"
      read Ans
      if [ "$Ans" = "skip" ]
      then
        skip=1
        echo " "
        echo "Skipping this tape. "
        echo " "
        Ans=go
      fi
    done

if [ "$skip" != "1" ]
then
  echo $Dn "rewinding the tape... $Sc"
  if [ "$Tapehost" != "" ]
    then
      $Dbgecho /usr/ucb/rsh $Tapehost "mt -f $Taperw rewind"
    else
      $Dbgecho mt -f $Taperw rewind
    fi
  echo ""
  #
  # Do the tape id file
  #
  Pkgname=`pkginfo pkgname`
  Idinfo="$Pkgname `pkginfo version $Pkgname` $Tapen"
  echo $Dn "writing id \"$Idinfo\"... $Sc"
  if [ "$Tapehost" != "" ]
    then
      $Dbgecho echo $Idinfo |\
         $Dbgecho /usr/ucb/rsh $Tapehost /bin/sh -c \
	    "\" dd of=$Tapenrw $Obs >/dev/null 2>&1 \""
    else
      $Dbgecho echo $Idinfo | $Dbgecho dd of=$Tapenrw $Obs >/dev/null 2>&1
    fi
fi
## end non-skip code
  echo ""
}

#
# End of shell functions
#

Tapen=1
while pkginfo volume $To $Tapen >/dev/null 2>&1
  do
    Tapen=`expr $Tapen + 1`
 done

Pkgto=$To

if [ "$Tapehost" != "" ]
  then
    #
    #  OK, some messy stuff here... the idea is to translate tape device names
    #  from SYS-V to BSD or vice versa if we are packaging on one type
    #  system but using the tape drive of another. Yuck, but here goes...
    #

    Myver=`uname -v`
    Tpver=`/usr/ucb/rsh $Tapehost uname -v`

    #
    # Part of the wierdness of the following if comes from the fact that some
    # early (System V) unames returned "UMIPS_V", while newer ones simply
    # call themselves "UMIPS".
    #
    if [ "$Myver" != "$Tpver" -a \
         \( "$Myver" = "UMIPS-BSD" -o "$Tpver" = "UMIPS-BSD" \) ]
      then
        # I was hoping we wouldn't have to do this...
        echo "Translating tape devices names from $Myver to $Tpver..."
        case "$Tpver" in
          *BSD) case "$To" in
	           Q24) To=pt ;;
	           Q11) To=qt ;;
	          Q11h) To=st ;;
	          Q120) echo "BSD does not support QIC-120 tape format!"
		        echo "Try a different Tapehost or tape format."
  		        exit 1
		        ;;
	          esac
	        Taperw=/dev/r${To}8
	        Tapenrw=/dev/r${To}12
	        ;;
             *) case "$To" in
	          pt) To=Q24 ;;
	          qt) To=Q11 ;;
	          st) To=Q11h ;;
	          esac
                Taperw=/dev/rmt/${To}-0
                Tapenrw=/dev/rmt/${To}n-0
	        ;;
          esac
        echo "Taperw=$Taperw, Tapenrw=$Tapenrw"
      fi
  fi
#
#  Blocksize is only meaningful for some kinds of devices...
#

case "$To" in
#  st|Q11) Obs="obs=16k" ;;
       *) Obs="obs=16k" ;;
  esac

Ntapes=`expr $Tapen - 1`
echo
case $Ntapes in
  1) echo "this package requires 1 tape.\n" ;;
  *) echo "this package requires $Ntapes tapes.\n" ;;
  esac

Tapen=1
gettape

cd $Pkgroot

if [ "$skip" != "1" ]
then
echo $Dn "writing instd... $Sc"
if [ "$Tapehost" != "" ]
  then
    $Dbgecho dd if=$Pkg/lib/instd  2>/dev/null |\
      $Dbgecho /usr/ucb/rsh $Tapehost /bin/sh -c \
	"\" dd of=$Tapenrw $Obs >/dev/null 2>&1 \""
  else
    $Dbgecho dd if=$Pkg/lib/instd of=$Tapenrw $Obs >/dev/null 2>&1
  fi
echo ""

if pkginfo os
  then
   if [ "$KernelOnMiniroot" != "y" ]
   then
    ##
    ## There must be a total of 15 unix and space_holder entries below
    ## If you add a new unix, delete one of the space_holder lines
    ##

    echo >$Pkg/lib/space_holder
    for ddfile in tapevol.std tapevol.2030 miniroot \
      unix.r2300_std.boot \
      unix.r2400_std.boot \
      unix.r3200_std.boot \
      unix.i2000_std.boot \
      unix.r3200_ijc.boot \
      unix.r6000_std.boot \
      unix.r3030_std.boot \
      unix.rb3125_std.boot \
      space_holder \
      space_holder \
      space_holder \
      space_holder \
      space_holder \
      space_holder \
      space_holder 

      do
        echo $Dn "writing `basename $ddfile`... $Sc"
        if [ "$Tapehost" != "" ]
          then
            if echo $ddfile | grep unix > /dev/null 2>&1
            then
              if [ -f $Pkgroot/$ddfile ]
              then
                :
              else
                echo "Error.  File $ddfile not found."
                exit
              fi
              $Dbgecho dd if=$Pkgroot/$ddfile 2>/dev/null |\
	      $Dbgecho /usr/ucb/rsh $Tapehost /bin/sh -c \
		"\" dd of=$Tapenrw $Obs >/dev/null 2>&1 \""
            else
              if [ -f $Pkg/lib/$ddfile ]
              then
                :
              else
                echo "Error.  File $ddfile not found."
                exit
              fi
              $Dbgecho dd if=$Pkg/lib/$ddfile 2>/dev/null |\
	      $Dbgecho /usr/ucb/rsh $Tapehost /bin/sh -c \
		"\" dd of=$Tapenrw $Obs >/dev/null 2>&1 \""
            fi
	  else
            if echo $ddfile | grep unix > /dev/null 2>&1
            then
              if [ -f $Pkgroot/$ddfile ]
              then
                :
              else
                echo "Error.  File $ddfile not found."
                exit
              fi
	      $Dbgecho dd if=$Pkgroot/$ddfile of=$Tapenrw $Obs >/dev/null 2>&1
            else
              if [ -f $Pkg/lib/$ddfile ]
              then
                :
              else
                echo "Error.  File $ddfile not found."
                exit
              fi
	      $Dbgecho dd if=$Pkg/lib/$ddfile of=$Tapenrw $Obs >/dev/null 2>&1
            fi
	  fi
        echo ""
      done
   else
    for ddfile in tapevol miniroot
      do
        echo $Dn "writing `basename $ddfile`... $Sc"
        if [ "$Tapehost" != "" ]
          then
              if [ -f $Pkg/lib/$ddfile ]
              then
                :
              else
                echo "Error.  File $ddfile not found."
                exit
              fi
            $Dbgecho dd if=$Pkg/lib/$ddfile 2>/dev/null |\
	      $Dbgecho /usr/ucb/rsh $Tapehost /bin/sh -c \
		"\" dd of=$Tapenrw $Obs >/dev/null 2>&1 \""
	  else
              if [ -f $Pkg/lib/$ddfile ]
              then
                :
              else
                echo "Error.  File $ddfile not found."
                exit
              fi
	    $Dbgecho dd if=$Pkg/lib/$ddfile of=$Tapenrw $Obs >/dev/null 2>&1
	  fi
        echo ""
      done
   fi
  fi
fi
## end non-skip code

while pkginfo volume $Pkgto $Tapen >/dev/null 2>&1
  do
    if [ $Tapen -gt 1 ]
      then
	gettape
      fi
  if [ "$skip" != "1" ]
  then
    for Subpkg in `pkginfo volume $Pkgto $Tapen`
      do
	 echo $Dn "writing subpackage $Subpkg... $Sc"
         if [ "$Verbose" != "/dev/null" ]
	   then
             echo ""
	   fi
         if [ "$Tapehost" != "" ]
           then
             $Dbgecho cat $Pkg/boms/`pkginfo bomname $Subpkg` | \
	       $Dbgecho bom_to_tar | $Dbgecho tar cvnfb - - 32 2>$Verbose |\
	         $Dbgecho /usr/ucb/rsh $Tapehost /bin/sh -c \
	           "\" dd of=$Tapenrw $Obs >/tmp/dd.$$ 2>&1 \""
             /usr/ucb/rsh $Tapehost cat /tmp/dd.$$ > /tmp/dd.$$
             /usr/ucb/rsh $Tapehost rm -f /tmp/dd.$$
             if grep error /tmp/dd.$$ >/dev/null 2>&1
             then
               echo " "
               echo "*****************************************"
               echo "*****************************************"
               echo " "
               echo " ERROR DETECTED IN THE REMOTE dd COMMAND "
               echo " "
               cat /tmp/dd.$$
               echo " "
               echo " EXITING... "
               echo " "
               echo "*****************************************"
               echo "*****************************************"
               rm -f /tmp/dd.$$
               exit 1
             fi
	   else
             $Dbgecho cat $Pkg/boms/`pkginfo bomname $Subpkg` | \
	       $Dbgecho bom_to_tar |\
		 $Dbgecho tar cvnfb - $Tapenrw 32 >$Verbose
	   fi
	 echo ""
      done
    echo $Dn "rewinding the tape... $Sc"
    if [ "$Tapehost" != "" ]
      then
        $Dbgecho /usr/ucb/rsh $Tapehost "mt -f $Taperw rewind"
      else
	$Dbgecho mt -f $Taperw rewind
      fi
    fi
    ## end of non-skip
    echo ""
    Tapen=`expr $Tapen + 1`
  done


