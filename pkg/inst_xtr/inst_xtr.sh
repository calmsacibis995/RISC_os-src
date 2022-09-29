#!/bin/sh
#
# $Header: inst_xtr.sh,v 2.4.1.8.1.2 90/07/11 18:24:17 hawkes Exp $
#
# ---------------------------------------------------
# | Copyright (c) 1986 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------

Netuser=${Netuser="bin"}

case "$Instenv" in
  "") . inst_env ;;
  esac

if [ "$Onmini" = "y" ]
then
  Tar=tar
else
  Tar=/bin/tar
fi
         
positiontape()
{
  echo " "
  echo "positioning the tape... "
  $Dbgecho rewind
  case $Tapen in
    1) case $Os in
         y) case $Os2 in
              y) $Dbgecho fsf 20 ;; # id, instd, tpvls, mroot, unixes
              n) $Dbgecho fsf 4 ;; # past id, instd, tapevol, mroot
            esac ;;
         n) $Dbgecho fsf 2 ;; # past id, instd
       esac ;;
    *) $Dbgecho fsf 1 ;; # past id
  esac
  echo ""
  File=1
  if [ "$File" -lt "$Spkgfile" ]
  then
    $Dbgecho fsf `expr $Spkgfile - $File`
    File=$Spkgfile
  fi
}

gettape()
{
  Tapen=`expr $Tapen + 1`
  Verified=n
  Pkgname=`pkginfo pkgname`
  case $Havetape in
    y) $Dbgecho rewind ;;
    esac
  while [ $Verified != y ]
    do
      #
      # This test keeps us from asking for the first tape, which
      # is already mounted if the install was started from the
      # inst command...
      #
      if [ "$Tapemnted" != "$Spkgvol" ]
	then
          echo $Dn "
Please mount $Pkgname tape number $Spkgvol and press return: $Sc"
          read Ans </dev/tty
	fi
      Tapemnted="x" # From here on out you don't know what is mounted
      $Dbgecho rewind
      echo $Dn "Verifying tape id... $Sc"
      if [ "$EnTapehost" != "" ]
      then
        Id="`su -c $Netuser /usr/ucb/rsh $EnTapehost < /dev/null dd if=$Taperw 2>/dev/null | sed -e '2,$d'`"
      else
        Id="`dd if=$Taperw 2>/dev/null | sed -e '2,$d'`"
      fi
      case "$Id" in
   	"") Id=bogusid ;;
	esac
      set $Id
      case $# in
	3) Version=`pkginfo version $Pkgname`
           if [ "$1" != "$Pkgname" -o "$2" \
	        != "$Version" -o "$3" != "$Spkgvol" ]
             then
	       echo "\n
Tape id does not verify; expected:
  $Pkgname $Version tape number $Spkgvol
got:
  $1 $2 tape number $3"
	     else
	       Verified=y
	       echo "ok"
	     fi
	   ;;
        *) echo "
bad tape id file"
           ;;
        esac
    done
  Havetape=y
}

Readapkg()
{
echo " "
Filelist=""
##size=0
#get the sizes and total files over here
#
Thename=`pkginfo bomname $Subpkg`
su -c $Netuser /usr/ucb/rsh $Server < /dev/null "grep Mb $Rpkg/boms/$Thename.total" | sed s/...Mb.*// | sed s/.*\ // >$Thename.total
## Just in case total file is empty
if grep \. $Thename.total >/dev/null
then
  :
else
  echo 20 >$Thename.total
fi
# Div is the number of 5 MEG blocks in the subpkg
Div=`cat $Thename.total`
rm -f $Thename.total
Div=`expr "$Div" / 5`
if [ $Div = "0" ]
then
  Div=1
fi
# Brkpt is the number of files after which we should break
Brkpt=`expr "$#" / "$Div" + 1`
if [ $Brkpt -gt 50 ]
then
  # A break point > 50 results in an "Arg list too long" error
  Brkpt=50
fi
##echo "Brkpt if $Brkpt"
j=0
k=0
for i in $* 
do
  j=`expr $j + 1`
  k=`expr $k + 1`
  Filelist="$Filelist $i"
  if [ $k = $Brkpt -o $j = $# ]
  then
    k=0
    echo "$j of $#"
    ##echo "Filelist is $Filelist"
    Retrys=0
    Tardone="N"
    while [ "$Tardone" = "N" ]
    do 
      rm -f Error_file
      # This is the actual read of the files
      su -c $Netuser /usr/ucb/rsh $Server < /dev/null "cd $Rpkgroot;tar cf - $Filelist 2> /tmp/E$$" | $Tar xvf - >>$Verbose 2>>Error_file
      ####tar cf - -C $Rpkgroot $Filelist | tar xvf - >>$Verbose 2>>Error_file
      Tarexit=$?
      # If there were remote errors, get them over here
    su -c $Netuser /usr/ucb/rsh $Server < /dev/null "cat /tmp/E$$;rm /tmp/E$$" >>Error_file
      # if there is anything in Error_file, then there were errors
      if grep \. Error_file >/dev/null
      then
	echo " "
	echo "Error(s):"
	echo " "
        cat Error_file
	echo " "
        echo "There were errors in the transmission of the last set of files."
        if [ $Retrys = "3" ]
        then
          echo "Third retry failed."
          Ans="n"
        else
          ask "Retry this file set?" y n y
        fi
        if [ $Ans = "y" ]
        then
          Retrys=`expr $Retrys + 1`
        else
	  echo " "
	  echo "Choices: "
	  echo " "
	  echo "  Abort    - give up on this installation and clean up. "
	  echo "  Continue - go on to the next file set.  Installer takes responsibility"
	  echo "             for correcting the transmission errors."
          echo " "
          ask "Abort or Continue?" a c a
          if [ $Ans = "a" ]
          then
            exit 1
          else
            Tardone="Y"
            Tarexit="0"
          fi
        fi
      else
	if [ $Tarexit = "0" ]
	then
	  Tardone="Y"
          rm -f $Error_file
        fi
      fi
    done
#    echo " "
    ##size=0
    Filelist=""
  fi
done
}

if [ "$inst1by1" != "y" ]
then
  section "extracting files from subpackage archives"
fi

Tapen=0

#
# Extract the archives
#

cd $Pkgroot
Havetape=n

Pkgname=`pkginfo pkgname`
Timestamp=`pkginfo timestamp`
set `dates -s $Timestamp`
Stampdate=$2
Stamptime=$3
Stamptz=$4
           
Hostsys=`uname -v`
## LocalFrom is used by pkginfo to determine where a
## package is on a tape.  We just need a legal device.
LocalFrom=${LocalFrom=$From}

if [ "$EnTapehost" != "" ]
then
  LocalFrom=RemoteTape
fi

XtrSubpkgs=$Subpkgs
for Subpkg in $XtrSubpkgs
  do

if [ "$inst1by1" != "y" ]
then
  :
else
TempSubpkgs1=$Subpkgs
Subpkgs=$Subpkg
export Subpkgs
section "Installing subpackage: $Subpkg"
. inst_presv 	#  run preserve -s
. inst_space 	#  check disk space availablility
. inst_stpln 	#  run stripln
section "extracting files from subpackage archives"
Subpkgs=$TempSubpkgs1
export Subpkgs
fi

    case "$Taperw" in
      "") ;;
       *) set `pkginfo position $LocalFrom $Subpkg`
          Spkgvol="$1"; Spkgfile="$2";
          if [ "$Spkgvol" -ne "$Tapen" ]
            then
	      gettape
	      Tapen=$Spkgvol
	      case $Tapen in
		1) case $Os in
	             y) case $Os2 in
                         y) $Dbgecho fsf 20 ;; # id, instd, tpvls, mroot, unixes
                         n) $Dbgecho fsf 4 ;; # past id, instd, tapevol, mroot
                       esac ;;
                     n) $Dbgecho fsf 2 ;; # past id, instd
		     esac ;;
	        *) $Dbgecho fsf 1 ;; # past id
	        esac
	      echo ""
              File=1
            fi
          if [ "$File" -lt "$Spkgfile" ]
            then
              $Dbgecho fsf `expr $Spkgfile - $File`
	      File=$Spkgfile
            fi
	  ;;
      esac

    echo $Dn "Loading subpackage: $Subpkg... $Sc"
    echo "" >$Verbose

    case "$From" in

      en*) Thepackage=`pkginfo bomname $Subpkg`
           Readapkg `su -c $Netuser /usr/ucb/rsh $Server < /dev/null cat $Rpkg/boms/$Thepackage | bom_to_tar` 

### Readapkg does this whole thing...
#            $Dbgecho\
#	     /usr/ucb/rsh \
#	       $Server /usr/pkg/bin/netfeed $Rpkg $Subpkg $Rpkgroot | \
#	         tar xvf - >$Verbose
#	   Tarexit=$?
	   echo ""
	   ;;
      HC|hc|Q120|Q24|Q11|st|pt) \
Retrys=0
Tardone="N"
while [ "$Tardone" = "N" ]
do 
  if [ "$EnTapehost" != "" ]
  then
    $Dbgecho $Tar xvNfb $EnTapehost $Tapenrw 32 >$Verbose
    Tarexit=$?
  else
    $Dbgecho $Tar xvfb $Tapenrw 32 >$Verbose
    Tarexit=$?
  fi
  case $Tarexit in
    0) Tardone="Y" ;;
    *) echo " "
       if [ $Retrys = "3" ]
       then
         echo "Third retry failed."
         Ans="n"
       else
         echo "There was an error during the tar of the last set of files."
         echo " "
         ask "Retry this file set?" y n y
       fi
       if [ $Ans = "y" ]
       then
         Retrys=`expr $Retrys + 1`
         # get to the start of the bad tar archive
         positiontape
         echo $Dn "Retrying the Load of subpackage: $Subpkg... $Sc"
       else
         echo " "
         echo "Choices: "
         echo " "
         echo "  Abort    - give up on this installation and clean up. "
         echo "  Continue - go on to the next file set.  Installer takes responsibility"
         echo "             for correcting problems caused by the tar errors."
         echo " "
         ask "Abort or Continue?" a c a
         if [ $Ans = "a" ]
         then
           ## Abort
           Tardone="Y"
         else
           ## Continue
           # get to the start of the bad tar archive
           positiontape
           # The fsf 1 in the Tarexit case 0 below should
           # get past the bad tar archive (we hope)
           Tardone="Y"
           Tarexit="0"
         fi
       fi
       ;;
  esac
done
	   echo ""
           ;;
        *) $Dbgecho "inst_xtr: Internal error: bad \"From\" value"
	   exit 1
	   ;;
      esac

    case $Tarexit in
      0) case $From in
	   en) ;;
           Q120|Q24|Q11|st|pt) \
  	       $Dbgecho fsf 1
	       File=`expr $File + 1`
	       ;;
 	   esac
	 ;;
      *) echo "
inst_xtr: error extracting archive, tar exit status = $Tarexit

The tar command failed while extracting files from the archive for
subpackage $Subpkg. The installation procedure will be terminated.\n"
echo " "
echo "           WARNING!  The software installed is NOT complete. "
echo "           It is recommended that you retry the installation. "
echo " "
	 echo $Dn "Press return after reading the warning above... $Sc"
	 read Ans
	 exit 1
	 ;;
      esac

    #
    # Now write a record in the install log
    #

    Version=`pkginfo version $Subpkg`
    set `dates -n`
    Instdat=$2
    Insttim=$3
    Insttz=$4
    Insttime=`echo $Insttim | \
      sed -e 's/\([0-9][0-9]:[0-9][0-9]\):[0-9][0-9]/\1/'`

    Logent="$Pkgname.$Subpkg $Version $Timestamp "
    Logent="$Logent ($Stampdate $Stamptime $Stamptz) $Instdat $Insttim $Insttz"

    if [ ! -d $Pkgroot/etc ]
      then
        mkdir $Pkgroot/etc
      fi
	 
    echo $Logent >>$Pkgroot/etc/installlog

if [ "$inst1by1" != "y" ]
then
  :
else
TempSubpkgs1=$Subpkgs
Subpkgs=$Subpkg
export Subpkgs
. inst_mkdev
. inst_compl
. inst_rmold
. inst_restr
section "Done installing subpackage: $Subpkg"
Subpkgs=$TempSubpkgs1
export Subpkgs
fi

  done

case $Havetape in
  y) $Dbgecho rewind ;;
  esac
