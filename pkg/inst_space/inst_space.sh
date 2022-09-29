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
# $Header: inst_space.sh,v 2.8.1.3.1.1.1.2 90/12/20 19:20:53 beacker Exp $
#
case "$Instenv" in
  "") . inst_env ;;
esac

dospace()
{
  echo "\
The system will now be checked to verify that there is enough disk
space with the current configuration to successfully install the package
(and any selected optional subpackages). For large packages (especially
operating system packages), this can be time consuming...\n"
    
  Sizes=""
  for Subpkg in $Subpkgs
    do
      Sizes="$Sizes $Pkg/boms/`pkginfo bomname $Subpkg`.sizes"
    done

  if [ -f $Pkg/boms/reserve.sizes ]
    then
      Sizes="$Sizes $Pkg/boms/reserve.sizes"
    fi
  cd $Pkgroot

# Make sure that there are no duplicate entries
  cat $Sizes | sort -u > /tmp/uniq$$.tmp

  Smargin=${Smargin=0.03}
  space -m $Smargin -d /tmp/uniq$$.tmp > /tmp/$$.tmp 2>&1
  myexit=$?
  case $myexit in 
    9) cat /tmp/$$.tmp
       ask "Abort the installation?" y y n
       case $Ans in
         n) myexit=0
            echo " "
            echo "Checking for disk fragmentation..."
            echo " "
            exec 0</tmp/$$.tmp
            ignore=y
            while read line
            do
              Fragmented=n
              set aaa $line dummy
              diskname=$2
              if [ "$ignore" = "n" ]
              then
                case $diskname in
                  dummy) ignore=y ;;
                  *) rootdir=`df $diskname | grep -v Filesystem | sed "s/.* //`
                     ## blocks required - blocks credited
                     needsize=`expr $5 - $7`

nnn=`dumpfs.ffs $diskname | grep "^nbfree" | sed '1d' | grep -v "nbfree	0" | \
sed "s/nbfree	//" | sed "s/	.*//" | sed "s/ .*//"`
##dumpfs.ffs $diskname | grep "^nbfree" | sed '1d' | grep -v "nbfree	0" | \
##sed "s/nbfree	//" | sed "s/	.*//" | sed "s/ .*//"
bfree=0
for number in 0 $nnn
do
  bfree=`expr $bfree + $number`
done
rm -f /tmp/bfree.$$
## Sometimes can't use last 1%
bfree=`expr $bfree \* 99`
bfree=`expr $bfree \/ 100`
## Multiply nbfree by 8 to get free blocks
Kfree=`expr $bfree \* 8`

                     if [ $Kfree -gt $needsize ]
                     then
                       echo "Filesystem $diskname is okay."
                     else
                       Bshort=`expr $needsize - $Kfree`
                       ## add one k to be safe
                       Bshort=`expr $Bshort + 1`
                       Fragmented=y
                     fi
                     if [ "$Fragmented" = "y" ]
                     then
                       echo " "
                       echo "***************************************************************"
                       echo "* CAUTION!  There are not enough free blocks available on"
                       echo "* $diskname due to disk fragmentation.  Short $Bshort kbytes."
##                       echo "* "
                       echo "* Please create more free space on $rootdir or use"
                       echo "* dump, newfs, and restore to lessen the disk fragmentation."
                       echo "***************************************************************"
                       echo " "
                       myexit=1
                     fi ;;
                esac
              fi
              case $diskname in
                device*) ignore=n ;;
                dummy)   ignore=y ;;
              esac
            done
            exec 0</dev/tty
            echo " "
            echo " " > /tmp/$$.tmp ;;
         y) echo " " > /tmp/$$.tmp ;;
       esac ;;
  esac
  case $myexit in 
    0) echo "There is enough space." ;;
    *) cat /tmp/$$.tmp
       if grep fstabind /tmp/$$.tmp >/dev/null
       then
         
echo " "
echo "***** IMPORTANT *****"
echo " "
echo "The pathname listed above probably includes a link to another filesystem."
echo "To complete this install, you must rename (mv) that link before starting the"
echo "install and then move the link back into place after the install is complete. "
echo "Please check the Release Notes for details. "
echo " "
       fi
echo $Dn "
The package (and any selected optional subpackages) cannot be installed
at this time due to the shortage(s) of disk space and/or inodes shown
above. Please make the required space available and then retry the 
installation procedure.

(Press return to continue...) $Sc"
       read Ans </dev/tty
       rm -f /tmp/$$.tmp
       rm -f /tmp/uniq$$.tmp
       exit 1
       ;;
  esac
  rm -f /tmp/$$.tmp

}
    
section "verifying disk space"
if [ "$inst1by1" = "y" ]
then
  Ans=y
else
echo "Do you want to check for space (please do so unless you really"
ask "understand the consequences) " y y n
fi
case $Ans in
    y) dospace;;
esac
rm -f /tmp/uniq$$.tmp
