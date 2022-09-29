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
# $Header: pkg_miniroot.sh,v 2.2.1.12 90/05/10 04:01:14 wje Exp $
#
if [ "$Pkg" = "" ]
  then
    echo "\$Pkg undefined"
    exit 1
  fi

. pkg_env

#
# This function is like /bin/cp, except that it first tries to find
# the source file under $Pkgroot. If none is found there, it uses /, and
# issues a warning.
#
pcp ()
{
  if [ -f $Pkgroot/$1 ]
    then
      echo "cp $Pkgroot/$1 $2" >$Verbose 2>&1
      cp $Pkgroot/$1 $2
    else
      if [ -f /$1 ]
	then
          echo "warning: using host version \"/$1\""
          echo "cp /$1 $2" >$Verbose 2>&1
          cp /$1 $2
	else
	  echo "warning: no \$Pkgroot/$1 or /$1; no file copied"
	fi
    fi
}

section "creating miniroot filesystem for `pkginfo pkgname`"

case "$Hostsys" in
      *BSD) fsck=fsck
	    mkfs=mkfs
	    mkpdata=mkpdata
	    mkproto=mkproto
	    newfs=newfs
	    ;;
         *) fsck=fsck.ffs
	    mkfs=mkfs.ffs
	    mkpdata=mkpdata.ffs
	    mkproto=mkproto.ffs
	    newfs=newfs.ffs
	    ;;
  esac

echo "Tmp is $Tmp"

Miniroot=${Miniroot=$Pkg/lib/miniroot}
#
# Size of the miniroot filesystem (512 byte blocks)
#
# 27000 ~ 13.5 Mb
case $Hostsys in
      *BSD) Minirootsz=${Minirootsz=12000} ;;
      *) Minirootsz=${Minirootsz=27000} ;;
  esac
Swapsz=${Swapsz=39680}
Swapsz=`expr $Swapsz - $Minirootsz`
Prototmp=${Prototmp=$Tmp/pkg_miniroot.proto.$$}
Mroottmp=${Mroottmp=$Tmp/pkg_miniroot.mroot.$$}

cleanup()
{
  cd $Pkg
  echo $Dn "\ncleaning up... $Sc"
  rm -rf $Mroottmp $Prototmp $Miniroot $Miniroot.ls
  echo ""
}

trap "cleanup; exit 1" 1 2 3 15

# Initialize miniroot filesystem

echo $Dn "initializing miniroot filesystem... $Sc"
echo "" >$Verbose
$mkfs $Miniroot $Minirootsz >$Verbose
echo $Dn "\nchecking initialized miniroot filesystem... $Sc"
echo "" >$Verbose
$fsck -y $Miniroot >$Verbose

# Make miniroot directory tree in a tmp directory
mkdir $Mroottmp

cd $Mroottmp

echo $Dn "\ncopying miniroot files to $Mroottmp... $Sc"
echo "" >$Verbose

#
# The following functions are called for copying in the files which
# are present in both the BSD and V versons of the miniroot. Please
# continue to add such files to these functions. Add files which are
# unique to one system in the "case $Hostsys" statement below.
# 

common_root ()
{
  touch .miniroot
}

common_dirs ()
{
  mkdir bin
  mkdir etc
  mkdir mnt
  mkdir tmp
  mkdir usr
  mkdir usr/ucb
  mkdir sys
  mkdir boms
  mkdir stand
}

common_bin ()
{
  pcp bin/cat bin
  pcp bin/chgrp bin
  pcp bin/chmod bin
  pcp bin/cmp bin
  pcp bin/cp bin
  pcp bin/date bin
  pcp bin/dd bin
  pcp bin/df bin
  pcp bin/du bin
  pcp bin/echo bin
  pcp bin/expr bin
  pcp bin/ln bin
  pcp bin/ls bin
  pcp bin/mkdir bin
  pcp bin/mt bin
  pcp bin/mv bin
  pcp usr/bin/pg bin
  pcp bin/ps bin
  pcp bin/pwd bin
  pcp bin/rm bin
  pcp bin/sed bin
  pcp bin/sh bin
  pcp bin/stty bin
  pcp bin/sync bin
  pcp bin/tar bin
  pcp bin/tee bin
  pcp usr/bin/tr bin
  pcp usr/ucb/rsh usr/ucb
  pcp usr/ucb/rlogin usr/ucb
  pcp bin/uname bin
  pcp usr/ucb/uncompress usr/ucb
}

common_etc ()
{
  pcp etc/disktab etc
  pcp etc/dvhtool etc
  pcp etc/$fsck etc
  pcp etc/fstab etc/fstab.conf
  pcp etc/group etc
  pcp etc/hosts etc
  pcp etc/$newfs etc
  pcp etc/$mkfs etc
  pcp etc/mknod etc
  pcp etc/mount etc
  pcp etc/passwd etc
  pcp etc/services etc
  pcp etc/sysconinit etc
  pcp etc/umount etc
  pcp etc/restore etc
  pcp etc/restore.ffs etc
}

common_stand ()
{
#  if [ "$KernelOnMiniroot" != "y" ]
#  then
    pcp stand/sash.std stand
    pcp stand/format.std stand
    pcp stand/sash.2030 stand
    pcp stand/format.2030 stand
#  else
#    pcp stand/sash stand
#    pcp stand/format stand
#  fi
}

case $Hostsys in

      *BSD) pcp vmunix.gen vmunix
	    common_root
	    cat >.profile <<EOFFOE
umask 022
PATH=/usr/net:/bin:/etc:/usr/bin:/usr/etc:/usr/ucb:/usr/new ; export PATH

stty erase '^H' kill '^U' intr '^C' crt
echo "erase=^H, kill=^U, interrupt=^C"

EOFFOE

	    # Create a skeleton tree
	    common_dirs
	
	    # Now copy the miniroot command binaries into the tree

	    # bin
	    common_bin
	    pcp bin/awk bin
	    pcp bin/ed.int bin/ed
	    pcp bin/test bin
	    pcp bin/rcp bin
	    pcp bin/hostname bin
	    pcp bin/hostid bin
	    pcp usr/lib/grep_cmds/grep bin
	    pcp usr/bin/basename bin
	    pcp usr/bin/sleep bin
	    pcp usr/ucb/wc usr/ucb
	    strip bin/* >/dev/null 2>&1
	
	    # etc
	    common_etc
	    pcp etc/chown etc
	    pcp etc/dump etc
	    pcp etc/disktab etc
            pcp etc/fsirand etc
            cat >etc/fstab <<EOFFOE
/dev/dsk/ip0a / 4.3 rw 0 0
EOFFOE
	    pcp etc/halt etc
	    pcp etc/ifconfig etc
	    pcp etc/inetd etc
	    pcp etc/inetd.conf etc
	    pcp etc/init etc
	    if [ -f $Pkgroot/etc/mtab ] 
	      then
	        pcp etc/mtab etc
	      fi
	    pcp etc/restore etc
	    pcp etc/route etc
	    pcp etc/routed etc
	    strip etc/* >/dev/null 2>&1
	
	    # stand
	    common_stand
	    strip stand/* >/dev/null 2>&1
	    ;;

       *) 
#	    pcp unix.r2300_std unix.r2300_std
#	    pcp unix.r2400_std unix.r2400_std
#	    pcp unix.r3200_std unix.r3200_std
#	    pcp unix.i2000_std unix.i2000_std
	    common_root
	    cat >.profile <<EOFFOE
umask 022
PATH=/usr/net:/bin:/etc:/usr/bin:/usr/etc:/usr/ucb:/usr/new ; export PATH
TZ=PST8PDT ; export TZ
set -a

stty line 1 erase '^H' kill '^U' intr '^C' echoe 
echo "erase=^H, kill=^U, interrupt=^C"

EOFFOE

	    # Create a skeleton tree
	    common_dirs
	
	    # bin
	    common_bin
	    pcp bin/basename bin
	    pcp bin/chown bin
	    pcp bin/cpio bin
	    pcp bin/ed bin
	    pcp bin/false bin
	    pcp bin/find bin
            pcp bin/grep bin
	    pcp bin/kill bin
	    pcp bin/rmdir bin
	    ln bin/sh bin/-sh
	    pcp bin/sleep bin
	    pcp bin/sort bin
	    pcp bin/su bin
	    pcp bin/touch bin
	    pcp bin/wc bin
	    pcp usr/bin/awk bin
	    pcp usr/etc/ifconfig etc
	    pcp usr/etc/inetd etc
	    pcp usr/etc/route etc
	    pcp usr/etc/routed etc
	    pcp usr/ucb/rcp usr/ucb
	    pcp usr/ucb/hostname usr/ucb
	    pcp usr/ucb/hostid usr/ucb
	    pcp usr/new/multivol bin
	    strip bin/* >/dev/null 2>&1
	
	    # etc
	    common_etc
	    pcp etc/devnm etc
	    pcp etc/dumpfs.ffs etc
	    pcp etc/kopt etc
	    pcp etc/tunefs.ffs etc
	    pcp etc/fsirand.ffs etc
	    cat >etc/fstab <<EOFFOE
/dev/swap / ffs rw 0 0
EOFFOE
#
#  Just making an entry for /dev/swap should work generically; Am leaving
#  this stuff in here (commented out) just in case it doesn't, but it
#  can probably be removed once we know /dev/swap works. - rmg 4/23/88
#
#            case $Diskcont in
#	      ip*) cat >etc/fstab <<EOFFOE
#/dev/dsk/ipc0d0s1 / ffs rw 0 0
#EOFFOE
#	           ;;
#	      in*) cat >etc/fstab <<EOFFOE
#/dev/dsk/int0d0s1 / ffs rw 0 0
#EOFFOE
#	           ;;
#	      esac
	    # pcp etc/fsck1b etc
	    pcp etc/init etc
	    ln etc/init etc/telinit
	    cat >etc/inittab <<EOFFOE
#
# Miniroot /etc/inittab
#
is:1:initdefault:
sc:1:wait: /etc/sysconinit 
r1:1:wait:/etc/rc1 0</dev/console 1>/dev/console 2>&1
sd:1:wait: /etc/sysconinit 
s1:1:respawn:/bin/-sh 0</dev/console 1>/dev/console 2>&1
r0:0:wait:/etc/rc0 0</dev/console 1>/dev/console 2>&1
of:0:wait:/etc/uadmin 2 0 0</dev/console 1>/dev/console 2>&1
EOFFOE
	    if [ ! -d usr/etc ]
              then
	        mkdir -p usr/etc
	      fi
	    pcp usr/etc/inetd.conf usr/etc
	    pcp etc/killall etc
	    pcp etc/link etc
	    touch etc/mtab
	    pcp etc/mvdir etc
	    pcp etc/prtvtoc etc
	    cat >etc/rc0 <<EOFFOE
#!/bin/sh
/etc/umount -av
echo "\nMiniroot shutdown\n"
EOFFOE
	    chmod +x etc/rc0
	    cat >etc/rc1 <<EOFFOE
#!/bin/sh
echo "\nMiniroot run level 1\n"
## These link commands don't work on 4.50 since kernel is not there
#case \`uname -t\` in
#    rs2030|rc2030) ln /unix.i2000_std /unix >/dev/null 2>&1 ;;
#     RC3240|rc3240|m120*) ln /unix.r2400_std /unix >/dev/null 2>&1 ;;
#	   m2000*) ln /unix.r3200_std /unix >/dev/null 2>&1 ;;
#  m1000|m800|m500) ln /unix.r2300_std /unix >/dev/null 2>&1 ;;
#  esac

if [ ! -f /dev/HAVEDEVS ]
  then
    echo "Making miniroot device files for \`uname -t\` system...\c"
    cd /dev
    if [ -x ./mkdevcmd ]
      then
	./MKDEV -p -n | ./mkdevcmd >/dev/null 2>&1
      else
        ./MKDEV
      fi
    echo
    touch /dev/HAVEDEVS
  fi
[ -f /etc/mtab ] && rm /etc/mtab
/bin/touch /etc/mtab
mount -f /
#/etc/swap -a /dev/swap $Minirootsz $Swapsz
#/etc/swap -d /dev/swap 0
EOFFOE
	    chmod +x etc/rc1
	    pcp etc/swap etc
	    pcp etc/uadmin etc
	    pcp etc/unlink etc
	    pcp etc/umountall etc
	    strip etc/* >/dev/null 2>&1
	
	    # stand
	    common_stand
	    strip stand/* >/dev/null 2>&1
	    ;; #

    esac

#
# If $Nopkg is set, we will not do the packaging tools & information
# tree. This may prove useful in instances where a miniroot is needed
# for other purposes
#
case "$Nopkg" in
  "") # copy in the installation tools and packaging information tree

      pcp usr/pkg/bin/inst bin
      pcp usr/pkg/bin/bom_to_tar bin

      Pkgname=`pkginfo pkgname`
      Pkgnamever=$Pkgname`pkginfo version $Pkgname`
      echo $Dn "${Pkgnamever}$Sc" >$Mroottmp/Pkgname
      mkdir $Pkgnamever
      cd $Pkgnamever
      . pkg_cpinstd

      cd $Mroottmp

      if [ -r $Pkg/lib/miniroot_insert ]
        then
          . $Pkg/lib/miniroot_insert
        fi
      ;;
  esac

# make miniroot devices
mkdir dev

case "$Hostsys" in
      *BSD) pcp dev/MAKEDEV dev
	    chmod +x dev/MAKEDEV
	    cp /dev/null dev/MAKEDEV.local
	    cd dev
	    ./MAKEDEV std ip0 st0 PT0 rd0 cp0 pty0 local
	    cd ..
	    ;;
         *) pcp dev/MKDEV dev
	    pcp usr/pkg/bin/mkdevcmd dev
	    strip dev/mkdevcmd >/dev/null 2>&1
	    chmod +x dev/MKDEV
	    cd dev
	    mkdir DEV_DB
	    pcp dev/DEV_DB/* DEV_DB
	    if [ -x ./mkdev ]
	      then
		./MKDEV -m common -p -n | ./mkdevcmd >/dev/null 2>&1
	      else
	        ./MKDEV -m common
	      fi
	    #
	    # This is mainly because, apparently, the sh started from inittab
	    # for run level 1 doesn't have a control terminal for some reason,
	    # so you can't open the "real" /dev/tty. (?)
	    #
	    rm tty
	    ln console tty
	    cd ..
	    ;;
    esac

echo $Dn "\nwriting $Miniroot.ls... $Sc"
ls -lRFia >$Miniroot.ls

echo $Dn "\nbuilding prototype file... $Sc"
$mkpdata > $Prototmp
echo $Dn "\nconfiguring the miniroot... $Sc"
$mkproto $Miniroot $Prototmp

echo $Dn "\nrechecking the miniroot (first pass should take repairs)... $Sc"
echo "" >$Verbose
$fsck -y $Miniroot >$Verbose

echo "\nrechecking the miniroot (should be clean)..."
$fsck -y $Miniroot

case "$Inspectmroot" in
  y) cd $Mroottmp; 
     echo "You will now be given a subshell to inspect the miniroot..."
     sh
     ;;
  esac

echo $Dn "cleaning up... $Sc"
cd $Pkg
rm -rf $Mroottmp $Prototmp
echo "\ndone making miniroot\n"
