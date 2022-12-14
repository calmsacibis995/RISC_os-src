#! /bin/sh
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
# $Header: uupick.sh,v 1.4.2.2 90/05/10 00:45:13 wje Exp $
# #ident	"@(#)uucp:uupick	2.4"

# sys: system; user: login name;  cdir: current directory;
# tdir: temporary directory; pu: PUBDIR/receive/user;
cdir=`pwd`
dir=""
abs=""
sys=""
var=""
varto=""
varfrom=""
trap "exit" 1 2 13 15
# get options
if test $# -gt 2
then echo "Usage: $0 [-s sysname]"; exit
fi
while test $# -ge 1
do
	case $1 in
	 -s*) 	if [ $# -eq 1 ]
		then
			sys=`expr $1 : '-s\(.*\)'`
		else
			sys=$2
			shift
		fi
		;;
	 *)    echo "Usage: $0 [-s sysname]"; exit
	esac
	shift
done
user=`id | sed -n "/^uid=[0-9]*(\([^)]*\)).*/s//\1/p"`

if test -z "$user"
then echo "User id required!"; exit
fi

pu=/usr/spool/uucppublic/receive/$user
if test -d $pu -a -s $pu
then
for i in `ls $pu`
do
	if test $sys
	then	if test $sys != $i;  then continue;  fi
	fi
	if test -d $pu/$i -a -s $pu/$i
	then
		cd $pu/$i
		for j in `ls -a`
		do
			if test $j = "." -o $j = ".."; then continue; fi
			if test -d $j
			then echo "from system $i: directory $j"
			else echo "from system $i: file $j"
			fi
			while true
			do
			  echo '?'
			  if read cmd dir
			  then
				trap ": ;;" 1
				case $cmd in
				 d)	rm -fr $j ; break ;;
				 "")	break ;;
#options m, a: if dir path
#begins with a slash, use full
#path for destination;otherwise,
#use path relative to current dir;
#default destination is current dir
			         m)	eval dir="$dir"
					if test $dir
				 	then abs=`expr "$dir" : '/.*'`
						if test $abs != 0
						then tdir=$dir
 						elif test -d "$cdir/$dir"
						then tdir=$cdir/$dir
						fi
					else
						tdir=$cdir
					fi
					find $j -print |cpio -pdmuv $tdir \
					1>> /tmp/$$uupick	
					for k in `cat /tmp/$$uupick`
					do
					    varto="$k"
					    var=`echo $k |sed -n "s;^$tdir/;;p"`
					    varfrom="$pu/$i/$var"
					    cmp $varfrom $varto
					    if test $? -eq 0; then
					    	rm -fr $varfrom
					    fi
					done
					rm -f /tmp/$$uupick
					break;; 
#As files are transferred,
#put their names in /tmp/$$uupick.
#Only remove those named files
#from...receive/..dir if cmp
#verifies transfer took place
				 a)	eval dir="$dir"
					if test $dir
					then abs=`expr "$dir" : '/.*'`
						if test $abs != 0
							then tdir=$dir
						elif test -d "$cdir/$dir"
							then tdir=$cdir/$dir
						fi
					else
						tdir=$cdir
					fi
					find * -print |\
					cpio -pdmuv $tdir 1>> /tmp/$$uupick
					for k in `cat /tmp/$$uupick`
					do
					    varto="$k"
					    var=`echo $k |sed -n "s;^$tdir/;;p"`
					    varfrom="$pu/$i/$var"
					    cmp $varfrom $varto
					    if test $? -eq 0; then
					     	rm -fr $varfrom
					    fi
					done
					rm -f /tmp/$$uupick
					break 2 ;;
				 p)	if test -d $j
					then find . -print
					elif test -s $j 
				        then cat $j
					fi ;;
				 q)	break 3 ;;
				 !*)	ex=`expr "$cmd $dir" : '!\(.*\)'`
					tdir=`pwd`
					cd $cdir
					sh -c "$ex"
					cd $tdir
					echo '!' ;;
				 *)	echo "usage: [d][m dir][a dir][p][q]\c"
					echo "[cntl-d][!cmd][*][new-line]" ;;
				esac
				trap 1
			  else	break 3
			  fi
			done
		done
	fi
done
fi
