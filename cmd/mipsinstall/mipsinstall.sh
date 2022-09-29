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
# $Header: mipsinstall.sh,v 1.10.1.2 90/05/09 16:46:46 wje Exp $
#
# mipinstall - install files like BSD
#
PATH=/bin:/usr/bin
Myname=`basename $0`
cmd=""
stripit=""
chmod="chmod 755"
chown="chown root"
chgrp="chgrp staff"
hlinks=""
slinks=""
type="dir"
Usage="$Myname: usage: $Myname [-L link] [-l link] [-s] [-c] [-f] [-m mode] [-o owner] [-g group] source destination"

main() {
	while :
	do
		case "$1" in
			-L )	hlinks="$hlinks \"$2\""
				shift
				shift
				;;
			-l )	slinks="$slinks \"$2\""
				shift
				shift
				;;
			-s )	stripit="strip"
				shift
				;;
			-c )	case "$cmd" in
					"")
						;;
					*)
						echo "$Myname: multiple specifications of -c"
						exit 1
						;;
				esac
				cmd="cp"
				shift
				;;
			-m )	chmod="chmod $2"
				shift
				shift
				;;
			-o )	chown="chown $2"
				shift
				shift
				;;
			-g )	chgrp="chgrp $2"
				shift
				shift
				;;
			-f )	type="file"
				shift
				;;
			-* )	echo "$Usage"
				exit 1
				;;
			
			*)	break
				;;
		esac
	done

	case "$cmd" in
		"")
			cmd="mv"
			;;
	esac

	case "$#" in
		0)
			echo "$Usage"
			exit 1
			;;

		1)
			echo "$Myname: no destination specified"
			exit 1
			;;
		3)
			echo "$Myname: too many files specified -> $*"
			exit 1
			;;
	esac

	case "$2" in
		"$1"|".")
			echo "$Myname: can't move $1 onto itself"
			exit 1
			;;
	esac
	if [ ! -f "$1" ]
	then
		echo "$Myname: file $1 does not exist"
		exit 1
	fi

	case "$type" in
		"file")
			destdir=`dirname "$2"`
			;;
		*)
			destdir="$2"
			;;
	esac
	if [ ! -d "$destdir" -a ! -f "$destdir" ]
	then
		echo "$Myname: making directory $destdir"
		mkdir -p "$destdir"
	fi

	if [ -d "$2" ]
	then
		file="$2/`basename $1`"
	else
		file="$2"
	fi

	remove_file "$file"


	$cmd "$1" "$file"

	case "$stripit" in
		"")
			;;
		*)
			$stripit "$file"
			;;
	esac

	$chown "$file" >/dev/null 2>&1
	$chgrp "$file" >/dev/null 2>&1
	$chmod "$file" >/dev/null 2>&1

	if [ -f "$2" ]
	then
		dir=`echo "$2" | sed 's,/[^/]*$,,'`
	else
		dir="$2"
	fi

	case "$slinks" in
		"")
			;;
		
		*)
			eval set $slinks
			for f in "$@"
			{
				if [ `echo $f | sed 's/\(.\).*/\1/'` = "/" ]
				then	
					echo "Creating symbolic link from $f to" `basename "$file"`
					remove_file "$f"
					ln -s `combine_names "$file" "$f"`
				else	
					echo "Creating symbolic link from $dir/$f to" `basename "$file"`
					remove_file "$dir/$f"
					ln -s `"$file" "$f"`
				fi
			}
			;;
	esac

	case "$hlinks" in
		"")
			;;
		
		*)
			eval set $hlinks
			for f in "$@"
			{
				if [ `echo $f | sed 's/\(.\).*/\1/'` = "/" ]
				then	
					echo "Creating hard link from $f to" `basename "$file"`
					remove_file "$f"
					ln "$file" "$f"
				else	
					echo "Creating hard link from $dir/$f to" `basename "$file"`
					remove_file "$dir/$f"
					ln "$file" "$f"
				fi
			}
			;;
	esac
}

#
# remove_file() removes the named file.  If it can't be removed, the
# file is moved to #{file}.{year}{month}{day}{hour}{minute}{second}.
#

remove_file()
{
	rm -f "$1" >/dev/null 2>&1
	if [ ! -f "$1" ]
	then
		return
	fi

	rf_dir=`dirname "$1"`
	rf_base=`basename "$1"`
	rf_nname="$rf_dir/#$rf_base.`date +'%y%m%d%H%M%S'`"
	echo "$Myname: Couldn't remove file.  Moved to $rf_nname"

	mv "$1" "$rf_nname"
}



#
# getnth() gets the i'th element of the remaining list of arguments
#

getnth() {
	if [ $# -lt 2 ]
	then
		echo ""
	else
		count="$1"
		shift
		if [ $count -gt $# ] 
		then
			echo ""
		elif [ $count -lt 10 ]
		then
			eval echo \$$count
		else
			while [ $count -ge 10 ]
			do
				count=`expr $count - 1`
				shift
			done
			eval echo \$$count
		fi
	fi
}

combine_names() {
	targetname=`basename $1`
	targetpath=`dirname $1`
	targetpath=`(cd $targetpath; /bin/pwd | sed -e 's"/" "g')`
	sourcename=`basename $2`
	sourcepath=`dirname $2`
	sourcepath=`(cd $sourcepath; /bin/pwd | sed -e 's"/" "g')`

	index=1
	while true
	do
		targetitem=`getnth $index $targetpath`
		sourceitem=`getnth $index $sourcepath`
		index=`expr $index + 1`
		if [ "$targetitem" != "$sourceitem" ] || [ "$targetitem" = "" ] 
		then
			break;
		fi
	done

	sourceindex=$index
	newpath=""
	while [ "$targetitem" != "" ]
	do
		if [ "$newpath" = "" ]
		then
			newpath="$targetitem"
		else
			newpath="$newpath/$targetitem"
		fi
		targetitem=`getnth $index $targetpath`
		index=`expr $index + 1`
	done
	if [ "$newpath" != "" ]
	then
		newpath="$newpath/$targetname";
	else
		newpath="$targetname";
	fi

	index=$sourceindex
	while [ "$sourceitem" != "" ]
	do
		if [ "$newpath" = "" ]
		then
			newpath=".."
		else
			newpath="../$newpath"
		fi
		sourceitem=`getnth "$index" $sourcepath`
		index=`expr $index + 1`
	done

	echo "$newpath" "$2"
	exit 0
}

main ${1+"$@"}
exit 0
