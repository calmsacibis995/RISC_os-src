#! /bin/sh
#
# make-links.sh -- create symbolic links for emacs source files
#
PATH=/bin:/usr/bin
Myname=`basename $0`
Usage="$Myname: usage: $Myname sourcedir destdir"

main() {
	case "$#" in
		0)
			echo "$Usage"
			exit 1
			;;
		1)
			echo "$Myname: no destination specified"
			exit 1
			;;
		2)
			;;
		*)
			echo "$Myname: too many files specified -> $*"
			exit 1
			;;
	esac

	sourcedir="$1"
	destdir="$2"
	if [ ! -d "$sourcedir" ]
	then
		echo "$Myname: source directory $1 does not exist"
		exit 1
	fi
	if [ ! -d "$destdir" ]
	then
		echo "$Myname: destination directory $1 does not exist"
		exit 1
	fi

	for name in `(cd $sourcedir ; find . \( -type f -o -type l \) ! -name RCS ! -name .SRCS_CO ! -name .SRCS_BRANCH -print )` 
	{
		if [ ! -r "$destdir"/"$name" ]
		then
			namepair=`combine_names "$sourcedir"/"$name" "$destdir"/"$name"`
			namecmd="ln -s $namepair"
			echo "$namecmd"
			eval "$namecmd"
		fi
	}

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
