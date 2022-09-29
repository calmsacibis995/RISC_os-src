#! /bin/sh
#
# rm-links.sh -- remove symbolic links for emacs source files
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

	for name in `(cd "$destdir" ; find . -type l ! -name RCS ! -name .SRCS_CO ! -name .SRCS_BRANCH -print )` 
	{
		if [ -r "$sourcedir"/"$name" ] && \
		   cmp "$destdir"/"$name" "$sourcedir"/"$name" > /dev/null   
		then
			namecmd="rm -f $destdir"/"$name"
			echo "$namecmd"
			eval "$namecmd"
		fi
	}

}


main ${1+"$@"}
exit 0
