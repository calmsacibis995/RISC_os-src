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
# $Header: install.sh,v 1.9.2.2 90/05/09 16:16:25 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.

#
# Note: File and directory name arugments can be either absolute or relative.
#	If absolute, then they get prefixed by $ROOT;
#	if relative, then not.

# BUGS: 
#	> -dir and -F don't work together yet.
#

# added "-F dir" - Fancy new setup to do whatever brendon wants it to.  
#
# Dir is a directory which exists and where stuff should be installed.
# The file argument is a file or relative path.
# If file, then it gets installed in dir.  
# If path, then dir part of path gets created under dir and file part
#   is installed there.
#

# added "-ln nm" to make a hard link instead of copying
#	"-lns nm" to make a symbolic link
#       "-src nm" to use 'nm' as the source path
#	"-dir" to make a directory instead of a file

#	INSTALL COMMAND

LINKOPT=""
CP="cp"
DIR="n"

FLIST=$ROOT/etc/syslist
DEFAULT="$ROOT/bin $ROOT/usr/bin $ROOT/etc $ROOT/lib $ROOT/usr/lib" 
FOUND="" 
MOVOLD=""
ECHO=echo 
PATH=/bin:/etc:/usr/bin 
FLAG=off
USAGE="eval echo 'usage: install [options] file [dir1 ...]' 1>&2; exit 2"

MODE=755
GROUP=`expr "\`id\`" : ".*(\(.*\)).*"`
OWNER=`expr "\`id\`" : ".*(\(.*\)) "`
if [ $OWNER = root ]
then
	ROOTFLAG=on
	OWNER=bin
	GROUP=bin
else
	ROOTFLAG=off
fi

#
# Parse flags.
#
for i in $*
do
	if [ $FLAG = on ]
	then
		case $i in
		    -*) echo "install: The -c, -f, -F, -n options each require a directory following!" 1>&2
			exit 2;;
		     *) FLAG=off
			continue;;
		esac
	fi
	case $i in
	    -ln)
		LINKOPT=yes
		CP="ln"
		FILEP="$2"
		FLAG=on
		shift
		;;
	    -lns)
		LINKOPT=yes 
		CP="ln -s"
		FILEP="$2"
		FLAG=on
		shift; shift
		;;
	    -src)
	    	if [ $# -lt 2 ]
		then
			$USAGE
		fi
		FILEP="$2"
		FLAG=on
		shift; shift
		;;
	    -dir)
		DIR="y"
		shift
		;;

	    -c) if [ x$ARG = x-F -o x$ARG = x-f -o x$arg = x-i -o x$arg = x-o -o x$arg = x-n ]
		then
			echo "install: -c dir: illegal option with ${arg-"-f"} option!" 1>&2
			exit 2
		elif test $# -lt 3
		then
			echo "install: -c option must have at least 3 args!" 1>&2
			exit 2
		else
			DIRECT=$2
			FLAG=on
			ARG=-c
			shift; shift
		fi;;
	    -f) if [ x$ARG = x-F -o x$ARG = x-c -o x$arg = x-i -o x$arg = x-n ]
		then
			echo "install: -f dir: illegal option with ${arg-"-c"} option!" 1>&2
			exit 2
		elif test $# -lt 3
		then
			echo "install: -f option must have at least 3 args!" 1>&2
			exit 2
		else
			DIRECT=$2
			FLAG=on
			ARG=-f
			shift; shift
		fi;;
	    -F) if [ x$ARG = x-f -o x$ARG = x-c -o x$arg = x-i -o x$arg = x-n ]
		then
			echo "install: -f dir: illegal option with ${arg-"-c"} option!" 1>&2
			exit 2
		elif test $# -lt 3
		then
			echo "install: -f option must have at least 3 args!" 1>&2
			exit 2
		else
			DIRECT=$2
			FLAG=on
			ARG=-F
			shift; shift
		fi;;
	  -i) if [ x$ARG  = x-c -o x$ARG = x-f ]
		then
			echo "install: -i: illegal option with $ARG option!" 1>&2
			exit 2
		elif test $# -lt 3
		then
			echo "install: -i option requires at least 3 args!" 1>&2
			exit 2
		else
			DEFAULT=""
			arg=-i
			shift
		fi;;
	    -o) if  [ x$ARG = x-c ]
		then
			echo "install: -o: illegal option with $ARG option!" 1>&2
			exit 2
		elif test $# -lt 2
		then
			$USAGE
		else
			MOVOLD=yes
			arg=-o
			shift
		fi;;
	    -n) if [ x$ARG = x-F -o  x$ARG = x-c -o x$ARG = x-f ]
		then
			echo "install: -n dir: illegal option with $ARG option!" 1>&2
			exit 2
		elif test $# -lt 3
		then
			echo "install: -n option requires at least 3 args!" 1>&2
			exit 2
		else
			LASTRES=$2
			FLAG=on
			FOUND=n
			arg=-n
			shift; shift
		fi;;
	    -s) if test $# -lt 2
		then
			$USAGE
		else
			ECHO=:
			arg=-s
			shift
		fi;;
	    -u) if [ $ROOTFLAG = off ]
		then
			echo "install: -u option available only to root -- ignored" 1>&2
		else
			OWNER=$2
			echo new owner is $OWNER
		fi
		FLAG=on
		shift; shift;;
	    -g) if [ $ROOTFLAG = off ]
		then
			echo "install: -g option available only to root -- ignored" 1>&2
		else
			GROUP=$2
		fi
		FLAG=on
		shift; shift;;
	    -m)	MODE=$2
		FLAG=on
		shift; shift;;
	    -*)
		$USAGE
		;;
	     *) break;;
	esac
done

if [ "$1" = "" -a "$FILEP" = "" ]
then
	echo "nothing to install"
	exit 1
fi

FILEP=${FILEP-$1}
FILE=`basename "$FILEP"`
DIRECT=${DIRECT-}

#
# just create the directory
# (This needs to coordinate with -F.)
#
if [ "$DIR" = "y" ] 
then
	if [ `expr "$FILEP" : "/.*"` -gt 0 ]
	then
		# Prefix $ROOT only for absolute paths.
		FILEP=$ROOT/$FILEP
	fi
	if [ ! -d $FILEP ] 
	then
		mkdir $FILEP
	fi
	chmod $MODE $FILEP
	chown $OWNER $FILEP
	chgrp $GROUP $FILEP
	$ECHO "$FILEP created"
	exit 0
fi


#
# Handle installation in specified directory. (-c and -f)
#
if [ x$ARG = x-c -o x$ARG = x-f ]
then
	if [ `expr "$DIRECT" : "/.*"` -gt 0 ]
	then
		# Prefix $ROOT only for absolute paths.
		DIRECT=$ROOT/$DIRECT
	fi

	case $2 in
		-*) $USAGE ;;
		"") :	;;
	esac
	if [ -f $DIRECT/$FILE -o -f $DIRECT/$FILE/$FILE ]
	then
		case $ARG in
			-c) echo "install: $FILE already exists in $DIRECT"
			    exit 2;;
			-f) GROUP=`ls -l $DIRECT/$FILE | awk '{print $4}'`
			    OWNER=`ls -l $DIRECT/$FILE | awk '{print $3}'`
			    if [ -k $DIRECT/$FILE ]
			    then
				chmod -t $DIRECT/$FILE
				$DIRECT/$FILE < /dev/null > /dev/null
				tbit=on
			    fi
			    if [ "$MOVOLD" = yes ]
			    then
				mv -f $DIRECT/$FILE $DIRECT/OLD$FILE
				cp $DIRECT/OLD$FILE $DIRECT/$FILE
				if [ $? = 0 ]
				then
				   $ECHO "$FILE moved to $DIRECT/OLD$FILE"
				   chgrp $GROUP $DIRECT/$FILE
				   chown $OWNER $DIRECT/$FILE
				else
				   echo "install: cp $DIRECT/OLD$FILE $DIRECT/$FILE failed" 1>&2
				   exit 2
				fi
			    fi
			    if test "$CP" != "cp"; then rm -f $DIRECT/$FILE; fi
			    if $CP $FILEP $DIRECT/$FILE
			    then
				$ECHO "$FILEP installed as $DIRECT/$FILE"
			    fi
			    if [ "$tbit" = on ]
			    then
				chmod +t $DIRECT/$FILE
			    fi
			    exit;;
		esac
	else
		if test "$CP" != "cp"; then rm -f $DIRECT/$FILE; fi
		$CP $FILEP $DIRECT/$FILE
		if [ $? = 0 ]
		then
			$ECHO "$FILEP installed as $DIRECT/$FILE"
			chgrp $GROUP $DIRECT/$FILE
			chown $OWNER $DIRECT/$FILE
			chmod $MODE $DIRECT/$FILE
		fi
	fi
	exit
fi

#
# DWIM.
#
# See -F comment at start of code.
#
if [ x$ARG = x-F ]
then
	if [ `expr "$FILEP" : "/.*"` -gt 0 ]
	then
		echo "$0: Installing absolute pathname file with -F flag" 1>&2
		echo "    probably won't do what you expect." 1>&2
		exit 1;
	fi

	if [ `expr "$DIRECT" : "/.*"` -gt 0 ]
	then
		# Prefix $ROOT only for absolute paths.
		DIRECT=$ROOT/$DIRECT
	fi

	if [ ! -d $DIRECT ] 
	then
		echo "$0: Installation directory $DIRECT does not exist" 1>&2
		exit 1
	fi

	# Create required directories for the target file.
	TMP=$DIRECT
	for I in `dirname $FILEP | tr '/' ' '`
	do
		TMP=$TMP/$I
		if [ ! -d $TMP ]
		then
			echo "creating $TMP"
			mkdir $TMP || exit 1;
		fi
	done

	rm -f $DIRECT/$FILE
	$CP $FILEP $DIRECT/$FILEP
	if [ $? = 0 ]
	then
		$ECHO "$FILEP installed as $DIRECT/$FILEP"
		chgrp $GROUP $DIRECT/$FILEP
		chown $OWNER $DIRECT/$FILEP
		chmod $MODE $DIRECT/$FILEP
	fi
	exit 0;
fi

shift

PUTHERE=""

#
# Look for file in user specified directories.
#
for i in $*
do
	case $i in
		-*) $USAGE ;;
	esac

	if [ `expr "$i" : "/.*"` -gt 0 ]
	then
		# Prefix $ROOT only for absolute paths.
		i=$ROOT/$i
	fi

	PUTHERE=`find $i -name $FILE -type f -print | sed -e '2,$d'`
	if [ "$PUTHERE" != "" ]
	then break
	fi
done

#
# Look for file in $FLIST.  ($ROOT/etc/syslist)
#
if [ -r $FLIST -a "$PUTHERE" = "" ]
then
	PUTHERE=`grep "/${FILE}$" $FLIST | sed -e '2,$d'`
	if [ "$PUTHERE" != "" ]
	then
		PUTHERE=$ROOT/$PUTHERE
	fi
fi

#
# Look for file in $DEFAULT directories.
#
if [ "$PUTHERE" = "" ]
then
	PUTHERE=`find $DEFAULT -name $FILE -type f -print | sed -e '2,$d'`
fi

#
# Special handling for the link option
# Use FILEP as the source and link any remaining files relitive to FILEP
#
if [ "$LINKOPT" != "" ] ; then
	BASEDIR=`dirname $PUTHERE`
	for n in $* ; do
		echo "Linking $n to $PUTHERE"
		$CP $PUTHERE $BASEDIR/$n
	done
	exit 0
fi

#
# If found where it goes, put it there.
#
if [ "$PUTHERE" != "" ]
then
	 if [ -k $PUTHERE ]
	 then
	    chmod -t $PUTHERE
	    $PUTHERE < /dev/null > /dev/null
	    tbit=on
	fi
	if [ "$MOVOLD" = yes ]
	then
	    old=`echo $PUTHERE | sed -n -e "s;\(.*/\).*;\1;"`
	    mv -f $PUTHERE $old/OLD$FILE > /dev/null 2>&1
	    #
	    # If the file wasn't there in the first place don't
	    # do anything else.
	    #
	    if [ $? = 0 ] ; then
		cp $old/OLD$FILE $PUTHERE
		if [ $? = 0 ]
		then
			$ECHO "old $FILE moved to $old/OLD$FILE"
			chgrp $GROUP $PUTHERE
		else
			echo "install: cp $DIRECT/OLD$FILE $DIRECDIRECT/$FILE failed" 1>&2
			exit 2
		fi
	    fi
	fi
	FOUND=y
	if test "$CP" != "cp"; then rm -f $PUTHERE; fi
	if $CP $FILEP $PUTHERE
	then
	if [ "$tbit" = on ]
	then
	    chmod +t $PUTHERE
	fi
	    $ECHO "$FILEP installed as $PUTHERE"
	    break
	else
	    exit 2
	fi
fi

#
# Couldn't find it anywhere.  Try $LASTRES as last resort.
#
case $FOUND in
	"") echo "install: $FILE was not found anywhere!" 1>&2
	    exit 2;;
	 y) :	;;
	 n) 
	    if test "$CP" != "cp"; then rm -f $LASTRES/$FILE; fi

	    if [ `expr "$LASTRES" : "/.*"` -gt 0 ]
	    then
		    # Prefix $ROOT only for absolute paths.
		    LASTRES=$ROOT/$LASTRES
	    fi

	    $CP $FILEP $LASTRES/$FILE
	    if [ $? = 0 ]
	    then
		$ECHO "$FILEP installed as $LASTRES/$FILE by default!"
		cd $LASTRES
		chgrp $GROUP $FILE
		chown $OWNER $FILE
		chmod $MODE $FILE
	    fi;;
esac

exit 0
