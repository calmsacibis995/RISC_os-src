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
# $Header: fsck.sh,v 1.6.2.6 90/05/09 16:02:54 wje Exp $
#
# fsck - wrapper for fsck.ffs and fsck.S51K.
#
# Options are recognized, and passed on if they apply.  The -b option
# is ambiguous, and thus can not be used.
#

PATH=/bin:/usr/bin

Myname=`basename "$0"`

Ffs_opts=""
S5_opts=""
TYPE=""
FSTAB="/etc/fstab"

main()
{
	while :
	do
		case "$1" in
		# common options
		-[yYnN])
			Ffs_opts="$Ffs_opts $1"
			S5_opts="$S5_opts $1"
			;;

		# special debug option: translate types
		-[d])
			Ffs_opts="$Ffs_opts $1"
			S5_opts="$S5_opts -D"
			;;

		# ffs-only options
		-[aprC])
			Ffs_opts="$Ffs_opts $1"
			;;

		# S51K-only simple options
		-[DfqBF])
			S5_opts="$S5_opts $1"
			;;

		# special S51K-only options
		-[sS]*)
			S5_opts="$S5_opts $1"
			;;

		# over-ride filesystem type
		-T)
			TYPE="$2"
			shift
			;;

		-t)
			S5_opts="$S5_opts -t $2"
			shift
			;;

		-b*)
			dashb_err
			exit 1
			;;
		-*)
			error "Unknown option $1 -- See manual page for usage"
			exit 1
			;;
		*)
			break
			;;
		esac
		shift
	done

	if expr " $Ffs_opts" : " .*-a.*" > /dev/null 2>&1; then
		# execute all fsck types
		/etc/fsck.ffs $Ffs_opts
		exit
	fi

	case "$#" in
	    0)
		error "Must either specify filesystems or '-a' option with fsck."
		error "Use filesystem-specific command for default lists"
		exit 1
		;;
	esac

	for fs
	{
		if test "$TYPE" = ""; then
			type=`get_fsinfo "$fs"`
		else
			type="$TYPE"
		fi
		case "$type" in
			ffs | ufs | 4.3)
				/etc/fsck.ffs $Ffs_opts "$fs"
				;;
			S51K)
				/etc/fsck.S51K $S5_opts "$fs"
				;;
			"")
				error "Cannot determine filesystem type for $fs"
				error "Must use '-T <filesystem type>' option"
				;;
			*)
				error "Unsupported filesystem type $type for $fs"
				;;
		esac
	}
}

#
# get_fsinfo() prints the type of the named filesystem.  This is done by
# parsing the $FSTAB file.  If a match isn't found, the name is changed
# to match other potential names.  Should return a NULL string if no match
# is found.
#
get_fsinfo()
{
	if [ ! -f "$FSTAB" ]; then
		return
	fi
	block_dev=""
	raw_dev=""
	directory="$1"
	while true; do
		_type_=`awk 'NF > 1 { if ($1=="'"$directory"'" || $2=="'"$directory"'") { print $3 }}' $FSTAB`
		if test "$_type_" != ""; then
			echo $_type_
			break
		elif test "$block_dev" = "" && expr "$1" : ".*dev/r" > /dev/null 2>&1; then
			block_dev=`echo "$1" | sed 's@dev/r@dev/@'`
			directory="$block_dev"
			continue
		elif test "$raw_dev" = "" && expr "$1" : ".*dev/" > /dev/null 2>&1; then
			raw_dev=`echo "$1" | sed 's@dev/@dev/r@'`
			directory="$raw_dev"
			continue
		fi
		break
	done
}

#
# error() prints an error message
#

error()
{
	echo "$Myname: $*" 1>&2
}

#
# dashb_err() prints an error message about the -b option
#

dashb_err()
{
	echo "$Myname: The -b option is ambiguous. Use the \c" 1>&2
	echo "filesystem-specific command for your filesystem" 2>&2
}

main ${1+"$@"}
