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
# $Header: MKDEV.sh,v 1.22.2.9.1.1.1.2 90/11/06 17:34:29 beacker Exp $
#
# MKDEV - make devices
#
# Usage: MKDEV [-p] [-n] [-m machine] [-d directory] [class...]
#
# -p	Print commands to be executed
# -n	Don't execute commands
# -m	Use the given machine type instead of the current machine type
# -d	Use the named directory for the database instead of ./DEV_DB
#

PATH=/bin:/usr/bin:/usr/net:/etc

Dbdir=./DEV_DB

Myname=`basename "$0"`

Pgmfile=/tmp/mkdev.$$

Machtype=""

Execute="yes"
Print="no"

usage()
{
	echo "$Myname: usage: $Myname [-p] [-n] [-m machine] [-d directory] [class...]" 1>&2
}

main()
{

	while :
	do
		case "$1" in
		-p)
			Print="yes"
			;;
		-n)
			Execute="no"
			;;
		-m)
			shift
			Machtype="$1"
			;;
		-d)
			shift
			Dbdir="$1"
			;;
		-*)
			usage
			exit 1
			;;
		*)
			break
			;;
		esac
		shift
	done

	case "$#" in
	0)
		set default
		;;
	esac

	if [ ! -d "$Dbdir" ]
	then
		echo "$Myname: No database directory ($Dbdir)" 1>&2
		exit 2
	fi

	trap 'rm -f "$Pgmfile" ; exit' 0 1 2 15
	buildpgm

	printdb | preprocess | getcmds ${1+"$@"} | docmds

	rm -f "$Pgmfile"
	exit 0
}

#
# The function printdb() finds the names of the database files, and
# prints each of them using a special line-number processor.
#

printdb()
{

	case "$Machtype" in
	"")
		Machtype="`uname -t`"
		;;
	esac
	# device files for m2000-33 are the same as those of m2000-25
	if [ "$Machtype" = "m2000-33" ] 
	then
		Machtype="m2000-25"
	fi
	Thishost="`(hostname) 2>/dev/null`"

	Thisconfig="`diskconfig $Machtype`"

	for suff in system $Thisconfig local $Thishost
	{
		if [ -f "$Dbdir/common.$suff" ]
		then
			linecat "$Dbdir/common.$suff" "common.$suff"
		fi

		if [ -f "$Dbdir/$Machtype.$suff" ]
		then
			linecat "$Dbdir/$Machtype.$suff" "$Machtype.$suff"
		fi
	}
}

#
# diskconfig() returns the root disk configuration of this system.
# This will be used to source the appropriate additional configuration
# dependent device database file.  Currently this routine will only
# return a valid value for M2000 or 6000 systems only.  The current valid values
# are "ipc" for SMD rooted systems, and "ijc" for SCSI rooted systems.
# Note we are parsing the hex value output from kopt, therefore the case
# statement is done appropriately.
#
## It now knows about Genesis and returns "sdc" for major=21 hex
#
diskconfig()
{
        case "$1" in
        RC62*|RS62*|m2000*)
                Diskmaj=`/etc/kopt get rootdev 2>&1 | sed -e 's#^.*0x##'`
                Diskmaj=`echo $Diskmaj | sed -e 's#......$##'`
                ;;
        *)
                exit 0
                ;;
        esac

        case "$Diskmaj" in
        4|9)    			echo ipc ;;
        16|17|18|19|1a|1b|1c|1d)        echo ijc ;;
        21)                             echo sdc ;;
        *)      ;;
        esac

        exit 0
}

#
# linecat() prints the non-comment lines in the given file (file is $1, short
# name is $2), preceding each line with a line of the form
#
#	# filename linenumber
#
# This information is used to print error messages.
#

linecat()
{
	awk '
	BEGIN {
		Name="'"$2"'";
		Line=1;
	}
	{
		if ($0 !~ /^#/) {
			printf "# %s %d\n", Name, Line;
			print;
		}
		Line++;
	}' "$1"
}

#
# The function preprocess() "regularizes" the input data.  This is done
# by removing comments appearing after the beginning of the line (to avoid
# removing line info), turning semicolons into newlines, putting braces
# on separate lines, putting spaces around commas and parentheses, and
# doubling all backslashes.
#
# It would be nice if this were extended to ignore characters that are
# preceded by backslashes, but the only time is really matters is with
# message text, which gets processed by echo, so users can use the
# \xxx stuff.
#

preprocess()
{
	sed '
		s/\(.\)#.*$/\1/
		s/[ 	]*$//
		s/\([{}]\)/;\1;/g
		s/\([(),]\)/ \1 /g
		s/\\/\\\\/g
	' | tr -s ';' '\012'
}

#
# The function buildpgm() puts the awk program to be used in a temporary
# file.  Sadly, we can't leave it all on the command line.
#
# The awk script reads the database from stdin, and puts the information
# in an array.  When it gets an "end of database" marker, it searches the
# database for each of the named items.  The data printed begins with an
# operation and is followed by the data.  The following are the known
# operators:
#
#    #E		Error message
#    #D		Device specification
#    #M		Message
#    #C		Subclass name (usually not output)
#    #L		Link
#

buildpgm()
{

cat > "$Pgmfile" <<\!EOF!
BEGIN {
	Slot = 1;
	Count = 0;
	Lastclass = "(none)"

	# States
	ERROR  = -1;
	ARGS   = 0;
	CLASS  = 1;
	LBRACE = 2;
	INFO   = 3;

	State = CLASS;

	File = "(unknown)";
	Line = 0;

}
(State == ERROR) {  # error
	next;
}

($0 == "") {	# empty line
	next;
}

($1 == "#") {	# line directive
	File = $2;
	Line = $3;
	next;
}

($1 == "##EnD_Of_DaTa") {	# end of data -- arguments follow
	if (State != CLASS) {
		printf "#E (%s, line %d): Missing class terminator at end of file\n", File, Line;
		State = ERROR;
		next;
	}
	State = ARGS;
	next;
}
	
(State == CLASS) {	# looking for "class"
	if ($1 != "class") {
		printf "#E (%s, line %d): Expected class definition.  Got %s\n", File, Line, $1;
		State = ERROR;
		next;
	}
	if ($2 != "(" || $NF != ")" || NF < 4) {
		printf "#E (%s, line %d): Syntax error in class definition.\n", File, Line;
		State = ERROR;
		next;
	}
	Lastclass = $3;
	Name[$3] = Slot;
	Info[Slot] = 0;
	Count = 0;

	# the code just ignores commas instead of checking for
	# alternating commas
	for (i = 4; i < NF; i++) {
		if ($i == ",") {
			continue;
		}
		Name[$i] = -Slot;
	}
	State = LBRACE;
	next;
}
(State == LBRACE) { # looking for left brace
	if ($1 != "{") {	# }
		printf "#E (%s, line %d): Expected { after class definition.\n", File, Line; # }
		printf "#E Class defined was %s\n", Lastclass;
		State = ERROR;
		next;
	}
	State = INFO;
	next;
}
# process class info ({)
(State == INFO && $1 == "}") {
	Slot++;
	State = CLASS;
	next;
}
(State == INFO && $1 == "device") {
	if ($2 != "(" || $NF != ")" || NF != 16) {
		printf "#E (%s, line %d): Syntax error in device definition.\n", File, Line;
		State = ERROR;
		next;
	}
	if ($4 $6 $8 $10 $12 $14 !~ /,,,,,,/) {
		printf "#E (%s, line %d): Missing comma in device definition.\n", File, Line;
		State = ERROR;
		next;
	}
	if ($5 != "b" && $5 != "c") {
		printf "#E (%s, line %d): Invalid device type %s.\n", File, Line, $5;
		State = ERROR;
		next;
	}
	if ($7 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid major number %s.\n", File, Line, $7;
		State = ERROR;
		next;
	}
	if ($9 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid minor number %s.\n", File, Line, $9;
		State = ERROR;
		next;
	}
	if ($11 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid mode number %s.\n", File, Line, $11;
		State = ERROR;
		next;
	}
	ind = sprintf("%d_%d", Slot, Count);
	Info[ind] = sprintf("#D %s %s %s %s %s %s %s", $3, $5, $7, $9, $11, $13, $15);
	Count++;
	Info[Slot]++;
	next;
}
(State == INFO && $1 == "idevice") {
	if ($2 != "(" || $NF != ")" || NF != 20) {
		printf "#E (%s, line %d): Syntax error in iterative device definition.\n", File, Line;
		State = ERROR;
		next;
	}
	if ($4 $6 $8 $10 $12 $14 $16 $18 !~ /,,,,,,,,/) {
		printf "#E (%s, line %d): Missing comma in iterative device definition.\n", File, Line;
		State = ERROR;
		next;
	}
	if ($3 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid iterative device count.\n", File, Line;
		State = ERROR;
		next;
	}
	icnt = $3 + 0;
	if ($7 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid iterative device start.\n", File, Line;
		State = ERROR;
		next;
	}
	ncnt = $7 + 0;
	if ($9 != "b" && $9 != "c") {
		printf "#E (%s, line %d): Invalid device type %s.\n", File, Line, $9;
		State = ERROR;
		next;
	}
	if ($11 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid major number %s.\n", File, Line, $11;
		State = ERROR;
		next;
	}
	if ($13 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid minor number %s.\n", File, Line, $13;
		State = ERROR;
		next;
	}
	if ($15 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid mode number %s.\n", File, Line, $15;
		State = ERROR;
		next;
	}
	for (i = 0; i < icnt; i++) {
		ind = sprintf("%d_%d", Slot, Count);
		Info[ind] = sprintf("#D %s%d %s %s %d %s %s %s", $5, ncnt, $9, $11, $13 + i, $15, $17, $19);
		Count++;
		Info[Slot]++;
		ncnt++;
	}
	next;
}
(State == INFO && $1 == "message") {
	if ($2 != "(" || $NF != ")") {
		printf "#E (%s, line %d): Syntax error in message definition.\n", File, Line;
		State = ERROR;
		next;
	}
	message = "#M";
	for (i = 3; i < NF; i++) {
		message = sprintf("%s %s", message, $i);
	}
	ind = sprintf("%d_%d", Slot, Count);
	Info[ind] = message;
	Count++;
	Info[Slot]++;
	next;
}
(State == INFO && $1 == "link") {
	if ($2 != "(" || $NF != ")" || NF != 6) {
		printf "#E (%s, line %d): Syntax error in link definition.\n", File, Line;
		State = ERROR;
		next;
	}
	if ($4 != ",") {
		printf "#E (%s, line %d): Missing comma in link definition.\n", File, Line;
		State = ERROR;
		next;
	}
	ind = sprintf("%d_%d", Slot, Count);
	Info[ind] = sprintf("#L %s %s", $3, $5);
	Count++;
	Info[Slot]++;
	next;
}
(State == INFO && $1 == "ilink") {
	if ($2 != "(" || $NF != ")" || NF != 12) {
		printf "#E (%s, line %d): Syntax error in iterative link definition.\n", File, Line;
		State = ERROR;
		next;
	}
	if ($4 $6 $8 $10 !~ /,,,,/) {
		printf "#E (%s, line %d): Missing comma in iterative link definition.\n", File, Line;
		State = ERROR;
		next;
	}
	if ($3 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid iterative link count.\n", File, Line;
		State = ERROR;
		next;
	}
	icnt = $3 + 0;
	if ($7 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid iterative link name start.\n", File, Line;
		State = ERROR;
		next;
	}
	cnt1 = $7 + 0;
	if ($11 !~ /^[0-9][0-9]*$/) {
		printf "#E (%s, line %d): Invalid iterative link target start.\n", File, Line;
		State = ERROR;
		next;
	}
	cnt2 = $11 + 0;
	for (i = 0; i < icnt; i++) {
		ind = sprintf("%d_%d", Slot, Count);
		Info[ind] = sprintf("#L %s%d %s%d", $5, cnt1, $9, cnt2);
		Count++;
		Info[Slot]++;
		cnt1++;
		cnt2++;
	}
	next;
}
(State == INFO) {	# default
	if ($0 ~ /^[ 	]*$/) {
		next;
	}
	ind = sprintf("%d_%d", Slot, Count);
	Info[ind] = sprintf("#C %s", $1);
	Count++;
	Info[Slot]++;
	next;
}
(State == ARGS) {
	class = $1;
	stk_cslot[0] = 0;
	stk_index[0] = 0;
	stk_maxin[0] = 0;

	if (Name[class] == 0) {
		printf "#E %s: No such class\n", $1;
		State = ERROR;
		next;
	}

	i = Name[class];
	if (i < 0) {
		i = -i;
	}

	stk_cslot[1] = i;
	stk_index[1] = 0;
	stk_maxin[1] = Info[i];
	Info[i] = -1;
	depth = 1;

	while (depth > 0) {
		i = stk_cslot[depth];
		j = stk_index[depth];
		max = stk_maxin[depth];
		while (j < max) {
			ind = sprintf("%d_%d", i, j);
			if (Info[ind] ~ /^#C .*$/) {
				stk_cslot[depth] = i;
				stk_index[depth] = j + 1;
				depth++;
				# split into parts
				flds = split(Info[ind], parts, " ");
				# find new class slot number
				class = parts[2];
				if (Name[class] == 0) {
					printf "#E %s: No such class\n", class;
					State = ERROR;
					next;
				}
				i = Name[class];
				if (i < 0) {
					i = -i;
				}
				# check for class already used
				if (Info[i] < 0) {
					printf "#E Class %s used ", class;
					printf "multiple times -- ignored\n";
					j++;
					break;
				}
				stk_cslot[depth] = i;
				stk_index[depth] = 0;
				stk_maxin[depth] = Info[i];
				Info[i] = -1;

				depth++;  # gets decremented at loop end
				break;
			} else {
				printf "%s\n", Info[ind];
				j++;
			}
		}
		depth--;
	}
}
!EOF!
}

#
# The function getcmds() sends the database, a separator, and the
# arguments to a huge awk script that stores the database and extracts
# the information requested by the arguments.
#

getcmds()
{
	(cat ;
	 echo "##EnD_Of_DaTa" ;
	 for i
	 {
		echo "$i"
	 } ) |\
	awk -f "$Pgmfile"
}

#
# The function docmds() reads the output from the command generator
# and processes them.  The "comment" commands are processed by this
# routine.  Other commands are sent to the print and/or exec processor
# function, depending on the command-line options.
# 

docmds()
{
	while read op f1 f2 f3 f4 f5 f6 f7
	do
		case "$op" in
		"#E")
			echo "$Myname: $f1 $f2 $f3 $f4 $f5 $f6 $f7" 1>&2
			continue
			;;
		"#C")
			echo "# subclass: $f2"
			continue
			;;
		esac

		case "$Print" in
		"yes")
			do_print "$op" "$f1" "$f2" "$f3" "$f4" "$f5" "$f6" "$f7"
			;;
		esac

		case "$Execute" in
		"yes")
			do_exec "$op" "$f1" "$f2" "$f3" "$f4" "$f5" "$f6" "$f7"
			;;
		esac
			
	done
}

#
# do_print() prints the commands that would be executed for each of
# the given operations.
#

do_print()
{
	case "$1" in
	"#M")
		echo "echo '$2 $3 $4 $5 $6 $7 $8'"
		;;
	"#D")
		echo "rm -f $2"
		case "$2" in
		*/*)
			dir=`expr "$2" : '^\(.*\)/[^/]*$'`
			if [ ! -d "$dir" ]
			then
				echo "mkdir -p $dir"
			fi
			;;
		esac
		echo "mknod $2 $3 $4 $5"
		echo "chown $7 $2"
		echo "chgrp $8 $2"
		echo "chmod $6 $2"
		;;
	"#L")
		echo "rm -f $3"
		case "$2" in
		*/*)
			dir=`expr "$2" : '^\(.*\)/[^/]*$'`
			if [ ! -d "$dir" ]
			then
				echo "mkdir -p $dir"
			fi
			;;
		esac
		case "$3" in
		*/*)
			dir=`expr "$3" : '^\(.*\)/[^/]*$'`
			if [ ! -d "$dir" ]
			then
				echo "mkdir -p $dir"
			fi
			;;
		esac
		echo "ln $2 $3"
		;;
	*)
		echo "$1: $2 $3 $4 $5 $6 $7 $8" 1>&2
		;;
	esac
}

#
# do_print() prints the commands that would be executed for each of
# the given operations.
#

do_exec()
{
	case "$1" in
	"#M")
		echo "$2 $3 $4 $5 $6 $7 $8"
		;;
	"#D")
		rm -f "$2"
		case "$2" in
		*/*)
			dir=`expr "$2" : '^\(.*\)/[^/]*$'`
			if [ ! -d "$dir" ]
			then
				mkdir -p "$dir"
			fi
			;;
		esac
		mknod "$2" "$3" "$4" "$5"
		chown "$7" "$2"
		chgrp "$8" "$2"
		chmod "$6" "$2"
		;;
	"#L")
		rm -f "$3"
		case "$2" in
		*/*)
			dir=`expr "$2" : '^\(.*\)/[^/]*$'`
			if [ ! -d "$dir" ]
			then
				mkdir -p "$dir"
			fi
			;;
		esac
		case "$3" in
		*/*)
			dir=`expr "$3" : '^\(.*\)/[^/]*$'`
			if [ ! -d "$dir" ]
			then
				mkdir -p "$dir"
			fi
			;;
		esac
		ln "$2" "$3"
		;;
	esac
}

main ${1+"$@"}
exit 0
