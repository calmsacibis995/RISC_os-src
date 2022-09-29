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
# $Header: mkdepend.sh,v 1.3.3.2 90/05/09 16:48:27 wje Exp $
# NAME
#	mkdepend - compute header file dependencies
# SYNOPSIS
#	mkdepend [-c compilehow] [-e sedprog] [-f force] [-i] makefile file ...
# DESCRIPTION
#	Mkdepend infers make dependencies from source containing C #include
#	directives.  Given a shell command compilehow which consists of cc
#	followed by options, mkdepend processes its file arguments and edits
#	the generated dependency information into makefile.
#
#	The -e flag passes an immediate program to sed, which is applied to
#	raw dependency information of the following form:
#	
#	target: dependent
#
#	Thus one may substitute pathname prefixes with envariable parameters,
#	for example.
#
#	The -f flag causes mkdepend to add a dependent named force to each
#	target file's dependency list.  Using -f '$(FRC)' and setting FRC=FRC
#	in a make's environment, one may rebuild certain objects without first
#	removing them.
#
#	Normally, old dependencies are deleted from the makefile.  The -i
#	option causes mkdepend to preserve old dependencies.  When invoked
#	from a makefile, the following rule enables incremental updates to
#	the dependency database:
#
#	incdepend: $(SRCS)
#		mkdepend -c "$(CC) $(CFLAGS)" -i Makefile $?
#		touch incdepend
#
usage="usage: $0 [-c compilehow] [-e sedprog] [-f force] [-i] makefile file ..."
depgen="cc -M"
incr="no"

# process option flags
while test $# -gt 0
do
	case $1 in
	  -c)	depgen="$2 -M" shift; shift;;
	  -e)	sedprog="$sedprog -e $2" shift; shift;;
	  -f)	force="$force $2" shift; shift;;
	  -i)	incr=yes shift;;
	  *)	break
	esac
done
if test $# -lt 2
then
	echo $usage; exit 1
fi

# an awk script to compress dependencies
awkprog='
BEGIN {
	INDENT = "\t"
	INDENTSIZE = 8
	MAXLINE = 72
	MAXINDENTEDLINE = MAXLINE - INDENTSIZE - 1
}

# for each line of the form "target: dependent"
NF > 0 {
	if ($1 != target) {
		target = $1
		if (depline) print depline 
		if ("'$force'" != "") {
			depline = $1 " '"$force"' "
		} else {
			depline = $1 " "
		}
		lim = MAXLINE
	}
	if (length(depline) + length($2) > lim) {
		print depline "\\"
		depline = INDENT
		lim = MAXINDENTEDLINE
	}
	depline = depline $2 " "
}

END {
	print depline 
}'

# get make file names
newmakefile="$1"
oldmakefile="#$1"
shift

# save the old file in case of problems
if test -f $newmakefile
then
	mv -f $newmakefile $oldmakefile
fi
if test ! -f $oldmakefile
then
	echo "Neither $oldmakefile nor $newmakefile exists."
	exit 1
fi

# define flag lines
firstline='# DO NOT DELETE THIS LINE -- make depend uses it'
lastline='# DO NOT DELETE THIS 2nd LINE -- make depend uses it'

# remove old dependencies from the makefile
sed -e "/^$firstline/,/^$lastline/d" -e "/^$firstline/"',$d' $oldmakefile \
    > $newmakefile

# delimit the dependencies
if test "$incr" = "no"
then
	echo $firstline >> $newmakefile
else
	# save old dependences in 'incremental' mode
	deptmp=/tmp/incdep.$$
	sed -n -e "/^$firstline/,/^$lastline/p" $oldmakefile |
	    grep -v "^$lastline" > $deptmp
	awkinc='/^[^ 	]*$/ { deleting = 0; }'
	for f in $*
	do
		basename=`expr $f : '\(.*\)\.[cflpsy]'`
		awkinc="$awkinc /^$basename/ { deleting = 1; }"
	done
	awkinc="$awkinc { if (deleting == 0) print \$0; }"
	awk "$awkinc" $deptmp >> $newmakefile
	rm -f $deptmp
fi

# process source files - the third sed command is needed because of bogus IRIS
# cc blather on stdout instead of on stderr
$depgen $* |
    sed -e 's:\.\./[^\./][^\./]*/\.\.:..:g' \
	-e 's: \./: :' \
	-e '/^[^ 	]*\.c:/d' \
	$sedprog |
    awk "$awkprog" >> $newmakefile

echo $lastline >> $newmakefile
