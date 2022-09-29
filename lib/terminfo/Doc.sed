# #ident	"@(#)terminfo:Doc.sed	1.4"
#ident	"$Header: Doc.sed,v 1.3.3.1 89/11/27 12:15:54 wje Exp $"
#
#	This script is used to strip info from the terminfo
#	source files.
#
sed -n '
	/^# \{1,\}Manufacturer:[ 	]*\(.*\)/s//.M \1/p
	/^# \{1,\}Class:[ 	]*\(.*\)/s//.C \1/p
	/^# \{1,\}Author:[ 	]*\(.*\)/s//.A \1/p
	/^# \{1,\}Info:[ 	]*/,/^[^#][^	]/ {
		s/^# *Info:/.I/p
		/^#[	 ]\{1,\}/ {
			s/#//p
		}
		/^#$/ i\
.IE
	}
	/^\([^#	 ][^ 	]*\)|\([^|,]*\),[ 	]*$/ {
		s//Terminal:\
	"\2"\
		\1/
		s/|/, /g
		p
	}
' $*
