#ident "$Header: mkdepend.awk,v 1.1.4.1 89/11/28 09:57:00 wje Exp $"
# compute header file dependencies
#   This is intended to work with the C preprocessor.
#   Modified to work on concatenated cpp -M output; also no associative
#   arrays.  -be
# command line options:
#	FRC - adds $FRC to end of each dependency
#	LIB - if set to "LIB":target is of form $(LIBNAME)(x.o): rather than x.o:
#	    - otherwise if set its value is prefixed to target ($LIBx.o:)
#	INC -
#	INCDIR - if dependency contains $INC, that substring is replaced with
#		$INCDIR
#	INC1 -
#	INCDIR1 - if dependency contains $INC1, that substring is replaced with
#		$INCDIR1

#   1 April 1986

# start things
BEGIN {
    INDENT = "\t"
    top=1
}

# for each line of pruned cpp -M output
NF > 0 {
# generate a (non-redundant) dependency entry for every header file
    if ($1 != lhs) {
	# new target
	lhs = $1
	if (top == 0) {
		# don't put out for first one!
		if (FRC != 0) {
		    print p_line "\\"
		    print INDENT FRC
		} else {
		    print p_line 
		}
	} else {
		top = 0
	}
	if (LIB == "LIB") {
		split($1, nm, ":")
		p_line = "$(LIBNAME)("nm[1]"):"nm[2] " "
	} else if (LIB != 0) {
		p_line = LIB$1
	} else {
		p_line = $1
	}
	lim = 72
    }
    # handle path substituions
    if ((INC != 0) && ((idx = index($2, INC)) != 0)) {
	# contains $INC skip over and prefix with $INCDIR
	idx += length(INC)
	dep=INCDIR""substr($2, idx)
    } else if ((INC1 != 0) && ((idx = index($2, INC1)) != 0)) {
	# contains $INC1 skip over and prefix with $INCDIR1
	idx += length(INC1)
	dep=INCDIR1""substr($2, idx)
    } else {
	dep=$2
    }

    if (length(p_line) + length(dep) > lim) {
	print p_line "\\"
	p_line = INDENT
	lim = 63
    }
    p_line = p_line dep " "
}


END {
    if (FRC != 0) {
	print p_line "\\"
	print INDENT FRC
    } else {
        print p_line 
    }
	
}
