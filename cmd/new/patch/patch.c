/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: patch.c,v 1.1.2.2 90/05/09 18:10:17 wje Exp $"

#include "INTERN.h"
#include "common.h"
#include "EXTERN.h"
#include "version.h"
#include "util.h"
#include "pch.h"
#include "inp.h"

/* procedures */

void reinitialize_almost_everything();
void get_some_switches();
LINENUM locate_hunk();
void abort_hunk();
void apply_hunk();
void do_ed_script();
void init_output();
void init_reject();
void copy_till();
void spew_output();
void dump_line();
bool patch_match();
bool similar();
void re_input();
void my_exit();

/* Apply a set of diffs as appropriate. */

main(argc,argv)
int argc;
char **argv;
{
    LINENUM where;
    LINENUM newwhere;
    LINENUM fuzz;
    LINENUM mymaxfuzz;
    int hunk = 0;
    int failed = 0;
    int i;

    setbuf(stderr, serrbuf);
    for (i = 0; i<MAXFILEC; i++)
	filearg[i] = Nullch;
    Mktemp(TMPOUTNAME);
    Mktemp(TMPINNAME);
    Mktemp(TMPREJNAME);
    Mktemp(TMPPATNAME);

    /* parse switches */
    Argc = argc;
    Argv = argv;
    get_some_switches();
    
    /* make sure we clean up /tmp in case of disaster */
    set_signals();

    for (
	open_patch_file(filearg[1]);
	there_is_another_patch();
	reinitialize_almost_everything()
    ) {					/* for each patch in patch file */

	if (outname == Nullch)
	    outname = savestr(filearg[0]);
    
	/* initialize the patched file */
	if (!skip_rest_of_patch)
	    init_output(TMPOUTNAME);
    
	/* for ed script just up and do it and exit */
	if (diff_type == ED_DIFF) {
	    do_ed_script();
	    continue;
	}
    
	/* initialize reject file */
	init_reject(TMPREJNAME);
    
	/* find out where all the lines are */
	if (!skip_rest_of_patch)
	    scan_input(filearg[0]);
    
	/* from here on, open no standard i/o files, because malloc */
	/* might misfire and we can't catch it easily */
    
	/* apply each hunk of patch */
	hunk = 0;
	failed = 0;
	out_of_mem = FALSE;
	while (another_hunk()) {
	    hunk++;
	    fuzz = Nulline;
	    mymaxfuzz = pch_context();
	    if (maxfuzz < mymaxfuzz)
		mymaxfuzz = maxfuzz;
	    if (!skip_rest_of_patch) {
		do {
		    where = locate_hunk(fuzz);
		    if (hunk == 1 && where == Nulline && !force) {
						/* dwim for reversed patch? */
			if (!pch_swap()) {
			    if (fuzz == Nulline)
				say1("\
Not enough memory to try swapped hunk!  Assuming unswapped.\n");
			    continue;
			}
			reverse = !reverse;
			where = locate_hunk(fuzz);  /* try again */
			if (where == Nulline) {	    /* didn't find it swapped */
			    if (!pch_swap())         /* put it back to normal */
				fatal1("Lost hunk on alloc error!\n");
			    reverse = !reverse;
			}
			else if (noreverse) {
			    if (!pch_swap())         /* put it back to normal */
				fatal1("Lost hunk on alloc error!\n");
			    reverse = !reverse;
			    say1("\
Ignoring previously applied (or reversed) patch.\n");
			    skip_rest_of_patch = TRUE;
			}
			else {
			    ask3("\
%seversed (or previously applied) patch detected!  %s -R? [y] ",
				reverse ? "R" : "Unr",
				reverse ? "Assume" : "Ignore");
			    if (*buf == 'n') {
				ask1("Apply anyway? [n] ");
				if (*buf != 'y')
				    skip_rest_of_patch = TRUE;
				where = Nulline;
				reverse = !reverse;
				if (!pch_swap())  /* put it back to normal */
				    fatal1("Lost hunk on alloc error!\n");
			    }
			}
		    }
		} while (!skip_rest_of_patch && where == Nulline &&
		    ++fuzz <= mymaxfuzz);

		if (skip_rest_of_patch) {		/* just got decided */
		    Fclose(ofp);
		    ofp = Nullfp;
		}
	    }

	    newwhere = pch_newfirst() + last_offset;
	    if (skip_rest_of_patch) {
		abort_hunk();
		failed++;
		if (verbose)
		    say3("Hunk #%d ignored at %ld.\n", hunk, newwhere);
	    }
	    else if (where == Nulline) {
		abort_hunk();
		failed++;
		if (verbose)
		    say3("Hunk #%d failed at %ld.\n", hunk, newwhere);
	    }
	    else {
		apply_hunk(where);
		if (verbose) {
		    say3("Hunk #%d succeeded at %ld", hunk, newwhere);
		    if (fuzz)
			say2(" with fuzz %ld", fuzz);
		    if (last_offset)
			say3(" (offset %ld line%s)",
			    last_offset, last_offset==1L?"":"s");
		    say1(".\n");
		}
	    }
	}

	if (out_of_mem && using_plan_a) {
	    Argc = Argc_last;
	    Argv = Argv_last;
	    say1("\n\nRan out of memory using Plan A--trying again...\n\n");
	    continue;
	}
    
	assert(hunk);
    
	/* finish spewing out the new file */
	if (!skip_rest_of_patch)
	    spew_output();
	
	/* and put the output where desired */
	ignore_signals();
	if (!skip_rest_of_patch) {
	    if (move_file(TMPOUTNAME, outname) < 0) {
		toutkeep = TRUE;
		chmod(TMPOUTNAME, filemode);
	    }
	    else
		chmod(outname, filemode);
	}
	Fclose(rejfp);
	rejfp = Nullfp;
	if (failed) {
	    if (!*rejname) {
		Strcpy(rejname, outname);
		Strcat(rejname, ".rej");
	    }
	    if (skip_rest_of_patch) {
		say4("%d out of %d hunks ignored--saving rejects to %s\n",
		    failed, hunk, rejname);
	    }
	    else {
		say4("%d out of %d hunks failed--saving rejects to %s\n",
		    failed, hunk, rejname);
	    }
	    if (move_file(TMPREJNAME, rejname) < 0)
		trejkeep = TRUE;
	}
	set_signals();
    }
    my_exit(0);
}

/* Prepare to find the next patch to do in the patch file. */

void
reinitialize_almost_everything()
{
    re_patch();
    re_input();

    input_lines = 0;
    last_frozen_line = 0;

    filec = 0;
    if (filearg[0] != Nullch && !out_of_mem) {
	free(filearg[0]);
	filearg[0] = Nullch;
    }

    if (outname != Nullch) {
	free(outname);
	outname = Nullch;
    }

    last_offset = 0;

    diff_type = 0;

    if (revision != Nullch) {
	free(revision);
	revision = Nullch;
    }

    reverse = FALSE;
    skip_rest_of_patch = FALSE;

    get_some_switches();

    if (filec >= 2)
	fatal1("You may not change to a different patch file.\n");
}

/* Process switches and filenames up to next '+' or end of list. */

void
get_some_switches()
{
    Reg1 char *s;

    rejname[0] = '\0';
    Argc_last = Argc;
    Argv_last = Argv;
    if (!Argc)
	return;
    for (Argc--,Argv++; Argc; Argc--,Argv++) {
	s = Argv[0];
	if (strEQ(s, "+")) {
	    return;			/* + will be skipped by for loop */
	}
	if (*s != '-' || !s[1]) {
	    if (filec == MAXFILEC)
		fatal1("Too many file arguments.\n");
	    filearg[filec++] = savestr(s);
	}
	else {
	    switch (*++s) {
	    case 'b':
		origext = savestr(Argv[1]);
		Argc--,Argv++;
		break;
	    case 'c':
		diff_type = CONTEXT_DIFF;
		break;
	    case 'd':
		if (!*++s) {
		    Argc--,Argv++;
		    s = Argv[0];
		}
		if (chdir(s) < 0)
		    fatal2("Can't cd to %s.\n", s);
		break;
	    case 'D':
	    	do_defines = TRUE;
		if (!*++s) {
		    Argc--,Argv++;
		    s = Argv[0];
		}
		Sprintf(if_defined, "#ifdef %s\n", s);
		Sprintf(not_defined, "#ifndef %s\n", s);
		Sprintf(end_defined, "#endif /* %s */\n", s);
		break;
	    case 'e':
		diff_type = ED_DIFF;
		break;
	    case 'f':
		force = TRUE;
		break;
	    case 'F':
		if (*++s == '=')
		    s++;
		maxfuzz = atoi(s);
		break;
	    case 'l':
		canonicalize = TRUE;
		break;
	    case 'n':
		diff_type = NORMAL_DIFF;
		break;
	    case 'N':
		noreverse = TRUE;
		break;
	    case 'o':
		outname = savestr(Argv[1]);
		Argc--,Argv++;
		break;
	    case 'p':
		if (*++s == '=')
		    s++;
		strippath = atoi(s);
		break;
	    case 'r':
		Strcpy(rejname, Argv[1]);
		Argc--,Argv++;
		break;
	    case 'R':
		reverse = TRUE;
		break;
	    case 's':
		verbose = FALSE;
		break;
	    case 'S':
		skip_rest_of_patch = TRUE;
		break;
	    case 'v':
		version();
		break;
#ifdef DEBUGGING
	    case 'x':
		debug = atoi(s+1);
		break;
#endif
	    default:
		fatal2("Unrecognized switch: %s\n", Argv[0]);
	    }
	}
    }
}

/* Attempt to find the right place to apply this hunk of patch. */

LINENUM
locate_hunk(fuzz)
LINENUM fuzz;
{
    Reg1 LINENUM first_guess = pch_first() + last_offset;
    Reg2 LINENUM offset;
    LINENUM pat_lines = pch_ptrn_lines();
    Reg3 LINENUM max_pos_offset = input_lines - first_guess
				- pat_lines + 1; 
    Reg4 LINENUM max_neg_offset = first_guess - last_frozen_line - 1
				+ pch_context();

    if (!pat_lines)			/* null range matches always */
	return first_guess;
    if (max_neg_offset >= first_guess)	/* do not try lines < 0 */
	max_neg_offset = first_guess - 1;
    if (first_guess <= input_lines && patch_match(first_guess, Nulline, fuzz))
	return first_guess;
    for (offset = 1; ; offset++) {
	Reg5 bool check_after = (offset <= max_pos_offset);
	Reg6 bool check_before = (offset <= max_neg_offset);

	if (check_after && patch_match(first_guess, offset, fuzz)) {
#ifdef DEBUGGING
	    if (debug & 1)
		say3("Offset changing from %ld to %ld\n", last_offset, offset);
#endif
	    last_offset = offset;
	    return first_guess+offset;
	}
	else if (check_before && patch_match(first_guess, -offset, fuzz)) {
#ifdef DEBUGGING
	    if (debug & 1)
		say3("Offset changing from %ld to %ld\n", last_offset, -offset);
#endif
	    last_offset = -offset;
	    return first_guess-offset;
	}
	else if (!check_before && !check_after)
	    return Nulline;
    }
}

/* We did not find the pattern, dump out the hunk so they can handle it. */

void
abort_hunk()
{
    Reg1 LINENUM i;
    Reg2 LINENUM pat_end = pch_end();
    /* add in last_offset to guess the same as the previous successful hunk */
    LINENUM oldfirst = pch_first() + last_offset;
    LINENUM newfirst = pch_newfirst() + last_offset;
    LINENUM oldlast = oldfirst + pch_ptrn_lines() - 1;
    LINENUM newlast = newfirst + pch_repl_lines() - 1;
    char *stars = (diff_type == NEW_CONTEXT_DIFF ? " ****" : "");
    char *minuses = (diff_type == NEW_CONTEXT_DIFF ? " ----" : " -----");

    fprintf(rejfp, "***************\n");
    for (i=0; i<=pat_end; i++) {
	switch (pch_char(i)) {
	case '*':
	    if (oldlast < oldfirst)
		fprintf(rejfp, "*** 0%s\n", stars);
	    else if (oldlast == oldfirst)
		fprintf(rejfp, "*** %ld%s\n", oldfirst, stars);
	    else
		fprintf(rejfp, "*** %ld,%ld%s\n", oldfirst, oldlast, stars);
	    break;
	case '=':
	    if (newlast < newfirst)
		fprintf(rejfp, "--- 0%s\n", minuses);
	    else if (newlast == newfirst)
		fprintf(rejfp, "--- %ld%s\n", newfirst, minuses);
	    else
		fprintf(rejfp, "--- %ld,%ld%s\n", newfirst, newlast, minuses);
	    break;
	case '\n':
	    fprintf(rejfp, "%s", pfetch(i));
	    break;
	case ' ': case '-': case '+': case '!':
	    fprintf(rejfp, "%c %s", pch_char(i), pfetch(i));
	    break;
	default:
	    say1("Fatal internal error in abort_hunk().\n"); 
	    abort();
	}
    }
}

/* We found where to apply it (we hope), so do it. */

void
apply_hunk(where)
LINENUM where;
{
    Reg1 LINENUM old = 1;
    Reg2 LINENUM lastline = pch_ptrn_lines();
    Reg3 LINENUM new = lastline+1;
#define OUTSIDE 0
#define IN_IFNDEF 1
#define IN_IFDEF 2
#define IN_ELSE 3
    Reg4 int def_state = OUTSIDE;
    Reg5 bool R_do_defines = do_defines;

    where--;
    while (pch_char(new) == '=' || pch_char(new) == '\n')
	new++;
    
    while (old <= lastline) {
	if (pch_char(old) == '-') {
	    copy_till(where + old - 1);
	    if (R_do_defines) {
		if (def_state == OUTSIDE) {
		    fputs(not_defined, ofp);
		    def_state = IN_IFNDEF;
		}
		else if (def_state == IN_IFDEF) {
		    fputs(else_defined, ofp);
		    def_state = IN_ELSE;
		}
		fputs(pfetch(old), ofp);
	    }
	    last_frozen_line++;
	    old++;
	}
	else if (pch_char(new) == '+') {
	    copy_till(where + old - 1);
	    if (R_do_defines) {
		if (def_state == IN_IFNDEF) {
		    fputs(else_defined, ofp);
		    def_state = IN_ELSE;
		}
		else if (def_state == OUTSIDE) {
		    fputs(if_defined, ofp);
		    def_state = IN_IFDEF;
		}
	    }
	    fputs(pfetch(new), ofp);
	    new++;
	}
	else {
	    if (pch_char(new) != pch_char(old)) {
		say3("Out-of-sync patch, lines %ld,%ld\n",
		    pch_hunk_beg() + old - 1,
		    pch_hunk_beg() + new - 1);
#ifdef DEBUGGING
		say3("oldchar = '%c', newchar = '%c'\n",
		    pch_char(old), pch_char(new));
#endif
		my_exit(1);
	    }
	    if (pch_char(new) == '!') {
		copy_till(where + old - 1);
		if (R_do_defines) {
		   fputs(not_defined, ofp);
		   def_state = IN_IFNDEF;
		}
		while (pch_char(old) == '!') {
		    if (R_do_defines) {
			fputs(pfetch(old), ofp);
		    }
		    last_frozen_line++;
		    old++;
		}
		if (R_do_defines) {
		    fputs(else_defined, ofp);
		    def_state = IN_ELSE;
		}
		while (pch_char(new) == '!') {
		    fputs(pfetch(new), ofp);
		    new++;
		}
		if (R_do_defines) {
		    fputs(end_defined, ofp);
		    def_state = OUTSIDE;
		}
	    }
	    else {
		assert(pch_char(new) == ' ');
		old++;
		new++;
	    }
	}
    }
    if (new <= pch_end() && pch_char(new) == '+') {
	copy_till(where + old - 1);
	if (R_do_defines) {
	    if (def_state == OUTSIDE) {
	    	fputs(if_defined, ofp);
		def_state = IN_IFDEF;
	    }
	    else if (def_state == IN_IFNDEF) {
		fputs(else_defined, ofp);
		def_state = IN_ELSE;
	    }
	}
	while (new <= pch_end() && pch_char(new) == '+') {
	    fputs(pfetch(new), ofp);
	    new++;
	}
    }
    if (R_do_defines && def_state != OUTSIDE) {
	fputs(end_defined, ofp);
    }
}

/* Apply an ed script by feeding ed itself. */

void
do_ed_script()
{
    Reg1 char *t;
    Reg2 long beginning_of_this_line;
    Reg3 bool this_line_is_command = FALSE;
    Reg4 FILE *pipefp;
    FILE *popen();

    if (!skip_rest_of_patch) {
	Unlink(TMPOUTNAME);
	copy_file(filearg[0], TMPOUTNAME);
	if (verbose)
	    Sprintf(buf, "/bin/ed %s", TMPOUTNAME);
	else
	    Sprintf(buf, "/bin/ed - %s", TMPOUTNAME);
	pipefp = popen(buf, "w");
    }
    for (;;) {
	beginning_of_this_line = ftell(pfp);
	if (pgets(buf, sizeof buf, pfp) == Nullch) {
	    next_intuit_at(beginning_of_this_line);
	    break;
	}
	for (t=buf; isdigit(*t) || *t == ','; t++) ;
	this_line_is_command = (isdigit(*buf) &&
	  (*t == 'd' || *t == 'c' || *t == 'a') );
	if (this_line_is_command) {
	    if (!skip_rest_of_patch)
		fputs(buf, pipefp);
	    if (*t != 'd') {
		while (pgets(buf, sizeof buf, pfp) != Nullch) {
		    if (!skip_rest_of_patch)
			fputs(buf, pipefp);
		    if (strEQ(buf, ".\n"))
			break;
		}
	    }
	}
	else {
	    next_intuit_at(beginning_of_this_line);
	    break;
	}
    }
    if (skip_rest_of_patch)
	return;
    fprintf(pipefp, "w\n");
    fprintf(pipefp, "q\n");
    Fflush(pipefp);
    Pclose(pipefp);
    ignore_signals();
    if (move_file(TMPOUTNAME, outname) < 0) {
	toutkeep = TRUE;
	chmod(TMPOUTNAME, filemode);
    }
    else
	chmod(outname, filemode);
    set_signals();
}

/* Open the new file. */

void
init_output(name)
char *name;
{
    ofp = fopen(name, "w");
    if (ofp == Nullfp)
	fatal2("patch: can't create %s.\n", name);
}

/* Open a file to put hunks we can't locate. */

void
init_reject(name)
char *name;
{
    rejfp = fopen(name, "w");
    if (rejfp == Nullfp)
	fatal2("patch: can't create %s.\n", name);
}

/* Copy input file to output, up to wherever hunk is to be applied. */

void
copy_till(lastline)
Reg1 LINENUM lastline;
{
    Reg2 LINENUM R_last_frozen_line = last_frozen_line;

    if (R_last_frozen_line > lastline)
	say1("patch: misordered hunks! output will be garbled.\n");
    while (R_last_frozen_line < lastline) {
	dump_line(++R_last_frozen_line);
    }
    last_frozen_line = R_last_frozen_line;
}

/* Finish copying the input file to the output file. */

void
spew_output()
{
#ifdef DEBUGGING
    if (debug & 256)
	say3("il=%ld lfl=%ld\n",input_lines,last_frozen_line);
#endif
    if (input_lines)
	copy_till(input_lines);		/* dump remainder of file */
    Fclose(ofp);
    ofp = Nullfp;
}

/* Copy one line from input to output. */

void
dump_line(line)
LINENUM line;
{
    Reg1 char *s;
    Reg2 char R_newline = '\n';

    /* Note: string is not null terminated. */
    for (s=ifetch(line, 0); putc(*s, ofp) != R_newline; s++) ;
}

/* Does the patch pattern match at line base+offset? */

bool
patch_match(base, offset, fuzz)
LINENUM base;
LINENUM offset;
LINENUM fuzz;
{
    Reg1 LINENUM pline = 1 + fuzz;
    Reg2 LINENUM iline;
    Reg3 LINENUM pat_lines = pch_ptrn_lines() - fuzz;

    for (iline=base+offset+fuzz; pline <= pat_lines; pline++,iline++) {
	if (canonicalize) {
	    if (!similar(ifetch(iline, (offset >= 0)),
			 pfetch(pline),
			 pch_line_len(pline) ))
		return FALSE;
	}
	else if (strnNE(ifetch(iline, (offset >= 0)),
		   pfetch(pline),
		   pch_line_len(pline) ))
	    return FALSE;
    }
    return TRUE;
}

/* Do two lines match with canonicalized white space? */

bool
similar(a,b,len)
Reg1 char *a;
Reg2 char *b;
Reg3 int len;
{
    while (len) {
	if (isspace(*b)) {		/* whitespace (or \n) to match? */
	    if (!isspace(*a))		/* no corresponding whitespace? */
		return FALSE;
	    while (len && isspace(*b) && *b != '\n')
		b++,len--;		/* skip pattern whitespace */
	    while (isspace(*a) && *a != '\n')
		a++;			/* skip target whitespace */
	    if (*a == '\n' || *b == '\n')
		return (*a == *b);	/* should end in sync */
	}
	else if (*a++ != *b++)		/* match non-whitespace chars */
	    return FALSE;
	else
	    len--;			/* probably not necessary */
    }
    return TRUE;			/* actually, this is not reached */
					/* since there is always a \n */
}

/* Exit with cleanup. */

void
my_exit(status)
int status;
{
    Unlink(TMPINNAME);
    if (!toutkeep) {
	Unlink(TMPOUTNAME);
    }
    if (!trejkeep) {
	Unlink(TMPREJNAME);
    }
    Unlink(TMPPATNAME);
    exit(status);
}
