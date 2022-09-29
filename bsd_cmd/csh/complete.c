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
#ident	"$Header: complete.c,v 1.3.1.2 90/05/07 18:12:49 wje Exp $"


/*
 * Completion for tsh.  Taken from the ncsh "tenex" completion routines by Ken
 * Greer and Mike Ellis but changed to make the "complete" and "recognize"
 * functions both happen at once, eliminating the need for two different keys.
 *
 * All routines herein are internal except complete().  Complete returns the
 * number of matches it found; if it found more than 1, it will have already
 * printed the choices in columnar format.  The command string passed to it
 * is modified by appending the unique completion to it (the old recognize
 * function), but nothing in the command line is printed and no bell is
 * sounded.
 *
 * --bruce, 18 sep 84
 *
 * Taken from:
 * 	"Tenex style file name recognition, .. and more."
 * History:
 *	Author: Ken Greer, Sept. 1975, CMU.
 *	Finally got around to adding to the Cshell., Ken Greer, Dec. 1981.
 *
 *	Search and recognition of command names (in addition to file names)
 *	by Mike Ellis, Fairchild A.I. Labs, Sept 1983.
 *
 *	Changed declarations of the form 'char foo[] = "abc"' to
 *	'char *foo = "abc"' so xstr doesn't foul them up. Also eliminate
 *	cmd expansion near beginning of line.--Steven Correll, MIPS
 *	30 Oct 85.
 *
 */

#include "sh.h"
#include "tsh.h"
#include <sys/dir.h>
#include <pwd.h>

#define ON		1
#define OFF		0
#define FILSIZ		512		/* Max reasonable file name length */
#define ESC		'\033'
#define equal(a, b)	(strcmp(a, b) == 0)
#define is_set(var)	adrof(var)
#define BUILTINS	"/usr/local/lib/builtins/" /* fake builtin bin */

extern short SHIN, SHOUT;
extern char *getenv ();

typedef enum {LIST, RECOGNIZE} COMMAND;

/*
 * Concatonate src onto tail of des.
 * Des is a string whose maximum length is count.
 * Always null terminate.
 */
catn (des, src, count)
register char *des, *src;
register count;
{
    while (--count >= 0 && *des)
    des++;
    while (--count >= 0)
	if ((*des++ = *src++) == 0)
	    return;
    *des = '\0';
}

static
max (a, b)
{
    if (a > b)
	return (a);
    return (b);
}

/*
 * like strncpy but always leave room for trailing \0
 * and always null terminate.
 */
copyn (des, src, count)
register char *des, *src;
register count;
{
    while (--count >= 0)
	if ((*des++ = *src++) == 0)
	    return;
    *des = '\0';
}

/*
 * For qsort()
 */
static
fcompare (file1, file2)
char  **file1, **file2;
{
    return (strcmp (*file1, *file2));
}

static char
filetype (dir, file)
char *dir, *file;
{
    if (dir)
    {
	char path[512];
	struct stat statb;
	strcpy (path, dir);
	catn (path, file, sizeof path);
	if (stat (path, &statb) >= 0)
	{
	    if ((statb.st_mode & S_IFMT) == S_IFDIR)
		return ('/');
	    if (statb.st_mode & 0111)
		return ('*');
	}
    }
    return (' ');
}

/*
 * Print sorted down columns
 */
static
print_by_column (dir, items, count, looking_for_command)
register char *dir, *items[];
{
    register int i, rows, r, c, maxwidth = 0, columns;
    for (i = 0; i < count; i++)
	maxwidth = max (maxwidth, strlen (items[i]));
    maxwidth += looking_for_command ? 1:2;	/* for the file tag and space */
    columns = MAX_LINE_LEN / maxwidth;
    if( columns <= 0 )  columns = 1;
    rows = (count + (columns - 1)) / columns;
    for (r = 0; r < rows; r++)
    {
	cshputchar('\n');
	for (c = 0; c < columns; c++)
	{
	    i = c * rows + r;
	    if (i < count)
	    {
		register int w;
		printf("%s", items[i]);
		w = strlen (items[i]);
		/* Print filename followed by '/' or '*' or ' ' */
		if (!looking_for_command)
			cshputchar (filetype (dir, items[i])), w++;
		if (c < (columns - 1))			/* Not last column? */
		    for (; w < maxwidth; w++)
			cshputchar (' ');
	    }
	}
    }
}

/*
 * expand "old" file name with possible tilde usage
 *		~person/mumble
 * expands to
 *		home_directory_of_person/mumble
 * into string "new".
 */

char *
tilde (new, old)
char *new, *old;
{
    extern struct passwd *getpwuid (), *getpwnam ();

    register char *o, *p;
    register struct passwd *pw;
    static char person[40] = {0};

    if (old[0] != '~')
    {
	strcpy (new, old);
	return (new);
    }

    for (p = person, o = &old[1]; *o && *o != '/'; *p++ = *o++);
    *p = '\0';

    if (person[0] == '\0')			/* then use current uid */
	pw = getpwuid (getuid ());
    else
	pw = getpwnam (person);

    if (pw == NULL)
	return (NULL);

    strcpy (new, pw -> pw_dir);
    (void) strcat (new, o);
    return (new);
}

/*
 * parse full path in file into 2 parts: directory and file names
 * Should leave final slash (/) at end of dir.
 */
static
extract_dir_and_name (path, dir, name)
char   *path, *dir, *name;
{
    extern char *rindex ();
    register char  *p;
    p = rindex (path, '/');
    if (p == NULL)
    {
	copyn (name, path, MAXNAMLEN);
	dir[0] = '\0';
    }
    else
    {
	p++;
	copyn (name, p, MAXNAMLEN);
	copyn (dir, path, p - path);
    }
}


char *
getentry (dir_fd, looking_for_lognames)
DIR *dir_fd;
{
    if (looking_for_lognames)			/* Is it login names we want? */
    {
	extern struct passwd *getpwent ();
	register struct passwd *pw;
	if ((pw = getpwent ()) == NULL)
	    return (NULL);
	return (pw -> pw_name);
    }
    else					/* It's a dir entry we want */
    {
	register struct direct *dirp;
	if (dirp = readdir (dir_fd))
	    return (dirp -> d_name);
	return (NULL);
    }
}

static
free_items (items)
register char **items;
{
    register int i;
    for (i = 0; items[i]; i++)
	free (items[i]);
    free (items);
}

#define FREE_ITEMS(items)\
{\
    int omask; \
    omask = sigblock(sigmask(SIGINT)); \
    free_items (items);\
    items = NULL;\
    (void) sigsetmask(omask); \
}

#define FREE_DIR(fd)\
{\
    int omask; \
    omask = sigblock(sigmask(SIGINT)); \
    closedir (fd);\
    fd = NULL;\
    (void) sigsetmask(omask); \
}

static int  dirctr;		/* -1 0 1 2 ... */
static char dirflag[5];		/*  ' nn\0' - dir #s -  . 1 2 ... */

/*
 * Strip next directory from path; return ptr to next unstripped directory.
 */
 
char *extract_dir_from_path (path, dir)
char *path, dir[];
{
    register char *d = dir;

    while (*path && (*path == ' ' || *path == ':')) path++;
    while (*path && (*path != ' ' && *path != ':')) *(d++) = *(path++);
    while (*path && (*path == ' ' || *path == ':')) path++;

    ++dirctr;
    if (*dir == '.')
        strcpy (dirflag, " .");
    else
    {
        dirflag[0] = ' ';
	if (dirctr <= 9)
	{
		dirflag[1] = '0' + dirctr;
		dirflag[2] = '\0';
	}
	else
	{
		dirflag[1] = '0' + dirctr / 10;
		dirflag[2] = '0' + dirctr % 10;
		dirflag[3] = '\0';
	}
    }
    *(d++) = '/';
    *d = 0;

    return path;
}

/*
 * Perform a RECOGNIZE or LIST command on string "word".
 */
static
search (word, wp, max_word_length, looking_for_command)
char   *word,
       *wp;			/* original end-of-word */
int max_word_length, looking_for_command;
{
#   define MAXITEMS 2048
    register numitems,
	    name_length,		/* Length of prefix (file name) */
	    looking_for_lognames;	/* True if looking for login names */
    int	    showpathn;			/* True if we want path number */
    struct stat
	    dot_statb,			/* Stat buffer for "." */
	    curdir_statb;		/* Stat buffer for current directory */
    int	    dot_scan,			/* True if scanning "." */
	    dot_got;			/* True if have scanned dot already */
    char    tilded_dir[FILSIZ + 1],	/* dir after ~ expansion */
	    dir[FILSIZ + 1],		/* /x/y/z/ part in /x/y/z/f */
            name[MAXNAMLEN + 1],	/* f part in /d/d/d/f */
            extended_name[MAXNAMLEN+1],	/* the recognized (extended) name */
            *entry,			/* single directory entry or logname */
	    *path;			/* hacked PATH environment variable */
    static DIR 
	    *dir_fd = NULL;
    static char
           **items = NULL;		/* file names when doing a LIST */

    if (items != NULL)
	FREE_ITEMS (items);
    if (dir_fd != NULL)
	FREE_DIR (dir_fd);

    looking_for_lognames = (*word == '~') && (index (word, '/') == NULL);
    looking_for_command &= (*word != '~') && (index (word, '/') == NULL);

    if (looking_for_command) {
        copyn (name, word, MAXNAMLEN);
        if ((path = getenv ("PATH")) == NULL)
	    path = "";
	/* setup builtins as 1st to search before PATH */
	copyn (dir, BUILTINS, sizeof dir);

	dirctr = -1;		/* BUILTINS -1 */
	dirflag[0] = 0;
    }
    numitems = 0;

    dot_got = FALSE;
    stat (".", &dot_statb);

cmdloop:	/* One loop per directory in PATH, if looking_for_command */

    if (looking_for_lognames) {			/* Looking for login names? */
	setpwent ();				/* Open passwd file */
	copyn (name, &word[1], MAXNAMLEN);	/* name sans ~ */
    }
    else    {						/* Open directory */
        if (!looking_for_command)
	    extract_dir_and_name (word, dir, name);
	if ((tilde (tilded_dir, dir) == 0) ||	/* expand ~user/... stuff */
	   ((dir_fd = opendir (*tilded_dir ? tilded_dir : ".")) == NULL)) {
	    if (looking_for_command)
	        goto try_next_path;
	    else
		return (0);
	}
	dot_scan = FALSE;
	if (looking_for_command) {
	    /*
	     * Are we searching "."?
	     */
	    fstat (dir_fd->dd_fd, &curdir_statb);
	    if (curdir_statb.st_dev == dot_statb.st_dev &&
	        curdir_statb.st_ino == dot_statb.st_ino) {
	        if (dot_got)			/* Second time in PATH? */
			goto try_next_path;
		dot_scan = TRUE;
		dot_got = TRUE;
	    }
	}
    }

    name_length = strlen (name);
    showpathn = looking_for_command && is_set("listpathnum");

    while (entry = getentry (dir_fd, looking_for_lognames)) {
	if (!is_prefix (name, entry))
	    continue;

	/*
	 * Don't match . files on null prefix match
	 */
	if (name_length == 0 && entry[0] == '.' && !looking_for_lognames)
	    continue;

	/*
	 * Skip non-executables if looking for commands:
	 * Only done for directory "." for speed.
	 * (Benchmarked with and without:
	 * With filetype check, a full search took 10 seconds.
	 * Without filetype check, a full search took 1 second.)
	 *                                   -Ken Greer
         */
	if (looking_for_command && dot_scan && filetype (dir, entry) != '*')
	    continue;

	{
	    extern char *malloc ();
	    register int length;
	    if (numitems >= MAXITEMS) {
		printf ("\nYikes!! Too many %s!!\n",
		    looking_for_lognames ? "names in password file":"files");
		break;
	    }
	    if (items == NULL) {
		items = (char **) calloc (sizeof (items[1]), MAXITEMS + 1);
		if (items == NULL)
		    break;
	    }
	    length = strlen(entry) + 1;
	    if (showpathn)
		length += strlen(dirflag);
	    if ((items[numitems] = malloc (length)) == NULL) {
		printf ("out of mem\n");
		break;
	    }
	    copyn (items[numitems], entry, MAXNAMLEN);
	    if (showpathn)
	        catn (items[numitems], dirflag, MAXNAMLEN);
	    recognize (extended_name, entry, name_length, ++numitems);
	}
    }

    if (looking_for_lognames)
	endpwent ();
    else
	FREE_DIR (dir_fd);

try_next_path:
    if (looking_for_command && *path &&
    	(path = extract_dir_from_path (path, dir), dir)) 
    	goto cmdloop;
    
    if (numitems > 0)    {
	if (looking_for_lognames)
	    copyn (word, "~", 1);
	else if (looking_for_command)
	    word[0] = 0;
	else
	    copyn (word, dir, max_word_length);		/* put back dir part */
	catn (word, extended_name, max_word_length);	/* add extended name */
    }

    if (numitems > 1) {
    	qsort (items, numitems, sizeof (items[1]), fcompare);
	print_by_column (looking_for_lognames ? NULL:tilded_dir, items,
			 numitems, looking_for_command);
    }

    return numitems;
}

/*
 * Object: extend what user typed up to an ambiguity.
 * Algorithm:
 * On first match, copy full entry (assume it'll be the only match) 
 * On subsequent matches, shorten extended_name to the first
 * character mismatch between extended_name and entry.
 */
recognize (extended_name, entry, name_length, numitems)
char *extended_name, *entry;
{
    if (numitems == 1)				/* 1st match */
	copyn (extended_name, entry, MAXNAMLEN);
    else					/* 2nd and subsequent matches */
    {
	register char *x, *ent;
	register int len = 0;
	for (x = extended_name, ent = entry; *x && (*x == *ent++); x++, len++);
	*x = '\0';				/* Shorten at 1st char diff */
    }
}

/*
 * return true if check items initial chars in template
 * This differs from PWB imatch in that if check is null
 * it items anything
 */
static
is_prefix (check, template)
char   *check,
       *template;
{
    register char  *check_char,
                   *template_char;

    check_char = check;
    template_char = template;
    do
	if (*check_char == 0)
	    return (TRUE);
    while (*check_char++ == *template_char++);
    return (FALSE);
}

starting_a_command (wordstart, inputline)
register char *wordstart, *inputline;
{
    static char
        *cmdstart = ";&(|`",
        *cmdalive = " \t'\"";

    while (--wordstart >= inputline)
    {
	if (index (cmdstart, *wordstart))
	    break;
	if (!index (cmdalive, *wordstart))
	    return (FALSE);
    }
    if (wordstart > inputline && *wordstart == '&')	/* Look for >& */
    {
	while (wordstart > inputline &&
			(*--wordstart == ' ' || *wordstart == '\t'));
	if (*wordstart == '>')
		return (FALSE);
    }
    return (TRUE);
}

complete (inputline, inputline_size, num_read)
char   *inputline;		/* match string prefix */
int     inputline_size;		/* max size of string */
int	num_read;		/* # actually in inputline */
{
    char *str_end, *word_start, *cmd_start;
    int space_left;
    int is_a_cmd;		/* UNIX command rather than filename */
    static char 
	 *delims = " '\"\t;&<>()|^%";

    str_end = &inputline[num_read];

   /*
    * Find LAST occurence of a delimiter in the inputline.
    * The word start is one character past it.
    */
    for (word_start = str_end; word_start > inputline; --word_start)
	if (index (delims, word_start[-1]))
	    break;

    space_left = inputline_size - (word_start - inputline) - 1;

    is_a_cmd = starting_a_command (word_start, inputline);

    /* Avoid expanding zero or one character(s) into zillions of commands */
    if (is_a_cmd && ((str_end - word_start) < 2))
	return 0;
   
    return search (word_start, str_end, space_left, is_a_cmd);
}
