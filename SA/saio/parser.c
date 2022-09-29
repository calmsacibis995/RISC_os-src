#ident "$Header: parser.c,v 1.2 90/01/16 17:26:49 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * parser.c -- command parser
 */

#include "sys/param.h"
#include "sys/file.h"
#include "prom/entrypt.h"
#include "saio/saio.h"
#include "saio/parser.h"
#include "saio/setjmp.h"
#include "saio/stringlist.h"
#include "saio/ctype.h"

#define	streq(x,y)	(strcmp(x, y) == 0)

static struct cmd_table *lookup_cmd();
static char *get_word();
static char *expand();
static char *get_var();
static char *get_quote();
static jmp_buf err_buf;

/*
 * command_parser -- get input line and vector to appropriate command routine
 */
command_parser(cmd_table, prompt, search_path)
struct cmd_table *cmd_table;
char *prompt;
{
	register struct cmd_table *ct;
	int argc;
	char linebuf[LINESIZE];
	struct string_list argv;
	struct promexec_args pa;
	extern char **environ;
	extern int ignore_xoff;

	while (1) {
		setjmp(err_buf);
		ignore_xoff = 0;
		_init_stdio();
		printf("%s", prompt);
		gets(linebuf);
		argc = _argvize(expand(linebuf, 0), &argv);
		if (argc == 0)
			continue;
		ct = lookup_cmd(cmd_table, argv.strptrs[0]);
		/*
		 * if not an internal command try searching path
		 * for a file to boot, if search_path is non-zero
		 */
		if (!ct) {
			if (search_path) {
				pa.pa_bootfile = argv.strptrs[0];
				pa.pa_argc = argv.strcnt;
				pa.pa_argv = argv.strptrs;
				pa.pa_environ = environ;
				pa.pa_flags = 0;
				promexec(&pa);
			}
			printf("%s: Command not found.\n", argv.strptrs[0]);
			continue;
		}
		if ((*ct->ct_routine)(argc, argv.strptrs, cmd_table))
			usage(ct);
	}
}

/*
 * _argvize -- break input line into tokens
 */
_argvize(linebuf, slp)
char *linebuf;
struct string_list *slp;
{
	register argc;

	init_str(slp);
	for (argc = 0; *linebuf; argc++)
		new_str1(get_word(&linebuf), slp);

	return(argc);
}

/*
 * expand -- deal with environment variables and redirection
 */
static char *
expand(cp, ignore_esc)
char *cp;
int ignore_esc;
{
	static char buf[LINESIZE];
	char tmp[LINESIZE];
	register char *bp;
	register char *wp;
	register char c;
	extern char *getenv();

	bp = buf;
	while (*cp) {
		switch (c = *cp) {

		/*
		 * someday, it would be nice to have history
		 */
		case '$':
			wp = getenv(get_var(&cp));
			if (!wp)
				parse_error("$variable not defined");
			while (*wp && bp < &buf[LINESIZE-1])
				*bp++ = *wp++;
			break;

		case '"':
			/*
			 * pay the price for recursion!
			 */
			*bp = 0;
			strcpy(tmp, buf);
			bp = &tmp[strlen(tmp)];
			wp = expand(get_quote(&cp, '"'), 1);
			if (bp < &tmp[LINESIZE-1])
				*bp++ = '"';
			while (*wp && bp < &tmp[LINESIZE-1])
				*bp++ = *wp++;
			if (bp < &tmp[LINESIZE-1])
				*bp++ = '"';
			*bp = 0;
			strcpy(buf, tmp);
			bp = &buf[strlen(buf)];
			break;

		case '\'':
			wp = get_quote(&cp, '\'');
			if (bp < &buf[LINESIZE-1])
				*bp++ = '\'';
			while (*wp && bp < &buf[LINESIZE-1])
				*bp++ = *wp++;
			if (bp < &buf[LINESIZE-1])
				*bp++ = '\'';
			break;

		case '\\':
			if (bp < &buf[LINESIZE-1]) {
				*bp++ = c;
				c = *++cp;
			}
			if (ignore_esc && c == '$')
				break;
			/* fall into default case */

		default:
			if (bp < &buf[LINESIZE-1])
				*bp++ = c;
			cp++;
			break;
		}
	}
	if (bp >= &buf[LINESIZE-1])
		parse_error("Line too long");
	*bp = 0;
	return(buf);
}

/*
 * get_var -- environment variable lexical routine
 */
static char *
get_var(cpp)
char **cpp;
{
	static char buf[LINESIZE];
	char *cp;
	register char *bp;
	register char *wp;
	register char c;

	bp = buf;
	cp = *cpp + 1;	/* skip leading $ */
	if (*cp == '{') {
		wp = get_quote(&cp, '}');
		while (*wp)
			*bp++ = *wp++;
	} else
		for (; (c = *cp) && isalnum(c); cp++)
			*bp++ = c;
	*bp = 0;
	*cpp = cp;
	return(buf);
}

/*
 * get_quote -- quoted string lexical routine
 */
static char *
get_quote(cpp, q)
char **cpp;
char q;
{
	static char buf[LINESIZE];
	register char *cp;
	register char *bp;
	register char c;

	bp = buf;
	for (cp = *cpp + 1; (c = *cp) && c != q; cp++) {
		if (c == '\\') {	/* \'s are protected inside quotes */
			*bp++ = c;
			c = *++cp;
		}
		*bp++ = c;
	}
	if (c != q)
		parse_error("quote syntax");
	*bp = 0;
	*cpp = ++cp;		/* skip trailing quote mark */
	return(buf);
}

/*
 * get_word -- break line into "words"
 */
static char *
get_word(cpp)
char **cpp;
{
	static char word[LINESIZE];
	register char *wp;
	register c;
	char *cp;

	cp = *cpp;

	while (isspace(*cp))
		cp++;

	wp = word;
	while ((c = *cp) && !isspace(c) && wp < &word[LINESIZE-1]) {
		switch (c) {

		case '"':
		case '\'':
			cp++;	/* skip opening quote */

			while (*cp && *cp != c && wp < &word[LINESIZE-2])
				*wp++ = *cp++;

			if (*cp == c) /* skip closing quote */
				cp++;
			break;

		default:
			*wp++ = *cp++;
			break;
		}
	}

	while (isspace(*cp))
		cp++;

	*cpp = cp;
	*wp = 0;
	if (wp == &word[LINESIZE-1])
		parse_error("Line too long");
	return(word);
}

/*
 * parse_error -- deal with syntax errors gracefully
 */
parse_error(msg)
char *msg;
{
	printf("%s\n", msg);
	longjmp(err_buf, 1);
}

/*
 * lookup_cmd -- search cmd table
 */
static struct cmd_table *
lookup_cmd(cmd_table, cp)
struct cmd_table *cmd_table;
char *cp;
{
	register struct cmd_table *ct;

	for (ct = cmd_table; ct->ct_string; ct++)
		if (streq(ct->ct_string, cp))
			return(ct);
	return((struct cmd_table *)0);
}

/*
 * usage -- print usage line for a command
 */
static
usage(ct)
struct cmd_table *ct;
{
	printf("Usage: %s\n", ct->ct_usage);
}

/*
 * help -- print usage line for all commands
 */
help(argc, argv, cmd_table)
int argc;
char **argv;
struct cmd_table *cmd_table;
{
	register struct cmd_table *ct;

	if (argc > 1) {
		while (--argc > 0) {
			argv++;
			ct = lookup_cmd(cmd_table, *argv);
			if (ct)
				printf("%s\n", ct->ct_usage);
			else
				printf("Not a command: %s\n", *argv);
		}
		return(0);
	}
	printf("COMMANDS:\n");
	for (ct = cmd_table; ct->ct_string; ct++)
		printf("\t%s\n", ct->ct_usage);
	printf("\nCOMMAND FLAGS\n");
	printf("\tcommands that reference memory take widths of:\n");
	printf("\t\t-b -- byte, -h -- halfword, -w -- word (default)\n");
	printf("\tRANGE's are specified as one of:\n");
	printf("\t\tBASE_ADDRESS#COUNT\n");
	printf("\t\tSTART_ADDRESS:END_ADDRESS\n");
	printf("Erase single characters by CTRL-H or DEL\n");
	printf("Rubout entire line by CTRL-U\n");
	return(0);
}

/*
 * parse_range -- interpret "range" specifications
 */
parse_range(cp, basep, cntp)
char *cp;
unsigned *basep;
int *cntp;
{
	char *oldcp = cp;
	int range_type;
	extern char *atob();

	cp = atob(cp, basep);
	switch (*cp) {

	case ':':
		if (*atob(++cp, cntp))
			goto bad;
		range_type = ADDR_RANGE;
		break;

	case '#':
		if (*atob(++cp, cntp))
			goto bad;
		range_type = CNT_RANGE;
		break;

	case '\0':
		range_type = CNT_RANGE;
		*cntp = 1;
		break;

	default:
bad:
		range_type = ERROR_RANGE;
		break;
	}
	return (range_type);
}
