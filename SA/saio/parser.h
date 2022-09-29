#ident "$Header: parser.h,v 1.2 90/01/16 17:28:45 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * parser.h -- definitions for command parser routines
 */

/*
 * range type for parse_range()
 */
#define	ADDR_RANGE	0		/* address:address */
#define	CNT_RANGE	1		/* address#count */
#define	ERROR_RANGE	-1		/* syntax error */

/*
 * cmd_table -- interface between parser and command execution routines
 * Add new commands by making entry here.
 */
struct cmd_table {
	char *ct_string;		/* command name */
	int (*ct_routine)();		/* implementing routine */
	char *ct_usage;			/* syntax */
};

#define	LINESIZE	128		/* line buffer size */
