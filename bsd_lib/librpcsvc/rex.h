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
/* $Header: rex.h,v 1.2.1.3 90/05/09 14:45:14 wje Exp $ */

# ifdef lint
static char sccsid[] = "@(#)rex.h 1.1 86/09/25 Copyr 1985 Sun Micro";
# endif lint

/*
 * rex - remote execution server definitions
 *
 * Copyright (c) 1985 Sun Microsystems, Inc.
 */

#define	REXPROG		100017
#define	REXPROC_NULL	0	/* no operation */
#define	REXPROC_START	1	/* start a command */
#define	REXPROC_WAIT	2	/* wait for a command to complete */
#define	REXPROC_MODES	3	/* send the tty modes */
#define REXPROC_WINCH	4	/* signal a window change */
#define REXPROC_SIGNAL	5	/* other signals */

#define	REXVERS	1

/* flags for rst_flags field */
#define REX_INTERACTIVE		1	/* Interative mode */

struct rex_start {
  /*
   * Structure passed as parameter to start function
   */
	char	**rst_cmd;	/* list of command and args */
	char	*rst_host;	/* working directory host name */
	char	*rst_fsname;	/* working directory file system name */
	char	*rst_dirwithin;	/* working directory within file system */
	char	**rst_env;	/* list of environment */
	u_short	rst_port0;	/* port for stdin */
	u_short	rst_port1;	/* port for stdin */
	u_short	rst_port2;	/* port for stdin */
	u_long	rst_flags;	/* options - see #defines above */
};

bool_t xdr_rex_start();

struct rex_result {
  /*
   * Structure returned from the start function
   */
   	int	rlt_stat;	/* integer status code */
	char	*rlt_message;	/* string message for human consumption */
};
bool_t xdr_rex_result();

struct rex_ttymode {
    /*
     * Structure sent to set-up the tty modes
     */
	struct sgttyb basic;	/* standard unix tty flags */
	struct tchars more;	/* interrupt, kill characters, etc. */
	struct ltchars yetmore;	/* special Bezerkeley characters */
	u_long andmore;		/* and Berkeley modes */
};

struct rex_ttysize {
	int ts_lines;
	int ts_cols;
};
bool_t xdr_rex_ttymode();
bool_t xdr_rex_ttysize();
