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
/* $Header: mkboot.h,v 1.5.2.2 90/05/09 16:20:32 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	<sys/types.h>
# include	"boothdr.h"
# include	<string.h>
# include	<setjmp.h>
#ifdef SYS4.2
#define DIRSIZ	14
#define dirent direct
#else
#include <sys/dir.h>
#endif


/*
 * CONSTANTS
 */

# define	TRUE		(2>1)
# define	FALSE		(1>2)

# define	MAXLITERAL	((1<<16)-1)	/* maximum literal that will fit in a struct format */

/*
 * ARRAY SIZES
 */

# define	MAXDEP		25	/* maximum dependencies per module */
# define	MAXRTN		200	/* maximum routine definitions per module */
# define	MAXSTRING	2000	/* maximum size of all strings/names per module */


/*
 * MACROS
 */

# define	min(x,y)	(((x)<=(y))? (x) : (y))
# define	max(x,y)	(((x)>=(y))? (x) : (y))
# define	umin(x,y)	(((unsigned)(x)<=(unsigned)(y))? (x) : (y))
# define	umax(x,y)	(((unsigned)(x)>=(unsigned)(y))? (x) : (y))


/*
 * STRUCTURES
 */

extern struct master		master;
extern struct depend		depend[], *ndepend;
extern struct routine		routine[], *nroutine;
extern char			string[], *nstring;

extern jmp_buf			*jmpbuf;
extern char			any_error;

/*
 * Function declarations
 */
typedef unsigned char	boolean;

extern char		*basename();
extern int		build_header();
extern boolean		check_master();
extern char		copy_driver();
extern char		*copystring();
extern void		fatal();
extern void		getparam();
extern int		getsize();
extern char		*lcase();
extern int		mylseek();
extern void		myperror();
extern int		myread();
extern int		mywrite();
extern struct tnode	*node();
extern void		print_expression();
extern char		*print_flag();
extern void		print_master();
extern char		*ucase();
extern void		warn();
extern void		yyerror();
extern void		yyfatal();
extern int		yyparse();

extern int		close();
extern void		exit();
extern int		fclose();
extern char		*fgets();
extern FILE		*fopen();
extern int 		fprintf();
extern void		free();
extern int		getopt();
extern int		getpid();
extern int		link();
extern long		lseek();
extern char		*malloc();
extern char		*memcpy();
extern int		open();
extern void 		perror();
extern int 		printf();
extern void 		qsort();
extern int		read();
extern int 		sprintf();
extern long		strtol();
extern long		time();
extern int 		tolower();
extern int 		toupper();
extern int		write();
extern int		unlink();
