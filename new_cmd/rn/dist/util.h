/* $Header: util.h,v 1.1.1.1 89/11/28 01:09:23 wje Exp $
 *
 * $Log:	util.h,v $
 * Revision 1.1.1.1  89/11/28  01:09:23  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:57:37  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:51:58  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT bool waiting INIT(FALSE);		/* are we waiting for subprocess (in doshell)? */
EXT int len_last_line_got INIT(0);
			/* strlen of some_buf after */
			/*  some_buf = get_a_line(bufptr,buffersize,fp) */

/* is the string for makedir a directory name or a filename? */

#define MD_DIR 0
#define MD_FILE 1

void	util_init();
int	doshell();
char	*safemalloc();
char	*saferealloc();
char	*safecpy();
char	*safecat();
char	*cpytill();
char	*instr();
#ifdef SETUIDGID
    int		eaccess();
#endif
char	*getwd();
void	cat();
void	prexit();
char	*get_a_line();
char	*savestr();
int	makedir();
void	setenv();
int	envix();
void	notincl();
char	*getval();
void	growstr();
void	setdef();
