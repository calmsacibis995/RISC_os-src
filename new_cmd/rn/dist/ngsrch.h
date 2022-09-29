/* $Header: ngsrch.h,v 1.1.1.1 89/11/28 01:06:20 wje Exp $
 *
 * $Log:	ngsrch.h,v $
 * Revision 1.1.1.1  89/11/28  01:06:20  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:56:35  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:44:56  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifdef NGSEARCH
#define NGS_ABORT 0
#define NGS_FOUND 1
#define NGS_INTR 2
#define NGS_NOTFOUND 3

EXT bool ng_doread INIT(FALSE);		/* search read newsgroups? */
#endif

void	ngsrch_init();
#ifdef NGSEARCH 
    int		ng_search();
    bool	ng_wanted();
#endif
#ifdef NGSORONLY
    char	*ng_comp();
#endif
