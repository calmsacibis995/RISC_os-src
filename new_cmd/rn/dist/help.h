/* $Header: help.h,v 1.1.1.1 89/11/28 01:02:14 wje Exp $
 *
 * $Log:	help.h,v $
 * Revision 1.1.1.1  89/11/28  01:02:14  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:55:15  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:39:19  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

void	help_init();
int	help_ng();
int	help_art();
int	help_page();
#ifdef ESCSUBS
    int	help_subs();
#endif
