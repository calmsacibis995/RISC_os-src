/* $Header: last.h,v 1.1.1.1 89/11/28 01:03:41 wje Exp $
 *
 * $Log:	last.h,v $
 * Revision 1.1.1.1  89/11/28  01:03:41  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:55:42  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:42:22  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *lastngname INIT(Nullch);	/* last newsgroup read, from .rnlast file */
EXT long lasttime INIT(0);	/* time last rn was started up */
EXT long lastactsiz INIT(0);	/* size of active file when rn last started up */

void	last_init();
void    writelast();
