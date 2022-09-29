/* $Header: init.h,v 1.1.1.1 89/11/28 01:02:29 wje Exp $
 *
 * $Log:	init.h,v $
 * Revision 1.1.1.1  89/11/28  01:02:29  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:55:20  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:40:46  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *lockname INIT(nullstr);

bool	initialize();
void	lock_check();
void	newsnews_check();
void	version_check();
