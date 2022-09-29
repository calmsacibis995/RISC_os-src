/* $Header: addng.h,v 1.1.1.1 89/11/28 00:59:26 wje Exp $
 *
 * $Log:	addng.h,v $
 * Revision 1.1.1.1  89/11/28  00:59:26  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:54:22  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:34:48  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

void	addng_init();
#ifdef FINDNEWNG
    bool	newlist();
    long	birthof();
    bool	scanactive();
#endif
