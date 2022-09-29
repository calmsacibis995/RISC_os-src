/* $Header: ngstuff.h,v 1.1.1.1 89/11/28 01:06:34 wje Exp $
 *
 * $Log:	ngstuff.h,v $
 * Revision 1.1.1.1  89/11/28  01:06:34  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:56:40  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:45:12  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#define NN_NORM 0
#define NN_INP 1
#define NN_REREAD 2
#define NN_ASK 3

void	ngstuff_init();
int	escapade();
int	switcheroo();
int	numnum();
int	perform();
