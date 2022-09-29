/* $Header: backpage.h,v 1.1.1.1 89/11/28 01:00:43 wje Exp $
 *
 * $Log:	backpage.h,v $
 * Revision 1.1.1.1  89/11/28  01:00:43  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:54:47  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:36:11  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

/* things for doing the 'back page' command */

EXT int varyfd INIT(0);			/* virtual array file for storing  */
					/* file offsets */
EXT ART_POS varybuf[VARYSIZE];		/* current window onto virtual array */

EXT long oldoffset INIT(-1);		/* offset to block currently in window */

void	backpage_init();
ART_POS	vrdary();
void	vwtary();
