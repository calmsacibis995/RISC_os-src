/* $Header: respond.h,v 1.1.1.1 89/11/28 01:07:56 wje Exp $
 *
 * $Log:	respond.h,v $
 * Revision 1.1.1.1  89/11/28  01:07:56  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:57:07  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:47:50  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *savedest INIT(Nullch);	/* value of %b */
EXT ART_POS savefrom INIT(0);		/* value of %B */
EXT char *headname INIT(Nullch);

#define SAVE_ABORT 0
#define SAVE_DONE 1

void	respond_init();
int	save_article();
int	cancel_article();
void	reply();
void	followup();
void	invoke();
