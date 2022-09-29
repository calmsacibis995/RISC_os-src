/* $Header: ng.h,v 1.1.1.1 89/11/28 01:05:44 wje Exp $
 *
 * $Log:	ng.h,v $
 * Revision 1.1.1.1  89/11/28  01:05:44  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:56:23  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:44:29  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT ART_NUM art INIT(0);	/* current or prospective article # */

EXT int checkcount INIT(0);	/* how many articles have we read */
			/*   in the current newsgroup since */
			/*   the last checkpoint? */
EXT int docheckwhen INIT(20);	/* how often to do checkpoint */

#ifdef MAILCALL
EXT int mailcount INIT(0);			/* check for mail when 0 mod 10 */
#endif
EXT char *mailcall INIT(nullstr);

EXT bool forcelast INIT(FALSE);			/* ought we show "End of newsgroup"? */
EXT bool forcegrow INIT(FALSE);		/* do we want to recalculate size */
				    /* of newsgroup, e.g. after posting? */

#define NG_ERROR -1
#define NG_NORM 0
#define NG_ASK 1
#define NG_MINUS 2

void    ng_init();
int	do_newsgroup();
int	art_switch();
#ifdef MAILCALL
    void	setmail();
#endif
void	setdfltcmd();
