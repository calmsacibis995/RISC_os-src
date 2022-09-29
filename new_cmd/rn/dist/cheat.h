/* $Header: cheat.h,v 1.1.1.1 89/11/28 01:01:13 wje Exp $
 *
 * $Log:	cheat.h,v $
 * Revision 1.1.1.1  89/11/28  01:01:13  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:54:57  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:36:58  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifdef ARTSEARCH
EXT ART_NUM srchahead INIT(0); 	/* are we in subject scan mode? */
				/* (if so, contains art # found or -1) */
#endif

#ifdef PENDING
#   ifdef CACHESUBJ
	EXT ART_NUM subj_to_get;
#   endif
#endif

void	cheat_init();
void	look_ahead();
void	collect_subjects();
