/* $Header: ngdata.h,v 1.1.1.1 89/11/28 01:06:05 wje Exp $
 *
 * $Log:	ngdata.h,v $
 * Revision 1.1.1.1  89/11/28  01:06:05  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:56:30  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:44:48  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT FILE *actfp INIT(Nullfp);	/* the active file */
EXT bool writesoft INIT(FALSE);	/* rewrite the soft pointer file? */
EXT int softtries INIT(0), softmisses INIT(0);

#ifdef CACHEFIRST
    EXT ART_NUM abs1st[MAXRCLINE];	/* 1st real article in newsgroup */
#else
# ifdef MININACT
    EXT ART_NUM abs1st INIT(0);
# endif
#endif


void	ngdata_init();
ART_NUM	getngsize();
ACT_POS findact();
ART_NUM	getabsfirst();
ART_NUM	getngmin();
