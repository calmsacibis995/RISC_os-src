/* $Header: rn.h,v 1.1.1.1 89/11/28 01:08:19 wje Exp $
 *
 * $Log:	rn.h,v $
 * Revision 1.1.1.1  89/11/28  01:08:19  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:57:15  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:48:19  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

EXT char *ngname INIT(Nullch);		/* name of current newsgroup */
EXT int ngnlen INIT(0);			/* current malloced size of ngname */
EXT char *ngdir INIT(Nullch);		/* same thing in directory name form */
EXT int ngdlen INIT(0);			/* current malloced size of ngdir */

EXT NG_NUM ng INIT(0);		/* current newsgroup index into rcline and toread */
EXT NG_NUM current_ng INIT(0);	/* stable current newsgroup so we can ditz with ng */
EXT NG_NUM starthere INIT(0);   /* set to the first newsgroup with unread news on startup */
EXT char *spool INIT(Nullch);		/* public news spool directory */

void	rn_init();
void	main();
void	set_ngname();
char	*getngdir();
