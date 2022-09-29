/* $Header: kfile.h,v 1.1.1.1 89/11/28 01:03:00 wje Exp $
 *
 * $Log:	kfile.h,v $
 * Revision 1.1.1.1  89/11/28  01:03:00  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:55:30  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:42:00  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#define KF_GLOBAL 0
#define KF_LOCAL 1

#ifdef KILLFILES
EXT FILE *globkfp INIT(Nullfp);		/* global article killer file */
EXT FILE *localkfp INIT(Nullfp);	/* local (for this newsgroup) */
					/*  article killer file */
#endif

void	kfile_init();
int	do_kfile();
void	kill_unwanted();
int	edit_kfile();
void	open_kfile();
void    kf_append();
void	setthru();

