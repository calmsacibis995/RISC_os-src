/* $Header: sw.h,v 1.1.1.1 89/11/28 01:08:54 wje Exp $
 *
 * $Log:	sw.h,v $
 * Revision 1.1.1.1  89/11/28  01:08:54  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:57:27  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:51:07  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifdef INNERSEARCH
EXT int gline INIT(0);
#endif

void    sw_init();
void    sw_file();
void    sw_list();
void	decode_switch();
void	pr_switches();
void	cwd_check();
