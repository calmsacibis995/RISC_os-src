/* $Header: rcln.h,v 1.1.1.1 89/11/28 01:07:25 wje Exp $
 *
 * $Log:	rcln.h,v $
 * Revision 1.1.1.1  89/11/28  01:07:25  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/05/26  20:56:58  kris
 * Initial revision
 * 
 * Revision 4.3  85/05/01  11:45:52  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifdef DEBUGGING
EXT ART_NUM ngmax[MAXRCLINE];
#endif

void    rcln_init();
#ifdef CATCHUP
    void	catch_up();
#endif
int	addartnum();
#ifdef MCHASE
    void	subartnum();
#endif
void	prange();
void	set_toread();
void	checkexpired();
