/*	@(#)mailstats.h	1.2 88/05/05 4.0NFSSRC SMI;	from UCB 5.1 5/2/86	*/
/*				@(#) from SUN 1.7	*/

/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
**
**
*/

/*
**  Statistics structure.
*/
# define NAMELEN 16	/* length of each mailer name */

struct statistics
{
	time_t	stat_itime;		/* file initialization time */
	short	stat_size;		/* size of this structure */
	long	stat_nf[MAXMAILERS];	/* # msgs from each mailer */
	long	stat_bf[MAXMAILERS];	/* kbytes from each mailer */
	long	stat_nt[MAXMAILERS];	/* # msgs to each mailer */
	long	stat_bt[MAXMAILERS];	/* kbytes to each mailer */
	char 	stat_names[MAXMAILERS][NAMELEN];
					/* symbolic names of mailers */
};
