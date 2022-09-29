/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: sigsetops.c,v 1.5.1.2 90/05/10 04:14:07 wje Exp $"

#include <limits.h>
#include <signal.h>
#include <errno.h>

static	int	__maxsigs = 0;

int 
sigemptyset(set)
sigset_t	*set;
{
	int	i;
	
	for (i = 0; i < _SIGSETS; set->sig_bits[i++] = 0);
	return(0);
}

int 
sigfillset(set)
sigset_t *set;
{
	int 	i;

	for (i = 0; i < _SIGSETS; set->sig_bits[i++] = UINT_MAX);
	return(0);
}

int 
sigaddset(set, signo)
sigset_t *set; 
int signo;
{
	if (__maxsigs == 0)
		__maxsigs = __getmaxsig();

	if (signo <= 0 || signo > __maxsigs) {
		errno = EINVAL;
		return(-1);
	}
	set->sig_bits[(signo-1)/WORD_BIT] |= (1 << ((signo-1) % WORD_BIT));
	return(0);
}

int 
sigdelset(set, signo)
sigset_t *set; 
int signo;
{
	if (__maxsigs == 0)
		__maxsigs = __getmaxsig();

	if (signo <= 0 || signo > __maxsigs) {
		errno = EINVAL;
		return(-1);
	}
	set->sig_bits[(signo-1)/WORD_BIT] &= ~(1 << ((signo-1) % WORD_BIT));
	return(0);
}

int 
sigismember(set, signo)
sigset_t *set; 
int signo;
{
	if (__maxsigs == 0)
		__maxsigs = __getmaxsig();

	if (signo <= 0 || signo > __maxsigs) {
		errno = EINVAL;
		return(-1);
	}
	return((set->sig_bits[(signo-1)/WORD_BIT] & (1<<((signo-1)%WORD_BIT))) != 0);
}
