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
#ident	"$Header: interface.c,v 1.5.2.2 90/05/10 00:36:07 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	interface( label )
	provide alternate definitions for the I/O functions through global
	interfaces.
*/
#include	"uucp.h"

#ifdef TLI
#include	<sys/tiuser.h>
#endif /*  TLI  */

#ifdef DATAKIT
#include	<dk.h>

static int	dkteardown();
#endif	/* DATAKIT */

extern int	read(), write(), ioctl();
static int	usetup(), uteardown();

int	(*Read)() = read,
	(*Write)() = write,
	(*Ioctl)() = ioctl,
	(*Setup)() = usetup,
	(*Teardown)() = uteardown;

#ifdef TLI
static int	tread(), twrite(),	/* TLI i/o */
		tioctl(),		/* TLI i/o control */
		tsetup(),		/* TLI setup without streams module */
		tssetup(),		/* TLI setup with streams module */
		tteardown();		/* TLI teardown, works with either setup
					*/
#endif /*  TLI  */
/*	The IN_label in Interface[] imply different caller routines:
	e.g. tlicall().
	If so, the names here and the names in callers.c must match.
*/

static
  struct Interface {
	char	*IN_label;		/* interface name */
	int	(*IN_read)();		/* read fucntion */
	int	(*IN_write)();		/* write fucntion */
	int	(*IN_ioctl)();		/* ioctl fucntion */
	int	(*IN_setup)();		/* setup fucntion, called before first
					i/o operation */
	int	(*IN_teardown)();	/* teardown fucntion, called after last
					i/o operation */
} Interface[] = {
			/* vanilla UNIX */
		{ "UNIX", read, write, ioctl, usetup, uteardown },
#ifdef TCP
			/* same as UNIX */
		{ "TCP", read, write, ioctl, usetup, uteardown },
#endif
#ifdef TLI
			/* AT&T Transport Interface Library WITHOUT streams */
		{ "TLI", tread, twrite, tioctl, tsetup, tteardown },
#ifdef TLIS
			/* AT&T Transport Interface Library WITH streams */
		{ "TLIS", read, write, tioctl, tssetup, uteardown },
#endif /*  TLIS  */
#endif /*  TLI  */
#ifdef DATAKIT
		{ "DK", read, write, ioctl, usetup, dkteardown },
#endif /* DATAKIT */
		{ 0, 0, 0, 0, 0, 0 }
	};


int
interface( label )
char	*label;
{
	register int	i;

	for ( i = 0;  Interface[i].IN_label;  ++i ) {
		if( !strcmp( Interface[i].IN_label, label ) ) {
			Read = Interface[i].IN_read;
			Write = Interface[i].IN_write;
			Ioctl = Interface[i].IN_ioctl;
			Setup = Interface[i].IN_setup;
			Teardown = Interface[i].IN_teardown;
			DEBUG(5, "set interface %s\n", label);
			return( 0 );
		}
	}
	return( FAIL );
}

/*
 *	usetup - vanilla unix setup routine
 */
static
int
usetup( role, fdreadp, fdwritep )
int	*fdreadp, *fdwritep;
{
	if ( role == SLAVE )
	{
		*fdreadp = 0;
		*fdwritep = 1;
	}
	return(SUCCESS);
}

/*
 *	uteardown - vanilla unix teardown routine
 */
static
int
uteardown( role, fdread, fdwrite )
{
	int ret;
	char *ttyn;

	if (role == SLAVE) {
		ret = restline();
		DEBUG(4, "ret restline - %d\n", ret);
		sethup(0);
	}
	if (fdwrite != -1) {
		ttyn = ttyname(fdread);
		if (ttyn != NULL)
			chmod(ttyn, 0644);	/* can fail, but who cares? */
		(void) close(fdread);
		(void) close(fdwrite);
	}
	return(SUCCESS);
}

#ifdef DATAKIT
/*
 *	dkteardown  -  DATAKIT teardown routine
 */
static 
int
dkteardown( role, fdread, fdwrite )
int	role, fdread, fdwrite;
{
	char	*ttyn;

	if ( role == MASTER ) {
		ttyn = ttyname(fdread);
		if ( ttyn != NULL )
			chmod(ttyn, 0644);	/* can fail, but who cares? */
	}

	/*	must flush fd's for datakit	*/
	/*	else close can hang		*/
	if ( ioctl(fdread, DIOCFLUSH, NULL) != 0 )
		DEBUG(4, "dkteardown: DIOCFLUSH of input fd %d failed", fdread);
	if ( ioctl(fdwrite, DIOCFLUSH, NULL) != 0 )
		DEBUG(4, "dkteardown: DIOCFLUSH of output fd %d failed", fdwrite);

	(void)close(fdread);
	(void)close(fdwrite);
	return(SUCCESS);
}
#endif /* DATAKIT */


#ifdef TLI
/*
 *	tread - tli read routine
 */
static
int
tread(fd, buf, nbytes)
int		fd;
char		*buf;
unsigned	nbytes;
{
	int		nreceived, rcvflags;
	extern int	t_errno;

	t_errno = 0;
	if ( t_getstate(fd) != T_DATAXFER ) {
		tfaillog(fd, "tread: unexpected state\n");
		return(FAIL);
	}
	t_errno = 0;
	if ( (nreceived = t_rcv(fd, buf, nbytes, &rcvflags)) < 0 ) {
		tfaillog(fd, "tread: t_rcv\n");
		return(FAIL);
	}
	return(nreceived);

}

/*
 *	twrite - tli write routine
 */
static
int
twrite(fd, buf, nbytes)
int		fd;
char		*buf;
unsigned	nbytes;
{
	int		nsent;
	extern int	t_errno;

	t_errno = 0;
	if ( t_getstate(fd) != T_DATAXFER ) {
		tfaillog(fd, "twrite: unexpected state\n");
		return(FAIL);
	}
	t_errno = 0;
	if ( (nsent = t_snd(fd, buf, nbytes, NULL)) < 0 ) {
		tfaillog(fd, "twrite: t_snd\n");
		return(FAIL);
	}
	return(nsent);
}


/*
 *	tioctl - stub for tli ioctl routine
 */
static
tioctl(fd, request, arg)
int	fd, request;
{
	return(SUCCESS);
}

/*
 *	tsetup - tli setup routine
 *	note blatant assumption that *fdreadp == *fdwritep == 0
 */
static
int
tsetup( role, fdreadp, fdwritep )
int	*fdreadp, *fdwritep;
{
	
	int		initstate = 0;
	struct stat	statbuf;
	extern int	errno, t_errno;

	if ( role == SLAVE ) {
		*fdwritep = *fdreadp = 0;
		errno = t_errno = 0;
		if ( t_sync( *fdreadp ) == -1 ) {
			tfaillog(*fdreadp, "tsetup: t_sync\n");
			return(FAIL);
		}
	}
	return(SUCCESS);
}

/*
 *	tteardown - tli shutdown routine
 */
static
int
tteardown( role, fdread, fdwrite )
{
	(void)t_unbind(fdread);
	(void)t_close(fdread);
	return(SUCCESS);
}

#ifdef TLIS

#include	<sys/stropts.h>
#include	<sys/conf.h>

/*
 *	tssetup - tli, with streams module, setup routine
 *	note blatant assumption that *fdreadp == *fdwritep
 */
static
int
tssetup( role, fdreadp, fdwritep )
int	role;
int	*fdreadp;
int	*fdwritep;
{
	char		strmod[FMNAMESZ], onstream[FMNAMESZ];
	int		optional;
	extern int	getpop(), getpush();

	if ( role == SLAVE ) {
		*fdwritep = *fdreadp = 0;
		DEBUG(5, "tssetup: SLAVE mode: leaving ok\n", 0);
		return(SUCCESS);
	}

	/*	check for streams modules to pop	*/
	while ( getpop(strmod, sizeof(strmod), &optional) ) {
		DEBUG(5, 
			( optional ? "tssetup: optionally POPing %s\n"
				   : "tssetup: POPing %s\n" ),
			strmod);
		if ( ioctl(*fdreadp, I_LOOK, onstream) == -1 ) {
			DEBUG(5, "tssetup: I_LOOK on fd %d failed", *fdreadp);
			return(FAIL);
		}
		if ( strcmp(strmod, onstream) != SAME ) {
			if ( optional )
				continue;
			DEBUG(5, "tssetup: I_POP: %s not there\n", strmod);
			return(FAIL);
		}
		if ( ioctl(*fdreadp, I_POP, 0) == -1 ) {
			DEBUG(5, "tssetup: I_POP on fd %d failed", *fdreadp);
			return(FAIL);
		}
	}

	/*	check for streams modules to push	*/
	while ( getpush(strmod, sizeof(strmod)) ) {
		DEBUG(5, "tssetup: PUSHing %s\n", strmod);
		if ( ioctl(*fdreadp, I_PUSH, strmod) == -1 ) {
			DEBUG(5, "tssetup: I_PUSH on fd %d failed", *fdreadp);
			return(FAIL);
		}
	}

	DEBUG(4, "tssetup: MASTER mode: leaving ok\n", "");
	return(SUCCESS);
}

/**
 **	Report why a TLI call failed.
 */
tfaillog(fd, s)
int	fd;
char	*s;
{
	extern char	*t_errlist[];
	extern int	errno, t_errno, t_nerr;
	char	fmt[ BUFSIZ ];

	if (0 < t_errno && t_errno < t_nerr) {
		sprintf( fmt, "%s: %%s\n", s );
		DEBUG(5, fmt, t_errlist[t_errno]);
		logent(s, t_errlist[t_errno]);
		if ( t_errno == TSYSERR ) {
			DEBUG(5, "", perror("tlicall: system error"));
		} else if ( t_errno == TLOOK ) {
			show_tlook(fd);
		}
	} else {
		sprintf(fmt, "unknown tli error %d", t_errno);
		logent(s, fmt);
		sprintf(fmt, "%s: unknown tli error %d", s, t_errno);
		DEBUG(5, fmt, 0);
		DEBUG(5, "", perror(s));
	}
}

static
show_tlook(fd)
int fd;
{
	register int reason;
	register char *msg;
/*
 * Find out the current state of the interface.
 */
	errno = t_errno = 0;
	switch( reason = t_getstate(fd) ) {
	case T_UNBND:		msg = "T_UNBIND";	break;
	case T_IDLE:		msg = "T_IDLE";		break;
	case T_OUTCON:		msg = "T_OUTCON";	break;
	case T_INCON:		msg = "T_INCON";	break;
	case T_DATAXFER:	msg = "T_DATAXFER";	break;
	case T_OUTREL:		msg = "T_OUTREL";	break;
	case T_INREL:		msg = "T_INREL";	break;
	default:		msg = NULL;		break;
	}
	if( msg == NULL )
		return;

	DEBUG(5, "state is %s", msg);
	switch( reason = t_look(fd) ) {
	case -1:		msg = ""; break;
	case 0:			msg = "NO ERROR"; break;
	case T_LISTEN:		msg = "T_LISTEN"; break;
	case T_CONNECT:		msg = "T_CONNECT"; break;
	case T_DATA:		msg = "T_DATA";	 break;
	case T_EXDATA:		msg = "T_EXDATA"; break;
	case T_DISCONNECT:	msg = "T_DISCONNECT"; break;
	case T_ORDREL:		msg = "T_ORDREL"; break;
	case T_ERROR:		msg = "T_ERROR"; break;
	case T_UDERR:		msg = "T_UDERR"; break;
	default:		msg = "UNKNOWN ERROR"; break;
	}
	DEBUG(4, " reason is %s\n", msg);

	if ( reason == T_DISCONNECT )
	{
		struct t_discon	*dropped;
		if ( ((dropped = 
			(struct t_discon *)t_alloc(fd, T_DIS, T_ALL)) == 0) 
		||  (t_rcvdis(fd, dropped) == -1 )) {
			return;
		}
		DEBUG(5, "disconnect reason #%d\n", dropped->reason);
		t_free(dropped);
	}
	return;
}

#endif /*  TLIS  */

#endif /*  TLI  */
