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
#ident	"$Header: clocal.c,v 1.1.1.8 90/05/28 17:51:53 wje Exp $"

#ifdef RISCOS

#include <sys/types.h>
#include <sysv/sys/termio.h>

struct	termio	saved_termio[2];
int	termio_is_saved[2] = { 0 , 0 };


struct termio def_termio = 
	{				/* 		*/
		(IXANY),		/* c_iflag: input modes	*/
		OPOST|TAB0,	/* c_oflag: output modes	*/
		CNEW_RTSCTS | HUPCL | CREAD | CS8 |
				 B9600,
					/* c_cflag: control modes	*/
		ISIG|ICANON|ECHO|ECHOK,	/* c_lflag: line discipline modes */
		LDISC0,			/* c_line:  line discipline	*/
		  			/* c_cc: special characters	*/
		{  CINTR,		/* 0: VINTR 			*/
		   CQUIT,		/* 1: VQUIT			*/
		   CERASE,		/* 2: VERASE			*/
		   CKILL,		/* 3: VKILL			*/
		   CEOF,		/* 4: VEOF (ICANON),		*/
					/*    VMIN (~ICANON)		*/
		   CNUL,		/* 5: VEOL (ICANON),		*/
					/*    VTIME (~ICANON)		*/
		   CNUL,		/* 6: VEOL2			*/
		   CDEL,		/* 7: VSWTCH			*/
		   CSTART,		/* 8: V_START			*/
		   CSTOP,		/* 9: V_STOP			*/
		   CDEL,		/* 10: V_SUSP			*/
		   CDEL,		/* 11: V_DSUSP			*/
		   CRPRNT,		/* 12: V_RPRNT			*/
		   CFLUSH,		/* 13: V_FLUSH			*/
		   CWERASE,		/* 14: V_WERAS			*/
		   CESC,		/* 15: V_LNEXT			*/
		   CDEL,		/* 16: V_STATUS			*/
		   CDEL,		/* 17: V_SAVED_EOF		*/
		   CDEL,		/* 18: V_SAVED_EOL		*/
		   CDEL,		/* 19: (unused)			*/
		   CDEL,		/* 20: (unused)			*/
		   CDEL,		/* 21: (unused)			*/
		   CDEL},		/* 22: (unused)			*/
		0,			/* c_saved_flags 	*/
		0,			/* c_filler		*/
	};

save_termio(fd)
	int	fd;
{
	int	unit;

	if (fd < 0)
		return;

	unit = (fd == 0 ? 0 : 1);	

	if (ioctl(fd,TCGETA,&saved_termio[unit]) == -1)
		termio_is_saved[unit] = 0;
	else 
		termio_is_saved[unit] = 1;
}


restore_termio(fd)
	int	fd;
{
	int	unit;

	if (fd < 0)
		return;

	unit = (fd == 0 ? 0 : 1);	

	if (termio_is_saved[unit])
		ioctl(fd,TCSETA,&saved_termio[unit]);
}

init_termio(fd)
	int	fd;
{
	struct	termio	termio;

	if (fd < 0)
		return;

	if (fd == 0) {
		if (termio_is_saved[0])
			termio = saved_termio[0];
		else {
			if (ioctl(fd,TCGETA,&termio) == -1)
				return;
		};
	} else {
		termio = def_termio;
	};
	termio.c_saved_flags = 0;
	ioctl(fd,(fd == 0 ? TCSETAW : TCSETA),&termio);
}


int	defclocal = -1;
int	clocal = -1;
int	FD;

record_host_clocal()
{
	struct	termio	termio;
	int	old_clocal;

	if (FD < 0)
		return;

	if (ioctl(FD,TCGETA,&termio) == -1)
		defclocal = -1;
	else {
		defclocal = ((termio.c_cflag & CLOCAL) ? 1 : 0);
		old_clocal = clocal;
		clocal = defclocal;
		if (old_clocal != -1)
			change_host_clocal(clocal);
	};
}


change_host_clocal(new_value)
	int	new_value;
{
	struct	termio termio;

	if (FD < 0)
		return;

	if (clocal == new_value)
		return;
	clocal = new_value;
	if (defclocal != 0)
		return;

	if (ioctl(FD,TCGETA,&termio) == -1)
		return;

	if (new_value == 1)
		termio.c_cflag |= CLOCAL;
	else
		termio.c_cflag &= ~CLOCAL;

	if (ioctl(FD,TCSETA,&termio) == -1)
		return;

	clocal = new_value;
}


int	defrtscts = -1;
int	rtscts = -1;

record_host_rtscts()
{
	struct	termio	termio;
	int	old_rtscts;

	if (FD < 0)
		return;

	if (ioctl(FD,TCGETA,&termio) == -1)
		defrtscts = -1;
	else {
		defrtscts = ((termio.c_cflag & CNEW_RTSCTS) ? 1 : 0);
		old_rtscts = rtscts;
		rtscts = defrtscts;
		if (old_rtscts != -1)
			change_host_rtscts(old_rtscts);
	};
}


change_host_rtscts(new_value)
	int	new_value;
{
	struct	termio termio;

	if (FD < 0)
		return;

	if (rtscts == new_value)
		return;
	rtscts = new_value;
	if (defrtscts == -1)
		return;

	if (ioctl(FD,TCGETA,&termio) == -1)
		return;

	if (new_value == 1)
		termio.c_cflag |= CNEW_RTSCTS;
	else
		termio.c_cflag &= ~CNEW_RTSCTS;

	if (ioctl(FD,TCSETA,&termio) == -1)
		return;

	rtscts = new_value;
}


int	defixon = -1;
int	ixon = -1;

record_host_ixon()
{
	struct	termio	termio;
	int	old_ixon;

	if (FD < 0)
		return;

	if (ioctl(FD,TCGETA,&termio) == -1)
		defixon = -1;
	else {
		defixon = ((termio.c_iflag & IXON) ? 1 : 0);
		old_ixon = ixon;
		ixon = defixon;
		if (old_ixon != -1)
			change_host_ixon(old_ixon);
	};
}


change_host_ixon(new_value)
	int	new_value;
{
	struct	termio termio;

	if (FD < 0)
		return;

	if (ixon == new_value)
		return;
	ixon = new_value;
	if (defixon == -1)
		return;

	if (ioctl(FD,TCGETA,&termio) == -1)
		return;

	if (new_value == 1)
		termio.c_iflag |= IXON;
	else
		termio.c_iflag &= ~IXON;

	if (ioctl(FD,TCSETA,&termio) == -1)
		return;

	ixon = new_value;
}


#endif RISCOS
