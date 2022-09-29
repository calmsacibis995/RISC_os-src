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
#ident	"$Header: machine_setup.c,v 1.1.1.4 90/05/10 02:51:14 wje Exp $"

#include <nlist.h>

#include <sys/types.h>
#include <sys/param.h>
#ifdef SYSTYPE_SYSV
#include <sys/sysmacros.h>
#endif SYSTYPE_SYSV
#include <sys/stat.h>

/*
 * The subroutine machine_setup() figures out what kind of machine
 * this is, and sets up the machine-specific items.
 * It does this by checking the major number of the /dev/root device
 */
char DevName[] =	/* name of block devices */
	"XXXXXXXX";	/* see machine_setup() */
char Dname_tmp[32];

int	DevMajor;

machine_setup(nlst,x_id,x_ideq)
	struct nlist *nlst;
	int	x_id;
	int	x_ideq;
{
	DevMajor = get_major();
	switch( DevMajor ) {

	    case 4 :	/* dkip */
	    case 9 :
		    strcpy(DevName, "ipc");
		    nlst[x_id].n_name = "dkipiotime";
		    nlst[x_ideq].n_name = "ndkip";
		    break;

	    case 16 :	/* dkis */
		    strcpy(DevName, "isc");
		    nlst[x_id].n_name = "scsiiotime";
		    nlst[x_ideq].n_name = "nscsi";
		    break;

	    case 22 :	/* dkvj */
	    case 23 :
	    case 24 :
	    case 25 :
	    case 26 :
	    case 27 :
	    case 28 :
	    case 29 :
		    strcpy(DevName, "ijc");
		    nlst[x_id].n_name = "dkvjiotime";
		    nlst[x_ideq].n_name = "ndkvj";
		    break;
	    case 33 :
		    strcpy(DevName, "sdc");	/* used to be isd */
		    nlst[x_id].n_name = "isd_iotime";
		    nlst[x_ideq].n_name = "nisd";
		    break;
	}
}

get_major()
{
struct	stat	buf;

	stat("/dev/root", &buf);

	return( (int)major(buf.st_rdev) );
}


rdev_to_drive_number(rdev)
	dev_t	rdev;
{
	int	maj_ctlr;
	int	min_ctlr;

	maj_ctlr = 0;
	min_ctlr = minor(rdev) >> 4;

	switch( major(rdev) ) {
	    case 4 :	/* dkip */
		    break;
	    case 9 :
		    maj_ctlr = 1;
		    break;

	    case 16 :	/* dkis */
		    break;

	    case 22 :	/* dkvj */
	    case 23 :
	    case 24 :
	    case 25 :
	    case 26 :
	    case 27 :
	    case 28 :
	    case 29 :
		    maj_ctlr = major(rdev) - 22;
		    break;
	    case 33 :	/* dksd */
		    break;
	}
	return((maj_ctlr << 4) + min_ctlr);
}

char *
dname(i)
int i;
{
    int controller, drive;


	switch( DevMajor ) {
	    case 4:
		controller = i / 4;
		drive = i % 4;
		break;
	    case 9:
		controller = (i / 4) + 4;
		drive = i % 4;
		break;
	    case 16:	/* dkis */
	    case 22:	/* 1st dkvj */
	    case 33:	/* dksd */
		controller = 0;
		drive = i;
		break;
	    case 23:
		controller = 1;
		drive = i;
		break;
	    case 24:
		controller = 2;
		drive = i;
		break;
	    case 25:
		controller = 3;
		drive = i;
		break;
	    case 26:
		controller = 4;
		drive = i;
		break;
	    case 27:
		controller = 5;
		drive = i;
		break;
	    case 28:
		controller = 6;
		drive = i;
		break;
	    case 29:
		controller = 7;
		drive = i;
		break;
	    default:
		controller = -1;
		break;
	}

	if( controller == -1 )
	    sprintf(Dname_tmp,"%s***",DevName);
	else
	    sprintf(Dname_tmp,"%s%dd%d",DevName,controller,drive);
	return(Dname_tmp);
}
