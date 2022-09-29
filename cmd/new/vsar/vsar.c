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
#ident	"$Header: vsar.c,v 1.14.1.7 90/05/09 18:12:01 wje Exp $"

/*
 *	vsar.c		visual sar
 *			see sar(1) for details
 *	Randy Menna 12/10/87
*/

#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <sys/var.h>
#include <sys/iobuf.h>
#include <sys/stat.h>
#include <sys/elog.h>
#include <sys/inode.h>
#define KERNEL 1
#include <sys/file.h>
#undef KERNEL
#include <sys/sbd.h>
#include <sys/immu.h>
#include <sys/region.h>
#include <sys/proc.h>
#include <sys/sysinfo.h>
#include <sys/fcntl.h>
#include <sys/flock.h>
#include <sys/utsname.h>
#include <sys/lock.h>
#include <sys/errno.h>
#include <nlist.h>
#undef SYSV
#include <curses.h>
#include <signal.h>
#include <time.h>
#include "machine_info.h"
#include <bsd/sys/time.h>
#include "libutil.h"
#include "machine_setup.h"
#include "vsar.h"


#define	KBYTE	(1024)		/* used to scale mem and disk numbers	*/

/*			EXTERNALS
*/
extern	char	*optarg;
extern	int	optind, opterr;

/* 			FUNCTIONS 
*/
int	cleanup();
int	cleanup2();
char	*longname();

/* 			DISPLAY VARIABLES 
*/
WINDOW	*Scr;		/* the curses "pad" */
vscreen	Screen[768];	/* the total display screen */

char	TermType[128];	/* the type of terminal being used */

int	NumPages;	/* number of pages in display */
int	Row;		/* Row of pad that represents the top of the display */
int	PageIndex;	/* index into display page array */
int	Scrlen;		/* number of entries in the screen array */
int	DisplayMode;	/* mode of display, multi page, single, aves */

/* 			KERNEL READING VARIABLES 
*/
int	Kmem;			/* kmem file descriptor */
int	Loc;			/* location of space for finding table sizes */
int 	NumDevices;		/* number of block devices on system */
int	NumSamples;		/* number of kmem samples taken */
int	Curses_init;		/* curses has be init'ed flag */
int	Lines;			/* number of lines on the terminal */

long	Aprocsz;		/* average size of proc table */
long	Ainodesz;		/* average size of inode table */
long	Afilesz;		/* average size of file table */
long	Aflocksz;		/* average size of flock table */
long	Inodeovf;		/* overflows of inode table */
long	Ainodeovf;		/* Average overflows of inode table */
long	Fileovf;		/* overflows of file table */
long	Afileovf;		/* Average overflows of file table */
long	Procovf;		/* overflows of proc table */
long	Aprocovf;		/* Average overflows of proc table */
long	Flockovf;		/* overflows of flock table */
long	Aflockovf;		/* Average overflows of flock table */
long	Devio[NDEVS][4]; 	/* block device information  */
long	Odevio[NDEVS][4]; 	/* block device information  */
long	Adevio[NDEVS][4]; 	/* Average block device information  */

float	Tdiff;			/* time difference between samplings of kmem */
float	TotTdiff;		/* total time difference */
float	Atdiff;			/* average time difference */
	
double	Afreemem;		/* average free memory */

struct	sysinfo Si, Osi, Asi;	/* kernel information structures */
struct	minfo	Mi, Omi, Ami;	
struct	var	Vi;
struct	syserr	E;
struct	flckinfo	FlockInfo;

struct nlist Kinfo[] = {	/* nlist structure */
	{"XXX"},	/* see machine_setup() */
	{"sysinfo"},
	{"minfo"},
	{"inode"},
	{"file"},
	{"text"},
	{"proc"},
	{"flckinfo"},
	{"v"},
	{"XXX"},	/* see machine_setup() */
	{"syserr"},
	{"dinfo"},
	{"minserve"},
	{"maxserve"},
	{"id_string"},
	{0},
};

int	freemem_units = (4*KBYTE);	/* units of freemem val, usu 4KB */

extern int	errno;		/* system call error number		*/

main( argc, argv )
int	argc;
char	*argv[];
{
	int	i;
	int	c;
	int	firsttime;
	int	devcap;		/* cap on num of devices to monitor */
	int	sleepval;	/* time between samples */

	int	aflag = 0;		/* option seen flags */
	int	bflag = 0;
	int	cflag = 0;
	int	dflag = 0;
	int	mflag = 0;
	int	pflag = 0;
	int	qflag = 0;
	int	uflag = 0;
	int	rflag = 0;
	int	vflag = 0;
	int	wflag = 0;
	int	yflag = 0;
	int	lflag = 0;
	int	allflag = 0;

	char	options[LEN];	/* command line options */

	struct	utsname	machinfo;

	/* pre init */
	strcpy(options,"");
	sleepval = 1;
	devcap = 0;
	firsttime = 1;
	PageIndex = 0;
	Row = Pages[PageIndex];
	DisplayMode = MULTI;
	NumSamples = 0;
	TotTdiff = 0;
	Curses_init = 0;

	machine_setup(Kinfo,ID,IDEQ);

	/* process command line options */
	while ((c = getopt(argc,argv,"uybdfvcwaqmplrAi:D:SV")) != EOF)
	    switch(c){
		case 'i':	/* sleep interval */
			sleepval = atoi(optarg);
			if (sleepval < 1) {
				fprintf(stderr,"vsar: invalid time interval %d, defaulting to 1\n", sleepval);
				sleep(1);
				sleepval = 1;
			}
			
			break;
		case 'D':	/* device cap */
			devcap = atoi(optarg);
			if (devcap < 0 || devcap > MAXDEVCAP) {
				fprintf(stderr,"vsar: invalid number of devices %d, defaulting to %d\n", devcap, MAXDEVCAP);
				sleep(1);
				sleepval = 1;
			}
			break;
		case 'S':	/* single page display */
			DisplayMode = SINGLE;
			break;
		case 'V':
			DisplayMode = AVERAGE;
			break;

		case 'a':	/* file access */
			if ( !aflag && !allflag ) {
			    strcat( options,"a" );
			    ++aflag;
			}
			break;
		case 'b':	/* buffer activity */
			if ( !bflag && !allflag ) {
			    strcat( options,"b" );
			    ++bflag;
			}
			break;
		case 'c':	/* sys calls */
			if ( !cflag && !allflag ) {
			    strcat( options,"c" );
			    ++cflag;
			}
			break;
		case 'd':	/* block device */
			if ( !dflag && !allflag ) {
			    strcat( options,"d" );
			    ++dflag;
			}
			break;
		case 'm':	/* ipc */
			if ( !mflag && !allflag ) {
			    strcat( options,"m" );
			    ++mflag;
			}
			break;
		case 'p':	/* paging */
			if ( !pflag && !allflag ) {
			    strcat( options,"p" );
			    ++pflag;
			}
			break;
		case 'q':	/* queue */
			if ( !qflag && !allflag ) {
			    strcat( options,"q" );
			    ++qflag;
			}
			break;
		case 'u':	/* cpu utilization */
			if ( !uflag && !allflag ) {
			    strcat( options,"u" );
			    ++uflag;
			}
			break;
		case 'r':	/* memory */
			if ( !rflag && !allflag ) {
			    strcat( options,"r" );
			    ++rflag;
			}
			break;
		case 'v':	/* tables */
			if ( !vflag && !allflag ) {
			    strcat( options,"v" );
			    ++vflag;
			}
			break;
		case 'w':	/* swapping */
			if ( !wflag && !allflag ) {
			    strcat( options,"w" );
			    ++wflag;
			}
			break;
		case 'y':	/* tty */
			if ( !yflag && !allflag ) {
			    strcat( options,"y" );
			    ++yflag;
			}
			break;

		case 'A':	/* All */
			if ( !allflag ) {
			    strcpy( options,OPTIONS );
			    ++allflag;
			}
			break;

		case '?':
			fprintf(stderr,"%s\n",USAGE);
			exit(2);
			break;
		case 'l':
			/* lock the process incore if possible */
			lflag = 1;
			break;
	    }
	/* get the process locked into memory if we have been
	 * asked to do that.
	 */

	if (lflag && (plock(PROCLOCK) < 0)) {
		if (errno == EAGAIN) {
			int	tries=0;
			/* "temporary" real memory exhaustion.
			 * hang out for a while and keep trying
			 * to lock into memory.
			 * The sync call is superstition.
			 */
			while (plock(PROCLOCK) < 0) {
				if (tries > 5) {
					fprintf(stderr, "vsar: couldn't lock.  not enough memory\n");
					break;
				}
				sync();
				tries++;
				sleep(1);
			}
		} else if (errno == EPERM) {
			fprintf(stderr,"vsar: you must be superuser to lock into memory\n");
		} else {
			fprintf(stderr,"vsar: problem locking process into memory\n");
			perror ("vsar");
		}
	}

	/* open kmem and nlist */
	getkern();

	/* get the machine type and figure out page size */
	if (uname(&machinfo) < 0) {
		perror("Trying to get machine info with uname");
		exit(1);
	}
	freemem_units = getpagesize();

	/* post init */
	if ( DisplayMode == SINGLE ){
		strcpy(options,SINGOPTS);
	}
	else {
	    if ( !strcmp(options,"") )
		    strcpy(options,"u");
	}
	if ( devcap )
		NumDevices = devcap;

	/* map windows */
	Scrlen = map( options, &NumPages );
	
	/* set up signal handlers */
	init_sigs();

	/* set up curses, i.e. open windows & set modes */
	init_curses();

	/* put the labels on the pad */
	label_pad();

	/* write the status line to the pad */
	status();

	/* display the pad */
	doupdate();

	while(1) {
	    /* read kmem values */
	    getvals();

	    if ( firsttime ) {	/* must have two readings to get values */
		agevals(); 		/* save current values */
		touchwin(Scr);		/* force screen update */
		firsttime = 0;
		sleep(1);		/* sleep a while */
		continue;
	    }

	    /* average the time past if in AVERAGE MODE */
	    if (DisplayMode == AVERAGE) {
		++NumSamples;	/* increment sampling count */
		TotTdiff += Tdiff;
		Atdiff = TotTdiff/(float)NumSamples;
	    }

	    /* update the pad with values from kernel */
	    show_values();

	    /* get display controlling input, if any */
	    get_input();

	    /* refresh status line */
	    status();

	    /* refresh display */
	    doupdate();

	    /* save current values */
	    agevals();

	    sleep(sleepval);

	} /* end while(1) */
}

cleanup()
{
    if ( Curses_init )
	endwin();
    exit(0);
}

cleanup2()
{
    if ( Curses_init )
	endwin();
    fprintf(stderr,"*** core would have been dumped ***\n");
    exit(0);
}

/*	Get Input: read the keyboard for input if any, change display
*/
get_input()
{
int	c;

	if ( (c = wgetch(Scr)) != ERR )
	    switch( c ) {
		    case 'p':
		    case 'P':
		    case 'k':
		    case 'K':
			    if ( DisplayMode != SINGLE ) {
				if ( PageIndex > 0 ) {
				    --PageIndex;
				    Row = Pages[PageIndex];
				}
				else
				    flash();
			    }
			    else {
				if ( Row > 0 ) {
				    --Row;
				}
				else {
				    flash();
				}
			    }
			    break;

		    case 'n':
		    case 'N':
		    case 'j':
		    case 'J':
			    if ( DisplayMode != SINGLE ) {
				if ( PageIndex < NumPages ){
				    ++PageIndex;
				    Row = Pages[PageIndex];
				}
				else {
				    flash();
				}
			    }
			    else {
				if ( Row < (NumDevices - DEV_ON_SCREEN) ){
				    ++Row;
				}
				else {
				    flash();
				}
			    }
			    break;

		    case 'q':
			    cleanup();
			    break;
		    
		    case CNTR_L:
			    clear();
			    wclear(Scr);
			    refresh();
			    prefresh(Scr,Row,0,1,0,Lines,79);
			    status();
			    label_pad();
			    break;

		    default: beep();
			    break;
		}

	pnoutrefresh(Scr,Row,0,1,0,Lines,79);
}

/*	Initialize Curses Windows: create the pad and set modes
*/
init_curses()
{
	initscr();
	nonl();
	cbreak();
	Scr = newpad( WINLEN,WINWIDTH );
	nodelay(Scr,TRUE);
	leaveok(Scr,TRUE);
	curs_set(0);
	strcpy(TermType,longname());
	if ( DisplayMode == SINGLE )
	    Lines = tgetnum("lines");
	else
	    Lines = 23;
	Curses_init = 1;
}

/*	Set Up Signal Handlers
*/
init_sigs()
{
    signal(SIGINT,cleanup);
    signal(SIGQUIT,cleanup);
    signal(SIGSEGV,cleanup2);
    signal(SIGBUS,cleanup2);
}

/*	Label Pad: write the display label to the pad
*/
label_pad()
{
int	i;

    for( i = 0; i < Scrlen; i++ ) {
	if ( Screen[i].ly > -1 && Screen[i].lx > -1 ) {

	    if ( Screen[i].ftype == NONE ) {	/* if a group label, BOLD */
		wattron(Scr,A_BOLD);
		mvwprintw(Scr, Screen[i].ly, Screen[i].lx,"%s",
			Screen[i].label );
		wattroff(Scr,A_BOLD);
	    }
	    else {
		if ( DisplayMode == SINGLE ) {
		    wattron(Scr,A_UNDERLINE);
		    mvwprintw(Scr, Screen[i].ly, Screen[i].lx,"%s",
			    Screen[i].label );
		    wattroff(Scr,A_UNDERLINE);
		}
		else {
		    mvwprintw(Scr, Screen[i].ly, Screen[i].lx,"%s",
			    Screen[i].label );
		}
	    }
	}
    }
    pnoutrefresh(Scr,Row,0,1,0,Lines,79);

}

map( s, w )
char	*s;
int	*w;
{
char	*p;
	
int	i,j;
int	k;
int	yoff = 0;
int	xoff = 0;
int	len = 0;
int	max = 0;

	for( i=0, p=s; *p != NULL; *p++) {
		switch( *p ) {
		    case 'u':
			j = fillmap( &i, CPU, &xoff, &yoff );
			len += j;
			break;

		    case 'b':
			j = fillmap( &i, BUF, &xoff, &yoff );
			len += j;
			break;

		    case 'd':
			for(k=0;k<NumDevices;k++){
			    j = fillmap( &i, DEV, &xoff, &yoff, k );
			    len += j;
			}
			break;

		    case 'y':
			j = fillmap( &i, TTY, &xoff, &yoff );
			len += j;
			break;

		    case 'c':
			j = fillmap( &i, SYSC, &xoff, &yoff );
			len += j;
			break;

		    case 'w':
			j = fillmap( &i, SWAP, &xoff, &yoff );
			len += j;
			break;

		    case 'a':
			j = fillmap( &i, FILE, &xoff, &yoff );
			len += j;
			break;

		    case 'q':
			j = fillmap( &i, QUE, &xoff, &yoff );
			len += j;
			break;

		    case 'v':
			j = fillmap( &i, TBL, &xoff, &yoff );
			len += j;
			break;

		    case 'm':
			j = fillmap( &i, IPC, &xoff, &yoff );
			len += j;
			break;

		    case 'p':
			j = fillmap( &i, PAGE, &xoff, &yoff );
			len += j;
			break;

		    case 'r':
			j = fillmap( &i, MEM, &xoff, &yoff );
			len += j;
			break;

		} /* switch */

	} /* for */

	(*w) = yoff/2;
	return( len );
}

fillmap( m, d, x, y, arg )
int	*m;
int	d;
int	*x;
int	*y;
int	arg;
{
int	j;

switch( DisplayMode ) {

    case MULTI:
	    for(j=0; display[d][j].name != NULL; j++){
		Screen[*m].label = display[d][j].name;
		Screen[*m].ftype = display[d][j].ftype;
		switch( Screen[*m].ftype ) {
		    case NONE:
			Screen[*m].func.lvalue = 0;
			break;
		    case FLOAT:
		    case FLOAT_B:
		    case FLOAT_1d:
		    case FLOAT_3:
			Screen[*m].func.fv.fvalue = dispfval[d][j].value;
			Screen[*m].func.fv.farg = arg;
			break;
		    case LONG:
		    case LONG_K:
			Screen[*m].func.lvalue = displval[d][j].value;
			break;
		    case CHAR:
			Screen[*m].func.cv.cvalue = dispcval[d][j].value;
			Screen[*m].func.cv.carg = arg;
			break;
		    case DOUBLE:
			Screen[*m].func.dvalue = dispdval[d][j].value;
			break;
		}
		Screen[*m].lx = pos[d][j].lx + x_offset[*x];
		Screen[*m].ly = pos[d][j].ly + y_offset[*y];
		if ( pos[d][j].vx > -1 )
		    Screen[*m].vx = pos[d][j].vx + x_offset[*x];
		else
		    Screen[*m].vx = pos[d][j].vx;
		if ( pos[d][j].vy > -1 )
		    Screen[*m].vy = pos[d][j].vy + y_offset[*y];
		else
		    Screen[*m].vy = pos[d][j].vy;
		++(*m);
	    }
	    if ( *x+1 == XSETS)
		*y = *y + 1;
	    *x = (*x + 1) % XSETS;
	    break;

	case SINGLE:
	    for(j=0; spdisplay[d][j].name != NULL; j++){
		if ( d == DEV && arg > 0 )
		    Screen[*m].label = NULL;
		else
		    Screen[*m].label = spdisplay[d][j].name;
		Screen[*m].ftype = spdisplay[d][j].ftype;
		switch( Screen[*m].ftype ) {
		    case NONE:
			Screen[*m].func.lvalue = 0;
			break;
		    case FLOAT:
		    case FLOAT_B:
		    case FLOAT_1d:
		    case FLOAT_3:
			Screen[*m].func.fv.fvalue = spdispfval[d][j].value;
			Screen[*m].func.fv.farg = arg;
			break;
		    case LONG:
		    case LONG_K:
			Screen[*m].func.lvalue = spdisplval[d][j].value;
			break;
		    case CHAR:
			Screen[*m].func.cv.cvalue = spdispcval[d][j].value;
			Screen[*m].func.cv.carg = arg;
			break;
		    case DOUBLE:
			Screen[*m].func.dvalue = spdispdval[d][j].value;
			break;
		}
		if ( sppos[d][j].lx > -1 )
		    Screen[*m].lx = sppos[d][j].lx + spx_offset[*x];
		else
		    Screen[*m].lx = sppos[d][j].lx;
		if ( sppos[d][j].lx > -1 )
		    Screen[*m].ly = sppos[d][j].ly + spy_offset[*y];
		else
		    Screen[*m].ly = sppos[d][j].ly;
		if ( sppos[d][j].vx > -1 )
		    Screen[*m].vx = sppos[d][j].vx + spx_offset[*x];
		else
		    Screen[*m].vx = sppos[d][j].vx;
		if ( sppos[d][j].vy > -1 )
		    Screen[*m].vy = sppos[d][j].vy + spy_offset[*y];
		else
		    Screen[*m].vy = sppos[d][j].vy;
		++(*m);
	    }
	    *y = *y + 1;
	    *x = *x + 1;
	    break;

    case AVERAGE:
	    for(j=0; avdisplay[d][j].name != NULL; j++){
		Screen[*m].label = avdisplay[d][j].name;
		Screen[*m].ftype = avdisplay[d][j].ftype;
		switch( Screen[*m].ftype ) {
		    case NONE:
			Screen[*m].func.lvalue = 0;
			break;
		    case FLOAT:
		    case FLOAT_B:
		    case FLOAT_1d:
		    case FLOAT_3:
			Screen[*m].func.fv.fvalue = avdispfval[d][j].value;
			Screen[*m].func.fv.farg = arg;
			break;
		    case LONG:
		    case LONG_K:
			Screen[*m].func.lvalue = avdisplval[d][j].value;
			break;
		    case CHAR:
			Screen[*m].func.cv.cvalue = avdispcval[d][j].value;
			Screen[*m].func.cv.carg = arg;
			break;
		    case DOUBLE:
			Screen[*m].func.dvalue = avdispdval[d][j].value;
			break;
		}
		Screen[*m].lx = avpos[d][j].lx + avx_offset[*x];
		Screen[*m].ly = avpos[d][j].ly + avy_offset[*y];
		if ( avpos[d][j].vx > -1 )
		    Screen[*m].vx = avpos[d][j].vx + avx_offset[*x];
		else
		    Screen[*m].vx = avpos[d][j].vx;
		if ( avpos[d][j].vy > -1 )
		    Screen[*m].vy = avpos[d][j].vy + avy_offset[*y];
		else
		    Screen[*m].vy = avpos[d][j].vy;
		++(*m);
	    }
	    if ( *x+1 == AVXSETS)
		*y = *y + 1;
	    *x = (*x + 1) % AVXSETS;
	    break;
    } /* end switch */
   return(j);
}

/*	Show Values: write the values being monitored to the pad
*/
show_values()
{
int	i;

    for( i = 0; i < Scrlen; i++ ) {
	if ( Screen[i].vx > -1 && Screen[i].vy > -1 ) {
		mvwprintw(Scr,Screen[i].vy, Screen[i].vx,"%s", MASK );
	    }
	    switch( Screen[i].ftype ) {
		case NONE:
			break;
		case FLOAT:
		    mvwprintw(Scr, Screen[i].vy, Screen[i].vx,
		     "%5.0f",(Screen[i].func.fv.fvalue == 0) ? 0.0 :
			(*Screen[i].func.fv.fvalue)
				(Screen[i].func.fv.farg) );
			break;
		case FLOAT_3:
		    mvwprintw(Scr, Screen[i].vy, Screen[i].vx,
		     "%4.0f",(Screen[i].func.fv.fvalue == 0) ? 0.0 :
			(*Screen[i].func.fv.fvalue)
				(Screen[i].func.fv.farg) );
			break;
		case FLOAT_1d:
		    mvwprintw(Scr, Screen[i].vy, Screen[i].vx,
		     "%5.1f",(Screen[i].func.fv.fvalue == 0) ? 0.0 :
			(*Screen[i].func.fv.fvalue)
				(Screen[i].func.fv.farg) );
			break;
		case FLOAT_B:
		    mvwprintw(Scr, Screen[i].vy, Screen[i].vx,
		     "%5.0f",(Screen[i].func.fv.fvalue == 0) ? 0.0 :
			(*Screen[i].func.fv.fvalue)
				(Screen[i].func.fv.farg) / 2.0 );
			break;
		case LONG:
		    mvwprintw(Scr, Screen[i].vy, Screen[i].vx,
		     "%-3ld",(Screen[i].func.lvalue == 0) ? 0 :
			(*Screen[i].func.lvalue)() );
			break;
		case LONG_K:
		    mvwprintw(Scr, Screen[i].vy, Screen[i].vx,
		     "%-3ld",(Screen[i].func.lvalue == 0) ? 0 :
			((*Screen[i].func.lvalue)() + 512) / KBYTE );
			break;
		case CHAR:
		    mvwprintw(Scr, Screen[i].vy, Screen[i].vx,
		     "%-1s",(Screen[i].func.cv.cvalue == 0) ? " " :
			(*Screen[i].func.cv.cvalue)
				(Screen[i].func.cv.carg) );
			break;
		case DOUBLE:
		    mvwprintw(Scr, Screen[i].vy, Screen[i].vx,
		     "%-3.0f",(Screen[i].func.dvalue == 0) ? 0.0 :
			(*Screen[i].func.dvalue)() );
			break;
	    }
	}
}

/*	Display Status Line Info: hostname, date, etc
*/
status()
{
struct	utsname	name;
time_t	timeoday;
char	*osname;

    uname(&name);
    timeoday = time(0);	

    osname = name.version;
    if (! strcmp(osname,"UMIPS")) 
	osname = "RISC/os";
    mvprintw(0,0,"%s(%s)  %s.%s                   %s",name.sysname,
	name.m_type, osname, name.release, ctime(&timeoday));
    wnoutrefresh(stdscr);
}

/* nlist the kernel and set up the values for reading */
getkern()
{
int	i;
int	tblsize;

    if ( nlist("/unix",Kinfo) == -1 )
		perrexit();

    for (i=0;i<NNLIST;i++)
	    Kinfo[i].n_value &= ~(0x80000000);

	/* open /dev/kmem */
    if((Kmem = open("/dev/kmem", 0)) == -1)
	    perrexit();

    /* Check to ensure that the kernel id strings match */
    if (check_kernel_id (Kmem,
			(long)Kinfo[N_ID_STRING].n_value, "/unix") > 0) {
        if ( Curses_init )
		endwin();
	fprintf(stderr,"/unix does not match /dev/kmem\n");
	exit(1);
    };
	/* get some space for finding table sizes */
	/* whichever table is bigger is the max we need */
    if(lseek(Kmem,(long)Kinfo[V].n_value,0) == -1)
	    perrexit();
    if(read(Kmem,&Vi,sizeof Vi) == -1)
	    perrexit();
    
    tblsize =sizeof(struct inode)*Vi.v_inode;
    if (tblsize < sizeof(struct file)*Vi.v_file)
	    tblsize = sizeof (struct file)*Vi.v_file;
    if (tblsize < sizeof (struct proc)*Vi.v_proc)
	    tblsize = sizeof (struct proc)*Vi.v_proc;
    Loc = malloc(tblsize);
    if (Loc == NULL){
	    perrexit();
    }

    /* get number of block devices */
    if (Kinfo[IDEQ].n_value !=0){
	    lseek(Kmem,(long)Kinfo[IDEQ].n_value, 0);
	    if (read(Kmem, &NumDevices, sizeof NumDevices) != sizeof NumDevices)
		    perrexit();
    }

    /* kludge so that we don't have to change display for SCSI m2000 */
    if ( NumDevices > MAXDEVCAP )
	NumDevices = MAXDEVCAP;

}

agevals()
{
int	i,j;

	Osi = Si;
	Omi = Mi;
	for(i=0;i<NDEVS;i++)
	    for(j=0;j<4;j++)
		Odevio[i][j] = Devio[i][j];
	Procovf = E.procovf;
	Inodeovf = E.inodeovf;
	Fileovf = E.fileovf;
	Flockovf = FlockInfo.recovf;
}

/* read values from kmem */
getvals()
{
int	i,j,k;
struct iotime idstat[NDEVS];

/* read sys info structure si */
    lseek(Kmem,(long)Kinfo[SINFO].n_value,0);
    if (read(Kmem, &Si, sizeof Si) == -1)
	    perrexit();

/* calculate total cpu time that has past */
    Tdiff = Si.cpu[0] - Osi.cpu[0] +
	    Si.cpu[1] - Osi.cpu[1] +
	    Si.cpu[2] - Osi.cpu[2] +
	    Si.cpu[3] - Osi.cpu[3];

 /* convert clicks to reporting units of KBytes */
    Si.bswapin=ctob(Si.bswapin)/KBYTE; /* BSIZE to KBYTES */
    Si.bswapout=ctob(Si.bswapout)/KBYTE; /* BSIZE to KBYTES */

/*  read memory structure mi */
    if (Kinfo[MINFO].n_value != 0)	{
	lseek( Kmem, (long)Kinfo[MINFO].n_value, 0);
	if (read( Kmem, &Mi, sizeof Mi) == -1)
		perrexit();
    }

/* get block device information */
    if ( Kinfo[ID].n_value != 0 ) {
	lseek( Kmem, (long) Kinfo[ID].n_value, 0 );
	if ( read( Kmem, idstat, sizeof( struct iotime ) * NumDevices) == -1) {
		    perrexit();
	    }
    }

    if (Kinfo[0].n_value != 0){
	for (i=0,k=0;k<NumDevices;k++){
	    Devio[i][IO_OPS] = idstat[k].io_cnt;
	    Devio[i][IO_BCNT] = idstat[k].io_bcnt;
	    Devio[i][IO_ACT] = idstat[k].io_act;
	    Devio[i][IO_RESP] = idstat[k].io_resp;
	    i++;
	}
    }

/* get file locking information */
    if ( Kinfo[FLCK].n_value != 0 ) {
	lseek( Kmem, (long) Kinfo[FLCK].n_value, 0 );
	if (read(Kmem, &FlockInfo, sizeof(struct flckinfo)) == -1) {
		perrexit();
	}
    }

/*		record system tables overflows	*/
    if ( Kinfo[SERR].n_value != 0 ) {
	lseek(Kmem,(long)Kinfo[SERR].n_value,0);
	if (read(Kmem,&E,sizeof (struct syserr)) == -1){
	    perrexit();
	}
    }

}

float
pct_user()
{
    if (DisplayMode == AVERAGE)
	Asi.cpu[CPU_USER] += (Si.cpu[CPU_USER] - Osi.cpu[CPU_USER]);

    return((float)(Si.cpu[CPU_USER] - Osi.cpu[CPU_USER])/Tdiff * 100.0);
}

float
ave_pct_user()
{
    return(((float)(Asi.cpu[CPU_USER]/NumSamples))/(Atdiff) * 100.0);
}

float
pct_sys()
{
    if (DisplayMode == AVERAGE)
	Asi.cpu[CPU_KERNEL] += (Si.cpu[CPU_KERNEL] - Osi.cpu[CPU_KERNEL]);

    return((float)(Si.cpu[CPU_KERNEL] - Osi.cpu[CPU_KERNEL])/Tdiff * 100.0);
}

float
ave_pct_sys()
{
    return((float)(Asi.cpu[CPU_KERNEL]/NumSamples)/Atdiff * 100.0);
}

float
pct_waitio()
{
    if (DisplayMode == AVERAGE)
	Asi.cpu[CPU_WAIT] += (Si.cpu[CPU_WAIT] - Osi.cpu[CPU_WAIT]);

    return((float)(Si.cpu[CPU_WAIT] - Osi.cpu[CPU_WAIT])/Tdiff * 100.0);
}

float
ave_pct_waitio()
{
    return((float)(Asi.cpu[CPU_WAIT]/NumSamples)/Atdiff * 100.0);
}

float
pct_idle()
{
    if (DisplayMode == AVERAGE)
	Asi.cpu[CPU_IDLE] += (Si.cpu[CPU_IDLE] - Osi.cpu[CPU_IDLE]);

    return((float)(Si.cpu[CPU_IDLE] - Osi.cpu[CPU_IDLE])/Tdiff * 100.0);
}

float
ave_pct_idle()
{
    return((float)(Asi.cpu[CPU_IDLE]/NumSamples)/Atdiff * 100.0);
}

long
bread()
{
    if (DisplayMode == AVERAGE)
	Asi.bread += (Si.bread - Osi.bread);

    return((long)((float)(Si.bread - Osi.bread)/Tdiff * HZ));
}

long
ave_bread()
{
    return((long)(Asi.bread/NumSamples)/Atdiff * HZ);
}

long
bwrite()
{
    if (DisplayMode == AVERAGE)
	Asi.bwrite += (Si.bwrite - Osi.bwrite);

    return((long)((float)(Si.bwrite - Osi.bwrite)/Tdiff * HZ));
}

long
ave_bwrite()
{
    return((long)(Asi.bwrite/NumSamples)/Atdiff * HZ);
}

long
sread()
{
    if (DisplayMode == AVERAGE)
	Asi.lread += (Si.lread - Osi.lread);

    return((long)((float)(Si.lread - Osi.lread)/Tdiff * HZ));
}

long
ave_sread()
{
    return((long)(Asi.lread/NumSamples)/Atdiff * HZ);
}

long
swrite()
{
    if (DisplayMode == AVERAGE)
	Asi.lwrite += (Si.lwrite - Osi.lwrite);

    return((long)((float)(Si.lwrite - Osi.lwrite)/Tdiff * HZ));
}

long
ave_swrite()
{
    return((long)(Asi.lwrite/NumSamples)/Atdiff * HZ);
}

float
pct_rdcache()
{
    return( (float)(Si.lread - Osi.lread) == 0.0 ? 0.0 :
	(((float)(Si.lread - Osi.lread) - (float)(Si.bread - Osi.bread)) /
	  (float)(Si.lread - Osi.lread) * 100.0) );
}

float
ave_pct_rdcache()
{
    return( (float)(Asi.lread / NumSamples) == 0.0 ? 0.0 :
	(((float)(Asi.lread / NumSamples) - (float)(Asi.bread / NumSamples)) /
	  (float)(Asi.lread / NumSamples) * 100.0) );
}

float
pct_wrcache()
{
    return( (float)(Si.lwrite - Osi.lwrite) == 0.0 ? 0.0 :
	(((float)(Si.lwrite - Osi.lwrite) - (float)(Si.bwrite - Osi.bwrite)) /
	  (float)(Si.lwrite - Osi.lwrite) * 100.0) );
}

float
ave_pct_wrcache()
{
    return( (float)(Asi.lwrite / NumSamples) == 0.0 ? 0.0 :
	(((float)(Asi.lwrite / NumSamples) - (float)(Asi.bwrite / NumSamples)) /
	  (float)(Asi.lwrite / NumSamples) * 100.0) );
}

long
pread()
{
    if (DisplayMode == AVERAGE)
	Asi.phread += (Si.phread - Osi.phread);

    return((long)((float)(Si.phread - Osi.phread)/Tdiff * HZ));
}

long
ave_pread()
{
    return((long)(Asi.phread/NumSamples)/Atdiff * HZ);
}

long
pwrite()
{
    if (DisplayMode == AVERAGE)
	Asi.phwrite += (Si.phwrite - Osi.phwrite);

    return((long)((float)(Si.phwrite - Osi.phwrite)/Tdiff * HZ));
}

long
ave_pwrite()
{
    return((long)(Asi.phwrite/NumSamples)/Atdiff * HZ);
}

float
pct_busy(i)
int i;
{
    if (DisplayMode == AVERAGE)
	Adevio[i][IO_ACT] += (Devio[i][IO_ACT] - Odevio[i][IO_ACT]);

    return( (float)(Devio[i][IO_ACT] - Odevio[i][IO_ACT]) == 0.0 ? 0.0 :
		(float)(Devio[i][IO_ACT] - Odevio[i][IO_ACT]) / Tdiff * 100.0);
}

float
ave_pct_busy(i)
int i;
{
    return( (float)(Adevio[i][IO_ACT] / NumSamples) == 0.0 ? 0.0 :
		(float)(Adevio[i][IO_ACT] / NumSamples) / Atdiff * 100.0);
}

float
pending(i)
int i;
{
    if (DisplayMode == AVERAGE)
	Adevio[i][IO_RESP] += (Devio[i][IO_RESP] - Odevio[i][IO_RESP]);

    return( ((float)(Devio[i][IO_RESP] - Odevio[i][IO_RESP]) == 0.0 ||
	     (float)(Devio[i][IO_ACT] - Odevio[i][IO_ACT]) == 0.0) ? 0.0 :
	    (float)(Devio[i][IO_RESP] - Odevio[i][IO_RESP]) / 
			(float)(Devio[i][IO_ACT] - Odevio[i][IO_ACT]));
}

float
ave_pending(i)
int i;
{
    return( ((float)(Adevio[i][IO_RESP] / NumSamples) == 0.0 ||
	     (float)(Adevio[i][IO_ACT] / NumSamples) == 0.0) ? 0.0 :
	    (float)(Adevio[i][IO_RESP] / NumSamples) / 
			(float)(Adevio[i][IO_ACT] / NumSamples));
}

float
rdwr(i)
int i;
{
    if (DisplayMode == AVERAGE)
	Adevio[i][IO_OPS] += (Devio[i][IO_OPS] - Odevio[i][IO_OPS]);

    return((float)(Devio[i][IO_OPS] - Odevio[i][IO_OPS]) == 0.0 ? 0.0 :
		(float)(Devio[i][IO_OPS] - Odevio[i][IO_OPS]) / Tdiff * HZ);
}

float
ave_rdwr(i)
int i;
{
    return((float)(Adevio[i][IO_OPS] / NumSamples) == 0.0 ? 0.0 :
		(float)(Adevio[i][IO_OPS] / NumSamples) / Atdiff * HZ);
}

float
blktran(i)
int i;
{
    if (DisplayMode == AVERAGE)
	Adevio[i][IO_BCNT] += (Devio[i][IO_BCNT] - Odevio[i][IO_BCNT]);

    return((float)(Devio[i][IO_BCNT] - Odevio[i][IO_BCNT]) == 0.0 ? 0.0 :
		(float)(Devio[i][IO_BCNT] - Odevio[i][IO_BCNT]) / Tdiff * HZ);
}

float
ave_blktran(i)
int i;
{
    return((float)(Adevio[i][IO_BCNT] / NumSamples) == 0.0 ? 0.0 :
		(float)(Adevio[i][IO_BCNT] / NumSamples) / Atdiff * HZ);
}

float
waittime(i)
int i;
{
    return( ((float)(Devio[i][IO_RESP] - Odevio[i][IO_RESP]) == 0.0 ||
	     (float)(Devio[i][IO_ACT] - Odevio[i][IO_ACT]) == 0.0 ||
	     (float)(Devio[i][IO_OPS] - Odevio[i][IO_OPS]) == 0.0 ) ? 0.0 :
    ((float)(Devio[i][IO_RESP] - Odevio[i][IO_RESP]) - 
	(float)(Devio[i][IO_ACT] - Odevio[i][IO_ACT])) /
	    (float)(Devio[i][IO_OPS] - Odevio[i][IO_OPS]) / HZ * 1000.0);
}

float
ave_waittime(i)
int i;
{
    return( ((float)(Adevio[i][IO_RESP] / NumSamples) == 0.0 ||
	     (float)(Adevio[i][IO_ACT] / NumSamples) == 0.0 ||
	     (float)(Adevio[i][IO_OPS] / NumSamples) == 0.0 ) ? 0.0 :
    ((float)(Adevio[i][IO_RESP] / NumSamples) - 
	(float)(Adevio[i][IO_ACT] / NumSamples)) /
	    (float)(Adevio[i][IO_OPS] / NumSamples) / HZ * 1000.0);
}

float
servtime(i)
int i;
{
    return( ((float)(Devio[i][IO_ACT] - Odevio[i][IO_ACT]) == 0.0 ||
	     (float)(Devio[i][IO_OPS] - Odevio[i][IO_OPS]) == 0.0 ) ? 0.0 :
		(float)(Devio[i][IO_ACT] - Odevio[i][IO_ACT]) / 
		(float)(Devio[i][IO_OPS] - Odevio[i][IO_OPS]) / HZ * 1000.0);
}

float
ave_servtime(i)
int i;
{
    return( ((float)(Adevio[i][IO_ACT] / NumSamples) == 0.0 ||
	     (float)(Adevio[i][IO_OPS] / NumSamples) == 0.0 ) ? 0.0 :
		(float)(Adevio[i][IO_ACT] / NumSamples) / 
		(float)(Adevio[i][IO_OPS] / NumSamples) / HZ * 1000.0);
}

long
inchar()
{
    if (DisplayMode == AVERAGE)
	Asi.rawch += (Si.rawch - Osi.rawch);

    return((long)((float)(Si.rawch - Osi.rawch)/Tdiff * HZ));
}

long
ave_inchar()
{
    return((long)((float)(Asi.rawch / NumSamples)/Atdiff * HZ));
}

long
canchar()
{
    if (DisplayMode == AVERAGE)
	Asi.canch += (Si.canch - Osi.canch);

    return((long)((float)(Si.canch - Osi.canch)/Tdiff * HZ));
}

long
ave_canchar()
{
    return((long)((float)(Asi.canch / NumSamples)/Atdiff * HZ));
}

long
outchar()
{
    if (DisplayMode == AVERAGE)
	Asi.outch += (Si.outch - Osi.outch);

    return((long)((float)(Si.outch - Osi.outch)/Tdiff * HZ));
}

long
ave_outchar()
{
    return((long)((float)(Asi.outch / NumSamples)/Atdiff * HZ));
}

long
recintr()
{
    if (DisplayMode == AVERAGE)
	Asi.rcvint += (Si.rcvint - Osi.rcvint);

    return((long)((float)(Si.rcvint - Osi.rcvint)/Tdiff * HZ));
}

long
ave_recintr()
{
    return((long)((float)(Asi.rcvint / NumSamples)/Atdiff * HZ));
}

long
xmitintr()
{
    if (DisplayMode == AVERAGE)
	Asi.xmtint += (Si.xmtint - Osi.xmtint);

    return((long)((float)(Si.xmtint - Osi.xmtint)/Tdiff * HZ));
}

long
ave_xmitintr()
{
    return((long)((float)(Asi.xmtint / NumSamples)/Atdiff * HZ));
}

long
modintr()
{
    if (DisplayMode == AVERAGE)
	Asi.mdmint += (Si.mdmint - Osi.mdmint);

    return((long)((float)(Si.mdmint - Osi.mdmint)/Tdiff * HZ));
}

long
ave_modintr()
{
    return((long)((float)(Asi.mdmint / NumSamples)/Atdiff * HZ));
}

long
allcall()
{
    if (DisplayMode == AVERAGE)
	Asi.syscall += (Si.syscall - Osi.syscall);

    return((long)((float)(Si.syscall - Osi.syscall)/Tdiff *HZ));
}

long
ave_allcall()
{
    return((long)((float)(Asi.syscall / NumSamples)/Atdiff * HZ));
}

long
rdcall()
{
    if (DisplayMode == AVERAGE)
	Asi.sysread += (Si.sysread - Osi.sysread);

    return((long)((float)(Si.sysread - Osi.sysread)/Tdiff *HZ));
}

long
ave_rdcall()
{
    return((long)((float)(Asi.sysread / NumSamples)/Atdiff * HZ));
}

long
wrcall()
{
    if (DisplayMode == AVERAGE)
	Asi.syswrite += (Si.syswrite - Osi.syswrite);

    return((long)((float)(Si.syswrite - Osi.syswrite)/Tdiff *HZ));
}

long
ave_wrcall()
{
    return((long)((float)(Asi.syswrite / NumSamples)/Atdiff * HZ));
}

long
fkcall()
{
    if (DisplayMode == AVERAGE)
	Asi.sysfork += (Si.sysfork - Osi.sysfork);

    return((long)((float)(Si.sysfork - Osi.sysfork)/Tdiff *HZ));
}

long
ave_fkcall()
{
    return((long)((float)(Asi.sysfork / NumSamples)/Atdiff * HZ));
}

long
excall()
{
    if (DisplayMode == AVERAGE)
	Asi.sysexec += (Si.sysexec - Osi.sysexec);

    return((long)((float)(Si.sysexec - Osi.sysexec)/Tdiff *HZ));
}

long
ave_excall()
{
    return((long)((float)(Asi.sysexec / NumSamples)/Atdiff * HZ));
}

long
rdcallchar()
{
    if (DisplayMode == AVERAGE)
	Asi.readch += (Si.readch - Osi.readch);

    return((long)((float)(Si.readch - Osi.readch)/Tdiff *HZ));
}

long
ave_rdcallchar()
{
    return((long)((float)(Asi.readch / NumSamples)/Atdiff * HZ));
}

long
wrcallchar()
{
    if (DisplayMode == AVERAGE)
	Asi.writech += (Si.writech - Osi.writech);

    return((long)((float)(Si.writech - Osi.writech)/Tdiff *HZ));
}

long
ave_wrcallchar()
{
    return((long)((float)(Asi.writech / NumSamples)/Atdiff * HZ));
}

long
swapin()
{
    if (DisplayMode == AVERAGE)
	Asi.swapin += (Si.swapin - Osi.swapin);

    return((long)((float)(Si.swapin - Osi.swapin)/Tdiff * HZ));
}

long
ave_swapin()
{
    return((long)((float)(Asi.swapin / NumSamples)/Atdiff * HZ));
}

long
swapout()
{
    if (DisplayMode == AVERAGE)
	Asi.swapout += (Si.swapout - Osi.swapout);

    return((long)((float)(Si.swapout - Osi.swapout)/Tdiff * HZ));
}

long
ave_swapout()
{
    return((long)((float)(Asi.swapout / NumSamples)/Atdiff * HZ));
}

/* swap input activity in KBytes */
long
swapkbin()
{
    if (DisplayMode == AVERAGE)
	Asi.bswapin += (Si.bswapin - Osi.bswapin);

    return((long)((float)(Si.bswapin - Osi.bswapin)/Tdiff * HZ));
}

/* swap input activity in KBytes */
long
ave_swapkbin()
{
    return((long)((float)(Asi.bswapin / NumSamples)/Atdiff * HZ));
}

/* swap output activity in KBytes */
long
swapkbout()
{
    if (DisplayMode == AVERAGE)
	Asi.bswapout += (Si.bswapout - Osi.bswapout);

    return((long)((float)(Si.bswapout - Osi.bswapout)/Tdiff * HZ));
}

/* swap output activity in KBytes */
long
ave_swapkbout()
{
    return((long)((float)(Asi.bswapout / NumSamples)/Atdiff * HZ));
}

long
pswitch()
{
    if (DisplayMode == AVERAGE)
	Asi.pswitch += (Si.pswitch - Osi.pswitch);

    return((long)((float)(Si.pswitch - Osi.pswitch)/Tdiff * HZ));
}

long
ave_pswitch()
{
    return((long)((float)(Asi.pswitch / NumSamples)/Atdiff * HZ));
}

long
igets()
{
    if (DisplayMode == AVERAGE)
	Asi.iget += (Si.iget - Osi.iget);

    return((long)((float)(Si.iget - Osi.iget)/Tdiff * HZ));
}

long
ave_igets()
{
    return((long)((float)(Asi.iget / NumSamples)/Atdiff * HZ));
}

long
nameis()
{
    if (DisplayMode == AVERAGE)
	Asi.namei += (Si.namei - Osi.namei);

    return((long)((float)(Si.namei - Osi.namei)/Tdiff * HZ));
}

long
ave_nameis()
{
    return((long)((float)(Asi.namei / NumSamples)/Atdiff * HZ));
}

long
dirblks()
{
    if (DisplayMode == AVERAGE)
	Asi.dirblk += (Si.dirblk - Osi.dirblk);

    return((long)((float)(Si.dirblk - Osi.dirblk)/Tdiff * HZ));
}

long
ave_dirblks()
{
    return((long)((float)(Asi.dirblk / NumSamples)/Atdiff * HZ));
}

long
runq()
{
    if (DisplayMode == AVERAGE)
	Asi.runque += (Si.runque - Osi.runque);

    return((Si.runque - Osi.runque) == 0 ? 0 :
	((long)((float)(Si.runque - Osi.runque) / 
		(float)(Si.runocc - Osi.runocc))) );
}

long
ave_runq()
{
    return((Asi.runque / NumSamples) == 0 ? 0 :
	((long)((float)(Asi.runque / NumSamples) / 
		(float)(Asi.runocc / NumSamples))) );
}

float
runnable()
{
    if (DisplayMode == AVERAGE)
	Asi.runocc += (Si.runocc - Osi.runocc);

    return((float)(Si.runocc - Osi.runocc)/Tdiff * HZ * 100.0);
}

float
ave_runnable()
{
    return((float)(Asi.runocc / NumSamples)/Atdiff * HZ * 100.0);
}

long
swapq()
{
    if (DisplayMode == AVERAGE)
	Asi.swpque += (Si.swpque - Osi.swpque);

    return((Si.swpque - Osi.swpque) == 0 ? 0 :
	((long)((float)(Si.swpque - Osi.swpque) / 
		(float)(Si.swpocc - Osi.swpocc))) );
}

long
ave_swapq()
{
    return((Asi.swpque / NumSamples) == 0 ? 0 :
	((long)((float)(Asi.swpque / NumSamples) / 
		(float)(Asi.swpocc / NumSamples))) );
}

float
swapable()
{
    if (DisplayMode == AVERAGE)
	Asi.swpocc += (Si.swpocc - Osi.swpocc);

    return((float)(Si.swpocc - Osi.swpocc)/Tdiff * HZ * 100.0);
}

float
ave_swapable()
{
    return((float)(Asi.swpocc / NumSamples)/Atdiff * HZ * 100.0);
}

long
procsz()
{
register struct proc *x;
register int i,n;

	x = (struct proc *)Loc;

	lseek(Kmem,(long)Kinfo[PRO].n_value,0);
	read (Kmem,x,Vi.v_proc*sizeof(struct proc));

	for (i=n=0;i<Vi.v_proc;i++,x++)
		if(x->p_stat !=NULL)
			n++;

	if (DisplayMode == AVERAGE)
	    Aprocsz += n;

	return((long)n);
}

long
ave_procsz()
{
    return((long)Aprocsz / NumSamples);
}

long
procszmax()
{
	return((long)Vi.v_proc);
}

long
procszovf()
{
	if (DisplayMode == AVERAGE)
	    Aprocovf += ((long)E.procovf - Procovf);

	return((long)E.procovf - Procovf);
}

long
ave_procszovf()
{
    return((long)Aprocovf / NumSamples);
}

long
inodesz()
{
register struct inode *x;
register int i,n;

	x = (struct inode *)Loc;

	lseek(Kmem,(long)Kinfo[INO].n_value,0);
	read (Kmem,x,Vi.v_inode*sizeof(struct inode));

	for (i=n=0;i < Vi.v_inode; i++,x++)
		if(x->i_count != 0)
			n++;
	if (DisplayMode == AVERAGE)
	    Ainodesz += n;

	return((long)n);
}

long
ave_inodesz()
{
    return((long)Ainodesz / NumSamples);
}

long
inodeszmax()
{
	return((long)Vi.v_inode);
}

long
inodeszovf()
{
	if (DisplayMode == AVERAGE)
	    Ainodeovf += ((long)E.inodeovf - Inodeovf);

	return((long)E.inodeovf - Inodeovf);
}

long
ave_inodeszovf()
{
    return((long)Ainodeovf / NumSamples);
}

long
filesz()
{
register struct file *x;
register int i,n;

	x = (struct file *)Loc;

	lseek(Kmem,(long)Kinfo[FLE].n_value,0);
	read(Kmem,x,Vi.v_file*sizeof(struct file));

	for (i=n=0;i<Vi.v_file; i++,x++)
		if (x->f_count != 0)
			n++;
	if (DisplayMode == AVERAGE)
	    Afilesz += n;

	return((long)n);
}

long
ave_filesz()
{
    return((long)Afilesz / NumSamples);
}

long
fileszmax()
{
	return((long)Vi.v_file);
}

long
fileszovf()
{
	if (DisplayMode == AVERAGE)
	    Afileovf += ((long)E.fileovf - Fileovf);

	return((long)E.fileovf - Fileovf);
}

long
ave_fileszovf()
{
    return((long)Afileovf / NumSamples);
}

long
locksz()
{
    if (DisplayMode == AVERAGE)
	Aflocksz += FlockInfo.reccnt;

    return((long)FlockInfo.reccnt);
}

long
ave_locksz()
{
    return((long)Aflocksz / NumSamples);
}

long
lockszmax()
{
    return((long)FlockInfo.recs);
}

long
lockszovf()
{
    if (DisplayMode == AVERAGE)
	Aflockovf += ((long)FlockInfo.recovf - Flockovf);

    return((long)FlockInfo.recovf - Flockovf);
}

long
ave_lockszovf()
{
    return((long)Aflockovf / NumSamples);
}

char 
*slash()
{
    return("/");
}

long
mess()
{
    if (DisplayMode == AVERAGE)
	Asi.msg += (Si.msg - Osi.msg);

    return((long)((float)(Si.msg - Osi.msg)/Tdiff * HZ));
}

long
ave_mess()
{
    return((long)((float)(Asi.msg / NumSamples)/Atdiff * HZ));
}

long
sema()
{
    if (DisplayMode == AVERAGE)
	Asi.sema += (Si.sema - Osi.sema);

    return((long)((float)(Si.sema - Osi.sema)/Tdiff * HZ));
}

long
ave_sema()
{
    return((long)((float)(Asi.sema / NumSamples)/Atdiff * HZ));
}

long
addr()
{
    if (DisplayMode == AVERAGE)
	Ami.vfault += (Mi.vfault - Omi.vfault);

    return((long)((float)(Mi.vfault - Omi.vfault)/Tdiff * HZ));
}

long
ave_addr()
{
    return((long)((float)(Ami.vfault / NumSamples)/Atdiff * HZ));
}

float
pct_demand()
{
    if (DisplayMode == AVERAGE)
	Ami.demand += (Mi.demand - Omi.demand);

    return((Mi.vfault - Omi.vfault) == 0 ? 0.0 :
    	(float)(Mi.demand - Omi.demand)/(Mi.vfault - Omi.vfault) * 100.0);
}

float
ave_pct_demand()
{
    return(Ami.vfault == 0 ? 0.0 : (float)(Ami.demand)/(Ami.vfault) * 100.0);
}

float
pct_pgsonswap()
{
    if (DisplayMode == AVERAGE)
	Ami.swap += (Mi.swap - Omi.swap);

    return((Mi.vfault - Omi.vfault) == 0 ? 0.0 :
    	(float)(Mi.swap - Omi.swap)/(Mi.vfault - Omi.vfault) * 100.0);
}

float
ave_pct_pgsonswap()
{
    return(Ami.vfault == 0 ? 0.0 : (float)(Ami.swap/Ami.vfault) * 100.0);
}

float
pct_pgsincache()
{
    if (DisplayMode == AVERAGE)
	Ami.cache += (Mi.cache - Omi.cache);

    return((Mi.vfault - Omi.vfault) == 0 ? 0.0 :
	(float)(Mi.cache - Omi.cache)/(Mi.vfault - Omi.vfault) * 100.0);
}

float
ave_pct_pgsincache()
{
    return(Ami.vfault == 0 ? 0.0 : (float)(Ami.cache)/(Ami.vfault) * 100.0);
}

float
pct_pgsonfile()
{
    /* don't increment Ami.file again, we aready do it in pagein() */

    return((Mi.vfault - Omi.vfault) == 0 ? 0.0 :
    	(float)(Mi.file - Omi.file)/(Mi.vfault - Omi.vfault) * 100.0);
}

float
ave_pct_pgsonfile()
{
    return(Ami.vfault == 0 ? 0.0 : (float)(Ami.file)/(Ami.vfault) * 100.0);
}

long
prot()
{
    if (DisplayMode == AVERAGE)
	Ami.pfault += (Mi.pfault - Omi.pfault);

    return((long)((float)(Mi.pfault - Omi.pfault)/Tdiff * HZ));
}

float
pct_cow()
{
    if (DisplayMode == AVERAGE)
	Ami.cw += (Mi.cw - Omi.cw);

    return((Mi.pfault - Omi.pfault) == 0 ? 0.0 :
    	(float)(Mi.cw - Omi.cw)/(Mi.pfault - Omi.pfault) * 100.0);
}

float
ave_pct_cow()
{
    return(Ami.pfault == 0 ? 0.0 : (float)(Ami.cw)/(Ami.pfault) * 100.0);
}

float
pct_steal()
{
    if (DisplayMode == AVERAGE)
	Ami.steal += (Mi.steal - Omi.steal);

    return((Mi.pfault - Omi.pfault) == 0 ? 0.0 :
    	(float)(Mi.steal - Omi.steal)/(Mi.pfault - Omi.pfault) * 100.0);
}

float
ave_pct_steal()
{
    return(Ami.pfault == 0 ? 0.0 : (float)(Ami.steal)/(Ami.pfault) * 100.0);
}

long
ave_prot()
{
    return((long)((float)(Ami.pfault / NumSamples)/Atdiff * HZ));
}

long
pagein()
{
    if (DisplayMode == AVERAGE)
	Ami.file += (Mi.file - Omi.file);

    return((long)((float)(Mi.file - Omi.file)/Tdiff * HZ));
}

long
ave_pagin()
{
    return((long)((float)(Ami.file / NumSamples)/Atdiff * HZ));
}

long
reclaim()
{
    if (DisplayMode == AVERAGE)
	Ami.freedpgs += (Mi.freedpgs - Omi.freedpgs);

    return((long)((float)(Mi.freedpgs - Omi.freedpgs)/Tdiff * HZ));
}

long
ave_reclaim()
{
    return((long)((float)(Ami.freedpgs / NumSamples)/Atdiff * HZ));
}

/* return freemem value in KBytes.
* It is recorded in hardware page units and we know
* the freemem_units because we checked the machine type.
*/
double
memfree()
{
unsigned long m0,m1;
double	magic = 4.294967296e9;
double  tmp;

    m1 = Mi.freemem[1] - Omi.freemem[1];
    if ( Mi.freemem[0] >= Omi.freemem[0] ) {
	m0 = Mi.freemem[0] - Omi.freemem[0];
    }
    else {
	m0 = m1 + (~(Omi.freemem[0] - Mi.freemem[0]));
        --m1;
    }
    tmp = ((double)((m0 + magic * m1)/Tdiff));

    tmp *= (freemem_units / KBYTE);		/* pages to KBytes */

    if (DisplayMode == AVERAGE)
	Afreemem += tmp;

    return(tmp);
}

/* return freemem value in KBytes.
* It is accumulated in display-value-units -- i.e. Kbytes.
*/
double
ave_memfree()
{
    double  tmp;
    return((double)(Afreemem / (double)NumSamples));
}


/* Free swap space in Kbytes.
* It is recorded in 512-byte block units.
*/
long
swapfree()
{
long	tmp;

    tmp = (long)Mi.freeswap / 2;

    if (DisplayMode == AVERAGE)
	Ami.freeswap += tmp;

    return(tmp);
}

/* Average swap space in Kbytes
* This is accumulated in display units -- i.e. Kbytes
*/
long
ave_swapfree()
{
    return(Ami.freeswap / NumSamples);
}

perrexit()
{
    if ( Curses_init )
	endwin();
    perror("vsar");
    exit(1);
}

