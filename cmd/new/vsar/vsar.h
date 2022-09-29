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
/* $Header: vsar.h,v 1.6.1.4 90/05/09 18:12:18 wje Exp $ */

/*
 * vsar.h	: include file for vsar.c
 *
 *	Randy Menna	12/9/87
*/

/*			VSAR SPECIFICS
*/
#define	USAGE	"usage: vsar [[-ubdycwaqvmprA] | [-S]] [-i <secs>] [-D <num>]"
#define OPTIONS	"ubdycwaqvmpr"
#define SINGOPTS	"urmyabvqwpcd"

/*			DISPLAY VALUES
*/
#define WINLEN		264	/* length of virtual screen */
#define WINWIDTH	80	/* width of virtual screen */
#define	MASK	"        "	/* to clear portion of screen */
#define	LEN		64
#define	MULTI	0		/* multi page display */
#define SINGLE	1		/* single page display */
#define	AVERAGE	2		/* running averages displayed */
#define	DEV_ON_SCREEN	3	/* in single mode 3 devices are displayed */
#define	MAXDEVCAP	32	/* maximum number of devices to display */
#define	CNTR_L  0xC

/*			FUNCTION VALUE TYPES
*/
#define	NONE		-1
#define	FLOAT		0
#define	LONG		1
#define	CHAR		2
#define	DOUBLE		3
#define	FLOAT_3		4	/* 3-digit float.  typ percents */
#define	LONG_K		5	/* a value div by 1024 for display */
#define	FLOAT_1d	6	/* floating with 1 decimal pt displayed */
#define	FLOAT_B		7	/* value is disk blocks	- disp in KB */

/* 			NLIST DEFINES 
*/
#define ID		0
#define SINFO		1
#define	MINFO		2
#define INO		3
#define FLE		4
#define TXT		5
#define PRO		6
#define FLCK		7
#define V		8
#define IDEQ		9
#define SERR		10
#define N_DINFO		11
#define N_MINSERVE	12
#define N_MAXSERVE	13
#define N_ID_STRING	14

#define NNLIST		15


/*			SYSTEM DEPENDENT
*/
#define NDEVS 64	/* max number of block devices */
#define	IO_OPS	0  	/* number of I /O requests since boot  */
#define	IO_BCNT	1  	/* number of blocks transferred since boot */
#define	IO_ACT	2  	/* cumulative time in ticks when drive is active  */
#define	IO_RESP	3  	/* cumulative I/O response time in ticks since boot  */

/* 			VALUE ROUTINES
*/
extern	long	allcall();
extern	float	blktran();
extern	long	bread();
extern	long	bwrite();
extern	long	canchar();
extern	char	*dname();
extern	long	dirblks();
extern	long	excall();
extern	long	fkcall();
extern	long	igets();
extern	long	inchar();
extern	long	nameis();
extern	long	modintr();
extern	long	outchar();
extern	float	pct_user();
extern	float	pct_sys();
extern	float	pct_waitio();
extern	float	pct_idle();
extern	float	pct_rdcache();
extern	float	pct_wrcache();
extern	long	pread();
extern	long	pwrite();
extern	float	pct_busy();
extern	float	pending();
extern	long	rdcall();
extern	long	recintr();
extern	float	rdwr();
extern	float	servtime();
extern	long	sread();
extern	long	swrite();
extern	long	swapin();
extern	long	swapout();
extern	long	swapkbin();
extern	long	swapkbout();
extern	float	waittime();
extern	long	wrcall();
extern	long	rdcallchar();
extern	long	wrcallchar();
extern	long	xmitintr();
extern	long	pswitch();
extern	long	runq();
extern	float	runnable();
extern	long	swapq();
extern	float	swapable();
extern	long	procsz();
extern	long	inodesz();
extern	long	filesz();
extern	long	locksz();
extern	long	procszmax();
extern	long	inodeszmax();
extern	long	fileszmax();
extern	long	lockszmax();
extern	long	procszovf();
extern	long	inodeszovf();
extern	long	fileszovf();
extern	long	lockszovf();
extern	long	mess();
extern	long	sema();
extern	long	addr();
extern	long	prot();
extern	long	pagein();
extern	long	reclaim();
extern	float	pct_demand();
extern	float	pct_pgsonswap();
extern	float	pct_pgsincache();
extern	float	pct_pgsonfile();
extern	float	pct_cow();
extern	float	pct_steal();
extern	double	memfree();
extern	long	swapfree();
extern	char	*slash();
extern	long	ave_allcall();
extern	float	ave_blktran();
extern	long	ave_bread();
extern	long	ave_bwrite();
extern	long	ave_canchar();
extern	long	ave_dirblks();
extern	long	ave_excall();
extern	long	ave_fkcall();
extern	long	ave_igets();
extern	long	ave_inchar();
extern	long	ave_nameis();
extern	long	ave_modintr();
extern	long	ave_outchar();
extern	float	ave_pct_user();
extern	float	ave_pct_sys();
extern	float	ave_pct_waitio();
extern	float	ave_pct_idle();
extern	float	ave_pct_rdcache();
extern	float	ave_pct_wrcache();
extern	long	ave_pread();
extern	long	ave_pwrite();
extern	float	ave_pct_busy();
extern	float	ave_pending();
extern	long	ave_rdcall();
extern	long	ave_recintr();
extern	float	ave_rdwr();
extern	float	ave_servtime();
extern	long	ave_sread();
extern	long	ave_swrite();
extern	long	ave_swapin();
extern	long	ave_swapout();
extern	long	ave_swapkbin();
extern	long	ave_swapkbout();
extern	float	ave_waittime();
extern	long	ave_wrcall();
extern	long	ave_rdcallchar();
extern	long	ave_wrcallchar();
extern	long	ave_xmitintr();
extern	long	ave_pswitch();
extern	long	ave_runq();
extern	float	ave_runnable();
extern	long	ave_swapq();
extern	float	ave_swapable();
extern	long	ave_procsz();
extern	long	ave_inodesz();
extern	long	ave_filesz();
extern	long	ave_locksz();
extern	long	ave_procszovf();
extern	long	ave_inodeszovf();
extern	long	ave_fileszovf();
extern	long	ave_lockszovf();
extern	long	ave_mess();
extern	long	ave_sema();
extern	long	ave_addr();
extern	long	ave_prot();
extern	long	ave_pagin();
extern	long	ave_reclaim();
extern	double	ave_memfree();
extern	long	ave_swapfree();
extern	float	ave_pct_demand();
extern	float	ave_pct_pgsonswap();
extern	float	ave_pct_pgsincache();
extern	float	ave_pct_pgsonfile();
extern	float	ave_pct_cow();
extern	float	ave_pct_steal();


/*			DATA STRUCTURES
*/

/* label holds the predefined
 * display label and the function type that generates the
 * value associated with that label, if any
*/
typedef struct label {
	char	*name;
	int	ftype;
} label;

/* floating point function */
typedef struct fval {
	float	(*value)();
} fval;

/* long function */
typedef struct lval {
	long	(*value)();
} lval;

/* char pointer function */
typedef struct cval {
	char	*(*value)();
} cval;

/* double function */
typedef struct dval {
	double	(*value)();
} dval;

/* position holds the "unit" positions for the label
 * and the value associated with the label
*/
typedef struct position {
	int	lx;
	int	ly;
	int	vx;
	int	vy;
} position;

/* vscreen is a structure that contains the
 * entire virtual screen to be displayed
 * it is built from the predefines described above
*/

/* floating point function with int arg */
typedef struct vsfval {
	float	(*fvalue)();
	int	farg;
} vsfval;

/* char function with int arg */
typedef struct vscval {
	char	*(*cvalue)();
	int	carg;
} vscval;

typedef struct vscreen {
	char	*label;
	int	ftype;
	union  {
	    vsfval	fv;
	    long	(*lvalue)();
	    vscval	cv;
	    double	(*dvalue)();
	} func;
	int	lx, ly;
	int	vx, vy;
} vscreen;


/*			PREDEFINED LABELS AND VALUE FUNCTIONS
*/

/*		MULTI-PAGE DISPLAY
*/
#define	CPU	0
label	cpu[] =	{
			{ "CPU Utilization", NONE },
			{ "%user", FLOAT_3},
			{ "%system", FLOAT_3},
			{ "%wait io", FLOAT_3},
			{ "%idle", FLOAT_3},
			{0}
};

fval	cpufv[] = { 0, pct_user, pct_sys, pct_waitio, pct_idle };

position	cpos[] = {
			{ 5, 0, -1, -1 },
			{ 5, 1, 14, 1 },
			{ 5, 2, 14, 2 },
			{ 5, 3, 14, 3 },
			{ 5, 4, 14, 4 }
};

#define	BUF	1
label	buf[] = 	{
			{ "Buffer Activity", NONE },
			{ "blocks read/sec", LONG },
			{ "sys buf read/sec", LONG },
			{ "%read cache hit", FLOAT_1d },
			{ "blocks write/sec", LONG },
			{ "sys buf write/sec", LONG },
			{ "%write cache hit", FLOAT_1d },
			{ "phys io read/sec", LONG },
			{ "phys io write/sec", LONG },
			{0}
};

lval	buflv[] = { 0, bread, sread, 0, bwrite, swrite, 0, pread, pwrite };
fval	buffv[] = { 0, 0, 0, pct_rdcache, 0, 0, pct_wrcache, 0, 0 };

position	bpos[] = {
			{ 3, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
			{ 1, 3, 19, 3 },
			{ 1, 4, 19, 4 },
			{ 1, 5, 19, 5 },
			{ 1, 6, 19, 6 },
			{ 1, 7, 19, 7 },
			{ 1, 8, 19, 8 },
};

#define	DEV	2
label	dev[] = 	{
			{ "Block Device Activity", NONE },
			{ "device name :", CHAR },
			{ "%busy service", FLOAT },
			{ "ave req pending", FLOAT },
			{ "read & write/sec", FLOAT },
			{ "KBytes transfered", FLOAT_B },
			{ "ave wait time", FLOAT },
			{ "ave service time", FLOAT },
			{0}
};

cval	devcv[] = { 0, dname, 0, 0, 0, 0, 0, 0 };
fval	devfv[] = { 0, 0, pct_busy, pending, rdwr, blktran, waittime, servtime};

position	dpos[] = {
			{ 1, 0, -1, -1 },
			{ 1, 1, 16, 1 },
			{ 0, 2, 18, 2 },
			{ 0, 3, 18, 3 },
			{ 0, 4, 18, 4 },
			{ 0, 5, 18, 5 },
			{ 0, 6, 18, 6 },
			{ 0, 7, 18, 7 },
};

#define	TTY	3
label	tty[] = 	{
			{ "TTY Device Activity", NONE },
			{ "input char/sec", LONG },
			{ "input canon/sec", LONG },
			{ "output char/sec", LONG },
			{ "rec intrpt/sec", LONG },
			{ "xmit intrpt/sec", LONG },
			{ "modem intrpt/sec", LONG },
			{0}
};

lval	ttylv[] = { 0, inchar, canchar, outchar, recintr, xmitintr, modintr };
fval	ttyfv[] = { 0, 0, 0, 0, 0, 0, 0 };

position	ypos[] = {
			{ 0, 0, -1, -1 },
			{ 0, 1, 17, 1 },
			{ 0, 2, 17, 2 },
			{ 0, 3, 17, 3 },
			{ 0, 4, 17, 4 },
			{ 0, 5, 17, 5 },
			{ 0, 6, 17, 6 },
};

#define	SYSC	4
label	sys[] = 	{
			{ "System Call Activity", NONE  },
			{ "all sys calls/sec", LONG  },
			{ "read/sec", LONG  },
			{ "write/sec", LONG  },
			{ "fork/sec", LONG  },
			{ "exec/sec", LONG  },
			{ "read KBytess/sec", LONG_K  },
			{ "write KBytes/sec", LONG_K  },
			{0}
};

lval	syslv[] = { 0, allcall, rdcall, wrcall, fkcall, 
			excall, rdcallchar, wrcallchar };

position	spos[] = {
			{ 1, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
			{ 1, 3, 19, 3 },
			{ 1, 4, 19, 4 },
			{ 1, 5, 19, 5 },
			{ 1, 6, 19, 6 },
			{ 1, 7, 19, 7 },
};

#define	SWAP	5
label	swap[] = 	{
			{ "Swapping & Switching", NONE },
			{ "swap in/sec", LONG },
			{ "swap out/sec", LONG },
			{ "KB swap in/sec", LONG },
			{ "KB swap out/sec", LONG },
			{ "proc switch/sec", LONG },
			{0}
};

lval	swaplv[] = { 0, swapin, swapout, swapkbin, swapkbout, pswitch };

position	wpos[] = {
			{ 1, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
			{ 1, 3, 19, 3 },
			{ 1, 4, 19, 4 },
			{ 1, 5, 19, 5 },
};

#define	FILE	6
label	fileact[] = 	{
			{ "File System Call Activity", NONE },
			{ "iget/sec", LONG },
			{ "namei/sec", LONG },
			{ "dirblk/sec", LONG },
			{0}
};

lval	filelv[] = { 0, igets, nameis, dirblks };

position	fpos[] = {
			{ 0, 0, -1, -1 },
			{ 5, 1, 16, 1 },
			{ 5, 2, 16, 2 },
			{ 5, 3, 16, 3 },
};

#define	QUE	7
label	que[] = 	{
			{ "Queue Activity", NONE },
			{ "runq size", LONG },
			{ "%runnable-in", FLOAT },
			{ "swapq size", LONG },
			{ "%runnable-out", FLOAT },
			{0}
};

lval	quelv[] = { 0, runq, 0, swapq, 0 };
fval	quefv[] = { 0, 0, runnable, 0, swapable };

position	qpos[] = {
			{ 1, 0, -1, -1 },
			{ 0, 1, 14, 1 },
			{ 0, 2, 14, 2 },
			{ 0, 3, 14, 3 },
			{ 0, 4, 14, 4 },
};

#define	TBL	8
label	tbl[] = 	{
			{ "Table Activity", NONE },
			{ "proc tbl size", LONG },
			{ "inode tbl size", LONG },
			{ "file tbl size", LONG },
			{ "lock tbl size", LONG },
			{ "", CHAR },
			{ "", CHAR },
			{ "", CHAR },
			{ "", CHAR },
			{ "", LONG },
			{ "", LONG },
			{ "", LONG },
			{ "", LONG },
			{ "proc tbl overflows", LONG },
			{ "inode tbl overflows", LONG },
			{ "file tbl overflows", LONG },
			{ "lock tbl overflows", LONG },
			{0}
};

lval	tbllv[] = { 0, procsz, inodesz, filesz, locksz, 0, 0, 0, 0,
			procszmax, inodeszmax, fileszmax, lockszmax,
			procszovf, inodeszovf, fileszovf, lockszovf };
cval	tblcv[] = { 0, 0, 0, 0, 0, slash, slash, slash, slash, 
			0, 0, 0, 0, 0, 0, 0, 0 };
position	tpos[] = {
			{ 3, 0, -1, -1 },
			{ 0, 1, 15, 1 },
			{ 0, 2, 15, 2 },
			{ 0, 3, 15, 3 },
			{ 0, 4, 15, 4 },
			{ -1, -1, 18, 1 },
			{ -1, -1, 18, 2 },
			{ -1, -1, 18, 3 },
			{ -1, -1, 18, 4 },
			{ -1, -1, 19, 1 },
			{ -1, -1, 19, 2 },
			{ -1, -1, 19, 3 },
			{ -1, -1, 19, 4 },
			{ 0, 5, 20, 5 },
			{ 0, 6, 20, 6 },
			{ 0, 7, 20, 7 },
			{ 0, 8, 20, 8 },
};

#define	IPC	9
label	ipc[] = 	{
			{ "IPC Activity", NONE },
			{ "message ops/sec", LONG },
			{ "semaphore ops/sec", LONG },
			{0}
};

lval	ipclv[] = { 0, mess, sema };

position	ipos[] = {
			{ 3, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
};

#define	PAGE	10
label	page[] = 	{
			{ "Paging Activity", NONE },
			{ "addr trans flt/sec", LONG },
			{ "% demand fill", FLOAT },
			{ "% pages on swap", FLOAT },
			{ "% pages in cache", FLOAT },
			{ "% page-ins", FLOAT },
			{ "prot err flt/sec", LONG },
			{ "% copy on write", FLOAT },
			{ "% steal the page", FLOAT },
			{ "page-in/sec", LONG },
			{ "reclaim flt/sec", LONG },
			{0}
};

lval	pagelv[] = { 0, addr, 0, 0, 0, 0, prot, 0, 0, pagein, reclaim };
fval	pagefv[] = { 0, 0,
		     pct_demand, pct_pgsonswap, pct_pgsincache, pct_pgsonfile, 
		     0, pct_cow, pct_steal, 0, 0 };

position	ppos[] = {
			{ 1, 0, -1, -1 },
			{ 0, 1, 25, 1 },
			{ 0, 2, 25, 2 },
			{ 0, 3, 25, 3 },
			{ 0, 4, 25, 4 },
			{ 0, 5, 25, 5 },
			{ 0, 6, 25, 6 },
			{ 0, 7, 25, 7 },
			{ 0, 8, 25, 8 },
			{ 0, 9, 25, 9 },
			{ 0, 10, 25, 10 },
};

#define	MEM	11
label	mem[] = 	{
			{ "Memory Activity", NONE },
			{ "# free mem KBytes", DOUBLE },
			{ "# free swap KBytes", LONG },
			{0}
};

dval	memdv[] = { 0, memfree, 0 };
lval	memlv[] = { 0, 0, swapfree };

position	mpos[] = {
			{ 5, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
};

/* all the labels */
label	*display[] = {
		cpu,
		buf,
		dev,
		tty,
		sys,
		swap,
		fileact,
		que,
		tbl,
		ipc,
		page,
		mem
};

/* the floating point functions */
fval	*dispfval[] = {
	cpufv,
	buffv,
	devfv,
	ttyfv,
	0,
	0,
	0,
	quefv,
	0,
	0,
	pagefv,
	0
};

/* the long functions */
lval	*displval[] = {
	0,
	buflv,
	0,
	ttylv,
	syslv,
	swaplv,
	filelv,
	quelv,
	tbllv,
	ipclv,
	pagelv,
	memlv
};

/* the char functions */
cval	*dispcval[] = {
	0,
	0,
	devcv,
	0,
	0,
	0,
	0,
	0,
	tblcv,
	0,
	0,
	0
};

/* the double functions */
dval	*dispdval[] = {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	memdv
};

/* all the positions */
position	*pos[] = {
		cpos,
		bpos,
		dpos,
		ypos,
		spos,
		wpos,
		fpos,
		qpos,
		tpos,
		ipos,
		ppos,
		mpos
};

/*			MAIN SCREEN PREDEFINES
*/

/* Row offset for with which to modify "unit" positions */
int	y_offset[] = { 2, 13, 26, 37, 50, 61, 74, 85, 98, 109, 122, 133, 146, 157, 170, 181, 194, 205, 218, 229, 242, 253 };

/* Col offset for with which to modify "unit" positions */
#define XSETS	3
int	x_offset[] = { 0, 26, 53 };

/* Page offset for displaying the pages */
int	Pages[] = { 0, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240 };


/*		SINGLE PAGE DISPLAY
*/
label	spcpu[] =	{
			{ "Cpu:", NONE },
			{ "%usr", FLOAT_3},
			{ "%sys", FLOAT_3},
			{ "%wio", FLOAT_3},
			{ "%idl", FLOAT_3},
			{0}
};

fval	spcpufv[] = { 0, pct_user, pct_sys, pct_waitio, pct_idle };

position	spcpos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 6, 0, 6, 1 },
			{ 12, 0, 12, 1 },
			{ 19, 0, 19, 1 }
};

label	spbuf[] = 	{
			{ "Buf:", NONE },
			{ "bread/s", LONG },
			{ "lread/s", LONG },
			{ "%rcache", FLOAT_1d },
			{ "bwrite/s", LONG },
			{ "lwrite/s", LONG },
			{ "%wcache", FLOAT_1d },
			{ "pread/s", LONG },
			{ "pwrite/s", LONG },
			{0}
};

lval	spbuflv[] = { 0, bread, sread, 0, bwrite, swrite, 0, pread, pwrite };
fval	spbuffv[] = { 0, 0, 0, pct_rdcache, 0, 0, pct_wrcache, 0, 0 };

position	spbpos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 9, 0, 9, 1 },
			{ 18, 0, 18, 1 },
			{ 27, 0, 27, 1 },
			{ 37, 0, 37, 1 },
			{ 47, 0, 47, 1 },
			{ 56, 0, 56, 1 },
			{ 65, 0, 65, 1 },
};

label	spdev[] = 	{
			{ "Block Device Activity", NONE },
			{ "device", CHAR },
			{ "%busy", FLOAT },
			{ "avque", FLOAT },
			{ "r+w/s", FLOAT },
			{ " KB/s", FLOAT_B },
			{ "avwait", FLOAT },
			{ "avserv", FLOAT },
			{0}
};

cval	spdevcv[] = { 0, dname, 0, 0, 0, 0, 0, 0 };
fval	spdevfv[] = { 0, 0, pct_busy, pending, rdwr, blktran, waittime, servtime};

position	spdpos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 8, 0, 8, 1 },
			{ 15, 0, 15, 1 },
			{ 22, 0, 22, 1 },
			{ 29, 0, 29, 1 },
			{ 36, 0, 36, 1 },
			{ 44, 0, 44, 1 },
};

label	sptty[] = 	{
			{ "Tty:", NONE },
			{ "in-ch/s", LONG },
			{ "in-can/s", LONG },
			{ "out-ch/s", LONG },
			{ "rec/s", LONG },
			{ "xmit/s", LONG },
			{ "modem/s", LONG },
			{0}
};

lval	spttylv[] = { 0, inchar, canchar, outchar, recintr, xmitintr, modintr };
fval	spttyfv[] = { 0, 0, 0, 0, 0, 0, 0 };

position	spypos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 8, 0, 8, 1 },
			{ 17, 0, 17, 1 },
			{ 27, 0, 27, 1 },
			{ 33, 0, 33, 1 },
			{ 40, 0, 40, 1 },
};

label	spsys[] = 	{
			{ "Sys Call:", NONE  },
			{ "scall/s", LONG  },
			{ "read/s", LONG  },
			{ "write/s", LONG  },
			{ "fork/s", LONG  },
			{ "exec/s", LONG  },
			{ "rd_KB/s", LONG_K  },
			{ "wr_KB/s", LONG_K  },
			{0}
};

lval	spsyslv[] = { 0, allcall, rdcall, wrcall, fkcall, 
			excall, rdcallchar, wrcallchar };

position	spspos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 10, 0, 10, 1 },
			{ 20, 0, 20, 1 },
			{ 30, 0, 30, 1 },
			{ 40, 0, 40, 1 },
			{ 50, 0, 50, 1 },
			{ 60, 0, 60, 1 },
};

label	spswap[] = 	{
			{ "Swap:", NONE },
			{ "swpin/s", LONG },
			{ "swpot/s", LONG },
			{ "Kswpin/s", LONG },
			{ "Kswpot/s", LONG },
			{ "pswtch/s", LONG },
			{0}
};

lval	spswaplv[] = { 0, swapin, swapout, swapkbin, swapkbout, pswitch };

position	spwpos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 8, 0, 8, 1 },
			{ 16, 0, 16, 1 },
			{ 25, 0, 25, 1 },
			{ 34, 0, 34, 1 },
};

label	spfileact[] = 	{
			{ "Fil:", NONE },
			{ "iget/s", LONG },
			{ "namei/s", LONG },
			{ "dirbk/s", LONG },
			{0}
};

lval	spfilelv[] = { 0, igets, nameis, dirblks };

position	spfpos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 7, 0, 7, 1 },
			{ 15, 0, 15, 1 },
};

label	spque[] = 	{
			{ "Que:", NONE },
			{ "runq-sz", LONG },
			{ "%runocc", FLOAT },
			{ "swpq-sz", LONG },
			{ "%swpocc", FLOAT },
			{0}
};

lval	spquelv[] = { 0, runq, 0, swapq, 0 };
fval	spquefv[] = { 0, 0, runnable, 0, swapable };

position	spqpos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 9, 0, 9, 1 },
			{ 18, 0, 18, 1 },
			{ 27, 0, 27, 1 },
};

label	sptbl[] = 	{
			{ "Tbl:", NONE },
			{ "proc-size", LONG },
			{ "", CHAR },		/* slash */
			{ "", LONG },		/* max */
			{ "ov", LONG },		/* overflow */
			{ "inod-size", LONG },
			{ "", CHAR },		/* slash */
			{ "", LONG },		/* max */
			{ "ov", LONG },		/* overflow */
			{ "file-size", LONG },
			{ "", CHAR },		/* slash */
			{ "", LONG },		/* max */
			{ "ov", LONG },		/* overflow */
			{ "lock-size", LONG },
			{ "", CHAR },		/* slash */
			{ "", LONG },		/* max */
			{ "ov", LONG },		/* overflow */
			{0}
};

lval	sptbllv[] = { 0, procsz, 0, procszmax, procszovf, 
			inodesz, 0, inodeszmax, inodeszovf, 
			filesz, 0, fileszmax, fileszovf, 
			locksz, 0, lockszmax, lockszovf };
cval	sptblcv[] = { 0, 0, slash, 0, 0, 0, slash, 0, 0, 
			0, slash, 0, 0, 0, slash, 0, 0 };

position	sptpos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ -1, -1, 6, 1 },
			{ -1, -1, 7, 1 },
			{ 13, 0, 13, 1 },
			{ 18, 0, 18, 1 },
			{ -1, -1, 24, 1 },
			{ -1, -1, 25, 1 },
			{ 31, 0, 31, 1 },
			{ 36, 0, 36, 1 },
			{ -1, -1, 42, 1 },
			{ -1, -1, 43, 1 },
			{ 49, 0, 49, 1 },
			{ 54, 0, 54, 1 },
			{ -1, -1, 60, 1 },
			{ -1, -1, 61, 1 },
			{ 67, 0, 67, 1 },
};

label	spipc[] = 	{
			{ "Ipc:", NONE },
			{ "msg/s", LONG },
			{ "sema/s", LONG },
			{0}
};

lval	spipclv[] = { 0, mess, sema };

position	spipos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 6, 0, 6, 1 },
};

label	sppage[] = 	{
			{ "Page:", NONE },
			{ "vflt/s", LONG },
			{ "%demand", FLOAT },
			{ "%swap", FLOAT },
			{ "%cache", FLOAT },
			{ "%pgfil", FLOAT },
			{ "pflt/s", LONG },
			{ "%cow", FLOAT },
			{ "%steal", FLOAT },
			{ "pgfil/s", LONG },
			{ "rclm/s", LONG },
			{0}
};

lval	sppagelv[] = { 0, addr, 0, 0, 0, 0, prot, 0, 0, pagein, reclaim };
fval	sppagefv[] = { 0, 0, pct_demand, pct_pgsonswap, pct_pgsincache, 
		       pct_pgsonfile, 0, pct_cow, pct_steal, 0, 0 };

position	spppos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 8, 0, 10, 1 },
			{ 16, 0, 16, 1 },
			{ 24, 0, 25, 1 },
			{ 32, 0, 33, 1 },
			{ 40, 0, 40, 1 },
			{ 49, 0, 48, 1 },
			{ 56, 0, 57, 1 },
			{ 64, 0, 64, 1 },
			{ 72, 0, 72, 1 },
};

label	spmem[] = 	{
			{ "Mem:", NONE },
			{ "free-Kmem", DOUBLE },
			{ "free-Kswp", LONG },
			{0}
};

dval	spmemdv[] = { 0, memfree, 0 };
lval	spmemlv[] = { 0, 0, swapfree };

position	spmpos[] = {
			{ -1, -1, -1, -1 },
			{ 0, 0, 0, 1 },
			{ 10, 0, 10, 1 },
};

/* all the labels */
label	*spdisplay[] = {
		spcpu,
		spbuf,
		spdev,
		sptty,
		spsys,
		spswap,
		spfileact,
		spque,
		sptbl,
		spipc,
		sppage,
		spmem
};

/* the floating point functions */
fval	*spdispfval[] = {
	spcpufv,
	spbuffv,
	spdevfv,
	spttyfv,
	0,
	0,
	0,
	spquefv,
	0,
	0,
	sppagefv,
	0
};

/* the long functions */
lval	*spdisplval[] = {
	0,
	spbuflv,
	0,
	spttylv,
	spsyslv,
	spswaplv,
	spfilelv,
	spquelv,
	sptbllv,
	spipclv,
	sppagelv,
	spmemlv
};

/* the char functions */
cval	*spdispcval[] = {
	0,
	0,
	spdevcv,
	0,
	0,
	0,
	0,
	0,
	sptblcv,
	0,
	0,
	0
};

/* the double functions */
dval	*spdispdval[] = {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	spmemdv
};

/* all the positions */
position	*sppos[] = {
		spcpos,
		spbpos,
		spdpos,
		spypos,
		spspos,
		spwpos,
		spfpos,
		spqpos,
		sptpos,
		spipos,
		spppos,
		spmpos
};

/* Row offset for with which to modify "unit" positions */
int	spy_offset[] = { 1, 1, 1, 4, 4, 7, 10, 13, 13, 16, 19, 22, 
			23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
			34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,
			47, 48, 49, 50, 51, 52, 53, 54, 55, 60, 61, 62, 63,
			64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
			77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
			90, 91, 92 };

/* Col offset for with which to modify "unit" positions */
int	spx_offset[] = { 0, 34, 58, 0, 53, 0, 3, 0, 37, 0, 5, 15, 15,
			15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
			15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
			15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
			15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
			15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15 };


/*		MULTI-PAGE DISPLAY
*/
label	avcpu[] =	{
			{ "CPU Utilization", NONE },
			{ "%user", FLOAT_3},
			{ "%system", FLOAT_3},
			{ "%wait io", FLOAT_3},
			{ "%idle", FLOAT_3},
			{ "%av:", FLOAT_3},
			{ "%av:", FLOAT_3},
			{ "%av:", FLOAT_3},
			{ "%av:", FLOAT_3},
			{0}
};

fval	avcpufv[] = { 0, pct_user, pct_sys, pct_waitio, pct_idle,
	ave_pct_user, ave_pct_sys, ave_pct_waitio, ave_pct_idle };

position	avcpos[] = {
			{ 12, 0, -1, -1 },
			{ 5, 1, 14, 1 },
			{ 5, 2, 14, 2 },
			{ 5, 3, 14, 3 },
			{ 5, 4, 14, 4 },
			{ 22, 1, 27, 1 },
			{ 22, 2, 27, 2 },
			{ 22, 3, 27, 3 },
			{ 22, 4, 27, 4 }
};

label	avbuf[] = 	{
			{ "Buffer Activity", NONE },
			{ "blocks read/sec", LONG },
			{ "sys buf read/sec", LONG },
			{ "%read cache hit", FLOAT_1d },
			{ "blocks write/sec", LONG },
			{ "sys buf write/sec", LONG },
			{ "%write cache hit", FLOAT_1d },
			{ "phys io read/sec", LONG },
			{ "phys io write/sec", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "%av:", FLOAT_1d },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "%av:", FLOAT_1d },
			{ "av:", LONG },
			{ "av:", LONG },
			{0}
};

lval	avbuflv[] = { 0, bread, sread, 0, bwrite, swrite, 0, pread, pwrite,
    ave_bread, ave_sread, 0, ave_bwrite, ave_swrite, 0, ave_pread, ave_pwrite };
fval	avbuffv[] = { 0, 0, 0, pct_rdcache, 0, 0, pct_wrcache, 0, 0,
	0, 0, 0, ave_pct_rdcache, 0, 0, ave_pct_wrcache, 0, 0 };

position	avbpos[] = {
			{ 9, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
			{ 1, 3, 19, 3 },
			{ 1, 4, 19, 4 },
			{ 1, 5, 19, 5 },
			{ 1, 6, 19, 6 },
			{ 1, 7, 19, 7 },
			{ 1, 8, 19, 8 },
			{ 27, 1, 32, 1 },
			{ 27, 2, 32, 2 },
			{ 27, 3, 32, 3 },
			{ 27, 4, 32, 4 },
			{ 27, 5, 32, 5 },
			{ 27, 6, 32, 6 },
			{ 27, 7, 32, 7 },
			{ 27, 8, 32, 8 },
};

label	avdev[] = 	{
			{ "Block Device Activity", NONE },
			{ "device name :", CHAR },
			{ "%busy service", FLOAT },
			{ "ave req pending", FLOAT },
			{ "read & write/sec", FLOAT },
			{ "KBytes transfered", FLOAT_B },
			{ "ave wait time", FLOAT },
			{ "ave service time", FLOAT },
			{ "%av:", FLOAT },
			{ "av:", FLOAT },
			{ "av:", FLOAT },
			{ "av:", FLOAT_B },
			{ "av:", FLOAT },
			{ "av:", FLOAT },
			{0}
};

cval	avdevcv[] = { 0, dname, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
fval	avdevfv[] = { 0, 0, pct_busy, pending, rdwr, blktran, waittime, 
		servtime, ave_pct_busy, ave_pending, ave_rdwr, ave_blktran, 
		ave_waittime, ave_servtime};

position	avdpos[] = {
			{ 6, 0, -1, -1 },
			{ 6, 1, 22, 1 },
			{ 0, 2, 18, 2 },
			{ 0, 3, 18, 3 },
			{ 0, 4, 18, 4 },
			{ 0, 5, 18, 5 },
			{ 0, 6, 18, 6 },
			{ 0, 7, 18, 7 },
			{ 26, 2, 31, 2 },
			{ 26, 3, 31, 3 },
			{ 26, 4, 31, 4 },
			{ 26, 5, 31, 5 },
			{ 26, 6, 31, 6 },
			{ 26, 7, 31, 7 },
};

label	avtty[] = 	{
			{ "TTY Device Activity", NONE },
			{ "input char/sec", LONG },
			{ "input canon/sec", LONG },
			{ "output char/sec", LONG },
			{ "rec intrpt/sec", LONG },
			{ "xmit intrpt/sec", LONG },
			{ "modem intrpt/sec", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{0}
};

lval	avttylv[] = { 0, inchar, canchar, outchar, recintr, xmitintr, modintr,
		    ave_inchar, ave_canchar, ave_outchar, ave_recintr, 
		    ave_xmitintr, ave_modintr };
fval	avttyfv[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

position	avypos[] = {
			{ 5, 0, -1, -1 },
			{ 0, 1, 17, 1 },
			{ 0, 2, 17, 2 },
			{ 0, 3, 17, 3 },
			{ 0, 4, 17, 4 },
			{ 0, 5, 17, 5 },
			{ 0, 6, 17, 6 },
			{ 25, 1, 30, 1 },
			{ 25, 2, 30, 2 },
			{ 25, 3, 30, 3 },
			{ 25, 4, 30, 4 },
			{ 25, 5, 30, 5 },
			{ 25, 6, 30, 6 },
};

label	avsys[] = 	{
			{ "System Call Activity", NONE  },
			{ "all sys calls/sec", LONG  },
			{ "read/sec", LONG  },
			{ "write/sec", LONG  },
			{ "fork/sec", LONG  },
			{ "exec/sec", LONG  },
			{ "read KByte/sec", LONG_K  },
			{ "write KByte/sec", LONG_K  },
			{ "av:", LONG  },
			{ "av:", LONG  },
			{ "av:", LONG  },
			{ "av:", LONG  },
			{ "av:", LONG  },
			{ "av:", LONG_K  },
			{ "av:", LONG_K  },
			{0}
};

lval	avsyslv[] = { 0, allcall, rdcall, wrcall, fkcall, excall, rdcallchar, 
		    wrcallchar, ave_allcall, ave_rdcall, ave_wrcall, 
		    ave_fkcall, ave_excall, ave_rdcallchar, ave_wrcallchar };

position	avspos[] = {
			{ 10, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
			{ 1, 3, 19, 3 },
			{ 1, 4, 19, 4 },
			{ 1, 5, 19, 5 },
			{ 1, 6, 19, 6 },
			{ 1, 7, 19, 7 },
			{ 27, 1, 32, 1 },
			{ 27, 2, 32, 2 },
			{ 27, 3, 32, 3 },
			{ 27, 4, 32, 4 },
			{ 27, 5, 32, 5 },
			{ 27, 6, 32, 6 },
			{ 27, 7, 32, 7 },
};

label	avswap[] = 	{
			{ "Swapping & Switching", NONE },
			{ "swap in/sec", LONG },
			{ "swap out/sec", LONG },
			{ "KB swap in/sec", LONG },
			{ "KB swap out/sec", LONG },
			{ "proc switch/sec", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{0}
};

lval	avswaplv[] = { 0, swapin, swapout, swapkbin, swapkbout, pswitch, 
	ave_swapin, ave_swapout, ave_swapkbin, ave_swapkbout, ave_pswitch };

position	avwpos[] = {
			{ 10, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
			{ 1, 3, 19, 3 },
			{ 1, 4, 19, 4 },
			{ 1, 5, 19, 5 },
			{ 27, 1, 32, 1 },
			{ 27, 2, 32, 2 },
			{ 27, 3, 32, 3 },
			{ 27, 4, 32, 4 },
			{ 27, 5, 32, 5 },
};

label	avfileact[] = 	{
			{ "File System Call Activity", NONE },
			{ "iget/sec", LONG },
			{ "namei/sec", LONG },
			{ "dirblk/sec", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{0}
};

lval	avfilelv[] = { 0, igets, nameis, dirblks, ave_igets, ave_nameis, 
			ave_dirblks };

position	avfpos[] = {
			{ 7, 0, -1, -1 },
			{ 5, 1, 29, 1 },
			{ 5, 2, 29, 2 },
			{ 5, 3, 29, 3 },
			{ 24, 1, 29, 1 },
			{ 24, 2, 29, 2 },
			{ 24, 3, 29, 3 },
};

label	avque[] = 	{
			{ "Queue Activity", NONE },
			{ "runq size", LONG },
			{ "%runnable-in", FLOAT },
			{ "swapq size", LONG },
			{ "%runnable-out", FLOAT },
			{ "av:", LONG },
			{ "%av:", FLOAT },
			{ "av:", LONG },
			{ "%av:", FLOAT },
			{0}
};

lval	avquelv[] = { 0, runq, 0, swapq, 0, ave_runq, 0, ave_swapq, 0 };
fval	avquefv[] = { 0, 0, runnable, 0, swapable, 0, ave_runnable, 0, 
			ave_swapable };

position	avqpos[] = {
			{ 10, 0, -1, -1 },
			{ 0, 1, 14, 1 },
			{ 0, 2, 14, 2 },
			{ 0, 3, 14, 3 },
			{ 0, 4, 14, 4 },
			{ 22, 1, 27, 1 },
			{ 22, 2, 27, 2 },
			{ 22, 3, 27, 3 },
			{ 22, 4, 27, 4 },
};

label	avtbl[] = 	{
			{ "Table Activity", NONE },
			{ "proc tbl size", LONG },
			{ "inode tbl size", LONG },
			{ "file tbl size", LONG },
			{ "lock tbl size", LONG },
			{ "", CHAR },
			{ "", CHAR },
			{ "", CHAR },
			{ "", CHAR },
			{ "", LONG },
			{ "", LONG },
			{ "", LONG },
			{ "", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "proc tbl overflows", LONG },
			{ "inode tbl overflows", LONG },
			{ "file tbl overflows", LONG },
			{ "lock tbl overflows", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{0}
};

lval	avtbllv[] = { 0, procsz, inodesz, filesz, locksz, 0, 0, 0, 0,
		procszmax, inodeszmax, fileszmax, lockszmax,
		ave_procsz, ave_inodesz, ave_filesz, ave_locksz,
		procszovf, inodeszovf, fileszovf, lockszovf,
		ave_procszovf, ave_inodeszovf, ave_fileszovf, ave_lockszovf };
cval	avtblcv[] = { 0, 0, 0, 0, 0, slash, slash, slash, slash, 
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0  };
position	avtpos[] = {
			{ 13, 0, -1, -1 },
			{ 0, 1, 15, 1 },
			{ 0, 2, 15, 2 },
			{ 0, 3, 15, 3 },
			{ 0, 4, 15, 4 },
			{ -1, -1, 18, 1 },
			{ -1, -1, 18, 2 },
			{ -1, -1, 18, 3 },
			{ -1, -1, 18, 4 },
			{ -1, -1, 19, 1 },
			{ -1, -1, 19, 2 },
			{ -1, -1, 19, 3 },
			{ -1, -1, 19, 4 },
			{ 27, 1, 32, 1 },
			{ 27, 2, 32, 2 },
			{ 27, 3, 32, 3 },
			{ 27, 4, 32, 4 },
			{ 0, 5, 20, 5 },
			{ 0, 6, 20, 6 },
			{ 0, 7, 20, 7 },
			{ 0, 8, 20, 8 },
			{ 28, 5, 33, 5 },
			{ 28, 6, 33, 6 },
			{ 28, 7, 33, 7 },
			{ 28, 8, 33, 8 },
};

label	avipc[] = 	{
			{ "IPC Activity", NONE },
			{ "message ops/sec", LONG },
			{ "semaphore ops/sec", LONG },
			{ "av:", LONG },
			{ "av:", LONG },
			{0}
};

lval	avipclv[] = { 0, mess, sema, ave_mess, ave_sema };

position	avipos[] = {
			{ 13, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
			{ 27, 1, 32, 1 },
			{ 27, 2, 32, 2 },
};

label	avpage[] = 	{
			{ "Paging Activity", NONE },
			{ "addr trans flt/sec", LONG },
			{ "% demand fill", FLOAT },
			{ "% pages on swap", FLOAT },
			{ "% pages in cache", FLOAT },
			{ "% pages on file", FLOAT },
			{ "prot err flt/sec", LONG },
			{ "% copy on write", FLOAT },
			{ "% steal the page", FLOAT },
			{ "page in flt/sec", LONG },
			{ "reclaim flt/sec", LONG },
			{ "av:", LONG },
			{ "av:", FLOAT },
			{ "av:", FLOAT },
			{ "av:", FLOAT },
			{ "av:", FLOAT },
			{ "av:", LONG },
			{ "av:", FLOAT },
			{ "av:", FLOAT },
			{ "av:", LONG },
			{ "av:", LONG },
			{0}
};

lval	avpagelv[] = { 0, addr, 0, 0, 0, 0, prot, 0, 0, pagein, reclaim,
		     ave_addr, 0, 0, 0, 0, ave_prot, 0, 0, ave_pagin, 
		     ave_reclaim };
fval	avpagefv[] = { 0, 0,
		     pct_demand, pct_pgsonswap, pct_pgsincache, pct_pgsonfile, 
		     0, pct_cow, pct_steal, 0, 0,
		     0, ave_pct_demand, ave_pct_pgsonswap, ave_pct_pgsincache, 
		     ave_pct_pgsonfile, 0, ave_pct_cow, ave_pct_steal, 0, 0 };

position	avppos[] = {
			{ 1, 0, -1, -1 },
			{ 0, 1, 25, 1 },
			{ 0, 2, 25, 2 },
			{ 0, 3, 25, 3 },
			{ 0, 4, 25, 4 },
			{ 0, 5, 25, 5 },
			{ 0, 6, 25, 6 },
			{ 0, 7, 25, 7 },
			{ 0, 8, 25, 8 },
			{ 0, 9, 25, 9 },
			{ 0, 10, 25, 10 },
			{ 33, 1, 39, 1 },
			{ 33, 2, 39, 2 },
			{ 33, 3, 39, 3 },
			{ 33, 4, 39, 4 },
			{ 33, 5, 39, 5 },
			{ 33, 6, 39, 6 },
			{ 33, 7, 39, 7 },
			{ 33, 8, 39, 8 },
			{ 33, 9, 39, 9 },
			{ 33, 10, 39, 10 },
};

label	avmem[] = 	{
			{ "Memory Activity", NONE },
			{ "# free mem KBytes", DOUBLE },
			{ "# free swap KBytes", LONG },
			{ "#av:", DOUBLE },
			{ "#av:", LONG },
			{0}
};

dval	avmemdv[] = { 0, memfree, 0, ave_memfree, 0 };
lval	avmemlv[] = { 0, 0, swapfree, 0, ave_swapfree };

position	avmpos[] = {
			{ 10, 0, -1, -1 },
			{ 1, 1, 19, 1 },
			{ 1, 2, 19, 2 },
			{ 27, 1, 32, 1 },
			{ 27, 2, 32, 2 },
};

/* all the labels */
label	*avdisplay[] = {
		avcpu,
		avbuf,
		avdev,
		avtty,
		avsys,
		avswap,
		avfileact,
		avque,
		avtbl,
		avipc,
		avpage,
		avmem
};

/* the floating point functions */
fval	*avdispfval[] = {
	avcpufv,
	avbuffv,
	avdevfv,
	avttyfv,
	0,
	0,
	0,
	avquefv,
	0,
	0,
	avpagefv,
	0
};

/* the long functions */
lval	*avdisplval[] = {
	0,
	avbuflv,
	0,
	avttylv,
	avsyslv,
	avswaplv,
	avfilelv,
	avquelv,
	avtbllv,
	avipclv,
	avpagelv,
	avmemlv
};

/* the char functions */
cval	*avdispcval[] = {
	0,
	0,
	avdevcv,
	0,
	0,
	0,
	0,
	0,
	avtblcv,
	0,
	0,
	0
};

/* the double functions */
dval	*avdispdval[] = {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	avmemdv
};

/* all the positions */
position	*avpos[] = {
		avcpos,
		avbpos,
		avdpos,
		avypos,
		avspos,
		avwpos,
		avfpos,
		avqpos,
		avtpos,
		avipos,
		avppos,
		avmpos
};

/* Row offset for with which to modify "unit" positions */
int	avy_offset[] = { 2, 13, 26, 37, 50, 61, 74, 85, 98, 109, 122, 133, 146, 157, 170, 181, 194, 205, 218, 229, 242, 253 };

/* Col offset for with which to modify "unit" positions */
#define AVXSETS	2
int	avx_offset[] = { 0, 40 };
