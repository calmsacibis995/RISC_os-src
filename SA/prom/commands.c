#ident "$Header: commands.c,v 1.47.1.5 91/01/02 18:56:41 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* COPYRIGHT_KEYWORD_GOES_HERE
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
 * commands.c -- prom commands
 */

#include "sys/param.h"
#include "sys/file.h"
#include "saio/saio.h"			/* demon.h redefines CBUFSIZE */
#include "machine/demon.h"		/* <-- includes cpu.h */
#include "machine/cpu_board.h"
#include "machine/dvh.h"
#include "machine/i8254clock.h"
#include "machine/hd146818.h"
#include "machine/mk48t02.h"
#include "saio/protocol.h"
#include "saio/setjmp.h"
#include "saio/stringlist.h"
#include "saio/saioctl.h"
#include "saio/parser.h"
#include "prom/prom.h"
#include "prom/entrypt.h"
#include "saio/ctype.h"

extern struct string_list environ_str;
unsigned client_pc;	/* initial client pc, set on load or promexec */

extern int TIM0_ACK_ADDR[];
extern int RT_CLOCK_ADDR[];
extern int TODC_CLOCK_ADDR[];
extern char version[];
extern int calcchk();

/*
 * boot -- boot new image
 */
boot(argc, argv)
int argc;
char **argv;
{
	struct string_list newargv;
	struct string_list bootargv;
	struct promexec_args pa;
	char *bootfile;
	int flags, i;
	extern char *getenv();

#ifdef PROM
	/*
	 * For m/120 make sure keyswitch is in position to boot
	 */
	if (IS_R2400 && (*getenv("bootmode") != 'd') &&
		(!(*(ushort *)PHYS_TO_K1(SCR) & SCR_NOBOOTLOCK))) {

		printf("* Key is in LOCKED position - boot aborted! *\n");
		return(0);
	}
#endif
	/*
	 * initialize defaults
	 */
	bootfile = getenv("bootfile");
	flags = 0;

	init_str(&newargv);
	if (new_str1("", &newargv))	/* leave space for bootfile as arg0 */
		return(0);

    	if (argc == 1) {
		if (bootfile) {
			argc = _argvize(bootfile, &bootargv);
			argv = bootargv.strptrs;
			bootfile = *argv;
		}
	}

	while (--argc > 0) {
		argv++;

		if ((*argv)[0] == '-') {
			switch ((*argv)[1]) {

			case 'f':
				if (--argc <= 0)
					return(1);
				bootfile = *++argv;
				break;

			case 'n':
				flags |= EXEC_NOGO;
				break;

			case '-':
				(*argv)++;	/* skip initial - */
				goto cmd_arg;

			default:
				printf("unknown boot option: %s\n", *argv);
				printf("precede option with extra '-' to\n");
				printf("pass to booted program\n");
				return(1);
			}
		} else {
cmd_arg:
			if (new_str1(*argv, &newargv))
				return(0);
		}
	} 

	if (bootfile == NULL) {
		eprintf("$bootfile not set\n");
		return(0);
	} else if (streq(bootfile, "NONE")) {
		/*
		 * No 2nd level boot, so delete arg 0
		 */
		delete_strnum(0, &newargv);
	} else if (set_str(bootfile, 0, &newargv))
		return(0);

	pa.pa_bootfile = newargv.strptrs[0];
	pa.pa_argc = newargv.strcnt;
	pa.pa_argv = newargv.strptrs;
	pa.pa_environ = environ_str.strptrs;
	pa.pa_flags = flags;

	set_boot_dev(pa.pa_bootfile);
	if (promexec(&pa) == -1)
		printf("couldn't load %s\n", pa.pa_bootfile);

	return(0);
}

/*
 * autoboot -- jumped to early in initialization to perform autoboot seq
 */
autoboot()
{
	struct string_list newargv;
	struct string_list bootargv;
	struct promexec_args pa;
	char *bootfile;
	extern char *getenv();
	extern int ignore_xoff;
	int		i;
	int argc;
	char **argv;

#ifdef PROM
	/*
	 * For m/120 make sure keyswitch is in position to boot
	 */
	if (IS_R2400 && (*getenv("bootmode") != 'd') &&
		(!(*(ushort *)PHYS_TO_K1(SCR) & SCR_NOBOOTLOCK))) {

		printf("* Key is in LOCKED position - auto boot aborted! *\n");
		exit(1);
	}
#endif
	bootfile = getenv("bootfile");
	if (bootfile == NULL || streq(bootfile, "NONE")) {
		printf("$bootfile not set\n");
		exit(1);	/* forces entry into command mode */
	}

	argc = _argvize(bootfile, &bootargv);
	argv = bootargv.strptrs;
	bootfile = *argv;

	init_str(&newargv);
	if (new_str1(bootfile, &newargv))	/* arg0 is boot device */
		exit(1);
	if (new_str1("-a", &newargv))	/* tell boot this is an autoboot */
		exit(1);

	while (--argc > 0) {
		argv++;
		if (new_str1(*argv, &newargv))
			exit(1);
	}

	pa.pa_bootfile = bootfile;
	pa.pa_argc = newargv.strcnt;
	pa.pa_argv = newargv.strptrs;
	pa.pa_environ = environ_str.strptrs;
	pa.pa_flags = 0;

	ignore_xoff = 1;
#ifdef R3030
	printf("\nAutoboot: Waiting to load %s (CTRL-C to abort)",
	    bootfile);
#else
	printf("\nAutoboot: Waiting to load %s (CTRL-C to abort, RETURN to expedite)",
	    bootfile);
#endif
	ioctl(0,FIOCNBLOCK,1);
	for (i=0;i<AUTOBOOT_DELAY;i++) {
		char	c;
		if (((c=getchar())=='\n') || (c=='\r')) break;
		delay(1);
	}
	ioctl(0,FIOCNBLOCK,0);
	printf("\nloading\n");
	set_boot_dev(pa.pa_bootfile);
	promexec(&pa);

	return(0);
}

#ifdef SASH
/*
 * kernel_name -- construct a rational path name for the kernel
 * given the file that sash was loaded from
 */
char *
kernel_name(oboot)
char *oboot;
{
	register char *cp;
	register char *bp;
	register int commas;
	register char c;
	static char bootfile[64];
	struct io_arg io_arg;
	char *bootend;
	extern char *index();
	int fd, arg;
	struct volume_header vh;
	char endbuf[BFNAMESIZE+9];
	static char *btoa();

	cp = index(oboot, ')');
	if (!cp) {
		printf("bad sash filename format, can't load kernel\n");
		return;
	}
	/*
	 * boot kernel from same device sash was booted from.
	 * find correct partition and kernel name from volume header
	 */
	bp = bootfile;
	cp = oboot;
	commas = 0;
	while ((c = *cp) && commas < 2 && c != ')') {
		if (c == ',')
			commas++;
		*bp++ = *cp++;
	}
	while(commas++ < 2)
		*bp++ = ',';
	/*
	 * Someday we might also want to let the kernel be loaded
	 * via $path.
	 */
	bootend = "";
	fd = open(oboot, O_RDONLY);
        if (IS_COMMON_SCSI && Is_Internal_SCSI(oboot)) {
            io_arg.memaddr = (unsigned long)&vh;
            io_arg.datasz = (unsigned long)sizeof(vh);
            arg = (int)&io_arg;
        } else {
            arg = (int)&vh;
        }
	if ((fd > 0) && (ioctl(fd, DIOCGETVH, arg) >= 0)
	    && (vh.vh_rootpt >= 0) && (vh.vh_rootpt < NPARTAB)
	    && vh.vh_bootfile[0]) {
		bzero(endbuf, sizeof(endbuf));
		strcpy(endbuf, btoa(vh.vh_rootpt));
		strcat(endbuf, ")");
		strncat(endbuf, vh.vh_bootfile, BFNAMESIZE);
		bootend = endbuf;
	}
	strcpy(bp, bootend);
	return(bootfile);
}

static char *
btoa(val)
register int val;
{
	static char buf[3];
	register char *cp;

	if (val < 0 || val > 99)
		return("1");

	cp = buf;
	if (val >= 10) {
		*cp++ = '0' + (val / 10);
		val %= 10;
	}
	*cp++ = '0' + val;
	*cp = 0;
	return(buf);
}
	
/*
 * boot2 -- perform a second level boot
 */
boot2(bootfile, argp)
char *bootfile;
char **argp;
{
	struct string_list newargv;
	struct promexec_args pa;
	extern int ignore_xoff;

	init_str(&newargv);
	if (new_str1(bootfile, &newargv))
		return;
	while (*argp)
		if (new_str1(*argp++, &newargv))
			return;

	pa.pa_bootfile = bootfile;
	pa.pa_argc = newargv.strcnt;
	pa.pa_argv = newargv.strptrs;
	pa.pa_environ = environ_str.strptrs;
	pa.pa_flags = 0;

	ignore_xoff = 1;
	printf("\nLoading %s\n", bootfile);
	set_boot_dev(pa.pa_bootfile);
	promexec(&pa);
	printf("Can't load %s\n", bootfile);
}
#endif

/*
 * warm_cmd -- command interface to restart routine
 */
warm_cmd()
{
	struct restart_blk *rb = (struct restart_blk *)RESTART_ADDR;

	rb->rb_occurred = 0;
	if (warm_start())
		eprintf("warm start failed\n");
	return(0);
}

/*
 * warm_start -- check if restart block is properly initialized, if so,
 * transfer to restart routine
 */
warm_start()
{
	register struct restart_blk *rb = (struct restart_blk *)RESTART_ADDR;
	register int *ip, *ep;
	register sum;
	register i;
	char *tmpargv[2];
	extern char **environ;
	extern int ignore_xoff;

	if (rb->rb_occurred)
		return(1);
	rb->rb_occurred = 1;
	if (rb->rb_magic != RESTART_MAGIC)
		return(1);

	ip = (int *)rb->rb_restart;
	ep = ip + RESTART_CSUMCNT;
	sum = 0;
	for (; ip < ep; ip++)
		sum += *ip;

	if (sum != rb->rb_checksum)
		return(1);
	ignore_xoff = 1;
	printf("Warm start: Waiting to go at 0x%x (CTRL-C to abort) ... ",
	    rb->rb_restart);
	delay(AUTOBOOT_DELAY);
	printf("restarting\n");
	tmpargv[0] = "warm_start";
	tmpargv[1] = NULL;
	client_start(1, tmpargv, environ, rb->rb_restart, 0);
	exit(1);	/* shouldn't return, but just in case */
}

/*
 * This bug is currently in the 1.11 version of the compilers.  Basically
 * what happens is that the optimizer doesn't see a possible exit from
 * this routine and gets confused.  What it then does is to dump swc1
 * instructions into the code by mistake.
 */

#define OPTIMIZER_BUG
#ifdef OPTIMIZER_BUG
int opt_bug_memory;
#endif

/*
 * delay -- pause to allow console interrupt
 */
delay(secs)
{
	jmp_buf delay_buf;
	extern int *_timer_jmpbuf;
#ifdef OPTIMIZER_BUG
	register volatile int *opt_bug = &opt_bug_memory;
#endif

	if (setjmp(delay_buf))
		return;

	_timer_jmpbuf = delay_buf;
	_set_timer(secs);

#ifdef OPTIMIZER_BUG
	while (*opt_bug == 0)
	  _scandevs(0);
#else
	while(1)
		_scandevs(0);
#endif
}

/*
 * cat -- cat files
 */
cat(argc, argv)
int argc;
char **argv;
{
	register char *bp;
	int cc;
	int fd;
	char buf[DEV_BSIZE];

	if (argc < 2)
		return(1);

	while (--argc > 0) {
		argv++;

		fd = open(*argv, O_RDONLY);
		if (fd < 0) {
			printf("can't open %s\n", *argv);
			continue;
		}

		while ((cc = read(fd, buf, sizeof(buf))) > 0)
			for (bp = buf; bp < &buf[cc]; bp++)
				putchar(*bp);
		if (cc < 0)
			printf("*** error termination on read\n");
		close(fd);
	}
	return(0);
}

#ifdef SASH

#define	MAXBLOCKS	40	/* 20kB blocks max */
/*
char copy_buf[MAXBLOCKS * DEV_BSIZE];
 */
static char* copy_buf;

copy(argc, argv)
int argc;
char **argv;
{
	int src_fd, dst_fd;
	int bcnt, dbcnt;
	int totcnt, discnt;
	unsigned blocksize = DEV_BSIZE;
	unsigned bytes = ((unsigned)(-1)) >> 1;	/* max # without sign set */
	extern char *atob();

	argc--; argv++;

	for (; argc > 0 && **argv == '-'; argc--, argv++) {
		switch ((*argv)[1]) {
		case 'b':	/* set blocksize */
			if (--argc <= 0)
				return(1);
			switch (*atob(*++argv, &blocksize)) {
			case NULL:
				break;
			case 'b': case 'B':
				blocksize *= DEV_BSIZE;
				break;
			case 'k': case 'K':
				blocksize *= 1024;
				break;
			default:
				return(1);
			}
#ifdef fixthis
			if (blocksize > sizeof(copy_buf)) {
				printf("blocksize %d too large\n", blocksize);
				return(0);
			}
#endif fixthis
			break;

		case 'c':
			if (--argc <= 0)
				return(1);
			if (*atob(*++argv, &bytes)) {
				printf("bad count: %s\n", *argv);
				return(0);
			}
			break;

		default:
			return(1);
		}
	}

	if (argc != 2)
		return(1);

	src_fd = open(*argv, O_RDONLY);
	if (src_fd < 0) {
		printf("couldn't open %s\n", *argv);
		return(0);
	}

	dst_fd = open(*++argv, O_WRONLY);
	if (dst_fd < 0) {
		printf("couldn't open %s\n", *argv);
		return(0);
	}

	totcnt = 0;
	discnt = 0;
	if (!copy_buf) { /* just once*/
		copy_buf = (char*)align_malloc(MAXBLOCKS * DEV_BSIZE,64);
	}
	while (bytes) {
		dbcnt = _min(bytes, blocksize);
		if ((bcnt = read(src_fd, copy_buf, dbcnt)) <= 0)
			break;
		if (write(dst_fd, copy_buf, bcnt) != bcnt) {
			printf("write error, offset=0x%x\n",
			    lseek(dst_fd, 0, 1));
			break;
		}
		printf (".");
		if ((discnt % 64) == 0 && discnt > 0)
			printf ("\n");
		discnt++;
		bytes -= bcnt;
		totcnt += bcnt;
	}

	if (bcnt < 0)
		printf(" read error, offset=0x%x\n", lseek(src_fd, 0, 1));
	printf("\n%d (0x%x) bytes copied\n", totcnt, totcnt);
	close (src_fd);
	close (dst_fd);

	return(0);
}
#endif SASH


/*
 * get -- read memory
 */
get(argc, argv)
int argc;
char **argv;
{
	int width = SW_WORD;
	unsigned val;
	unsigned address;
	extern char *atob();

	argv++; argc--;
	if (argc == 2 && **argv == '-') {
		switch ((*argv)[1]) {
		case 'b':
			width = SW_BYTE;
			break;

		case 'h':
			width = SW_HALFWORD;
			break;

		case 'w':
			width = SW_WORD;
			break;

		default:
			return(1);
		}
		argv++; argc--;
	}

	if (argc != 1)
		return(1);

	if (*atob(*argv, &address))
		return(1);

	val = get_memory(address, width);
	printf("0x%x:\t%d\t0x%x\t'", address, val, val);
	showchar(val);
	printf("'\n");
	return(0);
}

/*
 * put -- write memory
 */
put(argc, argv)
int argc;
char **argv;
{
	int width = SW_WORD;
	unsigned address;
	unsigned val;
	extern char *atob();

	argv++; argc--;
	if (argc == 3 && **argv == '-') {
		switch ((*argv)[1]) {
		case 'b':
			width = SW_BYTE;
			break;

		case 'h':
			width = SW_HALFWORD;
			break;

		case 'w':
			width = SW_WORD;
			break;

		default:
			return(1);
		}
		argv++; argc--;
	}

	if (argc != 2)
		return(1);

	if (*atob(argv[1], &val))
		return(1);

	if (*atob(*argv, &address))
		return(1);

	if (range_check(address, width))
		printf("overwrite permitted\n");
	set_memory(address, width, val);
	return(0);
}

#ifdef PROM
char *index();
/*
 * environment initialization table
 */

struct env_table {
	char *et_name;
	char *et_value;
	int et_nvaddr;
	int et_nvlen;
	};

static struct env_table env_table[] = {
	{ "netaddr",	"97.0.255.255",	NVADDR_NETADDR,NVLEN_NETADDR },
	{ "lbaud",	"9600", 	NVADDR_LBAUD,	NVLEN_LBAUD },
	{ "rbaud",	"9600", 	NVADDR_RBAUD,	NVLEN_RBAUD },
	{ "bootfile",	DEFAULT_BOOTFILE,NVADDR_BOOTFILE,NVLEN_BOOTFILE },
	{ "bootmode",	"m",		NVADDR_BOOTMODE,NVLEN_BOOTMODE },
	{ "console",	"l",		NVADDR_CONSOLE,	NVLEN_CONSOLE },
	{ "cpuid",	"0",		NVADDR_CPUID,	NVLEN_CPUID },
	{ "resetepc",	0,		NVADDR_RESETEPC,NVLEN_RESETEPC },
	{ "resetra",	0,		NVADDR_RESETRA,NVLEN_RESETRA },
	/*
	 * Not used, didn't want to redo offsets so this can be re-used 
	 *
	{ "memparity",	"1",		NVADDR_MEMPARITY,NVLEN_MEMPARITY },
	*/
	{ "version",	version,	0,		0 },
	{ 0,		0,		0,		0 }
};

/*
** note: only real new nvram fields can be put here!
*/
static struct env_table new_nvram_env_table[] = {
	{ "magic",	"RISCPROM",	NVADDR_MAGIC,NVLEN_MAGIC },
	{ "vendor",	"",		NVADDR_VENDOR,NVLEN_VENDOR },
	{ "model",	"",		NVADDR_MODEL,NVLEN_MODEL },
	{ "rootname",	"0",     	NVADDR_ROOT,NVLEN_ROOT },
#ifdef RC6280	
	{ "ioa_param",	"0",		NVADDR_IOAPARM,NVLEN_IOAPARM },
#endif RC6280	
#ifdef R3030
	{ "use_bootparams",	"0x0",	NVADDR_UBOOTPARAMS, NVLEN_UBOOTPARAMS },
	{ "keyswtch",	"0x1",		NVADDR_KEYSWTCH, NVLEN_KEYSWTCH },
	{ "keyboard",	"MIPS",		NVADDR_KEYBOARD, NVLEN_KEYBOARD },
	{ "scsi_id",	"7",		NVADDR_SCSI_ID, NVLEN_SCSI_ID },
	{ "scsi_reset",	"1",		NVADDR_SCSI_RESET, NVLEN_SCSI_RESET },
	{ "bus_test",	"1",		NVADDR_BUS_TEST, NVLEN_BUS_TEST },
	{ "language",	"american",	NVADDR_LANGUAGE, NVLEN_LANGUAGE },
#endif
	{ 0,		0,		0,		0 }
};

#endif

#ifdef PROM

struct baud {
	long	code;
	char	*str;
	};

#define	MAXBAUDCODE	11
#define	TRUE		1
#define	FALSE		0
#define	DO_INIT_NVRAM	1
#define	NO_INIT_NVRAM	0


/*
** build_env_from_default
*/
build_env_from_default
(
struct env_table *et,
int    option
)
{
register char *cp;
extern char *get_nvram();

for (; et->et_name; et++)
    {
    if (strcmp(et->et_name, "resetepc") == 0 ||
	strcmp(et->et_name, "resetra") == 0)
	{
    	if (*(cp = get_nvram(et->et_nvaddr, et->et_nvlen)))
	    {
	    /* this is a special case for "resetepc" and " resetra" */
	    if (new_str2(et->et_name, cp, &environ_str))
	        return;
	    }
	}
    else
	{
    	if (new_str2(et->et_name, et->et_value, &environ_str))
    	    return;
    	if (et->et_nvlen && (option == DO_INIT_NVRAM))
    	    set_nvram(et->et_nvaddr, et->et_nvlen, et->et_value);
	}
    }
}

/*
** build_env_from_nvram
*/
build_env_from_nvram
(
struct env_table *et
)
{
extern char *get_nvram();
register char *cp;

for (; et->et_name; et++)
    {
    if (et->et_nvlen && *(cp = get_nvram(et->et_nvaddr, et->et_nvlen)))
    	{
    	/* build environment vars from NVRAM */
    	if (new_str2(et->et_name, cp, &environ_str))
	    return;
    	}
    else
    	{
    	/* build environment vars from NON-NVRAM */
    	if (new_str2(et->et_name, et->et_value, &environ_str))
	    return;
    	}
    }
}

/*
** This routine is used to check if the old_nvram exists or not.
** if old_nvram exists then return 1 else return 0
*/
old_nvram_exist ()
{
register int i,chksum;
char *cp;
char *get_nvram();
register unsigned char tmpchksum;
register volatile struct todc_clock *todc = 
  (struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);

chksum = 0;
cp = get_nvram(NVADDR_NETADDR,NVLEN_NETADDR);
for( i = 0; i < NVLEN_NETADDR; i++)
    chksum = (chksum + *(cp+i)) + 0xff;

cp = get_nvram(NVADDR_LBAUD,NVLEN_LBAUD);
for( i = 0; i < NVLEN_LBAUD; i++)
    chksum = (chksum + *(cp+i)) + 0xff;

cp = get_nvram(NVADDR_RBAUD,NVLEN_RBAUD);
for( i = 0; i < NVLEN_RBAUD; i++)
    chksum = (chksum + *(cp+i)) + 0xff;

cp = get_nvram(NVADDR_BOOTFILE,NVLEN_BOOTFILE);
for( i = 0; i < NVLEN_BOOTFILE; i++)
    chksum = (chksum + *(cp+i)) + 0xff;

cp = get_nvram(NVADDR_CONSOLE,NVLEN_CONSOLE);
for( i = 0; i < NVLEN_CONSOLE; i++)
    chksum = (chksum + *(cp+i)) + 0xff;

chksum = chksum & 0xff; 
tmpchksum = todc->todc_mem[NVADDR_CHKSUM].value;
if (chksum == tmpchksum)
    return (TRUE);
else
    return (FALSE);
}

#endif


/*
 * init_env -- initialize environment variables
 */
init_env()
{
#ifdef PROM
	register struct env_table *et;
	register char *cp;
	register nv_valid;
	extern char *get_nvram();
	register unsigned char tmpchk,chksum;
	register int i;
	register volatile struct rt_clock *rt = 
	  (struct rt_clock *)MACHDEP(RT_CLOCK_ADDR);
	register volatile struct todc_clock *todc = 
	  (struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
	register int flag;

#endif
#ifdef SASH
	register char **ep;
#endif
	extern char **environ;

	init_str(&environ_str);
#ifdef PROM
	nv_valid = is_nvvalid();
	if (! nv_valid)
		_errputs("nvram not valid\n");

	/*
	 * Check for a valid checksum 
	 */
	tmpchk = (char)calcchk();

	if(rt)
	    chksum = rt->rt_mem[RT_MEMX(NVADDR_CHKSUM)];
	else if (todc) {
	    chksum = todc->todc_mem[NVADDR_CHKSUM].value;
	}
	if( tmpchk != chksum){
	    _errputs("nvram: invalid checksum\n");
	    nv_valid = 0;
	}


	if(rt)
	    {
	    if (nv_valid)
		build_env_from_nvram(env_table);
	    else
		build_env_from_default(env_table, DO_INIT_NVRAM);
	    }
	else if (todc)
	    {
		register int 	magic_exist;

    		cp = get_nvram(NVADDR_MAGIC, NVLEN_MAGIC);
		if (strcmp (cp, "RISCPROM") == 0)
		    magic_exist = TRUE;
		else
		    magic_exist = FALSE;
		
	    if (nv_valid)
		{
		build_env_from_nvram(env_table);
		build_env_from_nvram(new_nvram_env_table);
		}
	    else
		{
		if (old_nvram_exist())
		    {
		    build_env_from_nvram(env_table);
		    build_env_from_default(new_nvram_env_table, DO_INIT_NVRAM);
		    }
	    	else
		    {
		    build_env_from_default(env_table, DO_INIT_NVRAM);
		    build_env_from_default(new_nvram_env_table, DO_INIT_NVRAM);
		    }
		}
	    if (!nv_valid || !magic_exist)
	        {
	    	replace_str("magic", "RISCPROM", &environ_str);
    		set_nvram(NVADDR_MAGIC, NVLEN_MAGIC, "RISCPROM");
	    	replace_str("vendor", "MIPS", &environ_str);
    		set_nvram(NVADDR_VENDOR, NVLEN_VENDOR, "MIPS");
		cp = "";
		switch (machine_type)
		    {
		    case BRDTYPE_R2400:
			cp = "M/120";
			break;
		    case BRDTYPE_M180:
			cp = "RC3240";
			break;
		    case BRDTYPE_R3200:
			cp = "M/2000";
			break;
		    case BRDTYPE_RB3125:
			cp = (char *)PHYS_TO_K1(IDPROM_REV);
			if ((*cp & 0xF0) == REV_RB3133)
			    cp = "m2000-33";
			else
			    cp = "m2000-25";
			break;
		    case BRDTYPE_R6300:
			cp = "RC6280";
			break;
		    case BRDTYPE_R3030:
    			cp = (char *)PHYS_TO_K1(IDPROM_R3030+ID_REV_OFF_R3030);
			if (*cp & 0x20) 
			    cp = "Rx3330";
			else	
			    cp = "Rx3230";
			break;
		    }
	    	    replace_str("model", cp, &environ_str);
    		    set_nvram(NVADDR_MODEL,NVLEN_MODEL, cp);
	    	}
	    }

	set_nvvalid();
#endif
#ifdef SASH
	for (ep = environ; ep && *ep; ep++)
		new_str1(*ep, &environ_str);
#endif
	environ = environ_str.strptrs;
}


/*
 * setenv -- set prom environment variables
 */
setenv(argc, argv)
int argc;
char **argv;
{
#ifdef PROM
	register struct env_table *et;
#endif PROM
	int fd;
	extern int Debug;
	extern int Verbose;
	extern int _udpcksum;

	if (argc != 3)
		return(1);
 	_setenv(argv[1], argv[2]);
	return(0);
}

/*
 * _setenv -- set prom environment variables
 * C callable entry
 */
_setenv(s1, s2)
char *s1, *s2;
{
#ifdef PROM
	register struct env_table *et;
#endif
	int fd;
	extern int Debug;
	extern int Verbose;
	extern int _udpcksum;

	if (streq(s1, "version"))	/* Don't allow changes to "version" */
		return(0);
	replace_str(s1, s2, &environ_str);
	if (streq(s1, "DEBUG"))
		atob(s2, &Debug);
	if (streq(s1, "VERBOSE"))
		atob(s2, &Verbose);
	if (streq(s1, "UDPCKSUM"))
		atob(s2, &_udpcksum);
	if (streq(s1, "lbaud")) {
		if ((fd = open("tty(0)", O_RDWR)) >= 0) {
			ioctl(fd, TIOCREOPEN, 0);
			close(fd);
		}
	}
	if (streq(s1, "rbaud")) {
		if ((fd = open("tty(1)", O_RDWR)) >= 0) {
			ioctl(fd, TIOCREOPEN, 0);
			close(fd);
		}
	}
#ifdef PROM
	/*
	 * search env_table to see if this is a non-volatile parameter,
	 * if so, update nvram
	 */
	for (et = env_table; et->et_name; et++)
		if (streq(et->et_name, s1)) {
			if (et->et_nvlen)
				set_nvram(et->et_nvaddr, et->et_nvlen, s2);
			break;
		}
	/*
	 * search new_nvram_env_table to see if this is a non-volatile
	 * parameter, if so, update new nvram area
	 */
	for (et = new_nvram_env_table; et->et_name; et++)
		if (streq(et->et_name, s1)) {
			if (et->et_nvlen)
				set_nvram(et->et_nvaddr, et->et_nvlen, s2);
			break;
		}
#endif
	return(0);
}

/*
 * unsetenv -- unset prom environment variables
 */
unsetenv(argc, argv)
int argc;
char **argv;
{
	extern int Debug;
	extern int Verbose;
	extern int _udpcksum;

	if (argc != 2)
		return(1);
	
	if (streq(argv[1], "version"))/* Don't allow changes to "version" */
		return(0);
	delete_str(argv[1], &environ_str);
	if (streq(argv[1], "DEBUG"))
		Debug = 0;
	if (streq(argv[1], "VERBOSE"))
		Verbose = 0;
	if (streq(argv[1], "UDPCKSUM"))
		_udpcksum = 0;
	return(0);
}

/*
 * printenv -- show prom environment variables
 */
printenv(argc, argv)
int argc;
char **argv;
{
	char *cp;
	int i;
	extern char *find_str();

	if (argc == 1) {
		for (i = 0; i < environ_str.strcnt; i++)
			printf("%s\n", environ_str.strptrs[i]);
	} else while (--argc > 0) {
		cp = find_str(*++argv, &environ_str);
		if (cp)
			printf("%s\n", cp);
	}

	return(0);
}


/*
 * init_restartblk -- initialize warm start variables
 */
init_restartblk()
{
	register struct restart_blk *rb;
	extern char _fbss[], end[];
	extern breakpoint();

	rb = (struct restart_blk *)RESTART_ADDR;
#ifdef PROM
	bzero(rb, sizeof(struct restart_blk));
#endif
	rb->rb_fbss = _fbss;
	rb->rb_ebss = (char *)PROM_STACK;
#ifdef PROM
	rb->rb_bpaddr = breakpoint;
#endif
}

/*
 * clear_restart -- inhibit further warm starts
 */
clear_restart()
{
	((struct restart_blk *)RESTART_ADDR)->rb_occurred = 1;
}

#define	NADDR		10
#define	READ_SPIN	0
#define	WRITE_SPIN	1

/*
 * spin -- repeat memory reference pattern
 */
spin(argc, argv)
int argc;
char **argv;
{
	struct addr_table {
		unsigned at_addr;
		unsigned at_width;
		unsigned at_rw;
		unsigned at_value;
		int at_cnt;
	} addr_table[NADDR];
	register struct addr_table *at = addr_table;
	register i;
	unsigned value = 0;
	int iteration_count = 1;
	int cnt = 1;
	char c;
	extern char *atob();

	if (argc < 2)
		return(1);

	while (--argc > 0) {
		argv++;

		switch (c = (*argv)[1]) {

		case 'i':
			if (--argc < 1)
				return(1);
			if (*atob(*++argv, &iteration_count))
				return(1);
			break;

		case 'c':
			if (--argc < 1)
				return(1);
			if (*atob(*++argv, &cnt))
				return(1);
			break;

		case 'v':
			if (--argc < 1)
				return(1);
			if (*atob(*++argv, &value))
				return(1);
			break;

		case 'w':
		case 'r':
			if (at >= &addr_table[NADDR-1]) {
				eprintf("address table filled\n");
				return(0);
			}
			if (--argc < 1)
				return(1);
			at->at_rw = (c == 'r') ? READ_SPIN : WRITE_SPIN;
			at->at_width = ((*argv)[2] == 'b')
			    ? SW_BYTE
			    : ((*argv)[2] == 'h') ? SW_HALFWORD : SW_WORD;
			if (*atob(*++argv, &at->at_addr))
				return(1);
			at->at_value = value;
			at->at_cnt = cnt;
			at++;
			break;

		default:
			return(1);
		}
	}
	at->at_width = 0;

	while (iteration_count) {
	    for (at = addr_table; at->at_width; at++) {
		for (i = 1; i <= at->at_cnt; i++) {
		    static int junk;

		    switch (at->at_rw) {
		    case READ_SPIN:
			    switch (at->at_width) {
				case sizeof(char):
					junk = *(volatile char *)at->at_addr;
					break;
				case sizeof(short):
					junk = *(volatile short *)at->at_addr;
					break;
				case sizeof(int):
					junk = *(volatile int *)at->at_addr;
					break;
			    }
			    break;

		    case WRITE_SPIN:
			    switch (at->at_width) {
				case sizeof(char):
					*(char *)at->at_addr = at->at_value;
					break;
				case sizeof(short):
					*(short *)at->at_addr = at->at_value;
					break;
				case sizeof(int):
					*(int *)at->at_addr = at->at_value;
					break;
			    }
			    break;
		    } /* read or write? */
		} /* for each iteration of the address list element */
	    } /* for each address list element */

	/* check for keyboard interrupt every 16 iterations */
	if (!(iteration_count & 0xf)) _scandevs();   
	iteration_count--;
	} /* while */

	return(0);
}


/*
 * fill -- fill block of memory with pattern
 */
fill(argc, argv)
int argc;
char **argv;
{
	int size = SW_WORD;
	int range_type;
	unsigned base;
	int value = 0;
	int cnt;
	extern char *atob();

	if (argc < 2)
		return(1);

	while (--argc > 0) {
		if (**++argv == '-') {
			switch ((*argv)[1]) {
			case 'b':
				size = SW_BYTE;
				break;

			case 'h':
				size = SW_HALFWORD;
				break;

			case 'w':
				size= SW_WORD;
				break;

			case 'v':
				if (--argc < 1)
					return(1);
				if (*atob(*++argv, &value))
					return(1);
				break;
			default:
				return(1);
			}
		} else {
			range_type = parse_range(*argv, &base, &cnt);
			if (range_type == ERROR_RANGE)
				return(1);
		}
	}

	if (range_type == ADDR_RANGE)
  	{
	    if (base > cnt)
	    {
		eprintf("illegal address range\n");
		return(0);
	    }
	    else
		cnt = ((unsigned)cnt - base + 1) / size;
	}
	if (cnt == 0)
		cnt = 1;

	switch (size) {
	case SW_BYTE:
		while (cnt-- > 0) {
			*(char *)base = value;
			base += size;
		}
		break;

	case SW_HALFWORD:
		while (cnt-- > 0) {
			*(short *)base = value;
			base += size;
		}
		break;

	case SW_WORD:
		while (cnt-- > 0) {
			*(int *)base = value;
			base += size;
		}
		break;

	default:
		prom_error("fill");
	}
	return(0);
}

#define	HEX	16
#define	DEC	-10		/* signed decimal */
#define	UNS	10		/* unsigned decimal */
#define	OCT	8
#define	BIN	2
#define	CHR	1

#define	NO_FILL	0
#define	FILL	1

static char *pad = "   ";	/* can't use pad[] due to xstr */

/*
 * dump -- dump contents of block of memory
 */
dump(argc, argv)
int argc;
char **argv;
{
	register int i, same_printed, different, range_type, val, base;
	register unsigned width, baddr, linelen, fwidth, fields;
	unsigned addr;
	int cnt;
	int values[16];

	if (argc < 2)
		return(1);

	base = HEX;
	width = SW_WORD;

	while (--argc > 0) {
		if (**++argv == '-') {
			switch ((*argv)[1]) {
			
			case 'b':
				width = SW_BYTE;
				break;

			case 'h':
				width = SW_HALFWORD;
				break;

			case 'w':
				width = SW_WORD;
				break;

			case 'd':
				base = DEC;
				break;

			case 'u':
				base = UNS;
				break;

			case 'x':
				base = HEX;
				break;

			case 'o':
				base = OCT;
				break;

			case 'B':
				base = BIN;
				break;

			case 'c':
				base = CHR;
				break;

			default:
				return(1);
			}
		} else {
			range_type = parse_range(*argv, &addr, &cnt);
			if (range_type == ERROR_RANGE)
				return(1);
		}
	}

	if (range_type == ADDR_RANGE)
  	{
	    if (addr > cnt)
	    {
		eprintf("illegal address range\n");
		return(0);
	    }
	    else
		cnt = ((unsigned)cnt - addr + 1) / width;
	}
	if (cnt == 0)
		cnt = 1;

	linelen = 68;
	fwidth = bc(base, width);
	fields = linelen / (fwidth + sizeof(pad));
	fields = 1 << fms(fields);	/* force to a power of two */

	same_printed = 0;
	for (different = 1; cnt > 0; different = 0, cnt -= fields) {
		baddr = addr;
		for (i = 0; i < fields && i < cnt; i++) {
			val = get_memory(addr, width);
			if (base < 0)
				val = sign_extend(val, width);
			if (val != values[i]) {
				different = 1;
				values[i] = val;
			}
			addr += width;
		}
		if (!different && cnt > fields) {
			if (!same_printed) {
				printf("SAME\n");
				same_printed = 1;
			}
			continue;
		}

		same_printed = 0;
		puts("0x");
		dprint(baddr, HEX, 8, FILL);
		putchar(':');
		for (i = 0; i < fields && i < cnt; i++) {
			puts(pad);
			dprint(values[i], base, fwidth, NO_FILL);
		}
		putchar('\n');
	}
	return(0);
}

/*
 * sign_extend -- sign extend to 32 bits
 */
static
sign_extend(val, width)
{
	switch (width) {
	case sizeof(char):
		val = (char)val;
		break;
	case sizeof(short):
		val = (short)val;
		break;
	}
	return(val);
}

/*
 * bc -- return byte count for formatted print widths
 */
static
bc(base, width)
{
	switch (base) {

	case HEX:
		return((width*8+3)/4);

	case DEC:
		switch (width) {
		case SW_BYTE:
			return(4);

		case SW_HALFWORD:
			return(6);

		case SW_WORD:
			return(11);

		default:
			prom_error("bc");
		}

	case UNS:
		switch (width) {
		case SW_BYTE:
			return(3);

		case SW_HALFWORD:
			return(5);

		case SW_WORD:
			return(10);

		default:
			prom_error("bc");
		}

	case OCT:
		return((width*8+2)/3);

	case CHR:
		return(width * 4);

	case BIN:
		return(width*8);

	default:
		prom_error("bc");
	}
}

/*
 * dprint -- formatted output
 */
dprint(n, b, fwidth, fill_wanted)
register unsigned n;
register int b;
register int fwidth;
int fill_wanted;
{
	register char *cp;
	register char c;
	register i;
	char prbuf[32];
	int minus = 0;

	if (b < 0) {
		b = -b;
		if ((int)n < 0) {
			minus = 1;
			n = (unsigned)(-(int)n);
		}
	}

	cp = prbuf;
	if (b == CHR) {
		/* this is pretty much a kludge, but it gets the job done */
		for (i = 0; i < (fwidth/4); i++) {
			c = n >> (i * 8);
			c &= 0xff;
			if (isprint(c))
				*cp++ = c;
			else switch (c) {
			case '\b':
				*cp++ = 'b';
				*cp++ = '\\';
				break;
			case '\f':
				*cp++ = 'f';
				*cp++ = '\\';
				break;
			case '\n':
				*cp++ = 'n';
				*cp++ = '\\';
				break;
			case '\r':
				*cp++ = 'r';
				*cp++ = '\\';
				break;
			case '\t':
				*cp++ = 't';
				*cp++ = '\\';
				break;
			case ' ':
				*cp++ = ' ';
				*cp++ = '\\';
				break;
			case '\\':
				*cp++ = '\\';
				*cp++ = '\\';
				break;
			default:
				*cp++ = (c&07) + '0';
				*cp++ = ((c>>3) & 07) + '0';
				*cp++ = ((c>>6) & 03) + '0';
				*cp++ = '\\';
				break;
			}
		}
	} else {
		do {
			*cp++ = "0123456789abcdef"[n%b];
			n /= b;
		} while (n);
	}

	fwidth -= cp - prbuf;
	if (minus)
		fwidth--;

	while (fwidth-- > 0)
		putchar(fill_wanted ? '0' : ' ');

	if (minus)
		putchar('-');

	do
		putchar(*--cp);
	while (cp > prbuf);
}

/*
 * fms -- find most significant bit
 * bits numbered from 31 (msb) to 0 (lsb)
 * returns 0 if bits == 0
 */
fms(bits)
register int bits;
{
	register i;

	if (bits == 0)
		return (0);

	for (i = 31; bits > 0; i--)
		bits <<= 1;

	return (i);
}

/*
 * htob -- convert ascii hex digits to binary
 */
htob(cp, len)
register char *cp;
register int len;
{
	register unsigned result;

	result = 0;
	while (len-- > 0)
		result = (result << 4) | _digit(*cp++ & 0x7f);
	return(result);
}

enable(argc, argv)
int argc;
char **argv;
{
	if (argc == 1) {
		ioctl(0, PIOCSHOW, 0);
		return(0);
	}

	if (argc != 2)
		return(1);

	ioctl(0, PIOCENABLE, argv[1]);
	return(0);
}

disable(argc, argv)
int argc;
char **argv;
{
	if (argc == 1) {
		ioctl(0, PIOCSHOW, 0);
		return(0);
	}

	if (argc != 2)
		return(1);

	ioctl(0, PIOCDISABLE, argv[1]);
	return(0);
}

/*
 * go -- transfer control to client code
 */
go(argc, argv)
int argc;
char **argv;
{
	extern char **environ;
#ifdef PROM
	extern unsigned memtop;
#endif

#ifdef PROM
	/*
	 * For m/120 make sure keyswitch is in position to boot
	 */
	if (IS_R2400 && (*getenv("bootmode") != 'd') &&
		(!(*(ushort *)PHYS_TO_K1(SCR) & SCR_NOBOOTLOCK))) {

		printf("* Key is in LOCKED position - go aborted! *\n");
		return(0);
	}
#endif

	if (argc > 2)
		return(1);
	if (argc == 2) {
		if (*atob(argv[1], &client_pc))
			return(1);
	}
	init_restartblk();
#ifdef PROM
#ifdef R3030
	client_start(0, 0, environ, client_pc, PHYS_TO_K1(0x800000 - 512*1024));
#else
	client_start(0, 0, environ, client_pc, PHYS_TO_K1(memtop - 0x4000));
#endif R3030
#endif
#ifdef SASH
	client_start(0, 0, environ, client_pc, 0);
#endif
	/* shouldn't return */
	exit(1);
}

static int csum;	/* checksum is accumulated here */

/*
 * in-line implementation of get_pair for speed
 */

#define DIGIT(c)      ((c)>'9'?((c)>='a'?(c)-'a':(c)-'A')+10:(c)-'0')

#define	get_pair(fd) \
	( \
		c1 = getc(fd) & 0x7f, \
		c2 = getc(fd) & 0x7f, \
		byte = (DIGIT(c1) << 4) | DIGIT(c2) \
	)

int
getc_csum(fd, do_binary)
int fd;
int do_binary;
{
	register c1, c2, byte;

	if (do_binary)
		byte = getc(fd) & 0xff;
	else
		get_pair(fd);
	csum += byte;
	return(byte);
}
	

#define	ACK	0x6
#define	NAK	0x15

sload(argc, argv)
int argc;
char **argv;
{
	register length, address;
	int i, first, done, reccount, fd, type, save_csum;
	int ack = 1;
	int do_binary = 0;	/* o.w. transfer entirely in ascii */

	argv++; argc--;
	while (argc > 0 && **argv == '-') {
		argc--;
		switch ((*argv++)[1]) {
		case 'a':
			ack = 0;	/* turn-off acknowledgements */
			break;
		case 'b':
			do_binary = 1;	/* length/addr/data in binary */
			break;
		default:
			return(1);
		}
	}

	if (argc != 1)
		return(1);
	if ((fd = open(*argv, ack ? O_RDWR : O_RDONLY)) < 0) {
		printf("can't open %s\n", *argv);
		return(0);
	}
	if (!isatty(fd)) {
		printf("%s is not a character device\n", *argv);
		return(0);
	}
	if (do_binary) {
		ioctl(fd, TIOCRAWRAW, 1);	/* completely binary mode */
	}

	init_restartblk();
	reccount = 1;
	for (first = 1, done = 0; !done; first = 0, reccount++) {
		while ((i = getc(fd)) != 'S') {
			if (i == EOF) {
				/*
				 * Only check for errors here,
				 * if user gave a bad device problem
				 * will usually be caught.
				 */
				printf("Error or EOF on %s\n", *argv);
				printf("sload aborted\n");
				goto sload_exit;
			}
			continue;
		}
		csum = 0;
		type = getc(fd);
		if (first && type != '0') {
			printf("Missed initial s-record\n");
			goto sload_exit;
		}
		length = getc_csum(fd,do_binary);
		if (length < 0 || length >= 256) {
			printf("record %d: invalid length\n", reccount);
			goto sload_exit;
		}
		length--;	/* save checksum for later */
		switch (type) {

		case '0':	/* initial record, ignored */
			while (length-- > 0)
				getc_csum(fd,do_binary);
			break;

		case '3':	/* data with 32 bit address */
			address = 0;
			for (i = 0; i < 4; i++) {
				address <<= 8;
				address |= getc_csum(fd,do_binary);
				length--;
			}
			while (length-- > 0)
				*(char *)address++ = getc_csum(fd,do_binary);
			break;

		case '7':	/* end of data, 32 bit initial pc */
			address = 0;
			for (i = 0; i < 4; i++) {
				address <<= 8;
				address |= getc_csum(fd,do_binary);
				length--;
			}
			client_pc = address;
			if (length)
				printf("record %d: type 7 record with unexpected data\n", reccount);
			done = 1;
			break;
		
		default:
			printf("record %d: unknown record type, %c\n",
			    reccount, type);
			break;
		}
		save_csum = (~csum) & 0xff;
		csum = getc_csum(fd,do_binary);
		if (csum != save_csum) {
			printf("record %d: checksum error, calculated 0x%x, received 0x%x\n", reccount, save_csum, csum);
			if (ack)
				putc(NAK, fd);
		} else if (ack)
			putc(ACK, fd);
	}
	printf("Done: %d records, initial pc: 0x%x\n", reccount-1, client_pc);
sload_exit:
	if (do_binary) {
		ioctl(fd, TIOCRAWRAW, 0);	/* undo binary mode */
	}
	close(fd);
	return(0);
}

extern (*get_tod[])();

pr_tod()
{
	sa_spl();
	printf("tod = 0x%x\n", (*MACHDEP(get_tod))());
	return(0);
}

init_tod(argc,argv)
int argc;
char **argv;
{
	extern char *getenv();

	if (*getenv("bootmode") == 'd') {	/* Only allow in bootmode=d */
		if (argc == 2) {
			printf("Setting tod to %d seconds\n", datoi(argv[1]));
			write_todclk(datoi(argv[1]));
		} else {
			printf("Setting tod to 0 seconds\n");
			write_todclk(0);
		}
	} else {
		printf("Setting of TOD not supported in this bootmode\n");
	}
	return(0);
}


datoi(ptr)
register char *ptr;
{
	register int num;
	num = 0;
	while(*ptr){
		num += (*ptr++ - '0');
		if(*ptr && (*ptr != '-'))
			num = num * 10;
		else
			break;
	}
	return(num);
}

unsigned char boot_device[16];
set_boot_dev(bootfile)
char *bootfile;
{
	char * cp;

	strncpy(boot_device,bootfile,15);
	if ( (cp = index(boot_device,'(')) ) {
		*cp = '\0';
	}
	replace_str("boot_dev",boot_device,environ);
	replace_str("boot_dev",boot_device,&environ_str);
}

#ifdef PROM
int diagmon()
{
	/* extern int diag_monitor(); */
	int	(*call_diagmon)();

	call_diagmon = (int (*)())(PROM_DIAGMON);
	/* 
	 * invoke diags monitor from the prom entry point: 
	 * 	returns 0 if not found,
	 *		1 if machine type mismatch.
	 */
	if (call_diagmon() == 0)
		printf("Diagnostics monitor: Not available\n");
	else
		printf("Diagnostics monitor: Incorrect machine type found\n");
	return(0);
}
#endif PROM
