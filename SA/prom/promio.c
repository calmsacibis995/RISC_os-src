#ident "$Header: promio.c,v 1.6 90/04/10 17:27:09 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright
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
 * promio.c -- prom io sub-system routines
 */

#include "prom/prom.h"
#include "prom/entrypt.h"
#include "sys/param.h"
#include "sys/inode.h"
#include "sys/fs.h"
#include "sys/dir.h"
#include "sys/file.h"
#include "netinet/in.h"
#include "machine/dvh.h"
#include "machine/cpu.h"
#include "saio/tpd.h"
#include "saio/saio.h"
#include "saio/setjmp.h"
#include "saio/stringlist.h"
#include <a.out.h>
#include "mips/cpu_board.h"  

/*
 * Define the MIPS2 MAGIC values here for now, until header files are updated.
 */
#define MIPS2EBMAGIC_OLD 0x0150
#define MIPS2EBMAGIC	 0x0163
#define MIPS2ELMAGIC_OLD 0x5001  
#define MIPS2ELMAGIC	 0x6301  

/*
 * Macros to verify correct byte order for executables
 */
#ifdef MIPSEB
#define	N_BADBO(fh)	((fh).f_magic != MIPSEBMAGIC)
#define MIPS2MAGIC MIPS2EBMAGIC
#define MIPS2MAGIC_OLD MIPS2EBMAGIC_OLD
#endif

#ifdef MIPSEL
#define	N_BADBO(fh)	((fh).f_magic != MIPSELMAGIC)
#define MIPS2MAGIC MIPS2ELMAGIC
#define MIPS2MAGIC_OLD MIPS2ELMAGIC_OLD
#endif

/*
 * format of bootable a.out file headers
 */
struct execinfo {
	struct filehdr fh;
	AOUTHDR ah;
};

/*
 * getxfile -- load binary image
 */
getxfile(filename)
char *filename;
{
	struct execinfo ei;
	int fd;
	extern unsigned client_pc;

	init_restartblk();
	if ((fd = open(filename, O_RDONLY)) < 0)
		return(-1);

	if (read(fd, &ei, sizeof(ei)) != sizeof(ei)) {
		eprintf("%s: not in a.out format\n", filename);
		goto bad;
	}
	if (N_BADBO(ei.fh)) {
	  /* Well, it's not MIPS1.  If we're a 6000 based machine see if
	   * program is MIPS2.
	   */
	  if ((ei.fh.f_magic != MIPS2MAGIC_OLD)
			      && (ei.fh.f_magic != MIPS2MAGIC)) {
		eprintf("%s: inappropriate byte order\n", filename);
		goto bad;
	      }
	  if (!(IS_R6300)) {
		eprintf("%s: can't run mips2 program\n", filename);
		goto bad;
	      }
	}
	if (N_BADMAG(ei.ah)) {
		eprintf("%s: bad magic number\n", filename);
		goto bad;
	}
	lseek(fd, N_TXTOFF(ei.fh, ei.ah), 0);
	printf("%d", ei.ah.tsize);
	if (range_check(ei.ah.text_start, ei.ah.tsize))
		goto bad;
	if (read(fd, ei.ah.text_start, ei.ah.tsize)
	    != ei.ah.tsize) {
		eprintf("\n%s: short read\n", filename);
		goto bad;
	}
	printf("+%d", ei.ah.dsize);
	if (range_check(ei.ah.data_start, ei.ah.dsize))
		goto bad;
	if (read(fd, ei.ah.data_start, ei.ah.dsize)
	    != ei.ah.dsize) {
		eprintf("\n%s: short read\n", filename);
		goto bad;
	}
	printf("+%d", ei.ah.bsize);
	if (range_check(ei.ah.bss_start, ei.ah.bsize)) {
		/*
		 * minor hack: set client_pc and print intended entry point
		 * to make downloading lowprom easier (since downloading
		 * lowprom always fails on a range_check)
		 */
		client_pc = ei.ah.entry;
		printf("intended entry point: 0x%x\n", ei.ah.entry);
		goto bad;
	}
	close(fd);
	bzero(ei.ah.bss_start, ei.ah.bsize);
	printf(" entry: 0x%x\n", ei.ah.entry);
	return(ei.ah.entry);

bad:
	close(fd);
	return(-1);
}

/*
 * promexec -- prom monitor version of exec
 * load new image and execute
 *
 * callable from standalone programs, so copy args and
 * environment one more time to make sure they're in a
 * safe place
 *
 * NOTE: can't be on stack, since we don't know who's stack were on!
 */
struct string_list exec_argv;
struct string_list exec_environ;

promexec(pap)
register struct promexec_args *pap;
{
	register char **wp;
	struct string_list path_list;
	int pc, pathcnt, i;
	char path[64];
	char *cp;
	extern unsigned client_pc;
	extern char *index();
	extern char *getenv();
#ifdef PROM
	extern unsigned memsize;
#endif

	init_str(&exec_argv);
	for (wp = pap->pa_argv; wp && *wp; wp++)
		if (new_str1(*wp, &exec_argv))
			return(-1);
	init_str(&exec_environ);
	for (wp = pap->pa_environ; wp && *wp; wp++)
		if (new_str1(*wp, &exec_environ))
			return(-1);

	if (index(pap->pa_bootfile, '(')) {
		/*
		 * file had a device specified, so just try booting it
		 */
		pc = getxfile(pap->pa_bootfile);
		if (pc == -1)
			return(-1);
	} else {
		/*
		 * no device specified so try searching path
		 */
		if ((cp = getenv("path")) == NULL) {
			printf("no device and $path not set: %s\n",
			    pap->pa_bootfile);
			return(-1);
		}
		pathcnt = _argvize(cp, &path_list);
		for (i = 0; i < pathcnt; i++) {
			strncpy(path, path_list.strptrs[i], sizeof(path));
			strncat(path, pap->pa_bootfile, sizeof(path));
			pc = getxfile(path);
			if (pc != -1)
				break;
		}
		if (i >= pathcnt)
			return(-1);
		/*
		 * replace argv[0] with full pathname, so booted program
		 * can figure out device it was booted off of
		 */
		set_str(path, 0, &exec_argv);
	}

	client_pc = pc;		/* incase of nogo and later go */

	if (pap->pa_flags & EXEC_NOGO)
		return(pc);

	/*
	 * transfer control to new image
	 * Call client_start, pass ptrs to stdio routines
	 */
#ifdef PROM
	/*
	 * start stack near top of memory (leave a couple of pages
	 * for kernel message buffers
	 */
#ifdef R3030
	save_gsparam();
	client_start(exec_argv.strcnt, exec_argv.strptrs, exec_environ.strptrs,
	    pc, PHYS_TO_K1(0x800000 - 512*1024));
#else
	client_start(exec_argv.strcnt, exec_argv.strptrs, exec_environ.strptrs,
	    pc, PHYS_TO_K1(memsize - 0x4000));
#endif R3030
#endif PROM
#ifdef SASH
	/*
	 * start stack immediately below current stack
	 */
	if (IS_R3030)
	    save_gsparam();
	client_start(exec_argv.strcnt, exec_argv.strptrs, exec_environ.strptrs,
	    pc, 0);
#endif
	/* shouldn't return */
	exit(0);
}

/*
 * range_check -- make sure not overlaying prom/sash
 * very half-hearted checking, though; if addressed is mapped, we
 * assume the user knows what he doing
 */
range_check(start, size)
unsigned start, size;
{
#ifdef PROM
	if (!IS_KSEG0(start) && !IS_KSEG1(start))
		return(0);	/* they must know what they're doing (?) */

	/* K0_TO_K1 of something in KSEG1 shouldn't hurt */
	if (K0_TO_K1(start) >= K0_TO_K1(PROM_STACK))
		return(0);	/* after anything we're interested in */

	eprintf("\nattempt to overlay prom bss or stack\n");
	return(1);
#endif PROM
#ifdef SASH
	extern char _ftext[], end[];

	if (!IS_KSEG0(start) && !IS_KSEG1(start))
		return(0);	/* they must know what they're doing (?) */
	if (K0_TO_K1(start) >= K0_TO_K1(end)
	    || K0_TO_K1(start + size) <= K0_TO_K1(_ftext))
		return(0);

	eprintf("\nattempt to overlay sash text, data, or bss\n");
	return(1);
#endif SASH
}
