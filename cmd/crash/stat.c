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
#ident	"$Header: stat.c,v 1.4.1.2 90/05/09 15:27:51 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash function stat.
 */

#include "crash.h"
#include "time.h"

static struct syment *Sys, *Time, *Lbolt;               /* namelist symbol */
static struct syment *Putbuf, *Putbufndx, *Putbufsz;    /* namelist symbol */
extern char *ctime();
char *asctime();
struct tm *localtime();


/* get arguments for stat function */
int
getstat()
{
	int c;

	optind = 1;
	while((c = getopt(argcnt,args,"w:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(args[optind])
		longjmp(syn,0);
	else prstat(); 
}


/* print system statistics */
int
prstat()
{
	int toc, lbolt;
	int panicstr;
	char *putbuf, *cp;
	int putbufndx, putbufsz;
	struct utsname utsbuf;
	extern char *malloc();

	/* 
	 * Locate, read, and print the system name, node name, release number,
	 * version number, and machine name.
	 */

	if(!Sys)
		if(!(Sys = symsrch("utsname")))
			error("utsname not found in symbol table\n");
	readmem((long)Sys->n_value,1,-1,(char *)&utsbuf,
		sizeof utsbuf,"utsname structure");

	fprintf(fp,"system name:\t%s\nrelease:\t%s\n",
		utsbuf.sysname,
		utsbuf.release);
	fprintf(fp,"node name:\t%s\nversion:\t%s\n",
		utsbuf.nodename,
		utsbuf.version);
	fprintf(fp,"machine name:\t%s\n", utsbuf.machine) ;
	/*
	 * Locate, read, and print the time of the crash.
	 */

	if(!Time)
		if(!(Time = symsrch("time")))
			error("time not found in symbol table\n");

	readmem((long)Time->n_value,1,-1,(char *)&toc,
		sizeof toc,"time of crash");
	fprintf(fp,"time of crash:\t%s", ctime((long *)&toc));

	/*
	 * Locate, read, and print the age of the system since the last boot.
	 */

	if(!Lbolt)
		if(!(Lbolt = symsrch("lbolt")))
			error("lbolt not found in symbol table\n");

	if(!Putbuf) {
		if(!(Putbuf = symsrch("putbuf")))
			error("putbuf not found in symbol table\n");
		if(!(Putbufndx = symsrch("putbufndx")))
			error("putbufndx not found in symbol table\n");
		if(!(Putbufsz = symsrch("putbufsz")))
			error("putbufsz not found in symbol table\n");
	}
	readmem((long)Lbolt->n_value,1,-1,(char *)&lbolt,
		sizeof lbolt,"lbolt");

	fprintf(fp,"age of system:\t");
	lbolt = lbolt/(60*HZ);
	if(lbolt / (long)(60 * 24))
		fprintf(fp,"%d day, ", lbolt / (long)(60 * 24));
	lbolt %= (long)(60 * 24);
	if(lbolt / (long)60)
		fprintf(fp,"%d hr., ", lbolt / (long)60);
	lbolt %= (long) 60;
	if(lbolt)
		fprintf(fp,"%d min.", lbolt);
	fprintf(fp,"\n");

	/*
	 * Determine if a panic occured by examining the size of the panic string. If
	 * no panic occurred return to main(). If a panic did occur locate, read, and
	 *  print the panic registers. Note: in examining an on-line system, the panic
	 *  registers will always appear to be zero.
	 */

	fprintf(fp,"panicstr:\t");
	seekmem((long)Panic->n_value,1,-1);
	if(read(mem, (char *)&panicstr, sizeof panicstr) == sizeof panicstr &&
		panicstr != 0) {
		char panicbuf[1024];
		readmem(panicstr,1,-1,panicbuf,1024,"panicbuf");
		fprintf(fp,"%s\n",panicbuf);
	}
	fprintf(fp,"\nputbuf:\t");
	readmem(Putbufndx->n_value,1,-1,&putbufndx,sizeof(putbufndx),"putbufndx");
	readmem(Putbufsz->n_value,1,-1,&putbufsz,sizeof(putbufsz),"putbufsz");
	if ((putbuf = malloc(putbufsz)) == NULL)
		error("cannot allocate buffer for putbuf\n");
	seekmem((long)Putbuf->n_value,1,-1);
	if(read(mem, (char *)putbuf, putbufsz) == putbufsz) {
		cp = &putbuf[putbufndx%putbufsz];
		if (cp < putbuf || cp >= &putbuf[putbufsz] || *cp == 0)
			cp = putbuf;
		if (cp == putbuf)
			putbuf[putbufsz-1] = 0;
		else {
			fputc(*cp, fp);
			*cp++ = 0;
		}
		while (*cp) {
			fputc(*cp++, fp);
			if (cp == &putbuf[putbufsz])
				cp = putbuf;
		}
	}
	fputc('\n',fp);
}
