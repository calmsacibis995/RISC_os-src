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
#ident	"$Header: dosys.c,v 1.5.2.2 90/05/09 16:43:59 wje Exp $"
/*
 * ke	3/7/85	sys/types.h and sys/dir.h now get included defs.  This is
 *		because of the ifdefs for the 4.2bsd file system stuff.
 * ke	3/14/85	added fix for correct exec'ing of shell scripts using the
 *		sh (/bin/sh) or csh (/bin/csh).  Ifdef'ed the await()
 *		routine to work under a 4.2bsd UNIX kernel.
 */

#ifdef BSD42
#include <sys/wait.h>
#endif BSD42
# include "defs"
# include "sys/stat.h"

extern char Makecall;

dosys(comstring, nohalt)
register CHARSTAR comstring;
int nohalt;
{
	register CHARSTAR p;
	register int i;
	int status;

	p = comstring;
	while(	*p == BLANK ||
		*p == TAB) p++;
	if(!*p)
		return(-1);

	if(IS_ON(NOEX) && Makecall == NO)
		return(0);

	/* If meta-chars are in comstring, automatically use the shell to
	 * execute the command.  Else, try execing the command directly.
	 * If doexec() returns -2, it means the the file is a shell script
	 * and doshell() is then called to execute it.  This is because of
	 * a problem in the cshell that will not let it interpret /bin/sh
	 * scripts if invoked as `csh script' (where script is the name of
	 * the (sh) script file).  (A return of -1 means that a null command
	 * was passed.)
 	 */
	if(metas(comstring))
		status = doshell(comstring,nohalt);
	else
	{
		status = doexec(comstring);
		if ((char)(status >> 8) == -2)
		    status = doshell(comstring,nohalt);
	}
	return(status);
}



metas(s)   /* Are there are any  Shell meta-characters? */
register CHARSTAR s;
{
	while(*s)
		if( funny[*s++] & META)
			return(YES);

	return(NO);
}

doshell(comstring,nohalt)
register CHARSTAR comstring;
register int nohalt;
{
	register CHARSTAR shell;

	if ((waitpid = fork()) == 0) {
		char evalshell[1024];
		char *endshell, *subst();

		enbint(0);
		doclose();

		setenv();
		shell = varptr("SHELL")->varval;
		if (shell == 0 || shell[0] == CNULL) {
			endshell = &SHELLCOM[sizeof SHELLCOM - 1];
			shell = SHELLCOM;
		} else {
			endshell = subst(shell, evalshell);
			shell = evalshell;
		}
		execl(shell, "sh", 
		    (strcmp(endshell - 3, "csh") == 0) ?
			(nohalt? "-cf" : "-cef") :	/* csh */
			(nohalt? "-c"  : "-ce"),  	/* sh */
		    comstring, 0);
		fatal("Couldn't load Shell");
	}

	return await();
}




await()
{
	int intrupt();
#ifdef BSD42
	union wait status;
#else
	int status;
#endif BSD42
	int pid;

	enbint(intrupt);
	while( (pid = wait(&status)) != waitpid)
		if(pid == -1)
			fatal("bad wait code");
	waitpid = 0;
#ifdef BSD42
	return((status.w_coredump << 15)|(status.w_termsig << 8)|
	 status.w_retcode);
#else
	return(status);
#endif BSD42
}






doclose()	/* Close open directory files before exec'ing */
{
	register OPENDIR od;

	for (od = firstod; od != 0; od = od->nextopendir) {
		if (od->dirfc != NULL) {
			closedir(od->dirfc);
		}
	}
}





doexec(str)
register CHARSTAR str;
{
	CHARSTAR Nlocn = str;	/* set to point at first char of str */
	CHARSTAR Elocn;		/* will be set to loc of null at end of str */
				/* see comments in dosys() */
	register CHARSTAR t;
	register CHARSTAR *p;
	CHARSTAR argv[4096];
	int status;

	while( *str==BLANK || *str==TAB )
		++str;
	if( *str == CNULL )
		return(-1);	/* no command */

	p = argv;
	for(t = str ; *t ; )
	{
		*p++ = t;
		while(*t!=BLANK && *t!=TAB && *t!=CNULL)
			++t;
		if(*t)
			for( *t++ = CNULL ; *t==BLANK || *t==TAB  ; ++t);
	}
	Elocn = t;	/* save the end location */

	*p = NULL;

	if((waitpid = fork()) == 0)
	{
		enbint(0);
		doclose();
		setenv();
		/* The test was for -2 which I think was wrong. ke 3/14/85 */
		if (execvp(str, argv) == -1)
		    exit(-2);
		fatal1("Cannot load %s",str);
	}
	status = await();
	    if ((char)(status >> 8) == -2)
		for( ; Nlocn < Elocn; Nlocn++)
		    if (*Nlocn == CNULL)
		        *Nlocn = BLANK;

	return( status );
}

touch(force, name)
register int force;
register char *name;
{
        struct stat stbuff;
        char junk[1];
        int fd;

        if( stat(name,&stbuff) < 0)
                if(force)
                        goto create;
                else
                {
                        fprintf(stderr,"touch: file %s does not exist.\n",name);
                        return;
                }
        if(stbuff.st_size == 0)
                goto create;
        if( (fd = open(name, 2)) < 0)
                goto bad;
        if( read(fd, junk, 1) < 1)
        {
                close(fd);
                goto bad;
        }
        lseek(fd, 0L, 0);
        if( write(fd, junk, 1) < 1 )
        {
                close(fd);
                goto bad;
        }
        close(fd);
        return;
bad:
        fprintf(stderr, "Cannot touch %s\n", name);
        return;
create:
        if( (fd = creat(name, 0666)) < 0)
                goto bad;
        close(fd);
}
