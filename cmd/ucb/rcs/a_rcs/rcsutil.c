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
#ident	"$Header: rcsutil.c,v 1.5.1.3 90/05/10 00:24:06 wje Exp $"

/*
 *                     RCS utilities
 */
/*****************************************************************************
 *****************************************************************************
 *
 * Copyright (C) 1982 by Walter F. Tichy
 *                       Purdue University
 *                       Computer Science Department
 *                       West Lafayette, IN 47907
 *
 * All rights reserved. No part of this software may be sold or distributed
 * in any form or by any means without the prior written permission of the
 * author.
 * Report problems and direct all inquiries to Tichy@purdue (ARPA net).
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include "rcsbase.h"

extern char * malloc();
extern FILE * finptr;
extern char * getfullRCSname();

struct lock * addlock(delta,who)
struct hshentry * delta; char * who;
/* Given a delta, addlock checks whether
 * the delta is locked by somebody other than who.
 * If so, an error message is printed, and false returned.
 * If the delta is not reserved at all, a lock for it is added,
 * and a pointer for the lock returned.
 */
{
        struct lock * next;

        next=Locks;
        while (next!=nil) {
                if (cmpnum(delta->num,next->delta->num)==0) {
                        if (strcmp(who,next->login)==0)
                                return next;
                                /* lock exists already */
                        else {
                                error("revision %s already locked by %s",
                                      delta->num, next->login);
                                return false;
                        }
                } else {
                        if (strcmp(who,next->login)==0) {
                                error("you already locked %s; only one lock allowed per person.",
                                       next->delta->num);
                                return false;
                        } else {
                                next=next->nextlock;
                        }
                }
        }
        /* not found; set up new lockblock */
        next= (struct lock *) malloc(sizeof (struct lock));
        delta->lockedby=next->login=who;
        next->delta= delta;
        next->nextlock=Locks;
        Locks=next;
        return next;
}



int addsymbol(delta,name,rebind)
struct hshentry * delta; char * name; int rebind;
/* Function: adds a new symbolic name and associates it with node delta.
 * If name already exists and rebind is true, the name is associated
 * with the new delta; otherwise, an error message is printed and
 * false returned. Returns true it successful.
 */
{       register struct assoc * next;
        next=Symbols;
        while (next!=nil) {
                if (strcmp(name,next->symbol)==0) {
                        if (rebind) {
                                next->delta=delta;
                                return true;
                        } else {
                                error("symbolic name %s already bound to %s",
                                        name,next->delta->num);
                                return false;
                        }
                } else  next = next->nextassoc;
        }
        /* not found; insert new pair. */
        next = (struct assoc *) malloc(sizeof(struct assoc));
        next->symbol=name;
        next->delta=delta;
        next->nextassoc=Symbols;
        Symbols = next;
        return true;
}




int checkaccesslist(who)
char * who;
/* function: Returns true if who is the superuser, the owner of the
 * file, the access list is empty, or who is on the access list.
 * Prints an error message and returns false otherwise.
 */
{
        register struct access * next;
        struct stat statbuf;

        if ((AccessList==nil) || (strcmp(who,"root")==0))
                return true;

        next=AccessList;
        do {
                if (strcmp(who,next->login)==0)
                        return true;
                next=next->nextaccess;
        } while (next!=nil);

        VOID fstat(fileno(finptr),&statbuf);  /* get owner of file */
        if (getuid() == statbuf.st_uid) return true;

        error("User %s not on the access list",who);
        return false;
}

void catchsig(sig)
{
	VOID signal(sig, SIG_IGN);
        diagnose("\nRCS: cleaning up\n");
        cleanup();
        exit(1);
}

/*
 * This routine may be called multiple times.  The first time through,
 * it checks to see whether or not the signal is ignored so that
 * commands run in the background don't catch signals.  Subsequent
 * times through just use the "ignore" flag value from the first time.
 */

void catchints()
{
	static int first_time = 1;
	static int i_int = 0;
	static int i_hup = 0;
	static int i_quit = 0;
	static int i_pipe = 0;
	static int i_term = 0;

        if (signal(SIGINT,catchsig) == SIG_IGN) {
		if (first_time) {
			i_int = 1;
		}
		if (i_int) {
			VOID signal(SIGINT,SIG_IGN);
		}
	}
	if (signal(SIGHUP,catchsig) == SIG_IGN) {
		if (first_time) {
			i_hup = 1;
		}
		if (i_hup) {
			VOID signal(SIGHUP,SIG_IGN);
		}
	}
        if (signal(SIGQUIT,catchsig) == SIG_IGN) {
		if (first_time) {
			i_quit = 1;
		}
		if (i_quit) {
			VOID signal(SIGQUIT,SIG_IGN);
		}
	}
	if (signal(SIGPIPE,catchsig) == SIG_IGN) {
		if (first_time) {
			i_pipe = 1;
		}
		if (i_pipe) {
			VOID signal(SIGPIPE,SIG_IGN);
		}
	}
	if (signal(SIGTERM,catchsig) == SIG_IGN) {
		if (first_time) {
			i_term = 1;
		}
		if (i_term) {
			VOID signal(SIGTERM,SIG_IGN);
		}
	}

	first_time = 0;
}

void ignoreints()
{
        VOID signal(SIGINT,SIG_IGN); VOID signal(SIGHUP,SIG_IGN);
        VOID signal(SIGQUIT,SIG_IGN); VOID signal(SIGPIPE,SIG_IGN);
	VOID signal(SIGTERM,SIG_IGN);
}


fastcopy(inf,outf)
FILE * inf, * outf;
/* Function: copies the remainder of file inf to outf. First copies the
 * rest that is in the IO-buffer of inf character by character, and then
 * copies the remainder in blocks.
 */
{       char buf[BUFSIZ];
        register int rcount, wcount;

        /* write the rest of the buffer to outf */
        while ((--inf->_cnt)>=0) {
                VOID putc(*inf->_ptr++&0377,outf);
        }
        if (fflush(outf) == EOF) {
		faterror("write error");
	}

        /*now read the rest of the file in blocks*/
        while ((rcount=read(fileno(inf),buf,BUFSIZ))>0) {
                wcount=write(fileno(outf),buf,rcount);
                if (wcount!=rcount) {
                    faterror("write error");
                }
        }
}






#ifdef SNOOPFILE

#include "time.h"
extern struct tm* localtime();
extern long time();

logcommand(commandname,delta, sequence,login)
char* commandname; struct hshentry * delta, * sequence[];char * login;
/* Function: start a process to write the file that
 * logs the RCS command.
 * Each line in the log file contains the following information:
 * operation, revision(r), backward deltas applied(b), forward deltas applied(f),
 * total deltas present(t), creation date of delta(d), date of operation(o),
 * login of caller, full path of RCS file
 */
{
        char command[200];
        char curdate[datelength];
        register int i, backward, forward;
        long clock;
        struct tm * tm;

        clock=time((long *)0);
        tm=localtime(&clock);

        VOID sprintf(curdate,DATEFORM,
                tm->tm_year, tm->tm_mon+1, tm->tm_mday,
                tm->tm_hour, tm->tm_min, tm->tm_sec);

        i= backward=forward=0;
        while(sequence[i]!=nil) {  /* count deltas to be applied*/
        if (countnumflds(sequence[i]->num) == 2)
                backward++;  /* reverse delta */
        else    forward++;   /* branch delta  */
        i++;
        }
        VOID sprintf(command,"%s \"%s %10sr %3db %3df %3dt %sc %so %s %s\" &\n",
                SNOOP, commandname,delta->num,backward,forward,TotalDeltas,delta->date,
                curdate,login,getfullRCSname());
        VOID system(command);
}
#endif
