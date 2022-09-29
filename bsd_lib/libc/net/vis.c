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
#ident	"$Header: vis.c,v 1.2.1.5 90/05/10 20:12:27 wje Exp $"

/*
 * VIS - the virtual information service interface for RISC/os.
 *
 * The goal is to allow multiple information services, such as Yellow Pages,
 * Domain Name Service, and X.500, to operate simultaneously on a single
 * RISC/os system in a sane manner.
 *
 * A file, /etc/vis.conf, contains ordered lists of services to be used on
 * a per request class (HOST, GROUP, etc.) basis.  This file can be over-ridden
 * by an environment variable, VIS_SERVICES, which may point to a file of
 * identical syntax which is private.  A routine, vis_nextserv(), is used
 * to ascertain the next service to be tried for a particular request. This
 * routine lives in this file.  See vis.conf(5) for more information.
 *
 * Any section 3 routine which accesses information does so through the
 * VIS interface.  It first calls vis_nextserv() with a magic cookie
 * indicating that this is the first request, and the request it wishes to
 * make.  Then it calls the Service Routine returned by vis_nextserv()
 * to access the information.  A simple example is gethostbyname(), which
 * resides in ./gethostnamadr.c.
 *
 * Each information service has its own directory in "..".  Each must
 * provide a "Service Routine". The Service Routine determines if the
 * service can provide the requested information.  If so, it calls the
 * correct routine and returns a caddr_t pointing to the requested information,
 * or NULL.  If not, it returns -1 to allow the requestor to try the next
 * service in line..
 *
 * All VIS information resides in the include file <netdb.h>.
 */

#define VIS		/* to declare some structs */
#include <sys/types.h>
#include <sys/stat.h>
#include <netdb.h>
#include <stdio.h>

/* We store the information from the service order file in a linked list
 * of these guys, each of which has a linked list of service structures
 * hanging from it.  You walk the class list until you find the correct
 * class, then walk the service list until you find the one after the
 * one you are passed.
 */
struct vis_class_list {
	unsigned int c_class;
	struct vis_serv_list *c_serv;
	struct vis_class_list *c_next;
};
typedef struct vis_class_list VIS_CLASS_LIST;

struct vis_serv_list {
	char *(*s_func)();
	struct vis_serv_list *s_next;
};
typedef struct vis_serv_list VIS_SERV_LIST;

/*
 *     The error recovery attempted by this routine is intended to be simple.
 * Under most classes of errors, we simply fail immediately, returning a
 * condition which should cause wrapper routines to fail.  Error messages
 * can not be sent to stderr because this may be redefined (remember, we're
 * in a library).  We shouldn't report anything via syslog() either because
 * we use a file descriptor which may be critical to the application--this
 * point is debatable.
 *     The one error which we recover from is a syntax error in the vis service
 * file.  Any string which is unrecognizable gets skipped and parsing continues.
 * This will give the wrong functionality, but the only alternative is to
 * return a VIS fatal error (like above) when any parsing error is encountered
 * anywhere in the file.
 */
char *(*
vis_nextserv(last_func, request))()
	char *(*last_func)();
	int request;
{
	static vis_initialized = 0;
	static VIS_CLASS_LIST *class_list = (VIS_CLASS_LIST *)NULL;
	VIS_CLASS_LIST *cp;
	VIS_SERV_LIST *sp;
	FILE *fp;
	char *serv_file;
	char *getenv();
	int file_from_env = 0;

	if (!vis_initialized) {
		if ((serv_file = getenv("VIS_SERVICES")) == (char *)NULL)
			serv_file = VIS_DEFAULT_SERVICES;
		else
			file_from_env = 1;
		fp = fopen(serv_file, "r");
		/* there is a default if the file doesn't exist */
		if (fp == NULL && file_from_env) {
			/* Can't open Information Service file */
			return(VIS_DONE);
		}
		/* parse_servfile recovers from syntax errors */
		parse_servfile(fp, &class_list,(fp ? 0 : 1));
		if (fp != NULL)
		 	(void) fclose(fp);
		vis_initialized = 1;
	}

	cp = class_list;
	while (cp && (cp->c_class != VIS_CLASS(request)))
		cp = cp->c_next;
	if (!cp)
		return(VIS_DONE);

	sp = cp->c_serv;
	if (last_func == VIS_FIRST_CALL) {
		if (sp)
			return(sp->s_func);
		else
			return(VIS_DONE);
	}
	while (sp && (sp->s_func != last_func))
		sp = sp->s_next;
	if (!sp || !sp->s_next)
		return(VIS_DONE);
	else
		return(sp->s_next->s_func);
		
} /* vis_nextserv() */

#define VIS_MAXLINE	256

/* File format is "class: serv,serv,serv"
 *
 * 1) Lines without a recognizable class or ':' are tossed.
 * 2) Lines looking like "class: " define the class with no services
 * 3) Lines with only unrecognizable services define a class w/ no services
 */
static int
parse_servfile(fp, head, using_default)
	FILE *fp;
	VIS_CLASS_LIST **head;
	int using_default;
{
	char *fgets(), *malloc();
	char line[VIS_MAXLINE];
	VIS_CLASS_LIST *cp, *tail = (VIS_CLASS_LIST *)NULL;
	VIS_SERV_LIST *sp, *sp1 = (VIS_SERV_LIST *)NULL;
	int i;
	char *c, *colon, *index();
	char *(*next_serv())();

	while (!using_default && fgets(line,VIS_MAXLINE,fp)!=(char *)NULL) {
		if (line[0] == '#')
			continue;
		/* alloc and link cp */
		cp = (VIS_CLASS_LIST *)malloc(sizeof(VIS_CLASS_LIST));
		if (cp == NULL)
			goto fatal_err;
		bzero((char *)cp, sizeof(VIS_CLASS_LIST)); /* zero ->c_next */
		/* isolate class name, and strip leading & trailing white */
		c = line;
		i = 0;
		while (line[i] == ' ' || line[i++] == '\t')
			c++;
		colon = index(line, ':');
		if (colon == 0) {
			(void) free(cp);
			continue;		/* syntax error: skip line */
		}
		*colon = ' ';
		i = 0;
		while (*(colon - i) == ' ' || *(colon - i) == '\t')
			*(colon - i++) = '\0';
		/* set class name, and alloc and link serv structs */
		for (i = 0 ; Vis_classes[i].name ; i++) {
			if (!strcmp(Vis_classes[i].name, c))
				break;
		}

		if (Vis_classes[i].name == NULL ||
		    strcmp(Vis_classes[i].name, c)) {
			(void) free(cp);
			continue;		/* unknown class: skip line */
		}
			
		cp->c_class = Vis_classes[i].class;
		c = colon + 1;	/* set c to char after ':' */

		/* done before while loop so that fatal_err works */
		if (tail == (VIS_CLASS_LIST *)NULL)
			*head = cp;
		else
			tail->c_next = cp;
		tail = cp;

		sp1 = (VIS_SERV_LIST *)NULL;
		while (c) {
			sp = (VIS_SERV_LIST *)malloc(sizeof(VIS_SERV_LIST));
			if (sp == (VIS_SERV_LIST *)NULL)
				goto fatal_err;
			bzero(sp, sizeof(VIS_SERV_LIST));
			if (sp1)
				sp1->s_next = sp;
			sp1 = sp;
			if (!cp->c_serv)
				cp->c_serv = sp;
			sp->s_func = next_serv(&c);
			if (sp->s_func == (char *(*)())NULL)   /* end of line */
				break;
		}
	} /* while fgets() != NULL */
	/* Now, make sure there is at least one entry for each class. If
	 * not, create one that points to a default Service Routine on
	 * the list.  It should, (and will be for 4.50 at least) be the
	 * 'files' Service for every class except VIS_HOST which may put
	 *  'dns' before 'files' as a default.
	 */
	for (i=0; Vis_classes[i].name; i++) {
		for (cp = *head; cp ; cp = cp->c_next) {
			if (cp->c_class == Vis_classes[i].class)
				break;
		}
		if (cp)
			continue;

		cp = (VIS_CLASS_LIST *)malloc(sizeof(VIS_CLASS_LIST));
		if (*head == NULL)
			*head = cp;
		bzero(cp, sizeof(VIS_CLASS_LIST));
		cp->c_class = Vis_classes[i].class;
		if (tail)
			tail->c_next = cp;
		tail = cp;
		sp = (VIS_SERV_LIST *)malloc(sizeof(VIS_SERV_LIST));
		bzero(sp, sizeof(VIS_SERV_LIST));
		cp->c_serv = sp;

		switch (Vis_classes[i].class) {	/* fill in func ptr list */
		    struct stat buf;

		    case VIS_HOST:
			if (stat("/etc/named.pid", &buf) == 0) {
				sp->s_func = Vis_services[VIS_DNS].vs_func;
				sp1 = sp;
				sp = (VIS_SERV_LIST *)malloc
				                        (sizeof(VIS_SERV_LIST));
				bzero(sp, sizeof(VIS_SERV_LIST));
				sp1->s_next = sp;
			}
		        sp->s_func = Vis_services[VIS_FILES].vs_func;
			break;
		    default:
			sp->s_func = Vis_services[VIS_FILES].vs_func;
			break;
		}
	}
	return;

fatal_err:
	/* free up all the space we allocated and return a NULL list */
	for (cp = *head; cp; cp = tail) {
		for (sp = cp->c_serv; sp; sp = sp1) {
			sp1 = sp->s_next;
			(void) free(sp);
		}
		tail = cp->c_next;
		(void) free(cp);
	}
	*head = (VIS_CLASS_LIST *)NULL;
} /* parse_servfile() */


/* Stub for service routines that don't exist (yet). */
char *
no_service(request, args)
	int request;
	char *args;
{
	return(NULL);
}


/* pass me a char ** that points to a service name, I will set the char *
 * to point to the next service name, and return the service func for the
 * original name.  If an unrecognizable service name is encountered, we skip
 * it and continue parsing rather than returning an error.  This routine
 * will only return a valid service or indication that we've hit the end
 * of the line.
 */
static char *(*
next_serv(c))()
	char **c;
{
	char *cp, *cp1;
	int i, eol = 0;
	int error;

	do {
		error = 0;
		cp = *c;
		while (*cp == ' ' || *cp == '\t')
			cp++;
		if (*cp == '\0') {  /* eof-line terminated by null, not \n */
			*c = NULL;
			return((char *(*)())NULL);
		}
		cp1 = cp;
		while (*cp1 != ' ' && *cp1 != '\t' && *cp1 != '\n')
			cp1++;
		if (*cp1 == '\n') {
			*c = '\0';	/* NULL */
			eol++;
		}
		*cp1 = '\0';
		for (i = 0 ; Vis_services[i].vs_name ; i++)
			if (!strcmp(cp, Vis_services[i].vs_name))
				break;
		if (Vis_services[i].vs_name == 0 ||
		    strcmp(cp, Vis_services[i].vs_name)) {
			/* Unrecognizable service name */
			error++;
		}
		if (eol) continue;	/* no need to go any further */
		cp1++;
		while (*cp1 == ' ' || *cp1 == '\t')
			cp1++;
		if (*cp1 == '\n' || *cp1 == '\0' || eol) {
			*c = '\0';	/* NULL */
			eol++;
		} else
			*c = cp1;
	} while (error && !eol);
	return(error ? ((char *(*)())NULL) : Vis_services[i].vs_func);
} /* next_serv() */
