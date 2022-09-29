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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: sm_statd.c,v 1.2.1.2.1.1.1.2 90/11/17 12:01:51 beacker Exp $"

/* @(#)sm_statd.c 1.1 88/04/20 4.0NFSSRC Copyright 1988 Sun Microsystems */

/* sm_statd.c consists of routines used for the intermediate
 * statd implementation(3.2 rpc.statd);
 * It creates an entry in "current" directory for each site that it monitors;
 * after crash and recovery, it moves all entries in "current" to "backup"
 * directory and notifies the corresponding statd of its recovery.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#ifndef RISCOS
#include <sys/dirent.h>
#endif
#include <sys/dir.h>
#include <rpc/rpc.h>
#include <rpcsvc/sm_inter.h>
#include <errno.h>
#include "sm_statd.h"
#ifdef RISCOS
#include <bsd43/sys/syslog.h>
#endif

#define MAXPGSIZE 8192
#define SM_INIT_ALARM 15
extern int debug;
extern int errno;
extern char STATE[MAXHOSTNAMELEN], CURRENT[MAXHOSTNAMELEN],
  BACKUP[MAXHOSTNAMELEN];
extern char *strcpy(), *strcat();
int LOCAL_STATE;

struct name_entry {
	char *name;
	int count;
	struct name_entry *prev;
	struct name_entry *nxt;
};
typedef struct name_entry name_entry;

name_entry *find_name();
name_entry *insert_name();
name_entry *record_q;
name_entry *recovery_q;

char hostname[MAXNAMLEN];
extern char hostname[MAXNAMLEN];

sm_notify(ntfp)
	stat_chge *ntfp;
{
	if (debug)
#ifdef RISCOS
		syslog(LOG_ERR,"sm_notify: %s state =%d\n", ntfp->name, ntfp->state);
#else
		printf("sm_notify: %s state =%d\n", ntfp->name, ntfp->state);
#endif
	send_notice(ntfp->name, ntfp->state);
}

/*
 * called when statd first comes up; it searches /etc/sm to gather
 * all entries to notify its own failure
 */
statd_init()
{
	int cc, fd;
	char buf[MAXPGSIZE];
	long base;
	struct direct *dirp;
	DIR 	*dp;
	char from[MAXNAMLEN], to[MAXNAMLEN], path[MAXNAMLEN];
	FILE *fp, *fopen();

	if (debug)
#ifdef RISCOS
		syslog(LOG_ERR,"enter statd_init\n");
#else
		printf("enter statd_init\n");
#endif
	(void) gethostname(hostname, MAXNAMLEN);
	if ((fp = fopen(STATE, "a+")) == NULL) {
#ifdef RISCOS
		syslog(LOG_ERR, "rpc.statd: fopen(stat file) error\n");
#else
		fprintf(stderr, "rpc.statd: fopen(stat file) error\n");
#endif
		exit(1);
	}
	if (fseek(fp, 0, 0) == -1) {
#ifdef RISCOS
		syslog(LOG_ERR,"rpc.statd: fseek failed %m");
		syslog(LOG_ERR, "\n");
#else
		perror("rpc.statd: fseek failed");
		fprintf(stderr, "\n");
#endif
		exit(1);
	}
	if ((cc = fscanf(fp, "%d", &LOCAL_STATE)) == EOF) {
		if (debug >= 2)
#ifdef RISCOS
      		  syslog(LOG_ERR,"empty file\n");
#else
      		  printf("empty file\n");
#endif
		LOCAL_STATE = 0;
	}
	LOCAL_STATE = ((LOCAL_STATE%2) == 0) ? LOCAL_STATE+1 : LOCAL_STATE+2;
	if (fseek(fp, 0, 0) == -1) {
#ifdef RISCOS
		syslog(LOG_ERR,"rpc.statd: fseek failed %m");
		syslog(LOG_ERR, "\n");
#else
		perror("rpc.statd: fseek failed");
		fprintf(stderr, "\n");
#endif
		exit(1);
	}
#ifdef RISCOS
	syslog(LOG_ERR, "%d", LOCAL_STATE);
#else
	fprintf(fp, "%d", LOCAL_STATE);
#endif
	(void) fflush(fp);
	if (fsync(fileno(fp)) == -1) {
#ifdef RISCOS
		syslog(LOG_ERR,"rpc.statd: fsync failed %m");
		syslog(LOG_ERR, "\n");
#else
		perror("rpc.statd: fsync failed");
		fprintf(stderr, "\n");
#endif
		exit(1);
	}
	(void) fclose(fp);
	if (debug)
#ifdef RISCOS
		syslog(LOG_ERR,"local state = %d\n", LOCAL_STATE);
#else
		printf("local state = %d\n", LOCAL_STATE);
#endif
	
	if ((mkdir(CURRENT, 00777)) == -1) {
		if (errno != EEXIST) {
#ifdef RISCOS
			syslog(LOG_ERR,"rpc.statd: mkdir current %m");
			syslog(LOG_ERR, "\n");
#else
			perror("rpc.statd: mkdir current");
			fprintf(stderr, "\n");
#endif
			exit(1);
		}
	}
	if ((mkdir(BACKUP, 00777)) == -1) {
		if (errno != EEXIST) {
#ifdef RISCOS
			syslog(LOG_ERR,"rpc.statd: mkdir backup %m");
			syslog(LOG_ERR, "\n");
#else
			perror("rpc.statd: mkdir backup");
			fprintf(stderr, "\n");
#endif
			exit(1);
		}
	}

	/* get all entries in CURRENT into BACKUP */
	if ((dp = opendir(CURRENT)) == NULL) {
#ifdef RISCOS
		syslog(LOG_ERR,"rpc.statd: open current directory %m");
		syslog(LOG_ERR, "\n");
#else
		perror("rpc.statd: open current directory");
		fprintf(stderr, "\n");
#endif
		exit(1);
	}
	for (dirp = readdir(dp); dirp != NULL; dirp = readdir(dp)) {

		if (debug)
#ifdef RISCOS
		  syslog(LOG_ERR,"d_name = %s\n", dirp->d_name);
#else
		  printf("d_name = %s\n", dirp->d_name);
#endif
		if (strcmp(dirp->d_name, ".") != 0  &&
		strcmp(dirp->d_name, "..") != 0) {
		/* rename all entries from CURRENT to BACKUP */
			(void) strcpy(from , CURRENT);
			(void) strcpy(to, BACKUP);
			(void) strcat(from, "/");
			(void) strcat(to, "/");
			(void) strcat(from, dirp->d_name);
			(void) strcat(to, dirp->d_name);
			if (rename(from, to) == -1) {
#ifdef RISCOS
				syslog(LOG_ERR,"rpc.statd: rename %m");
				syslog(LOG_ERR, "\n");
#else
				perror("rpc.statd: rename");
				fprintf(stderr, "\n");
#endif
				exit(1);
			}
			if (debug >= 2)
#ifdef RISCOS
				syslog(LOG_ERR,"rename: %s to %s\n", from ,to);
#else
				printf("rename: %s to %s\n", from ,to);
#endif
		}
	}
	closedir(dp);

	/* get all entries in BACKUP into recovery_q */
	if ((dp = opendir(BACKUP)) == (DIR *)NULL) {
#ifdef RISCOS
		syslog(LOG_ERR,"rpc.statd: open backup directory %m");
		syslog(LOG_ERR, "\n");
#else
		perror("rpc.statd: open backup directory");
		fprintf(stderr, "\n");
#endif
		exit(1);
	}
	for (dirp = readdir(dp); dirp != NULL; dirp = readdir(dp)) {
		if (strcmp(dirp->d_name, ".") != 0  &&
		strcmp(dirp->d_name, "..") != 0) {
		/* get all entries from BACKUP to recovery_q */
			if (statd_call_statd(dirp->d_name) != 0) {
				(void) insert_name(&recovery_q, dirp->d_name);
			}
			else { /* remove from BACKUP directory */
				(void) strcpy(path, BACKUP);
				(void) strcat(path, "/");
				(void) strcat(path, dirp->d_name);
			if (debug >= 2)
#ifdef RISCOS
				syslog(LOG_ERR,"remove monitor entry %s\n", path);
#else
				printf("remove monitor entry %s\n", path);
#endif
			if (unlink(path) == -1) {
#ifdef RISCOS
				syslog(LOG_ERR,"rpc.statd:");
				syslog(LOG_ERR,"%s %m",path);
				syslog(LOG_ERR, "\n");
#else
				fprintf(stderr,"rpc.statd:");
				perror(path);
				fprintf(stderr, "\n");
#endif
				exit(1);
			}
			}
		}
	}
	closedir(dp);

	/* notify statd */
	if (recovery_q != (name_entry *)NULL)
		(void) alarm(SM_INIT_ALARM);
}

xdr_notify(xdrs, ntfp)
	XDR *xdrs;
	stat_chge *ntfp;
{
	if (!xdr_string(xdrs, &ntfp->name, MAXNAMLEN+1)) {
		return(FALSE);
	}
	if (!xdr_int(xdrs, &ntfp->state)) {
		return(FALSE);
	}
	return(TRUE);
}

statd_call_statd(name)
	char *name;
{
	stat_chge ntf;
	int err;

	ntf.name =hostname;
	ntf.state = LOCAL_STATE;
	if (debug)
#ifdef RISCOS
		syslog(LOG_ERR,"statd_call_statd at %s\n", name);
#else
		printf("statd_call_statd at %s\n", name);
#endif
	if ((err = call_tcp(name, SM_PROG, SM_VERS, SM_NOTIFY, xdr_notify,
 &ntf, xdr_void, NULL, 0)) == (int) RPC_TIMEDOUT || err == (int) RPC_SUCCESS) {
		return(0);
	}
	else {
#ifdef RISCOS
		syslog(LOG_ERR, "rpc.statd: cannot talk to statd at %s\n", name);
#else
		fprintf(stderr, "rpc.statd: cannot talk to statd at %s\n", name);
#endif
		return(-1);
	}
}


sm_try()
{
	name_entry *nl, *next;

	if (debug >= 2)
#ifdef RISCOS
		syslog(LOG_ERR,"enter sm_try: recovery_q = %s\n", recovery_q->name);
#else
		printf("enter sm_try: recovery_q = %s\n", recovery_q->name);
#endif
	next = recovery_q;
	while ((nl = next) != (name_entry *)NULL) {
		next = next->nxt;
		if (statd_call_statd(nl->name) == 0) {
			/* remove entry from recovery_q */ 
			delete_name(&recovery_q, nl->name);
		}
	}
	if (recovery_q != (name_entry *)NULL)
		(void) alarm(SM_INIT_ALARM);
}

char *
xmalloc(len)
	unsigned len;
{
	char *new;

	if ((new = malloc(len)) == 0) {
#ifdef RISCOS
		syslog(LOG_ERR,"rpc.statd: malloc %m");
		syslog(LOG_ERR, "\n");
#else
		perror("rpc.statd: malloc");
		fprintf(stderr, "\n");
#endif
		return ((char *)NULL);
	}
	else {
		bzero(new, len);
		return (new);
	}
}

/*
 * the following two routines are very similar to
 * insert_mon and delete_mon in sm_proc.c, except the structture
 * is different
 */
name_entry *
insert_name(namepp, name)
	name_entry **namepp;
	char *name;
{
	name_entry *new;

	new = (name_entry *) xmalloc(sizeof(struct name_entry)); 
	new->name = xmalloc(strlen(name) + 1);
	(void) strcpy(new->name, name);
	new->nxt = *namepp;
	if (new->nxt != (name_entry *)NULL)
		new->nxt->prev = new;
	*namepp = new; 
	return(new);
}

delete_name(namepp, name)
	name_entry **namepp;
	char *name;
{
	name_entry *nl;

	nl = *namepp;
	while (nl != (name_entry *)NULL) {
		if (strcmp(nl->name, name) == 0) {/*found */
			if (nl->prev != (name_entry *)NULL)
				nl->prev->nxt = nl->nxt;
			else 
				*namepp = nl->nxt;
			if (nl->nxt != (name_entry *)NULL)
				nl->nxt->prev = nl->prev;
			free(nl->name);
			free(nl);
			return;
		}
		nl = nl->nxt;
	}
	return;
}

name_entry *
find_name(namep, name)
	name_entry *namep;
	char *name;
{
	name_entry *nl;

	nl = namep;
	while (nl != (name_entry *)NULL) {
		if (strcmp(nl->name, name) == 0) {
			return(nl);
		}
		nl = nl->nxt;
	}
	return(NULL);
}

record_name(name, op)
	char *name;
	int op;
{
	name_entry *nl;
	int fd;
	char path[MAXNAMLEN];

	if (op == 1) { /* insert */
		if ((nl = find_name(record_q, name)) == (name_entry *)NULL) {
			nl = insert_name(&record_q, name);
			/* make an entry in current directory */
			(void) strcpy(path, CURRENT);
			(void) strcat(path, "/");
			(void) strcat(path, name);
			if (debug >= 2)
#ifdef RISCOS
				syslog(LOG_ERR,"create monitor entry %s\n", path);
#else
				printf("create monitor entry %s\n", path);
#endif
			if ((fd = open(path, O_CREAT, 00200)) == -1){
#ifdef RISCOS
				syslog(LOG_ERR, "rpc.statd: open of \n");
				syslog(LOG_ERR,"%s %m",path);
				syslog(LOG_ERR, "\n");
#else
				fprintf(stderr, "rpc.statd: open of \n");
				perror(path);
				fprintf(stderr, "\n");
#endif
				if (errno != EACCES)
					exit(1);
			}
			else {
				if (debug >= 2)
#ifdef RISCOS
					syslog(LOG_ERR,"%s is created\n", path);
#else
					printf("%s is created\n", path);
#endif
				if (close(fd)) {
#ifdef RISCOS
					syslog(LOG_ERR,"rpc.statd: close %m");
					syslog(LOG_ERR, "\n");
#else
					perror("rpc.statd: close");
					fprintf(stderr, "\n");
#endif
					exit(1);
				}
			}
		}
		nl->count++;
	}
	else { /* delete */
		if ((nl = find_name(record_q, name)) == (name_entry *)NULL) {
			return;
		}
		nl->count--;
		if (nl->count == 0) {
			delete_name(&record_q, name);
		/* remove this entry from current directory */
			(void) strcpy(path, CURRENT);
			(void) strcat(path, "/");
			(void) strcat(path, name);
			if (debug >= 2)
#ifdef RISCOS
				syslog(LOG_ERR,"remove monitor entry %s\n", path);
#else
				printf("remove monitor entry %s\n", path);
#endif
			if (unlink(path) == -1) {
#ifdef RISCOS
				syslog(LOG_ERR,"rpc.statd: unlink of ");
				syslog(LOG_ERR,"%s %m",path);
				syslog(LOG_ERR, "\n");
#else
				fprintf(stderr,"rpc.statd: unlink of ");
				perror(path);
				fprintf(stderr, "\n");
#endif
				exit(1);
			}

		}
	}
	
}

sm_crash()
{
	name_entry *nl, *next;

	if (record_q == (name_entry *)NULL)
		return;
	next = record_q;	/* clean up record queue */
	while ((nl = next) != (name_entry *)NULL) {
		next = next->nxt;
		delete_name(&record_q, nl->name);
	}

	if (recovery_q != (name_entry *)NULL) { /* clean up all onging recovery act*/
		if (debug)
#ifdef RISCOS
			syslog(LOG_ERR,"sm_crash clean up\n");
#else
			printf("sm_crash clean up\n");
#endif
		(void) alarm(0);
		next = recovery_q;
		while ( (nl = next) != (name_entry *)NULL) {
			next = next ->nxt;
			delete_name(&recovery_q, nl->name);
		}
	}
	statd_init();
}
