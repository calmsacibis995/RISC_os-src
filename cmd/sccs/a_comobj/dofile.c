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
#ident	"$Header: dofile.c,v 1.7.2.2 90/05/09 18:40:53 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	"defines.h"
# include	<dirent.h>


int	nfiles;
char	had_dir;
char	had_standinp;


do_file(p,func)
register char *p;
int (*func)();
{
	extern char *Ffile;
	char str[FILESIZE];
	char ibuf[FILESIZE];
	char dbuf[BUFSIZ];
	DIR *dirf;
	struct dirent *dp;

	if (p[0] == '-') {
		had_standinp = 1;
		while (gets(ibuf) != NULL) {
			if (exists(ibuf) && (Statbuf.st_mode & S_IFMT) == S_IFDIR) {
				had_dir = 1;
				Ffile = ibuf;
				if((dirf = opendir(ibuf)) == NULL)
					return;
				while(dp = readdir(dirf)) {
					if ( strcmp(dp->d_name, ".") == 0 
					||   strcmp(dp->d_name, "..") == 0)
						continue;
					sprintf(str,"%s/%s",ibuf,dp->d_name);
					if(sccsfile(str)) {
						Ffile = str;
						(*func)(str);
						nfiles++;
					}
				}
				closedir(dirf);
			}
			else if (sccsfile(ibuf)) {
				Ffile = ibuf;
				(*func)(ibuf);
				nfiles++;
			}
		}
	}
	else if (exists(p) && (Statbuf.st_mode & S_IFMT) == S_IFDIR) {
		had_dir = 1;
		Ffile = p;
		if((dirf = opendir(p)) == NULL)
			return;
		while(dp = readdir(dirf)) {
			if ( strcmp(dp->d_name, ".") == 0 
			||   strcmp(dp->d_name, "..") == 0)
				continue;
			sprintf(str,"%s/%s",p,dp->d_name);
			if(sccsfile(str)) {
				Ffile = str;
				(*func)(str);
				nfiles++;
			}
		}
		closedir(dirf);
	}
	else {
		Ffile = p;
		(*func)(p);
		nfiles++;
	}
}
