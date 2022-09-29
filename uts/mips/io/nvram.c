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
#ident	"$Header: nvram.c,v 1.6.1.2 90/05/10 05:24:17 wje Exp $"
/*
 * $Header: nvram.c,v 1.6.1.2 90/05/10 05:24:17 wje Exp $
 *
 * For RS2030/RC2030 - use environment passed to kernel rather
 * than reading nvram in the IOP.  
 */
#include "sys/param.h"
#include "sys/types.h"
#include "sys/sbd.h"
#include "sys/iop.h"
#include "sys/nvram.h"
#include "sys/cpu_board.h"


#define MAXENVIRON 100
#define MAXENVSTRING	32
#define NETADDR	"netaddr="
#define LBAUD	"lbaud="
#define RBAUD	"rbaud="
#define BOOTFILE	"bootfile="
#define BOOTMODE	"bootmode="
#define CONSOLE	"console="
#define USEBOOTPARAMS	"use_bootparams="

char env_netaddr[MAXENVSTRING];
char env_lbaud[MAXENVSTRING];
char env_rbaud[MAXENVSTRING];
char env_bootfile[MAXENVSTRING];
char env_bootmode[MAXENVSTRING];
char env_console[MAXENVSTRING];
char env_usebootparams[MAXENVSTRING];


#define EMPTYSTRING ""

char *
prom_getenv(envfield)
char *envfield;
{
	register char *env_addr;

	if ( strcmp(envfield,"netaddr") == 0 ) {
		env_addr = env_netaddr;;
	} else if ( strcmp(envfield,"lbaud") == 0 ) {
		env_addr = env_lbaud;
	} else if ( strcmp(envfield,"rbaud") == 0 ) {
		env_addr = env_rbaud;
	} else if ( strcmp(envfield,"bootfile") == 0 ) {
		env_addr = env_bootfile;
	} else if ( strcmp(envfield,"bootmode") == 0 ) {
		env_addr = env_bootmode;
	} else if ( strcmp(envfield,"console") == 0 ) {
		env_addr = env_console;
	} else if ( strcmp(envfield,"use_bootparams") == 0 ) {
		env_addr = env_usebootparams;
	} else {
		env_addr = EMPTYSTRING;
	}
	return(env_addr);
}
prom_setenv(nvfield,nvvalue)
char *nvfield, *nvvalue;
{
	return(0);		/* not supported in R?2030 */
}

environ_save(environ)
register char *environ[];
{
	register unsigned i, n;

	for ( i=0; environ[i] && i<MAXENVIRON; i++ ) {
		if ( strncmp(NETADDR,environ[i],(n=strlen(NETADDR))) == 0 ) {
			strncpy(env_netaddr,(environ[i]+n),MAXENVSTRING);
		} else if ( strncmp(LBAUD,environ[i],(n=strlen(LBAUD))) == 0 ) {
			strncpy(env_lbaud,(environ[i]+n),MAXENVSTRING);
		} else if ( strncmp(RBAUD,environ[i],(n=strlen(RBAUD))) == 0 ) {
			strncpy(env_rbaud,(environ[i]+n),MAXENVSTRING);
		} else if ( strncmp(BOOTFILE,environ[i],(n=strlen(BOOTFILE))) 
									== 0 ) {
			strncpy(env_bootfile,(environ[i]+n),MAXENVSTRING);
		} else if ( strncmp(BOOTMODE,environ[i],(n=strlen(BOOTMODE))) 
									== 0 ) {
			strncpy(env_bootmode,(environ[i]+n),MAXENVSTRING);
		} else if ( strncmp(CONSOLE,environ[i],(n=strlen(CONSOLE))) 
									== 0 ) {
			strncpy(env_console,(environ[i]+n),MAXENVSTRING);
		} else if ( strncmp(USEBOOTPARAMS,environ[i],
					(n=strlen(USEBOOTPARAMS))) == 0 ) {
			strncpy(env_usebootparams,(environ[i]+n),MAXENVSTRING);
		}
	}
}
