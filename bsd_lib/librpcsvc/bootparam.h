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
/* $Header: bootparam.h,v 1.2.1.2 90/05/09 14:42:27 wje Exp $ */
/*
 * @(#)bootparam.h 1.3 88/05/08 4.0NFSSRC SMI
 * @(#)bootparam.h 1.2 Copyright (c) 1988 by Sun Microsystems, Inc.
 */

#ifndef KERNEL
#include <rpc/types.h>
#ifdef SYSTYPE_BSD43
#include <sys/errno.h>
#define bsd43_bool_t bool_t
#include <nfs/nfs.h>
#endif
#ifdef SYSTYPE_SYSV
#include <errno.h>
#include <sys/fs/nfs.h>
#endif
#endif  /* !KERNEL */
#define	MAX_MACHINE_NAME 255
#define	MAX_PATH_LEN	1024
#define	MAX_FILEID	32
#define	IP_ADDR_TYPE	1

typedef char *bp_machine_name_t;


typedef char *bp_path_t;


typedef char *bp_fileid_t;


struct ip_addr_t {
	char net;
	char host;
	char lh;
	char impno;
};
typedef struct ip_addr_t ip_addr_t;


struct bp_address {
	int address_type;
	union {
		ip_addr_t ip_addr;
	} bp_address;
};
typedef struct bp_address bp_address;


struct bp_whoami_arg {
	bp_address client_address;
};
typedef struct bp_whoami_arg bp_whoami_arg;


struct bp_whoami_res {
	bp_machine_name_t client_name;
	bp_machine_name_t domain_name;
	bp_address router_address;
};
typedef struct bp_whoami_res bp_whoami_res;


struct bp_getfile_arg {
	bp_machine_name_t client_name;
	bp_fileid_t file_id;
};
typedef struct bp_getfile_arg bp_getfile_arg;


struct bp_getfile_res {
	bp_machine_name_t server_name;
	bp_address server_address;
	bp_path_t server_path;
};
typedef struct bp_getfile_res bp_getfile_res;


#define BOOTPARAMPROG 100026
#define BOOTPARAMVERS 1
#define BOOTPARAMPROC_WHOAMI 1
#define BOOTPARAMPROC_GETFILE 2

bool_t xdr_bp_machine_name_t();
bool_t xdr_bp_path_t();
bool_t xdr_bp_fileid_t();
bool_t xdr_ip_addr_t();
bool_t xdr_bp_address();
bool_t xdr_bp_whoami_arg();
bool_t xdr_bp_whoami_res();
bool_t xdr_bp_getfile_arg();
bool_t xdr_bp_getfile_res();
