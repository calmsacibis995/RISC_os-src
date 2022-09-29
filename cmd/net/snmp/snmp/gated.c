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
#ident	"$Header: gated.c,v 1.2.1.2 90/05/09 17:27:50 wje Exp $"
#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include "../../gated/snmp.h"

struct snmpmsg * get_gated_info (msg)
struct snmpmsg * msg;

{
	int client_pipe, server_pipe;
	int cnt;
	struct timeval timeout;
	fd_set fds;

	if ((server_pipe = open (ServerPipeName, O_RDWR)) < 0) {
		msg->type = SNMP_ERROR;
		return ((struct snmpmsg *) msg);
	}

	if ((client_pipe = open (ClientPipeName, O_RDWR)) < 0) {
		msg->type = SNMP_ERROR;
		return ((struct snmpmsg *) msg);
	}

	if ((cnt = write (server_pipe, msg, sizeof (struct snmpmsg))) <= 0) {
		(void) close (server_pipe);
		(void) close (client_pipe);
		msg->type = SNMP_ERROR;
		return ((struct snmpmsg *) msg);
	}

	timeout.tv_sec = 0;
	timeout.tv_usec = 1000;

	FD_ZERO (&fds);
	FD_SET (client_pipe, &fds);

	if (select (client_pipe + 1, &fds, 0, 0, &timeout) == 0) {
		(void) close (server_pipe);
		(void) close (client_pipe);
		msg->type = SNMP_ERROR;
		return ((struct snmpmsg *) msg);
	}

	if ((cnt = read (client_pipe, msg, sizeof (struct snmpmsg))) <= 0) {
		(void) close (server_pipe);
		(void) close (client_pipe);
		msg->type = SNMP_ERROR;
		return ((struct snmpmsg *) msg);
	}

	close (server_pipe);
	close (client_pipe);

	return (msg);
}
