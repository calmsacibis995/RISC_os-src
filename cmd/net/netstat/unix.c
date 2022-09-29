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
#ident	"$Header: unix.c,v 1.4.1.2 90/05/09 17:14:47 wje Exp $"

/*
 * Display protocol blocks in the unix domain.
 */
#include <sys/param.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/mbuf.h>
#include <sys/un.h>
#include <sys/unpcb.h>

#ifdef RISCOS
#include <sysv/sys/var.h>   /* System Configuration information structure */
#define KERNEL
#include <sys/file.h> 
#include <ufs/inode.h>
#else RISCOS
#define	KERNEL
#include <sys/file.h>
#endif RISCOS

int	Aflag, kmem;

#ifdef RISCOS
unixpr (vaddr, fileaddr, unixsw)
	off_t vaddr, fileaddr;
	struct protosw *unixsw;
#else RISCOS
unixpr (nfileaddr, fileaddr, unixsw)
	off_t nfileaddr, fileaddr;
	struct protosw *unixsw;
#endif RISCOS
{
	register struct file *fp;
#ifdef RISCOS
	struct inode inodeBlk, *inode_p = &inodeBlk;
	struct var v;
	int	nfile;
	struct file *file;
	struct file *fileNFILE;
#else RISCOS
	struct file *filep;
#endif RISCOS
	struct socket sock, *so = &sock;

#ifdef RISCOS
	if (vaddr == 0) {
		printf("v address not in namelist.\n");
		return;
	}
	if (fileaddr == 0) {
		printf("file not in namelist.\n");
		return;
	}
	klseek(kmem, vaddr, L_SET);
	if (read(kmem, &v, sizeof (v)) != sizeof (v)) {
		printf("v structure: bad read.\n");
		return;
	}
	nfile = v.v_file;
	if (nfile == 0) {
#define MIPS_DEBUG 1
#if MIPS_DEBUG
		printf("v.v_file kernel value zero\n");
#endif MIPS_DEBUG
		return;
	}
#else RISCOS
	if (nfileaddr == 0 || fileaddr == 0) {
		printf("nfile or file not in namelist.\n");
		return;
	}
	klseek(kmem, nfileaddr, L_SET);
	if (read(kmem, &nfile, sizeof (nfile)) != sizeof (nfile)) {
		printf("nfile: bad read.\n");
		return;
	}
#endif RISCOS
	file = (struct file *)calloc(nfile, sizeof (struct file));
	if (file == (struct file *)0) {
		printf("Out of memory (file table).\n");
		return;
	}

	klseek(kmem, fileaddr, L_SET);
#ifndef RISCOS
	if (read(kmem, &filep, sizeof (filep)) != sizeof (filep)) {
		printf("File table address, bad read.\n");
		return;
	}
	klseek(kmem, (off_t)filep, L_SET);
#endif RISCOS
	if (read(kmem, file, nfile * sizeof (struct file)) !=
	    nfile * sizeof (struct file)) {
		printf("File table read error.\n");
		return;
	}

	fileNFILE = file + nfile;
	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_count == 0 || fp->f_type != DTYPE_SOCKET)
		  continue;
		klseek(kmem, fp->f_data, L_SET);
		if (read(kmem, so, sizeof (*so)) != sizeof (*so)) continue;
		/* kludge */
		if (so->so_proto >= unixsw && so->so_proto <= unixsw + 2)
			if (so->so_pcb)
				unixdomainpr(so, fp->f_data);
	}
	free((char *)file);
}

static	char *socktype[] =
    { "#0", "stream", "dgram", "raw", "rdm", "seqpacket" };

unixdomainpr(so, soaddr)
	register struct socket *so;
	caddr_t soaddr;
{
	struct unpcb unpcb, *unp = &unpcb;
	struct mbuf mbuf, *m;
	struct sockaddr_un *sa;
	static int first = 1;

	klseek(kmem, so->so_pcb, L_SET);
	if (read(kmem, unp, sizeof (*unp)) != sizeof (*unp))
		return;
	if (unp->unp_addr) {
		m = &mbuf;
		klseek(kmem, unp->unp_addr, L_SET);
		if (read(kmem, m, sizeof (*m)) != sizeof (*m))
			m = (struct mbuf *)0;
		sa = mtod(m, struct sockaddr_un *);
	} else
		m = (struct mbuf *)0;
	if (first) {
		printf("Active UNIX domain sockets\n");
		printf(
"%-8.8s %-6.6s %-6.6s %-6.6s %8.8s %8.8s %8.8s %8.8s Addr\n",
		    "Address", "Type", "Recv-Q", "Send-Q",
		    "Vnode", "Conn", "Refs", "Nextref");
		first = 0;
	}
	printf("%8x %-6.6s %6d %6d %8x %8x %8x %8x",
	    soaddr, socktype[so->so_type], so->so_rcv.sb_cc, so->so_snd.sb_cc,
	    unp->unp_vnode, unp->unp_conn,
	    unp->unp_refs, unp->unp_nextref);
	if (m)
		printf(" %.*s", m->m_len - sizeof(sa->sun_family),
		    sa->sun_path);
	putchar('\n');
}
