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
#ident	"$Header: nfs_vfsops.c,v 1.6.1.9 90/05/22 18:28:41 wje Exp $"

/* @(#)nfs_vfsops.c 1.5 88/08/04 NFSSRC4.0 from 2.84 88/03/21 SMI
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 */

#include "sys/types.h"
#include "sys/errno.h"
#include "sys/param.h"
#include "sys/systm.h"

#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/debug.h"

#include "sys/vfs.h"
#include "sys/vfs_stat.h"
#include "sys/vnode.h"
#include "sys/pathname.h"
#include "sys/uio.h"
#include "sys/socket.h"
#include "sys/socketvar.h"
       /* #include "kernel.h" Original includes => NOT required in risc/os */
#include "sys/utsname.h"
#include "bsd/netinet/in.h"

#include "../rpc/types.h"
#include "../rpc/xdr.h"
#include "../rpc/auth.h"
#include "../rpc/clnt.h"

#include "../rpc/pmap_rmt.h"
#include "../rpc/pmap_prot.h"
#include "../librpcsvc/bootparam.h"

#include "sys/fs/nfs.h"
#include "sys/fs/nfs_clnt.h"
#include "sys/fs/nfs_rnode.h"

#ifndef NFSCLIENT
#define NFSCLIENT 1 /* To get nfs_args structure definition */
#endif NFSCLIENT
#include "sys/mount.h"
#include "sys/kmem.h"

#include "bsd/net/if.h"
#include "bsd/net/route.h"

#include "../librpcsvc/mount.h"
#include "sys/bootconf.h"
#include "sys/ctype.h"
#include "sys/cmn_err.h"

#include "bsd/net/if.h"
#include "bsd/net/soioctl.h"           /* Original "ioctl.h" */

#define	satosin(sa)	((struct sockaddr_in *)(sa))

#ifdef NFSDEBUG
extern int nfsdebug;
#endif

int nfsrootvp();
extern struct vnode *makenfsnode();
extern short	nfs_majors[];		/* Major device numbers for NFS */
extern	int	nfs_maxmajors;		/* Max major number for NFS */
extern	char	nfs_minmap[];		/* Map for minor device allocation */

/*
 * nfs vfs operations.
 */
int nfs_mount();
int nfs_unmount();
int nfs_root();
int nfs_statfs();
int nfs_sync();
int nfs_mountroot();
int nfs_swapvp();
int nfs_badop();

struct vfsops nfs_vfsops = {
	nfs_mount,
	nfs_unmount,
	nfs_root,
	nfs_statfs,
	nfs_sync,
	nfs_badop,
	nfs_mountroot,
	nfs_swapvp,
};

/*
 * Called by vfs_mountroot when nfs is going to be mounted as root
 */
int
#if RISCOS
nfs_mountroot(vfsp, vpp, name, op)
	struct vfs *vfsp;
	struct vnode **vpp;
	char *name;
	mountrootop_t op;
#else
nfs_mountroot(vfsp, vpp, name)
	struct vfs *vfsp;
	struct vnode **vpp;
	char *name;
#endif
{
	extern struct sockaddr_in root_sin;
	struct vnode *rtvp;
	extern char *root_path;
	extern char root_hostname[MAXHOSTNAMELEN+1];
	fhandle_t root_fhandle;
	int error, rc;
	struct pathname pn;

	/* do this BEFORE getfile which causes */
	/* xid stamps to be initialized */

	if ( op == ROOT_MOUNT ) {
#ifdef sun
		getfsname("root", name);
#endif
#if RISCOS
		error = vfs_lock(vfsp);
		if (error)
			return error;
		pn.pn_path = NULL;
#endif /* RISCOS */
		if ( root_path == NULL ) {
			inittodr(-1L);    /* hack now-until we get time svc? */
			pn_alloc(&pn);
			root_path = pn.pn_path;
			do {
#ifdef sun
				if (*name) {
					rc = getfile(name, root_hostname,
					    (struct sockaddr *)&root_sin, 
								root_path);
				} else {
					rc = getfile("root", root_hostname,
					    (struct sockaddr *)&root_sin, 
								root_path);
				}
#else
				rc = getfile("root", root_hostname,
				    (struct sockaddr *)&root_sin, root_path);
#endif
			} while (rc == ETIMEDOUT);
			if (rc) {
#if RISCOS
				vfs_unlock(vfsp);
#endif /* RISCOS */
				if ( pn.pn_path != NULL ) {
					pn_free(&pn);
					root_path = NULL;
				}
				return (rc);
			}
		}
		rc = mountnfs(&root_sin, root_hostname, 
					root_path, &root_fhandle);
		if (rc) {
#if RISCOS
			vfs_unlock(vfsp);
#endif /* RISCOS */
			printf("mount root %s:%s failed, rpc status %d\n",
				root_hostname, root_path, rc);
			if ( pn.pn_path != NULL ) {
				pn_free(&pn);
				root_path = NULL;
			}
			return (rc);
		}

		rc = nfsrootvp(&rtvp, vfsp, &root_sin, 
					&root_fhandle, root_hostname,
		    (char *)NULL, -1, 0);
		if (rc) {
#if RISCOS
			vfs_unlock(vfsp);
#endif /* RISCOS */
			if ( pn.pn_path != NULL ) {
				pn_free(&pn);
				root_path = NULL;
			}
			return (rc);
		}
		if (rc = vfs_add((struct vnode *)0, vfsp, 0)) {
#if RISCOS
			vfs_unlock(vfsp);
#endif /* RISCOS */
			if ( pn.pn_path != NULL ) {
				pn_free(&pn);
				root_path = NULL;
			}
			return (rc);
		}
		vftomi(vfsp)->mi_acregmax = 100 * ACREGMAX;
		vftomi(vfsp)->mi_acdirmax = 100 * ACDIRMAX;
		vfs_unlock(rtvp->v_vfsp);
		*vpp = rtvp;
		{
			register char *nm;
			extern char *strcpy();

			nm = strcpy(name, root_hostname);
			*nm++ = ':';
			(void) strcpy(nm, root_path);
		}

		if ( pn.pn_path != NULL ) {
			pn_free(&pn);
			root_path = NULL;
		}
		return (0);
	} else if ( op == ROOT_REMOUNT ) {
		return(0);
	} else if ( op == ROOT_UNMOUNT ) {
		return(0);
	}
}

extern struct sockaddr_in nfsdump_sin;
extern fhandle_t nfsdump_fhandle;
extern int nfsdump_maxcount;

/*
 * Set up for swapping to NFS.
 * Call nfsrootvp to set up the
 * RPC/NFS machinery.
 */
int
nfs_swapvp(vfsp, vpp, path, nswap_ptr)
	struct vfs *vfsp;
	struct vnode **vpp;
	char *path;
	int *nswap_ptr;
{
	extern struct sockaddr_in swap_sin;
	extern char *swap_path;
	extern char swap_hostname[MAXHOSTNAMELEN + 1];
	fhandle_t swap_fhandle;
	extern char *dump_path;
	extern char dump_hostname[MAXHOSTNAMELEN + 1];
	int rc;
	struct vnode *xxvp;
	struct vattr va;
	struct pathname pn;

	if ( swap_path == NULL ) {
		pn.pn_path = NULL;
		pn_alloc(&pn);
		swap_path = pn.pn_path;
#ifdef sun
		getfsname("swap", path);
#endif
		do {
#ifdef sun
			if (*path) {
				rc = getfile(path, swap_hostname,
				    (struct sockaddr *)&swap_sin, swap_path);
			} else {
				rc = getfile("swap", swap_hostname,
				    (struct sockaddr *)&swap_sin, swap_path);
			}
#else
			rc = getfile("swap", swap_hostname,
			    (struct sockaddr *)&swap_sin, swap_path);
#endif
		
		} while (rc == ETIMEDOUT);
		if (rc) {
			if ( pn.pn_path != NULL ) {
				pn_free(&pn);
				swap_path = NULL;
			}
			return (rc);
		}
	}
	rc = mountnfs(&swap_sin, swap_hostname, swap_path, &swap_fhandle);
	if (rc) {
		printf("mount swapfile %s:%s failed, rpc status %d\n",
			swap_hostname, swap_path, rc);
		if ( pn.pn_path != NULL ) {
			pn_free(&pn);
			swap_path = NULL;
		}
		return (rc);
	}
	rc = nfsrootvp(&xxvp, vfsp, &swap_sin, &swap_fhandle,
	    swap_hostname, (char *)NULL, -1, 0);
	if (rc) {
		if ( pn.pn_path != NULL ) {
			pn_free(&pn);
			swap_path = NULL;
		}
		return (rc);
	}
	*vpp = xxvp;
#ifdef sun
	hostpath(path, swap_hostname, swap_path);
#endif
	if ( pn.pn_path != NULL ) {
		pn_free(&pn);
		swap_path = NULL;
	}
	(void)VOP_GETATTR(*vpp, &va, u.u_cred);
	*nswap_ptr = va.va_size / DEV_BSIZE;
	nfsdump_maxcount = va.va_size;
	nfsdump_fhandle = swap_fhandle;
	nfsdump_sin = swap_sin;
	dumpdev = -1;		/* indicates nfs dump file */

	/*
	 * While we're at it, configure dump too.  We need to be sure to
	 * call VOP_GETATTR for the dump file or else it'll never get done.
	 */
	pn.pn_path = NULL;
	if ( dump_path == NULL ) {
		pn_alloc(&pn);
		dump_path = pn.pn_path;
		do {
#ifdef sun
			if (*path) {
				rc = getfile(path, dump_hostname,
				    (struct sockaddr *)&nfsdump_sin, dump_path);
			} else {
				rc = getfile("dump", dump_hostname,
				    (struct sockaddr *)&nfsdump_sin, dump_path);
			}
#else
			rc = getfile("dump", dump_hostname,
			    (struct sockaddr *)&nfsdump_sin, dump_path);
#endif
		
		} while (rc == ETIMEDOUT);
		if (rc) {
			if ( pn.pn_path != NULL ) {
				pn_free(&pn);
				dump_path = NULL;
			}
			return (rc);
		}
	}
	if (dump_path != NULL) {
		rc = mountnfs(&nfsdump_sin, dump_hostname, dump_path,
		    					&nfsdump_fhandle);
		if (rc) {
			printf("mount dumpfile %s:%s failed, rpc status %d\n",
				dump_hostname, dump_path, rc);
		} else {
			struct vfs *dumpvfsp;
			dumpvfsp = (struct vfs *)kmem_alloc(sizeof (*vfsp));
			VFS_INIT(dumpvfsp, &nfs_vfsops, (caddr_t)0);
			rc = nfsrootvp(&dumpfile.bo_vp, dumpvfsp, &nfsdump_sin,
			    &nfsdump_fhandle, dump_hostname, (char *)NULL, -1,
			    0);
			if (rc == 0) {
				(void) strcpy(dumpfile.bo_fstype, "nfs");
				hostpath(dumpfile.bo_name, dump_hostname,
				    dump_path);
				(void) VOP_GETATTR(dumpfile.bo_vp, &va,
				    u.u_cred);
				dumplo = 0;
				nfsdump_maxcount = 0;
			} else {
				kmem_free((char *)dumpvfsp, sizeof (*vfsp));
			}
		      }
	      }
	if ( pn.pn_path != NULL ) {
		pn_free(&pn);
	}
	return (0);
      }

/*
 * pmapper remote-call-service interface.
 * This routine is used to call the pmapper remote call service
 * which will look up a service program in the port maps, and then
 * remotely call that routine with the given parameters.  This allows
 * programs to do a lookup and call in one step.
*/
enum clnt_stat
pmap_rmtcall(call_addr, progn, versn, procn, xdrargs, argsp, xdrres, resp, tout, resp_addr)
	struct sockaddr_in *call_addr;
	u_long progn, versn, procn;
	xdrproc_t xdrargs, xdrres;
	caddr_t argsp, resp;
	struct timeval tout;
	struct sockaddr_in *resp_addr;
{
	register CLIENT *client;
	struct rmtcallargs a;
	struct rmtcallres r;
	enum clnt_stat stat, clntkudp_callit_addr();
	u_long port;

	call_addr->sin_port = htons(PMAPPORT);
	client = clntkudp_create(call_addr, PMAPPROG, PMAPVERS, 5, u.u_cred);
	if (client != (CLIENT *)NULL) {
		a.prog = progn;
		a.vers = versn;
		a.proc = procn;
		a.args_ptr = argsp;
		a.xdr_args = xdrargs;
		r.port_ptr = &port;
		r.results_ptr = resp;
		r.xdr_results = xdrres;
		stat = clntkudp_callit_addr(client, PMAPPROC_CALLIT,
		    xdr_rmtcall_args, (caddr_t)&a,
		    xdr_rmtcallres, (caddr_t) &r, tout, resp_addr);
		resp_addr->sin_port = htons((u_short) port);
		CLNT_DESTROY(client);
	} else {
		panic("pmap_rmtcall: clntkudp_create failed");
	}
	return (stat);
}

struct ifnet *ifb_ifwithaf();

static struct sockaddr_in bootparam_addr;
struct in_addr gblipaddr;

int
whoami()
{
	struct sockaddr_in sa;
	struct ifnet *ifp;
	struct bp_whoami_arg arg;
	struct bp_whoami_res res;
	struct timeval tv;
	enum clnt_stat status;
	struct rtentry rtentry;
	struct sockaddr_in *sin;
	struct in_addr ipaddr;
	struct ifreq req;
	int stat;
	enum clnt_stat callrpc();

	bzero((caddr_t)&sa, sizeof sa);
	ifp = ifb_ifwithaf(AF_INET);
	if (ifp == 0) {
		printf("whoami: zero ifp\n");
		return (EHOSTUNREACH);
	}
	try_netaddr(ifp);		/* see if netaddr is passed in */
	if (!address_known(ifp)) {
		revarp_myaddr(ifp);
	}

#ifdef notdef
	sa = *((struct sockaddr_in *)&(ifp->if_broadaddr));
#else
	/*
	 * Pick up the interface broadcast address.
	 */
	if ((stat = in_control((struct socket *)0, SIOCGIFBRDADDR,
	    (caddr_t)&req, ifp)) != 0) {
		printf("whoami: in_control 0x%x if_flags 0x%x\n",
			stat, ifp->if_flags);
		panic("bad SIOCGIFBRDADDR in_control");
	}
	bcopy((caddr_t)&req.ifr_dstaddr, (caddr_t)&sa,
	    sizeof (struct sockaddr_in));
#endif notdef

	arg.client_address.address_type = IP_ADDR_TYPE;

#ifdef notdef
	ipaddr = ((struct sockaddr_in *)&ifp->if_addr)->sin_addr;
#else
	if (in_control((struct socket *)0, SIOCGIFADDR, (caddr_t)&req,
	    ifp) != 0)
		panic("bad SIOCGIFADDR in_control");
	ipaddr = (satosin (&req.ifr_addr)->sin_addr);
	gblipaddr = ipaddr;
#endif notdef

	bcopy((caddr_t)&ipaddr, (caddr_t)&arg.client_address.bp_address.ip_addr,
	    sizeof (struct in_addr));

	tv.tv_sec = 3;
	tv.tv_usec = 0;
	res.client_name = (bp_machine_name_t)kmem_alloc(MAX_MACHINE_NAME + 1);
	if (res.client_name == NULL) {
		return (ENOMEM);
	}
	res.domain_name = (bp_machine_name_t)kmem_alloc(MAX_MACHINE_NAME + 1);
	if (res.domain_name == NULL) {
		return (ENOMEM);
	}
	for (;;) {
		int msg = 0;

		status = pmap_rmtcall(&sa, (u_long)BOOTPARAMPROG,
		    (u_long)BOOTPARAMVERS, (u_long)BOOTPARAMPROC_WHOAMI,
		    xdr_bp_whoami_arg, (caddr_t)&arg,
		    xdr_bp_whoami_res, (caddr_t)&res,
		    tv, &bootparam_addr);
		if (status == RPC_TIMEDOUT) {
#ifdef RISCOS
			/* add retry exponential backoff code */
			if (tv.tv_sec <= 60) {
				tv.tv_sec = tv.tv_sec << 1;
				continue;
			}
#endif
			if (msg == 0) {
				printf("%s%s\n", "No bootparam server",
				    " responding; still trying");
				printf("whoami: pmap_rmtcall status 0x%x\n",
				    status);
				msg = 1;
			}
			continue;
		} else {
			if (msg) {
				printf("%s%s\n", "Bootparam response",
					" received");
			}
			break;
		}

	}
	if (status != RPC_SUCCESS) {
		printf("whoami RPC call failed with status %d\n", status);
		/*
		 * XXX should get real error here
		 */
		return ((int)status);
	}

	hostnamelen = strlen(res.client_name);
#ifdef RISCOS
	if (hostnamelen > MAXHOSTNAMELEN) {
#else
	if (hostnamelen > sizeof hostname) {
#endif
		printf("whoami: hostname too long");
		return (ENAMETOOLONG);
	}
	if (hostnamelen > 0) {
		bcopy((caddr_t)res.client_name, (caddr_t)hostname,
		    (u_int)hostnamelen);
	} else {
		printf("whoami: no host name\n");
		return (ENXIO);
	}
	kmem_free(res.client_name, MAX_MACHINE_NAME);
	printf("hostname: %s\n", hostname);

	domainnamelen = strlen(res.domain_name);
#ifdef RISCOS
	if (domainnamelen > MAXHOSTNAMELEN) {
#else
	if (domainnamelen > sizeof domainname) {
#endif
		printf("whoami: domainname too long");
		return (ENAMETOOLONG);
	}
	if (domainnamelen > 0) {
		bcopy((caddr_t)res.domain_name, (caddr_t)domainname,
		    (u_int)domainnamelen);
		printf("domainname: %s\n", domainname);
	} else {
		printf("whoami: no domain name\n");
	}
	kmem_free(res.domain_name, MAX_MACHINE_NAME);

	bcopy((caddr_t)&res.router_address.bp_address.ip_addr, (caddr_t)&ipaddr,
	    sizeof (struct in_addr));
	if (ipaddr.s_addr != (u_long) 0) {
		if (res.router_address.address_type == IP_ADDR_TYPE) {
			sin = (struct sockaddr_in *)&rtentry.rt_dst;
			bzero((caddr_t)sin, sizeof *sin);
			sin->sin_family = AF_INET;
			sin = (struct sockaddr_in *)&rtentry.rt_gateway;
			bzero((caddr_t)sin, sizeof *sin);
			sin->sin_family = AF_INET;
			sin->sin_addr.s_addr = ipaddr.s_addr;
			rtentry.rt_flags = RTF_GATEWAY | RTF_UP;
			(void) rtrequest(SIOCADDRT, &rtentry);
		} else {
			printf("whoami: unknown gateway addr family %d\n",
			    res.router_address.address_type);
		}
	}
	return (0);
}

enum clnt_stat
callrpc(sin, prognum, versnum, procnum, inproc, in, outproc, out)
	struct sockaddr_in *sin;
	u_long prognum, versnum, procnum;
	xdrproc_t inproc, outproc;
	char *in, *out;
{
	CLIENT *cl;
	struct timeval tv;
	enum clnt_stat cl_stat;

	cl = clntkudp_create(sin, prognum, versnum, 5, u.u_cred);
	tv.tv_sec = 3;
	tv.tv_usec = 0;
	cl_stat = CLNT_CALL(cl, procnum, inproc, in, outproc, out, tv);
	AUTH_DESTROY(cl->cl_auth);
	CLNT_DESTROY(cl);
	return (cl_stat);
}

int
getfile(fileid, server_name, server_address, server_path)
	char *fileid;
	char *server_name;
	struct sockaddr *server_address;
	char *server_path;
{
	struct bp_getfile_arg arg;
	struct bp_getfile_res res;
	enum clnt_stat status;
	int tries;
	int error;
	struct in_addr ipaddr;

	arg.client_name = hostname;
	arg.file_id = fileid;
	bzero((caddr_t)&res, sizeof res);
	if (bootparam_addr.sin_addr.s_addr == 0) {
		error = whoami();
		if (error) {
			return (error);
		}
	}
	res.server_name = (bp_machine_name_t) kmem_alloc(MAX_MACHINE_NAME + 1);
	res.server_path = (bp_machine_name_t) kmem_alloc(MAX_MACHINE_NAME + 1);
	if (res.server_name == NULL || res.server_path == NULL) {
		error = ENOMEM;
		goto bad;
	}
	for (tries = 0; tries < 5; tries++) {
		status = callrpc(&bootparam_addr, (u_long)BOOTPARAMPROG,
		    (u_long)BOOTPARAMVERS, (u_long)BOOTPARAMPROC_GETFILE,
		    xdr_bp_getfile_arg, (caddr_t)&arg,
		    xdr_bp_getfile_res, (caddr_t)&res);
		if (status == RPC_TIMEDOUT) {
			continue;
		} else {
			break;
		}
	}
	if (status != RPC_SUCCESS) {
		if (status == RPC_TIMEDOUT) {
			error = ETIMEDOUT;
		} else {
			error = (int)status;
		}
		goto bad;
	}
	(void) strcpy(server_name, res.server_name);
	(void) strcpy(server_path, res.server_path);
	kmem_free(res.server_name, MAX_MACHINE_NAME + 1);
	kmem_free(res.server_path, MAX_MACHINE_NAME + 1);
	bcopy((caddr_t)&res.server_address.bp_address.ip_addr, (caddr_t)&ipaddr,
	    sizeof (struct in_addr));

	if (*server_name == '\0' || *server_path == '\0' ||
	    ipaddr.s_addr == (u_long) 0) {
		return (EINVAL);
	}
	switch (res.server_address.address_type) {
	case IP_ADDR_TYPE:
		bzero((caddr_t)server_address, sizeof *server_address);
		server_address->sa_family = AF_INET;
		satosin(server_address)->sin_addr.s_addr = ipaddr.s_addr;
		break;
	default:
		printf("getfile: unknown address type %d\n",
			res.server_address.address_type);
		return (EPROTONOSUPPORT);
	}
	return (0);
bad:
	if (res.server_name) {
		kmem_free(res.server_name, MAX_MACHINE_NAME + 1);
	}
	if (res.server_path) {
		kmem_free(res.server_path, MAX_MACHINE_NAME + 1);
	}
	return (error);
}

/*
 * Call mount daemon on server sin to mount path.
 * sin_port is set to nfs port and fh is the fhandle
 * returned from the server.
 */

mountnfs(sin, server, path, fh)
	struct sockaddr_in *sin;
	char *server;
	char *path;
	fhandle_t *fh;
{
	struct fhstatus fhs;
	int error;
	enum clnt_stat status;

	do {
		error = pmap_kgetport(sin, (u_long)MOUNTPROG,
		    (u_long)MOUNTVERS, (u_long)IPPROTO_UDP);
		if (error == -1) {
			return ((int)RPC_PROGNOTREGISTERED);
		} else if (error == 1) {
			printf("mountnfs: %s:%s portmap not responding\n",
			    server, path);
		}
	} while (error == 1);
	do {
		status = callrpc(sin, (u_long)MOUNTPROG, (u_long)MOUNTVERS,
		    (u_long)MOUNTPROC_MNT,
		    xdr_bp_path_t, (caddr_t)&path,
		    xdr_fhstatus, (caddr_t)&fhs);
		if (status == RPC_TIMEDOUT) {
			printf("mountnfs: %s:%s mount server not responding\n",
			    server, path);
		}
	} while (status == RPC_TIMEDOUT);
	if (status != RPC_SUCCESS) {
		return ((int)status);
	}
	sin->sin_port = htons(NFS_PORT);
	*fh = fhs.fhs_fh;
	return ((int)fhs.fhs_status);
}

/*
 * nfs mount vfsop
 * Set up mount info record and attach it to vfs struct.
 */
/*ARGSUSED*/
int
nfs_mount(vfsp, path, data)
	struct vfs *vfsp;
	char *path;
	caddr_t data;
{
	int error;
	struct vnode *rtvp = NULL;	/* the server's root */
	struct mntinfo *mi;		/* mount info, pointed at by vfs */
	fhandle_t fh;			/* root fhandle */
	struct sockaddr_in saddr;	/* server's address */
	char shostname[HOSTNAMESZ];	/* server's hostname */
	int hlen;			/* length of hostname */
	char netname[MAXNETNAMELEN+1];	/* server's netname */
	int nlen;			/* length of netname */
	struct nfs_args args;		/* nfs mount arguments */

	/*
	 * For now, ignore remount option.
	 */
	if (vfsp->vfs_flag & VFS_REMOUNT) {
		return (0);
	}
	/*
	 * get arguments
	 */
	error = copyin(data, (caddr_t)&args, sizeof (args));
	if (error) {
#ifdef RISCOS
		error = EFAULT;
#endif /* RISCOS */
		goto errout;
	}

	/*
	 * Get server address
	 */
	error = copyin((caddr_t)args.addr, (caddr_t)&saddr,
	    sizeof (saddr));
	if (error) {
#ifdef RISCOS
		error = EFAULT;
#endif /* RISCOS */
		goto errout;
	}
	/*
	 * For now we just support AF_INET
	 */
	if (saddr.sin_family != AF_INET) {
		error = EPFNOSUPPORT;
		goto errout;
	}

	/*
	 * Get the root fhandle
	 */
	error = copyin((caddr_t)args.fh, (caddr_t)&fh, sizeof (fh));
	if (error) {
#ifdef RISCOS
		error = EFAULT;
#endif /* RISCOS */
		goto errout;
	}

	/*
	 * Get server's hostname
	 */
	if (args.flags & NFSMNT_HOSTNAME) {
		error = copyinstr(args.hostname, shostname,
			sizeof (shostname), (u_int *)&hlen);
		if (error) {
			goto errout;
		}
	} else {
		addr_to_str(&saddr, shostname);
	}

	/*
	 * Get server's netname
	 */
	if (args.flags & NFSMNT_SECURE) {
		error = copyinstr(args.netname, netname, sizeof (netname),
			(u_int *)&nlen);
	} else {
		nlen = -1;
	}

	/*
	 * Get root vnode.
	 */
	error = nfsrootvp(&rtvp, vfsp, &saddr, &fh, shostname, netname, nlen,
	    args.flags);
	if (error)
		return (error);

	/*
	 * Set option fields in mount info record
	 */
	mi = vtomi(rtvp);
	mi->mi_noac = ((args.flags & NFSMNT_NOAC) != 0);
	if (args.flags & NFSMNT_RETRANS) {
		mi->mi_retrans = args.retrans;
		if (args.retrans < 0) {
			error = EINVAL;
			goto errout;
		}
	}
	if (args.flags & NFSMNT_TIMEO) {
		mi->mi_timeo = args.timeo;
		if (args.timeo <= 0) {
			error = EINVAL;
			goto errout;
		}
	}
	if (args.flags & NFSMNT_RSIZE) {
		if (args.rsize <= 0) {
			error = EINVAL;
			goto errout;
		}
		mi->mi_tsize = MIN(mi->mi_tsize, args.rsize);
	}
	if (args.flags & NFSMNT_WSIZE) {
		if (args.wsize <= 0) {
			error = EINVAL;
			goto errout;
		}
		mi->mi_stsize = MIN(mi->mi_stsize, args.wsize);
	}
	if (args.flags & NFSMNT_ACREGMIN) {
		if (args.acregmin == 0) {
			error = EINVAL;
			printf("nfs_mount: acregmin == 0\n");
			goto errout;
		}
		mi->mi_acregmin = args.acregmin;
	}
	if (args.flags & NFSMNT_ACREGMAX) {
		if (args.acregmax < mi->mi_acregmin) {
			error = EINVAL;
			printf("nfs_mount: acregmax == 0\n");
			goto errout;
		}
		mi->mi_acregmax = args.acregmax;
	}
	if (args.flags & NFSMNT_ACDIRMIN) {
		if (args.acdirmin == 0) {
			error = EINVAL;
			printf("nfs_mount: acdirmin == 0\n");
			goto errout;
		}
		mi->mi_acdirmin = args.acdirmin;
	}
	if (args.flags & NFSMNT_ACDIRMAX) {
		if (args.acdirmax < mi->mi_acdirmin) {
			error = EINVAL;
			printf("nfs_mount: acdirmax == 0\n");
			goto errout;
		}
		mi->mi_acdirmax = args.acdirmax;
	}
	mi->mi_authflavor =
		(args.flags & NFSMNT_SECURE) ? AUTH_DES : AUTH_UNIX;

#ifdef NFSDEBUG
	dprint(nfsdebug, 1,
	    "nfs_mount: hard %d timeo %d retries %d wsize %d rsize %d\n",
	    mi->mi_hard, mi->mi_timeo, mi->mi_retrans, mi->mi_stsize,
	    mi->mi_tsize);
	dprint(nfsdebug, 1,
	    "           regmin %d regmax %d dirmin %d dirmax %d\n",
	    mi->mi_acregmin, mi->mi_acregmax, mi->mi_acdirmin, mi->mi_acdirmax);
#endif

errout:
	if (error) {
		if (rtvp) {
			VN_RELE(rtvp);
		}
	}
	return (error);
}

int
nfsrootvp(rtvpp, vfsp, sin, fh, shostname, netname, nlen, flags)
	struct vnode **rtvpp;		/* where to return root vp */
	register struct vfs *vfsp;	/* vfs of fs, if NULL make one */
	struct sockaddr_in *sin;	/* server address */
	fhandle_t *fh;			/* swap file fhandle */
	char *shostname;		/* server's hostname */
	char *netname;			/* server's netname */
	int nlen;			/* length of netname, -1 if none */
	int flags;			/* mount flags */
{
	register struct vnode *rtvp = NULL;	/* the server's root */
	register struct mntinfo *mi = NULL;	/* mount info, pointed at by vfs */
	struct vattr va;		/* root vnode attributes */
	struct nfsfattr na;		/* root vnode attributes in nfs form */
#ifdef RISCOS
	struct bsd43_statfs sb;		/* server's file system stats */
#else
	struct statfs sb;		/* server's file system stats */
#endif
	register int error;

	/*
	 * Create a mount record and link it to the vfs struct.
	 */
	mi = (struct mntinfo *)kmem_zalloc(sizeof (*mi));
	mi->mi_hard = ((flags & NFSMNT_SOFT) == 0);
	mi->mi_int = ((flags & NFSMNT_INT) != 0);
	mi->mi_addr = *sin;
	mi->mi_retrans = NFS_RETRIES;
	mi->mi_timeo = NFS_TIMEO;
	if ((mi->mi_mntno = vfs_getnum(nfs_minmap, nfs_maxmajors*(256/NBBY))) == -1)
		goto bad; 		/* too many mounts */
	bcopy(shostname, mi->mi_hostname, HOSTNAMESZ);
	mi->mi_acregmin = ACREGMIN;
	mi->mi_acregmax = ACREGMAX;
	mi->mi_acdirmin = ACDIRMIN;
	mi->mi_acdirmax = ACDIRMAX;
	mi->mi_netnamelen = nlen;
	if (nlen >= 0) {
		mi->mi_netname = (char *)kmem_alloc((u_int)nlen);
		bcopy(netname, mi->mi_netname, (u_int)nlen);
	}

	/*
	 * Make a vfs struct for nfs.  We do this here instead of below
	 * because rtvp needs a vfs before we can do a getattr on it.
	 */
	vfsp->vfs_fsid.val[0] = 0xffff & (long)make_nfsdev(mi->mi_mntno);
	vfsp->vfs_fsid.val[1] = MOUNT_NFS;
	vfsp->vfs_data = (caddr_t)mi;

	/*
	 * Make the root vnode, use it to get attributes,
	 * then remake it with the attributes.
	 */
	rtvp = makenfsnode(fh, (struct nfsfattr *)0, vfsp);
	if ((rtvp->v_flag & VROOT) != 0) {
		error = EINVAL;
		goto bad;
	}
	rtvp->v_flag |= VROOT;
	error = VOP_GETATTR(rtvp, &va, u.u_cred);
	if (error)
		goto bad;
	VN_RELE(rtvp);
	vattr_to_nattr(&va, &na);
	rtvp = makenfsnode(fh, &na, vfsp);
	rtvp->v_flag |= VROOT;
	mi->mi_rootvp = rtvp;

	/*
	 * Get server's filesystem stats but not used, so must be to
	 * verify existence of filesystem.  No need to zero then.
	 */
	error = VFS_STATFS(vfsp, &sb);
	if (error)
		goto bad;
	mi->mi_tsize = MIN(NFS_MAXDATA, (u_int)nfstsize());

	/*
	 * Set filesystem block size to maximum data transfer size
	 */
	mi->mi_bsize = NFS_MAXDATA;
	vfsp->vfs_bsize = mi->mi_bsize;

	/*
	 * Need credentials in the rtvp so do_bio can find them.
	 */
	crhold(u.u_cred);
	vtor(rtvp)->r_cred = u.u_cred;

	*rtvpp = rtvp;
	return (0);
bad:
	if (mi) {
		if (mi->mi_netnamelen >= 0) {
			kmem_free((caddr_t)mi->mi_netname, (u_int)mi->mi_netnamelen);
		}
		kmem_free((caddr_t)mi, sizeof (*mi));
	}
	if (rtvp) {
		VN_RELE(rtvp);
	}
	*rtvpp = NULL;
	return (error);
}

/*
 * vfs operations
 */
int
nfs_unmount(vfsp)
	struct vfs *vfsp;
{
	struct mntinfo *mi = (struct mntinfo *)vfsp->vfs_data;

#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_unmount(%x) mi = %x\n", vfsp, mi);
#endif
	rflush(vfsp);
	rinval(vfsp);

	if (mi->mi_refct != 1 || mi->mi_rootvp->v_count != 1) {
		return (EBUSY);
	}
	VN_RELE(mi->mi_rootvp);
	rp_rmhash(vtor(mi->mi_rootvp));
	rinactive(vtor(mi->mi_rootvp));
	vfs_putnum(nfs_minmap, mi->mi_mntno);
	if (mi->mi_netnamelen >= 0) {
		kmem_free((caddr_t)mi->mi_netname, (u_int)mi->mi_netnamelen);
	}
	kmem_free((caddr_t)mi, sizeof (*mi));
	return (0);
}

/*
 * find root of nfs
 */
int
nfs_root(vfsp, vpp)
	struct vfs *vfsp;
	struct vnode **vpp;
{

	*vpp = (struct vnode *)((struct mntinfo *)vfsp->vfs_data)->mi_rootvp;
	VN_HOLD((*vpp));
#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_root(0x%x) = %x\n", vfsp, *vpp);
#endif
	VFS_RECORD(vfsp, VS_ROOT, VS_CALL);
	return (0);
}

/*
 * Get file system statistics.
 */
int
#if RISCOS
nfs_statfs(vfsp, sbp, vp)
	register struct vfs *vfsp;
	struct bsd43_statfs *sbp;
	struct vnode *vp;
#else
nfs_statfs(vfsp, sbp)
	register struct vfs *vfsp;
	struct statfs *sbp;
#endif /* RISCOS */
{
	struct nfsstatfs fs;
	struct mntinfo *mi;
	fhandle_t *fh;
	int error = 0;

	if (vfsp == (struct vfs *)0 || vp != (struct vnode *)0)
		return (EINVAL);
	mi = vftomi(vfsp);
	fh = vtofh(mi->mi_rootvp);
#ifdef NFSDEBUG
	dprint(nfsdebug, 4, "nfs_statfs vfs %x\n", vfsp);
#endif
	error = rfscall(mi, RFS_STATFS, xdr_fhandle,
	    (caddr_t)fh, xdr_statfs, (caddr_t)&fs, u.u_cred);
	if (!error) {
		error = geterrno(fs.fs_status);
	}
	if (!error) {
		if (mi->mi_stsize) {
			mi->mi_stsize = MIN(mi->mi_stsize, fs.fs_tsize);
		} else {
			mi->mi_stsize = fs.fs_tsize;
		}
		sbp->f_bsize = fs.fs_bsize;
		sbp->f_blocks = fs.fs_blocks;
		sbp->f_bfree = fs.fs_bfree;
		sbp->f_bavail = fs.fs_bavail;
		sbp->f_files = -1;	/* undefined = -1; see statfs(2-BSD) */
		sbp->f_ffree = -1;	/* undefined = -1; see statfs(2-BSD) */
#ifdef RISCOS
		/* Only defined in the kernel's bsd43_statfs structure, so
		 * no need to set to -1.  This is needed for sysv_statfs().
		 */
		sbp->f_frsize = 0;
#endif
		/*
		 * XXX - This is wrong, should be a real fsid
		 */
		bcopy((caddr_t)&vfsp->vfs_fsid,
		    (caddr_t)&sbp->f_fsid, sizeof (fsid_t));
#if RISCOS
		bcopy(mi->mi_hostname, sbp->f_fname,
		    sizeof sbp->f_fname);
		sbp->f_fpack[0] = '\0';
#endif /* RISCOS */
	}
#ifdef NFSDEBUG
	dprint(nfsdebug, 5, "nfs_statfs returning %d\n", error);
#endif
	return (error);
}

/*
 * Flush dirty nfs files for file system vfsp.
 * If vfsp == NULL, all nfs files are flushed.
 */
int
nfs_sync(vfsp)
	struct vfs *vfsp;
{
	static int nfslock;

	if (nfslock == 0) {
#ifdef NFSDEBUG
		dprint(nfsdebug, 5, "nfs_sync\n");
#endif
		nfslock++;
		rflush(vfsp);
		nfslock = 0;
	}
	return (0);
}

int
nfs_badop()
{

	panic("nfs_badop");
}

char *
itoa(n, str)
	u_short n;
	char *str;
{
	char prbuf[11];
	register char *cp;

	cp = prbuf;
	do {
		*cp++ = "0123456789"[n%10];
		n /= 10;
	} while (n);
	do {
		*str++ = *--cp;
	} while (cp > prbuf);
	return (str);
}


hostpath(name, shostname, path)
	char *name;
	char *shostname;
	char *path;
{
	register char *nm;

	(void) strcpy(name, shostname);
	for (nm = name; *nm; nm++)
		;
	*nm++ = ':';
	(void) strcpy(nm, path);
}

/*
 * Convert a INET address into a string for printing
 */

addr_to_str(addr, str)
	struct sockaddr_in *addr;
	char *str;
{
	str = itoa((u_short)((addr->sin_addr.s_addr >> 24) & 0xff), str);
	*str++ = '.';
	str = itoa((u_short)((addr->sin_addr.s_addr >> 16) & 0xff), str);
	*str++ = '.';
	str = itoa((u_short)((addr->sin_addr.s_addr >> 8) & 0xff), str);
	*str++ = '.';
	str = itoa((u_short)(addr->sin_addr.s_addr & 0xff), str);
	*str = '\0';
}

/*
 * Internet address interpretation routine.
 * All the network library routines call this
 * routine to interpret entries in the data bases
 * which are expected to be an address.
 * The value returned is in network order.
 */

char _ctype[] = {
	0,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_S,	_S,	_S,	_S,	_S,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_S|_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_N,	_N,	_N,	_N,	_N,	_N,	_N,	_N,
	_N,	_N,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_P,	_P,	_P,	_P,	_P,
	_P,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_P,	_P,	_P,	_P,	_C
};

struct in_addr
inet_addr(cp)
	register char *cp;
{
	register u_long val, base, n;
	register char c;
	u_long parts[4], *pp = parts;
	struct in_addr in_addr;

again:
	/*
	 * Collect number up to ``.''.
	 * Values are specified as for C:
	 * 0x=hex, 0=octal, other=decimal.
	 */
	val = 0; base = 10;
	if (*cp == '0')
		base = 8, cp++;
	if (*cp == 'x' || *cp == 'X')
		base = 16, cp++;
	while (c = *cp) {
		if (isdigit(c)) {
			val = (val * base) + (c - '0');
			cp++;
			continue;
		}
		if (base == 16 && isxdigit(c)) {
			val = (val << 4) + (c + 10 - (islower(c) ? 'a' : 'A'));
			cp++;
			continue;
		}
		break;
	}
	if (*cp == '.') {
		/*
		 * Internet format:
		 *	a.b.c.d
		 *	a.b.c	(with c treated as 16-bits)
		 *	a.b	(with b treated as 24 bits)
		 */
		if (pp >= parts + 4) {
			in_addr.s_addr = -1;
			return (in_addr);
		}
		*pp++ = val, cp++;
		goto again;
	}
	/*
	 * Check for trailing characters.
	 */
	if (*cp && !isspace(*cp)) {
		in_addr.s_addr = -1;
		return (in_addr);
	}
	*pp++ = val;
	/*
	 * Concoct the address according to
	 * the number of parts specified.
	 */
	n = pp - parts;
	switch (n) {

	case 1:				/* a -- 32 bits */
		val = parts[0];
		break;

	case 2:				/* a.b -- 8.24 bits */
		val = (parts[0] << 24) | (parts[1] & 0xffffff);
		break;

	case 3:				/* a.b.c -- 8.8.16 bits */
		val = (parts[0] << 24) | ((parts[1] & 0xff) << 16) |
			(parts[2] & 0xffff);
		break;

	case 4:				/* a.b.c.d -- 8.8.8.8 bits */
		val = (parts[0] << 24) | ((parts[1] & 0xff) << 16) |
		      ((parts[2] & 0xff) << 8) | (parts[3] & 0xff);
		break;

	default:
		in_addr.s_addr = -1;
		return (in_addr);
	}
	val = htonl(val);
	in_addr.s_addr = val;
	return (in_addr);
}

#include "bsd/net/if_arp.h"
#include "bsd/netinet/if_ether.h"
try_netaddr(ifp)
	struct ifnet *ifp;
{
	struct in_addr ipaddr;
	struct sockaddr_in *sin;
	char *cp;
	extern struct in_addr inet_addr();
	struct ifreq ifr;
	extern char *prom_getenv();

	cp = prom_getenv("netaddr");
	if (!cp || !*cp) {
		return;
	}
	ipaddr = inet_addr(cp);	/* Form internet address */
	if ( ipaddr.s_addr == -1 ) {
		ipaddr.s_addr = 0;
	} else {
		bzero((caddr_t)&ifr, sizeof(ifr));
		sin = (struct sockaddr_in *)&ifr.ifr_addr;
		sin->sin_family = AF_INET;
		sin->sin_addr = ipaddr;
		ifp->if_flags |= IFF_NOTRAILERS | IFF_BROADCAST;
		if (in_control((struct socket*)0, SIOCSIFADDR, 
						(caddr_t)&ifr, ifp)) {
			printf("try_netaddr cannot SIOCSIFADDR\n");
			ipaddr.s_addr = 0;
		}
	}
	((struct arpcom *)ifp)->ac_ipaddr = ipaddr;
}

make_nfsdev(mntno)
int	mntno;
{
	return(makedev(nfs_majors[mntno>>8], mntno%0xff));
}

nfs_init()
{
	/* NFS file system specific initialization */

	register int	i;
	register short	maj;

	/* Reserve nfs_maxmajors major device numbers.  If we can't allocate
	 * all of them, we inform the user and update nfs_maxmajors to 
	 * reflect howmany we allocated.
	 */
	ASSERT(nfs_maxmajors > 0);
	for (i = 0; i < nfs_maxmajors; i++)
	    if ((nfs_majors[i] = vfs_getmajor(NULL)) == -1) {
		printf("nfs_init:  Reserved only %d major numbers for NFS.\n",i);
			break;
	    }
	nfs_maxmajors = i;
}
