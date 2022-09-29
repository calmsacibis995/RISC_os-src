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
#ident	"$Header: fuser.c,v 1.6.1.3 90/05/09 16:05:56 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include <nlist.h>
#include <fcntl.h>
#include <pwd.h>
#include <stdio.h>

#define KERNEL 1
#include <sys/param.h>
#include <sys/types.h>

#include <sys/file.h>
#undef KERNEL

#include <sys/immu.h>
#include <sys/psw.h>
#include <sys/pcb.h>
#include <sys/region.h>
#include <sys/sbd.h>

#include <sys/proc.h>
#include <sys/signal.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <sys/user.h>
#include <sys/var.h>
#include <sys/sema.h>
#include <sys/comm.h>

#define KERNEL 1
#include "sys/vfs.h"
#undef KERNEL
#include "sys/vnode.h"
#include "sys/stream.h"
#undef free
#undef malloc
#undef calloc
#define KERNEL 1
#include "sys/fs/ufs_inode.h"
#undef KERNEL
#include "sys/fs/ufs_fsdir.h"
#undef itoo
#undef itod
#undef INOPB
#include "sys/fs/ufs_fs.h"
#include "sys/fs/ufs_mount.h"
#include "sys/errno.h"
#include "sys/fs/nfs.h"
#include "bsd/netinet/in.h"
#include "sys/fs/nfs_clnt.h"
#include "sys/fs/nfs_rnode.h"
#define KERNEL 1
#include "sys/fs/snode.h"
#undef KERNEL

#ifndef MAJOR_MIN
#define MAJOR_MIN 128
#endif MAJOR_MIN

#ifndef MAJOR_MAX
#define MAJOR_MAX 255
#endif MAJOR_MAX

/* symbol names */
#define V_STR		"v"
#define PROC_STR	"proc"
#define FILE_STR	"file"
#define ROOTVFS_STR	"rootvfs"
#define SPEC_VFSOPS_STR	"spec_vfsops"
#define UFS_VFSOPS_STR	"ufs_vfsops"
#define NFS_VFSOPS_STR	"nfs_vfsops"
#define PCFS_VFSOPS_STR	"pcfs_vfsops"
#define LO_VFSOPS_STR	"lo_vfsops"
#define RFS_VFSOPS_STR	"rfs_vfsops"
#define PROC_VFSOPS_STR	"proc_vfsops"
#define SPEC_VNODEOPS_STR	"spec_vnodeops"
#define UFS_VNODEOPS_STR	"ufs_vnodeops"
#define NFS_VNODEOPS_STR	"nfs_vnodeops"
#define PCFS_VNODEOPS_STR	"pcfs_vnodeops"
#define LO_VNODEOPS_STR	"lo_vnodeops"
#define RFS_VNODEOPS_STR	"rfs_vnodeops"
#define PROC_VNODEOPS_STR	"proc_vnodeops"
#define STABLE_STR	"stable"
#define VFSSW_STR	"vfssw"
#define VFSNVFS_STR	"vfsNVFS"

#define KMEMF "/dev/kmem"
#define MEMF "/dev/mem"
#define SYSTEM "/unix"

enum	VFS_TYPE {
		VFS_TYPE_null,
		VFS_TYPE_spec,
		VFS_TYPE_ufs,
		VFS_TYPE_nfs,
		VFS_TYPE_pcfs,
		VFS_TYPE_lo,
		VFS_TYPE_rfs,
		VFS_TYPE_proc };

enum VFS_TYPE get_vfs_type();
enum VFS_TYPE get_vnode_type();

#define MIN(X, Y)  (((X) < (Y)) ? (X) : (Y))

#define valid_file(X) (file_adr <= ((unsigned) (X)) &&\
	((unsigned) (X)) < (file_adr + v.v_file * sizeof (struct file)))

#define valid_vnode(X) ((((((long) (X)) >= K0BASE) && \
			  (((long) (X)) < K0BASE + K0SIZE)) \
		|| ((((long) (X)) >= K2BASE) && \
			(((long) (X)) < Kptebase))) \
		&& ((((long) (X)) & 0x3) == 0))
#define valid_snode(X) valid_vnode((X))
#define valid_ptr(X) valid_vnode((X))

void exit(), perror();
extern char *malloc();

int gun = 0, usrid = 0, mount_point = 0;

struct vnode vna, vnb;
struct vnode *vna_addr, *vnb_addr;
struct vfs vfsa;
struct vfs *vfsa_addr;
enum VFS_TYPE vna_vfs_type;
struct stat sta, stb;
int	vna_remote;

#define DEV_MASK(x) (((unsigned int) (x)) & 0x0000FFFF)

int	kmem = -1;
int	mem = -1;

struct nlist nl[] = {
	{V_STR},
	{PROC_STR},
	{FILE_STR},
	{ROOTVFS_STR},
	{ SPEC_VFSOPS_STR},
	{ UFS_VFSOPS_STR},
	{ NFS_VFSOPS_STR},
	{ PCFS_VFSOPS_STR},
	{ LO_VFSOPS_STR},
	{ RFS_VFSOPS_STR},
	{ PROC_VFSOPS_STR},
	{ SPEC_VNODEOPS_STR},
	{ UFS_VNODEOPS_STR},
	{ NFS_VNODEOPS_STR},
	{ PCFS_VNODEOPS_STR},
	{ LO_VNODEOPS_STR},
	{ RFS_VNODEOPS_STR},
	{ PROC_VNODEOPS_STR},
	{ STABLE_STR },
	{ VFSSW_STR },
	{ VFSNVFS_STR },
	{""}
};

struct snode **stable_addr = NULL;
unsigned file_adr, rootvfs_addr;
struct vfs *vfssw_addr, *vfsNVFS_addr;

int	Usize;
int	Nbpc;
unsigned Kptebase;
int	pte_to_pfn_shift;
int	pfn_to_byte_shift;
#define	pte_to_byte(x)	(((x) >> pte_to_pfn_shift) << pfn_to_byte_shift)
extern	int getpagesize();

copylval(symbol, ptr)
char *symbol;
unsigned *ptr;
{		/* Copies the lvalue of the UNIX symbol "symbol" into
		 * the variable pointed to by "ptr". The lvalue of
		 * "symbol" is read from SYSTEM.
		 */
	int i = 0;

	while(nl[i].n_name[0] != '\0') {
		if(!strcmp(symbol,nl[i].n_name)) {
			if(nl[i].n_value == 0) {
				return(-1);
			}
			*ptr = nl[i].n_value;
			return(0);
		}
		i++;
	}
	return(1);
}

copyrval(symbol, ptr, size)
char *symbol, *ptr;
int size;
{	/* Copies the rvalue of the UNIX symbol "symbol" into the structure
	 * pointed to by "ptr". The rvalue is read from memory at the location
	 * specified by the value of "symbol" in the name list file "nl_file".
	 */
	unsigned lval;

	assure_copylval(symbol, &lval);
	if (assure_kmem() == -1)
		exit(1);
	return(rread(kmem, (long) lval, ptr, size));
}

file_to_vnode(file_in_addr, in, vnode_addr)
struct vnode *in;
struct file *file_in_addr;
struct vnode **vnode_addr;
{	/* Takes a pointer to one of the system's file entries
	 * and copies the vnode  to which it refers into the area
	 * pointed to by "in". "file_in_addr" is a system pointer, usually
	 * from the user area. */
	struct file f;

	if (! valid_file(file_in_addr) ||
	    assure_kmem() == -1)
		return(-1);

	/* read the file table entry */
	if (rread(kmem, (long) file_in_addr, (char *) &f, sizeof f) == -1)
		return(-1);
	if(f.f_flag &&
	   f.f_type == DTYPE_VNODE) {
		*vnode_addr = (struct vnode *) f.f_data;
		return(read_vnode((struct vnode *) f.f_data, in));
	};
	return(-1);
}

struct var v;
unsigned proc_adr;
unsigned *pofile;
struct proc p;
char	*argv_0;

main(argc, argv)
int argc;
char **argv;
{
	int newfile = 0, errcnt = 0;
	register i, j, k;
	struct user *ublock;
	int	ckuse_status;

	/* once only code */
	argv_0 = argv[0];
	if(argc < 2) {
		fprintf(stderr, "Usage:  %s { [-[kum]] file } ... \n", argv_0);
		exit(2);
	}

	/* open file to access memory */
	if (assure_kmem())
		exit(1);
	/* get values of system variables */
	if(nlist(SYSTEM, nl) == -1) {
		perror("nlist:");
		return(-1);
	}

	/* get values of system variables and address of process table */
	Nbpc = getpagesize();
	switch (Nbpc) {
	    case 4096:
		Kptebase = KPTEBASE;
		Usize = USIZE;
		pte_to_pfn_shift  = 12;
		pfn_to_byte_shift = 12;
		break;
	    case 16384:
		Kptebase = (-0x2000000);
		Usize = 1;
		pte_to_pfn_shift  = 10;
		pfn_to_byte_shift = 14;
		break;
	    default:
		fprintf(stderr,"Cannot handle page size %d bytes\n", Nbpc);
		exit(2);
		break;
	} /* switch */
	assure_copyrval(V_STR, (char *) &v, sizeof v);
	ublock = (user_t *)malloc(sizeof(user_t) + sizeof(int) * v.v_nofiles);
	if (ublock == NULL) {
		fprintf(stderr,"%s: could not allocate user block\n",
			argv_0);
		exit(1);
	};
	assure_copylval(PROC_STR, &proc_adr);
	assure_copylval(FILE_STR, &file_adr);
	assure_copylval(ROOTVFS_STR, &rootvfs_addr);
	assure_copylval(STABLE_STR, (char *) &stable_addr, sizeof(stable_addr));
	assure_copylval(VFSSW_STR,&vfssw_addr);
	assure_copyrval(VFSNVFS_STR,&vfsNVFS_addr);

	/* for each argunent on the command line */
	for(i = 1; i < argc; i++) {
		if(argv[i][0] == '-') {
			/* options processing */
			if(newfile) {
				gun = 0;
				usrid = 0;
				mount_point = 0;
			}
			newfile = 0;
			for(j = 1; argv[i][j] != '\0'; j++)
			switch(argv[i][j]) {
			case 'k':
				gun++; 
				break;
			case 'u':
				usrid++; 
				break;
			case 'm':
				mount_point++;
				break;
			default:
				fprintf(stderr, "Usage:  %s { [-[kum]] file } ... \n", argv[0]);
				exit(2);
			}
			continue;
		} else
			newfile = 1;
		fflush(stdout);
	
		/* First print file name on stderr (so stdout (pids) can
		 * be piped to kill) */
		fprintf(stderr, "%s: ", argv[i]);

		/* check the path */
		if(stat(argv[i], &sta) == -1) {
			fprintf(stderr," not found\n");
			errcnt++;
			continue;
		}
		if (mount_point &&
		    (sta.st_mode & S_IFMT) != S_IFDIR) {
			fprintf(stderr," is not a mount point\n");
			errcnt++;
			continue;
		};
		vna_addr, vnb_addr = NULL;
		vna.v_rdev = 0;
		vfsa_addr = NULL;
		vna_vfs_type = VFS_TYPE_null;
		if (major(DEV_MASK(sta.st_dev)) > MAJOR_MIN) {
			if (fixedmajor_to_vfs_type(major(DEV_MASK(sta.st_dev)),
				&vna_vfs_type) == -1) {
				fprintf(stderr," vfssw not found\n");
				errcnt++;
				continue;
			};
		};
		switch (sta.st_mode & S_IFMT) {
		case S_IFDIR:
			vna.v_type = VDIR;
			if (mount_point) {
				if (find_vfs_for_mount_point(argv[i],&sta,
					    &vna_vfs_type,&vfsa,&vfsa_addr)
						== -1) {
					fprintf(stderr," vfs not found\n");
					errcnt++;
					continue;
				};
			};
			break;
		case S_IFCHR:
		case S_IFBLK:
			if (read_dev_vnode(DEV_MASK(sta.st_rdev),&vna,&vna_addr) == -1) {
				fprintf(stderr," vnode not found\n");
				errcnt++;
				continue;
			};
			vna_vfs_type = VFS_TYPE_spec;
			break;
		case S_IFREG:
			vna.v_type = VREG;
			break;
		case S_IFIFO:
			vna.v_type = VFIFO;
			break;
		case S_IFLNK:
			vna.v_type = VLNK;
			break;
		case S_IFSOCK:
			vna.v_type = VSOCK;
			break;				
		case 0:
			vna.v_type = VNON;

			break;				
		default:
			vna.v_type = VBAD;
			break;
		};
		switch (vna_vfs_type) {
		case VFS_TYPE_nfs:
		case VFS_TYPE_rfs:
			vna_remote = 1;
			break;
	
		default:
			vna_remote = 0;
			break;
		};
			
		/* then for each process */
		for(j = 0; j < v.v_proc; j++) {
			/* read in the per-process info */
			if (rread(kmem, (long) (proc_adr + j * sizeof p),
					(char *) &p, sizeof p) == -1)
				continue;
			if(p.p_stat == 0 
			|| p.p_stat == SZOMB 
			|| p.p_stat == SIDL) 
				continue;
			/* and the user area */
			if (read_user(&p, ublock, j) == -1)
				continue;
			pofile = (unsigned *)((unsigned)ublock 
				+ ((unsigned)(ublock->u_pofile) & 0xffff));
			if(read_vnode(ublock->u_cdir, &vnb) == 0) {
				vnb_addr = ublock->u_cdir;
				if(ckuse("c") > 0) {
					errcnt++;
					continue;
				}
			}
			if(read_vnode(ublock->u_rdir, &vnb) == 0) {
				vnb_addr = ublock->u_rdir;
				if(ckuse("r") > 0) {
					errcnt++;
					continue;
				}
			};
			/* then, for each file */
			for(k = 0; k < v.v_nofiles; k++)
			{	/* check if it is the fs being checked */
				if(file_to_vnode(ublock->u_ofile[k], &vnb, &vnb_addr))
					continue;
				ckuse_status = ckuse("");
				if (ckuse_status > 0) {
					errcnt++;
					break;
				} else if (ckuse_status)
					break;
			}
		}
		printf("\n");
		fflush(stdout);
	};
	
	exit(errcnt);
}


ckuse(fc)
char *fc;
{
	int b_remote;
	int do_match;
	
	switch (get_vnode_type(NULL,&vnb,vnb_addr)) {
	case VFS_TYPE_nfs:
	case VFS_TYPE_rfs:
		b_remote = 1;
		break;

	default:
		b_remote = 0;
		break;
	};

	if ((vna_remote && (! b_remote)) ||
	    ((! vna_remote) && b_remote))
		return(0);

	do_match = 0;
	if (vna_addr != NULL &&
	    vna_addr == vnb_addr)
		do_match = 1;
	else if (mount_point) {
		 if (vfsa_addr != NULL &&
		     vnb.v_vfsp == vfsa_addr)
			do_match = 1;
	} else {
		if (vnode_to_stat(&stb,&vnb,vnb_addr) == -1)
			return(0);
		if (vna_vfs_type == VFS_TYPE_spec &&
			 vna.v_type == VBLK) {
			if (DEV_MASK(stb.st_dev) == vna.v_rdev) 
				do_match = 1;
		} else if (DEV_MASK(stb.st_dev) == DEV_MASK(sta.st_dev) &&
			   stb.st_ino == sta.st_ino &&
			   (stb.st_mode & S_IFMT) == (sta.st_mode & S_IFMT))
			do_match = 1;
	};	

	if (! do_match)
		return(0);

	fprintf(stdout, " %7d", (int) p.p_pid);
	fflush(stdout);
	switch(fc[0]) {
	case 'c':
	case 'r':
		fprintf(stderr,"%c", fc[0]);
	default:
		break;
	}
	if(usrid) 
		puname();
	if(gun) {
		kill((int) p.p_pid, 9);
		return(1);
	};
	return(-1);
}

puname()
{	struct passwd *getpwuid(), *pid;

	pid = getpwuid(p.p_uid);
	if(pid == NULL) return;
	fprintf(stderr, "(%s)", pid->pw_name);
}

struct fns_arg {
	int	fns_index;
	struct	snode *fns_sp;
	};

struct fns_arg null_fns_arg = { -1, NULL };

read_next_snode(fns,snode_buf,snode_addr)
	struct	fns_arg *fns;
	struct	snode *snode_buf;
	struct	snode **snode_addr;
{
	if (fns->fns_index == -1) {
		fns->fns_sp = NULL;
	};
	while (1) {
		while (fns->fns_sp == NULL) {
			if ((++fns->fns_index) >= STABLESIZE) 
				return(-1);
			if (read_ptr((char *) (stable_addr + fns->fns_index),
					(char *) &fns->fns_sp) == -1) {
				fns->fns_sp = NULL;
				continue;
			};
		};
		if (fns->fns_sp != NULL) {
			if (read_snode(fns->fns_sp,
				snode_buf) == -1) {
				fns->fns_sp = NULL;
				continue;
			};
			*snode_addr = fns->fns_sp;
			fns->fns_sp = snode_buf->s_next;
			if (snode_buf->s_count == 0)
				continue;
			return(0);
		};
	}
}


read_snode(snode_addr,snode_buf)
	struct	snode *snode_addr;
	struct	snode *snode_buf;
{
	if (stable_addr == NULL ||
	    snode_addr == NULL ||
	    ! valid_snode(snode_addr) ||
	    assure_kmem() == -1)
		return(-1);

	/* read the snode */
	return(rread(kmem, (long) snode_addr, (char *) snode_buf,
		      sizeof (struct snode)));
}


read_inode(inode_addr,inode_buf)
	struct	inode *inode_addr;
	struct	inode *inode_buf;
{
	if (stable_addr == NULL ||
	    inode_addr == NULL ||
	    ! valid_ptr(inode_addr) ||
	    assure_kmem() == -1)
		return(-1);

	/* read the inode */
	return(rread(kmem, (long) inode_addr, (char *) inode_buf,
		      sizeof (struct inode)));
}


read_rnode(rnode_addr,rnode_buf)
	struct	rnode *rnode_addr;
	struct	rnode *rnode_buf;
{
	if (stable_addr == NULL ||
	    rnode_addr == NULL ||
	    ! valid_ptr(rnode_addr) ||
	    assure_kmem() == -1)
		return(-1);

	/* read the rnode */
	return(rread(kmem, (long) rnode_addr, (char *) rnode_buf,
		      sizeof (struct rnode)));
}


read_vfs(vfs_addr,vfs_buf)
	struct	vfs *vfs_addr;
	struct	vfs *vfs_buf;
{
	if (vfs_addr == NULL ||
	    ! valid_ptr(vfs_addr) ||
	    assure_kmem() == -1)
		return(-1);

	/* read the vfs */
	return(rread(kmem, (long) vfs_addr, (char *) vfs_buf,
		      sizeof (struct vfs)));
}


read_ptr(ptr_addr,ptr_buf)
	char *ptr_addr;
	char **ptr_buf;
{
	if (ptr_addr == NULL ||
	    ! valid_ptr(ptr_addr) ||
	    assure_kmem() == -1)
		return(-1);

	/* read the ptr */
	return(rread(kmem, (long) ptr_addr, (char *) ptr_buf,
		      sizeof (char *)));
}


read_dev_vnode(dev,vbuf,vnode_adr)
	dev_t	dev;
	struct	vnode *vbuf;
	struct	vnode **vnode_adr;
{
	struct	snode	snode_buf;
	struct	fns_arg	fns;
	struct	snode	*snode_addr;

	fns = null_fns_arg;
	while (read_next_snode(&fns,&snode_buf,&snode_addr) != -1) {
		if (snode_buf.s_dev == dev) {
			*vbuf = snode_buf.s_vnode;
			*vnode_adr = &(snode_addr->s_vnode);
			return(0);
		};
	}
	return(-1);
}


read_vnode(vnode_addr, i)
struct vnode *i, *vnode_addr;
{	/* Takes a pointer to one of the system's vnode entries
	 * and reads the vnode into the structure pointed to by "i" */
	int	status;

	if(vnode_addr == NULL ||
	   ! valid_vnode(vnode_addr) ||
	   assure_kmem() == -1)
		return(-1);

	/* read the vnode */
	status = rread(kmem, (long) vnode_addr, (char *) i,
			 sizeof (struct vnode));
	if (status == -1 ||
	    i->v_count == 0)
		return(-1);
	return(0);
}


assure_kmem()
{
	if (kmem == -1)
		if ((kmem = open(KMEMF, O_RDONLY)) == -1) {
			fprintf(stderr, "%s: ", argv_0);
			perror(KMEMF);
			return(-1);
		}
	return(0);
}


assure_mem()
{
	if (mem == -1)
		if ((mem = open(MEMF, O_RDONLY)) == -1) {
			fprintf(stderr, "%s: ", argv_0);
			perror(MEMF);
			return(-1);
		}
	return(0);
}


read_user(pr, ub, pndx)
int pndx;
struct proc *pr;
struct user *ub;
{	/* Copies the system's user area (in memory) pointed
	 * to by "pr" into the structure pointed to by "u"
	 */
	int	status;
	int i, x;

	if (assure_mem() == -1)
		exit(1);
	for(i = 0; i < Usize; i++) {
		status |=
		   rread(mem,
		      (long)(phystokv(pte_to_byte(pr->p_ubptbl[i].pgi.pg_pde))),
		      ((char *) ub) + (i * Nbpc), Nbpc);
	}
	return(status);
}


rread(device, position, buffer, count)
char *buffer;
int count, device;
long position;
{	/* Seeks to "position" on device "device" and reads "count"
	 * bytes into "buffer". Zeroes out the buffer on errors.
	 */
	int i;
	long lseek();

	if (position == 0) {
		for(i = 0; i < count; buffer++, i++) *buffer = '\0';
		return(-1);
	};
	position &= 0x7FFFFFFF;
	if(lseek(device, position, 0) == (long) -1) {
		fprintf(stderr, "Seek error for file number %d: ", device);
		perror("");
		for(i = 0; i < count; buffer++, i++) *buffer = '\0';
		return(-1);
	}
	if(read(device, buffer, (unsigned) count) == -1) {
		fprintf(stderr, "Read error for file number %d: ", device);
		perror("");
		for(i = 0; i < count; buffer++, i++) *buffer = '\0';
		return(-1);
	}
	return(0);
}


struct vfs_ops_to_type {
	char	*sym_name;
	enum	VFS_TYPE type_code;
	long	sym_value;
	};

struct vfs_ops_to_type vfs_ops_table[] = {
	{ SPEC_VFSOPS_STR, VFS_TYPE_spec },
	{ UFS_VFSOPS_STR, VFS_TYPE_ufs },
	{ NFS_VFSOPS_STR, VFS_TYPE_nfs },
	{ PCFS_VFSOPS_STR, VFS_TYPE_pcfs },
	{ LO_VFSOPS_STR, VFS_TYPE_lo },
	{ RFS_VFSOPS_STR, VFS_TYPE_rfs },
	{ PROC_VFSOPS_STR, VFS_TYPE_proc },
	{ NULL, VFS_TYPE_null } };

enum VFS_TYPE get_vfs_type(vfs_buf)
	struct	vfs *vfs_buf;
{
	int	i;

	get_vfs_symbols();

	for (i = 0; vfs_ops_table[i].sym_name != NULL; i++)
		if (vfs_ops_table[i].sym_value == (long) vfs_buf->vfs_op)
			break;
	return(vfs_ops_table[i].type_code);
}


get_vfs_symbols()
{
	static	found_symbols = 0;
	int	i;

	if (found_symbols)
		return;

	for (i = 0; vfs_ops_table[i].sym_name != NULL; i++) {
		if (copylval(vfs_ops_table[i].sym_name,
			     &vfs_ops_table[i].sym_value))
			vfs_ops_table[i].sym_value = NULL;
	};
	found_symbols = 1;
}

struct vnode_ops_to_type {
	char	*sym_name;
	enum	VFS_TYPE type_code;
	long	sym_value;
	};

struct vnode_ops_to_type vnode_ops_table[] = {
	{ SPEC_VNODEOPS_STR, VFS_TYPE_spec },
	{ UFS_VNODEOPS_STR, VFS_TYPE_ufs },
	{ NFS_VNODEOPS_STR, VFS_TYPE_nfs },
	{ PCFS_VNODEOPS_STR, VFS_TYPE_pcfs },
	{ LO_VNODEOPS_STR, VFS_TYPE_lo },
	{ RFS_VNODEOPS_STR, VFS_TYPE_rfs },
	{ PROC_VNODEOPS_STR, VFS_TYPE_proc },
	{ NULL, VFS_TYPE_null } };

enum VFS_TYPE get_vnode_type(stat_buf,vnode_buf,vnode_addr)
	struct	stat	*stat_buf;
	struct	vnode *vnode_buf;
	struct	vnode *vnode_addr;
{
	int	i;

	get_vnode_symbols();

	if (vnode_addr != NULL) {
		for (i = 0; vnode_ops_table[i].sym_name != NULL; i++)
			if (vnode_ops_table[i].sym_value ==
				 (long) vnode_buf->v_op)
				break;
		return(vnode_ops_table[i].type_code);
	};
	return(VFS_TYPE_null);
}


get_vnode_symbols()
{
	static	found_symbols = 0;
	int	i;

	if (found_symbols)
		return;

	for (i = 0; vnode_ops_table[i].sym_name != NULL; i++) {
		if (copylval(vnode_ops_table[i].sym_name,
			     &vnode_ops_table[i].sym_value))
			vnode_ops_table[i].sym_value = NULL;
	};
	found_symbols = 1;
}



fixedmajor_to_vfs_type(major_num,vfs_type)
	int	major_num;
	enum	VFS_TYPE *vfs_type;
{
	struct	vfssw vfssw_buf;
	int	i;

	*vfs_type = VFS_TYPE_null;
	if (major_num < MAJOR_MIN ||
	    major_num > MAJOR_MAX ||
	   (major_num - MAJOR_MIN) >= (vfsNVFS_addr - vfssw_addr) ||
	   (rread(kmem, (long) (vfssw_addr + (major_num - MAJOR_MIN)), 
			(char *) &vfssw_buf,
			 sizeof (vfssw_buf)) == -1))
		return(-1);
	
	get_vfs_symbols();

	for (i = 0; vfs_ops_table[i].sym_name != NULL; i++)
		if (vfs_ops_table[i].sym_value == (long) vfssw_buf.vsw_ops)
			break;
	*vfs_type = vfs_ops_table[i].type_code;

	return(0);
}


assure_copyrval(str,addr,size)
	char	*str;
	char	*addr;
	int	size;
{
	if(copyrval(str, (char *) addr, size)) {
		fprintf(stderr,"%s: Copyrval: did not find '%s'\n",argv_0,str);
		exit(1);
	}
}


assure_copylval(str,addr,size)
	char	*str;
	char	*addr;
	int	size;
{
	if(copylval(str, (char *) addr, size)) {
		fprintf(stderr,"%s: Copylval: did not find '%s'\n",argv_0,str);
		exit(1);
	}
}


vnode_to_stat(stat_buf,vnode_buf,vnode_addr)
	struct	stat *stat_buf;
	struct	vnode *vnode_buf;
	struct	vnode *vnode_addr;
{
	struct	vfs vfs_buf;
	enum	VFS_TYPE vfs_type;
	struct	snode snode_buf;
	struct	inode inode_buf;
	struct	rnode rnode_buf;
	struct	vnode real_vnode_buf;

	bzero((char *) stat_buf, sizeof(*stat_buf));
	switch (vnode_buf->v_type) {
	case VREG:
		stat_buf->st_mode = S_IFREG;
		break;
	case VDIR:
		stat_buf->st_mode = S_IFDIR;
		break;
	case VBLK:
		stat_buf->st_mode = S_IFBLK;
		break;
	case VCHR:
		stat_buf->st_mode = S_IFCHR;
		break;
	case VLNK:
		stat_buf->st_mode = S_IFLNK;
		break;
	case VSOCK:
		stat_buf->st_mode = S_IFSOCK;
		return(0);
	case VFIFO:
		stat_buf->st_mode = S_IFIFO;
		break;
	default:
		break;
	};
	stat_buf->st_rdev = vnode_buf->v_rdev;
	if (read_vfs(vnode_buf->v_vfsp,	&vfs_buf) == -1)
		return(-1);
	vfs_type = get_vfs_type(&vfs_buf);
	switch (vfs_type) {
	case VFS_TYPE_spec:
		if (read_snode(VTOS(vnode_buf),&snode_buf) == -1 ||
		    read_vnode(snode_buf.s_realvp,&real_vnode_buf) == -1)
		if (snode_buf.s_realvp != NULL) {
			if (vnode_to_stat(&stat_buf,&real_vnode_buf,
				  snode_buf.s_realvp) == -1)
				return(-1);
		} else {
			stat_buf->st_dev = snode_buf.s_dev;
			stat_buf->st_size = snode_buf.s_size;
		};
		stat_buf->st_atime = snode_buf.s_atime.tv_sec;
		stat_buf->st_mtime = snode_buf.s_mtime.tv_sec;
		stat_buf->st_ctime = snode_buf.s_ctime.tv_sec;
		break;

	case VFS_TYPE_ufs:
		if (read_inode(VTOI(vnode_buf),&inode_buf) == -1)
			return(-1);
		stat_buf->st_dev = inode_buf.i_dev;
		stat_buf->st_ino = inode_buf.i_number;
		stat_buf->st_mode = inode_buf.i_mode;
		stat_buf->st_nlink = inode_buf.i_nlink;
		stat_buf->st_uid = inode_buf.i_uid;
		stat_buf->st_gid = inode_buf.i_gid;
		stat_buf->st_rdev = inode_buf.i_rdev;
		stat_buf->st_size = inode_buf.i_size;
		stat_buf->st_atime = inode_buf.i_atime;
		stat_buf->st_mtime = inode_buf.i_mtime;
		stat_buf->st_ctime = inode_buf.i_ctime;
		break;

	case VFS_TYPE_nfs:
		if (read_rnode(vtor(vnode_buf),&rnode_buf) == -1)
			return(-1);
		stat_buf->st_dev = rnode_buf.r_attr.va_fsid;
		stat_buf->st_ino = rnode_buf.r_attr.va_nodeid;
		stat_buf->st_mode = rnode_buf.r_attr.va_mode;
		stat_buf->st_nlink = rnode_buf.r_attr.va_nlink;
		stat_buf->st_uid = rnode_buf.r_attr.va_uid;
		stat_buf->st_gid = rnode_buf.r_attr.va_gid;
		stat_buf->st_rdev = rnode_buf.r_attr.va_rdev;
		stat_buf->st_size = rnode_buf.r_attr.va_size;
		stat_buf->st_atime = rnode_buf.r_attr.va_atime.tv_sec;
		stat_buf->st_mtime = rnode_buf.r_attr.va_mtime.tv_sec;
		stat_buf->st_ctime = rnode_buf.r_attr.va_ctime.tv_sec;
		break;

	case VFS_TYPE_rfs:
	case VFS_TYPE_proc:
	case VFS_TYPE_pcfs:
	case VFS_TYPE_lo:
	case VFS_TYPE_null:
	default:
		return(-1);
	};

	return(0);
}


find_vfs_for_mount_point(path,st,vfs_type,vfs_buf,vfs_addr)
	char	*path;
	struct	stat *st;
	enum VFS_TYPE *vfs_type;
	struct	vfs *vfs_buf;
	struct	vfs **vfs_addr;
{
	struct	mount mount_buf;
	struct	mntinfo mntinfo_buf;
	struct	vnode vnode_buf;
	struct	rnode rnode_buf;

	if (*vfs_addr == NULL) {
		if (read_ptr((char *) rootvfs_addr,vfs_addr) == -1) {
			*vfs_addr = NULL;
			*vfs_type = VFS_TYPE_null;
			return(-1);
		};
		while (*vfs_addr != NULL) {
			if (read_vfs(*vfs_addr,vfs_buf) == -1) {
				*vfs_addr = NULL;
				*vfs_type = VFS_TYPE_null;
				return(-1);
			};
			*vfs_type = get_vfs_type(vfs_buf);
			switch (*vfs_type) {
			case VFS_TYPE_ufs:
	   			if (rread(kmem, (long) vfs_buf->vfs_data,
					  (char *) &mount_buf,
					  sizeof(mount_buf)) == -1)
					break;
				if (mount_buf.m_dev != DEV_MASK(st->st_dev))
					break;
				return(0);				

			case VFS_TYPE_nfs:
	   			if (rread(kmem, (long) vfs_buf->vfs_data,
					  (char *) &mntinfo_buf,
					  sizeof(mntinfo_buf)) == -1)
					break;
				if (read_vnode(mntinfo_buf.mi_rootvp,&vnode_buf)
					    == -1 ||
				    read_rnode(vtor(&vnode_buf),&rnode_buf)
					    == -1 ||
				    DEV_MASK(st->st_dev) != rnode_buf.r_attr.va_fsid ||
				    st->st_ctime != rnode_buf.r_attr.va_ctime.tv_sec)
					break;
				return(0);
		
			case VFS_TYPE_spec:
			case VFS_TYPE_rfs:
			case VFS_TYPE_proc:
			case VFS_TYPE_pcfs:
			case VFS_TYPE_lo:
			case VFS_TYPE_null:
			default:
				break;
			};
			*vfs_addr = vfs_buf->vfs_next;
		};
		*vfs_type = VFS_TYPE_null;
		return(-1);		
	};
	return(0);
}


