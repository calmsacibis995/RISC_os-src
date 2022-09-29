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
#ident	"$Header: mount.c,v 1.4.1.2 90/05/09 15:27:14 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions:  mount.
 */

#include "crash.h"

#include "bsd/netinet/in.h"
#include "sys/fs/nfs_clnt.h"
#include "sys/fs/ufs_mount.h"

struct	vfs vfs_buf;

/* get arguments for mount function */
int
getmount()
{
	int slot = -1;
	int all = 0;
	int phys = 0;
	long addr = -1;
	int c;
	long arg1 = -1;
	long arg2 = -1;

	get_mount_symbols();

	optind = 1;
	while((c = getopt(argcnt,args,"epw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"ADDR     FS   BSIZ MAJ/MIN COVERED  MOUNTED  FLAGS RD\n");
	if(args[optind]) {
		all = 1;
		do {
			getargs(GETARGS_NO_SLOT,&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg2 != -1)
				for(addr = arg1; addr <= arg2; 
				    addr = (long) (((struct vfs *) addr) + 1))
					prmount(all,phys,addr);
			else {
				prmount(all,phys,arg1);
			}
			addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else {
		readmem((long)SYM_rootvfs->n_value,1,-1,
			&addr,sizeof(struct vfs *),"rootvfs");
		for (;addr != (long) NULL;) {
			prmount(all,phys,addr);
			addr = (long) vfs_buf.vfs_next;
		};
	};
}

/* print mount table */
int
prmount(all,phys,addr)
int all,phys;
long addr;
{
	struct	vnode	covered_vnode_buf;
	struct	vnode	vnode_buf;
	long	mounted_vnode_addr;
	char	fs_name[MAXNAMLEN + 1];
	enum	VFS_TYPE vfs_type;

	get_mount_symbols();

	readbuf(addr,0,phys,-1,
		(char *)&vfs_buf,sizeof vfs_buf,"vfs entry");
	fprintf(fp,"%8x",addr);
	read_vfs_name(fs_name,&vfs_buf);
	fprintf(fp," %4s %4u",
		fs_name,
		vfs_buf.vfs_bsize);
	if (vfs_buf.vfs_vnodecovered != NULL) {
		readbuf((long) vfs_buf.vfs_vnodecovered,0,0,-1,
			(char *) &covered_vnode_buf,
			sizeof(struct vnode),
			"covered vnode");
		read_mounted_vnode(&vfs_type,
				   &mounted_vnode_addr,
				   &vnode_buf,addr,&vfs_buf,
				   (long) vfs_buf.vfs_vnodecovered,
				   &covered_vnode_buf);
		if (mounted_vnode_addr != (long) NULL &&
		    vfs_type == VFS_TYPE_ufs) {
			struct	mount mount_buf;

			readbuf((long) vfs_buf.vfs_data,0,0,-1,
				(char *)&mount_buf,sizeof(mount_buf),
				"ufs mount entry");
			fprintf(fp," %3u,%-3u",
				major(mount_buf.m_dev),
				minor(mount_buf.m_dev));
		} else
			fprintf(fp," ---,---");
		fprintf(fp," %8x",(long) vfs_buf.vfs_vnodecovered);
		if (mounted_vnode_addr != (long) NULL) {
			fprintf(fp," %8x",mounted_vnode_addr);
		} else
			fprintf(fp,"  -      ");
	} else
		fprintf(fp," ---,---  -        -      ");

	fprintf(fp,"  %s%s%s%s",
		(vfs_buf.vfs_flag & VFS_MLOCK) ? "L" : "-",
		(vfs_buf.vfs_flag & VFS_MWAIT) ? "W" : "-",
		(vfs_buf.vfs_flag & VFS_NOSUID) ? "S" : "-",
		(vfs_buf.vfs_flag & VFS_GRPID) ? "G" : "-",
		(vfs_buf.vfs_flag & VFS_NOSUB) ? "U" : "-",
		(vfs_buf.vfs_flag & VFS_MULTI) ? "M" : "-");
	fprintf(fp," %s",vfs_buf.vfs_flag & VFS_RDONLY ? "ro" : "rw");
	fprintf(fp,"\n");
}


int
read_mounted_vnode(vfs_type,
		   mounted_vnode_addr,
		   vnode_buf,addr,vfs_buf,
		   covered_vnode_addr,
		   covered_vnode_buf)
	enum	VFS_TYPE *vfs_type;
	long	*mounted_vnode_addr;
	struct	vnode	*vnode_buf;
	long	addr;
	struct	vfs	*vfs_buf;
	long	covered_vnode_addr;
	struct	vnode	*covered_vnode_buf;
{
	char	fs_name[MAXNAMLEN + 1];
	enum VFS_TYPE fs_type;

	*mounted_vnode_addr = (long) NULL;
	fs_type = get_vfs_type(addr,vfs_buf);
	switch (fs_type){
	case VFS_TYPE_spec:
		break;

	case VFS_TYPE_ufs:
		{
			long	inode_addr;
			struct	inode inode_buf;

			if (find_inode(&inode_addr,&inode_buf,
					addr,vfs_buf)) {
				*mounted_vnode_addr = (long) ITOV(((struct inode *) inode_addr));
				*vnode_buf = *ITOV(&inode_buf);
				*vfs_type = fs_type;
				return(1);
			};			
		}
		break;

	case VFS_TYPE_nfs:
		{
			struct	mntinfo mntinfo_buf;

			readbuf((long) vfs_buf->vfs_data,0,0,-1,
				(char *)&mntinfo_buf,sizeof(mntinfo_buf),
				"nfs mount entry");
			readbuf((long) mntinfo_buf.mi_rootvp,0,0,-1,
				(char *)vnode_buf,
				sizeof(*vnode_buf),
				"nfs mounted vnode");
			*mounted_vnode_addr = (long) mntinfo_buf.mi_rootvp;
			*vfs_type = fs_type;
			return(1);
		}
		break;

#ifdef LATER
	case VFS_TYPE_proc:
		break;
#endif LATER

	case VFS_TYPE_null:
	case VFS_TYPE_pcfs:
	case VFS_TYPE_lo:
	case VFS_TYPE_rfs:
	default:
		break;
	};
	return(0);
}


struct vfs_ops_to_type {
	struct	syment **sym;
	enum	VFS_TYPE type_code;
	};

struct vfs_ops_to_type vfs_ops_table[] = {
	{ &SYM_spec_vfsops, VFS_TYPE_spec },
	{ &SYM_ufs_vfsops, VFS_TYPE_ufs },
	{ &SYM_nfs_vfsops, VFS_TYPE_nfs },
	{ &SYM_pcfs_vfsops, VFS_TYPE_pcfs },
	{ &SYM_lo_vfsops, VFS_TYPE_lo },
	{ &SYM_rfs_vfsops, VFS_TYPE_rfs },
	{ &SYM_proc_vfsops, VFS_TYPE_proc },
	{ NULL, VFS_TYPE_null } };

enum VFS_TYPE get_vfs_type(vfsp,vfs_buf)
	long	vfsp;
	struct	vfs *vfs_buf;
{
	int	i;

	get_mount_symbols();

	for (i = 0; vfs_ops_table[i].sym != NULL; i++)
		if (*vfs_ops_table[i].sym != NULL &&
		    (*vfs_ops_table[i].sym)->n_value == (long) vfs_buf->vfs_op)
			break;
	return(vfs_ops_table[i].type_code);
}


get_mount_symbols()
{
	static	found_symbols = 0;

	if (found_symbols)
		return;

	get_symbol_value(&SYM_spec_vfsops,"spec_vfsops",0);
	get_symbol_value(&SYM_ufs_vfsops,"ufs_vfsops",0);
	get_symbol_value(&SYM_nfs_vfsops,"nfs_vfsops",0);
	get_symbol_value(&SYM_pcfs_vfsops,"pcfs_vfsops",0);
	get_symbol_value(&SYM_lo_vfsops,"lo_vfsops",0);
	get_symbol_value(&SYM_rfs_vfsops,"rfs_vfsops",0);
  	get_symbol_value(&SYM_rootvfs,"rootvfs",1);
  	get_symbol_value(&SYM_vfssw,"vfssw",1);
	get_symbol_value(&SYM_vfsNVFS,"vfsNVFS",1);
	found_symbols = 1;
}


get_symbol_value(sym,name,required)
	struct	syment **sym;
	char	*name;
	int	required;
{
	char	msgbuf[256];
	
	if (! (*sym)) 
		if(!(*sym = symsrch(name)))
			if (required) {
				sprintf(msgbuf,"%s not found\n",name);
				error(msgbuf);
			};
}

