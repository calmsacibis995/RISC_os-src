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
#ident	"$Header: inode.c,v 1.4.1.3 90/05/09 15:26:12 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions:  inode, file, vnode
 */

#include "crash.h"

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

extern struct syment *SYM_spec_vnodeops;
struct syment *SYM_stable;
struct syment *SYM_rtable;

#ifndef RTABLESIZE
#define RTABLESIZE 64
#endif RTABLESIZE

struct	findvnodeargs null_findvnodeargs =
		{ VFS_TYPE_spec, 0, -1, NULL };

struct inode ibuf;		/* buffer for inode */


/* get arguments for inode function */
/* inode now just lists ufs inodes */
int
getinode()
{
	int slot = -1;
	int full = 0;
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int free = 0;
	long next;
	int list = 0;
	struct inode *freebuf;
	int c;
	char *heading = "SLOT MAJ/MIN  INUMB RCNT LINK  UID   GID    SIZE     MODE  MOUNT    M/ST     FLAGS\n";

	optind = 1;
	while((c = getopt(argcnt,args,"efprlw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'f' :	full = 1;
					break;
			case 'p' :	phys = 1;
					break;
			case 'r' :	free = 1;
					break;
			case 'l' :	list = 1;
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(list)
		listinode();
	else {
		if(!SYM_inode)
			if(!(SYM_inode = symsrch("inode")))
				error("inode not found in symbol table\n");
		fprintf(fp,"INODE TABLE SIZE = %d\n",vbuf.v_inode);
		if(!full)
			fprintf(fp,"%s",heading);
		if(free) {
			if(!SYM_ifreeh)
				if(!(SYM_ifreeh = symsrch("ifreeh")))
					error("ifreeh not found in symbol table\n");
			readmem((long)SYM_ifreeh->n_value,1,-1,(char *)&freebuf,
			sizeof freebuf,"ifreeh buffer");
			next = (long)freebuf;
			while(next) {
				prinode(1,full,slot,phys,next,heading);
				next = (long)ibuf.i_freef;
			}
		}
		else if(args[optind]) {
			all = 1;
			do {
				getargs(- vbuf.v_inode,&arg1,&arg2);
				if(arg1 == -1)
					continue;
				if(arg2 != -1) {
					if (arg1 >= 0 && 
					    arg1 <= vbuf.v_inode) {
						for(slot = arg1; slot <= arg2; slot++)
							prinode(all,full,slot,phys,addr,
								heading);
					} else {
						for (addr = arg1; addr <= arg2;
						     addr = (long) (((struct inode *) addr) + 1)) 
							prinode(all,full,-1,phys,addr,
							        heading);
					};
				} else {
					if(arg1 >= 0 &&
					   arg1 < vbuf.v_inode)
						slot = arg1;
					else addr = arg1;
					prinode(all,full,slot,phys,addr,heading);
				}
				slot = addr = arg1 = arg2 = -1;
			}while(args[++optind]);
		}
		else for(slot = 0; slot < vbuf.v_inode; slot++)
			prinode(all,full,slot,phys,addr,heading);
	}
}


int
listinode()
{
	char inodebuf[2000];
	int i,j;
	long next;
	struct inode *freebuf;

	if(!SYM_ifreeh)
		if(!(SYM_ifreeh = symsrch("ifreeh")))
			error("ifreeh not found in symbol table\n");
	if(!SYM_inode)
		if(!(SYM_inode = symsrch("inode")))
			error("inode not found in symbol table\n");
	for(i = 0; i < vbuf.v_inode; i++)
		inodebuf[i] = 'n';
	for(i = 0; i < vbuf.v_inode; i++) {
		readmem((long)(SYM_inode->n_value+i*sizeof ibuf),1,-1,
			(char *)&ibuf,sizeof ibuf,"inode table");
		if(ibuf.i_vnode.v_count != 0)
			inodebuf[i] = 'u';
	}
	readmem((long)SYM_ifreeh->n_value,1,-1,(char *)&freebuf,
		sizeof freebuf,"ifreeh buffer");
	next = (long)freebuf;
	while(next) {
		i = getslot(next,(long)SYM_inode->n_value,sizeof ibuf,0,vbuf.v_inode);
		readmem((long)(SYM_inode->n_value+i*sizeof ibuf),1,-1,
			(char *)&ibuf,sizeof ibuf,"inode table");
		inodebuf[i] = 'f';
		if(ibuf.i_vnode.v_count != 0)
			inodebuf[i] = 'b';
		next = (long)ibuf.i_freef;
	}
	fprintf(fp,"The following ufs inodes are in use:\n");
	for(i = 0,j = 0; i < vbuf.v_inode; i++) {
		if(inodebuf[i] == 'u') {
			if(j && (j % 10) == 0)
				fprintf(fp,"\n");
			fprintf(fp,"%3d    ",i);
			j++;
		}
	}
	fprintf(fp,"\n\nThe following ufs inodes are on the freelist:\n");
	for(i = 0,j=0; i < vbuf.v_inode; i++) {
		if(inodebuf[i] == 'f') {
			if(j && (j % 10) == 0)
				fprintf(fp,"\n");
			fprintf(fp,"%3d    ",i);
			j++;
		}
	}
	fprintf(fp,"\n\nThe following ufs inodes are on the freelist but have non-zero reference counts:\n");
	for(i = 0,j=0; i < vbuf.v_inode; i++) {
		if(inodebuf[i] == 'b') {
			if(j && (j % 10) == 0)
				fprintf(fp,"\n");
			fprintf(fp,"%3d    ",i);
			j++;
		}
	}
	fprintf(fp,"\n\nThe following ufs inodes are in unknown states:\n");
	for(i = 0,j = 0; i < vbuf.v_inode; i++) {
		if(inodebuf[i] == 'n') {
			if(j && (j % 10) == 0)
				fprintf(fp,"\n");
			fprintf(fp,"%3d    ",i);
			j++;
		}
	}
	fprintf(fp,"\n");
}

/* get inode mode according to file system switch type */
short
getimode()
{
	return(ibuf.i_mode);
}

/* print inode table */
int
prinode(all,full,slot,phys,addr,heading)
int all,full,slot,phys;
long addr;
char *heading;
{
	short mode;
	char ch;

	readbuf(addr,(long)(SYM_inode->n_value+slot*sizeof ibuf),phys,-1,
		(char *)&ibuf,sizeof ibuf,"inode table");
	if(!ibuf.i_vnode.v_count && !all)
			return ;
	if(full)
		fprintf(fp,"%s",heading);
	if(addr != -1)
		slot = getslot(addr,(long)SYM_inode->n_value,sizeof ibuf,phys,
			vbuf.v_inode);
	if(slot == -1)
		fprintf(fp,"  - ");
	else fprintf(fp,"%4d",slot);
	fprintf(fp," %3u,%-3u %6u  %3d %4d %5d %5d %8ld",
		major(ibuf.i_dev),
		minor(ibuf.i_dev),
		ibuf.i_number,
		ibuf.i_vnode.v_count,
		ibuf.i_nlink,
		ibuf.i_uid,
		ibuf.i_gid,
		ibuf.i_size);
	switch((ibuf.i_mode & IFMT)) {
		case IFDIR: ch = 'd'; break;
		case IFCHR: ch = 'c'; break;
		case IFBLK: ch = 'b'; break;
		case IFREG: ch = 'f'; break;
		case IFIFO: ch = 'p'; break;
		case IFLNK: ch = 'l'; break;
		default:    ch = '-'; break;
	}
	fprintf(fp," %c",ch);
	mode = getimode();
	fprintf(fp,"%s%s%s%03o",
		mode & ISUID ? "u" : "-",
		mode & ISGID ? "g" : "-",
		mode & ISVTX ? "v" : "-",
		mode & 0777);
	fprintf(fp," %8x",(long)ibuf.i_vnode.v_vfsp);
	if (ibuf.i_vnode.v_vfsmountedhere)
		fprintf(fp," %8x",(long) ibuf.i_vnode.v_vfsmountedhere);
	else if (ibuf.i_vnode.v_stream)
		fprintf(fp," %8x",(long) ibuf.i_vnode.v_stream);
	else
		fprintf(fp,"  -      ");
	fprintf(fp,"%s%s%s%s%s%s%s%s\n",
		ibuf.i_flag & ILOCKED ? " lk" : "",
		ibuf.i_flag & IUPD ? " up" : "",
		ibuf.i_flag & IACC ? " ac" : "",
		ibuf.i_flag & IMOD ? " md" : "",
		ibuf.i_flag & IWANT ? " wt" : "",
		ibuf.i_flag & ILWAIT ? " lw" : "",
		ibuf.i_flag & ICHG ? " ch" : "",
		ibuf.i_flag & IREF ? " rf" : "");
	if(!full)
		return;
	fprintf(fp,"\tFORW BACK FREF FREB\n");
	slot = ((long)ibuf.i_chain[0] - (long)SYM_inode->n_value) / sizeof ibuf;
	if((slot >= 0) && (slot < vbuf.v_inode))
		fprintf(fp,"\t%4d",slot);
	else fprintf(fp,"\t  - ");
	slot = ((long)ibuf.i_chain[1] - (long)SYM_inode->n_value) / sizeof ibuf;
	if((slot >= 0) && (slot < vbuf.v_inode))
		fprintf(fp," %4d",slot);
	else fprintf(fp,"   - ");
	slot = ((long)ibuf.i_freef - (long)SYM_inode->n_value) / sizeof ibuf;
	if((slot >= 0) && (slot < vbuf.v_inode))
		fprintf(fp," %4d",slot);
	else fprintf(fp,"   - ");
	slot = ((long)ibuf.i_freeb - (long)SYM_inode->n_value) / sizeof ibuf;
	if((slot >= 0) && (slot < vbuf.v_inode))
		fprintf(fp," %4d\n",slot);
	else fprintf(fp,"   - \n");
	fprintf(fp,"\tRMAJ/MIN\tVNODE\n");
	fprintf(fp,"\t %3u,%-3u\t%8x",
		major(ibuf.i_rdev),
		minor(ibuf.i_rdev),
		&ibuf.i_vnode);
	fprintf(fp,"\n");
}


/* find inode in table */
int
find_inode(inode_addr,inode_buf,vfsp,vfs_buf)
	long	*inode_addr;
	struct	inode *inode_buf;
	long	vfsp;
	struct	vfs	*vfs_buf;
{
	struct	mount	mount_buf;
	struct	buf	buf_buf;

	if(!SYM_inode)
		if(!(SYM_inode = symsrch("inode")))
			return(0);

	readbuf((long) vfs_buf->vfs_data,0,0,-1,
		(char *)&mount_buf,sizeof(mount_buf),"ufs mount entry");
	readbuf((long) mount_buf.m_bufp,0,0,-1,
		(char *)&buf_buf,sizeof(buf_buf),"ufs mount buffer header");
	for (*inode_addr = (long) SYM_inode->n_value;
	     ((struct inode *) *inode_addr) <
		(((struct inode *) SYM_inode->n_value) + vbuf.v_inode);
	     *inode_addr = (long) (((struct inode *) (*inode_addr)) + 1)) {
		readbuf(*inode_addr,0,0,-1,
			(char *)inode_buf,sizeof *inode_buf,"inode table");
		if (inode_buf->i_fs == buf_buf.b_un.b_fs &&
		    inode_buf->i_dev == mount_buf.m_dev &&
		    inode_buf->i_number == (ino_t) ROOTINO)
			return(1);
	};
	return(0);
}


int
getfile()
{
	int slot = -1;
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int c;

	optind = 1;
	while((c = getopt(argcnt,args,"epw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'p' :	phys = 1;
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"FILE TABLE SIZE = %d\n",vbuf.v_file);
	fprintf(fp,"SLOT  RCNT  VN/SOC/FL   OFFSET  FLAGS\n");
	if(args[optind]) {
		all = 1;
		do {
			getargs(vbuf.v_file,&arg1,&arg2);
			if(arg1 == -1)
				continue;
			if(arg2 != -1)
				for(slot = arg1; slot <= arg2; slot++)
					prfile(all,slot,phys,addr);
			else {
				if(arg1 < vbuf.v_file)
					slot = arg1;
				else addr = arg1;
				prfile(all,slot,phys,addr);
			}
			slot = addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	}
	else for(slot = 0; slot < vbuf.v_file; slot++)
		prfile(all,slot,phys,addr);
}


/* print file table */
int
prfile(all,slot,phys,addr)
int all,slot,phys;
long addr;
{
	struct file fbuf;
	int inoslot,fileslot;

	readbuf(addr,(long)(File->n_value+slot*sizeof fbuf),phys,-1,
		(char *)&fbuf,sizeof fbuf,"file table");
	if(!fbuf.f_count && !all)
		return;
	if(addr > -1)
		slot = getslot(addr,(long)File->n_value,sizeof fbuf,phys,
			vbuf.v_file);
	if(slot == -1)
		fprintf(fp,"  - ");
	else fprintf(fp,"%4d",slot);
	fprintf(fp,"   %3d", fbuf.f_count);
	if (fbuf.f_count > 0)
		fprintf(fp,"  %c%8x",
			(fbuf.f_type == DTYPE_SOCKET ? 'S' : 'V'),
			(long) fbuf.f_data);
	else {
		fileslot = ((long)fbuf.f_next - File->n_value)/
			(sizeof (struct file));
		if((fileslot >= 0) && (fileslot < vbuf.v_file))
			fprintf(fp,"  F%3d    ",fileslot);
		else fprintf(fp,"    -      ");
	}
	fprintf(fp,"  %8x",fbuf.f_offset);
	fprintf(fp,"%s%s%s%s%s%s%s%s%s%s\n",
		fbuf.f_flag & FREAD ? " read" : "",
		fbuf.f_flag & FWRITE ? " write" : "",  /* print the file flag */
		fbuf.f_flag & FAPPEND ? " appen" : "",
		fbuf.f_flag & FSYNC ? " sync" : "",
		fbuf.f_flag & FNET ? " net" : "",
		fbuf.f_flag & FCREAT ? " creat" : "",
		fbuf.f_flag & FTRUNC ? " trunc" : "",
		fbuf.f_flag & FEXCL ? " excl" : "",
		fbuf.f_flag & FNDELAY ? " ndelay" : "",
		fbuf.f_flag & FNOCTTY ? " noctty" : "",
		fbuf.f_flag & FMARK ? " mark" : "",
		fbuf.f_flag & FDEFER ? " fdefer" : "");
}


struct vnode vnode_buf;

/* get arguments for vnode function */
/* this only lists specific vnodes, since there is no global table */
int
getvnode()
{
	int full = 0;
	int all = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	long next;
	int c;
	char *heading = "ADDR     CNT SHL EXL VFSP     VFS NAME DATA PTR MNTD VFS STREAM   FLAGS\n";

	optind = 1;
	while((c = getopt(argcnt,args,"efpw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'f' :	full = 1;
					break;
			case 'p' :	phys = 1;
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(!full)
		fprintf(fp,"%s",heading);
	if(args[optind]) {
		all = 1;
		do {
			getargs(GETARGS_NO_SLOT,&arg1,&arg2);
			if(arg1 == -1)
				continue;
			if(arg2 != -1)
				for(addr = arg1; addr <= arg2; 
				    addr += sizeof(vnode_buf))
					prvnode(all,full,phys,addr,
						heading,NULL);
			else {
				prvnode(all,full,phys,arg1,heading,NULL);
			}
			addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	} else {
		struct findvnodeargs fva;
		struct	vnode vnode_buf;

		fva = null_findvnodeargs;
		if (all)
			fva.fva_flags |= FVA_ALL;
		while (find_next_vnode(&fva,&vnode_buf))
			prvnode(all,full,phys,(long) fva.fva_vp,heading,
				&vnode_buf);
	};
}


/* print vnodes */
int
prvnode(all,full,phys,addr,heading,vnode_buf_p)
int all,full,phys;
long addr;
char *heading;
struct vnode *vnode_buf_p;
{
	short mode;
	char ch;

	if (vnode_buf_p == NULL) {
		vnode_buf_p = &vnode_buf;
		readbuf(addr,0,phys,-1,
			(char *)vnode_buf_p,sizeof *vnode_buf_p,"vnode table");
	};
	if (vnode_buf_p->v_count == 0 && ! all)
		return;
	if(full)
		fprintf(fp,"%s",heading);
	fprintf(fp,"%8x %3d %3d %3d %8x",
		(long) addr,
		vnode_buf_p->v_count,
		vnode_buf_p->v_shlockc,
		vnode_buf_p->v_exlockc,
		(long)vnode_buf_p->v_vfsp);
	prvfsname(" %8s",(long)vnode_buf_p->v_vfsp,&vnode_buf);
	if (vnode_buf_p->v_data)
		fprintf(fp, " %8x",(long) vnode_buf_p->v_data);
	else
		fprintf(fp, " -       ");
	if (vnode_buf_p->v_vfsmountedhere)
		fprintf(fp," %8x",(long) vnode_buf_p->v_vfsmountedhere);
	else 
		fprintf(fp,"  -      ");
	if (vnode_buf_p->v_stream)
		fprintf(fp," %8x",(long) vnode_buf_p->v_stream);
	else
		fprintf(fp,"  -      ");
	fprintf(fp,"%s%s%s%s%s%s%s%s\n",
		vnode_buf_p->v_flag & VROOT ? " rt" : "",
		vnode_buf_p->v_flag & VTEXT ? " tx" : "",
		vnode_buf_p->v_flag & VEXLOCK ? " ex" : "",
		vnode_buf_p->v_flag & VSHLOCK ? " sh" : "",
		vnode_buf_p->v_flag & VLWAIT ? " wt" : "",
		vnode_buf_p->v_flag & VTEXTMOD ? " tm" : "",
		vnode_buf_p->v_flag & VNOCACHE ? " nc" : "",
		vnode_buf_p->v_flag & VISSWAP ? " sw" : "",
		vnode_buf_p->v_flag & VHASMAP ? " hm" : "");
	if(!full)
		return;
	fprintf(fp,"\tRMAJ/MIN\n");
	fprintf(fp,"\t %3u,%-3u\t%8x",
		major(vnode_buf_p->v_rdev),
		minor(vnode_buf_p->v_rdev));
	fprintf(fp,"\n");
}



prvfsname(fmt,vfsp,vnode_buf_p)
	char	*fmt;
	long	vfsp;
	struct	vnode *vnode_buf_p;
{
	char	fs_name[MAXNAMLEN + 1];
	struct	vfs vfs_buf;

	if (vfsp != (long) NULL)
		readmem(vfsp,1,-1,(char *) &vfs_buf,sizeof vfs_buf,"vfs buffer");
	if (vnode_buf_p != NULL) {
		if (!SYM_spec_vnodeops)
			if (! (SYM_spec_vnodeops = symsrch("spec_vnodeops")))
				error("spec_vnodeops not found in symbol table\n");
		if (((long) vnode_buf_p->v_op) == SYM_spec_vnodeops->n_value) {
			fprintf(fp,fmt,"spec");
			return;
		};		
	};
	if (vfsp == (long) NULL) {
		fprintf(fp,fmt," -");
		return;
	};
	read_vfs_name(fs_name,&vfs_buf);
	fprintf(fp,fmt,fs_name);
}


read_vfs_name(fs_name,vfs_buf)
	char	*fs_name;		/* MAXNAMLEN + 1 bytes long */
	struct	vfs *vfs_buf;
{
	long	vfssw_p;
	long	vfsNVFS;
	struct	vfssw vfssw_buf;

	if(!SYM_vfssw)
		if(!(SYM_vfssw = symsrch("vfssw")))
			error("vfssw not found in symbol table\n");
	if(!SYM_vfsNVFS)
		if(!(SYM_vfsNVFS = symsrch("vfsNVFS")))
			error("vfsNVFS not found in symbol table\n");
	readmem((long)SYM_vfsNVFS->n_value,1,-1,(char *)&vfsNVFS,
		sizeof vfsNVFS,"vfsNVFS buffer");

	for (vfssw_p = (long) SYM_vfssw->n_value;;
	     vfssw_p = (long) (((struct vfssw *) vfssw_p) + 1)) {
		readmem(vfssw_p,1,-1,(char *)&vfssw_buf,
			sizeof vfssw_buf,"vfssw buffer");
		if (vfssw_buf.vsw_name == NULL) {
			strcpy(fs_name,"?");
			break;
		};
		if (vfssw_buf.vsw_ops == vfs_buf->vfs_op) {
			readmem((long)vfssw_buf.vsw_name,1,-1,fs_name,
				MAXNAMLEN,"vfssw vsw_name");
			fs_name[MAXNAMLEN] = 0;
			break;
		};
	};
}

struct syment *SYM_rtable;
struct syment *SYM_stable;

find_next_vnode(fva,vnode_buf)
	struct	findvnodeargs *fva;
	struct	vnode	*vnode_buf;
{
	switch (fva->fva_type) {
	case VFS_TYPE_spec:
		if (!SYM_stable)
			if (! (SYM_stable = symsrch("stable")))
				fva->fva_index = STABLESIZE;
		if (fva->fva_index == -1) {
			fva->fva_sp = NULL;
		};
next_snode:
		while (fva->fva_sp == NULL) {
			if ((++fva->fva_index) >= STABLESIZE)
				break;
			readmem((long) (((struct stable **) 
					   SYM_stable->n_value)
					 + fva->fva_index),1,-1,
				(char *) &fva->fva_sp,
				sizeof(fva->fva_sp),
				"stable");
		};
		if (fva->fva_sp != NULL) {
			struct snode snode_buf;

			readmem((long) fva->fva_sp,1,-1,
				(char *) &snode_buf,
				sizeof(snode_buf),
				"snode entry");
			fva->fva_vp = &(fva->fva_sp->s_vnode);
			fva->fva_sp = snode_buf.s_next;
			if ((fva->fva_flags & FVA_ALL) == 0 &&
			    snode_buf.s_count == 0)
				goto next_snode;
			*vnode_buf = snode_buf.s_vnode;
			return(1);
		};
		if (fva->fva_flags & FVA_SINGLE_TYPE)
			return(0);
		fva->fva_type = VFS_TYPE_ufs;
		fva->fva_index = -1;

	case VFS_TYPE_ufs:
		if(!SYM_inode)
			if(!(SYM_inode = symsrch("inode")))
				fva->fva_index = vbuf.v_inode - 1;
		while ((++fva->fva_index) < vbuf.v_inode) {
			struct	inode	inode_buf;
					
			fva->fva_ip =
				(((struct inode *) SYM_inode->n_value)
						+ fva->fva_index);
			readmem((long) fva->fva_ip,1,-1,
				(char *) &inode_buf,
				sizeof(inode_buf),
				"inode table");
			if (inode_buf.i_vnode.v_count != 0 ||
			    fva->fva_flags & FVA_ALL) {
				fva->fva_vp = &(fva->fva_ip->i_vnode);
				*vnode_buf = inode_buf.i_vnode;
				return(1);
			};
		};
		if (fva->fva_flags & FVA_SINGLE_TYPE)
			return(0);
		fva->fva_type = VFS_TYPE_nfs;
		fva->fva_index = -1;

	case VFS_TYPE_nfs:
		if (!SYM_rtable)
			if (! (SYM_rtable = symsrch("rtable")))
				fva->fva_index = RTABLESIZE;
		if (fva->fva_index == -1) {
			fva->fva_rp = NULL;
		};
		while (fva->fva_rp == NULL) {
			if ((++fva->fva_index) >= RTABLESIZE)
				break;
			readmem((long) (((struct rtable **) 
					   SYM_rtable->n_value)
					 + fva->fva_index),1,-1,
				(char *) &fva->fva_rp,
				sizeof(fva->fva_rp),
				"rtable");
		};
		if (fva->fva_rp != NULL) {
			struct rnode rnode_buf;

			readmem((long) fva->fva_rp,1,-1,
				(char *) &rnode_buf,
				sizeof(rnode_buf),
				"rnode entry");
			fva->fva_vp = &fva->fva_rp->r_vnode;
			fva->fva_rp = rnode_buf.r_hash;
			*vnode_buf = rnode_buf.r_vnode;
			return(1);
		};
		if (fva->fva_flags & FVA_SINGLE_TYPE)
			return(0);
		fva->fva_type = VFS_TYPE_pcfs;
		fva->fva_index = -1;

	case VFS_TYPE_pcfs:
		if (fva->fva_flags & FVA_SINGLE_TYPE)
			return(0);
		fva->fva_type = VFS_TYPE_lo;
		fva->fva_index = -1;

	case VFS_TYPE_lo:
		if (fva->fva_flags & FVA_SINGLE_TYPE)
			return(0);
		fva->fva_type = VFS_TYPE_rfs;
		fva->fva_index = -1;

	case VFS_TYPE_rfs:
		if (fva->fva_flags & FVA_SINGLE_TYPE)
			return(0);
		fva->fva_type = VFS_TYPE_proc;
		fva->fva_index = -1;

	case VFS_TYPE_proc:
		if (fva->fva_flags & FVA_SINGLE_TYPE)
			return(0);
		fva->fva_type = VFS_TYPE_null;
		fva->fva_index = -1;

	case VFS_TYPE_null:
		return(0);
	};
	return(1);
}


struct snode snode_buf;

/* get arguments for snode function */
int
getsnode()
{
	int all = 0;
	int full = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	long next;
	int list = 0;
	struct snode *freebuf;
	int c;
	char *heading = "ADDR     MAJ/MIN  VNODE   RCNT    SIZE  REAL VP  BDEV VP  FLAGS\n";

	optind = 1;
	while((c = getopt(argcnt,args,"efplw:")) !=EOF) {
		switch(c) {
			case 'e' : 	all = 1;
					break;
			case 'f' :	full = 1;
					break;
			case 'p' :	phys = 1;
					break;
			case 'l' :	list = 1;
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if (!SYM_stable)
		if (! (SYM_stable = symsrch("stable")))
			error("stable not found in symbol table\n");
	if(list)
		listsnode();
	else {
		if(!full)
			fprintf(fp,"%s",heading);
		if(args[optind]) {
			do {
				getargs(GETARGS_NO_SLOT,&arg1,&arg2);
				if(arg1 == -1)
					continue;
				if(arg2 != -1)
					for(addr = arg1; addr <= arg2; 
					    addr = (long) (((struct snode *)
							addr) + 1))
						prsnode(full,phys,addr,
							heading);
				else {
					prsnode(full,phys,arg1,heading);
				}
				addr = arg1 = arg2 = -1;
			}while(args[++optind]);
		}
		else {
			struct findvnodeargs fva;
			struct	vnode vnode_buf;

			fva = null_findvnodeargs;
			fva.fva_type = VFS_TYPE_spec;
			fva.fva_flags |= FVA_SINGLE_TYPE;
			if (all)
				fva.fva_flags |= FVA_ALL;
			while (find_next_vnode(&fva,&vnode_buf)) {
				prsnode(full,phys,vnode_buf.v_data,heading);
			};
		}
	}
}


int
listsnode()
{
	struct findvnodeargs fva;
	struct	vnode vnode_buf;
	int	j;

	fprintf(fp,"The following specfs snodes are in use:\n");

	fva = null_findvnodeargs;
	fva.fva_type = VFS_TYPE_spec;
	fva.fva_flags |= FVA_SINGLE_TYPE;
	j = 0;
	while (find_next_vnode(&fva,&vnode_buf)) {
		if(j && (j % 6) == 0)
			fprintf(fp,"\n");
		fprintf(fp,"%8x    ",(long) vnode_buf.v_data);
		j++;
	}
	fprintf(fp,"\n");
}

/* print snode table */
int
prsnode(full,phys,addr,heading)
int full,phys;
long addr;
char *heading;
{
	short mode;
	char ch;

	readbuf(addr,0,phys,-1,
		(char *)&snode_buf,sizeof snode_buf,"snode table");
	if(full)
		fprintf(fp,"%s",heading);
	fprintf(fp,"%8x",addr);
	fprintf(fp," %3u,%-3u %8x  %3d %8x",
		major(snode_buf.s_dev),
		minor(snode_buf.s_dev),
		(long) &(((struct snode *) addr)->s_vnode),
		snode_buf.s_count,
		snode_buf.s_size);
	if (snode_buf.s_realvp)
		fprintf(fp," %8x",(long) snode_buf.s_realvp);
	else
		fprintf(fp,"  -      ");
	if (snode_buf.s_bdevvp)
		fprintf(fp," %8x",(long) snode_buf.s_bdevvp);
	else
		fprintf(fp,"  -      ");
	fprintf(fp,"%s%s%s%s%s%s\n",
		snode_buf.s_flag & SLOCKED ? " lk" : "",
		snode_buf.s_flag & SUPD ? " up" : "",
		snode_buf.s_flag & SACC ? " ac" : "",
		snode_buf.s_flag & SWANT ? " wt" : "",
		snode_buf.s_flag & SCHG ? " ch" : "",
		snode_buf.s_flag & SCLOSING ? " cl" : "");
	if(!full)
		return;
	if (snode_buf.s_next != NULL) 
		fprintf(fp,"\tNEXT = %8x\n",(long)snode_buf.s_next);
	else
		fprintf(fp,"\tNEXT = -\n");
	fprintf(fp,"\tLAST READ = %d\n",(long) snode_buf.s_lastr);
}


struct rnode rnode_buf;

/* get arguments for rnode function */
int
getrnode()
{
	int all = 0;
	int full = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	long next;
	int list = 0;
	int c;
	char *heading = "ADDR     VNODE    OWNER LCNT ROOT VP  FLAGS\n";

	optind = 1;
	while((c = getopt(argcnt,args,"efplw:")) !=EOF) {
		switch(c) {
			case 'e' :	all = 1;
					break;
			case 'f' :	full = 1;
					break;
			case 'p' :	phys = 1;
					break;
			case 'l' :	list = 1;
					break;
			case 'w' :	redirect();
					break;
			default  :	longjmp(syn,0);
		}
	}
	if (!SYM_rtable)
		if (! (SYM_rtable = symsrch("rtable")))
			error("rtable not found in symbol table\n");
	if(list)
		listrnode();
	else {
		if(!full)
			fprintf(fp,"%s",heading);
		if(args[optind]) {
			do {
				getargs(GETARGS_NO_SLOT,&arg1,&arg2);
				if(arg1 == -1)
					continue;
				if(arg2 != -1)
					for(addr = arg1; addr <= arg2; 
					    addr = (long) (((struct rnode *)
							addr) + 1))
						prrnode(full,phys,addr,
							heading);
				else {
					prrnode(full,phys,arg1,heading);
				}
				addr = arg1 = arg2 = -1;
			}while(args[++optind]);
		}
		else {
			struct findvnodeargs fva;
			struct	vnode vnode_buf;

			fva = null_findvnodeargs;
			fva.fva_type = VFS_TYPE_nfs;
			fva.fva_flags |= FVA_SINGLE_TYPE;
			if (all)
				fva.fva_flags |= FVA_ALL;
			while (find_next_vnode(&fva,&vnode_buf)) {
				prrnode(full,phys,vnode_buf.v_data,heading);
			};
		}
	}
}


int
listrnode()
{
	struct findvnodeargs fva;
	struct	vnode vnode_buf;
	int	j;

	fprintf(fp,"The following nfs rnodes are in use:\n");

	fva = null_findvnodeargs;
	fva.fva_type = VFS_TYPE_nfs;
	fva.fva_flags |= FVA_SINGLE_TYPE;
	j = 0;
	while (find_next_vnode(&fva,&vnode_buf)) {
		if(j && (j % 6) == 0)
			fprintf(fp,"\n");
		fprintf(fp,"%8x    ",(long) vnode_buf.v_data);
		j++;
	}
	fprintf(fp,"\n");
}

/* print rnode table */
int
prrnode(full,phys,addr,heading)
int full,phys;
long addr;
char *heading;
{
	short mode;
	char ch;
	long	vfs_addr;
	struct	vfs	vfs_buf;
	long	mnt_addr;
	struct	mntinfo	mntinfo;
	long	rootvp_addr;
	int	i;

	readbuf(addr,0,phys,-1,
		(char *)&rnode_buf,sizeof rnode_buf,"rnode table");
	vfs_addr = (long) rnode_buf.r_vnode.v_vfsp;
	mnt_addr = (long) NULL;
	rootvp_addr = (long) NULL;
	if (vfs_addr != (long) NULL) {
		readbuf(vfs_addr,0,phys,-1,(char *)&vfs_buf, 
			sizeof(vfs_buf),"vfs entry");
		mnt_addr = (long) vfs_buf.vfs_data;
		if (mnt_addr != NULL) {
			readbuf(mnt_addr,0,phys,-1,(char *)&mntinfo,
				sizeof(mntinfo),"NFS mntinfo entry");
			rootvp_addr = (long) mntinfo.mi_rootvp;
		};
	};
	if(full)
		fprintf(fp,"%s",heading);
	fprintf(fp,"%8x %8x ",addr,(long) &(((struct rnode *) addr)->r_vnode));
	fprintf(fp,"%5u %4u ",
		rnode_buf.r_owner,
		rnode_buf.r_count);
	if (rootvp_addr != NULL)
		fprintf(fp,"%8x ",rootvp_addr);
	else
		fprintf(fp," -       ");
	fprintf(fp,"%s%s%s%s%s\n",
		rnode_buf.r_flags & RLOCKED ? " lk" : "",
		rnode_buf.r_flags & RWANT ? " wt" : "",
		rnode_buf.r_flags & RATTRVALID ? " av" : "",
		rnode_buf.r_flags & REOF ? " ef" : "",
		rnode_buf.r_flags & RDIRTY ? " dt" : "");
	if(!full)
		return;
	fprintf(fp,"\tFREEF    FREEB    HASH\n");
	if (rnode_buf.r_freef != NULL) 
		fprintf(fp," %8x",(long)rnode_buf.r_freef);
	else
		fprintf(fp,"  -      ");
	if (rnode_buf.r_freeb != NULL) 
		fprintf(fp," %8x",(long)rnode_buf.r_freeb);
	else
		fprintf(fp,"  -      ");
	if (rnode_buf.r_hash != NULL) 
		fprintf(fp," %8x\n",(long)rnode_buf.r_hash);
	else
		fprintf(fp,"  -      \n");
	fprintf(fp,"\tLAST READ = %d\n",(long) rnode_buf.r_lastr);
	if (mnt_addr != (long) NULL) {
		fprintf(fp,"\tHOST = %d.%d.%d.%d; PORT = %d\n",
			(mntinfo.mi_addr.sin_addr.s_addr >> 24) & 0xff,
			(mntinfo.mi_addr.sin_addr.s_addr >> 16) & 0xff,
			(mntinfo.mi_addr.sin_addr.s_addr >> 8) & 0xff,
			mntinfo.mi_addr.sin_addr.s_addr & 0xff,
			mntinfo.mi_addr.sin_port);
	};
	fprintf(fp,"\tFHANDLE = ");
	for (i = 0; i < NFS_FHSIZE; i++) {
		fprintf(fp,"%02x",(unsigned char) rnode_buf.r_fh.fh_data[i]);
		if ((i % 4) == 3)
			fprintf(fp," ");
	};
	fprintf(fp,"\n");
}
