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
#ident	"$Header: buf.c,v 1.5.1.3.1.1.1.2 91/01/04 17:48:30 beacker Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * This file contains code for the crash functions: bufhdr, buffer, od.
 */

#include "crash.h"

#define BSZ  1		/* byte size */
#define SSZ  2		/* short size */
#define LSZ  4		/* long size */

static struct syment *Buf;      /* namelist symbol pointer */
static char bformat = 'x';	/* buffer format */
static int type = LSZ;		/* od type */
static char mode = 'x';		/* od mode */
char buffer[FSMAXBSIZE];      /* buffer buffer */
struct	buf bbuf;		/* used by buffer for bufhdr */
extern char *ctime();
extern long vtop();
extern long lseek();
extern int pfn_to_byte_shift;


/* get arguments for bufhdr function */
int
getbufhdr()
{
	int slot = -1;
	int full = 0;
	int phys = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int c;
	char *heading = "SLOT MAJ/MIN   BLOCK   ADDRESS  FOR  BCK  AVF  AVB FLAGS\n";

	if(!Buf) {
		struct buf *bp;
		if(!(Buf = symsrch("buf")))
			error("buf not found in symbol table\n");
		readmem(Buf->n_value,1,-1,
			(char *)&bp,sizeof bp,"buf variable");
		if ((unsigned)bp > K0SEG)
			Buf->n_value = (long)bp;
	}

	optind = 1;
	while((c = getopt(argcnt,args,"fpw:")) !=EOF) {
		switch(c) {
			case 'f' :	full = 1;
					break;
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			default  :	longjmp(syn,0);
		}
	}
	fprintf(fp,"BUFFER HEADER TABLE SIZE = %d\n",vbuf.v_buf);
	if(!full)
		fprintf(fp,"%s",heading);
	if(args[optind]) 
		do {
			getargs(- vbuf.v_buf,&arg1,&arg2);
			if(arg1 == -1) 
				continue;
			if(arg2 != -1) {
			  	if (arg1 >= 0 &&
				    arg1 <= vbuf.v_inode) {
					for(slot = arg1; slot <= arg2; slot++)
						prbufhdr(full,slot,phys,addr,heading);
				} else {
					for (addr = arg1; addr <= arg2;
					     addr = (long) (((struct buf *) addr) + 1))
						prbufhdr(full,-1,phys,addr,heading);
				};
			} else {
				if(arg1 >= 0 &&
				   arg1 < vbuf.v_buf)
					slot = arg1;
				else addr = arg1;
				prbufhdr(full,slot,phys,addr,heading);
			}
			slot = addr = arg1 = arg2 = -1;
		}while(args[++optind]);
	else for(slot = 0; slot < vbuf.v_buf; slot++)
		prbufhdr(full,slot,phys,addr,heading);
}


/* print buffer headers */
int
prbufhdr(full,slot,phys,addr,heading)
int full,slot,phys;
long addr;
char *heading;
{
	struct buf bhbuf;
#ifdef V5.3.1
	struct rbuf rbhbuf;
#endif
	register int b_flags;
	extern struct syment *Proc;
	int procslot,forw,back,avf,avb,fforw,fback;

	readbuf(addr,(long)(Buf->n_value+slot*sizeof bhbuf),phys,-1,
		(char *)&bhbuf,sizeof bhbuf,"buffer header");
#ifdef V5.3.1
	if(bhbuf.b_flags & B_REMOTE) {
		readbuf(addr,(long)(Buf->n_value+slot*sizeof rbhbuf),phys,-1,
			(char *)&rbhbuf,sizeof rbhbuf,"buffer header");
		if(full)
			fprintf(fp,"%s",heading);
		if(addr != -1) 
			slot = getslot(addr,(long)Buf->n_value,sizeof rbhbuf,phys,
				vbuf.v_buf);
		if(slot == -1)
			fprintf(fp,"  - ");
		else fprintf(fp,"%4d",slot);
		fprintf(fp,"  - , -  %8x %8x",
			rbhbuf.b_blkno,
			rbhbuf.b_un.b_addr);
		forw = ((long)rbhbuf.b_forw - Buf->n_value)/sizeof rbhbuf;
		if((forw >= 0) && (forw < vbuf.v_buf))
			fprintf(fp," %4d",forw);
		else fprintf(fp,"   - ");
		back = ((long)rbhbuf.b_back - Buf->n_value)/sizeof rbhbuf;
		if((back >= 0) && (back < vbuf.v_buf))
			fprintf(fp," %4d",back);
		else fprintf(fp,"   - ");
		if(!(rbhbuf.b_flags & B_BUSY)) {
			avf = ((long)rbhbuf.av_forw - Buf->n_value)/sizeof rbhbuf;
			if((avf >= 0) && (avf < vbuf.v_buf))
				fprintf(fp," %4d",avf);
			else fprintf(fp,"   - ");
			avb = ((long)rbhbuf.av_back - Buf->n_value)/sizeof rbhbuf;
			if((avb >= 0) && (avb < vbuf.v_buf))
				fprintf(fp," %4d",avb);
			else fprintf(fp,"   - ");
		}
		else fprintf(fp,"   -    - ");
		b_flags = rbhbuf.b_flags;
		fprintf(fp,"%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n",
			b_flags == B_WRITE ? " write" : "",
			b_flags & B_READ ? " read" : "",
			b_flags & B_DONE ? " done" : "",
			b_flags & B_ERROR ? " error" : "",
			b_flags & B_BUSY ? " busy" : "",
			b_flags & B_PHYS ? " phys" : "",
			b_flags & B_MAP ? " map" : "",
			b_flags & B_WANTED ? " wanted" : "",
			b_flags & B_AGE ? " age" : "",
			b_flags & B_ASYNC ? " async" : "",
			b_flags & B_DELWRI ? " delwri" : "",
			b_flags & B_OPEN ? " open" : "",
			b_flags & B_STALE ? " stale" : "",
			b_flags & B_VERIFY ? " verify" : "",
			b_flags & B_FORMAT ? " format" : "",
			b_flags & B_REMOTE ? " remote" : "");
		if(full) {
			fprintf(fp,"\tF_FOR F_BCK  FHANDLE    QUEUE MNTINDX    VCODE  RELTIME\n");
			fforw = ((long)rbhbuf.b_forw - Buf->n_value)/sizeof rbhbuf;
			if((fforw >= 0) && (fforw < vbuf.v_buf))
				fprintf(fp,"\t%5d",fforw);
			else fprintf(fp,"\t  -  ");
			fback = ((long)rbhbuf.b_back - Buf->n_value)/sizeof rbhbuf;
			if((fback >= 0) && (fback < vbuf.v_buf))
				fprintf(fp," %5d",fback);
			else fprintf(fp,"   -  ");
			fprintf(fp," %8x %8x    %4x %8x %8x\n",
				rbhbuf.b_fhandle,
				rbhbuf.b_queue,
				rbhbuf.b_mntindx,
				rbhbuf.b_vcode,
				rbhbuf.b_reltime);
			fprintf(fp,"\n");
			}
	}
	else
#endif
	{
		if(full)
			fprintf(fp,"%s",heading);
		if(addr != -1) 
			slot = getslot(addr,(long)Buf->n_value,sizeof bhbuf,phys,
				vbuf.v_buf);
		if(slot == -1)
			fprintf(fp,"  - ");
		else fprintf(fp,"%4d",slot);
		fprintf(fp," %3u,%-3u %8x %8x",
			major(bhbuf.b_dev)&0377,
			minor(bhbuf.b_dev),
			bhbuf.b_blkno,
			bhbuf.b_un.b_addr);
		forw = ((long)bhbuf.b_forw - Buf->n_value)/sizeof bhbuf;
		if((forw >= 0) && (forw < vbuf.v_buf))
			fprintf(fp," %4d",forw);
		else fprintf(fp,"   - ");
		back = ((long)bhbuf.b_back - Buf->n_value)/sizeof bhbuf;
		if((back >= 0) && (back < vbuf.v_buf))
			fprintf(fp," %4d",back);
		else fprintf(fp,"   - ");
		if(!(bhbuf.b_flags & B_BUSY)) {
			avf = ((long)bhbuf.av_forw - Buf->n_value)/sizeof bhbuf;
			if((avf >= 0) && (avf < vbuf.v_buf))
				fprintf(fp," %4d",avf);
			else fprintf(fp,"   - ");
			avb = ((long)bhbuf.av_back - Buf->n_value)/sizeof bhbuf;
			if((avb >= 0) && (avb < vbuf.v_buf))
				fprintf(fp," %4d",avb);
			else fprintf(fp,"   - ");
		}
		else fprintf(fp,"   -    - ");
		b_flags = bhbuf.b_flags;
		fprintf(fp,"%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n",
			b_flags == B_WRITE ? " write" : "",
			b_flags & B_READ ? " read" : "",
			b_flags & B_DONE ? " done" : "",
			b_flags & B_ERROR ? " error" : "",
			b_flags & B_BUSY ? " busy" : "",
			b_flags & B_PHYS ? " phys" : "",
			b_flags & B_WANTED ? " wanted" : "",
			b_flags & B_AGE ? " age" : "",
			b_flags & B_ASYNC ? " async" : "",
			b_flags & B_DELWRI ? " delwri" : "",
			b_flags & B_PARTIAL ? " partial" : "",
			b_flags & B_TAPE ? " tape" : "",
			b_flags & B_UAREA ? " uarea" : "",
			b_flags & B_PAGET ? " paget" : "",
			b_flags & B_DIRTY ? " dirty" : "",
			b_flags & B_PGIN ? " pgin" : "",
			b_flags & B_CACHE ? " cache" : "",
			b_flags & B_INVAL ? " inval" : "",
			b_flags & B_LOCKED ? " locked" : "",
			b_flags & B_LRU ? " lru" : "",
			b_flags & B_HEAD ? " head" : "",
			b_flags & B_BAD ? " bad" : "",
			b_flags & B_CALL ? " call" : "",
			b_flags & B_NOCACHE ? " nocache" : "",
			b_flags & B_SPECIAL ? " special" : "",
			b_flags & B_FORMAT ? " format" : "",
			b_flags & B_VD_SYNC ? " vd_sync" : "",
			b_flags & B_VDBUF ? " vdbuf" : "",
			b_flags & B_VD_INTERNAL ? " vd_internal" : "",
			b_flags & B_VD_IODONE ? " vd_iodone" : "",
#ifdef V5.3.1
			b_flags & B_REMOTE ? " remote" : "",
#else
			"",
#endif

			b_flags & B_SWAP ? " swap" : "");
		if(full) {
			fprintf(fp,"\tBCNT ERR RESI   START  PROC BSIZ DADR DLEN\n");
			fprintf(fp,"\t%4d %3d %4d %8x",
				bhbuf.b_bcount,
				bhbuf.b_error,
				bhbuf.b_resid,
				bhbuf.b_start);
			procslot = ((long)bhbuf.b_proc - Proc->n_value)/
				sizeof (struct proc);
			if((procslot >= 0) && (procslot < vbuf.v_proc))
				fprintf(fp," %4d",procslot);
			else fprintf(fp,"  - ");
			fprintf(fp," %4d %4x %4d",
				bhbuf.b_bufsize / DEV_BSIZE,
				bhbuf.b_dmaaddr,
				bhbuf.b_dmalen);
			fprintf(fp,"\n");
		}
	}
}

/* get arguments for buffer function */
int
getbuffer()
{
	int slot = -1;
	int phys = 0;
	int fflag = 0;
	long addr = -1;
	long arg1 = -1;
	long arg2 = -1;
	int c;

	if(!Buf) {
		struct buf *bp;
		if(!(Buf = symsrch("buf")))
			error("buf not found in symbol table\n");
		readmem(Buf->n_value,1,-1,
			(char *)&bp,sizeof bp,"buf variable");
		Buf->n_value = (long)bp;
	}

	optind = 1;
	while((c = getopt(argcnt,args,"bcdrxiopw:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 'p' :	phys = 1;
					break;
			case 'b' :	bformat = 'b';
					fflag++;
					break;
			case 'c' :	bformat = 'c';
					fflag++;
					break;
			case 'd' :	bformat = 'd';
					fflag++;
					break;
			case 'x' :	bformat = 'x';
					fflag++;
					break;
			case 'i' :	bformat = 'i';
					fflag++;
					break;
			case 'r' :	bformat = 'r';
					fflag++;
					break;
			case 'o' :	bformat = 'o';
					fflag++;
					break;
			default  :	longjmp(syn,0);
					break;
		}
	}
	if(fflag > 1)
		longjmp(syn,0);
	if(args[optind]) {
		getargs(- vbuf.v_buf,&arg1,&arg2);
		if(arg1 != -1) {
			if(arg1 >= 0 &&
			   arg1 < vbuf.v_buf)
				slot = arg1;
			else addr = arg1;
			prbuffer(slot,phys,addr);
		}
	}
	else longjmp(syn,0);
}


/* print buffer */
int
prbuffer(slot,phys,addr)
int slot,phys;
long addr;
{
	register len;

	readbuf(addr,(long)(Buf->n_value+slot*sizeof bbuf),phys,-1,
		(char *)&bbuf,sizeof bbuf,"buffer");
	len = min(bbuf.b_bufsize, FSMAXBSIZE);
	if(slot > -1)
		fprintf(fp,"BUFFER %d  LENGTH %d:\n", slot, len);
	if (len == 0)
		return;
	readmem((long)bbuf.b_un.b_addr,1,-1,(char *)buffer,
		len,"buffer");
	switch(bformat) {
		case 'b' :      prbalpha(len);
				break;
		case 'c' :      prbalpha(len);
				break;
		case 'd' :      prbnum(len);
				break;
		case 'x' :      prbnum(len);
				break;
		case 'i' :      prbinode(len);
				break;
		case 'r' :      prbdirect(len);
				break;
		case 'o' :      prbnum(len);
				break;
		default  :	error("unknown format\n");
				break;
	}
}

/* print buffer in numerical format */
int
prbnum(len)
register len;
{
	int *ip,i;

	for(i = 0, ip=(int *)buffer; ip !=(int *)&buffer[len]; i++, ip++) {
		if(i % 4 == 0)
			fprintf(fp,"\n%5.5x:\t", i*4);
		fprintf(fp,bformat == 'o'? " %11.11o" :
			bformat == 'd'? " %10.10u" : " %8.8x", *ip);
	}
	fprintf(fp,"\n");
}


/* print buffer in character format */
int
prbalpha(len)
register len;
{
	char *cp;
	int i;

	for(i=0, cp = buffer; cp != &buffer[len]; i++, cp++) {
		if(i % (bformat == 'c' ? 16 : 8) == 0)
			fprintf(fp,"\n%5.5x:\t", i);
		if(bformat == 'c') putch(*cp);
		else fprintf(fp," %4.4o", *cp & 0377);
	}
	fprintf(fp,"\n");
}


/* print buffer in inode format */
int
prbinode(len)
register len;
{
	struct	dinode	*dip;
	long	_3to4();
	int	i,j;

	for(i=1,dip = (struct dinode *)buffer;dip <
		 (struct dinode*)&buffer[len]; i++, dip++) {
	fprintf(fp,"\ni#: +%ld  md: ", i);
		switch(dip->di_mode & IFMT) {
		case IFCHR: fprintf(fp,"c"); break;
		case IFBLK: fprintf(fp,"b"); break;
		case IFDIR: fprintf(fp,"d"); break;
		case IFREG: fprintf(fp,"f"); break;
		case IFIFO: fprintf(fp,"p"); break;
		case IFLNK: fprintf(fp,"l"); break;
		case IFSOCK: fprintf(fp,"s"); break;
		default:    fprintf(fp,"-"); break;
		}
		fprintf(fp,"\n%s%s%s%3x",
			dip->di_mode & ISUID ? "u" : "-",
			dip->di_mode & ISGID ? "g" : "-",
			dip->di_mode & ISVTX ? "t" : "-",
			dip->di_mode & 0777);
		fprintf(fp,"  ln: %u  uid: %u  gid: %u  sz: %ld",
			dip->di_nlink, dip->di_uid,
			dip->di_gid, dip->di_size);
		switch(dip->di_mode & IFMT) {
		case IFCHR:
		case IFBLK:
		case IFIFO:
			fprintf(fp,"\nmaj: %d  min: %1.1o\n",
				major(dip->di_rdev),
				minor(dip->di_rdev));
			break;
		case IFDIR: 
		case IFREG: 
		case IFLNK: 
			for(j = 0; j < NDADDR; j++) {
				if(j % 7 == 0)
					fprintf(fp,"\n");
				fprintf(fp,"db[%d]: %ld  ", j,
					dip->di_db[j]);
			}
			for(j = 0; j < NIADDR; j++) {
				if((j + NDADDR) % 7 == 0)
					fprintf(fp,"\n");
				fprintf(fp,"ib[%d]: %ld  ", j,
					dip->di_ib[j]);
			}
			break;
	
		case IFSOCK:
		default:    break;
		}
		fprintf(fp,"\nat: %s", ctime(&dip->di_atime));
		fprintf(fp,"mt: %s", ctime(&dip->di_mtime));
		fprintf(fp,"ct: %s", ctime(&dip->di_ctime));
	}
	fprintf(fp,"\n");
}


/* print buffer in directory format */
int
prbdirect(len)
register len;
{
	struct	direct	*dp;
	int	i,bad;
	char *cp;

	fprintf(fp,"\n");
	for(i=0, dp =(struct direct*) buffer;
	    (dp < (struct direct *)&buffer[len]) &&
		(((char *) dp->d_name) <= ((char *) &buffer[len]));
	    i++, dp = (struct direct *) (((char *) dp) + dp->d_reclen)) {
		bad = 0;
		for(cp = dp->d_name; cp != &dp->d_name[dp->d_reclen] &&
		    		cp < &buffer[len]; cp++)
			if((*cp < 040 || *cp > 0176) && *cp != '\0')
				bad++;
		fprintf(fp,"d%2d: %5u  ", i, dp->d_ino);
		if(bad) {
			fprintf(fp,"unprintable: ");
			for(cp = dp->d_name; cp < &dp->d_name[dp->d_reclen] &&
				cp < &buffer[len];
				cp++)
				putch(*cp);
		} else fprintf(fp,"%.14s", dp->d_name);
		fprintf(fp,"\n");
	}
}



#ifdef notdef
/* covert 3 byte disk block address to 4 byte address */
long
_3to4(ptr)
register  char  *ptr;
{
	long retval;
	register  char  *vptr;

	vptr = (char *)&retval;
	*vptr++ = 0;
	*vptr++ = *ptr++;
	*vptr++ = *ptr++;
	*vptr++ = *ptr++;
	return(retval);
}
#endif notdef


/* get arguments for od function */
int
getod()
{
	int phys = 0;
	int count = 1;
	int proc = Procslot;
	long addr = -1;
	int c;
	struct syment *sp;
	int typeflag = 0;
	int modeflag = 0;

	optind = 1;
	while((c = getopt(argcnt,args,"tlxcbdohapw:s:")) !=EOF) {
		switch(c) {
			case 'w' :	redirect();
					break;
			case 's' :	proc = setproc();
					break;
			case 'p' :	phys = 1;
					break;
			case 'c' :	mode = 'c';
					if (!typeflag)
						type = BSZ;
					modeflag++;
					break;
			case 'a' :	mode = 'a';
					if (!typeflag)
						type = BSZ;
					modeflag++;
					break;
			case 'x' :	mode = 'x';
					if (!typeflag)
						type = LSZ;
					modeflag++;
					break;
			case 'd' :	mode = 'd';
					if (!typeflag)
						type = LSZ;
					modeflag++;
					break;
			case 'o' :	mode = 'o';
					if (!typeflag)
						type = LSZ;
					modeflag++;
					break;
			case 'h' :	mode = 'h';
					type = LSZ;
					typeflag++;
					modeflag++;
					break;
			case 'b' :	type = BSZ;
					typeflag++;
					break;
			case 't' :	type = SSZ;
					typeflag++;
					break;
			case 'l' :	type = LSZ;
					typeflag++;
					break;
			default  :	longjmp(syn,0);
		}
	}
	if(typeflag > 1) 
		error("only one type may be specified:  b, t, or l\n");	
	if(modeflag > 1) 
		error("only one mode may be specified:  a, c, o, d, or x\n");	
	if(args[optind]) {
		if(*args[optind] == '(') 
			addr = eval(++args[optind]);
		else if(sp = symsrch(args[optind])) 
			addr = sp->n_value;
		else if(isasymbol(args[optind]))
			error("%s not found in symbol table\n",args[optind]);
		else addr = strcon(args[optind],'h');	
		if(addr == -1)
			error("\n");
		if(args[++optind]) 
			if((count = strcon(args[optind],'d')) == -1)
				error("\n");
		prod(addr,count,phys,proc);
	}
	else longjmp(syn,0);
}



/* print dump */
int
prod(addr,count,phys,proc)
long addr;
int count,phys,proc;
{
	int i,j;
	long padr;
	char ch;
	unsigned short shnum;
	long lnum;
	long value;
	char *format;
	int precision;
	char hexchar[16];
	char *cp;
	int nbytes;
	long oldaddr;

/*UH: alignment first, compute each page */
	switch(type) {
		case LSZ :  	if(addr & 0x3) {	/* word alignment */
					fprintf(fp,"warning: long word alignment performed\n");
		    			addr &= ~0x3;
			 	}
				break;
		case SSZ :  	if(addr & 0x1) {	/* word alignment */
					fprintf(fp,"warning: short word alignment performed\n");
		    			addr &= ~0x1;
			 	}
				break;
	}
	if(mode == 'h') {
		cp = hexchar;
		nbytes = 0;
	}
	oldaddr = addr + Ctob(1);
	for(i = 0; i < count; i++) {
		if (btoct(addr) != btoct(oldaddr)) {
			if(!phys) {
				padr = vtop(addr,proc);
				if(padr == -1)
					error("%x is an invalid address\n",addr);
			}
			else padr = addr;
			if(lseek(mem,padr,0) == -1)
				error("%8x is out of range\n",addr);
		}
		switch(type) {
			case BSZ :  if(read(mem,&ch,sizeof (ch)) != sizeof (ch))
					error("read error in buffer\n");
			            value = ch & 0377;
				    break;
			case SSZ : 
				    if(read(mem,(char *)&shnum,
					sizeof (short)) != sizeof (short))
					error("read error in buffer\n");
				    value = shnum;
				    break;	
			case LSZ :
				    if(read(mem,(char *)&lnum,sizeof (long)) !=
					sizeof (long))
					error("read error in buffer\n");
				    value = lnum;
				    break;
		}
		if(((mode == 'c') && ((i % 16) == 0)) ||
			((mode != 'a') && (mode != 'c') && (i % 4 == 0))) {
				if(i != 0) {
					if(mode == 'h') {
						fprintf(fp,"   ");
						for(j = 0; j < nbytes; j++) {
							if(hexchar[j] < 040 ||
							hexchar[j] > 0176)
								fprintf(fp,".");
							else fprintf(fp,"%c",
								hexchar[j]);
						}
						cp = hexchar;
						nbytes = 0;
					}
					fprintf(fp,"\n");
				}
				fprintf(fp,"%8.8x:  ", addr);
			}
		switch(mode) {
			case 'a' :  switch(type) {
					case BSZ :  putc(ch,fp);
						    break;
					case SSZ :  putc((char)shnum,fp);
						    break;
					case LSZ :  putc((char)lnum,fp);
						    break;
				    }
				    break;
			case 'c' :  switch(type) {
					case BSZ :  putch(ch);
						    break;
					case SSZ :  putch((char)shnum);
						    break;
					case LSZ :  putch((char)lnum);
						    break;
				    }
				    break;
			case 'o' :  format = "%.*o   ";
				    switch(type) {
					case BSZ :  precision = 3;
						    break;
					case SSZ :  precision = 6;
						    break;
					case LSZ :  precision = 11;
						    break;
			   		}
			 	    fprintf(fp,format,precision,value);
			 	    break;
			case 'd' :  format = "%.*d   ";
				    switch(type) {
					case BSZ :  precision = 3;
						    break;
					case SSZ :  precision = 5;
						    break;
					case LSZ :  precision = 10;
						    break;
				    }
			 	    fprintf(fp,format,precision,value);
			   	    break;
			case 'x' :  format = "%.*x   ";
				    switch(type) {
					case BSZ :  precision = 2;
						    break;
					case SSZ :  precision = 4;
						    break;
					case LSZ :  precision = 8;
						    break;
				    }
			 	    fprintf(fp,format,precision,value);
				    break;
			case 'h' :  fprintf(fp,"%.*x   ",8,value);
				    *((long *)cp) = value;
				    cp +=4;
				    nbytes += 4;
				    break;
		}
		oldaddr = addr;
		addr += type;
	}
	if(mode == 'h') {
		if(i % 4 != 0)  
			for(j = 0; (j+(i%4)) < 4; j++)
				fprintf(fp,"           ");
		fprintf(fp,"   ");
		for(j = 0; j < nbytes; j++) 
			if(hexchar[j] < 040 || hexchar[j] > 0176)
				fprintf(fp,".");
			else fprintf(fp,"%c",hexchar[j]);
	}
	fprintf(fp,"\n");
}
