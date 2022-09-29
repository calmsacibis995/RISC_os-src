/* $Header: drivers.c,v 1.29.1.4.1.1.1.2 90/10/05 10:02:30 beacker Exp $ */
/* |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

#include <sys/types.h>
#include <sys/param.h>
#include "lboot.h"
#include <a.out.h> /* includes #include <filehdr.h> #include <syms.h> */
#include <ldfcn.h>
#include <sys/sysmacros.h>
#include "boothdr.h"

#include "error.h"

#ifndef MIPSEBMAGIC_2
#define MIPSEBMAGIC_2		0x163
#endif
#define MIPSEBMAGIC_2_OLD	0x150

#ifdef u3b2
#include <sys/sbd.h>

/*
 * Default system devices from VTOC
 */
extern char	*VTOC_devname;		/* driver name associated with boot device */
extern int	VTOC_major;		/* board slot of boot device (major number) */
extern int 	VTOC_minor;		/* device number on controller */
extern int	VTOC_root;		/* partition number */
extern int	VTOC_swap;		/* partition number */
extern int	VTOC_nswap;		/* partition size */
#endif

/*
 * Compute external major number for a device on a supported LBE (the LBE must
 * be at local bus address 14 or 15)
 */
#define	LBEMAJ(lba,elb)	((((lba)-13)<<4) | (elb))

/*
 * Given a major number for a device on a LBE, compute the local bus address 
 * (lba) of its LBE and the extended local bus address (elb) of the device
 */
#define	LBELBA(m)	(13+((m)>>4))
#define	LBEELB(m)	((m)&0x0F)


/*
 * Global variables used during the dynamic loading process
 */

struct kernel *kernel;			/* head of object file linked list */
struct driver *driver;			/* head of struct driver linked list */

int	edt_count;	/* total number of EDT entries */
int	number_drivers; /* total number of drivers to be loaded */


struct rtname {
	char *name;
	char *routine;
} rtname[] = {
	/* functions defined in the UNIX kernel */
	/* these must be in one-to-one correspondence with RNULL,RNOSYS,... */
	/* [RNOTHING]*/	{ "",		"{ }"	},
	/* [RNULL] */	{ "nulldev",	"{ nulldev();}"	},
	/* [RNOSYS] */	{ "nosys",	"{ nosys();}"	},
	/* [RNODEV] */	{ "nodev",	"{ nodev();}"	},
	/* [RTRUE] */	{ "true",	"{ return(1);}"	},
	/* [RFALSE] */	{ "false",	"{ return(0);}"	},
	/* [RFSNULL]*/	{ "fsnull",	"{ return(fsnull());}"	},
	/* [RFSSTRAY]*/	{ "fsstray",	"{ return(fsstray());}"	},
	/* [RNOPKG]*/	{ "nopkg",	"{ nopkg();}"	},
	/* [RNOREACH]*/	{ "noreach",	"{ noreach();}"	},
	/* [RZERO]*/	{ "0",		"{ }"	},
			{  0,		0	}
};

char *Nzero = "0";
char *Nnofile = "nofile";

/*
 * private copies of variables that will be generated for UNIX
 */
int 		fstypcnt;
int             cdevcnt;
int             bdevcnt;
int		fmodcnt;
int 		fsincnt;
short		nfstyp;

dev_t           rootdev = NODEV;
dev_t           pipedev = NODEV;
dev_t           dumpdev = NODEV;
dev_t           swapdev = NODEV;
daddr_t         swplo = -1;
int             nswap = -1;


/*
 * structures used to generate io subsystem code
 */
struct Bdevsw {
	char *d_open;
	char *d_close;
	char *d_strategy;
	char *d_print;
	char *d_dump;
	char *d_size;
};
struct Cdevsw {
	char *d_open;
	char *d_close;
	char *d_read;
	char *d_write;
	char *d_ioctl;
	char *d_ttys;
	char *d_str;
	char *d_mmap;
	char *d_select;
};
struct Fmodsw {
	char *f_name;
	char *f_strmtab;
};

/*file system switch structure */
struct Fstypsw {
/* 0*/	char *fs_init;
/* 1*/	char *fs_iput;
/* 2*/	char *fs_iread;
/* 3*/	char *fs_filler;
/* 4*/	char *fs_iupdat;
/* 5*/	char *fs_readi;
/* 6*/	char *fs_writei;
/* 7*/	char *fs_itrunc;
/* 8*/	char *fs_statf;
/* 9*/	char *fs_namei;
/*10*/	char *fs_mount;
/*11*/	char *fs_umount;
/*12*/	char *fs_getinode;
/*13*/	char *fs_openi;
/*14*/	char *fs_closei;
/*15*/	char *fs_update;
/*16*/	char *fs_statfs;
/*17*/	char *fs_access;
/*18*/	char *fs_getdents;
/*19*/	char *fs_allocmap;
/*20*/	char *fs_freemap;
/*21*/	char *fs_readmap;
/*22*/	char *fs_setattr;
/*23*/	char *fs_notify;
/*24*/	char *fs_fcntl;
/*25*/	char *fs_fsinfo;
/*26*/	char *fs_ioctl;
/*27*/	char *fs_dirlookup;
/*28*/	char *fs_direnter;
/*29*/	char *fs_dirremove;
/*30*/	char *fs_dirinit;
/*31*/	char *fs_dirisempty;
/*32*/	char *fs_bmap;
/*33*/	char *fs_ialloc;
/*34*/	char *fs_idestroy;
/*35*/	char *fs_ifree;
/*36*/	char *fs_setsize;
/*37*/	char *fs_rmount;
/*38*/	char *fs_rumount;
/*39*/  char *fs_fsync;
};

/* FS specific data */
struct Fsinfo {
	char *fs_flags;		/* long fs_flags; */
	char *fs_pipe;		/* struct mount	*fs_pipe; */
	char *fs_name;		/* char *fs_name; */
	char *fs_notfy;		/* long fs_notfy; */
};

struct Bdevsw *bdevswp;
struct Cdevsw *cdevswp;
int Major[256];
struct Fmodsw *fmodswp;
struct Fstypsw *fstypswp;
struct Fsinfo *fsinfop;
char **io_init;
char **next_init;
char **io_pwrclr;
char **next_pwrclr;


/*
 * Static function declarations for this file
 */
static void             build_io_subsys();
static void             dependency();
static struct driver	*finddriver();
static void             print_configuration();


/*
 * finddriver()
 *
 * Search the master directory for master file "name".
 */
 static struct driver *
finddriver(name)
char *name;
{
	register struct master *mp;
	struct driver *dp;
	register i;
	int fd = -1;
	struct stat statbuf;
	FILHDR fhdr;
	SCNHDR shdr;
	ushort filetype;

	dp = NULL;
	mp = NULL;

	if ((dp=(struct driver*)malloc(sizeof(struct driver))) == NULL)
		panic("No memory for driver linked-list\n");

	dp->flag = 0;
	dp->nctl = 0;
	dp->edtp = 0;
	dp->mname = (char *)NULL;
	dp->dname = (char *)NULL;

	if ((dp->opthdr = mp = mkboot(name)) == 0) {
		goto badriver;
	}

	if (mp->flag & KOBJECT)
		/*
		 * ignore kernel objects
		 */
		goto badriver;

	if ((mp->soft > 255) && (mp->soft != DONTCARE)) {
		/* Driver <name>: major number greater than 255 */
		error(ER13, name);
		goto badriver;
	}

	dp->mname = mymalloc( strlen(name) + 1 );
	strcpy(dp->mname,name);

	/*
	 * see if we have an object associated with the master
	 */
		
	dp->dname = mymalloc(strlen(name) + 3);
	(void) strcat(strcpy(dp->dname,name),".o");

	if (stat(dp->dname,&statbuf) == -1) {
		char *p;

		p = strrchr(dp->dname,'.');
		*++p = 'a';
		if (stat(dp->dname,&statbuf) == -1) {
			free(dp->dname);
			dp->dname = (char *) NULL;
			goto nodriver;
		}
	}

	filetype = statbuf.st_mode & S_IFMT;
	if ((filetype == S_IFDIR) || (filetype == S_IFCHR) ||
	    (filetype == S_IFBLK) || (filetype == S_IFIFO) ) {
		goto badriver;
	}

	if ((fd=open(dp->dname,O_RDONLY)) == -1) {
		/* <name>: perror() message */
		error(ER7, dp->dname);
		goto badriver;
	}

	read_and_check(fd, (char*)&fhdr, FILHSZ);
	(void) close(fd);

	switch (fhdr.f_magic) {
	case MIPSEBMAGIC:
	case MIPSEBMAGIC_2:
	case MIPSEBMAGIC_2_OLD:
	case MIPSELMAGIC:
	case MIPSELMAGIC_2:
	case SMIPSEBMAGIC:
	case SMIPSEBMAGIC_2:
	case SMIPSELMAGIC:
	case SMIPSELMAGIC_2:
		break;
	default:
		if (memcmp((char *)&fhdr,"!<arch>\n",8)) {
			/* Driver <name>: not a valid object file */
			error(ER15, dp->dname);
			goto badriver;
		};
		break;
	}

nodriver:
	/*
	 * master is OK
	 */
	dp->next = driver;
	driver = dp;

	return(dp);

badriver:
	if (mp)
		free((char*)mp);
	if (dp->mname)
		free(dp->mname);
	if (dp->dname)
		free(dp->dname);
	if (dp)
		free((char*)dp);

	if (fd != -1) {
		(void) close(fd);
		fd = -1;
	}
	
	return((struct driver *)0);
}


/*
 * Print_configuration()
 *
 * Print the configuration table; this is only done if Debug is set
 */
 static
 void
print_configuration()
{

	register struct driver *dp;
	register struct master *mp;
	register int i;
	register boolean issued;
	char buffer[DIRSIZ+1];

	printf("\nCONFIGURATION SUMMARY\n=====================\n     ----driver---- #devices major\n");
	/*                                                     \n     DIRSIZ__LENGTH nnnnn   nnnn,nnnn */

	if ((dp = driver) != NULL) {
		/*
		 * handle drivers
		 */
		do	{
			if (! (dp->flag & LOAD) ||
			   ((mp=dp->opthdr)->flag & NOTADRV))
				continue;
			if ((mp->flag & (CHAR | BLOCK | FUNMOD | FUNDRV)) == FUNMOD)
				continue;

			strcpy(buffer, dp->mname);
			strncat(buffer, "              ", (int)(sizeof(buffer)-1-strlen(buffer)));

			printf("     %s %s   ", buffer, itoa((int)dp->nctl)+10);

			if (! (mp->flag & (BLOCK|CHAR|FUNDRV)))
				printf("\n");
			else
#ifndef mips
				if (mp->flag & SOFT)
					printf("%s\n", itoa((int)mp->soft)+11);
				else {
					for (i=0; i<dp->nctl; ++i) {
						printf((i+1==dp->nctl)?"%s\n":"%s,", itoa((int)dp->maj[i])+11);
					}
				}
#else mips
				printf("%s\n", itoa((int)mp->soft)+11);
#endif mips
		} while (dp = dp->next);

		/*
		 * handle modules
		 */
		dp = driver;

		issued = FALSE;
		do	{
			mp = dp->opthdr;
			if (!(dp->flag & LOAD) ||
			   !(mp->flag & (NOTADRV | FUNMOD)))
				continue;

			if (! issued) {
				/*
				 * print heading for first module found
				 */
				issued = TRUE;
				printf("\n     ----module----\n");
				/*       \n     DIRSIZ__LENGTH */
			}

			printf("     %s\n", dp->mname);

		} while (dp = dp->next);
	}

	printf("\n");
}

/*
 * Print_driver()
 *
 * Print the kernel and driver linked-list
 */

static void print_master();
static char *print_master_flag();
static char *print_driver_flag();

 void
print_driver(fd)
FILE *fd;
{
	register struct driver *dp;
	register int i;

	fprintf(fd,"\nKernel:\n");

	if (kernel != NULL)
		print_master(fd, kernel->opthdr);

	fprintf(fd, "\nDriver linked-list:\n");

	if ((dp = driver) == NULL || dp->next == driver)
		return;

	do	{
		fprintf(fd, "%9s  #C=%d  #M=%d", dp->mname, dp->nctl, dp->int_major );
		fprintf(fd, "	state=%s\n", print_driver_flag(dp->flag));

		print_master(fd, dp->opthdr);

	} while (dp = dp->next);
}


/*
 * Print_master()
 *
 * Print the master file optional header built by mkboot(1M)
 */
 static
 void
print_master(fd, master)
FILE *fd;
register struct master *master;
{
	register int i, j;


	if (master == NULL)
		return;

	fprintf(fd, "           master flag=");
	fprintf(fd, "%s\n", print_master_flag(master->flag));

	if (master->flag & KOBJECT)
		return;

	fprintf(fd, "           prefix=%s  ext major=%d  ndev=%d\n",
		master->prefix, master->soft, master->ndev);

	if (master->ndep) {
		register struct depend *dp = (struct depend *) POINTER(master->o_depend, master);
		for (i=0; i<master->ndep; ++i, ++dp)
			fprintf(fd, "           dependency=%s\n", POINTER(dp->name,master));
	}

	if (master->nrtn) {
		static char *id[] = { "", "nosys", "nodev", "true", "false", "fsnull", "fsstray", "nopkg", "noreach" };
		register struct routine *rp = (struct routine *) POINTER(master->o_routine, master);
		for (i=0; i<master->nrtn; ++i, ++rp)
			fprintf(fd, "           routine %s() {%s}\n", POINTER(rp->name,master), id[rp->id]);
	}

}


/*
 * print_driver_flag(flag)
 *
 * Print the driver flags symbolically
 */
 static
 char *
print_driver_flag(flag)
register unsigned short flag;
{
	static char buffer[80];

	strcpy(buffer, "");

	if (flag == 0)
		return("none");

	if (flag & LOAD)
		strcat(buffer, ",LOAD");

	if (flag & INEDT)
		strcat(buffer, ",INEDT");

	if (flag & INCLUDE)
		strcat(buffer, ",INCLUDE");

	if (flag & EXCLUDE)
		strcat(buffer, ",EXCLUDE");

	return(buffer+1);
}

/*
 * print_master_flag(flag)
 *
 * Print the master flags symbolically
 */
 static
 char *
print_master_flag(flag)
register unsigned short flag;
{
	static char buffer[80];

	strcpy(buffer, "");

	if (flag == 0)
		return("none");

	if (flag & KOBJECT)
		strcat(buffer, ",KERNEL");

	if (flag & ONCE)
		strcat(buffer, ",ONCE");

	if (flag & REQ)
		strcat(buffer, ",REQ");

	if (flag & BLOCK)
		strcat(buffer, ",BLOCK");

	if (flag & CHAR)
		strcat(buffer, ",CHAR");

	if (flag & FUNDRV)
		strcat(buffer, ",FUNDRV");

	if (flag & REQADDR)
		strcat(buffer, ",REQADDR");

	if (flag & TTYS)
		strcat(buffer, ",TTYS");

	if (flag & SOFT)
		strcat(buffer, ",SOFT");

	if (flag & FUNMOD)
		strcat(buffer, ",FUNMOD");

	if (flag & NOTADRV)
		strcat(buffer, ",NOTADRV");

	if (flag & FSTYP)
		strcat(buffer, ",FSTYP");

	return(buffer+1);
}

/*
 * searchdriver(name)
 *
 * Search the driver linked-list to locate driver `name'; when found, return
 * the pointer to the struct driver entry; if not found return NULL.
 */
 struct driver *
searchdriver(name)
char *name;
{
	register struct driver *dp;

	if ((dp=driver) != NULL)
		do	{
			if (EQUAL(dp->mname,name))
				return(dp);
		} while (dp = dp->next);

	return(finddriver(name));
}

/*
 * Include(dname, number)
 *
 * Mark driver `dname' to be included for `number' more controllers.
 *
 * If the driver was found in the EDT, then `number' is ignored since the
 * number of controllers is already determined.  Likewise, if the driver
 * is a required driver, then the number of controllers must be set to one,
 * so `number' is ignored.
 *
 * If the driver was not found in the EDT, then it cannot be included
 * unless it is a software driver, or it is just an independent module.
 */
 void
include(dname, number)
register char *dname;
int number;
{
	register struct driver *dp;
	register struct master *mp;

	if ((dp=searchdriver(dname)) == NULL)
		/* INCLUDE: <dname>; driver not found */
		error(ER18, dname);
	else {
		if (dp->flag & EXCLUDE) {
			/* INCLUDE: <dname>; driver is EXCLUDED */
			error(ER19, dname);
			return;
		}

		if (dp->flag & INEDT) {
			/*
			 * it will be included based on number of times in EDT
			 */
			dp->flag |= INCLUDE;
			return;
		}

		mp = dp->opthdr;
		if (mp->flag & (NOTADRV | FUNMOD)) {
			dp->flag |= INCLUDE;
			dp->nctl += number;
			return;
		}

		if (mp->flag & SOFT) {
			dp->flag |= INCLUDE;
			if (! (mp->flag & REQ))
				dp->nctl += number;
		} else
			/* INCLUDE: <dname>; device not equipped */
			error(ER20, dname);
	}
}

/*
 * Exclude(dname)
 *
 * Mark driver `dname' to be excluded or ignored
 */
 void
exclude(dname)
register char *dname;
{

	register struct driver *dp;

	if ((dp=searchdriver(dname)) == NULL)
		/*
		 * this may be an EDT entry that is to be ignored
		 */
		ignore(dname);
	else {
		if (dp->flag & INCLUDE)
			/* EXCLUDE: <dname>; driver is INCLUDED */
			error(ER21, dname);
		else
			dp->flag |= EXCLUDE;
	}
}

/*
 * Ignore(name)
 *
 * Remember this EDT device name so that ignoredt() can respond with the correct
 * answer.  This routine is called when the /etc/system EXCLUDE lines are being
 * processed and there is no driver in the driver linked-list for this name.
 *
 * The EXCLUDE lines were not available at the time the driver linked-list was
 * being built, so EDT entries were just bypassed if there was no corresponding
 * driver found in /boot -- it could not be determined whether a driver was
 * missing or the EDT entry was to be ignored.
 *
 * Later, the EDT will be scanned again, and ignoredt() called in order to catch
 * the EDT entries to be ignored.  Diagnostics will then be issued for missing
 * drivers.
 */

struct	ignore {
	struct ignore	*next;
	char		name[DIRSIZ+1];
};

static struct ignore *edtlist;

 void
ignore(name)
char *name;
{
	register struct ignore *ip;

	if ((ip=(struct ignore*)malloc(sizeof(*ip))) == NULL)
		panic("No memory for EXCLUDE list");

	ip->next = edtlist;
	strncat(strcpy(ip->name,""), name, sizeof(ip->name));

	edtlist = ip;
}


/*
 * ignoredt(name)
 *
 * Answer the question: should this entry from the EDT be ignored?
 * Return TRUE if so, FALSE if it is not to be ignored.
 */
 boolean
ignoredt(name)
char *name;
{
	register struct ignore *ip;

	for (ip=edtlist; ip!=NULL; ip=ip->next)
		if (EQUAL(ip->name,name))
			return(TRUE);

	return(FALSE);
}


/*
 * Dependency(pdriver)
 *
 * Driver *pdriver has dependencies.  Find them in the driver linked-list, mark
 * them to be loaded, and follow their dependencies.
 */
 static
 void
dependency(pdriver)
register struct driver *pdriver;
{
	register struct driver *dp;
	register struct master *m, *mp = pdriver->opthdr;
	register struct depend *dep;
	register count;
	char name[DIRSIZ+3];
	char **sp;

	dep = (struct depend*) POINTER(mp->o_depend, mp);

	for (count=0; count<mp->ndep; ++count, ++dep) {

		dp=searchdriver(strcpy(name,(char *)POINTER(dep->name,mp)));

		if (dp==(struct driver *)NULL) {
			/* <pdriver->name>: dependent driver <name> not available */
			error(ER22, pdriver->mname, name);
			continue;
		}

		if (dp->flag & LOAD)
			/*
			 * already marked to be loaded
			 */
			continue;

		if (dp->flag & EXCLUDE) {
			/*
			 * hey! this driver was excluded
			 */
			/* <pdriver->name>: dependent driver <name> is EXCLUDED */
			error(ER23, pdriver->mname, name);
			continue;
		}

		m = dp->opthdr;
		if (!((m->flag & (SOFT|NOTADRV)) ||
		    ((m->flag & (CHAR | BLOCK | FUNMOD | FUNDRV)) == FUNMOD)) &&
		   !(dp->flag & INEDT)) {
			/*
			 * driver is not a software driver (ie. it is a
			 * hardware driver) but the hardware does not exist
			 */
			/* <pdriver->name>: device not equipped for dependent driver <name> */
			error(ER24, pdriver->mname, name);
			continue;
		}

		dp->flag |= LOAD;

		if (m->ndep > 0)
			/*
			 * follow the dependency chain
			 */
			dependency(dp);

		if ((m->flag & (SOFT|NOTADRV) ||
		     ((m->flag & (CHAR | BLOCK | FUNMOD | FUNDRV)) == FUNMOD)) && 
		    ! (dp->flag & INEDT) && dp->nctl == 0)
			/*
			 * make sure that #C is set if not done already
			 */
			dp->nctl = 1;
	}
}

/*
 * loadunix()
 *
 *	This routine links the objects specified by the system list
 */

CONDITION ABORT ={ "ABORT" };

#ifndef MBOOT
char master_dot_o[120];
#endif /* !MBOOT */

 void
loadunix()
{
	register struct driver *dp;
	register struct master *mp;
	register struct dirent *d = NULL;
	DIR *dirp;
	register i;
	int fd = -1;
	int b_major, c_major, bc_major;
	FILHDR fhdr;
	struct stat statbuf;
	FILE *fdot;
	char master_dot_c[120];
#ifdef MBOOT
	static char objlistdflt[] = "objlist";
	char *objlistfile;
	FILE *objlistfd;
#else /* MBOOT */
	char *cc, *ld;
	int ldsize = 0;
#endif /* MBOOT */


	/*
	 * clean out the slash_boot closet
	 */
	dirp = opendir(slash_boot);
	if ( ! dirp)
		panic("cannot open boot directory");

	d = readdir(dirp);
	do	{
		if ((!(strncmp(d->d_name,".m",2))) || (EQUAL(d->d_name,"core")))
			unlink(d->d_name);
	} while ((d=readdir(dirp)) != NULL);
	closedir(dirp);

	/*
	 * Allocate kernel structure and read optional header if present;
	 * note that the driver linked list may not yet be allocated so it
	 * cannot be linked to the kernel list yet.
	 */
	if ((kernel=(struct kernel*)malloc((unsigned int) (sizeof(struct kernel)))) == NULL) {
		printf("No memory for kernel optional header\n");
		goto Exit;
	}

	/*
	 * If the /etc/system file name was found by findsystem(), then it must
	 * be parsed to extract the boot program name (among other things).
	 */
	if (etcsystem) {
		FILE *stream;

		if ((fd=open(etcsystem,O_RDONLY)) == -1) {
			/* <etcsystem>: perror() message */
			error(ER7, etcsystem);
			return;
		}

		fsystem(stream=fdopen(fd,"r"));
		fclose(stream);
	}

	/*
	 * if the kernel object name was not specified in /etc/system, it has
	 * to be gotten now
	 */
	while (! kernel_master)
		System("KERNEL:?");

	/*
	 * open the kernel object file, and do sanity checks
	 */
	kernel_object = malloc(DIRSIZ+1);
	if (kernel_object == (char *)NULL)
		panic("no memory");

	strcat(strcpy(kernel_object,kernel_master),".o");

	if (stat(kernel_object,&statbuf) == -1 || (fd=open(kernel_object,O_RDONLY)) == -1) {
		/* <kernel_object>: perror() message */
		error(ER7, kernel_object);
		return;
	}

	ON(IOERROR)
		goto Exit;

	/*
	 * Read file header and verify it.
	 */
	read_and_check(fd, (char*)&fhdr, FILHSZ);

	switch (fhdr.f_magic) {
	case MIPSEBMAGIC:
	case MIPSEBMAGIC_2:
	case MIPSEBMAGIC_2_OLD:
	case MIPSELMAGIC:
	case MIPSELMAGIC_2:
	case SMIPSEBMAGIC:
	case SMIPSEBMAGIC_2:
	case SMIPSELMAGIC:
	case SMIPSELMAGIC_2:
		break;
	default:
		/* <kernel_object>: not MAC32 magic */
		error(ER32, kernel_object);
		goto Exit;
	}

	if (fhdr.f_nscns == 0) {
		/* <kernel_object>: no section headers */
		error(ER33, kernel_object);
		goto Exit;
	}


/*
 * this is set up now
 * via system("KERNEL:?");
	kernel->opthdr = mp = mkboot(kernel_master);
*/
	mp = kernel->opthdr;
	kernel->flag = 0;
	kernel->nctl = 0;
	kernel->int_major = 0;
	kernel->mname = kernel_master;
	kernel->dname = kernel_object;;

/*
 * this is tested now
 * via system("KERNEL:?");
 */
/*	if ( mp == NULL ) {	*/
/*		printf("no master file %s\n", kernel_master);	*/
/*		goto Exit;	*/
/*	}	*/

/*	if (! (mp->flag & KOBJECT)) {	*/
		/* <kernel_master>: not flagged as KERNEL by mkboot(1M) */
/*		error(ER47, kernel_master);	*/
/*		goto Exit;	*/
/*	}	*/

	close(fd);
	fd = -1;

	ON(ABORT)
		goto Exit;

	/*
	 * if /etc/system file was not processed yet, then it was not
	 * provided or it does not exist; therefore, the driver linked-list
	 * must still be built, and system parameters must be prompted
	 */
	if (! etcsystem) {
		if (driver->next != driver)
			System("INCLUDE:?\nEXCLUDE:?");
	}

	if (driver->next == driver) {
		/*
		 * no driver linked-list could be built -- quit
		 */
		panic("no driver linked-list was built");
	}

	/*
	 * link the driver linked-list to the kernel data structure
	 */
	kernel->next = driver;

#ifdef u3b2
	/*
	 * if the system file was provided AND the system devices were not
	 * specified AND the boot device contained a vtoc AND the vtoc
	 * contained flagged partitions for root or swap, THEN use these
	 * defaults:
	 *
	 *	rootdev - boot device
	 *	pipedev - rootdev
	 *	swapdev - boot device
	 */
	if (etcsystem && VTOC_devname && (dp=searchdriver(VTOC_devname)) != NULL && (mp=dp->opthdr)->flag & BLOCK) {
		if (mp->flag & SOFT)
			/*
			 * major number is not the board slot
			 */
			VTOC_major = mp->soft;

		if (rootdev == NODEV && VTOC_root != -1) {
			rootdev = makedev(VTOC_major, VTOC_minor * 16 + VTOC_root);
		}
		if (pipedev == NODEV && rootdev != NODEV)
			pipedev = rootdev;
		if (swapdev == NODEV && VTOC_swap != -1) {
			swapdev = makedev(VTOC_major, VTOC_minor * 16 + VTOC_swap);
			swplo = 0;
			nswap = VTOC_nswap;
		}
	}
#endif

	/*
	 * make sure we have everything we need
	 */
	while (rootdev == NODEV)
		System("ROOTDEV:?");

	while (pipedev == NODEV)
		System("PIPEDEV:?");

	while (dumpdev == NODEV)
		System("DUMPDEV:?");

	while (swapdev == NODEV || swplo == -1 || nswap == -1)
		System("SWAPDEV:? ? ?");

	/*
	 * determine all drivers to be loaded
	 */
	dp = driver;
	do	{
		if ((mp=dp->opthdr)->flag & REQ) {
			if (dp->flag & EXCLUDE) {
				/* <dp->mname>: required driver is EXCLUDED */
				error(ER36, dp->mname);
				continue;
			}

			dp->flag |= LOAD;
			dp->nctl = 1;
		}

		if (dp->flag & EXCLUDE)
			continue;

		if (dp->flag & INEDT)
			dp->flag |= LOAD;;

		if (dp->flag & INCLUDE)
			dp->flag |= LOAD;

		if (dp->flag & LOAD && mp->ndep > 0)
			dependency(dp);

	} while (dp = dp->next);

	/*
	 * Assign the internal major numbers.  This is a two pass approach,
	 * first the drivers which are both BLOCK and CHAR are assigned numbers
	 * then the remaining drivers are assigned.  This will minimize the size
	 * of the character and block device switch tables.
	 */

	for (i=0; i<256; ++i)
		Major[i] = DONTCARE;

	for (bc_major=0, i=0; i<2; b_major=c_major=bc_major, ++i) {

#ifdef MULTIPLE_MAJOR
	        int nummaj, cnt;
#endif

		dp = driver;
		do {
			if (! (dp->flag & LOAD))
				continue;

			mp = dp->opthdr;
			if (!(mp->flag & (BLOCK|CHAR|FUNDRV))) /* SOFT? */
				continue;

#ifdef MULTIPLE_MAJOR
			if (mp->ncontmaj == 0 ||
				    mp->ncontmaj == DONTCARE)
			  nummaj = 1;
			else
			  nummaj = ( (dp->nctl-1) / mp->ncontmaj ) + 1;
#endif

			if (i == 0) {
				if ((mp->flag & (BLOCK|CHAR)) == (BLOCK|CHAR)){
#ifdef MULTIPLE_MAJOR
					if (nummaj > 1) {
					    dp->int_major = 0;
					    for (cnt=0; cnt < nummaj; cnt++)
					      dp->mint_major[cnt] = bc_major++;
					} else {
					    dp->mint_major[0] = bc_major;
#endif
					    dp->int_major = bc_major++;
#ifdef MULTIPLE_MAJOR
					}
#endif
				    } else
					continue;
			} else {
				if ((mp->flag & (BLOCK|CHAR)) == BLOCK) {
#ifdef MULTIPLE_MAJOR
					if (nummaj > 1) {
					    dp->int_major = 0;
					    for (cnt=0; cnt < nummaj; cnt++)
					      dp->mint_major[cnt] = b_major++;
					} else {
					    dp->mint_major[0] = b_major;
#endif
					    dp->int_major = b_major++;
#ifdef MULTIPLE_MAJOR
					}
#endif
				} else
					if ((mp->flag & (BLOCK|CHAR)) == CHAR || mp->flag & FUNDRV) {
#ifdef MULTIPLE_MAJOR
					if (nummaj > 1) {
					    dp->int_major = 0;
					    for (cnt=0; cnt < nummaj; cnt++)
					      dp->mint_major[cnt] = c_major++;
					} else {
					    dp->mint_major[0] = c_major;
#endif
					    dp->int_major = c_major++;
#ifdef MULTIPLE_MAJOR
					}
#endif
				} else
					continue;
			}
#ifdef MULTIPLE_MAJOR
			if ( (nummaj == 1) && (mp->soft != DONTCARE) ) {
#else
			if (mp->soft != DONTCARE) {
#endif
				if (Major[mp->soft] != DONTCARE)
					printf("Warning: major number collision -- %d\n", mp->soft);
				Major[mp->soft] = dp->int_major;
			}
#ifdef MULTIPLE_MAJOR
			if (nummaj > 1) {
			    for (cnt=0; cnt < nummaj; cnt++) {
			      if (Major[mp->msoft[cnt]] != DONTCARE)
				printf("Warning: major number collision --- %d\n", mp->msoft[cnt]);
			    
			      Major[mp->msoft[cnt]] = dp->mint_major[cnt];
 			    }
			}
#endif

		} while (dp = dp->next);
	}

	dp = driver;
	do	{
		int index = 0;

		if (! (dp->flag & LOAD))
			continue;

#ifdef MULTIPLE_MAJOR
/*
 * We are not going to deal with assigning major numbers for people when
 * they require multiple major numbers.  I think this is fair...
 */
#endif

		mp = dp->opthdr;
		if (!(mp->flag & (BLOCK|CHAR|FUNDRV))) /* SOFT? */
			continue;
#ifdef MULTIPLE_MAJOR
		if (mp->msoft[0] == DONTCARE && mp->soft == DONTCARE) {
#else
		if (mp->soft == DONTCARE) {
#endif
			while (Major[index] != DONTCARE) {
				index++;
				if (index > 256)
					panic("major table overflow");
			}
			mp->soft = index;
			Major[index] = dp->int_major;
		}

	} while (dp = dp->next);

	if (Debug) {
		FILE *df;
		char dstring[20];

		(void) sprintf(dstring, "/tmp/df%d.c", getpid());
		df = fopen(dstring, "w");
		if (df == (FILE *)NULL) {
			panic("cannot open dump-master file");
		}
		print_driver(df);
		fclose(df);
	}

	/*
	 * Compute cdevcnt, bdevcnt, and fmodcnt; also count the total 
	 * number of drivers to be loaded, and interrupt routines that 
	 * will be needed
	 */

	dp = driver;
	do	{
	  	int	nummaj;
		int	cnt;

		if (!(dp->flag & LOAD))
			continue;

		++number_drivers;

#ifndef MBOOT
		if (dp->dname)
			ldsize += strlen(dp->dname);
#endif /* !MBOOT */

		if ((mp=dp->opthdr)->flag & ONCE && dp->nctl != 1) {
			/* <dp->mname>: flagged as ONCE only; #C set to 1 */
			error(ER37, dp->mname);
			dp->nctl = 1;
		}

		if (mp->flag & FSTYP) {
			++fstypcnt;
			++fsincnt;
			++nfstyp;
		}

		if (mp->flag & NOTADRV)
			continue;

		if (mp->flag & FUNMOD) {
			++fmodcnt;
			if (!(mp->flag & (CHAR | BLOCK | FUNDRV)))
				continue;
		}

#ifdef MULTIPLE_MAJOR
		if (mp->ncontmaj == 0 ||
		    mp->ncontmaj == DONTCARE)
			nummaj = 1;
		else
			nummaj = ( (dp->nctl-1) / mp->ncontmaj ) + 1;
#endif MULTIPLE_MAJOR
		if (mp->flag & (CHAR | FUNDRV)) {
#ifdef MULTIPLE_MAJOR
			if (nummaj > 1) {
				for (cnt = 0; cnt < nummaj; cnt++)
					cdevcnt = max((long)cdevcnt,
					      (long)(dp->mint_major[cnt]));
			} else
#endif MULTIPLE_MAJOR
				cdevcnt = max((long)cdevcnt,
					      (long)(dp->int_major));
		} 
		if (mp->flag & BLOCK) {
#ifdef MULTIPLE_MAJOR
			if (nummaj > 1) {
				for (cnt = 0; cnt < nummaj; cnt++)
					bdevcnt = max((long)bdevcnt,
					      (long)(dp->mint_major[cnt]));
			} else
#endif MULTIPLE_MAJOR
				bdevcnt = max((long)bdevcnt,
					      (long)(dp->int_major));
		};
	} while (dp = dp->next);

#ifndef MBOOT
	ldsize += number_drivers;
#endif /* !MBOOT */
	++fstypcnt;
	++fsincnt;
	++nfstyp;
	++cdevcnt;
	++bdevcnt;

	/*
	 * Print configuration table
	 */
	if (Debug)
		print_configuration();

	/*
	 * build the "master.c" file -- this defines all constants
	 * and will instantiate all extern structures.
	 */

#ifdef MBOOT
	(void) sprintf(master_dot_c, "%s/master.c", master_dot_d);
#else /* MBOOT */
	(void) sprintf(master_dot_c, "%s/.master%d.c", slash_boot,  getpid());

	{
		char *p;

		p = strrchr(strcpy(master_dot_o,master_dot_c), '.');
		*++p = 'o';	/* replace ".c" with ".o" */
	}
#endif /* MBOOT */

	if ((fdot=fopen(master_dot_c,"w")) == (FILE *)NULL)
		panic("cannot create internal master");

#ifdef MBOOT
	objlistfile = NULL;	/* add option to specify objlist file?? */
	if (objlistfile == NULL)
		objlistfile = objlistdflt;
	if ((objlistfd = fopen(objlistfile,"w")) == NULL) {
		printf("could not open %s\n", objlistfile);
		exit(2);
	}
#else /* MBOOT */
	cc = mymalloc(30+strlen(root)+strlen(ccopts)+strlen(master_dot_c));
	
	(void) strcat(strcat(strcat(strcat(strcpy(cc,"cc -c -I -I"),root),"/usr/include "),ccopts),master_dot_c);

	ld = mymalloc(	ldsize +
			strlen(ldopts) +
			strlen(kernel_object) +
			strlen(master_dot_o) +
			8 );

	(void) strcat(strcpy(ld,"ld "),ldopts);
#endif /* MBOOT */
	
	dp = (struct driver *) kernel;

	dump_source(dp, fdot);
#ifndef MBOOT
	(void) strcat(strcat(ld, dp->dname), " ");
	(void) strcat(strcat(ld, master_dot_o), " ");
#endif /* !MBOOT */

	dp = driver;
	do	{

		if (! (dp->flag & LOAD))
			continue;

		dump_source(dp, fdot);

	} while (dp = dp->next);

	/*
	 * Put .o's before .a's ("except after ``c''...")
	 */
	dp = driver;
	do	{

		if (! (dp->flag & LOAD))
			continue;

		if (dp->dname)
			if (suffix(dp->dname, "o")) {
#ifdef MBOOT
			    fprintf(objlistfd, "%s/%s ", slash_boot, dp->dname);
#else /* MBOOT */
				(void) strcat(strcat(ld, dp->dname), " ");
#endif /* MBOOT */
			}

	} while (dp = dp->next);

	dp = driver;
	do	{

		if (! (dp->flag & LOAD))
			continue;

		if (dp->dname)
			if (suffix(dp->dname, "a")) {
#ifdef MBOOT
			    fprintf(objlistfd, "%s/%s ", slash_boot, dp->dname);
#else /* MBOOT */
				(void) strcat(strcat(ld, dp->dname), " ");
#endif /* MBOOT */
			}
	} while (dp = dp->next);

	fflush(fdot);

	/*
	 * Build all interrupt routines, pcb's,
	 * kernel data structures for I/O, etc.
	 */
	build_io_subsys(fdot);

	fflush(fdot);

#ifdef MBOOT
	fclose(objlistfd);
	exit(0);
#else /* MBOOT */
	/*
	 * Compile local master_dot_c...
	 */
	if (Verbose) {
		printf("%s\n", cc);
		fflush(stdout);
	}

	if ((i=system(cc)) != 0) {
		printf("lboot: compile returned %d, exiting\n", i);
		exit(i);
	}

	/*
	 * link master_dot_o, kernel_object, all ``loaded'' drivers
	 */
	if (Verbose) {
		printf("%s\n", ld);
		fflush(stdout);
	}
	exit(system(ld));
#endif /* MBOOT */


Exit:
	/*
	 * Error exits; cleanup and return
	 */
	if (fd != -1)
		close(fd);
	fclose(fdot);
	exit(1);
}


extern char master_file[];		/* current master file entity */
#define LSIZE 1024

/*
 * dump "parameters" sections of master.d file for "name"
 */
dump_source(dp, file)
struct driver *dp;
FILE *file;
{
	FILE *master;
	register c;
	char *p;
#ifdef MULTIPLE_MAJOR
	char sCDM[80];
	int nummaj;
#else
	char sCDM[12];
#endif
	char line[LSIZE];
	int nCDM;

#ifdef MULTIPLE_MAJOR
	/*
	 * Calculate the number of major numbers used by this device.
	 * This will be used if we get a ##N, ##I or ##X.
	 */
	if (dp->opthdr->ncontmaj == 0 || dp->opthdr->ncontmaj == DONTCARE)
	  nummaj = 1;
	else
	  nummaj = ( ( dp->nctl-1 ) / dp->opthdr->ncontmaj ) + 1;
#endif

	strcpy( master_file, master_dot_d );
	strcat( master_file, "/" );
	strcat( master_file, dp->mname );

	p=strrchr(master_file,'/');
	if ( (p=strchr(p,'.')) != NULL )
		*p = '\0';

	if ((master = fopen(master_file,"r")) == NULL) {
		printf("could not open %s\n", master_file);
		exit(2);
	}
	while ((fgets(line, LSIZE, master)) != NULL) {
		if (line[0] == '*')
			continue;
		if (line[0] == '$')
			break;
	}
	while ((c=getc(master)) != EOF) {
		if (c == '#') {
			if ((c=getc(master)) == '#') {
				switch(c=getc(master)) {

				case 'C':
				case 'c':
				        sprintf(sCDM,"%d",dp->nctl);
					break;
				case 'D':
				case 'd':
					sprintf(sCDM,"%d",dp->opthdr->ndev);
					break;
				case 'M':
				case 'm':
					sprintf(sCDM,"%d",dp->int_major);
					break;
				case 'E':
				case 'e':
					sprintf(sCDM,"%d",dp->opthdr->soft);
					break;
#ifdef MULTIPLE_MAJOR
				case 'N':	/* Number of majors used */
				case 'n':	
					sprintf(sCDM,"%d",nummaj);
					break;
				case 'P':
				case 'p':	/* Number of cont/major */
					/*
					 * If we only have one major, number of
					 * controller is the number of cont/major
					 */
					if (dp->opthdr->ncontmaj == DONTCARE)
						sprintf(sCDM,"%d",dp->nctl);
					else
						sprintf(sCDM,"%d",dp->opthdr->ncontmaj);
					break;
				case 'I':
				case 'i':
					{
					    int xxx;
					    char tmpstr[5];
					    strcpy(sCDM,"{ ");
					    for (xxx=0; xxx<nummaj; xxx++) {
						if (xxx != 0)
						  strcat(sCDM,", ");
						sprintf(tmpstr,"%d",
							dp->mint_major[xxx]);
						strcat(sCDM,tmpstr);
					    }
					    strcat(sCDM," }");
					}
					break;

				case 'X':
				case 'x':
					{
					    int xxx;
					    char tmpstr[5];
					    strcpy(sCDM,"{ ");
					    for (xxx=0; xxx<nummaj; xxx++) {
						if (xxx != 0)
						  strcat(sCDM,", ");
						sprintf(tmpstr,"%d",
							dp->opthdr->msoft[xxx]);
						strcat(sCDM,tmpstr);
					    }
					    strcat(sCDM," }");
					}
					break;
#endif MULTIPLE_MAJOR

				default:
					fprintf(stderr,"data initializer ##%c unknown -- using zero\n", c);
					sprintf(sCDM,"%d",0);
					break;
				}
				        p = sCDM;
				        while (*p)
					    putc(*p++,file);
				} else	{
					putc('#',file);
					putc(c,file);
				}
		} else putc(c,file);
	}
	(void) fclose(master);
}


static void
build_io_subsys(f)
FILE *f;
{
	struct master *mp;
	struct driver *dp;

	/*
	 * generate structures and set up default values
	 */
	initialize_subsys();

	dp = driver;

	do {
		if (! (dp->flag & LOAD))
			continue;

		mp = dp->opthdr;

		/*
		 * fill in tables with symbol table values
		 */
		if (dp->dname)
			load_values(dp);
		else
			printf("Warning: no object file for %s\n", dp->mname);

	} while (dp = dp->next);

	/*
	 * copy the generated data into the temp file
	 */
	generate(f);
}

/*
 * initialize structures for the i/o subsystem
 */
initialize_subsys()
{
	register i;
	struct Bdevsw *bp;
	struct Cdevsw *cp;
	struct Fmodsw *fp;
	struct Fstypsw *fs;
	struct Fsinfo *fo;

	bdevswp = (struct Bdevsw *) malloc(bdevcnt*sizeof(struct Bdevsw));
	if (bdevswp == (struct Bdevsw *) NULL) {
		panic("no space for device tables -- quitting\n");
	}
	for (i=0, bp=bdevswp; i<bdevcnt; i++, bp++) {
		bp->d_open = bp->d_close =
		bp->d_strategy = bp->d_print = 
		bp->d_dump = bp->d_size = rtname[RNODEV].name;
	}

	cdevswp = (struct Cdevsw *) malloc(cdevcnt*sizeof(struct Cdevsw));
	if (cdevswp == (struct Cdevsw *) NULL) {
		panic("no space for device tables -- quitting\n");
	}
	for (i=0, cp=cdevswp; i<cdevcnt; i++, cp++) {
		cp->d_open = cp->d_close =
		cp->d_read = cp->d_write = cp->d_ioctl = rtname[RNODEV].name;
		cp->d_ttys = cp->d_str = cp->d_mmap = cp->d_select = Nzero;
	}

	fmodswp = (struct Fmodsw *) malloc(fmodcnt*sizeof(struct Fmodsw));
	if (fmodswp == (struct Fmodsw *) NULL) {
		panic("no space for stream module tables -- quitting\n");
	}
	for (i=0, fp=fmodswp; i<fmodcnt; i++, fp++) {
		fp->f_name = Nnofile;
		fp->f_strmtab = Nzero;
	}

	fstypswp = (struct Fstypsw *) calloc((fstypcnt+1)*sizeof(struct Fstypsw),1);
	if (fstypswp == (struct Fstypsw *) NULL) {
		panic("no space for file system switch tables -- quitting\n");
	}
	/*
	 * just initialize fstypsw[0] --
	 * load_values() will get the rest
	 */
	fs=fstypswp;
/* 0*/	fs->fs_init =
/* 1*/	fs->fs_iput =
/* 2*/	fs->fs_iread =
/* 3*/	fs->fs_filler =
/* 4*/	fs->fs_iupdat =
/* 5*/	fs->fs_readi = 
/* 6*/	fs->fs_writei =
/* 7*/	fs->fs_itrunc =
/* 8*/	fs->fs_statf =
/* 9*/	fs->fs_namei =
/*10*/	fs->fs_mount =
/*11*/	fs->fs_umount =
/*12*/	fs->fs_getinode =
/*13*/	fs->fs_openi =
/*14*/	fs->fs_closei =
/*15*/	fs->fs_update =
/*16*/	fs->fs_statfs =
/*17*/	fs->fs_access = 
/*18*/	fs->fs_getdents =
/*19*/	fs->fs_allocmap =
/*20*/	fs->fs_freemap =
/*21*/	fs->fs_readmap =
/*22*/	fs->fs_setattr =
/*23*/	fs->fs_notify =
/*24*/	fs->fs_fcntl =
/*25*/	fs->fs_fsinfo =
/*26*/	fs->fs_ioctl =
/*27*/	fs->fs_dirlookup =
/*28*/	fs->fs_direnter =
/*29*/	fs->fs_dirremove=
/*30*/	fs->fs_dirinit =
/*31*/	fs->fs_dirisempty =
/*32*/	fs->fs_bmap =
/*33*/	fs->fs_ialloc =
/*34*/	fs->fs_idestroy =
/*35*/	fs->fs_ifree =
/*36*/	fs->fs_setsize =
/*37*/	fs->fs_rmount =
/*38*/	fs->fs_rumount = 
/*39*/  fs->fs_fsync = rtname[RFSSTRAY].name;

	fsinfop = (struct Fsinfo *) calloc(fsincnt*sizeof(struct Fsinfo),1);
	if (fsinfop == (struct Fsinfo *) NULL) {
		panic("no space for file system info table -- quitting\n");
	}
	fo=fsinfop;
	fo->fs_flags = fo->fs_pipe = fo->fs_notfy = Nzero;
	fo->fs_name = "\"\"";

	io_init = next_init = (char **) calloc((number_drivers+1)*(sizeof(io_init)+sizeof(io_pwrclr)), 1);
	if (io_init == (char **) NULL) {
		panic("no space for initialization arrays -- quitting");
	}
	io_pwrclr = next_pwrclr = io_init + (number_drivers+1);
}


/*
 * fill in correct values for generated structures
 */
load_values(dp)
struct driver *dp;
{
	struct driver *ddp;
	struct master *mp, *mmp;
	LDFILE *ldptr;
	SYMR symr;
	long imax, ibase, isym;
	char *np, *ldgetname();
	register struct Bdevsw *b;
	register struct Cdevsw *c;
	register struct Fstypsw *fs;
	register struct Fsinfo *fo;
	register struct Fmodsw *fp;
	int got_init, got_clr;	/* kludge -- myt FIXTHIS */
#ifdef MULTIPLE_MAJOR
	int nummaj, cnt;
#endif

	got_init = got_clr = 0;

	mp = dp->opthdr;
#ifdef MULTIPLE_MAJOR
	if (mp->ncontmaj == 0 || mp->ncontmaj == DONTCARE)
	  nummaj = 1;
	else
	  nummaj = ( (dp->nctl-1) / mp->ncontmaj ) + 1;

/* The end of this for statement is toward the end of the routine.  If
 * is surrounded by #ifdef MULTIPLE_MAJOR
 */

	for (cnt=0; cnt < nummaj; cnt++) {

	    if (nummaj > 1) {
		b = bdevswp+Major[mp->msoft[cnt]];
		c = cdevswp+Major[mp->msoft[cnt]];
	    } else {
		if (nummaj != 1)
		  fprintf(stderr,"load_values: nummaj is %d, should be 1.\n",
			  nummaj);
 		b = bdevswp+Major[mp->soft];
		c = cdevswp+Major[mp->soft];
	    }
#else
 		b = bdevswp+Major[mp->soft];
		c = cdevswp+Major[mp->soft];
#endif
	if (mp->flag & FSTYP) {
		for (fs=fstypswp; fs->fs_init != (char *)0; fs++)
			;
		fs->fs_init = fs->fs_iput =  fs->fs_iread =
		fs->fs_filler = fs->fs_iupdat = fs->fs_readi = 
		fs->fs_writei = fs->fs_itrunc = fs->fs_statf =
		fs->fs_namei = fs->fs_mount = fs->fs_umount =
		fs->fs_getinode = fs->fs_openi = fs->fs_closei =
		fs->fs_update = fs->fs_statfs = fs->fs_access = 
		fs->fs_getdents = fs->fs_allocmap = fs->fs_freemap =
		fs->fs_readmap = fs->fs_setattr = fs->fs_notify =
		fs->fs_fcntl = fs->fs_fsinfo = fs->fs_ioctl =
		fs->fs_dirlookup = fs->fs_direnter =
  		fs->fs_dirremove= fs->fs_dirinit = fs->fs_dirisempty =
  		fs->fs_bmap = fs->fs_ialloc = fs->fs_idestroy =
  		fs->fs_ifree = fs->fs_setsize =
 		fs->fs_rmount = fs->fs_rumount =
		fs->fs_fsync = rtname[RFSSTRAY].name;

		for (fo=fsinfop; fo->fs_name != (char *)0; fo++)
			;
		fo->fs_flags = strcat(strcpy(mymalloc(strlen(mp->prefix)+6),mp->prefix),"FLAGS");
		fo->fs_pipe = strcat(strcpy(mymalloc(strlen(mp->prefix)+6),mp->prefix),"PIPE");
		fo->fs_notfy = strcat(strcpy(mymalloc(strlen(mp->prefix)+6),mp->prefix),"NOTFY");
		fo->fs_name = strcat(strcpy(mymalloc(strlen(mp->prefix)+6),mp->prefix),"NAME");

	}

	/*
	 * inviolate names
	 */

	if (mp->flag & TTYS) {
		c->d_ttys = strcat(strcpy(mymalloc(strlen(mp->prefix)+5),mp->prefix),"_tty");
	}

	if (mp->flag & FUNMOD) {
		for (fp=fmodswp; strcmp(fp->f_name,"nofile"); fp++)
			;
		fp->f_name = dp->mname;
		fp->f_strmtab = strcat(strcpy(mymalloc(strlen(mp->prefix)+5),mp->prefix),"info");
	}

	if (mp->flag & FUNDRV) {
		c->d_str = strcat(strcpy(mymalloc(strlen(mp->prefix)+5),mp->prefix),"info");
	}

	ldptr = NULL;
	do {
		if ((ldptr = ldopen(dp->dname, ldptr)) != NULL) {

			imax = SYMHEADER(ldptr).isymMax + SYMHEADER(ldptr).iextMax;
			ibase = 0;
			/* iBase = SYMHEADER(ldptr).ismMax; */
			/* globals and undefined ? */

			for (isym = ibase; isym < imax; isym++) {

				/* read a symbol */
				if (ldtbread(ldptr, isym, &symr) != SUCCESS) {
					fprintf(stderr, "could not read symbol %d in %s/%s\n", isym, slash_boot, dp->dname);
					return;
				}

				/* if its not text, continue... */
				if (symr.sc != scText || symr.st != stProc)
					continue;

				np = ldgetname(ldptr, &symr);
				if (np == (char *) NULL)
					continue;


				/*
				 * stub out unnecessary stubs
				 */
				ddp = driver;
				do {
					register i;

					mmp = driver->opthdr;
					if (mmp->flag & LOAD)
						continue;
					if (mmp->nrtn) {
						register struct routine *rp;
						register char *sp;

						rp = (struct routine *) POINTER(mmp->o_routine, mmp);
						for (i=0; i<mmp->nrtn; ++i, ++rp)
							if ((sp=POINTER(rp->name,mmp)) && ! (strcmp(sp, np)))
								rp->name = 0;
					}
				} while (ddp = ddp->next);


				if (!(mp->flag & FSTYP))
					if ((!got_init) && function(mp->prefix,"init",np)) {
						*next_init++ = strcpy(mymalloc(strlen(np)+1),np);
						got_init++;
					}
				if ((!got_clr) && function(mp->prefix,"clr",np)) {
					*next_pwrclr++ = strcpy(mymalloc(strlen(np)+1),np);
					got_clr++;
				}


				if (mp->flag & FSTYP) {
					if (function(mp->prefix,"init",np))
						fs->fs_init = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"iput",np))
						fs->fs_iput = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"iread",np))
						fs->fs_iread = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"filler",np))
						fs->fs_filler = rtname[RFSSTRAY].name; /* FIXTHIS???*/
					if (function(mp->prefix,"iupdat",np))
						fs->fs_iupdat = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"readi",np))
						fs->fs_readi = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"writei",np))
						fs->fs_writei = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"itrunc",np))
						fs->fs_itrunc = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"statf",np))
						fs->fs_statf = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"namei",np))
						fs->fs_namei = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"mount",np))
						fs->fs_mount = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"umount",np))
						fs->fs_umount = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"getinode",np))
						fs->fs_getinode = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"openi",np))
						fs->fs_openi = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"closei",np))
						fs->fs_closei = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"update",np))
						fs->fs_update = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"statfs",np))
						fs->fs_statfs = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"access",np))
						fs->fs_access = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"getdents",np))
						fs->fs_getdents = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"allocmap",np))
						fs->fs_allocmap = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"freemap",np))
						fs->fs_freemap = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"readmap",np))
						fs->fs_readmap = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"setattr",np))
						fs->fs_setattr = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"notify",np))
						fs->fs_notify = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"fcntl",np))
						fs->fs_fcntl = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"fsinfo",np))
						fs->fs_fsinfo = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"ioctl",np))
						fs->fs_ioctl = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"lookup",np))
						fs->fs_dirlookup = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"enter",np))
						fs->fs_direnter = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"remove",np))
						fs->fs_dirremove = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"dirinit",np))
						fs->fs_dirinit = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"isempty",np))
						fs->fs_dirisempty = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"bmap",np))
						fs->fs_bmap = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"ialloc",np))
						fs->fs_ialloc = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"idestroy",np))
						fs->fs_idestroy = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"ifree",np))
						fs->fs_ifree = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"setsize",np))
						fs->fs_setsize = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"rmount",np))
						fs->fs_rmount = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"rumount",np))
						fs->fs_rumount = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"fsync",np))
						fs->fs_fsync = strcpy(mymalloc(strlen(np)+1),np);
				}


				if ((mp->flag & NOTADRV) || ((mp->flag & (CHAR | BLOCK | FUNMOD | FUNDRV)) == FUNMOD))
					/*
					 * no more special names if not a driver
					 */
					continue;

				if (function(mp->prefix,"open",np)) {
					if (mp->flag & BLOCK)
						b->d_open = strcpy(mymalloc(strlen(np)+1),np);
					if (mp->flag & CHAR)
						c->d_open = strcpy(mymalloc(strlen(np)+1),np);
				}
				if (function(mp->prefix,"close",np)) {
					if (mp->flag & BLOCK)
						b->d_close = strcpy(mymalloc(strlen(np)+1),np);
					if (mp->flag & CHAR)
						c->d_close = strcpy(mymalloc(strlen(np)+1),np);
				}
				if (mp->flag & BLOCK) {
					if (function(mp->prefix,"strategy",np))
						b->d_strategy = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"print",np))
						b->d_print = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"dump",np))
						b->d_dump = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"size",np))
						b->d_size = strcpy(mymalloc(strlen(np)+1),np);
				}
				if (mp->flag & CHAR) {
					if (function(mp->prefix,"read",np))
						c->d_read = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"write",np))
						c->d_write = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"ioctl",np))
						c->d_ioctl = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"mmap",np))
						c->d_mmap = strcpy(mymalloc(strlen(np)+1),np);
					if (function(mp->prefix,"select",np))
						c->d_select = strcpy(mymalloc(strlen(np)+1),np);
				}
#ifdef	EDTINIT
				if (dp->flag & INEDT) {
					if (function(mp->prefix,"edtinit",np))
						dp->edtp->e_init = strcpy(mymalloc(strlen(np)+1),np);
				}
#endif	EDTINIT

			}
		}
	} while (ldclose(ldptr) != SUCCESS);
#ifdef MULTIPLE_MAJOR
	}
#endif
}


/*
 * mindlessly spew out generated structures
 */
 void
generate(f)
FILE *f;
{
	struct driver *dp;
	struct master *mp;
	struct Bdevsw *bp;
	struct Cdevsw *cp;
	struct Fmodsw *fp;
	struct Fstypsw *fs;
	struct Fsinfo *fo;
	int i, j;

	fprintf( f, "\nextern " );
	for (next_init = io_init; *next_init; next_init++)
		fprintf( f, "%s(), ", *next_init);
	fprintf( f, "nodev();\n" );
	fprintf( f, "(*io_init[])() = {\n" );
	for (next_init = io_init; *next_init; next_init++)
		fprintf( f, "	%s,\n", *next_init);
	fprintf( f, "	0,\n" );
	fprintf( f, "};\n" );

	fprintf( f, "\nextern " );
	for (next_pwrclr = io_pwrclr; *next_pwrclr; next_pwrclr++)
		fprintf( f, "%s(), ", *next_pwrclr);
	fprintf( f, "nodev();\n" );
	fprintf( f, "\n(*io_pwrclr[])() = {\n" );
	for (next_pwrclr = io_pwrclr; *next_pwrclr; next_pwrclr++)
		fprintf( f, "	%s,\n", *next_pwrclr);
	fprintf( f, "	0,\n" );
	fprintf( f, "};\n" );

	fprintf( f, "\nchar MAJOR[256] = { " );
	for (i=0; i<128; i++)
		fprintf( f, "%d, " , Major[i]);
	fprintf( f, "\n");
	for (i=128; i<255; i++)
		fprintf( f, "%d, " , Major[i]);
	fprintf( f, "%d };\n" , Major[255] );
	

	fprintf( f, "\nextern " );
	for (i=0, bp=bdevswp; i<bdevcnt; i++, bp++)
		fprintf( f, "%s(), %s(), %s(), %s(), %s(), %s()," , bp->d_open, bp->d_close, bp->d_strategy, bp->d_print, bp->d_dump, bp->d_size);
	fprintf( f, "nodev();\n" );

	fprintf( f, "\nstruct bdevsw bdevsw[] = {\n" );
	for (i=0, bp=bdevswp; i<bdevcnt; i++, bp++) {
		fprintf( f, "	{ %s, %s, %s, %s, %s, %s,},\n" , bp->d_open, bp->d_close, bp->d_strategy, bp->d_print, bp->d_dump, bp->d_size);
	}
	fprintf( f, "};\n" );
	fprintf( f, "\nint bdevcnt = %d;\n", bdevcnt);


	fprintf( f, "\nextern " );
	for (i=0, cp=cdevswp; i<cdevcnt; i++, cp++) {
		if (strcmp("0", cp->d_open))
			fprintf( f, "%s(), ", cp->d_open);
		if (strcmp("0", cp->d_close))
			fprintf( f, "%s(), ", cp->d_close);
		if (strcmp("0", cp->d_read))
			fprintf( f, "%s(), ", cp->d_read);
		if (strcmp("0", cp->d_write))
			fprintf( f, "%s(), ", cp->d_write);
		if (strcmp("0", cp->d_ioctl))
			fprintf( f, "%s(), ", cp->d_ioctl);
		if (strcmp("0", cp->d_mmap))
			fprintf( f, "%s(), ", cp->d_mmap);
		if (strcmp("0", cp->d_select))
			fprintf( f, "%s(), ", cp->d_select);
		fprintf( f, "\n\t" );
	}
	fprintf( f, "nulldev();\n" );
	for (i=0, cp=cdevswp; i<cdevcnt; i++, cp++) {
		if (strcmp("0", cp->d_ttys))
			fprintf( f, "extern struct tty %s;\n", cp->d_ttys);
		if (strcmp("0", cp->d_str))
			fprintf( f, "extern struct streamtab %s;\n", cp->d_str);
	}

	fprintf( f, "\nstruct cdevsw cdevsw[] = {\n" );
	for (i=0, cp=cdevswp; i<cdevcnt; i++, cp++) {
		fprintf( f, "	{ %s, %s, %s, %s, %s, " , cp->d_open, cp->d_close, cp->d_read, cp->d_write, cp->d_ioctl);
		if (strcmp(cp->d_ttys, "0"))
			fprintf( f, "&%s, ", cp->d_ttys);
		else
			fprintf( f, "0, " );
		if (strcmp(cp->d_str, "0"))
			fprintf( f, "&%s, ", cp->d_str);
		else
			fprintf( f, "0, ", cp->d_str);
		fprintf( f, "%s, %s, },\n", cp->d_mmap, cp->d_select);
	}
	fprintf( f, "};\n" );
	fprintf( f, "\nint cdevcnt = %d;\n\n", cdevcnt);


	for (i=0, fp=fmodswp; i<fmodcnt; i++, fp++) {
		if (strcmp("0", fp->f_strmtab))
			fprintf( f, "extern struct streamtab %s;\n", fp->f_strmtab);
	}
	fprintf( f, "\nstruct fmodsw fmodsw[] = {\n" );
	for (i=0, fp=fmodswp; i<fmodcnt; i++, fp++) {
		fprintf( f,
	  	         ((! strcmp(fp->f_strmtab,Nzero)) 
				? "	{ \"%s\", %s },\n"
				: "	{ \"%s\", &%s },\n"),
			 fp->f_name, fp->f_strmtab );
	}
	fprintf( f, "	{ \"nofile\", 0 },\n" );
	fprintf( f, "};\n" );
	fprintf( f, "int fmodcnt = %d;\n\n" , fmodcnt);


	/*
	 * These values will be null if user didn't supply object
	 * with the file system master.  We only warn the user
	 * about the condition, but allow it.  Plug up empty
	 * fstypswp entries before the printfs.
	 */
	for (i=0, fs=fstypswp; i<fsincnt; i++, fs++) {
/* 0*/		if (!fs->fs_init) fs->fs_init=rtname[RFSSTRAY].name;
/* 1*/		if (!fs->fs_iput) fs->fs_iput=rtname[RFSSTRAY].name;
/* 2*/		if (!fs->fs_iread) fs->fs_iread=rtname[RFSSTRAY].name;
/* 3*/		if (!fs->fs_filler) fs->fs_filler=rtname[RFSSTRAY].name;
/* 4*/		if (!fs->fs_iupdat) fs->fs_iupdat=rtname[RFSSTRAY].name;
/* 5*/		if (!fs->fs_readi) fs->fs_readi=rtname[RFSSTRAY].name;
/* 6*/		if (!fs->fs_writei) fs->fs_writei=rtname[RFSSTRAY].name;
/* 7*/		if (!fs->fs_itrunc) fs->fs_itrunc=rtname[RFSSTRAY].name;
/* 8*/		if (!fs->fs_statf) fs->fs_statf=rtname[RFSSTRAY].name;
/* 9*/		if (!fs->fs_namei) fs->fs_namei=rtname[RFSSTRAY].name;
/*10*/		if (!fs->fs_mount) fs->fs_mount=rtname[RFSSTRAY].name;
/*11*/		if (!fs->fs_umount) fs->fs_umount=rtname[RFSSTRAY].name;
/*12*/		if (!fs->fs_getinode) fs->fs_getinode=rtname[RFSSTRAY].name;
/*13*/		if (!fs->fs_openi) fs->fs_openi=rtname[RFSSTRAY].name;
/*14*/		if (!fs->fs_closei) fs->fs_closei=rtname[RFSSTRAY].name;
/*15*/		if (!fs->fs_update) fs->fs_update=rtname[RFSSTRAY].name;
/*16*/		if (!fs->fs_statfs) fs->fs_statfs=rtname[RFSSTRAY].name;
/*17*/		if (!fs->fs_access) fs->fs_access=rtname[RFSSTRAY].name;
/*18*/		if (!fs->fs_getdents) fs->fs_getdents=rtname[RFSSTRAY].name;
/*19*/		if (!fs->fs_allocmap) fs->fs_allocmap=rtname[RFSSTRAY].name;
/*20*/		if (!fs->fs_freemap) fs->fs_freemap=rtname[RFSSTRAY].name;
/*21*/		if (!fs->fs_readmap) fs->fs_readmap=rtname[RFSSTRAY].name;
/*22*/		if (!fs->fs_setattr) fs->fs_setattr=rtname[RFSSTRAY].name;
/*23*/		if (!fs->fs_notify) fs->fs_notify=rtname[RFSSTRAY].name;
/*24*/		if (!fs->fs_fcntl) fs->fs_fcntl=rtname[RFSSTRAY].name;
/*25*/		if (!fs->fs_fsinfo) fs->fs_fsinfo=rtname[RFSSTRAY].name;
/*26*/		if (!fs->fs_ioctl) fs->fs_ioctl=rtname[RFSSTRAY].name;
/*27*/		if (!fs->fs_dirlookup) fs->fs_dirlookup=rtname[RFSSTRAY].name;
/*28*/		if (!fs->fs_direnter) fs->fs_direnter=rtname[RFSSTRAY].name;
/*29*/		if (!fs->fs_dirremove) fs->fs_dirremove=rtname[RFSSTRAY].name;
/*30*/		if (!fs->fs_dirinit) fs->fs_dirinit=rtname[RFSSTRAY].name;
/*31*/		if (!fs->fs_dirisempty) fs->fs_dirisempty=rtname[RFSSTRAY].name;
/*32*/		if (!fs->fs_bmap) fs->fs_bmap=rtname[RFSSTRAY].name;
/*33*/		if (!fs->fs_ialloc) fs->fs_ialloc=rtname[RFSSTRAY].name;
/*34*/		if (!fs->fs_idestroy) fs->fs_idestroy=rtname[RFSSTRAY].name;
/*35*/		if (!fs->fs_ifree) fs->fs_ifree=rtname[RFSSTRAY].name;
/*36*/		if (!fs->fs_setsize) fs->fs_setsize=rtname[RFSSTRAY].name;
/*37*/		if (!fs->fs_rmount) fs->fs_rmount=rtname[RFSSTRAY].name;
/*38*/		if (!fs->fs_rumount) fs->fs_rumount=rtname[RFSSTRAY].name;
/*39*/		if (!fs->fs_fsync) fs->fs_fsync=rtname[RFSSTRAY].name;
	}

	for (i=0, fs=fstypswp; i<fsincnt; i++, fs++) {
		if (strcmp(rtname[RFSSTRAY].name, fs->fs_iread))
/* 2*/			fprintf( f, "\nextern struct inode *%s();\n" , fs->fs_iread);
		if (strcmp(rtname[RFSSTRAY].name, fs->fs_getinode))
/*12*/			fprintf( f, "\nextern struct inode *%s();\n" , fs->fs_getinode);
		if (strcmp(rtname[RFSSTRAY].name, fs->fs_ialloc))
/*33*/			fprintf( f, "\nextern struct inode *%s();\n" , fs->fs_ialloc);
		if (strcmp(rtname[RFSSTRAY].name, fs->fs_freemap))
/*20*/			fprintf( f, "\nextern int *%s();\n" , fs->fs_freemap);
		fprintf( f, "extern int " );
/* 0*/		fprintf( f, "%s(), " , fs->fs_init);
/* 1*/		fprintf( f, "%s(), " , fs->fs_iput);
/* 3*/		fprintf( f, "%s(), " , fs->fs_filler);
/* 4*/		fprintf( f, "%s(), " , fs->fs_iupdat);
/* 5*/		fprintf( f, "%s(),\n" , fs->fs_readi);
/* 6*/		fprintf( f, "	%s(), " , fs->fs_writei);
/* 7*/		fprintf( f, "%s(), " , fs->fs_itrunc);
/* 8*/		fprintf( f, "%s(), " , fs->fs_statf);
/* 9*/		fprintf( f, "%s(), " , fs->fs_namei);
/*10*/		fprintf( f, "%s(),\n" , fs->fs_mount);
/*11*/		fprintf( f, "	%s(), " , fs->fs_umount);
/*13*/		fprintf( f, "%s(), " , fs->fs_openi);
/*14*/		fprintf( f, "%s(), " , fs->fs_closei);
/*15*/		fprintf( f, "%s(), " , fs->fs_update);
/*16*/		fprintf( f, "%s(),\n" , fs->fs_statfs);
/*17*/		fprintf( f, "	%s(), " , fs->fs_access);
/*18*/		fprintf( f, "%s(), " , fs->fs_getdents);
/*19*/		fprintf( f, "%s(), " , fs->fs_allocmap);
/*21*/		fprintf( f, "%s(), " , fs->fs_readmap);
/*22*/		fprintf( f, "%s(),\n" , fs->fs_setattr);
/*23*/		fprintf( f, "	%s(), " , fs->fs_notify);
/*24*/		fprintf( f, "%s(), " , fs->fs_fcntl);
/*25*/		fprintf( f, "%s(), " , fs->fs_fsinfo);
/*26*/		fprintf( f, "%s(), " , fs->fs_ioctl);
/*27*/		fprintf( f, "%s(),\n" , fs->fs_dirlookup);
/*28*/		fprintf( f, "	%s(), " , fs->fs_direnter);
/*29*/		fprintf( f, "%s(), " , fs->fs_dirremove);
/*30*/		fprintf( f, "%s(), " , fs->fs_dirinit);
/*31*/		fprintf( f, "%s(), " , fs->fs_dirisempty);
/*32*/		fprintf( f, "%s(),\n" , fs->fs_bmap);
/*34*/		fprintf( f, "	%s(), " , fs->fs_idestroy);
/*35*/		fprintf( f, "%s(), " , fs->fs_ifree);
/*36*/		fprintf( f, "%s(), " , fs->fs_setsize);
/*37*/		fprintf( f, "%s(), " , fs->fs_rmount);
/*38*/		fprintf( f, "%s();\n" , fs->fs_rumount);
/*39*/		fprintf( f, "%s();\n" , fs->fs_fsync);
  	}
  
  	fprintf( f, "\nstruct fstypsw fstypsw[] = {\n" );
	for (i=0, fs=fstypswp; i<fsincnt; i++, fs++) {
		fprintf( f, "	{\n" );
/* 0*/		fprintf( f, "		%s, " , fs->fs_init);
/* 1*/		fprintf( f, "%s, " , fs->fs_iput);
/* 2*/		fprintf( f, "(struct inode *(*)())%s,\n" , fs->fs_iread);
/* 3*/		fprintf( f, "		%s, " , fs->fs_filler);
/* 4*/		fprintf( f, "%s, " , fs->fs_iupdat);
/* 5*/		fprintf( f, "%s,\n" , fs->fs_readi);
/* 6*/		fprintf( f, "		%s, " , fs->fs_writei);
/* 7*/		fprintf( f, "%s, " , fs->fs_itrunc);
/* 8*/		fprintf( f, "%s,\n" , fs->fs_statf);
/* 9*/		fprintf( f, "		%s, " , fs->fs_namei);
/*10*/		fprintf( f, "%s, " , fs->fs_mount);
/*11*/		fprintf( f, "%s,\n" , fs->fs_umount);
/*12*/		fprintf( f, "		(struct inode *(*)())%s, " , fs->fs_getinode);
/*13*/		fprintf( f, "%s, " , fs->fs_openi);
/*14*/		fprintf( f, "%s,\n" , fs->fs_closei);
/*15*/		fprintf( f, "		%s, " , fs->fs_update);
/*16*/		fprintf( f, "%s, " , fs->fs_statfs);
/*17*/		fprintf( f, "%s,\n" , fs->fs_access);
/*18*/		fprintf( f, "		%s, " , fs->fs_getdents);
/*19*/		fprintf( f, "%s, " , fs->fs_allocmap);
/*20*/		fprintf( f, "(int *(*)())%s,\n" , fs->fs_freemap);
/*21*/		fprintf( f, "		%s, " , fs->fs_readmap);
/*22*/		fprintf( f, "%s, " , fs->fs_setattr);
/*23*/		fprintf( f, "%s,\n" , fs->fs_notify);
/*24*/		fprintf( f, "		%s, " , fs->fs_fcntl);
/*25*/		fprintf( f, "%s, " , fs->fs_fsinfo);
/*26*/		fprintf( f, "%s,\n" , fs->fs_ioctl);
/*27*/		fprintf( f, "		%s, " , fs->fs_dirlookup);
/*28*/		fprintf( f, "%s, " , fs->fs_direnter);
/*29*/		fprintf( f, "%s, " , fs->fs_dirremove);
/*30*/		fprintf( f, "%s, " , fs->fs_dirinit);
/*31*/		fprintf( f, "%s,\n" , fs->fs_dirisempty);
/*32*/		fprintf( f, "		%s," , fs->fs_bmap);
/*33*/		fprintf( f, "(struct inode*(*)())%s, " , fs->fs_ialloc);
/*34*/		fprintf( f, "%s,\n" , fs->fs_idestroy);
/*35*/		fprintf( f, "		%s," , fs->fs_ifree);
/*36*/		fprintf( f, "%s," , fs->fs_setsize);
/*37*/		fprintf( f, "%s," , fs->fs_rmount);
/*38*/		fprintf( f, "%s,\n" , fs->fs_rumount);
/*39*/		fprintf( f, "%s,\n" , fs->fs_fsync);
  		fprintf( f, "	},\n"  );
  	}
 	fprintf( f, "};\n" );

	fprintf( f, "\nint fstypcnt = %d;\n" , fstypcnt);
	fprintf( f, "short nfstyp = %d;\n" , nfstyp);

	dp = driver;
	do {
		if (!(dp->flag & LOAD))
			continue;
		if (!((mp=dp->opthdr)->flag & FSTYP))
			continue;

		fprintf( f, "\n#ifndef %sFLAGS\n", mp->prefix);
		fprintf( f, "#define %sFLAGS 0\n", mp->prefix);
		fprintf( f, "#endif\n" );
		fprintf( f, "#ifndef %sPIPE\n", mp->prefix);
		fprintf( f, "#define %sPIPE 0\n", mp->prefix);
		fprintf( f, "#endif\n" );
		fprintf( f, "#ifndef %sNAME\n", mp->prefix);
		fprintf( f, "#define %sNAME \"\"\n", mp->prefix);
		fprintf( f, "#endif\n" );
		fprintf( f, "#ifndef %sNOTFY\n", mp->prefix);
		fprintf( f, "#define %sNOTFY 0\n", mp->prefix);
		fprintf( f, "#endif\n" );

	} while (dp=dp->next);

	fprintf( f, "\nstruct fsinfo fsinfo[] = {\n" );
	for (i=0, fo=fsinfop; i<fsincnt; i++, fo++) {
		fprintf( f, "	{ %s, (struct mount *)%s, %s, %s, },\n" , fo->fs_flags, fo->fs_pipe, fo->fs_name, fo->fs_notfy);
	}
	fprintf( f, "};\n" );

	fprintf( f, "\nint fsincnt = %d;\n" , fsincnt);


	fprintf( f, "\ndev_t rootdev = %d;\n" , rootdev);
	fprintf( f, "\ndev_t dumpdev = %d;\n" , dumpdev);
	fprintf( f, "dev_t pipedev = %d;\n" , pipedev);
	fprintf( f, "dev_t swapdev = %d;\n" , swapdev);
	fprintf( f, "daddr_t swplo = %d;\n" , swplo);
	fprintf( f, "int nswap = %d;\n\n" , nswap);


	fprintf( f, "\n/* interrupt vectors */\n" );

	fprintf( f, "\nextern int " );
	dp = driver;
	do {
		struct edt *ep;
		struct vme_intrs *vp;
#ifdef ATBUS
		struct atbus_intrs *ap;
#endif

		if (!(dp->flag & INEDT))
			continue;

		for (ep=dp->edtp; ep; ep=ep->e_next)
			for (vp=ep->v_next; vp; vp=vp->v_next)
				fprintf( f, "%s(), " , vp->v_vname);
#ifdef ATBUS
		for (ep=dp->edtp; ep; ep=ep->e_next)
			for (ap=ep->a_next; ap; ap=ap->a_next)
				fprintf( f, "%s(), " , ap->a_aname);
#endif
	} while (dp=dp->next);

	fprintf( f, "nodev();\n" );

	/* added calls to all the interrupt routines to shut up lint */
	fprintf( f, "\n#ifdef lint\nlint_intr()\n{\n\tlint_intr();\n" );
	dp = driver;
	do {
		struct edt *ep;
		struct vme_intrs *vp;
#ifdef ATBUS
		struct atbus_intrs *ap;
#endif

		if (!(dp->flag & INEDT))
			continue;

		for (ep=dp->edtp; ep; ep=ep->e_next)
			for (vp=ep->v_next; vp; vp=vp->v_next)
				fprintf( f, "\t%s();\n" , vp->v_vname);
#ifdef ATBUS
		for (ep=dp->edtp; ep; ep=ep->e_next)
			for (ap=ep->a_next; ap; ap=ap->a_next)
				fprintf( f, "\t%s();\n" , ap->a_aname);
#endif
	} while (dp=dp->next);
	fprintf( f, "\tnodev();\n}\n#endif /* lint */\n" );
	
	dp = driver;
	do {
		struct edt *ep;
		struct vme_intrs *vp;
#ifdef ATBUS
		struct atbus_intrs *ap;
#endif

		if (!(dp->flag & INEDT))
			continue;
		
		mp=dp->opthdr;

		for (ep=dp->edtp,i=0; ep; ep=ep->e_next,++i) {

			if (!(vp=ep->v_next))
				continue;

			fprintf( f, "\nstruct vme_intrs %s%dintr_vec[] = {\n" , mp->prefix, i );
			do {
				fprintf( f, "	{ %s, 0x%x, 0x%x, 0x%x } ,\n" , vp->v_vname, vp->v_vec, vp->v_brl, vp->v_unit);

			} while (vp=vp->v_next);

			fprintf( f, "	{ (int (*)())0 },\n" );
			fprintf( f, "};\n" );
		}

#ifdef ATBUS

		for (ep=dp->edtp,i=0; ep; ep=ep->e_next,++i) {

			if (!(ap=ep->a_next))
				continue;

			fprintf( f, "\nstruct atbus_intrs %s%dintr_vec[] = {\n" , mp->prefix, i );
			do {
				fprintf( f, "	{ %s, 0x%x, 0x%x } ,\n" , ap->a_aname, ap->a_irq, ap->a_unit);

			} while (ap=ap->a_next);

			fprintf( f, "	{ (int (*)())0 },\n" );
			fprintf( f, "};\n" );
		}
#endif 
	} while (dp=dp->next);

	
#ifdef	EDTINIT
	fprintf( f, "\nextern int " );
	dp = driver;
	do {
		struct edt *ep;

		if (!(dp->flag & INEDT))
			continue;

		for (ep=dp->edtp,i=0; ep; ep=ep->e_next,++i)
			if (ep->e_init)
				fprintf( f, "%s(), " , ep->e_init);
	} while (dp=dp->next);

	fprintf( f, "nodev();\n" );
#endif	EDTINIT


	fprintf( f, "\nstruct edt edt[] = {\n" );
	dp = driver;
	do {
		struct edt *ep;

		if (!(dp->flag & INEDT))
			continue;

		mp = dp->opthdr;
		for (ep=dp->edtp,i=0; ep; ep=ep->e_next,++i) {
			fprintf( f, "	{ \"%s\", 0x%x, " ,
				mp->prefix, ep->e_base);
			if (ep->v_next)
				fprintf( f, "%s%dintr_vec, " , mp->prefix, i);
			else
				fprintf( f, "(struct vme_intrs *)0 , " );
#ifdef ATBUS
			if (ep->a_next)
				fprintf( f, "%s%dintr_vec, " , mp->prefix, i);
			else
				fprintf( f, "(struct atbus_intrs *)0 , " );
#endif
			
/*
 * Currently lboot will only make on edtinit entry per controller type.
 * Lets say you have two disk controllers.  The first one would have
 * an edtinit entry but the second one wouldn't.  There seems to be a 
 * fundemental promblem here.  Until I talk with Bruce Thompson about this
 * I'll fixed it by using the first entry I find and repeat it for all 
 * controllers of the same type.
 */
			if (dp->edtp->e_init)
				fprintf (f, "%s ", dp->edtp->e_init);
			else
				fprintf (f, "0 ");
#ifdef notdef
#ifdef	EDTINIT
			if (ep->e_init)
				fprintf( f, "%s " , ep->e_init);
			else
				fprintf( f, "0" );
#endif	EDTINIT
#endif
			fprintf( f, " } ,\n" );
		}

	} while (dp=dp->next);

	fprintf( f, "};\n" );

	fprintf( f, "\nint nedt = sizeof (edt)/ sizeof (struct edt);\n" );



	fprintf( f, "\n/* stubs */\n" );
	dp = driver;
	do {
		if (dp->flag & LOAD)
			continue;
		
		mp = dp->opthdr;
		if (mp->nrtn) {
			register struct routine *rp = (struct routine *) POINTER(mp->o_routine, mp);
			for (i=0; i<mp->nrtn; ++i, ++rp)
				if (rp->name)
					fprintf( f, "%s() %s\n" , POINTER(rp->name,mp), rtname[rp->id].routine);
		}
	} while (dp=dp->next);
}
