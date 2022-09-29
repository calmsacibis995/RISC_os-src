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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: sysmips.c,v 1.36.1.11.1.3.1.5 90/12/20 18:54:36 beacker Exp $"

/*
 *	Mips specific system calls
 */

#include "sys/param.h"
#include "sys/types.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/systm.h"
#include "sys/uio.h"
#include "sys/cmn_err.h"
#include "sys/uadmin.h"
#include "sys/utsname.h"
#include "sys/sysmips.h"
#include "sys/swap.h"
#include "sys/vnode.h"

#include "sys/sbd.h"	/* cpu.h */
#include "sys/cpu_board.h"
#include "sys/fpu.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/var.h"
#include "bsd43/mips/hwconf.h"
#include "sys/kmem.h"
#include "sys/buf.h"
#include "sys/conf.h"
#include "sys/sysmacros.h"

extern unsigned fptype_word;
extern struct proc *fpowner;

char	hostname[MAXHOSTNAMELEN+1];
short	hostnamelen;
char	domainname[MAXHOSTNAMELEN+1];
short	domainnamelen;

/* Data for device error injection */

#define NDEVICES 100
struct device_error default_device_error_array[NDEVICES];
struct device_error *device_error_array = NULL;
short device_error_count = 0;
int device_error_array_size = NDEVICES;
struct cdevsw *device_error_cdevsw = NULL;
struct bdevsw *device_error_bdevsw = NULL;



/* For sanity checking of machine types */
int omachine_type;
int wrong_kernel;
extern char *MACHINE_NAME[];
extern int	_posix_chown_restricted;
extern struct bsd43_hw_config bsd43_hwconf;
extern char *prom_getenv();

sysmips()
{
	register  struct  a {
		int	cmd;
		int	arg1, arg2, arg3;
	} *uap = (struct a *) u.u_ap;

#ifdef NOTYET
	struct todc	clkx;
#endif NOTYET
	register int	idx;
	register int	c;
	char		sysnamex[sizeof(utsname.sysname)];
	char 		*p;
	int s;

	extern rtodc();

	switch (uap->cmd) {

	case SETNAME:	/* rename the system */
		if (!suser())
			break;
		for (idx = 0;
		  (c = fubyte((caddr_t) uap->arg1 + idx)) > 0
		    && idx < sizeof(sysnamex) - 1;
		  ++idx)
			sysnamex[idx] = (char)c;
		if (c) {
			u.u_error = c < 0 ? EFAULT : EINVAL;
			break;
		}
		sysnamex[idx] = '\0';
		str8cpy(utsname.sysname, sysnamex);
		str8cpy(utsname.nodename, sysnamex);
#if MAXHOSTNAMELEN <= 8
		? ? ? error here in river city
#endif
		strcpy(hostname, sysnamex, hostnamelen=idx);
		break;
 
	case STIME:	/* set internal time, not hardware clock */
		if (!suser())
			break;
		s = splhi();
		time.tv_sec = (time_t) uap->arg1;
		time.tv_usec = 0;
		splx(s);
		break;

#ifdef NOTYET
	case RTODC:	/* read TODC */
		if (!suser())
			break;
		rtodc(&clkx);
		if (copyout((caddr_t) &clkx, (caddr_t) uap->arg1, sizeof(clkx)))
			u.u_error = EFAULT;
		break;
#endif NOTYET

	case FLUSH_CACHE:	/* flush all of both caches */
		flush_cache();
		break;
	

	/*	General interface for adding, deleting, or
	**	finding out about swap files.  See swap.h
	**	for a description of the argument.
	**		sysmips(SMIPSSWPI, arg_ptr);
	**/

	case SMIPSSWPI:
	{

		swpi_t	swpbuf;

		if (copyin(uap->arg1, &swpbuf, sizeof(swpi_t)) < 0) {
			u.u_error = EFAULT;
			return;
		}
		swapfunc(&swpbuf);
		break;
	}

	case MIPS_FPSIGINTR:
		u.u_procp->p_fp = uap->arg1;
		break;

	case MIPS_FPU:
		/*
		 * You must be super-user to do this.
		 * If the argument is non-zero turn the fpu
		 * back on. Else turn it off.
		 */
		if (!suser())
			break;
		if(uap->arg1){
			fptype_word =
				bsd43_hwconf.fpu_processor.ri_uint & IRR_IMP_MASK;
		}
		else{
			if(fpowner != 0)
				checkfp(fpowner, 0);
			fptype_word = 0;
		}
		break;

	case MIPS_FIXADE:
		if(uap->arg1)
			u.u_procp->p_flag |= SFIXADE;
		else
			u.u_procp->p_flag &= ~SFIXADE;
		u.u_error = 0; /* should I do this? */
		break;

	/*
	 * This call returns true if the current system has a key
	 * switch and the key is locked.
	 */
	case MIPS_KEYLOCKED:
		if (IS_R2400 && !(*(ushort *)PHYS_TO_K1(SCR) & SCR_NOBOOTLOCK))
			u.u_rval1 = 1;
		else
			u.u_rval1 = 0;
		break;

	case MIPS_PRIVILEGED_CHOWN:
		/*
		 * You must be super-user to set the value.
		 * If the argument is non-zero turn the fpu
		 * back on. Else turn it off.
		 */
		u.u_rval1 = (_posix_chown_restricted != 0);
		if (uap->arg1 < 0 &&
		    !suser())
			break;
		_posix_chown_restricted = (uap->arg1 != 0);
		break;

        case MIPS_RDNVRAM:
		u.u_error = EIO;
		return;
		
	/*
	 * This is an experimental atomic set operation.  It is similar
	 * to semaphores but works in user space so users can implement their
	 * own schemes.
	 * 
	 * The semantics are atomic_set(&i,j) atomically sets i to j and
	 * returns the old value of i.  (I and j are integers.)
	 */
	case MIPS_ATOMIC_SET: {
		int i;

		if (copyin(uap->arg1, &i, sizeof i) < 0) {
			u.u_error = EFAULT;
			return;
		}

		u.u_rval1 = i;
		i = uap->arg2;

		if (copyout(&i, uap->arg1, sizeof i) < 0) {
			u.u_error = EFAULT;
			return;
		}

		break;
	}

#ifndef	R6000
	case MIPS_UTLBHNDLR: {
		extern int	utlbhndlr;

		if (!suser())
			break;
		u.u_rval1 = utlbhndlr;
		if (uap->arg1 != GET_UTLBHNDLR)
			u.u_error = install_utlbhndlr(uap->arg1);
		break;
	}
#endif R6000

	case MIPS_KMEMLOG: {
		struct kmemlog	klog;
		extern	int *kmemlog, logkmem, kmemlogsz, kmemlogincr;
		extern	int kmemlogsz, kmemlogmin, kmemlogmax;
		extern	int alloc_kmemlog_buf();

		/* The passed argument, if 0, implies disable kernel
		 * logging.  Non-zero value implies enable it and
		 * if bigger than the current log size, then use
		 * the new size for logging.
		 */

		if (!suser())
			break;
		switch (uap->arg1) {
		  case KMEMLOG_EN:		/* enable logging */
			if (kmemlogsz <= 0)
			    u.u_error = EINVAL;
			else
			    logkmem = 1;
			break;
		  case KMEMLOG_DIS:		/* disable logging */
			logkmem = 0;
			break;
		  case KMEMLOG_STAT:		/* reutrn current state */
			u.u_rval1 = logkmem;
			break;
		  case KMEMLOG_SETP:		/* set logging parameters */
			if (copyin(uap->arg2, &klog, sizeof(klog)) < 0)
			    u.u_error = EFAULT;
			else if (klog.kmem_sz != kmemlogsz) {
			    if (alloc_kmemlog_buf(klog.kmem_min, 
					klog.kmem_incr, klog.kmem_sz) < 0)
				u.u_error = EINVAL;
			}
			logkmem = 1;
			break;
		  case KMEMLOG_GETP:		/* get logging parameters */
			klog.kmem_sz = kmemlogsz;
			klog.kmem_min = kmemlogmin;
			klog.kmem_incr = kmemlogincr;
			if (copyout(&klog, uap->arg2, sizeof(klog)) < 0)
				u.u_error = EFAULT;
			break;
		  case KMEMLOG_DATA:		/* get kmemlog data */
			if (copyout(kmemlog,uap->arg2,kmemlogsz*sizeof(int))<0)
				u.u_error = EFAULT;
			else
				u.u_rval1 = kmemlogsz;
			break;
		  default:
			u.u_error = EINVAL;
		}
		break;
	}

	case MIPS_TEST_KMEM: {
		extern	int test_kmem();

		if (!suser())
			break;

		u.u_rval1 = test_kmem(uap->arg1, uap->arg2, uap->arg3);
		break;
	}

	case MIPS_GET_MIB:
	case MIPS_GET_NEXT_MIB:
		u.u_rval1 = get_mib_data (uap->arg1, uap->arg2, uap->arg3,
						uap->cmd == MIPS_GET_NEXT_MIB);
		break;

	case MIPS_EXECMAP:
		execmap(uap->arg1,uap->arg2);
		break;

	case MIPS_PAUSE:
	{
		register struct proc *p = u.u_procp;

		while (p->p_pause_cnt == 0)
			if (sleep(&p->p_pause_cnt, PSLEP | PCATCH)) {
				u.u_error = EINTR;
				return;
			}

		p->p_pause_cnt--;
		break;
	}

	case MIPS_WAKEUP:
	{
		register struct proc *p, *q;
		register int pid = uap->arg1;

		for (p = &proc[1]; p < (struct proc *)v.ve_proc; p++)
			if (p->p_pid == pid)
				break;

		if (!p) {
			u.u_error = EINVAL;
			return;
		}

		p->p_pause_cnt++;
		wakeup(&p->p_pause_cnt);
		break;
	}

	case MIPS_DEVICE_ERROR:
		u.u_rval1 = device_error_command (uap->arg1, uap->arg2, uap->arg3);
		break;

	default:
		u.u_error = EINVAL;
	}
}

/*
 * Machine dependent code to reboot
 */
extern	int	use_prom_exit;

mdboot(fcn, mdep)
{
	bflushall();
	vdshutdown();
	switch (fcn) {
	case AD_IBOOT:			/* XXX change to prom_exec */
		cmn_err(CE_NOTE,"Sorry - No interactive reboot yet");
	/* FALL THROUGH */
	case AD_HALT:
		console_exit();
		splhi();
		reset_ctlrs();
		if (use_prom_exit)
			prom_restart();
		else
			prom_reinit();
		break;
	case AD_BOOT:
		console_exit();
		splhi();
		reset_ctlrs();
		prom_autoboot();
	}
}

/*	Sysmips swap function - manipulate swap files.
 *      Now understands the concept of swapping to a file, even
 *      an nfs mounted file.  The file to swap to must be on
 *      a mounted partition.
 */

swapfunc(si)
register swpi_t	*si;
{
	register int		i;
	struct vnode	*vp;
	char name[MAXPATHLEN];
	int namelen;

	switch (si->si_cmd) {
		case SI_LIST:
			i = sizeof(swpt_t) * MSFILES;
			if (copyout(swaptab, si->si_buf, i) < 0)
				u.u_error = EFAULT;
			break;

		case SI_ADD:
		case SI_DEL:
			if (!suser())
				break;
			u.u_error = 
			    lookupname(si->si_buf, UIO_USERSPACE, FOLLOW_LINK,
				       (struct vnode **)0, &vp);
			if (u.u_error)
				break;

			if (si->si_cmd == SI_DEL)
				swapdel(vp, si->si_swplo);
			else {
				u.u_error = copyinstr(si->si_buf, name, 
							MAXPATHLEN, &namelen);
				if (!u.u_error)
				    swapadd(vp, name, namelen, si->si_swplo, 
								si->si_nblks);
			}
			VN_RELE(vp);
			break;
	}
}


/*
 * sanity_machine_type -- When a kernel is booted, an initial value of
 *	machine type is set that is enough for the kernel to start running.
 *	Then there is a possibility that machine_type will be passed in
 *	from the prom exec routine.  This routine is merely the final test
 *	to see we have set the correct machine type.  If it is wrong,
 *	we will set to correct machine type.
 */

extern int IDPROM_ADDR[];

sanity_machine_type()
{
	unsigned char rev_id;
	char *cp;
#ifdef SABLE
	machine_type = get_machine_type();
#endif SABLE
	omachine_type = machine_type;		/* Save old machine_type */
	machine_type = get_machine_type();
	wrong_kernel = 0;

	/* Perform some minor sanity check on the machine type we
	 * just read.  And inconsitencies (which should NEVER happen),
	 * inform the user, and use (???) machine_type.  In fact the
	 * value passed in is for our sanity.
	 */
	if (machine_type != omachine_type) {
		if (((machine_type >= BRDTYPE_R2300) &&
		    (machine_type <= BRDTYPE_R2800)) &&
	     	  !((omachine_type >= BRDTYPE_R2300) &&
		    (omachine_type <= BRDTYPE_R2800)) ||

                   ((machine_type == BRDTYPE_M180) &&
                   !(omachine_type == BRDTYPE_R2400)) ||

		   ((machine_type == BRDTYPE_R2400) &&
		   !(omachine_type == BRDTYPE_R2400)) ||

		   ((machine_type == BRDTYPE_R3200) &&
		   !(omachine_type == BRDTYPE_R3200)) ||

		   ((machine_type == BRDTYPE_I2000) &&
		   !(omachine_type == BRDTYPE_I2000)) ||

		   ((machine_type == BRDTYPE_R6300) &&
		   !(omachine_type == BRDTYPE_R6300)) ||

		   ((machine_type == BRDTYPE_R3030) &&
		   !(omachine_type == BRDTYPE_R3030)) ||

		   ((machine_type == BRDTYPE_RB3125) &&
		   !(omachine_type == BRDTYPE_RB3125))
		   ) {

/* These will not work, du_init has not been called, catch-22 */

			wrong_kernel = 1;	/* Wrong kernel booted! */
		}
	}
	/* Set utsname correctly for new machine type */
	if ( !IS_I2000 && !IS_R3030 ) {
		rev_id = *(char *)(MACHDEP(IDPROM_ADDR)+ID_REV_OFF);
	}

	switch(machine_type) {

		case BRDTYPE_M6000:
		case BRDTYPE_RB3125:
#ifdef SABLE
			strcpy(utsname.m_type,MACHDEP(MACHINE_NAME));
#else
			cp = prom_getenv("model");
			if (cp && *cp != '\0') 
				strcpy(utsname.m_type, cp);
#endif SABLE
			break;
		case BRDTYPE_R2300:
		case BRDTYPE_R2600:
		case BRDTYPE_R2800:
			strcpy(utsname.m_type,MACHDEP(MACHINE_NAME));
			break;
		case BRDTYPE_R3030:
			/*
			 * we should check whether we are a RC or a RS
			 */
			strcpy(utsname.m_type,MACHDEP(MACHINE_NAME));
			break;
		case BRDTYPE_I2000:
		case BRDTYPE_I2000S:
			strcpy(utsname.m_type,MT_I2000);
			break;
		case BRDTYPE_R2400:
			if (rev_id == REV_R2400_12_5)
				strcpy(utsname.m_type, MT_DT1200_3);
			else
				strcpy(utsname.m_type,MACHDEP(MACHINE_NAME));
			break;
                case BRDTYPE_M180:
                        strcpy(utsname.m_type,MT_M180);
                        break;
		case BRDTYPE_R3200:
			if (rev_id == REV_R3200_20)
				strcpy(utsname.m_type, MT_M2000_6);
			else
				strcpy(utsname.m_type,MACHDEP(MACHINE_NAME));
			break;
		default:
			break;
	}
}


reset_ctlrs()
{
	if( has_dkip() ){
	   dkip_resetbootctlr();
	}
	clean_console();
}

struct device_error *
device_error_find_entry()
{
	int idx;
	struct device_error *ndeve;
	extern short device_error_count;
	int s;

	s = splbio ();
	for (idx = 0; idx < device_error_count; idx++) {
		if (device_error_array[idx].repeat == 0) {
			break;
		}
	}

	if (idx == device_error_array_size) {
		if ((ndeve = (struct device_error *)kmemzalloc (sizeof (struct device_error) * device_error_array_size * 2, M_MISC, M_WAITOK)) == NULL) {
			return (NULL);
		}
		bcopy (device_error_array, ndeve, sizeof (struct device_error) * device_error_array_size);
		device_error_array_size *= 2;
		if (device_error_array != default_device_error_array) {
			kmemfree (device_error_array, M_MISC, M_WAITOK);
		}
		device_error_array = ndeve;
	}

	device_error_array[idx].index = idx + 1;
	device_error_array[idx].type = 0;
	device_error_array[idx].repeat = -1;
	splx (s);
	return (&device_error_array[idx]);
}

int
device_error_command (option, device, entry)
	int option;
	caddr_t device;
	int entry;
{
	int idx;
	struct device_error *deve;
	struct device_error mydevice;
	int s;

	if (!suser ()) {
		return;
	}

	s = splbio ();
	if (device_error_array == NULL) {
		if ((device_error_cdevsw = (struct cdevsw *)kmemalloc (sizeof (struct cdevsw) * cdevcnt, M_MISC, M_WAITOK)) == NULL) {
			u.u_error = EAGAIN;
			splx (s);
			return;
		}
		bcopy (cdevsw, device_error_cdevsw, sizeof (struct cdevsw) * cdevcnt);
		if ((device_error_bdevsw = (struct bdevsw *)kmemalloc (sizeof (struct bdevsw) * bdevcnt, M_MISC, M_WAITOK)) == NULL) {
			u.u_error = EAGAIN;
			splx (s);
			return;
		}
		bcopy (bdevsw, device_error_bdevsw, sizeof (struct bdevsw) * bdevcnt);
		device_error_array = default_device_error_array;
		bzero (default_device_error_array, sizeof (default_device_error_array));
	}
	splx (s);

	switch (option) {
	case DEVICE_ERROR_ADD_ENTRY:
		if (copyin((caddr_t)device, (caddr_t)&mydevice, sizeof (struct device_error))) {
			u.u_error = EFAULT;
			return;
		}
		if (mydevice.version != DEVICE_ERROR_VERSION_1) {
			u.u_error = EINVAL;
			return;
		}
		if (mydevice.repeat == 0) {
			u.u_error = EINVAL;
			return;
		}
		if ((deve = device_error_find_entry()) == NULL) {
			u.u_error = EAGAIN;
			return;
		}
		deve->version = mydevice.version;
		deve->dev = mydevice.dev;
		deve->number = 0;
		deve->start = mydevice.start;
		deve->count = mydevice.count;
		deve->repeat = mydevice.repeat;
		deve->base = mydevice.base;
		deve->extent = mydevice.extent;
		if ((deve->error = mydevice.error) == NULL) {
			deve->error = EIO;
		}
		deve->ioctl = mydevice.ioctl;
		s = splbio ();
		deve->type = mydevice.type;
		splx (s);
		device_error_count++;
		set_error_devices (mydevice.dev);
		return (deve->index);
		break;

	case DEVICE_ERROR_DELETE_ENTRY:
		s = splbio ();
		entry--;
		if ((entry < 0) || (entry >= device_error_array_size)) {
			u.u_error = EINVAL;
			return;
		}

		if (device_error_array[entry].repeat == 0) {
			u.u_error = ENODEV;
			return;
		}
		idx = device_error_array[entry].dev;
		device_error_array[entry].repeat = 0;
		device_error_count--;
		splx (s);
		reset_error_devices (idx);
		break;

	case DEVICE_ERROR_GET_ENTRY:
		entry--;
		if ((entry < 0) || (entry >= device_error_array_size)) {
			u.u_error = EINVAL;
			return;
		}

		if (device_error_array[entry].repeat == 0) {
			u.u_error = ENODEV;
			return;
		}

		if (copyout((caddr_t)&device_error_array[entry], (caddr_t)device,
			sizeof(struct device_error))) {
			u.u_error = EFAULT;
			return;
		}
		return (entry + 1);
		break;

	case DEVICE_ERROR_GET_NEXT_ENTRY:
		if ((entry < 0) || (entry > device_error_array_size)) {
			u.u_error = EINVAL;
			return;
		}
		if (entry == device_error_array_size) {
			return (0);
		}

		for (idx = entry; idx < device_error_array_size; idx++) {
			if (device_error_array[idx].repeat != 0) {
				break;
			}
		}
		if (idx >= device_error_array_size) {
			return (0);
		}

		if (copyout((caddr_t)&device_error_array[idx], (caddr_t)device,
			sizeof(struct device_error))) {
			u.u_error = EFAULT;
			return;
		}
		return (idx + 1);
		break;

	default:
		u.u_error = EINVAL;
		break;
	}
}

int
read_write_device_error (dev, rw, base, extent)
	dev_t dev;
	u_int rw;
	daddr_t base;
	u_int extent;
{
	int i, s;

	if (device_error_array == NULL) {
		return 0;
	}

	s = splbio ();
	for (i = 0; i < device_error_array_size; i++) {
		if (device_error_array[i].dev != dev) {
			continue;
		}
		if (device_error_array[i].repeat == 0) {
			continue;
		}

		if (!(device_error_array[i].type & rw)) {
			continue;
		}

		if (device_error_array[i].extent &&
			((BBTOB (device_error_array[i].base) >= BBTOB (base) + extent) ||
			 (BBTOB (base) >= BBTOB (device_error_array[i].base) + device_error_array[i].extent))) {
			continue;
		}

		if ((device_error_array[i].number >= device_error_array[i].start) &&
		     (device_error_array[i].number < device_error_array[i].start + device_error_array[i].count)) {
			device_error_array[i].number++;
			if (device_error_array[i].number == device_error_array[i].start + device_error_array[i].count) {
				device_error_array[i].number = 0;
				if (device_error_array[i].repeat > 0) {
					device_error_array[i].repeat--;
					if (device_error_array[i].repeat == 0) {
						device_error_count--;
						reset_error_devices (dev);
					}
				}
			}
			splx (s);
			return (device_error_array[i].error);
		} else {
			device_error_array[i].number++;
			break;
			}
	}
	splx (s);
	return (0);
}

/* This routine can be used to inject errors into a strategy routine. */
int
device_error_strategy (bp)
	struct buf *bp;
{
	short error;
	u_int flag;

	if (bp->b_flags & B_READ) {
		flag = DEVICE_READ_ERROR;
	} else {
		flag = DEVICE_WRITE_ERROR;
	}

	if (device_error_count &&
		((error = read_write_device_error (bp->b_dev, flag,
						bp->b_blkno, bp->b_bcount)) != 0)) {
		bp->b_flags |= B_ERROR;
		bp->b_error = error;
		iodone (bp);
	} else {
		device_error_bdevsw[bmajor(bp->b_dev)].d_strategy (bp);
	}
}

/*
 * This routine can be used to inject errors into a read routine.  This
 * should not be used if the read routine calls the strategy routine and
 *  the above routine has been added to the strategy routine.
 */
int
device_error_read (dev)
	dev_t dev;
{
	short error;

	if (device_error_count &&
		((error = read_write_device_error (dev, DEVICE_READ_ERROR, btod (u.u_offset), u.u_count)) != 0)) {
		u.u_error = error;
		return;
	} else {
		device_error_cdevsw[major(dev)].d_read (dev);
	}
}

/*
 * This routine can be used to inject errors into a write routine.  This
 * should not be used if the write routine calls the strategy routine and
 *  the above routine has been added to the strategy routine.
 */

int
device_error_write (dev)
	dev_t dev;
{
	short error;

	if (device_error_count &&
		((error = read_write_device_error (dev, DEVICE_WRITE_ERROR, btod (u.u_offset), u.u_count)) != 0)) {
		u.u_error = error;
		return;
	} else {
		device_error_cdevsw[major(dev)].d_write (dev);
	}
}


int
open_close_device_error (dev, type)
	dev_t dev;
	u_int type;
{
	int i, s;

	if (device_error_array == NULL) {
		return 0;
	}

	s = splbio ();
	for (i = 0; i < device_error_array_size; i++) {
		if (device_error_array[i].dev != dev) {
			continue;
		}
		if (device_error_array[i].repeat == 0) {
			continue;
		}

		if (!(device_error_array[i].type & type)) {
			continue;
		}

		if ((device_error_array[i].number >= device_error_array[i].start) &&
		     (device_error_array[i].number < device_error_array[i].start + device_error_array[i].count)) {
			u.u_error = device_error_array[i].error;
			device_error_array[i].number++;
			if (device_error_array[i].number == device_error_array[i].start + device_error_array[i].count) {
				device_error_array[i].number = 0;
				if (device_error_array[i].repeat > 0) {
					device_error_array[i].repeat--;
					if (device_error_array[i].repeat == 0) {
						device_error_count--;
						reset_error_devices (dev);
					}
				}
			}
			splx (s);
			return (1);
		} else {
			device_error_array[i].number++;
			break;
			}
	}
	splx (s);
	return (0);
}

/* This routine injects errors on device open. */

device_error_open (dev, flag, otyp)
	dev_t dev;
	int flag;
	int otyp;
{
	if (device_error_count &&
		open_close_device_error (dev, DEVICE_OPEN_ERROR)) {
		return;
	} else {
		device_error_cdevsw[major(dev)].d_open (dev, flag, otyp);
	}
}

/* This routine injects errors on device close. */

device_error_close (dev, flag, otyp)
	dev_t dev;
	int flag;
	int otyp;
{
	if (device_error_count &&
		open_close_device_error (dev, DEVICE_CLOSE_ERROR)) {
		return;
	} else {
		device_error_cdevsw[major(dev)].d_close (dev, flag, otyp);
	}
}

device_error_ioctl (dev, cmd, arg, flag)
	dev_t dev;
	u_int cmd;
	caddr_t arg;
	int flag;
{
	int i, s;

	if (device_error_count == 0) {
		return;
	}


	s = splbio ();
	for (i = 0; i < device_error_array_size; i++) {
		if (device_error_array[i].dev != dev) {
			continue;
		}
		if (device_error_array[i].repeat == 0) {
			continue;
		}

		if (!(device_error_array[i].type & DEVICE_IOCTL_ERROR)) {
			continue;
		}

		if ((device_error_array[i].ioctl > 0) &&
			(device_error_array[i].ioctl != cmd)) {
			continue;
		}

		if ((device_error_array[i].number >= device_error_array[i].start) &&
		     (device_error_array[i].number < device_error_array[i].start + device_error_array[i].count)) {
			u.u_error = device_error_array[i].error;
			device_error_array[i].number++;
			if (device_error_array[i].number == device_error_array[i].start + device_error_array[i].count) {
				device_error_array[i].number = 0;
				if (device_error_array[i].repeat > 0) {
					device_error_array[i].repeat--;
					if (device_error_array[i].repeat == 0) {
						device_error_count--;
						reset_error_devices (dev);
					}
				}
			}
			splx (s);
			return;
		} else {
			device_error_array[i].number++;
			break;
			}
	}
	splx (s);
	device_error_cdevsw[major(dev)].d_ioctl (dev, cmd, arg, flag);
}

set_error_devices (dev)
	dev_t dev;
{
	u_int bmaj, cmaj;
	int s;

	s = splhigh ();
	if ((bmaj = bmajor (dev)) != 0xFF) {
		bdevsw[bmaj].d_open = device_error_open;
		bdevsw[bmaj].d_close = device_error_close;
		bdevsw[bmaj].d_strategy = device_error_strategy;
	}

	if ((cmaj = major (dev)) != 0xFF) {
		cdevsw[cmaj].d_open = device_error_open;
		cdevsw[cmaj].d_close = device_error_close;
		cdevsw[cmaj].d_read = device_error_read;
		cdevsw[cmaj].d_write = device_error_write;
		cdevsw[cmaj].d_ioctl = device_error_ioctl;
	}
	splx (s);
}

reset_error_devices (dev)
	dev_t dev;
{
	u_int bmaj, cmaj;
	int s, idx;
	u_int maj;

	if ((cmaj = major (dev)) == 0xFF) {
		return;
	}

	if ((bmaj = bmajor (dev)) == 0xFF) {
		return;
	}

	s = splhigh ();

	for (idx = 0; idx < device_error_array_size; idx++) {
		if ((major (device_error_array[idx].dev) == cmaj) &&
			(device_error_array[idx].repeat > 0)) {
			return;
		}
	}

	bdevsw[bmaj].d_open = device_error_bdevsw[bmaj].d_open;
	bdevsw[bmaj].d_close = device_error_bdevsw[bmaj].d_close;
	bdevsw[bmaj].d_strategy = device_error_bdevsw[bmaj].d_strategy;

	cdevsw[cmaj].d_open = device_error_cdevsw[cmaj].d_open;
	cdevsw[cmaj].d_close = device_error_cdevsw[cmaj].d_close;
	cdevsw[cmaj].d_read = device_error_cdevsw[cmaj].d_read;
	cdevsw[cmaj].d_write = device_error_cdevsw[cmaj].d_write;
	cdevsw[cmaj].d_ioctl = device_error_cdevsw[cmaj].d_ioctl;

	splx (s);
}
