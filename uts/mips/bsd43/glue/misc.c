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
#ident	"$Header: misc.c,v 1.6.1.6.1.4.1.5 90/12/20 19:21:49 beacker Exp $"

/*
 * This file contains miscellaneous bsd functionality.
 */

/*
 * Headers: r !types.h user.h param.h errno.h var.h debug.h
 */
#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/var.h"
#include "sys/errno.h"
#include "bsd43/mips/hwconf.h"
#include "sys/cpu_board.h"
#include "sys/sbd.h"
#include "sys/utsname.h"

#ifdef R6000_BUG_IDPROM
#include "sys/ctype.h"
#endif R6000_BUG_IDPROM

sstk()
{

}



bsd_getpagesize() 
{
	u.u_rval1 = NBPC;
}

bsd_getdtablesize() 
{
	u.u_rval1 = v.v_nofiles;
}


struct bsd43_hw_config bsd43_hwconf;

int
mipshwconf()
{
	register struct a {
		int	option;
		struct	bsd43_hw_config *info;
	} *uap = (struct a *) u.u_ap;

	u.u_error = bsd_mipshwconf(uap->option,uap->info);
	return(u.u_error);
}


int
bsd_mipshwconf(option,info)
	int	option;
	struct	bsd43_hw_config *info;
{
	char *prom_getenv();
	int prom_setenv();
	register int i;
	struct bsd43_hw_config tmp;
	int error = 0;
	extern int cpu_config;


	if (IS_R3030) {
		cpu_config &= ~P_DIGI;
		switch(is_digi()) {
		case 8:
			cpu_config |= P_DIG8;
			break;
		case 16:
			cpu_config |= P_DIG16;
			break;
		}
	}
	bsd43_hwconf.cpubd_config = cpu_config;	/* we set this up dynamically */
	switch (option) {

	case BSD43_HWCONF_GET:
		for (i=0; i < BSD43_ENV_ENTRIES; i++) {
			strncpy(bsd43_hwconf.bsd43_promenv[i].value, 
				prom_getenv(bsd43_hwconf.bsd43_promenv[i].bsd43_name), 
				BSD43_ENV_MAXLEN);
		}
		error = bsd_copyout((caddr_t)&bsd43_hwconf, (caddr_t)info, 
				sizeof(struct bsd43_hw_config));
		break;

	case BSD43_HWCONF_SET:
		if (!suser()) {
			error = EACCES;
			return(error);
		}
		error = bsd_copyin((caddr_t)info, (caddr_t)&tmp,
				sizeof(struct bsd43_hw_config));
		if (error)
			return(error);
		for (i=0; i < BSD43_ENV_ENTRIES; i++) {
		    if (!strcmp(tmp.bsd43_promenv[i].bsd43_name,
				bsd43_hwconf.bsd43_promenv[i].bsd43_name))
			prom_setenv(tmp.bsd43_promenv[i].bsd43_name,
				    tmp.bsd43_promenv[i].value);
		}
		break;

	default:
		return(EINVAL);
	}
	return(error);
}


extern unsigned cputype_word;
extern unsigned fptype_word;

extern unsigned icache_size;
extern unsigned dcache_size;

extern int cpu_config;

extern int IDPROM_ADDR[];

bsd43_hwconf_init()
{
	bsd43_hwconf.cpubd_type = machine_type;
	switch (machine_type) {
#ifndef SABLE
	case BRDTYPE_R2300:
	case BRDTYPE_R2600:
	case BRDTYPE_R2800:
	case BRDTYPE_R6300:
	case BRDTYPE_R2400:
	case BRDTYPE_M180:
	case BRDTYPE_R3200:
	case BRDTYPE_RB3125:
		bsd43_hwconf.cpubd_rev =
			 *(char *)(MACHDEP(IDPROM_ADDR) + ID_REV_OFF);
		if((machine_type == BRDTYPE_RB3125) && 
		  (((bsd43_hwconf.cpubd_rev & 0xf0) >> 4) == REV_R3200_33)){
			 bsd43_hwconf.cpubd_type = BRDTYPE_RB3133;
			 strcpy(utsname.m_type,MT_RB3133);
		}
		bsd43_hwconf.cpubd_snum[0] =
			 *(char *)(MACHDEP(IDPROM_ADDR) + ID_SN1_OFF);
		bsd43_hwconf.cpubd_snum[1] =
			 *(char *)(MACHDEP(IDPROM_ADDR) + ID_SN2_OFF);
		bsd43_hwconf.cpubd_snum[2] =
			 *(char *)(MACHDEP(IDPROM_ADDR) + ID_SN3_OFF);
		bsd43_hwconf.cpubd_snum[3] =
			 *(char *)(MACHDEP(IDPROM_ADDR) + ID_SN4_OFF);
		bsd43_hwconf.cpubd_snum[4] =
			 *(char *)(MACHDEP(IDPROM_ADDR) + ID_SN5_OFF);
#ifdef R6000_BUG_IDPROM
		if (!isalnum(bsd43_hwconf.cpubd_snum[0])) {
			bsd43_hwconf.cpubd_snum[0] = '0';
			bsd43_hwconf.cpubd_snum[1] = '0';
			bsd43_hwconf.cpubd_snum[2] = '0';
			bsd43_hwconf.cpubd_snum[3] = '0';
			bsd43_hwconf.cpubd_snum[4] = '0';
		}
#endif R6000_BUG_IDPROM
		break;
	case BRDTYPE_R3030:
	{
		long	*prom_ptr = (long *)PHYS_TO_K1(TODC_CLOCK_ADDR_R3030);

		bsd43_hwconf.cpubd_rev = prom_ptr[7];
		bsd43_hwconf.cpubd_snum[0] = prom_ptr[8];
		bsd43_hwconf.cpubd_snum[1] = prom_ptr[9];
		bsd43_hwconf.cpubd_snum[2] = prom_ptr[10];
		bsd43_hwconf.cpubd_snum[3] = prom_ptr[11];
		bsd43_hwconf.cpubd_snum[4] = prom_ptr[12];
		break;
	}
	case BRDTYPE_I2000:
	case BRDTYPE_I2000S:
#endif SABLE
	default:
		bsd43_hwconf.cpubd_rev = 0;

		bsd43_hwconf.cpubd_snum[0] = 0;
		bsd43_hwconf.cpubd_snum[1] = 0;
		bsd43_hwconf.cpubd_snum[2] = 0; 
		bsd43_hwconf.cpubd_snum[3] = 0;
		bsd43_hwconf.cpubd_snum[4] = 0;
		break;
	};
	bsd43_hwconf.cpubd_config = cpu_config;
	bsd43_hwconf.cpu_processor.ri_uint = cputype_word;
	bsd43_hwconf.fpu_processor.ri_uint = 
	  	(fptype_word != 0 ? get_fpc_irr() : 0);
	bsd43_hwconf.icache_size = icache_size;
	bsd43_hwconf.dcache_size = dcache_size;

	strcpy(bsd43_hwconf.bsd43_promenv[0].bsd43_name, "netaddr");
	strcpy(bsd43_hwconf.bsd43_promenv[1].bsd43_name, "lbaud");
	strcpy(bsd43_hwconf.bsd43_promenv[2].bsd43_name, "rbaud");
	strcpy(bsd43_hwconf.bsd43_promenv[3].bsd43_name, "bootfile");
	strcpy(bsd43_hwconf.bsd43_promenv[4].bsd43_name, "bootmode");
	strcpy(bsd43_hwconf.bsd43_promenv[5].bsd43_name, "console");

}



getdopt()
{

}

setdopt()
{

}


bsd_cachectl(addr,bcnt,op)
	uint addr, bcnt, op;
{
	register struct a {
		uint	addr;
		uint	bcnt;
		uint	op;
	} *uap = (struct a *) u.u_ap;

	uap->addr = addr;
	uap->bcnt = bcnt;
	uap->op = op;

	cachectl();

	return(u.u_error);
}


bsd_cacheflush(addr,bcnt,op)
	uint addr, bcnt, op;
{
	register struct a {
		uint	addr;
		uint	bcnt;
		uint	op;
	} *uap = (struct a *) u.u_ap;

	uap->addr = addr;
	uap->bcnt = bcnt;
	uap->op = op;

	cacheflush();

	return(u.u_error);
}


bsd_mipskopt(argname,value,op)
	char *argname;
	int value;
	int op;
{
	register struct a {
		char *argname;
		int value;
		int op;
	} *uap = (struct a *) u.u_ap;

	uap->argname = argname;
	uap->value = value;
	uap->op = op;

	mipskopt();

	return(u.u_error);
}
