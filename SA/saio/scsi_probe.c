#ident "$Header: scsi_probe.c,v 1.7 90/11/07 15:22:36 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

#include "sys/types.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "sys/cmn_err.h"
#include "machine/dvh.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "mips/scsi.h"

extern void common_scsi_string();

extern int machine_type;
#if	PROM
#if	(R3030 || RB3125)
extern int pdkis_Nlun, pdkis_Ntarget;
extern struct scsi_unit pdkis_un[]; 
#else
extern int dkis_Nlun, dkis_Ntarget;
extern struct scsi_unit dkis_un[];
#endif R3030
#else
extern int dkis_Nlun, dkis_Ntarget;
extern int pdkis_Nlun, pdkis_Ntarget;
extern struct scsi_unit pdkis_un[], dkis_un[];
#endif PROM

#define NTARGETS	7
#define NLUNS		8

scsi_probe()
{
    struct scsi_unit *un;
    register SCSI_INQUIRY *inq;
    register int target, lun, maxlun, i;
    char buf[128];
    char *scsi_id;
    char sid;
    int target_id;
    extern char *getenv();

#if	PROM
#if	(R3030 || RB3125)
    switch (machine_type) {
	case BRDTYPE_R3030:
	case BRDTYPE_RB3125:
	    maxlun = pdkis_Nlun;
	    break;
	default:
	    return(0);
    }
#else
    switch (machine_type) {
	case BRDTYPE_R2400:
	case BRDTYPE_M180:
	    maxlun = dkis_Nlun;
	    break;
	default:
	    return(0);
    }
#endif
#else
    switch (machine_type) {
	case BRDTYPE_R3030:
	case BRDTYPE_RB3125:
	    maxlun = pdkis_Nlun;
	    break;
	case BRDTYPE_R2400:
	case BRDTYPE_M180:
	    maxlun = dkis_Nlun;
	    break;
	default:
	    return(0);
    }
#endif PROM
    target_id = 7;
    if (IS_R3030) {
        sid = (scsi_id = getenv("scsi_id")) ? *scsi_id : 0;
        if ((sid >= '0') && (sid < '8')) target_id = sid - '0';
        verify_scsi_id(target_id);
    }

    cmn_err(CE_CONT,
"          Vendor  -Product ID      -Rev -M Rev   -Serial Nr   -Device Type\n");
    for (target = 0; target <= NTARGETS; ++target) {
      if (target==target_id) continue;  
	for (lun = 0; lun < maxlun; ++lun) 
	    if(scsi_inquiry(target,lun,&un) == 0) {
	        common_scsi_string(un,buf);
	        cmn_err(CE_CONT,"SCSI %dL%d: %s\n",target,lun,buf);
	    }
    }
    return (0);
}

scsi_inquiry(target,lun,pun)
register struct scsi_unit **pun;
{
    register SCSI_INQUIRY *inq;
    register struct scsi_unit *un;
    register int i;


#if	PROM
#if	(R3030 || RB3125)
		i = TAR_LUN(target,lun,pdkis_Nlun);
		un = &pdkis_un[i];
		if (!un->un_tmpbuf) {
		    un->un_tmpbuf = (u_char *)align_malloc(64,un->un_dmaalign);
		    un->un_buf_64 = un->un_tmpbuf;
		}
#else
	switch (machine_type) {
	    case BRDTYPE_R2400:
	    case BRDTYPE_M180:
		i = TAR_LUN(target,lun,dkis_Nlun);
		un = &dkis_un[i];
		break;
	}
#endif
#else
	switch (machine_type) {
	    case BRDTYPE_R3030:
	    case BRDTYPE_RB3125:
		i = TAR_LUN(target,lun,pdkis_Nlun);
		un = &pdkis_un[i];
		if (!un->un_tmpbuf) {
		    un->un_tmpbuf = (u_char *)align_malloc(64,un->un_dmaalign);
		    un->un_buf_64 = un->un_tmpbuf;
		}
		break;
	    case BRDTYPE_R2400:
	    case BRDTYPE_M180:
		i = TAR_LUN(target,lun,dkis_Nlun);
		un = &dkis_un[i];
		break;
	}
#endif PROM
	inq = (SCSI_INQUIRY *)K0_TO_K1(&un->un_inq);
	*pun = un;
	return(common_scsi_spcmd(un,C0_INQUIRY,0,sizeof(SCSI_INQUIRY),inq,POLLED));
}
