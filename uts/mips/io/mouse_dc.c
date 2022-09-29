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
#ident	"$Header: mouse_dc.c,v 1.2.1.2 90/05/10 05:24:03 wje Exp $"
/*
 * Mouse Data Converter Module for stream device
 */

#include "sys/types.h"
#include "sys/param.h" 
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/termio.h"
#include "sys/file.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/sysinfo.h"
#include "sys/strids.h"
#include "sys/debug.h"
#include "sys/kmem.h"

#include "sys/mouse_dc.h"

/* Map user level memory allocation interface to the one supported
 * by the kernel.
 */

#define free(addr) 		kmemfree(addr, M_MISC, M_WAITOK)
#define malloc(sz)		kmemalloc(sz, M_MISC, M_WAITOK)
#define calloc(nelem,elsz) 	kmemzalloc((nelem)*(elsz), M_MISC, M_WAITOK)

/* #define MDC_DEBUG 1 */

static struct module_info mdc_info = {
  STRID_MOUSE_DC,		/* module ID */
  "mouse_dc",			/* module name */
  0,				/* minimum packet size */
  1024,				/* maximum packet size */
  128,				/* high water mark */
  16,				/* low water mark */
};

int	mdc_open(), mdc_close(); 
int	mdc_wput(), mdc_rput(), mdc_rsrv();

static struct qinit mdc_rinit = {
	mdc_rput, mdc_rsrv, mdc_open, mdc_close,  NULL,  &mdc_info,  NULL
};

static struct qinit mdc_winit = {
 	mdc_wput,   NULL  ,   NULL  ,    NULL  ,  NULL,  &mdc_info,  NULL
};

struct streamtab mouse_dcinfo = { &mdc_rinit, &mdc_winit, NULL, NULL };

#define MOUSE_DATA_SIZE	4

#define SEI_MOUSE_DATA	3
#define ISI_MOUSE_DATA	5

/*
 * open new stream module
 */
static int
mdc_open( rq, dev, flag, sflag )
    register queue_t	*rq;
    dev_t	dev;
    int		flag;
    int 	sflag;
{

    register struct MouseConv	*mc;
    register queue_t *wq;

    if( sflag != MODOPEN )
	return OPENFAIL;

#ifdef	MDC_DEBUG
    cmn_err(CE_CONT,"mdc_open: open\n");
#endif
    /* allocate local structure */
    if (!(mc = (struct MouseConv *)rq->q_ptr)) {
	if (!(mc = (struct MouseConv *)malloc(sizeof(*mc))))
	    return OPENFAIL;

	/* initialize local structure */
 	mc->wbp = NULL;
 	mc->read_buffer = mc->read_head = mc->read_tail = NULL;
	mc->data_cnt = 0;
	mc->save_data = 0;
	wq = WR(rq);
	rq->q_ptr = (caddr_t)mc;
	wq->q_ptr = (caddr_t)mc;
	mc->rq = rq;
	mc->wq = wq;

/*	qenable( rq ); */
    }
#ifdef	MDC_DEBUG
    cmn_err(CE_CONT,"mdc_open: opened\n");
#endif
    return(0);
}

/*
 * close stream module
 */
static int
mdc_close( rq )
    register queue_t	*rq;
{
    register struct MouseConv *mc = (struct MouseConv *)rq->q_ptr;

    ASSERT(mc->rq == rq);

    /* should free message block */
    freemsg(mc->read_head);
    freemsg(mc->wbp);

    /* stop waiting for buffer allocation */
    str_unbcall(rq);
    
    /* free local structure */
    free((char *)mc);
#ifdef	MDC_DEBUG
    cmn_err(CE_CONT,"mdc_close: clised\n");
#endif
}

/*
 * put data & message on the write queue
 *	there is nothing to do except for handing message to next write queue
 */
static int
mdc_wput( wq, bp )
    register queue_t	*wq;
    register mblk_t	*bp;
{
    register struct MouseConv *mc = (struct MouseConv *)wq->q_ptr;

    ASSERT(mc->wq == wq);
    switch( bp->b_datap->db_type ) {
	case M_FLUSH :
		if( *bp->b_rptr & FLUSHR )
		    mdc_flush_rq( mc );
		putnext( wq, bp );
		break;
	default :	
    		putnext ( wq, bp );
    }
}

/*
 * put data & message on the read queue
 */
static int
mdc_rput( rq, bp )
    register queue_t	*rq;
    register mblk_t	*bp;
{
    register struct MouseConv *mc = (struct MouseConv *)rq->q_ptr;

    ASSERT(mc->rq == rq);
    switch( bp->b_datap->db_type ){
	case M_DATA :
#ifdef	MDC_DEBUG
		cmn_err(CE_CONT,"mdc_rput : M_DATA\n");
#endif
		putq( rq, bp );
		break;
	case M_FLUSH :
		if( *bp->b_rptr & FLUSHR )
		    mdc_flush_rq( mc );
		putnext( rq, bp );
		break;
	default :
#ifdef	MDC_DEBUG
		cmn_err(CE_CONT,"mdc_rput : default\n");
#endif
		putnext( rq, bp );
		break;
    }
}

/* 
 * flush messae on the read queue 
 */
mdc_flush_rq( mc )
    register struct MouseConv	*mc;
{
    freemsg( mc->read_head );
    mc->read_head = mc->read_buffer = NULL;
}

/*
 * make service to date & message on the read queue
 */
static int
mdc_rsrv( rq )
    register queue_t	*rq;
{
    register struct MouseConv *mc = (struct MouseConv *)rq->q_ptr;
    register mblk_t *mbp;

    ASSERT(mc->rq == rq);
    enableok( rq );
    for(;;){
	if(!(mbp = getq( rq )))
		break;

	switch(mbp->b_datap->db_type){
	    case M_DATA :
#ifdef	MDC_DEBUG
    		cmn_err(CE_CONT,"mdc_rsrv: M_DATA\n");
#endif	
		if( !canenable(rq) ){
		    putbq( rq, mbp );
		    return;
		}		
		mdc_conv( rq, mbp );
		if( mc->read_head ) {
    		    putnext( rq, mc->read_head );
		    mc->read_buffer = mc->read_head = NULL;
		}
		break;
	    default :
		putnext( rq, mbp );
		break;
	}
    }
}

/*
 * convert data 
 */
mdc_conv(rq, bp)
    register queue_t	*rq;
    register mblk_t	*bp;
{
    register struct MouseConv *mc = (struct MouseConv *)rq->q_ptr;
    register mblk_t *sbp = NULL;
    char out_data,in_data;
    register int size, i, j, state;

#ifdef	MDC_DEBUG
    cmn_err(CE_CONT,"bp->b_wptr = %d\n", *bp->b_wptr);
    cmn_err(CE_CONT,"bp->b_rptr = %d\n", *bp->b_rptr);
#endif	
    if(( size = bp->b_wptr - bp->b_rptr )){
	for(i = 0 ; i < size ; i++, bp->b_rptr++ ){
	        in_data = *bp->b_rptr;
#ifdef	MDC_DEBUG
		cmn_err(CE_CONT,"[%d:",(int)in_data);
#endif
		if(( state = convert(mc, in_data, &out_data )) == -1)
		    continue;
#ifdef	MDC_DEBUG
		cmn_err(CE_CONT,"%d],",(int)out_data);
#endif
		if( (sbp = mc->read_buffer) == 0 ){
		    mdc_allocb( mc, BPRI_HI );
		    if( (sbp = mc->read_buffer) == 0 ){
			cmn_err(CE_CONT,
			  "mdc_conv: can't get buffer. Data lost\n");
			continue;
		    }
		}
		*sbp->b_wptr = out_data;
#ifdef	MDC_DEBUG
		cmn_err(CE_CONT,"mc->read_head->b_wptr : 0x%x\n",
						mc->read_head->b_wptr);
#endif
		if( sbp->b_wptr++ >= sbp->b_datap->db_lim ) {
		    mdc_allocb( mc, BPRI_HI );
		    if( (sbp = mc->read_buffer) == 0 ){
			cmn_err(CE_CONT,
			  "mdc_conv: can't get buffer. Data lost\n");
			continue;
		    }
		}
		/* set dummy character */
		if(state == 1){
		    for(j = 0 ; j < 2 ; j++) {
			*sbp->b_wptr = 0;
			if( sbp->b_wptr++ >= sbp->b_datap->db_lim )
		    	    mdc_allocb( mc, BPRI_HI );
		    	if( (sbp = mc->read_buffer) == 0 ){
			    cmn_err(CE_CONT,
			  	"mdc_conv: can't get buffer. Data lost\n");
			    continue;
		        }
		    }
		}
	} 
        freemsg ( bp );
    } else 
	cmn_err(CE_CONT,"mdc_conv : there is no data in buffer\n");
}

/* 
 * data conversion
 *   return == -1 : character is lost
 *	    ==  1 : add dummy character
 *	    ==  0 : else case
 */
convert( mc, in_data, out_data )
    register struct MouseConv *mc;
    register char	in_data;
    register char	*out_data;
{
    register char tmp_data;
    int rtn;

#ifdef	MDC_DEBUG
    cmn_err(CE_CONT,"<%d>",mc->data_cnt);
#endif
    switch(mc->data_cnt){

	/* 1st. byte of data */
	case 0 :	
		if((in_data & 0x80) == 0x80) {
		    mc->save_data = in_data & 0x18;
		    tmp_data = in_data & 0x07;
		    tmp_data = (tmp_data & 0x01) ? 
					(tmp_data | 0x08) : (tmp_data & ~0x08);
		    *out_data = (((tmp_data >> 1) | 0x80) ^ 0x07);
		    mc->data_cnt = 1;
		    rtn = 0;
		} else{
		    mc->data_cnt = 0;
		    rtn = -1;
		}
		break;

	/* 2nd. byte of data */
	case 1 :
		if((in_data & 0x80) != 0x80) {
		    *out_data = (mc->save_data & 0x08) ? 
					(in_data | 0x80) : in_data;
		    mc->data_cnt = 2;
		    rtn = 0;
		} else {
		    mc->save_data = in_data & 0x18;
		    tmp_data = in_data & 0x07;
		    tmp_data = (tmp_data & 0x01) ? 
					(tmp_data | 0x08) : (tmp_data & ~0x08);
		    *out_data = (((tmp_data >> 1) | 0x80) ^ 0x07);
		    mc->data_cnt = 1;
		    rtn = 0;
		}
		break;

	/* 3rd. byte of data */
	case 2 :
		if((in_data & 0x80) != 0x80) {
		    *out_data = (mc->save_data & 0x10) ? 
					(in_data | 0x80) : in_data;
		    *out_data *= (-1);
		    mc->data_cnt = 0;
		    rtn = 1;
		} else {
		    mc->save_data = in_data & 0x18;
		    tmp_data = in_data & 0x07;
		    tmp_data = (tmp_data & 0x01) ? 
					(tmp_data | 0x08) : (tmp_data & ~0x08);
		    *out_data = (((tmp_data >> 1) | 0x80) ^ 0x07);
		    mc->data_cnt = 1;
		    rtn = 0;
		}
		break;
    }
    return( rtn );
}

/*
 * allocate data buffer 
 */
mdc_allocb( mc, pri )
    register struct MouseConv *mc;
    int pri;
{
    register mblk_t *bp;

    if( bp = allocb( MOUSE_DATA_SIZE, pri )){
	str_conmsg(&mc->read_head, &mc->read_tail, bp);
	mc->read_buffer = bp;
    }
#ifdef 	MDC_DEBUG
    else
        cmn_err(CE_CONT,"mdc_allocb : can't get buffer\n");
#endif
}
