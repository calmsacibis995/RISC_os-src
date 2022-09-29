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
/* $Header: bc.h,v 1.5.1.3 90/05/10 06:05:33 wje Exp $ */

#ifndef	_SYS_BC_
#define	_SYS_BC_	1

#define	SBC_SLOT_UPB	10	/* first implementation */

/*------------------------------------------------------------------------+
| Bus types                                                               |
+------------------------------------------------------------------------*/
#define	BTYPE_NODATA	0		/* no data                       */
#define	BTYPE_COMMAND	1		/* command                       */
#define	BTYPE_DATA	2		/* data                          */
#define	BTYPE_SWDATA	3		/* switch + data                 */
#define	BTYPE_BADDATA	5		/* bad data                      */
#define	BTYPE_WD_DATA	6		/* data (write disabled)         */
#define	BTYPE_WD_SWDATA	7		/* switch + data (write disabled)*/


/*------------------------------------------------------------------------+
|  Bus Chip's Control Space Registers                                     |
+------------------------------------------------------------------------*/
#define	CTL_SPACE	0xBE000000           /* BOARD CONTROL SPACE: ??? */
#define CTL_SPACE_PHYS	0x1E000000	     /* Physical control space   */
#define CTL_SPACE_UNIT_BSIZE	0x10000	     /* byte size of each board  */
#define	CSR_IVECTSET	(CTL_SPACE | 0xf000) /* interrupt vector set reg */
#define	CSR_IVECTCLR	(CTL_SPACE | 0xf004) /* interrupt vector clr reg */
#define	CSR_IVECTMASK	(CTL_SPACE | 0xf008) /* interrupt vector mask    */
#define	CSR_BRDADDR	(CTL_SPACE | 0xf00C) /* board address reg        */

#define	CSR_CTLMISC	(CTL_SPACE | 0xf010) /* bus control  misc reg    */
#define	CSR_SCANDATA	(CTL_SPACE | 0xf014) /* scan data reg            */
#define	CSR_MEMECC	(CTL_SPACE | 0xf018) /* memory ECC reg           */
#define	CSR_MEMCTL	(CTL_SPACE | 0xf01c) /* memory control reg       */

#define	CSR_ERRREG	(CTL_SPACE | 0xf020) /* error register           */
#define	CSR_RESREFRESH	(CTL_SPACE | 0xf024) /* reset refresh counter    */
#define	CSR_COMPARE	(CTL_SPACE | 0xf028) /* timer compare register   */
#define	CSR_COUNT	(CTL_SPACE | 0xf02c) /* timer counter register   */

#define	CSR_IDPROM	(CTL_SPACE | 0xf800) /* ID prom space            */

#define SBC_REGISTERS	0xf000		     /* base of internal regs    */

#define	SBC_IVECTSET	0xf000               /* interrupt vector set reg */
#define	SBC_IVECTCLR	0xf004               /* interrupt vector clr reg */
#define	SBC_IVECTMASK	0xf008               /* interrupt vector mask    */
#define	SBC_BRDADDR	0xf00C               /* board address reg        */

#define	SBC_CTLMISC	0xf010               /* bus control  misc reg    */
#define	SBC_SCANDATA	0xf014               /* scan data reg            */
#define	SBC_MEMECC	0xf018               /* memory ECC reg           */
#define	SBC_MEMCTL	0xf01c               /* memory control reg       */

#define	SBC_ERRREG	0xf020               /* error register           */
#define	SBC_RESREFRESH	0xf024               /* reset refresh counter    */
#define	SBC_COMPARE	0xf028               /* timer compare register   */
#define	SBC_COUNT	0xf02c               /* timer counter register   */

#define	SBC_IDPROM	0xf800               /* ID prom space            */

#define CSR_IDPSIZE	0x00000200	     /* ID prom size             */

#define BRDADDR_RDWRMSK	   0xFFFFFFFF	     /* board address rd/wr mask */
#define IVECTMASK_RDWRMSK  0xFFFFFFFF	     /* Interrupt Vector RW Mask */
#define SCANDATA_RDWRMSK   0xFFFFFFFF	     /* scan data reg RW mask    */
#define COMPARE_RDWRMSK    0xFFFFFFFF	     /* compare reg RW mask      */
#define COUNT_RDWRMSK      0xFFFFFFFF	     /* counter register RW Mask */


/*------------------------------------------------------------------------+
|  Bus control misc register.                                             |
+------------------------------------------------------------------------*/
#define	SlotNumber	   0xFF000000	/* slot number mask              */
#define	StopClocks	   0x00800000	/* stop clocks                   */
#define	ScanAfterRead	   0x00010000	/* scan after read               */
#define	ScanEnable	   0x00008000	/* scan enable                   */
#define	ScanClockEnable	   0x00004000	/* scan clock enable             */
#define	NonScanClockEnable 0x00002000	/* non-scan clock enable         */
#define	ScanCnt		   0x00001F00	/* scan count                    */

#define	IdProm		   0x000000FF	/* IDProm configuration byte:    */
#define IgnoreSlaveOps     0x00000020	/*  bit 5 - ignore slave ops.    */
#define BrdNeedsRefresh    0x00000010	/*  bit 4 - board needs refresh  */
#define	BrdHasBootProm	   0x00000008	/*  bit 3 - board has boot prom  */
#define	BrdUsesParity	   0x00000004	/*  bit 2 - board uses parity    */
#define	BrdUsesECC	   0x00000002	/*  bit 1 - board uses ECC       */
#define	BrdIsMem	   0x00000001	/*  bit 0 - board is a mem brd   */
#define	CTLMISC_RDWRMSK	   0xF081FF00	/* bus control reg - rd/wr mask  */

#ifndef LOCORE
typedef union ctl_misc_reg {
    u_int  wd;				/* the whole register            */
    struct a {				/* fields within the register    */
#ifdef MIPSEB
        u_int  sltno: 8;                /* slot number                   */
        u_int  stop : 1;		/* stop clock                    */
        u_int  rsv  : 6;		/* reserved                      */
        u_int  sar  : 1;                /* scan after read               */
        u_int  se   : 1;		/* scan enable                   */
        u_int  sce  : 1;		/* scan clock enable             */
        u_int  nsce : 1;		/* non-scan clock                */
        u_int  scnt : 5;		/* scan count                    */
        u_int  idp  : 8;		/* ID-Prom configuration byte    */
#endif MIPSEB
#ifdef MIPSEL
        u_int  idp  : 8;		/* ID-Prom configuration byte    */
        u_int  scnt : 5;		/* scan count                    */
        u_int  nsce : 1;		/* non-scan clock                */
        u_int  sce  : 1;		/* scan clock enable             */
        u_int  se   : 1;		/* scan enable                   */
        u_int  sar  : 1;                /* scan after read               */
        u_int  rsv  : 6;		/* reserved                      */
        u_int  stop : 1;		/* stop clock                    */
        u_int  sltno: 8;                /* slot number                   */
#endif MIPSEL
    }   f;
}   CtlMisc;
#endif !LOCORE


/*------------------------------------------------------------------------+
| Bus Error Register's bit definitions                                    |
+------------------------------------------------------------------------*/
#define	WhoHasLockReg	0xFFFE0000	/* who has lock reg[15..1]       */
#define	BrdHasLock	0x00010000	/* board has lock                */
#define	SBCerrBadType	0x00000038	/* bad type (from bus error)     */
#define SBCerrBadType4	0x00000020	/*   type 4			 */
#define SBCerrBadType5	0x00000028	/*   type 5			 */
#define	BusErrOnRetData	0x00000004	/* bus error on return data      */
#define	BusErrOnSndData	0x00000002	/* bus error on send data        */
#define	BusAddressErr	0x00000001	/* bus address error             */
#define	BUSERR_RDWRMSK	0xFFFF0000	/* bus error reg read/write mask */

#ifndef LOCORE
typedef union bus_err_reg {
    u_int  wd;				/* the whole register            */
    struct b {				/* fields within the register    */
#ifdef MIPSEB
        u_int  whl  :15;                /* who has lock                  */
        u_int  bhl  : 1;                /* board has lock                */
        u_int  rsv  :10;		/* reserved                      */
        u_int  bt   : 3;		/* bad type                      */
        u_int  berd : 1;		/* bus error on return data      */
        u_int  besd : 1;		/* bus error on send data        */
        u_int  bae  : 1;		/* bus address error             */
#endif MIPSEB
#ifdef MIPSEL
        u_int  bae  : 1;		/* bus address error             */
        u_int  besd : 1;		/* bus error on send data        */
        u_int  berd : 1;		/* bus error on return data      */
        u_int  bt   : 3;		/* bad type                      */
        u_int  rsv  :10;		/* reserved                      */
        u_int  bhl  : 1;                /* board has lock                */
        u_int  whl  :15;                /* who has lock                  */
#endif MIPSEL
    }   f;
}   BusErr;
#endif !LOCORE


/*------------------------------------------------------------------------+
| Memory ECC register                                                     |
+------------------------------------------------------------------------*/
#define	SavedECC	0xFE000000	/* saved ECC from last mem access*/
#define	SErrBank	0x01F80000	/* bank with single bit error    */
#define	SErrSyndrome	0x0007F000	/* syndrome bit for last SBE     */
#define	BankMask	0x00000780	/* bank mask                     */
#define	EccWriteReg	0x0000007F	/* ECC write register            */
#define	MEMECC_RDWRMSK	0x000007FF	/* ECC reg read/write mask       */

#ifndef LOCORE
typedef union mem_ecc_reg {
    u_int  wd;				/* the whole register            */
    struct c {				/* fields within the register    */
#ifdef MIPSEB
        u_int  save : 7;                /* saved ecc from last read      */
        u_int  bank : 6;                /* single bit error bank         */
        u_int  synd : 7;		/* single bit error syndrom bits */
        u_int  rsv  : 1;		/* reserved                      */
        u_int  bmask: 4;		/* bank mask                     */
        u_int  wreg : 7;		/* ecc write register            */
#endif MIPSEB
#ifdef MIPSEL
        u_int  wreg : 7;		/* ecc write register            */
        u_int  bmask: 4;		/* bank mask                     */
        u_int  rsv  : 1;		/* reserved                      */
        u_int  synd : 7;		/* single bit error syndrom bits */
        u_int  bank : 6;                /* single bit error bank         */
        u_int  save : 7;                /* saved ecc from last read      */
#endif MIPSEL
    }   f;
}   MemEcc;
#endif !LOCORE


/*------------------------------------------------------------------------+
| Memory Control register                                                 |
+------------------------------------------------------------------------*/
#define	SingleBitErr	0x80000000	/* single-bit-error flag         */
#define	MultiBitErr	0x40000000	/* multi-bit-error flag          */
#define	RefreshCtr	0x03FF0000	/* memory refresh counter        */
#define	MemIntEna	0x00008000	/* memory interrupt enable       */
#define	IntDescSlotNo	0x00007F80	/* interrupt dest slot number    */
#define	IntDescBitNo	0x00000060	/* interrupt dest bit  number    */
#define	EnaEccInSel	0x00000010	/* ECC write register select     */
#define	EnaSErrLocCmp	0x00000008	/* enable address compare        */
#define	EnaSErrCorrect	0x00000004	/* enable S-bit error correction */
#define	EnaSErrInt	0x00000002	/* single bit interrupt enable   */
#define	EnaMErrDet	0x00000001	/* enable multi-bit error intr.  */
#define	MEMCTL_RDWRMSK	0x0000FFFF	/* memory cntl reg - rd/wr mask  */

#ifndef LOCORE
typedef union mem_ctl_reg {
    u_int  wd;				/* the whole register            */
    struct d {				/* fields within the register    */
#ifdef MIPSEB
        u_int  sbe  : 1;                /* single-bit error              */
        u_int  mbe  : 1;                /* multi-bit error               */
        u_int  rsv0 : 4;		/* reserved                      */
        u_int  rfsh :10;		/* refresh counter               */
        u_int  iena : 1;                /* memory interrupt enable       */
        u_int  islt : 8;                /* memory interrupt slot         */
        u_int  ibit : 2;                /* memory interrupt bit          */
        u_int  wrsel: 1;		/* ecc write register select     */
        u_int  lcmp : 1;		/* enable location compare       */
        u_int  ecor : 1;		/* enable error correct          */
        u_int  sbei : 1;		/* enable single-bit err intr.   */
        u_int  mbei : 1;		/* enable multi-bit error intr.  */
#endif MIPSEB
#ifdef MIPSEL
        u_int  mbei : 1;		/* enable multi-bit error intr.  */
        u_int  sbei : 1;		/* enable single-bit err intr.   */
        u_int  ecor : 1;		/* enable error correct          */
        u_int  lcmp : 1;		/* enable location compare       */
        u_int  wrsel: 1;		/* ecc write register select     */
        u_int  ibit : 2;                /* memory interrupt bit          */
        u_int  islt : 8;                /* memory interrupt slot         */
        u_int  iena : 1;                /* memory interrupt enable       */
        u_int  rfsh :10;		/* refresh counter               */
        u_int  rsv0 : 4;		/* reserved                      */
        u_int  mbe  : 1;                /* multi-bit error               */
        u_int  sbe  : 1;                /* single-bit error              */
#endif MIPSEL
    }   f;
}   MemCtl;
#endif !LOCORE

#endif	_SYS_BC_
