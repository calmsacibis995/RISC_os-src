/*
 * if_egl.h : V/Ethernet 4207 EAGLE. 
 *
 * Created: 11/17/87 by Interphase Corp. 
 *
 * Author: Manlio D. Marquez 
 *
 * Contains all of the structs and types for Eagle MACSI. This is basically
 * snarfed from the Jaguar code. 
 *
 * Modification History 11/24/87 change layout of MCSB to agree with Eagle
 * internal MACSI change layout of CSB to agree with Eagle internal MACSI
 * eliminate 'duplicate' init workq iopb definition. 
 *
 * 11/24/87 add direction bit to topt for vme transfer direction. 
 *
 * 11/28/87 fix misdefinition of interface bits 
 *
 * 11/30/87 add iopbs for change node address and filter 
 */


#define S_SHORTIO		2048	/* total short i/o space */
#define S_HUS			1812	/* total host usable space */

#define S_MCSB			sizeof(EGL_MCSB)
#define S_MCE			sizeof(EGL_CQE)
#define S_CQE			sizeof(EGL_CQE)
#define S_IOPB			sizeof(EGL_IOPB)
#define S_IOPB_MCE		sizeof(EGL_IOPB)
#define N_SCRTCH		24
#define S_SCRTCH		sizeof(UWORD)*N_SCRTCH
#define S_CRB			sizeof(EGL_CRB)
#define S_CSTB			sizeof(EGL_CSTB)
#define S_CSB			sizeof(EGL_CSB)
#define S_CIB			sizeof(EGL_CIB)
#define S_WQIB			sizeof(EGL_WQIB)
#define S_WQCF			sizeof(EGL_WQCF)

#define MAX_CQE			(S_HUS - S_IOPB - S_SCRTCH) / (S_CQE + S_IOPB)
#define MAX_IOPB		MAX_CQE

#define O_MCE_IOPB		S_MCSB+S_MCE+(S_CQE+S_IOPB)*MAX_CQE
/* offset of MCE_IOPB */


#define WORDP(x)		((x)->U.w)
#define WORD_(x)		((x).U.w)



/**************** Master Control Status Block (MCSB) *******************/

typedef struct msr {		/* Master Status Register */
	union {
		struct {
			Bit    :13;	/* reserved bits */
			Bit QFC:1;	/* Queue Flush Complete */
			Bit BOK:1;	/* Board OK */
			Bit CNA:1;	/* Controller Not Available */
		} b;
		UWORD w;
	} U;
} EGL_MSR;

#define M_MSR_QFC	0x0004
#define M_MSR_BOK	0x0002
#define M_MSR_CNA	0x0001
#define B_MSR_QFC(x)	((x).U.b.QFC)
#define B_MSR_BOK(x)	((x).U.b.BOK)
#define B_MSR_CNA(x)	((x).U.b.CNA)
#define W_MSR(x)	((x).U.w)

typedef struct mcr {		/* Master Control Register */
	union {
		struct {
			Bit	    :2;	/* reserved bits */
			Bit	SFEN:1;	/* Sysfail Enable */
			Bit	 RES:1;	/* Reset controller */
			Bit	 FLQ:1;	/* Flush Queue */
			Bit	    :8;	/* reserved bits */
			Bit	FLQR:1;	/* Flush Queue and Report */
			Bit	    :1;	/* reserved bits */
			Bit	 SQM:1;	/* Start Queue Mode */
		} b;
		UWORD w;
	} U;
} EGL_MCR;

#define M_MCR_SFEN	0x2000
#define M_MCR_RES	0x1000
#define M_MCR_FLQ	0x0800
#define M_MCR_FLQR	0x0004
#define M_MCR_SQM	0x0001
#define B_MCR_SFEN(x)	((x).U.b.SFEN)
#define B_MCR_RES(x)	((x).U.b.RES)
#define B_MCR_FLQ(x)	((x).U.b.FLQ)
#define B_MCR_FLQR(x)	((x).U.b.FLQR)
#define B_MCR_SQM(x)	((x).U.b.SQM)
#define W_MCR(x)	((x).U.w)

typedef struct iqar {		/* Interrupt on Queue Available Reg */
	union {
		struct {
			Bit IQEA:1;	/* Intr on Queue entry Available */
			Bit IQEH:1;	/* Intr on Queue Half Empty Enable */
			Bit     :3;	/* Reserved bits */
			Bit ILVL:3;	/* Intr Level on Queue Available */
			Bit IVCT:8;	/* Intr Vector on Queue Available */
		} b;
		UWORD w;
	} U;
} EGL_IQAR;

#define M_IQAR_IQEA	0x8000
#define M_IQAR_IQEH	0x4000
#define M_IQAR_ILVL	0x0700
#define M_IQAR_IVCT	0x00FF
#define B_IQAR_IQEA(x)	((x).U.b.IQEA)
#define B_IQAR_IQEH(x)	((x).U.b.IQEH)
#define B_IQAR_ILVL(x)	((x).U.b.ILVL)
#define B_IQAR_IVCT(x)	((x).U.b.IVCT)
#define W_IQAR(x)	((x).U.w)

typedef struct mcsb {		/* Master control/Status Block */
	EGL_MSR		mcsb_MSR;	/* Master status register */
	UWORD		mcsb_RES0;	/* Reserved word 0 */
	EGL_MCR		mcsb_MCR;	/* Master Control register */
	EGL_IQAR	mcsb_IQAR;	/* Interrupt on Queue Available Reg */
	UWORD		mcsb_QHDP;	/* Queue head pointer */
	UWORD		mcsb_RES1;	/* Reserved word 1 */
	UWORD		mcsb_RES2;	/* Reserved word 2 */
	UWORD		mcsb_RES3;	/* Reserved word 3 */
} EGL_MCSB;

/**************** END Master Control Status Block (MCSB) *******************/


/*************** Controller Initialization Block (CIB)*****************/

typedef struct imod {	/* Interface Specific Mode Physical Interface Mode */
	union {
		struct {
			Bit PRM:1;	/* Promiscuous Mode */
			Bit INL:1;	/* Internal Loopback */
			Bit DRY:1;	/* Disable Retry */
			Bit COL:1;	/* Force Collision */
			Bit DTC:1;	/* Disable Transmit CRC */
			Bit  LP:1;	/* Loopback */
			Bit DTX:1;	/* Disable Transmit */
			Bit DRX:1;	/* Disable Receive */
			Bit    :4;	/* Reserved */
			Bit PX25:1;	/* X.25 (not supported)	 */
			Bit PSTR:1;	/* Starlan (not supported) */
			Bit PIEEE:1;	/* IEEE 802.3 */
			Bit  PE:1;	/* Ethernet */
		} b;
		UWORD w;
	} U;
} EGL_IMOD;

#define M_IMOD_PRM	0x8000
#define M_IMOD_INL	0x4000
#define M_IMOD_DRY	0x2000
#define M_IMOD_COL	0x1000
#define M_IMOD_DTC	0x0800
#define M_IMOD_LP	0x0400
#define M_IMOD_DTX	0x0200
#define M_IMOD_DRX	0x0100
#define M_IMOD_PX	0x0008
#define M_IMOD_PS	0x0004
#define M_IMOD_PIEEE	0x0002
#define M_IMOD_PE	0x0001
#define B_IMOD_PRM(x)	((x).U.b.PRM)
#define B_IMOD_INL(x)	((x).U.b.INL)
#define B_IMOD_DRY(x)	((x).U.b.DRY)
#define B_IMOD_COL(x)	((x).U.b.COL)
#define B_IMOD_DTC(x)	((x).U.b.DTC)
#define B_IMOD_LP(x)	((x).U.b.LP)
#define B_IMOD_DTX(x)	((x).U.b.DTX)
#define B_IMOD_DRX(x)	((x).U.b.DRX)
#define B_IMOD_PE(x)	((x).U.b.PE)
#define B_IMOD_PIEEE(x)	((x).U.b.PIEEE)
#define B_IMOD_PS(x)	((x).U.b.PS)
#define B_IMOD_PX(x)	((x).U.b.PX)

typedef struct vect {		/* Vector structure */
	union {
		struct {
			Bit     :5;	/* Reserved bits */
			Bit ILVL:3;	/* Interrupt Level */
			Bit IVCT:8;	/* Interrupt Vector */
		} b;
		UWORD w;
	} U;
} EGL_VECT;

#define M_VECT_ILVL	0x0700
#define M_VECT_IVCT	0x00FF
#define B_VECT_ILVL(x)	((x).U.b.ILVL)
#define B_VECT_IVCT(x)	((x).U.b.IVCT)
#define W_VECT(x)	((x).U.w)

typedef struct cib {		/* Controller Initialization Block */
	UBYTE	cib_RES0;		/* Reserved byte 0 */
	UBYTE	cib_NCQE;		/* Number of Command Queue Entries */
	EGL_IMOD cib_IMOD;	/* Interface Modes */
	UBYTE	cib_NTXR;		/* Num LANCE Tx Rings	*/
	UBYTE	cib_NRXR;		/* Num LANCE Rx Rings	*/
	UBYTE	cib_PHY[6];	/* Ethernet Physical Address */
	UBYTE	cib_FILT[8];	/* Ethernet Logical Address Filter */
	UWORD	cib_RXSIZ;	/* Rx Buffer Size / Length */
	UWORD	cib_NRBUF;	/* Number of Rx buffers	 */
	UWORD	cib_TXSIZ;	/* Tx Buffer Size / Length */
	UWORD	cib_NIBUF;	/* Number of Internal Tx Buffers */
	UWORD	cib_NHBUF;	/* Number of Host Managed Tx Buffers */
	EGL_VECT cib_NVECT;	/* Normal Completion Vector */
	EGL_VECT cib_EVECT;	/* Error Completion Vector */
	UWORD 	cib_BURST;	/* DMA Burst count */
	UWORD	cib_RES1[4];	/* Reserved words */
} EGL_CIB;

typedef struct wqs {		/* Work queue Status */
	union {
		struct {
			Bit  IACT:1;	/* Initialized Active */
			Bit INACT:1;	/* Initialized Inactive */
			Bit      :14;	/* Reserved Bits */
		} b;
		UWORD w;
	} U;
} EGL_WQS;

#define M_WQS_IACT	0x8000
#define M_WQS_INACT	0x4000
#define B_WQS_IACT(x)	((x).U.b.IACT)
#define B_WQS_INACT(x)	((x).U.b.INACT)
#define W_WQS(x)	((x).U.w)

typedef struct wqib {		/* Work Queue Information Block	 */
	EGL_WQS wqib_WQS;	/* Work Queue Status */
	ULONG wqib_MAXENT;	/* Work Queue Max Entries */
	ULONG wqib_CURENT;	/* Work Queue Current Entries */
	UWORD wqib_REF;	/* Work Queue Reference Number */
	UWORD wqib_PRIO;	/* Work Queue Priority */
	UWORD wqib_WDIV;	/* Work Queue Work division */
} EGL_WQIB;

/**************** END Controller Initialization Block (CIB)*****************/


/**************** Command Queue Entry (CQE) *******************/

typedef struct qecr {		/* Queue Entry Control Register */
	union {
		struct {
			Bit    :13;	/* Reserved bits */
			Bit HPC:1;	/* High Priority Command */
			Bit  AA:1;	/* Abort Acknowledge */
			Bit  GO:1;	/* Go/Busy */
		} b;
		UWORD w;
	} U;
} EGL_QECR;

#define M_QECR_HPC	0x0004
#define M_QECR_AA	0x0002
#define M_QECR_GO	0x0001
#define B_QECR_HPC(x)	((x).U.b.HPC)
#define B_QECR_AA(x)	((x).U.b.AA)
#define B_QECR_GO(x)	((x).U.b.GO)
#define W_QECR(x)	((x).U.w)

typedef struct cqe {		/* Command Queue Entry */
	EGL_QECR cqe_QECR;	/* Queue Entry Control Register */
	UWORD cqe_IOPB_ADDR;	/* IOPB Address */
	ULONG cqe_CTAG;		/* Command Tag */
	UBYTE cqe_RES0;		/* Reserved */
	UBYTE cqe_WORK_QUEUE;	/* Work Queue Number */
	UWORD cqe_RES1;		/* Reserved word */
} EGL_CQE;

/**************** END Command Queue Entry (CQE) *******************/


/**************** Command Response Block (CRB) *******************/

typedef struct crsw {		/* Command Response Status Word */
	union {
		struct {
			Bit    :9;	/* Reserved bits */
			Bit QEA:1;	/* Queue Entry Available */
			Bit QMS:1;	/* Queue Mode Started */
			Bit  AQ:1;	/* Abort Queue */
			Bit  EX:1;	/* Exception */
			Bit  ER:1;	/* Error */
			Bit  CC:1;	/* Command Complete */
			Bit CRBV:1;	/* Command Response Block Valid/Clear */
		} b;
		UWORD w;
	} U;
} EGL_CRSW;

#define M_CRSW_QEA	0x0040
#define M_CRSW_QMS	0x0020
#define M_CRSW_AQ	0x0010
#define M_CRSW_EX	0x0008
#define M_CRSW_ER	0x0004
#define M_CRSW_CC	0x0002
#define M_CRSW_CRBV	0x0001
#define B_CRSW_QEA(x)	((x).U.b.QEA)
#define B_CRSW_QMS(x)	((x).U.b.QMS)
#define B_CRSW_AQ(x)	((x).U.b.AQ)
#define B_CRSW_EX(x)	((x).U.b.EX)
#define B_CRSW_ER(x)	((x).U.b.ER)
#define B_CRSW_CC(x)	((x).U.b.CC)
#define B_CRSW_CRBV(x)	((x).U.b.CRBV)
#define W_CRSW(x)	((x).U.w)

typedef struct crb {		/* Command Response Block */
	EGL_CRSW crb_CRSW;	/* Command Response Status Word */
	UWORD crb_RES0;		/* Reserved word */
	ULONG crb_CTAG;		/* Command Tag */
	UBYTE crb_RES1;		/* Reserved byte */
	UBYTE crb_WORK_QUEUE;	/* Work Queue Number */
	UWORD crb_RES2;		/* Reserved word */
} EGL_CRB;

/**************** END Command Response Block (CRB) *******************/


/**************** Configuration Status Block (CSTB) *******************/

typedef struct cstb {		/* Configuration Status Block 72 bytes */
	UBYTE cstb_RES0;	/* Reserved byte */
	UBYTE cstb_PCODE[3];	/* Product Code */
	UBYTE cstb_RES1;	/* Reserved byte */
	UBYTE cstb_PVAR;	/* Product Variation */
	UBYTE cstb_RES2;	/* Reserved byte */
	UBYTE cstb_FREV[3];	/* Firmware Revision level */
	UBYTE cstb_FDATE[8];	/* Firmware Release date */
	UBYTE cstb_RES3;	/* Reserved word */
	UBYTE cstb_SREV[3];	/* Software Revision Level */
	UBYTE cstb_SDATE[8];	/* Software Release Date */
	UWORD cstb_BSIZE;	/* Buffer size in Kbytes */
	ULONG cstb_HUSO;	/* Host Usable Space Offset */
	ULONG cstb_RBPO; 	/* Internal Transmit Buffer Offset	 */
	ULONG cstb_HMTBO;	/* Host Transmit Buffer Offset		 */
	ULONG cstb_HUBO;	/* Host Usable Buffer Memory Offset */
	UBYTE cstb_PHY[6];	/* Current Physical Node Address	 */
	UBYTE cstb_FILT[8];	/* Current Logical Address Filter	 */
	UBYTE cstb_RES4[10]	/* Reserved */
} EGL_CSTB;

/**************** END Configuration Status Block (CSTB) *******************/

/**************** Controller Statistics Block (CSB) *******************/

typedef struct csb {		/* Controller Statistics Block		 */
	ULONG csb_TXATT;	/* Number of tx attempts */
	ULONG csb_TXDMACMP;	/* Number of tx dma completions */
	ULONG csb_LTXINTS;	/* Number of lance tx ints */
	ULONG csb_LGDTXS;	/* Number of good lance tx */
	ULONG csb_TXERRS;	/* Number of lance tx errors */
	ULONG csb_TXDONES;	/* Number of lance tx dones */
	ULONG csb_RXATMPS;	/* Number of lance rx attempts */
	ULONG csb_RXSTARVE;	/* Number of lance rx starvations */
	ULONG csb_RXLINTS;	/* Number of lance rx interrupts */
	ULONG csb_RXGOOD;	/* Number of good lance rx */
	ULONG csb_RXERROR;	/* Number of lance rx errors */
	ULONG csb_RXDMACMP;	/* Number of lance rx dma completions */
	ULONG csb_RXDONE;	/* Number of lance rx done */
	ULONG csb_MISCDMA;	/* Number of miscellaneous dma ints */
	ULONG csb_TOTLINTS;	/* total lance interrupts */
	ULONG csb_CSR0INIT;	/* init done for lance */
	ULONG csb_CSR0BABB;	/* csr0 babble packets */
	ULONG csb_CSR0CERR;	/* csr0 collisions */
	ULONG csb_CSR0MISS;	/* csr0 Miss */
	ULONG csb_CSR0MEM;	/* csr0 Mem errors */
} EGL_CSB;

/**************** END Controller Statistics Block (CSB) *******************/

/**************** IOPB Format (IOPB) *******************/

typedef struct option {		/* IOPB Option word */
	union {
		struct {
			Bit     :6;	/* Reserved bits */
			Bit DATA:1;	/* Transmit - pointer to data only */
			Bit BABL:1;	/* Transmit - ignore babble errors */
			Bit     :5;	/* Reserved bits */
			Bit  DMA:1;	/* DMA Enable */
			Bit   SG:1;	/* Scatter/Gather bit */
			Bit   IE:1;	/* Interrupt Enable */
		} b;
		UWORD w;
	} U;
} EGL_OPT;

#define M_OPT_DATA	0x0200
#define M_OPT_BABL	0x0100
#define M_OPT_DMA	0x0004
#define M_OPT_SG	0x0002
#define M_OPT_IE	0x0001
#define B_OPT_DATA(x)	((x).U.b.DATA)
#define B_OPT_BABL(x)	((x).U.b.BABL)
#define B_OPT_DMA(x)	((x).U.b.DMA)
#define B_OPT_SG(x)	((x).U.b.SG)
#define B_OPT_IE(x)	((x).U.b.IE)
#define W_OPT(x)	((x).U.w)

typedef struct topt {	/* VME bus transfer options */
	union {
		struct {
			Bit      :3;	/* Reserved bits */
			Bit   DIR:1;	/* Direction	 */
			Bit TRANS:2;	/* Transfer Type */
			Bit  MEMT:2;	/* Memory Type */
			Bit      :2;	/* Reserved bits */
			Bit  ADRM:6;	/* Address modifier */
		} b;
		UWORD w;
	} U;
} EGL_TOPT;

#define M_TOPT_DIR	0x1000
#define M_TOPT_TRANS	0x0C00
#define M_TOPT_MEMT	0x0300
#define M_TOPT_MOD	0x003F
#define B_TOPT_DIR(x)	((x).U.b.DIR)
#define B_TOPT_TRANS(x)	((x).U.b.TRANS)
#define B_TOPT_MEMT(x)	((x).U.b.MEMT)
#define B_TOPT_MOD(x)	((x).U.b.ADRM)
#define W_TOPT(x)	((x).U.w)

typedef struct iopb {		/* Transmit / Receive IOPB */
	UWORD iopb_CMD;		/* IOPB Command code 0x50 / 0x60 */
	EGL_OPT iopb_OPTION;	/* IOPB Option word */
	UWORD iopb_STATUS;	/* IOPB Return Status word */
	EGL_VECT iopb_NVCT;	/* IOPB Normal completion Vector */
	EGL_VECT iopb_EVCT;	/* IOPB Error completion Vector */
	EGL_TOPT iopb_TOPT;	/* IOPB VME bus transfer options	 */
	ULONG iopb_BUFF;	/* IOPB Buffer Address */
	ULONG iopb_LENGTH;	/* IOPB Max-Transfer Length */
	UWORD iopb_HBUF;	/* IOPB Host Managed Buffer Number	 */
	UWORD iopb_PTLF;	/* IOPB Packet Type / Length Field	 */
	UBYTE iopb_NODE[6];	/* IOPB Node Address			 */
	UWORD iopb_SGEC;	/* IOPB Scatter / Gather Element  Count */
	UWORD iopb_LAN1;	/* IOPB LANCE Descriptor Word 1		 */
	UWORD iopb_LAN3;	/* IOPB LANCE Descriptor Word 3		 */
} EGL_IOPB;

typedef struct iopb_dma {	/* DMA IOPB */
	UWORD iopb_CMD;		/* IOPB Command code 0x70 */
	EGL_OPT iopb_OPTION;	/* IOPB Option word */
	UWORD iopb_STATUS;	/* IOPB Return Status word */
	EGL_VECT iopb_NVCT;	/* IOPB Normal completion Vector */
	EGL_VECT iopb_EVCT;	/* IOPB Error completion Vector */
	UWORD iopb_BUFF_HI;	/* IOPB Error completion Vector */
	UWORD iopb_BUFF_LO;	/* IOPB VME Buffer Address */
	UWORD iopb_IBUFF_HI;	/* IOPB Host Managed Buffer Address */
	UWORD iopb_IBUFF_LO;	/* IOPB Host Managed Buffer Address */
	UWORD iopb_LENGTH_HI;	/* IOPB Transfer Length */
	UWORD iopb_LENGTH_LO;	/* IOPB Transfer Length */
	EGL_TOPT iopb_TOPT;	/* IOPB VME bus transfer options */
} EGL_DMA_IOPB;

typedef struct ic_iopb {	/* Initialize Controller IOPB		 */
	UWORD ic_iopb_CMD;	/* IOPB Command code 0x41 */
	EGL_OPT ic_iopb_OPTION;	/* IOPB Option word */
	UWORD ic_iopb_STATUS;	/* IOPB Return Status word */
	EGL_VECT ic_iopb_NVCT;	/* IOPB Normal completion Vector */
	EGL_VECT ic_iopb_EVCT;	/* IOPB Error completion Vector */
	UWORD ic_iopb_RES0;	/* IOPB Reserved word		 */
	ULONG ic_iopb_BUFF;	/* IOPB Buffer Address */
	ULONG ic_iopb_RES1[5];	/* IOPB Reserved words */
} EGL_IC_IOPB;

#define CIB_ALIGN	0x1;	/* Mips alignment hack enabling */

typedef struct cn_iopb {	/* Change Node Address IOPB */
	UWORD cn_iopb_CMD;	/* IOPB Command code 0x45 */
	EGL_OPT cn_iopb_OPTION;	/* IOPB Option word */
	UWORD cn_iopb_STATUS;	/* IOPB Return Status word */
	EGL_VECT cn_iopb_NVCT;	/* IOPB Normal completion Vector */
	EGL_VECT cn_iopb_EVCT;	/* IOPB Error completion Vector */
	UWORD cn_iopb_RES0[5];	/* IOPB Reserved word	 */
	UBYTE cn_iopb_PHY[6];	/* IOPB New Node Address */
	UWORD cn_iopb_RES1[5];	/* IOPB Reserved words	 */
} EGL_CN_IOPB;

typedef struct cf_iopb {	/* Change Address Filter IOPB  */
	UWORD cf_iopb_CMD;	/* IOPB Command code 0x46 */
	EGL_OPT cf_iopb_OPTION;	/* IOPB Option word */
	UWORD cf_iopb_STATUS;	/* IOPB Return Status word */
	EGL_VECT cf_iopb_NVCT;	/* IOPB Normal completion Vector */
	EGL_VECT cf_iopb_EVCT;	/* IOPB Error completion Vector */
	UWORD cf_iopb_RES0[5];	/* IOPB Reserved word		 */
	UBYTE cf_iopb_FILT[8];	/* IOPB New Logical Address Filter */
	UWORD cf_iopb_RES1[4];	/* IOPB Reserved words		 */
} EGL_CF_IOPB;

/**************** END IOPB Format (IOPB) *******************/

/**************** Initialize Work Queue Command Format (WQCF)***********/

typedef struct wopt {		/* Work Queue Options */
	union {
		struct {
			Bit IWQ:1;	/* Initialize Work Queue */
			Bit    :14;	/* Reserved bits */
			Bit  AE:1;	/* Abort Enable */
		} b;
		UWORD w;
	} U;
} EGL_WOPT;

#define M_WOPT_IWQ	0x8000
#define M_WOPT_AE	0x0001
#define B_WOPT_IWQ(x)	((x).U.b.IWQ)
#define B_WOPT_AE(x)	((x).U.b.AE)
#define W_WOPT(x)	((x).U.w)

#define EGL_MISCQ	0x00	/* Miscellaneous Work queue */
#define EGL_DMAQ	0x01	/* DMA Work Queue */
#define EGL_RECVQ	0x02	/* Receive Work Queue */
#define EGL_XMTQN(x)	((x) + 0x03)	/* Transmit Work Queue N */


typedef struct wqcf {		/* Initialize Work Queue Command Format */
	UWORD wqcf_CMD;		/* Command Normally (0x42) */
	EGL_OPT wqcf_OPTION;	/* Command Options */
	UWORD wqcf_STATUS;	/* Return Status */
	EGL_VECT wqcf_NVCT;	/* Normal Completion Vector */
	EGL_VECT wqcf_EVCT;	/* Error Completion Vector */
	UWORD wqcf_RES0[5];	/* Reserved Words */
	UWORD wqcf_WORKQ;	/* Work Queue Number */
	EGL_WOPT wqcf_WOPT;	/* Work Queue Options */
	UWORD wqcf_SLOTS;	/* Number of slots in the Work Queues */
	UWORD wqcf_PRIORITY;	/* Priority Level */
	UWORD wqcf_WDIV;	/* Work Division */
	UWORD wqcf_RES1[3];	/* Reserved words */
} EGL_WQCF;

/**************** END Initialize Work Queue Command Format (WQCF)***********/

/**************** Short I/O Format *******************/

typedef struct shortio {
	EGL_MCSB sh_MCSB;		/* Master Control / Status Block */
	EGL_CQE sh_MCE;			/* Master Command Entry */
	EGL_CQE sh_CQE[MAX_CQE];	/* Command Queue Entry */
	EGL_IOPB sh_IOPB[MAX_IOPB];	/* Host IOPB Space */
	EGL_IOPB sh_MCE_IOPB;		/* Host MCE IOPB Space */
	UWORD sh_SCRTCH[N_SCRTCH];	/* Scratch Area for Parameter IOPBS */
	EGL_CRB sh_CRB;			/* Command Response Block */
	UWORD sh_FILL[2];		/* filler		 */
	EGL_IOPB sh_RET_IOPB;		/* Returned IOPB */
	EGL_CSTB sh_CSTB;		/* Controller Status Block */
	EGL_CSB sh_CSB;			/* Controller Statistics Block */
	UWORD sh_PSEM;			/* Printf Semaphores	 */
	UWORD sh_VMEIV;			/* VME bus Interrupt Vector */
} EGL_SHIO;

/**************** END Short I/O Format *******************/

/*
 * Scatter/Gather Descriptor block 
 *
 * +-------------------------------------------------------+
   |			Entry 1 Byte Count		   |
 * +-------------------------------------------------------+
   |			Entry 1 Address MSW		   |
 * +-------------------------------------------------------+
   |			Entry 1 Address LSW		   |
 * +-------------------------------------------------------+
   | 	Entry 1 Memory Type  | Entry 1 Address Modifier    |
 * +-------------------------------------------------------+ . . .
 * +-------------------------------------------------------+ | 
 * Entry 128 Byte Count |
 * +-------------------------------------------------------+ | 
 * Entry 128 Address MSW |
 * +-------------------------------------------------------+ | 
 * Entry 128 Address LSW |
 * +-------------------------------------------------------+ | Entry 128
 * Memory Type | Entry 128 Address Modifier |
 * +-------------------------------------------------------+ 
 */
#define MAX_SG_ENTRIES	128	/* Max Number of SG block address entries */
typedef struct {
	UWORD sg_bcount;	/* Byte Count for transfer */
	ULONG sg_paddr;		/* Physical Address */
	EGL_TOPT sg_TOPT;	/* IOPB VME bus transfer options	 */
} SGENTRY;
/* $Header: if_egl.h,v 1.2.2.3 90/05/09 15:07:01 wje Exp $ 	Interphase source id */
/* $Locker:  $ 	Interphase source control */
/* ************************************************************************ */
/*      if_eglreg.h for eagle ethernet board							    */
/* ************************************************************************ */

#define EGLGETDBG	_IOWR(i, 33, struct ifreq) /* Get debugging level */
#define EGLSETDBG	_IOWR(i, 34, struct ifreq) /* Set debugging level */
#define EGLRESET	_IOWR(i, 35, struct ifreq) /* Physical Reset of Eagle */
#define EGLBLAST	_IOWR(i, 36, struct ifreq) /* Special blast of eagle */
#define EGLGTSTK	_IOWR(i, 37, struct ifreq) /* Get trans stacked up */

#define ADRM_STD_S_P            0x3E    /*    Standard Supervisory Program  */
#define ADRM_STD_S_D            0x3D    /*    Standard Supervisory Data     */
#define ADRM_STD_BLK		0x3B	/*    Standard BLOCK mode	    */
#define ADRM_STD_N_P            0x3A    /*    Standard Normal Program       */
#define ADRM_STD_N_D            0x39    /*    Standard Normal Data          */
#define ADRM_SHT_S_IO           0x2D    /*    Short Supervisory IO          */
#define ADRM_SHT_N_IO           0x29    /*    Short Normal IO               */
#define ADRM_EXT_S_P            0x0E    /*    Extended Supervisory Program  */
#define ADRM_EXT_S_D            0x0D    /*    Extended Supervisory Data     */
#define ADRM_EXT_N_P            0x0A    /*    Extended Normal Program       */
#define ADRM_EXT_N_D            0x09    /*    Extended Normal Data          */
#define ADRM_EXT_BLK            0x0B    /*    BLOCK Mode          */

/*  Eagle Control IOPB's                                                    */
#define CNTR_DIAG		0x40	/* Perform Diagnostics      */
#define CNTR_INIT		0x41	/* Initialize Controller    */
#define CNTR_INIT_WORKQ		0x42	/* Initialize Work Queue            */
#define CNTR_DUMP_INIT		0x43	/* Dump Initialization Parameters   */
#define CNTR_REPORT_STATS	0x44	/* Report Statistics		    */
#define CNTR_CHANGE_NODE	0x45	/* Change Default Node Address	    */
#define CNTR_CHANGE_FILTER	0x46	/* Change Def. Logical Address Filter*/
#define CNTR_FLUSH_WORKQ	0x49	/* Flush Work Queue                 */
#define CNTR_TRANSMIT		0x50	/* Transmit			    */
#define CNTR_RECEIVE		0x60	/* Receive			    */
#define CNTR_DMA		0x70	/* DMA			    */

/*  Memory types */
#define MEMT_16BIT		1	/* 16 Bit Memory type */
#define MEMT_32BIT		2	/* 32 Bit Memory type */
#define MEMT_SHIO 		3	/* Short I/O Memory   */

/*  Transfer types  */
#define TT_NORMAL		0	/* Normal Mode */
#define TT_BLOCK		1	/* Block  Mode */
#define TT_DISABLE_INC_ADDR	2	/* Disable Incrementing Addresses   */

#define DIR_READ		0x1000	/* Read from Eagle */
#define DIR_WRITE		0x0000	/* Write to Eagle  */

/*	LANCE error bits				*/
#define LANCE_RERR		0x4000	/* Receive Error	*/
#define LAN1_FRAM		0x2000	/* Framing error 	*/
#define LAN1_OFLO		0x1000	/* Overflow error	*/
#define LAN1_CRC		0x0800	/* CRC error		*/
#define LAN1_BUFF		0x0400	/* Buffer error		*/
#define LANCE_RENP		0x0100	/* End of Packet	*/
#define LAN1_RBITS "\20\16OWN\15ERR\14FRAM\13OFLO\12CRC\11BUFF\10STP\09ENDP"

#define LANCE_TERR		0x4000	/* Transmit Error	*/
#define LANCE_TMORE		0x1000	/* More Than One Retry	*/
#define LANCE_TONE		0x0800	/* Exactly One Retry	*/
#define LAN3_BUFF		0x8000	/* Transmit Buffer error	*/
#define LAN3_UFLO		0x4000	/* Underflow error	*/
#define LAN3_LCOL		0x1000	/* Late collision	*/
#define LAN3_LCAR		0x0800	/* Loss of carrier	*/
#define LAN3_RTRY		0x0400	/* Retry error		*/
#define LAN1_TBITS "\20\16OWN\15ERR\13MORE\12ONE\11DEF\10STP\09ENDP"
#define LAN3_TBITS "\20\16BUFF\15UFLO\13LCOL\12LCAR\11RTRY"
