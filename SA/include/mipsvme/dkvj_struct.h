/*
 *   VJ_struct.h : V/SCSI 4210 Jaguar MACSI drive header.
 *
 *   Created: 06/08/87 by Interphase Corp.
 *
 *   Author: Keith Wiles
 *
 *   Contains all of the structs and types for Jaguar MACSI.
 *
 */

#define NUM_CQE         20
#define MAX_IOPB        64
#define NUM_CQE_OFF	40 /* max allowed off-board */
#define NUM_IOPB        NUM_CQE
#define S_IOPB_RES      (MAX_IOPB - sizeof(VJ_short_IOPB))
#define S_SHORTIO       2048
#define S_IOPB          sizeof(VJ_IOPB)
#define S_CIB           sizeof(VJ_CIB)
#define S_MCSB          sizeof(VJ_MCSB)
#define S_MCE           sizeof(VJ_CQE)
#define S_CQE_OFF       (sizeof(VJ_CQE) * NUM_CQE_OFF)
#define S_CQE           (sizeof(VJ_CQE) * NUM_CQE)
#define S_HIOPB         (sizeof(VJ_IOPB) * NUM_IOPB)
#define S_HSB           sizeof(VJ_HSB)
#define S_CRB           sizeof(VJ_CRB)
#define S_CSS           sizeof(VJ_CSB)
#define S_NOT_HOST      (S_MCSB + S_MCE + S_CQE + S_HIOPB + S_IOPB + \
                         S_CIB + S_HSB + S_CRB + S_IOPB + S_CSS)
#define S_NOT_HOST_OFF  (S_MCSB + S_MCE + S_CQE_OFF + S_IOPB + \
			 S_CIB + S_HSB + S_CRB + S_IOPB + S_CSS)
#define S_HUS_FREE      (S_SHORTIO - S_NOT_HOST)
#define S_HUS_FREE_OFF  (S_SHORTIO - S_NOT_HOST_OFF)

#define S_WQCF          sizeof(VJ_WQCF)

#define HOST_ID         0x4321
#define WORDP(x)        ((x)->U.w)
#define WORD_(x)        ((x).U.w)

/****************     Master Control Status Block (MCSB) *******************/

typedef struct msr {                /* Master Status Register              */
    union {
        struct {
            Bit     RSRV : 13;      /* reserved bits                       */
            Bit     QFC  :  1;      /* Queue Flush Complete                */
            Bit     BOK  :  1;      /* Board OK                            */
            Bit     CNA  :  1;      /* Controller Not Available            */
        } b;
        UWORD   w;
    } U;
} VJ_MSR;

#define M_MSR_QFC       0x0004
#define M_MSR_BOK       0x0002
#define M_MSR_CNA       0x0001
#define B_MSR_QFC(x)    ((x).U.b.QFC)
#define B_MSR_BOK(x)    ((x).U.b.BOK)
#define B_MSR_CNA(x)    ((x).U.b.CNA)
#define W_MSR(x)        ((x).U.w)

typedef struct mcr {                /* Master Control Register             */
    union {
        struct {
            Bit     RSRV0 : 2;      /* reserved bits                       */
            Bit     SFEN  : 1;      /* Sysfail Enable                      */
            Bit     RES   : 1;      /* Reset controller                    */
            Bit     FLQ   : 1;      /* Flush Queue                         */
            Bit           : 8;      /* reserved bits                       */
            Bit     FLQR  : 1;      /* Flush Queue and Report              */
            Bit           : 1;      /* reserved bits                       */
            Bit     SQM   : 1;      /* Start Queue Mode                    */
        } b;
        UWORD   w;
    } U;
} VJ_MCR;

#define M_MCR_SFEN      0x2000
#define M_MCR_RES       0x1000
#define M_MCR_FLQ       0x0800
#define M_MCR_FLQR      0x0004
#define M_MCR_SQM       0x0001
#define B_MCR_SFEN(x)   ((x).U.b.SFEN)
#define B_MCR_RES(x)    ((x).U.b.RES)
#define B_MCR_FLQ(x)    ((x).U.b.FLQ)
#define B_MCR_FLQR(x)   ((x).U.b.FLQR)
#define B_MCR_SQM(x)    ((x).U.b.SQM)
#define W_MCR(x)        ((x).U.w)

typedef struct iqar {               /* Interrupt on Queue Available Reg    */
    union {
        struct {
            Bit     IQEA : 1;       /* Interrupt on Queue entry Available  */
            Bit     IQEH : 1;       /* Interrupt on Queue Half Empty Enable*/
            Bit          : 3;       /* Reserved bits                       */
            Bit     ILVL : 3;       /* Interrupt Level on Queue Available  */
            Bit     IVCT : 8;       /* Interrupt Vector on Queue Available */
        } b;
        UWORD   w;
    } U;
} VJ_IQAR;

#define M_IQAR_IQEA     0x8000
#define M_IQAR_IQED     0x4000
#define M_IQAR_ILVL     0x0700
#define M_IQAR_IVCT     0x00FF
#define B_IQAR_IQEA(x)  ((x).U.b.IQEA)
#define B_IQAR_IQED(x)  ((x).U.b.IQED)
#define B_IQAR_ILVL(x)  ((x).U.b.ILVL)
#define B_IQAR_IVCT(x)  ((x).U.b.IVCT)
#define W_IQAR(x)       ((x).U.w)

typedef struct thaw {               /* Unfreeze the given work queue       */
    union {
        struct {
            Bit     TWQN : 8;       /* Thaw Work Queue Number              */
            Bit          : 7;       /* Reserved bits                       */
            Bit     TWQE : 1;       /* Thaw Work Queue Enable              */
        } b;
        UWORD   w;
    } U;
} VJ_THAW;

#define M_THAW_TWQN     0xff00
#define M_THAW_TWQE     0x0001
#define B_THAW_TWQN(x)  ((x).U.b.TWQN)
#define B_THAW_TWQE(x)  ((x).U.b.TWQE)
#define W_THAW(x)       ((x).U.w)

typedef struct mcsb {               /* Master control/Status Block         */
    VJ_MSR      mcsb_MSR;           /* Master status register              */
    VJ_MCR      mcsb_MCR;           /* Master Control register             */
    VJ_IQAR     mcsb_IQAR;          /* Interrupt on Queue Available Reg    */
    UWORD       mcsb_QHDP;          /* Queue head pointer                  */
    VJ_THAW     mcsb_THAW;          /* Thaw work Queue                     */
    UWORD       mcsb_RES0;          /* Reserved word 0                     */
    UWORD       mcsb_RES1;          /* Reserved word 1                     */
    UWORD       mcsb_RES2;          /* Reserved word 2                     */
} VJ_MCSB;

/**************** END Master Control Status Block (MCSB) *******************/

/****************     Host Semaphore Block (HSB)         *******************/

typedef struct hsb {                /* Host Semaphore Block                */
    UWORD   hsb_INITQ;              /* Init MCE Flag                       */
    UWORD   hsb_WORKQ;              /* Work Queue number                   */
    UWORD   hsb_MAGIC;              /* Magic word                          */
    UWORD   hsb_RES0;               /* Reserved word                       */
} VJ_HSB;

/**************** END Host Semaphore Block (HSB)         *******************/

/****************     Perform Diagnostics Command Format *******************/

typedef struct pdcf {               /* Perform Diagnostics Command Format  */
    UWORD   pdcf_CMD;               /* Command normally 0x40               */
    UWORD   pdcf_RES0;              /* Reserved word                       */
    UWORD   pdcf_STATUS;            /* Return Status                       */
    UWORD   pdcf_RES1;              /* Reserved Word                       */
    UWORD   pdcf_ROM;               /* ROM Test Results                    */
    UWORD   pdcf_BUFRAM;            /* Buffer RAM results                  */
    UWORD   pdcf_EVENT_RAM;         /* Event Ram test Results              */
    UWORD   pdcf_SCSI_PRI_PORT;     /* SCSI Primary Port Register test     */
    UWORD   pdcf_SCSI_SEC_PORT;     /* SCSI Secondary Port Register test   */
} VJ_PDCF;
#define PDCF_SUCCESS        0xFFFF

/**************** END Perform Diagnostics Command Format *******************/

/***************      Controller Initialization Block (CIB)*****************/

typedef struct vect {               /* Vector structure                    */
    union {
        struct {
            Bit          : 5;       /* Reserved bits                       */
            Bit     ILVL : 3;       /* Interrupt Level                     */
            Bit     IVCT : 8;       /* Interrupt Vector                    */
        } b;
        UWORD   w;
    } U;
} VJ_VECT;

#define M_VECT_ILVL     0x0700
#define M_VECT_IVCT     0x00FF
#define B_VECT_ILVL(x)  ((x).U.b.ILVL)
#define B_VECT_IVCT(x)  ((x).U.b.IVCT)
#define W_VECT(x)       ((x).U.w)

typedef struct psid {               /* Primary/Secondary bus ID            */
    union {
        struct {
            Bit          : 12;      /* Reserved bits                       */
            Bit     DFT  : 1;       /* Default ID enable                   */
            Bit     ID   : 3;       /* Primary/Secondary SCSI ID           */
        } b;
        UWORD   w;
    } U;
} VJ_PSID;

#define M_PSID_DFT      0x0008
#define M_PSID_ID       0x0007
#define B_PSID_DFT(x)   ((x).U.b.DFT)
#define B_PSID_ID(x)    ((x).U.b.ID)
#define W_PSID(x)       ((x).U.w)

typedef struct addr {               /* Address type and Modifier           */
    union {
        struct {
            Bit           : 4;      /* Reserved bits                       */
            Bit     TRANS : 2;      /* Transfer Type                       */
            Bit     MEMT  : 2;      /* Memory   Type                       */
            Bit     ADRM  : 8;      /* Address modifier                    */
        } b;
        UWORD   w;
    } U;
} VJ_ADR;


typedef struct cib {                /* Controller Initialization Block     */
    UWORD   cib_NCQE;               /* Number of Command Queue Entries     */
    UWORD   cib_BURST;              /* DMA Burst count                     */
    VJ_VECT cib_NVECT;              /* Normal Completion Vector            */
    VJ_VECT cib_EVECT;              /* Error Completion Vector             */
    VJ_PSID cib_PID;                /* Primary SCSI Bus ID                 */
    VJ_PSID cib_SID;                /* Secondary SCSI Bus ID               */
    UWORD   cib_CRBO;               /* Command Response Block Offset       */
    UWORD   cib_SELECT_msw;         /* Selection timeout in milli-second   */
    UWORD   cib_SELECT_lsw;         /* Selection timeout in milli-second   */
    UWORD   cib_RESELECT_msw;       /* Reselection timeout in milli-second */
    UWORD   cib_RESELECT_lsw;       /* Reselection timeout in milli-second */
    UWORD   cib_RES0[5];            /* Reserved words                      */
} VJ_CIB;

/**************** END Controller Initialization Block (CIB)*****************/

/****************     Command Queue Entry (CQE)          *******************/

typedef struct qecr {               /* Queue Entry Control Register        */
    union {
        struct {
            Bit          : 4;       /* Reserved bits                       */
            Bit     IOPB : 4;       /* IOPB type   ( must be zero )        */
            Bit          : 5;       /* Reserved bits                       */
            Bit     HPC  : 1;       /* High Priority Command               */
            Bit     AA   : 1;       /* Abort Acknowledge                   */
            Bit     GO   : 1;       /* Go/Busy                             */
        } b;
        UWORD   w;
    } U;
} VJ_QECR;

#define M_QECR_IOPB     0x0F00
#define M_QECR_FOB      0x0010
#define M_QECR_FA       0x0008
#define M_QECR_HPC      0x0004
#define M_QECR_AA       0x0002
#define M_QECR_GO       0x0001
#define B_QECR_IOPB(x)  ((x).U.b.IOPB)
#define B_QECR_FA(x)    ((x).U.b.FA)
#define B_QECR_HPC(x)   ((x).U.b.HPC)
#define B_QECR_AA(x)    ((x).U.b.AA)
#define B_QECR_GO(x)    ((x).U.b.GO)
#define W_QECR(x)       ((x).U.w)

typedef struct cqe {                /* Command Queue Entry                 */
    VJ_QECR     cqe_QECR;           /* Queue Entry Control Register        */
    UWORD       cqe_IOPB_ADDR;      /* IOPB Address                        */
    ULONG       cqe_CTAG;           /* Command Tag                         */
    UBYTE       cqe_IOPB_LENGTH;    /* IOPB Length                         */
    UBYTE       cqe_WORK_QUEUE;     /* Work Queue Number                   */
#ifdef LATER
    ULONG       cqe_LINKP;          /* CQE Link pointer                    */
#endif
    UWORD       cqe_RES0;           /* Reserved word                       */
} VJ_CQE;

typedef struct cqe_off {            /* Command Queue Entry for off-board   */
    VJ_QECR     cqe_QECR;           /* Queue Entry Control Register        */
    VJ_ADR	cqe_ADDR_TYPE;      /* CQE Address type and modifier       */
    UWORD       cqe_HOST_ADDR_msw;  /* HOST Address                        */
    UWORD       cqe_HOST_ADDR_lsw;  /* HOST Address                        */
    UBYTE       cqe_IOPB_LENGTH;    /* IOPB Length                         */
    UBYTE       cqe_RES0;           /* Reserved byte                       */
    UWORD       cqe_RES1;           /* Reserved word                       */
} VJ_CQE_OFF;

/**************** END Command Queue Entry (CQE)          *******************/

/****************     Command Response Block (CRB)       *******************/

typedef struct crsw {               /* Command Response Status Word        */
    union {
        struct {
            Bit           : 9;      /* Reserved bits                       */
            Bit     CQA   : 1;      /* Command Queue Entry Available       */
            Bit     QMS   : 1;      /* Queue Mode Started                  */
            Bit     AQ    : 1;      /* Abort Queue                         */
            Bit     EX    : 1;      /* Exception                           */
            Bit     ER    : 1;      /* Error                               */
            Bit     CC    : 1;      /* Command Complete                    */
            Bit     CRBV  : 1;      /* Command Response Block Valid/Clear  */
        } b;
        UWORD   w;
    } U;
} VJ_CRSW;

#define M_CRSW_CQA      0x0040
#define M_CRSW_QMS      0x0020
#define M_CRSW_AQ       0x0010
#define M_CRSW_EX       0x0008
#define M_CRSW_ER       0x0004
#define M_CRSW_CC       0x0002
#define M_CRSW_CRBV     0x0001
#define B_CRSW_CQA(x)   ((x).U.b.CQA)
#define B_CRSW_QMS(x)   ((x).U.b.QMS)
#define B_CRSW_AQ(x)    ((x).U.b.AQ)
#define B_CRSW_EX(x)    ((x).U.b.EX)
#define B_CRSW_ER(x)    ((x).U.b.ER)
#define B_CRSW_CC(x)    ((x).U.b.CC)
#define B_CRSW_CRBV(x)  ((x).U.b.CRBV)
#define W_CRSW(x)       ((x).U.w)

typedef struct crb {                /* Command Response Block              */
    VJ_CRSW     crb_CRSW;           /* Command Response Status Word        */
    UWORD       crb_RES0;           /* Reserved word                       */
    ULONG       crb_CTAG;           /* Command Tag                         */
    UBYTE       crb_IOPB_LENGTH;    /* IOPB Length                         */
    UBYTE       crb_WORK_QUEUE;     /* Work Queue Number                   */
    UWORD       crb_RES1;           /* Reserved word                       */
} VJ_CRB;

/**************** END Command Response Block (CRB)       *******************/

/****************     Configuration Status Block (CSB)   *******************/

typedef struct csb {                /* Configuration Status Block 120 bytes*/
    UWORD   csb_RES0;               /* Reserved word                       */
    UBYTE   csb_RES1;               /* Reserved byte                       */
    UBYTE   csb_PCODE[3];           /* Product Code                        */
    UWORD   csb_RES2;               /* Reserved word                       */
    UBYTE   csb_RES3;               /* Reserved byte                       */
    UBYTE   csb_PVAR;               /* Product Variation                   */
    UWORD   csb_RES4;               /* Reserved word                       */
    UBYTE   csb_RES5;               /* Reserved byte                       */
    UBYTE   csb_FREV[3];            /* Firmware Revision level             */
    UWORD   csb_RES6;               /* Reserved word                       */
    UBYTE   csb_FDATE[8];           /* Firmware Release date               */
    UWORD   csb_RES7;               /* Reserved word                       */
    UWORD   csb_BSIZE;              /* Buffer size in Kbytes               */
    UBYTE   csb_RES8[4];            /* Reserved 4 bytes                    */
    UBYTE   csb_PID;                /* Primary Bus ID                      */
    UBYTE   csb_SID;                /* Secondary Bus ID                    */
    UBYTE   csb_PSEL;               /* Last device selected on primary Bus */
    UBYTE   csb_SSEL;               /* Last device selected on secondary Bus */
    UBYTE   csb_PBST;               /* Primary Bus status bits             */
    UBYTE   csb_SBST;               /* Secondary Bus status bits           */
    UBYTE   csb_RES9[80];           /* Reserved bytes                      */
} VJ_CSB;

/**************** END Configuration Status Block (CSB)   *******************/

/****************     IOPB Format (IOPB)                 *******************/

typedef struct option {             /* IOPB Option word                    */
    union {
        struct {
            Bit           : 7;      /* Reserved bits                       */
            Bit     DIR   : 1;      /* Vme Direction bit                   */
            Bit           : 6;      /* Reserved bits                       */
            Bit     SG    : 1;      /* Scatter/Gather                      */
            Bit     IE    : 1;      /* Interrupt Enable                    */
        } b;
        UWORD   w;
    } U;
} VJ_OPT;

#define M_OPT_DIR       0x0100
#define M_OPT_IE        0x0001
#define M_OPT_SG        0x0002
#define B_OPT_DIR(x)    ((x).U.b.DIR)
#define B_OPT_IE(x)     ((x).U.b.IE)
#define B_OPT_SG(x)     ((x).U.b.SG)
#define W_OPT(x)        ((x).U.w)

#define M_ADR_TRANS     0x0C00
#define M_ADR_MEMT      0x0300
#define M_ADR_MOD       0x00FF
#define B_ADR_TRANS(x)  ((x).U.b.TRANS)
#define B_ADR_MEMT(x)   ((x).U.b.MEMT)
#define B_ADR_MOD(x)    ((x).U.b.ADRM)
#define W_ADR(x)        ((x).U.w)

typedef struct unit {               /* SCSI Unit address                   */
    union {
        struct {
            Bit     EXT_ADR : 8;    /* Extended Adress                     */
            Bit     EXT     : 1;    /* Extended Address Enable             */
            Bit     BUS     : 1;    /* SCSI Bus Selection                  */
            Bit     LUN     : 3;    /* Logical Unit Number                 */
            Bit     SCSI_ID : 3;    /* SCSI Device ID                      */
        } b;
        UWORD   w;
    } U;
} VJ_UADR;

#define M_UNIT_EXT_ADR      0xFF00
#define M_UNIT_EXT          0x0080
#define M_UNIT_BUS          0x0040
#define M_UNIT_LUN          0x0038
#define M_UNIT_ID           0x0007
#define B_UNIT_EXT_ADR(x)   ((x).U.b.EXT_ADR)
#define B_UNIT_EXT(x)       ((x).U.b.EXT)
#define B_UNIT_BUS(x)       ((x).U.b.BUS)
#define B_UNIT_LUN(x)       ((x).U.b.LUN)
#define B_UNIT_ID(x)        ((x).U.b.SCSI_ID)
#define W_UNIT(x)           ((x).U.w)

typedef struct short_iopb {
    UWORD   iopb_CMD;               /* IOPB Command code                   */
    VJ_OPT  iopb_OPTION;            /* IOPB Option word                    */
    UWORD   iopb_STATUS;            /* IOPB Return Status word             */
    UWORD   iopb_RES0;              /* IOPB Reserved word                  */
    UBYTE   iopb_NVCT;              /* IOPB Normal completion Vector       */
    UBYTE   iopb_EVCT;              /* IOPB Error  completion Vector       */
    UWORD   iopb_LEVEL;             /* IOPB Interrupt Level                */
    UWORD   iopb_RES1;              /* IOPB Reserved word                  */
    VJ_ADR  iopb_ADDR;              /* IOPB Address type and modifer       */
    ULONG   iopb_BUFF;              /* IOPB Buffer Address                 */
    ULONG   iopb_LENGTH;            /* IOPB Max-Transfer Length            */
    ULONG   iopb_TTLENGTH;          /* IOPB Total transfer length for S/G  */
    UWORD   iopb_RES2;              /* IOPB Reserved word                  */
    VJ_UADR iopb_UNIT;              /* IOPB Unit address on SCSI bus       */
} VJ_short_IOPB;

typedef struct iopb {
    UWORD   iopb_CMD;               /* IOPB Command code                   */
    VJ_OPT  iopb_OPTION;            /* IOPB Option word                    */
    UWORD   iopb_STATUS;            /* IOPB Return Status word             */
    UWORD   iopb_RES0;              /* IOPB Reserved word                  */
    UBYTE   iopb_NVCT;              /* IOPB Normal completion Vector       */
    UBYTE   iopb_EVCT;              /* IOPB Error  completion Vector       */
    UWORD   iopb_LEVEL;             /* IOPB Interrupt Level                */
    UWORD   iopb_RES1;              /* IOPB Reserved word                  */
    VJ_ADR  iopb_ADDR;              /* IOPB Address type and modifer       */
    ULONG   iopb_BUFF;              /* IOPB Buffer Address                 */
    ULONG   iopb_LENGTH;            /* IOPB Max-Transfer Length            */
    ULONG   iopb_TTLENGTH;          /* IOPB Total transfer length for S/G  */
    UWORD   iopb_RES2;              /* IOPB Reserved word                  */
    VJ_UADR iopb_UNIT;              /* IOPB Unit address on SCSI bus       */
    UWORD   iopb_SCSI[S_IOPB_RES/2];/* IOPB SCSI words for pass through    */
} VJ_IOPB;

/**************** END IOPB Format (IOPB)                 *******************/

/****************     SCSI reset Command Format (RESCF) *****************/

typedef struct busid {			/* Work Queue Options		*/
    union {
	struct {
	    Bit     BUS  : 1;		/* Bus ID			*/
	    Bit          : 15;		/* Reserved bits		*/
        } b;
	UWORD   w;
    } U;
} VJ_BUSID;

typedef struct rescf {/* SCSI reset Command Format*/
    UWORD	rescf_CMD;		/* Command Normally (0x22)	*/
    VJ_OPT	rescf_OPTION;		/* Command Options		*/
    UWORD	rescf_STATUS;		/* Return Status		*/
    UWORD	rescf_RES0;		/* Reserved word		*/
    UBYTE	rescf_NVCT;		/* Normal Completion Vector	*/
    UBYTE	rescf_EVCT;		/* Error  Completion Vector	*/
    UWORD	rescf_ILVL;		/* Interrupt Level		*/
    UWORD	rescf_RES1[8];		/* Reserved words		*/
    VJ_BUSID	rescf_BUSID;		/* SCSI bus id			*/
} VJ_RESCF;

/**************** END SCSI reset Command Format (WQCF) ******************/

/****************     Initialize Work Queue Command Format (WQCF)********/

typedef struct wopt {               /* Work Queue Options                  */
    union {
        struct {
            Bit     IWQ  : 1;       /* Initialize Work Queue               */
            Bit          : 12;      /* Reserved bits                       */
            Bit     FE   : 1;       /* Freeze on error enable              */
            Bit     TM   : 1;       /* Target Mode Enable                  */
            Bit     AE   : 1;       /* Abort Enable                        */
        } b;
        UWORD   w;
    } U;
} VJ_WOPT;

#define M_WOPT_IWQ      0x8000
#define M_WOPT_FE       0x0004
#define M_WOPT_TM       0x0002
#define M_WOPT_AE       0x0001
#define B_WOPT_IWQ(x)   ((x).U.b.IWQ)
#define B_WOPT_FE(x)    ((x).U.b.FE)
#define B_WOPT_TM(x)    ((x).U.b.TM)
#define B_WOPT_AE(x)    ((x).U.b.AE)
#define W_WOPT(x)       ((x).U.w)

typedef struct wqcf {               /* Initialize Work Queue Command Format*/
    UWORD   wqcf_CMD;               /* Command Normally (0x42)             */
    VJ_OPT  wqcf_OPTION;            /* Command Options                     */
    UWORD   wqcf_STATUS;            /* Return Status                       */
    UWORD   wqcf_RES0;              /* Reserved word                       */
    UBYTE   wqcf_NVCT;              /* Normal Completion Vector            */
    UBYTE   wqcf_EVCT;              /* Error  Completion Vector            */
    UWORD   wqcf_ILVL;              /* Interrupt Level                     */
    UWORD   wqcf_RES1[8];           /* Reserved words                      */
    UWORD   wqcf_WORKQ;             /* Work Queue Number                   */
    VJ_WOPT wqcf_WOPT;              /* Work Queue Options                  */
    UWORD   wqcf_SLOTS;             /* Number of slots int the Work Queues */
    UWORD   wqcf_PRIORITY;          /* Priority Level                      */
} VJ_WQCF;

/**************** END Initialize Work Queue Command Format (WQCF)***********/

/**************** Flush Work Queue Command Format (FQCF)*****************/

typedef struct fopt {		/* Flush Work Queue Options		*/
    union {
        struct {
            Bit          : 6;	/* Reserved bits			*/
            Bit     RIP  : 1;	/* Reset SCSI bus on command in progress*/
            Bit     RPT  : 1;	/* Report flushed commands		*/
            Bit          : 7;	/* Reserved bits			*/
            Bit     IE   : 1;	/* Interrupt enable			*/
        } b;
        UWORD   w;
    } U;
} VJ_FOPT;

#define M_FOPT_RIP      0x0200
#define M_FOPT_RPT      0x0100
#define M_FOPT_IE       0x0001

typedef struct fst {		/* Flush Work Queue Status		*/
	UWORD	sip	: 1;	/* Secondary bus command in progress	*/
	UWORD	pip	: 1;	/* Primary bus command in progress	*/
	UWORD	number	: 14;	/* Number of commands flushed		*/
} VJ_FST;


typedef struct fqcf {		/* Flush Work Queue Command Format	*/
    UWORD	fqcf_CMD;	/* Command Normally (0x49)		*/
    VJ_FOPT	fqcf_OPTION;	/* Command Options			*/
    UWORD	fqcf_STATUS;	/* Return Status			*/
    UWORD	fqcf_RES0;	/* Reserved word			*/
    UBYTE	fqcf_NVCT;	/* Normal Completion Vector		*/
    UBYTE	fqcf_EVCT;	/* Error  Completion Vector		*/
    UWORD	fqcf_ILVL;	/* Interrupt Level			*/
    UWORD	fqcf_RES1[8];	/* Reserved words			*/
    UWORD	fqcf_WORKQ;	/* Work Queue Number			*/
    VJ_FST	fqcf_FST;	/* Flush status				*/
} VJ_FQCF;

/**************** END Flush Work Queue Command Format (FQCF)***********/

/****************     Short I/O Format                   *******************/

typedef struct shortio {
    VJ_MCSB     sh_MCSB;            /* Master Control / Status Block       */
    VJ_CQE      sh_MCE;             /* Master Command Entry                */
    VJ_CQE      sh_CQE[NUM_CQE];    /* Command Queue Entry                 */
    VJ_IOPB     sh_IOPB[NUM_IOPB];  /* Host IOPB   Space                   */
    VJ_IOPB     sh_MCE_IOPB;        /* Host MCE IOPB Space                 */
    VJ_CIB      sh_CIB;             /* Controller Initialization Block     */
    UBYTE       sh_HUS[S_HUS_FREE]; /* Host Usable Space                   */
    VJ_HSB      sh_HSB;             /* Host Semaphore Block                */
    VJ_CRB      sh_CRB;             /* Command Response Block              */
    VJ_IOPB     sh_RET_IOPB;        /* Returned IOPB                       */
    VJ_CSB      sh_CSS;             /* Controller Specific Space/Block     */
} VJ_SHIO;

typedef struct shortioff {          /* short io for off-board stuff        */
    VJ_MCSB     sh_MCSB;            /* Master Control / Status Block       */
    VJ_CQE      sh_MCE;             /* Master Command Entry                */
    VJ_CQE_OFF  sh_CQE_OFF[NUM_CQE_OFF];/* Command Queue Entry             */
    UBYTE       sh_HUS[S_HUS_FREE_OFF]; /* Host Usable Space               */
    VJ_IOPB     sh_MCE_IOPB;        /* Host MCE IOPB Space                 */
    VJ_CIB      sh_CIB;             /* Controller Initialization Block     */
    VJ_HSB      sh_HSB;             /* Host Semaphore Block                */
    VJ_CRB      sh_CRB;             /* Command Response Block              */
    VJ_IOPB     sh_RET_IOPB;        /* Returned IOPB                       */
    VJ_CSB      sh_CSS;             /* Controller Specific Space/Block     */
} VJ_SHIO_OFF;

/**************** END Short I/O Format                   *******************/

/* Scatter/Gather Descriptor block
 *
 *  +-------------------------------------------------------+
 *  |                Entry 1 Byte Count                     |
 *  +-------------------------------------------------------+
 *  |                Entry 1 Address MSW                    |
 *  +-------------------------------------------------------+
 *  |                Entry 1 Address LSW                    |
 *  +-------------------------------------------------------+
 *  | Entry 1 Memory Type    | Entry 1 Address Modifier     |
 *  +-------------------------------------------------------+
 *                               .
 *               .
 *               .
 *  +-------------------------------------------------------+
 *  |                Entry 128 Byte Count                   |
 *  +-------------------------------------------------------+
 *  |                Entry 128 Address MSW                  |
 *  +-------------------------------------------------------+
 *  |                Entry 128 Address LSW                  |
 *  +-------------------------------------------------------+
 *  | Entry 128 Memory Type  | Entry 128 Address Modifier   |
 *  +-------------------------------------------------------+
 */
#define MAX_SG_ENTRIES  128 /* Max Number of SG block address entries */
typedef struct {
    UWORD   sg_count;
    UWORD   sg_addr_msw;
    UWORD   sg_addr_lsw;
    UBYTE   sg_meminfo;
    UBYTE   sg_addrmod;
} IPSG;

/****************    OFF BOARD CQE/IOPB    ***************************/
typedef struct offbd_iopb {
	VJ_CQE  copycqe;
	VJ_IOPB copyiopb;
} OFFBD_IOPB;

