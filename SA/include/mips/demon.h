#ident "$Header: demon.h,v 1.2 90/01/23 14:10:51 huang Exp $"
/* $Copyright$ */

#include "cpu.h"
#include "fpu.h"

/************************************************************************/
/*      system addresses and sizes                                      */
/************************************************************************/
#ifdef ALTERNATE_MEMORY_BASE
#define MEMORY_OFFSET 0x10100000
#else
#define MEMORY_OFFSET 0x0
#endif
#define MONMEM_BASE (K1BASE + MEMORY_OFFSET)
#define MONMEM_SIZE 0x8000
#define MONDATA MONMEM_BASE 
#define MONSTACK_OFFSET (MONMEM_SIZE - 24)
#define MONSTACK (MONMEM_BASE + MONSTACK_OFFSET)


/************************************************************************/
/*      other monitor constants                                         */
/************************************************************************/

#define NCHAN		2
#define EMAGIC		0xaa557766	/* magic number for pseudovects */
#define MONTRAP         3               /* TRAP number for monitor ep   */
#define DEMTRAP         4               /* TRAP number for monitor call */
					/*   demultiplexer              */
#define BADDEMUX        0x100           /* message code for trap routine*/
#define BLANK   '\040'                  /* blank character              */
#define QUOTE   '"'
#define COMMAND_SEPARATOR ';'		/* multiple commands delimiter  */
#define COMMAND_SEPARATOR_STRING ";"	/* multiple commands delimiter  */
#define PROMPT  ':'                     /* monitor prompt character     */


/************************************************************************/
/*      structure of monitor data area                                  */
/************************************************************************/

#ifdef LANGUAGE_C
#ifdef VAX
#define mda ((struct mondata *)monitordata) /* init pointer to monitor data */
extern char monitordata[];
#define demon_exit exit
#else
/*#define mda ((struct mondata *)((int)GetSP() & ~(MONMEM_SIZE -1)))*/
#endif
#endif
#define mda ((struct mondata *)0xa0010000)
#define CBUFSIZE        512             /* size of monitor cmd buffer   */
#define MTBLSIZE        4096             /* size of monitors macro table */
#define ERROR_LOG_SIZE  4096		/* size of error log	*/
#define BRKPTS	        8               /* number of breakpoints supptd */



#ifdef LANGUAGE_C
struct breakpt 				/* breakpoint descriptor        */
   {
   unsigned int *addr;		/* address of breakpoint        */
   unsigned int code;			/* real code replaced           */
   int count;                           /* countdown to real break      */
   };
#endif

/*
  WARNING:  SEVERAL ASSEMBLER ROUTINES DEPEND ON THE ORDER OF
  THE FIELDS IN THIS STRUCTURE.  IF THE ARRANGEMENT OF THIS
  STRUCTURE IS CHANGED, THE OFFSET LITERALS MUST BE ADJUSTED ACCORDINGLY.
*/
#define EXCEPTION_HANDLER_SIZE 0x20	/* space for handler code in words*/
#define EXECUTE_AREA_SIZE 0x20		/* space for instruction assembly */
#define NVECTORS 32
#define MAXMEMBOARDS 8
#define PAD_AREA_SIZE	1024 - ( (EXCEPTION_HANDLER_SIZE * 2) + \
				 EXECUTE_AREA_SIZE + \
				 (NVECTORS * 7) + \
				 (MAXMEMBOARDS * 4) + 1 )
#define UTLBMISS_AREA	0
#define EXCEPTION_AREA	(UTLBMISS_AREA + (EXCEPTION_HANDLER_SIZE * 4))
#define EXECUTE_AREA	(EXCEPTION_AREA + (EXCEPTION_HANDLER_SIZE * 4))
#define VECTORS_AREA	(EXECUTE_AREA + (EXECUTE_AREA_SIZE * 4))
#define PAD_AREA	(VECTORS_AREA + (NVECTORS * 24))
/* #define R		4096	*/
/*#define R		8192*/
#define R		0x0
#define R0		R + 0x0
#define R1		R + 0x4
#define R2		R + 0x8
#define R3		R + 0xc
#define R4		R + 0x10
#define R5		R + 0x14
#define R6		R + 0x18
#define R7		R + 0x1c
#define R8		R + 0x20
#define R9		R + 0x24
#define R10		R + 0x28
#define R11		R + 0x2c
#define R12		R + 0x30
#define R13		R + 0x34
#define R14		R + 0x38
#define R15		R + 0x3c
#define R16		R + 0x40
#define R17		R + 0x44
#define R18		R + 0x48
#define R19		R + 0x4c
#define R20		R + 0x50
#define R21		R + 0x54
#define R22		R + 0x58
#define R23		R + 0x5c
#define R24		R + 0x60
#define R25		R + 0x64
#define R26		R + 0x68
#define R27		R + 0x6c
#define R28		R + 0x70
#define GP		R28
#define R29		R + 0x74
#define SP		R29
#define R30		R + 0x78
#define FP		R30
#define R31		R + 0x7c
#define HI		R + 0x80
#define LO		R + 0x84
#define PC		R + 0x88
#define SR		R + 0x8c
#define EPC		R + 0x90
#define BADVADDR	R + 0x94
#define CAUSE		R + 0x98
#define TLBHI		R + 0x9c
#define TLBLO		R + 0xa0
#define CONTEXT		R + 0xa4
#define INDEX		R + 0xa8
#define RANDOM		R + 0xac
#define FPREGS		R + 0xb0
#define FP0		FPREGS
#define FP1		R + 0xb4
#define FP2		R + 0xb8
#define FP3		R + 0xbc
#define FP4		R + 0xc0
#define FP5		R + 0xc4
#define FP6		R + 0xc8
#define FP7		R + 0xcc
#define FP8		R + 0xd0
#define FP9		R + 0xd4
#define FP10		R + 0xd8
#define FP11		R + 0xdc
#define FP12		R + 0xe0
#define FP13		R + 0xe4
#define FP14		R + 0xe8
#define FP15		R + 0xec
#define FP16		R + 0xf0
#define FP17		R + 0xf4
#define FP18		R + 0xf8
#define FP19		R + 0xfc
#define FP20		R + 0x100
#define FP21		R + 0x104
#define FP22		R + 0x108
#define FP23		R + 0x10c
#define FP24		R + 0x110
#define FP25		R + 0x114
#define FP26		R + 0x118
#define FP27		R + 0x11c
#define FP28		R + 0x120
#define FP29		R + 0x124
#define FP30		R + 0x128
#define FP31		R + 0x12c
#define FPR		R + 0x130
#define FPE		R + 0x134
#define FPCS		R + 0x138
#define LAST_FPREG	R + 0x13c
#define RAMSTART 	R + 0x13c
#define RAMEND  	R + 0x140
#define RAMSIZE 	R + 0x144
#define ICACHESIZE 	R + 0x148
#define DCACHESIZE 	R + 0x14c

#ifdef LANGUAGE_C
typedef struct {
	int (*handler)();
	int (*clearer)();
	int info[5];
} exception_vector;
typedef struct {
	unsigned int ioaddress; 	/* io address segment (1-63) */
	unsigned int memoffset;		/* mem start address (4Meg incremnts)*/
	unsigned int interleaved;	/* flag to indicate whether intlv'ed */
	unsigned int mem_vector;	/* unique interrupt vector */
} mem_config_info;

struct mondata                          /* resides in RAM               */ {
   unsigned long utlmiss_area[ EXCEPTION_HANDLER_SIZE ];
   unsigned long exception_area[ EXCEPTION_HANDLER_SIZE ];

   char *error_ptr;		/* pointer to current position in log */
   char error_log[ ERROR_LOG_SIZE ];

   unsigned long execute_area[ EXECUTE_AREA_SIZE ];
   exception_vector vectors[ NVECTORS ];
   mem_config_info meminfo[ MAXMEMBOARDS ];
   unsigned long pad[ PAD_AREA_SIZE ];

   /*
     REGISTER VALUES AT ENTRY TO MONITOR

     The monitor routine for displaying/modifying registers
     depends on the order of the fields rd0 - ra7
   */

   unsigned long r[32];

   unsigned long lo;
   unsigned long high;

   unsigned long pc;
   unsigned long sr;
   unsigned long epc;
   unsigned long badvaddr;
   unsigned long cause;
   unsigned long tlblo;
   unsigned long tlbhi;
   unsigned long context;
   unsigned long index;
   unsigned long random;
/*
   unsigned long fp[32];
   unsigned long fpr;
   unsigned long fpe;
   unsigned long fpcs;
*/

   unsigned long ramstart;
   unsigned long ramend;
   unsigned long ramsize;

   unsigned long icachesize;
   unsigned long dcachesize;

   /* END OF OFFSET DEPENDENT FIELDS */
   

   /* INTERRUPT AND EXCEPTION CONTROL */

   unsigned long mask;
   unsigned long enable;
   unsigned long pending;
#endif

#define	MASK_CODE(x)	(1<<(x))

#define EXTINT5_SHIFT	31
#define EXTINT5_ENBL	MASK_CODE(EXTINT5_SHIFT)
#define WBUSSERR_ENBL	EXTINT5_ENBL	

#define EXTINT4_SHIFT	30
#define EXTINT4_ENBL	MASK_CODE(EXTINT4_SHIFT)
#define TIM1_ENBL	EXTINT4_ENBL	

#define EXTINT3_SHIFT	29
#define EXTINT3_ENBL	MASK_CODE(EXTINT3_SHIFT)
#define COP_ENBL	EXTINT3_ENBL	

#define EXTINT2_SHIFT	28
#define EXTINT2_ENBL	MASK_CODE(EXTINT2_SHIFT)
#define TIM0_ENBL	EXTINT2_ENBL	

#define EXTINT1_SHIFT	27
#define EXTINT1_ENBL	MASK_CODE(EXTINT1_SHIFT)
#define UARTS_ENBL	EXTINT1_ENBL	

#define VME7_SHIFT	26
#define VME7_ENBL	MASK_CODE(VME7_SHIFT)
#define VME6_SHIFT	25
#define VME6_ENBL	MASK_CODE(VME6_SHIFT)
#define VME5_SHIFT	24
#define VME5_ENBL	MASK_CODE(VME5_SHIFT)
#define VME4_SHIFT	23
#define VME4_ENBL	MASK_CODE(VME4_SHIFT)
#define VME3_SHIFT	22
#define VME3_ENBL	MASK_CODE(VME3_SHIFT)
#define VME2_SHIFT	21
#define VME2_ENBL	MASK_CODE(VME2_SHIFT)
#define VME1_SHIFT	20
#define VME1_ENBL	MASK_CODE(VME1_SHIFT)

#define SWINT1_SHIFT	19
#define SWINT1_ENBL	MASK_CODE(SWINT1_SHIFT)
#define SWINT0_SHIFT	18
#define SWINT0_ENBL	MASK_CODE(SWINT0_SHIFT)

#undef RMISS_EXC
#define	RMISS_EXC	2	/* Read TLB Miss */
#undef OV_EXC
#define OV_EXC 12
#define NO_SHIFT	0
#define DUTLB_SHIFT	((OV_EXC + 1) - RMISS_EXC)
#define MOD_UTLB_SHIFT   OV_EXC
#define RMISS_UTLB_SHIFT (MOD_UTLB_SHIFT + 1)
#define WMISS_UTLB_SHIFT (RMISS_UTLB_SHIFT + 1)
#define	EXT_INT_ENBL	MASK_CODE(INT_EXC)	/* interrupt */
#define	TLB_MOD_ENBL	MASK_CODE(MOD_UTLB_SHIFT)	/* TLB mod */
#define	TLB_RMISS_ENBL	MASK_CODE(RMISS_UTLB_SHIFT)	/* Read TLB Miss */
#define	TLB_WMISS_ENBL	MASK_CODE(WMISS_UTLB_SHIFT)	/* Write TLB Miss */
#define	RADE_ENBL	MASK_CODE(RADE_EXC)	/* Read Address Error */
#define	WADE_ENBL	MASK_CODE(WADE_EXC)	/* Write Address Error */
#define	IBE_EXC		6	/* Instruction fetch Bus Error */
#define	IBE_ENBL	MASK_CODE(IBE_EXC)	/* Instruction Bus Error */
#define	DBE_EXC		7	/* Data Bus Error */
#define	DBE_ENBL	MASK_CODE(DBE_EXC)	/* Data Bus Error */
#define	SYSCALL_ENBL	MASK_CODE(SYSCALL_EXC)	/* SYSCALL */
#define	BREAK_ENBL	MASK_CODE(BREAK_EXC)	/* BREAKpoint */
#define	II_ENBL		MASK_CODE(II_EXC)	/* Illegal Instruction */
#define	CPU_ENBL	MASK_CODE(CPU_EXC)	/* CoProcessor Unusable */
#define	OV_ENBL		MASK_CODE(OV_EXC)	/* OVerflow */
#define NONVME_EXTERNALS (EXTINT5_ENBL | EXTINT4_ENBL | EXTINT3_ENBL | \
			  EXTINT2_ENBL | EXTINT1_ENBL)
#define VME_INTS	(VME1_ENBL | VME2_ENBL | VME3_ENBL | VME4_ENBL |\
			 VME5_ENBL | VME6_ENBL | VME7_ENBL)
#define SW_INTS		(SWINT1_ENBL | SWINT0_ENBL)
#define EXTERNAL_INTERRUPTS (NONVME_EXTERNALS | VME_INTS | SW_INTS)
   
   /* BREAKPOINT ARRAY */
#ifdef LANGUAGE_C
   struct breakpt bp[BRKPTS];

   /* STATE INFO */
   unsigned int state;
#endif

#define DEBUG		0x80000000
#define DEBUG_SHIFT	31
#define ERROR		0x60000000
#define ERROR_SHIFT	29
#define LOOPONERROR	(1 << ERROR_SHIFT)
#define HALTONERROR	(2 << ERROR_SHIFT)
#define SSTEP		0x10000000
#define SSTEP_SHIFT	28
#define LAST_ADDR	0x08000000
#define LAST_ADDR_SHIFT	27
#define CACHED		0x04000000
#define CACHED_SHIFT	26
#define PASS_PRINT	0x02000000
#define PASS_PRINT_SHIFT 25
#define ERROR_LOGGING	0x01000000
#define ERROR_LOGGING_SHIFT 24
#define ERROR_WRAP	0x00800000
#define ERROR_WRAP_SHIFT 23
#define QUIET		0x00400000
#define QUIET_SHIFT 	22
#define ERROR_ABORT	0x00200000
#define ERROR_ABORT_SHIFT 	21

#ifdef LANGUAGE_C
   /* FORMATTING CONTROL */

   char inbase;
   char outbase;
   char *next_addr;
   int last_length;

   /* BREAK PROCESSING VECTOR */

   /*
   jmp_buf break_restart;
   */
   int break_restart[ 15 ];

   /* ITERATION CONTROL */

#define LOOPSTACK_DEPTH 16
#define LOOPSTACKSIZE LOOPSTACK_DEPTH * 4
   long loop_stack[ LOOPSTACKSIZE ];
   int loop_sp;

   /* STDIO CONTROL

   struct ccbuf ccb[ NCHAN ];
*/

   /* COMMAND BUFFER */

   char prompt[ 32 ];		/* Prompt string saved here */
   short cmdlen;		/* current length of cmd contents */
   char cmd[CBUFSIZE];

   /* MACRO BUFFER */

#define MACRODEPTH 8
   int msp /* = 0 */;	
   char *mstack[ MACRODEPTH ];
   short macrolen;		/* current length of macro table*/
   char macro_tbl[ MTBLSIZE ];

   /* ERROR LOG */

   int correctable_err;		/* current number of correctable errors */
   int cum_correctable_err;	/* # of correctable errors since reset */
   int uncorrectable_err;	/* current number of uncorrectable errors */
   int cum_uncorrectable_err;	/* # of uncorrectable errors since reset */

   int errors;			/* diagnostic errors */
   int last_errors;		


   unsigned int handler_code;	/* used to selectively hush exception prints */


   /* END OF STRUCTURE MARKER -- MUST BE LAST DECLARATION */

   int last;			/* filled with badbeef if ok */
};

extern void MonitorDataInit();
#endif
