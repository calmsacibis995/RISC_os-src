#ident "$Header: pon_ecc_6000.c,v 1.5.1.1 90/07/18 14:30:53 huang Exp $"
/* $Copyright$ */

/*------------------------------------------------------------------------+
| File name : pon_ecc_6000.c                                              |
|                                                                         |
| NOTE:  this file *must* be compiled with at least -O1 optimization to   |
|        avoid unwanted physical memory reads and writes when the Memory  |
|        MemCtl register(s) specify that software is supplying the ECC    |
|        bits                                                             |
|                                                                         |
+------------------------------------------------------------------------*/
#include "sys/types.h"
#include "machine/bc.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/param.h"
#include "saio/setjmp.h"
#include "pon_ecc_6000.h"
#include "pon.h"


/*------------------------------------------------------------------------+
| MISC.                                                                   |
+------------------------------------------------------------------------*/
#define	BANK_INC	4
#define	NUM_OF_BANKS	8
#define	LAST_BANK	0x1C
#define	BANK_SIZE	0x20

#define	BLOCK_SIZE	0x80
#define LINE_SIZE	0x80
#define	CACHE_SIZE	0x040000		/* use for hashing funct */

#define	ECC_MASK	0x7F

#define	TEST_PAT	0x55555555

#define	SBC_MEMERR_MASK	0xF			/* least-signif 4 bits   */


/*------------------------------------------------------------------------+
| external variables.                                                     |
+------------------------------------------------------------------------*/
extern  char  skipped[],
              success[],
              failure[];


/*------------------------------------------------------------------------+
| Local, global variables.                                                |
+------------------------------------------------------------------------*/
u_int	cpu_slot;			/* current CPU slot number       */

extern int *nofault;			/* indicate handling exceptions  */
jmp_buf fault_buf;			/* long jump buffer for fault    */


/*------------------------------------------------------------------------+
| Define tests tables:                                                    |
+------------------------------------------------------------------------*/
typedef struct test_table {
    int   (*test_rtn)();
    char  *test_desc;
}   TestTable;

static
int   ecc_dpath(),  ecc_gen(),   ecc_syn(),   ecc_wedet(),  ecc_bedet(),
      ecc_wecor(),  ecc_becor(), ecc_wintr(), ecc_bintr(),  ecc_wacmp(),
      ecc_bacmp();

TestTable  ttable[] = {
  /*  Test routine      Test description                  */
  /*  ------------      --------------------------------- */
      ecc_dpath,	"    Check ECC data path...........................",
      ecc_gen,		"    Check ECC generation..........................",
      ecc_syn,          "    Check syndrome bits generation................",
      ecc_wedet,        "    Check error detection in word mode............",
      ecc_bedet,        "    Check error detection in block mode...........",
      ecc_wecor,	"    Check error correction in word mode...........",
      ecc_becor,	"    Check error correction in block mode..........",
      ecc_wintr,	"    Check ECC interrupt in work mode..............",
      ecc_bintr,	"    Check ECC interrupt in block mode.............",
      ecc_wacmp,	"    Check ECC interrupt w/ word address compare...",
      ecc_bacmp,	"    Check ECC interrupt w/ block address compare..",
      0,                 0,
};



/*------------------------------------------------------------------------+
| Routine name: Pon_Ecc		                                          |
| Description :                                                           |
+------------------------------------------------------------------------*/
Pon_Ecc()
{
    u_int    eflag,    save_val;
    u_int    sr,       mem_size,
             brd_size, ctl_space;
    register u_int     base_addr,
                       work_addr;
    register TestTable *ttp;
    register MemCtl    *ctl;		/* pointer to memory control reg */
    register MemEcc    *ecc;		/* pointer to memory ECC reg     */


    /*
     * Set LED's and display test message, then check for any previous
     * cache of memory error before starting ecc testing.
     */
    pon_set_leds(PON_ECC);
    pon_puts("ECC Test...");

    if (GetDepend() & (PON_FAULT_CACHE | PON_FAULT_MEM)){
        pon_puts(skipped);
        goto norun;
    }

    /*
     * Disable interrupts, turn off SR_BEV for _hook_exceptions
     */
    sr = GetSR();
    SetSR( sr & ~(SR_IMASK | SR_BEV) );
    *(u_int *)CSR_IVECTMASK = 0;

    /*
     * get CPU slot number in its own CtlMisc register.
     */
    cpu_slot = (((u_int)(*(u_int*)(CSR_CTLMISC)) >> 24) & 0x0F);

    pon_puts("\n\r");

    eflag = PASS;
    base_addr = 0;
    while (TRUE) {

        /*
         * get control space address and memory board size located
         * at "base_addr".
         */
	if (((ctl_space = find_ctlspace_r6000(base_addr/(1024*1024))) == 0) ||
	    ((brd_size  = get_membrd_size(ctl_space)) == 0)){
            break;
        }

        /*
         * for each memory board installed in the system, perform basic
         * ECC testing on it.
         */
        brd_info(base_addr, brd_size, ctl_space);

        /*
         * set up Bank mask for 8-way interleave memory.
         */
        ctl       = (MemCtl *)(ctl_space | SBC_MEMCTL);
        ecc       = (MemEcc *)(ctl_space | SBC_MEMECC);
        save_val   = ctl->wd ;
        ecc->f.bmask = 1;

        for (ttp = ttable; ttp->test_desc != 0; ttp++){
            /*
             * display test message and run it.
             */
            work_addr = PHYS_TO_K1((base_addr+(brd_size/2)) & 0xffff0000);
            pon_puts(ttp->test_desc);
            if (((ttp->test_rtn)(work_addr, ctl, ecc)) == PASS){
                pon_puts(success);
            }else{
                pon_puts(failure);
                eflag |= FAIL;
            }
        } /* while:  for each memory board */

        ctl->wd = save_val;

        /*
         * Go on to next memory board.
         */
        base_addr += brd_size;
    }

    /*
     * check for any error in the memory board.
     */
    SetSR(sr);
    if (!eflag)
        return(PASS);

    /*
     * incase of error detected, display failure message, flash the LED's,
     * set depend flag to indicate ECC error, and return error flag to
     * the caller.
     */

    FastFlash(PON_ECC);
    pon_set_leds(PON_ECC);

norun:
    SetDepend(PON_FAULT_ECC);
    return(FAIL);
}



/*------------------------------------------------------------------------+
| Routine name: brd_info                                                  |
| Description : display current board's informations.                     |
+------------------------------------------------------------------------*/
static brd_info( base_addr, brd_size, ctl_space)
    u_int base_addr, brd_size, ctl_space;
{
    pon_puts("  Memory board in slot # : 0x");
    pon_putc("0123456789ABCDEF"[(ctl_space >> 16) & 0x0F]);
    pon_puts("\n\r");
}



/*------------------------------------------------------------------------+
| Routine : ecc_dpath                                                     |
| Description: This routine checks the data path from memory ECC register |
|    to main memory and back to ECC register.  This is done by writing    |
|    bit exp_vals into ECC write register, forcing a memory write with    |
|    the check bit register select bit of the mem control reg set to      |
|    select ECC code from ECC write register rather than the generated    |
|    one.  Then, the same memory location is read back and the content of |
|    the saved ECC value from last memory access is checked against the   |
|    original test exp_vals.  This verifies the ECC data path  before     |
|    further testing can be done.                                         |
+------------------------------------------------------------------------*/
static ecc_dpath(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register u_int exp_val,		/* expected value                */
                   act_val;		/* actual   value                */


    /*
     * Write zeroes and ones into ECC array.
     */
    ctl->wd = 0;
    exp_val = 0;
    do  {
        write_ecc(work_addr, 0, exp_val, ecc, ctl);

        ecc->f.save = 0;
        act_val = *work_addr;
        act_val = ecc->f.save;
        if (act_val != exp_val)
            return(FAIL);

        exp_val   = (~exp_val) & EccWriteReg;
    } while (exp_val == EccWriteReg);


    /*
     * Write Fives and Ables.
     */
    exp_val = 0x55;
    do  {
        write_ecc(work_addr, 0, exp_val, ecc, ctl);

        ecc->f.save = 0;
        act_val = *work_addr;
        act_val = ecc->f.save;
        if (act_val != exp_val)
            return(FAIL);

        exp_val   = (~exp_val) & EccWriteReg;
    } while (exp_val == 0x2A);


    /*
     * walk one and zero
     */
    for (exp_val = 1; exp_val < 0x80; exp_val <<= 1){

        write_ecc(work_addr, 0, exp_val, ecc, ctl);
        ecc->f.save = 0;
        act_val = *work_addr;
        act_val = ecc->f.save;
        if (act_val != exp_val)
            return(FAIL);

        write_ecc(work_addr, 0, (~exp_val & 0x7f), ecc, ctl);
        ecc->f.save = 0;
        act_val = *work_addr;
        act_val = ecc->f.save;
        if (act_val != (~exp_val & 0x7f))
            return(FAIL);
    }

    /*
     * re-initialize memory with using generated ecc
     */
    ctl->wd    = 0;
    *work_addr = 0;
    return(PASS);
}



/*------------------------------------------------------------------------+
| Routine name: ecc_gen                                                   |
| Description : Checks ECC code generation logic.                         |
+------------------------------------------------------------------------*/
static ecc_gen(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register EccTbl  *etbl;
    register u_int exp_val,		/* expected value                */
                   act_val;		/* actual   value                */


    /*
     * go through the ecc test pattern table and execute every
     * single pattern until DONE...
     */
    ctl->wd   = 0;
    for (etbl = &ecc_tbl[0]; etbl->idata != THE_END; etbl++){
        /*
         * write data pattern into memory word.
         */
        exp_val    = etbl->idata;
        *work_addr = exp_val;

        /*
         * clear the ECC register, read the data word back, and check
         * generated ECC against the expected value in our test table.
         */
        ecc->f.save  = 0;
        exp_val      = etbl->ecc;
        act_val      = *work_addr;
        act_val      = ecc->f.save;
        if (act_val != exp_val)
            return(FAIL);
    }

    return(PASS);
}



/*------------------------------------------------------------------------+
| Routine name: ecc_syn                                                   |
| Description : Verifies                                                  |
+------------------------------------------------------------------------*/
static ecc_syn(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register SynTbl  *stbl;
    register u_int exp_val,		/* expected value                */
                   act_val;		/* actual   value                */


    /*
     * go through the syndrome test pattern table and execute every
     * single pattern until DONE...
     */
    for (stbl = &syn_tbl[0]; stbl->idata != THE_END; stbl++){
        /*
         * write data pattern into memory with ECC coming from
         * ECC write register.
         */
        write_ecc(work_addr, stbl->idata, stbl->ecc, ecc, ctl);

        /*
         * Read back data word, check the syndrome bits in memory
         * ECC register against the expected value in our test table.
         * NOTE: it is important to read the data at work_addr into
         * act_val so that the compiler will generate code that reads
         * the data first, then go after the syndrome bit.
         */
        ecc->f.synd = 0;
        act_val     = *work_addr;

        exp_val     = (u_int)(stbl->synd);
        act_val     = (u_int)(ecc->f.synd);
        if (act_val != exp_val)
            return(FAIL);
    }

    /*
     * re-initialize memory, using generated ecc.
     */
    ctl->wd    = 0;
    *work_addr = 0;
    return(PASS);
}



/*------------------------------------------------------------------------+
| Routine : ecc_wedet                                                     |
| Description:  Verify the error detection logic in word read operations. |
|     Single and multi-bit  errors are forced  into memory with different |
|     combinations of bad bits in data and ecc field. Once the bad memory |
|     location is read, the single and multi-bit error flags and the bank |
|     address is set to the appropriate value.                            |
+------------------------------------------------------------------------*/
static ecc_wedet(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register u_int  *tmp_addr,
                    bank_num,		/* current bank number           */
                    data_pat,		/* good data pattern             */
                    ecc_pat,		/* good ECC pattern              */
                    dbit, dbit1,
                    ebit, ebit1;
    register u_int  exp_val,		/* expected value                */
                    act_val;		/* actual   value                */


    /*
     * Check error detection logic.
     */
    work_addr = (u_int*)((u_int)work_addr & 0xffffff80);
    data_pat  = 0;
    do {
        ctl->wd    = 0;
        *work_addr = data_pat;
        ecc_pat    = *work_addr;
        ecc_pat    = ecc->f.save;
       
        /*
         * force single bit error and check error flag
         */
        exp_val = SingleBitErr;
        for (dbit = 1, ebit = 0; dbit || ebit; ){

            write_ecc(work_addr, data_pat^dbit, ecc_pat ^ebit, ecc, ctl);
            act_val = *work_addr;
            act_val = (u_int)(ctl->wd ) & (SingleBitErr | MultiBitErr);
            if (act_val != exp_val)
                return(FAIL);

            if (dbit != 0x80000000){
                dbit  =  dbit << 1;
                ebit  = (ebit << 1) & ECC_MASK;
            }else {
                dbit  = 0;
                ebit  = 1;
            }
        }

        /*
         * force multi bit error and check error flag
         */
        exp_val = MultiBitErr;
        for (dbit = 1, ebit = 0; dbit || ebit; ){

            if (dbit != 0x80000000){
                dbit1 =  dbit << 1;
                ebit1 = (ebit << 1) & ECC_MASK;
            }else {
                dbit1 = 0;
                ebit1 = 1;
            }

            while (dbit1 || ebit1) {

                write_ecc(work_addr, data_pat ^ (dbit | dbit1),
                          ecc_pat  ^ (ebit | ebit1), ecc, ctl);

                act_val      = *work_addr;
                act_val      = (u_int)(ctl->wd ) & (SingleBitErr | MultiBitErr);
                if (act_val != exp_val)
                    return(FAIL);

                if (dbit1 != 0x80000000){
                    dbit1  =  dbit1 << 1;
                    ebit1  = (ebit1 << 1) & ECC_MASK;
                }else {
                    dbit1  = 0;
                    ebit1  = 1;
                }
            }

            if (dbit != 0x80000000){
                dbit  =  dbit << 1;
                ebit  = (ebit << 1) & ECC_MASK;
            }else {
                dbit  = 0;
                ebit  = 1;
            }
        }

        data_pat = ~data_pat;

    } while (data_pat);


    /*
     * check bad bank number in ecc register and failure address in the
     * address register when single or multi-bit error occurrs.
     */
    data_pat  = ~data_pat;
    for (dbit = 2; dbit <= 3; dbit++) {

        for (bank_num = 0; bank_num < NUM_OF_BANKS; bank_num++){

            tmp_addr  = (u_int *)((u_int)work_addr | (bank_num << 2));
            exp_val   = ((((u_int)tmp_addr >> 22) & 0x030) |
                         (((u_int)tmp_addr >>  2) & 0x0f));

            write_ecc(tmp_addr, data_pat ^ dbit, ecc_pat, ecc, ctl);
            act_val = *tmp_addr;
            act_val = ecc->f.bank;
            if (act_val != exp_val)
                return(FAIL);

            /*
             * re-initialize memory.
             */
            *tmp_addr = data_pat;
        }
    }

    return(PASS);
}


/*------------------------------------------------------------------------+
| Routine : ecc_bedet                                                     |
| Description:  same as ecc_wedet except addressing to K0 space.          |
+------------------------------------------------------------------------*/
static ecc_bedet(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register u_int  k0_base,		/* k0 address                    */
                    k1_base,		/* base address                  */
                    data_pat,		/* good data pattern             */
                    ecc_pat,		/* good ECC pattern              */
                    bank_num,
                    dbit, dbit1,
                    ebit, ebit1;
    register u_int  exp_val,		/* expected value                */
                    act_val;		/* actual   value                */


    /*
     * get k0 address.
     */
    ctl->wd   = 0;
    k1_base   = (u_int)work_addr & 0xffffff80;
    k0_base   = K1_TO_K0(k1_base);

    /*
     * Check error detection logic.
     */
    data_pat  = 0x55555555;
    ecc->f.bmask= 1;
    do {
        /*
         * initialize memory with test pattern and get generated ECC.
         */
        ctl->wd    = 0;
        for (dbit = 0; dbit < BLOCK_SIZE; dbit+= 4) {
            *(u_int *)(k1_base + CACHE_SIZE*3 + dbit)= data_pat;
            *(u_int *)(k1_base + CACHE_SIZE*2 + dbit)= data_pat;
            *(u_int *)(k1_base + CACHE_SIZE*1 + dbit)= data_pat;
            *(u_int *)(k1_base + CACHE_SIZE*0 + dbit)= data_pat;
        }
        ecc_pat    = *(u_int*)k1_base;
        ecc_pat    = ecc->f.save;
       
        /*
         * force single bit error and check error flag
         */
        exp_val = SingleBitErr;
        for (dbit = 1, ebit = 0; dbit || ebit; ){

            write_ecc(k1_base, data_pat^dbit, ecc_pat^ebit, ecc, ctl);

            /*
             * now do block read of memory line with forced error.
             */
            act_val  = *(u_int*)k0_base;
            act_val  = (u_int)(ctl->wd ) & (SingleBitErr | MultiBitErr);
            if (act_val != exp_val)
                return(FAIL);
            /*
             * flush the cache.
             */
            *(u_int*)k0_base = 0;
            act_val = *(u_int *)(k0_base + CACHE_SIZE*1);
            act_val = *(u_int *)(k0_base + CACHE_SIZE*2);
            act_val = *(u_int *)(k0_base + CACHE_SIZE*3);

            if (dbit != 0x80000000){
                dbit  =  dbit << 1;
                ebit  = (ebit << 1) & ECC_MASK;
            }else {
                dbit  = 0;
                ebit  = 1;
            }
        }

        /*
         * force multi bit error and check error flag
         */
        exp_val = MultiBitErr;
        for (dbit = 1, ebit = 0; dbit || ebit; ){

            if (dbit != 0x80000000){
                dbit1 =  dbit << 1;
                ebit1 = (ebit << 1) & ECC_MASK;
            }else {
                dbit1 = 0;
                ebit1 = 1;
            }

            while (dbit1 || ebit1) {

                write_ecc(k1_base, data_pat ^ (dbit | dbit1),
                          ecc_pat ^ (ebit | ebit1), ecc, ctl);

                act_val = *(u_int*)k0_base;
                act_val = (u_int)(ctl->wd ) & (SingleBitErr | MultiBitErr);
                if (act_val != exp_val)
                    return(FAIL);

                /*
                 * flush the cache.
                 */
                *(u_int*)k0_base = 0;
                act_val = *(u_int *)(k0_base + CACHE_SIZE*1);
                act_val = *(u_int *)(k0_base + CACHE_SIZE*2);
                act_val = *(u_int *)(k0_base + CACHE_SIZE*3);

                if (dbit1 != 0x80000000){
                    dbit1  =  dbit1 << 1;
                    ebit1  = (ebit1 << 1) & ECC_MASK;
                }else {
                    dbit1  = 0;
                    ebit1  = 1;
                }
            }

            if (dbit != 0x80000000){
                dbit  =  dbit << 1;
                ebit  = (ebit << 1) & ECC_MASK;
            }else {
                dbit  = 0;
                ebit  = 1;
            }
        }

        data_pat = ~data_pat;

    } while (data_pat != 0x55555555);


    /*
     * check bad bank number in ecc register and failure address in the
     * address register when single or multi-bit error occurrs.
     */
    data_pat  = ~data_pat;
    for (dbit = 2; dbit <= 3; dbit++) {

        for (bank_num = 0; bank_num < NUM_OF_BANKS; bank_num++){

            work_addr = (u_int *)(k1_base | (bank_num << 2));
            exp_val   = ((((u_int)work_addr >> 22) & 0x030) |
                          (((u_int)work_addr >>  2) & 0x0f));

            write_ecc(work_addr, data_pat ^ dbit, ecc_pat, ecc, ctl);

            act_val = *(u_int *)(k0_base + (bank_num << 2));
            act_val = ecc->f.bank;
            if (act_val != exp_val)
                return(FAIL);

           /*
            * clean up memory, flush cache.
            */
           *work_addr = data_pat;
           act_val = *(u_int *)(k0_base + (bank_num << 2) + CACHE_SIZE*1);
           act_val = *(u_int *)(k0_base + (bank_num << 2) + CACHE_SIZE*2);
           act_val = *(u_int *)(k0_base + (bank_num << 2) + CACHE_SIZE*3);
        }
    }

    return(PASS);
}



/*------------------------------------------------------------------------+
| Routine name: ecc_wecor                                                 |
| Description :                                                           |
+------------------------------------------------------------------------*/
static ecc_wecor(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register u_int ecc_pat,		/* generation ecc for exp_val    */
                   dbit,		/* bad data bit                  */
                   ebit;		/* bad ecc bit                   */
    register u_int exp_val,		/* expected value                */
                   act_val;		/* actual   value                */

    /*
     * for single bit error in both data and ecc field. Read the data
     * back and verify that the failing bit is corrected.
     */
    exp_val = TEST_PAT;
    do {

        /*
         * generate corresponding ecc for a given data pattern.
         */
        ctl->wd    = 0;
        *work_addr = exp_val;
        ecc_pat    = *work_addr;
        ecc_pat    = ecc->f.save;

	/*
         * for each bit position in data and ecc field, force
         * an error to it. Read the data back and verify that
         * it is corrected.
         */
        for (dbit = 1, ebit = 0; (dbit | ebit) != 0; ) {

            write_ecc(work_addr, exp_val ^ dbit, ecc_pat ^ ebit, ecc, ctl);

            ctl->wd    = EnaSErrCorrect;	/* enable error correction    */
            act_val    = ~ctl->wd;
            act_val    = *work_addr;

            if (act_val != exp_val)
                return(FAIL);

            *work_addr = exp_val;
            if (dbit != 0x80000000) {
                dbit  = (dbit << 1);
                ebit  = (ebit << 1) & ECC_MASK;
            } else {
                dbit  = 0;
                ebit  = 1;
            }
        }

        /*
         * try it again with inverted test pattern
         */
        exp_val = ~exp_val;

    } while (exp_val != TEST_PAT);

    return(PASS);
}


/*------------------------------------------------------------------------+
| Routine name: ecc_becor                                                 |
| Description :                                                           |
+------------------------------------------------------------------------*/
static ecc_becor(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register u_int  k0_base,		/* k0 address                    */
                    k1_base,		/* base address                  */
                    ecc_pat,		/* generation ecc for exp_val    */
                    dbit,		/* bad data bit                  */
                    ebit;		/* bad ecc bit                   */
    register u_int  exp_val,		/* expected value                */
                    act_val;		/* actual   value                */


    k1_base = (u_int)work_addr & 0xffffff80;
    k0_base = k1_base & 0x9fffffff;

    /*
     * for single bit error in both data and ecc field. Read the data
     * back and verify that the failing bit is corrected.
     */
    exp_val = TEST_PAT;
    do {

        /*
         * initialize memory with test pattern and get generated ECC.
         */
        ctl->wd    = 0;
        for (dbit = 0; dbit < BLOCK_SIZE; dbit+= 4) {
            *(u_int *)(k1_base + CACHE_SIZE*3 + dbit)= exp_val;
            *(u_int *)(k1_base + CACHE_SIZE*2 + dbit)= exp_val;
            *(u_int *)(k1_base + CACHE_SIZE*1 + dbit)= exp_val;
            *(u_int *)(k1_base + CACHE_SIZE*0 + dbit)= exp_val;
        }
        ecc_pat    = *(u_int*)k1_base;
        ecc_pat    = ecc->f.save;

	/*
         * for each bit position in data and ecc field, force
         * an error to it. Read the data back and verify that
         * it is corrected.
         */
        for (dbit = 1, ebit = 0; (dbit | ebit) != 0; ) {

            write_ecc(k1_base, exp_val ^ dbit, ecc_pat ^ ebit, ecc, ctl);

            ctl->wd    = EnaSErrCorrect;	/* enable error correction    */
            act_val    = ~ctl->wd;
            act_val    = *(u_int*)k0_base;

            if (act_val != exp_val)
                return(FAIL);

            /*
             * clean up memory, flush cache.
             */
            *(u_int *)(k1_base) = exp_val;
            act_val = *(u_int *)(k0_base + CACHE_SIZE*1);
            act_val = *(u_int *)(k0_base + CACHE_SIZE*2);
            act_val = *(u_int *)(k0_base + CACHE_SIZE*3);

            if (dbit != 0x80000000) {
                dbit  = (dbit << 1);
                ebit  = (ebit << 1) & ECC_MASK;
            } else {
                dbit  = 0;
                ebit  = 1;
            }
        }

        /*
         * try it again with inverted test pattern
         */
        exp_val = ~exp_val;

    } while (exp_val != TEST_PAT);

    return(PASS);
}


/*------------------------------------------------------------------------+
| Routine name: ecc_wintr                                                 |
| Description :                                                           |
+------------------------------------------------------------------------*/
static ecc_wintr(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register u_int ecc_pat,		/* good ECC pattern              */
                   data_pat,
                   int_bit,
		   _cause;
    register u_int  act_val;		/* actual   value                */


    /*
     * get generated ecc for the test pattern.
     */
    ctl->wd    = 0;
    data_pat   = TEST_PAT;
    *work_addr = data_pat;
    ecc_pat    = *work_addr;
    ecc_pat    = ecc->f.save;

    /*
     * disable cpu External Interrupts for these sbc vector bits
     */
    *(u_int *)CSR_COMPARE   = *(u_int *)CSR_COUNT - 1; /* avoid clock ints */
    *(u_int *)CSR_IVECTMASK = 0;		/* disable cpu external ints */
    *(u_int *)CSR_IVECTCLR  = 0xffffffff;	/* clear cpu int vector bits */


    /*
     * Check single-bit error interrupt.  This is done by forcing a single-
     * bit error in memory with different patterns of interrupt bit set.
     * After accessing the memory location, it checks the cpu's sbc interrupt
     * vector bit, then clears it.  Because we have disabled cpu External
     * Interrupts, we don't actually get a cpu interrupt.
     */
    for (int_bit = 0; int_bit < 4; int_bit++){

        /*
         * force bad bit in data word.
         */
        write_ecc(work_addr, data_pat ^ 1, ecc_pat, ecc, ctl);

        /*
         * set interrupt slot and enable memory interrupt.
         */
        ctl->wd     = (MemIntEna | EnaSErrInt | EnaMErrDet | EnaSErrCorrect);
        ctl->f.islt = cpu_slot;
        ctl->f.ibit = int_bit;

        /*
         * read bad data word in memory and verify that the memory 
         * BusChip write to CPU's interrupt vector register.
         */
        act_val  = *work_addr;
        DELAY(1024);

        /*
         * clear all pending interrupt.
         */
        act_val  = *(u_int *)CSR_IVECTSET;
        *(u_int *)CSR_IVECTCLR = 0xffffffff;
        if ((act_val & SBC_MEMERR_MASK) != (1 << int_bit))
            return(FAIL);
    }


    /*
     * Now, do multi-bit error interrupt. This time, we should get bus
     * parity error interrupt and the exception code in cause register
     * set to DBE  (bus error exception for a data load or store).
     *
     */
    write_ecc(work_addr, data_pat ^ 3, ecc_pat, ecc, ctl);

    /*
     * set interrupt slot and enable memory interrupt.
     */
    ctl->wd     = (MemIntEna | EnaSErrInt | EnaMErrDet | EnaSErrCorrect);
    ctl->f.islt = cpu_slot;

    /*
     * set up to capture exceptions, make the memory reference which should 
     * cause a multi-bit error and generate a bus error exception, and verify.
     */
    if (!setjmp(fault_buf)) {
	nofault = &fault_buf[0];	/* indicate handling own exceptions */
	act_val = *work_addr;		/* cause the multi-bit error */
	DELAY(1024);

	/*
	 *  shouldn't get here!!
	 */
	nofault = 0;
	return(FAIL);
    }

    /*
     * Interrupt return here... now check error code in
     * the cause register.
     */
    nofault = 0;
    if ( (GetCause() & CAUSE_EXCMASK) != EXC_DBE )
        return(FAIL);

    /*
     * re-initialize memory and the cpu's sbc
     */
    ctl->wd    = 0;
    *work_addr = 0;
    *(u_int *)CSR_IVECTCLR  = 0xffffffff;
    return(PASS);
}


/*------------------------------------------------------------------------+
| Routine name: ecc_bintr                                                 |
| Description :                                                           |
+------------------------------------------------------------------------*/
static ecc_bintr(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register u_int k0_base,		/* k0 address                    */
                   k1_base,		/* k1 address                    */
                   ecc_pat,		/* generated ecc pattern         */
                   data_pat,		/* test data pattern.            */
                   int_bit,		/* cpu interrupt vector bit.     */
		   _cause;		/* value in cause register.      */
    register u_int act_val;		/* actual   value                */


    k1_base = (u_int)work_addr & 0xffffff80;
    k0_base = K1_TO_K0(k1_base);
 
    /*
     * initialize memory with test pattern and get generated ECC.
     */
    ctl->wd    = 0;
    for (int_bit = 0, data_pat = TEST_PAT; int_bit < BLOCK_SIZE; int_bit+= 4) {
        *(u_int *)(k1_base + CACHE_SIZE*3 + int_bit)= data_pat;
        *(u_int *)(k1_base + CACHE_SIZE*2 + int_bit)= data_pat;
        *(u_int *)(k1_base + CACHE_SIZE*1 + int_bit)= data_pat;
        *(u_int *)(k1_base + CACHE_SIZE*0 + int_bit)= data_pat;
    }
    ecc_pat    = *(u_int*)k1_base;
    ecc_pat    = ecc->f.save;

    /*
     * disable cpu External Interrupts for these sbc vector bits
     */
    *(u_int *)CSR_COMPARE   = *(u_int *)CSR_COUNT - 1; /* avoid clock ints */
    *(u_int *)CSR_IVECTMASK = 0;		/* disable cpu external ints */
    *(u_int *)CSR_IVECTCLR  = 0xffffffff;	/* clear cpu int vector bits */

    /*
     * Check single-bit error interrupt.  This is done by forcing a single-
     * bit error in memory with different patterns of interrupt bit set.
     * After accessing the memory location, it checks the cpu's sbc interrupt
     * vector bit, then clears it.  Because we have disabled cpu External
     * Interrupts, we don't actually get a cpu interrupt.
     */
    for (int_bit = 0; int_bit < 4; int_bit++){

        /*
         * force bad bit in data word.
         */
        write_ecc(k1_base, data_pat ^ 1, ecc_pat, ecc, ctl);

        /*
         * set interrupt slot and enable memory interrupt.
         */
        ctl->wd     = (MemIntEna | EnaSErrInt | EnaMErrDet | EnaSErrCorrect);
        ctl->f.islt = cpu_slot;
        ctl->f.ibit = int_bit;

        /*
         * read bad data word in memory and verify that the memory 
         * BusChip write to CPU's interrupt vector register.
         */
        act_val  = *(u_int*)k0_base;
        DELAY(1024);

        /*
         * read cpu's interrupt vector register in the system bus chip
         * and clear all pending interrupt.
         */
        act_val  = *(u_int *)CSR_IVECTSET;
        *(u_int *)CSR_IVECTCLR = 0xffffffff;
        if ((act_val & SBC_MEMERR_MASK) != (1 << int_bit))
            return(FAIL);

        /*
         * re-initialize memory and flush caches.
         */
        *(u_int*)k1_base = data_pat;
        act_val = *(u_int *)(k0_base + CACHE_SIZE*1);
        act_val = *(u_int *)(k0_base + CACHE_SIZE*2);
        act_val = *(u_int *)(k0_base + CACHE_SIZE*3);
    }


    /*
     * Now, do multi-bit error interrupt. This time, we should get bus
     * parity error interrupt and the exception code in cause register
     * set to DBE  (bus error exception for a data load or store).
     *
     */
    write_ecc(k1_base, data_pat ^ 3, ecc_pat, ecc, ctl);

    /*
     * set interrupt slot and enable memory interrupt.
     */
    ctl->wd     = (MemIntEna | EnaSErrInt | EnaMErrDet | EnaSErrCorrect);
    ctl->f.islt = cpu_slot;

    /*
     * set up to capture exceptions, make the memory reference which should 
     * cause a multi-bit error and generate a bus error exception, and verify.
     */
    if (!setjmp(fault_buf)) {
	nofault = &fault_buf[0];	/* indicate handling own exceptions */
	act_val = *(u_int*)k0_base;	/* cause the multi-bit error */
	DELAY(1024);

	/*
	 *  shouldn't get here!!
	 */
	nofault = 0;
	return(FAIL);
    }

    /*
     * Interrupt return here... now check error code in
     * the cause register.
     */
    nofault = 0;
    if ( (GetCause() & CAUSE_EXCMASK) != EXC_DBE )
        return(FAIL);

    /*
     * re-initialize memory and the cpu's sbc
     */
    ctl->wd    = 0;
    *(u_int*)k1_base = data_pat;
    act_val = *(u_int *)(k0_base + CACHE_SIZE*1);
    act_val = *(u_int *)(k0_base + CACHE_SIZE*2);
    act_val = *(u_int *)(k0_base + CACHE_SIZE*3);
    return(PASS);
}


/*------------------------------------------------------------------------+
| Routine name: ecc_wacmp                                                 |
| Description :                                                           |
+------------------------------------------------------------------------*/
static ecc_wacmp(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register u_int ecc_pat,		/* good ECC pattern              */
                   data_pat,
                   bank,
                   bad_bit;
    register u_int exp_val,		/* expected value                */
                   act_val;		/* actual   value                */


    /*
     * initialize memory block and get generated ecc pattern.
     */
    *(u_int *)CSR_IVECTCLR  = 0xffffffff;	/* clear extern intr bits */
    ctl->wd    = 0;
    for (bank = 0,  data_pat = TEST_PAT; bank < BANK_SIZE; bank += BANK_INC){
        *(u_int *)((u_int)work_addr + bank + CACHE_SIZE*3) = data_pat;
        *(u_int *)((u_int)work_addr + bank + CACHE_SIZE*2) = data_pat;
        *(u_int *)((u_int)work_addr + bank + CACHE_SIZE*1) = data_pat;
        *(u_int *)((u_int)work_addr + bank + CACHE_SIZE*0) = data_pat;
    }
    ecc_pat = *work_addr;
    ecc_pat = ecc->f.save;

    /*
     * disable cpu External Interrupts for these sbc vector bits
     */
    *(u_int *)CSR_COMPARE   = *(u_int *)CSR_COUNT - 1; /* avoid clock ints */
    *(u_int *)CSR_IVECTMASK = 0;		/* disable cpu external ints */
    *(u_int *)CSR_IVECTCLR  = 0xffffffff;	/* clear cpu int vector bits */

    /*
     * check  single-bit error interrupt  with  address compare  feature
     * enabled. This means that same single-bit error with in a same mem
     * bank would only interrupt on first access, not subsequence access.
     */
    ecc->f.bmask = 1;
    for (bank = 0; bank < BANK_SIZE; bank+= BANK_INC){

        /*
         * force bad bit in data word.
         */
        write_ecc((u_int)work_addr+bank+BANK_SIZE*3,data_pat^2,ecc_pat,ecc,ctl);
        write_ecc((u_int)work_addr+bank+BANK_SIZE*2,data_pat^2,ecc_pat,ecc,ctl);
        write_ecc((u_int)work_addr+bank+BANK_SIZE*1,data_pat^2,ecc_pat,ecc,ctl);
        write_ecc((u_int)work_addr+bank+BANK_SIZE*0,data_pat^2,ecc_pat,ecc,ctl);

        /*
         * set interrupt slot, enable memory interrupt and address
         * compare feature.
         */
        ctl->wd     = (MemIntEna | EnaSErrInt | EnaSErrLocCmp | EnaSErrCorrect);
        ctl->f.islt = cpu_slot;
        ctl->f.ibit = 0;

        /*
         * read bad data word in memory and verify that the memory
         * board writes to CPU's interrupt vector register.
         */
        act_val  = *(u_int *)((u_int)work_addr + bank + BANK_SIZE*0);
        DELAY(1024);

        exp_val  = 1;
        act_val  = *(u_int *)CSR_IVECTSET & SBC_MEMERR_MASK;
        if (act_val != exp_val)
            return(FAIL);

        /*
         * Clear all pending Interrupt.
         */
        *(u_int *)CSR_IVECTCLR = 0xffffffff;

        /*
         * make sure that  the next  three accesses  within  the
         * same bank  does not cause interrupt.
         */
        act_val  = *(u_int *)((u_int)work_addr + bank + BANK_SIZE*3);
        act_val  = *(u_int *)((u_int)work_addr + bank + BANK_SIZE*2);
        act_val  = *(u_int *)((u_int)work_addr + bank + BANK_SIZE*1);

        if (*(u_int *)CSR_IVECTSET & SBC_MEMERR_MASK)
            return(FAIL);

    }

    /*
     * make sure that single-bit interrupt could also be triggered by
     * different chip within the same bank.
     */
    for (bad_bit = 0x80; bad_bit > 0; bad_bit >>= 1){

        write_ecc(work_addr, data_pat ^ bad_bit, ecc_pat, ecc, ctl);

        /*
         * set interrupt slot, enable memory interrupt and address
         * compare feature.
         */
        ctl->wd     = (MemIntEna | EnaSErrInt | EnaSErrLocCmp | EnaSErrCorrect);
        ctl->f.islt = cpu_slot;
        ctl->f.ibit = 0;

        /*
         * read bad data word in memory and verify that the memory
         * board writes to CPU's interrupt vector register.
         */
        act_val  = *work_addr;
        DELAY(1024);

        exp_val  = 1;
        act_val  = *(u_int *)CSR_IVECTSET & SBC_MEMERR_MASK;
        if (act_val != exp_val)
            return(FAIL);

        /*
         * Clear all pending Interrupt.
         */
        *(u_int *)CSR_IVECTCLR = 0xffffffff;
    }


    /*
     * re-initialize memory block
     */
    ctl->wd   = 0;
    for (bank = 0; bank < BANK_SIZE; bank += BANK_INC){
        *(u_int *)((u_int)work_addr + bank + CACHE_SIZE*3) = data_pat;
        *(u_int *)((u_int)work_addr + bank + CACHE_SIZE*2) = data_pat;
        *(u_int *)((u_int)work_addr + bank + CACHE_SIZE*1) = data_pat;
        *(u_int *)((u_int)work_addr + bank + CACHE_SIZE*0) = data_pat;
    }

    return(PASS);
}


/*------------------------------------------------------------------------+
| Routine name: ecc_bacmp                                                 |
| Description :                                                           |
+------------------------------------------------------------------------*/
static ecc_bacmp(work_addr, ctl, ecc)
    u_int  *work_addr;
    MemCtl *ctl;
    MemEcc *ecc;
{
    register u_int k1_base,		/* k1 address                    */
                   k0_base,		/* k0 address                    */
                   ecc_pat,		/* generated ecc pattern         */
                   data_pat,		/* test data pattern             */
                   bank,		/* memory bank                   */
                   bad_bit;		/* force bad bit.                */
    register u_int exp_val,		/* expected value                */
                   act_val;		/* actual   value                */


    k1_base = (u_int)work_addr & 0xffffff80;
    k0_base = K1_TO_K0(k1_base);

    /*
     * initialize memory block and get generated ecc pattern.
     */
    ctl->wd    = 0;
    data_pat   = TEST_PAT;
    for (bank = 0; bank < BLOCK_SIZE; bank += 4){
        *(u_int *)(k1_base + bank + CACHE_SIZE*3) = data_pat;
        *(u_int *)(k1_base + bank + CACHE_SIZE*2) = data_pat;
        *(u_int *)(k1_base + bank + CACHE_SIZE*1) = data_pat;
        *(u_int *)(k1_base + bank + CACHE_SIZE*0) = data_pat;
    }
    ecc_pat    = *(u_int*)k1_base;
    ecc_pat    = ecc->f.save;

    /*
     * disable cpu External Interrupts for these sbc vector bits
     */
    *(u_int *)CSR_COMPARE   = *(u_int *)CSR_COUNT - 1; /* avoid clock ints */
    *(u_int *)CSR_IVECTMASK = 0;		/* disable cpu external ints */
    *(u_int *)CSR_IVECTCLR  = 0xffffffff;	/* clear cpu int vector bits */

    /*
     * check  single-bit error interrupt  with  address compare  feature
     * enabled. This means that same single-bit error with in a same mem
     * bank would only interrupt on first access, not subsequence access.
     */
    ecc->f.bmask = 1;
    for (bank = 0; bank < BANK_SIZE; bank+= BANK_INC){

        /*
         * force bad bit in data word.
         */
        write_ecc(k1_base + bank + CACHE_SIZE*0 + BANK_SIZE*0,
                  data_pat^4, ecc_pat, ecc, ctl);
        write_ecc(k1_base + bank + CACHE_SIZE*1 + BANK_SIZE*1,
                  data_pat^4, ecc_pat, ecc, ctl);
        write_ecc(k1_base + bank + CACHE_SIZE*2 + BANK_SIZE*2,
                  data_pat^4, ecc_pat, ecc, ctl);
        write_ecc(k1_base + bank + CACHE_SIZE*3 + BANK_SIZE*3,
                  data_pat^4, ecc_pat, ecc, ctl);

        /*
         * set interrupt slot, enable memory interrupt and address
         * compare feature.
         */
        ctl->wd     = (MemIntEna | EnaSErrInt | EnaSErrLocCmp | EnaSErrCorrect);
        *(u_int *)CSR_IVECTCLR  = 0xffffffff;	/* clear extern intr bits */
        ctl->f.islt = cpu_slot;
        ctl->f.ibit = 0;

        /*
         * read bad data word in memory and verify that the memory
         * board writes to CPU's interrupt vector register.
         */
        act_val = *(u_int *)(k0_base + bank);
        DELAY(1024);

        exp_val  = 1;
        act_val  = *(u_int *)CSR_IVECTSET & SBC_MEMERR_MASK;
        if (act_val != exp_val)
            return(FAIL);

        /*
         * make sure that  the next  three accesses  within  the
         * same bank  does not cause interrupt.
         */
        *(u_int *)CSR_IVECTCLR = 0xffffffff;
        act_val  = *(u_int *)(k0_base + bank + CACHE_SIZE*1 + BANK_SIZE*1);
        act_val  = *(u_int *)(k0_base + bank + CACHE_SIZE*2 + BANK_SIZE*2);
        act_val  = *(u_int *)(k0_base + bank + CACHE_SIZE*3 + BANK_SIZE*3);

        if (*(u_int *)CSR_IVECTSET & SBC_MEMERR_MASK)
            return(FAIL);

        /*
         * re-initialize the bad words in memory.
         */
        *(u_int *)(k1_base + bank + CACHE_SIZE*0 + BANK_SIZE*0) = data_pat;
        *(u_int *)(k1_base + bank + CACHE_SIZE*1 + BANK_SIZE*1) = data_pat;
        *(u_int *)(k1_base + bank + CACHE_SIZE*2 + BANK_SIZE*2) = data_pat;
        *(u_int *)(k1_base + bank + CACHE_SIZE*3 + BANK_SIZE*3) = data_pat;
    }

    /*
     * make sure that single-bit interrupt could also be triggered by
     * different chip within the same bank.
     */
    ecc->f.bmask = 1;
    for (bad_bit = 0x80000000; bad_bit > 0; bad_bit >>= 1){

        write_ecc(k1_base, data_pat ^ bad_bit, ecc_pat, ecc, ctl);

        /*
         * set interrupt slot, enable memory interrupt and address
         * compare feature.
         */
        ctl->wd     = (MemIntEna | EnaSErrInt | EnaSErrLocCmp | EnaSErrCorrect);
        *(u_int *)CSR_IVECTCLR = 0xffffffff;
        ctl->f.islt = cpu_slot;
        ctl->f.ibit = 0;

        /*
         * read bad data word in memory and verify that the memory
         * board writes to CPU's interrupt vector register.
         */
        act_val  = *(u_int*)k0_base;
        DELAY(1024);

        exp_val  = 1;
        act_val  = *(u_int *)CSR_IVECTSET & SBC_MEMERR_MASK;
        if (act_val != exp_val)
            return(FAIL);

        /*
         * Clear all pending Interrupt, re-initialize memory and flush
         * caches.
         */
        *(u_int*)k1_base  = data_pat;
        act_val = *(u_int *)(k0_base + CACHE_SIZE*1);
        act_val = *(u_int *)(k0_base + CACHE_SIZE*2);
        act_val = *(u_int *)(k0_base + CACHE_SIZE*3);
    }

    return(PASS);
}


/*------------------------------------------------------------------------+
| Routine name: write_ecc                                                 |
| Description :                                                           |
+------------------------------------------------------------------------*/
write_ecc(wr_addr, wr_data, wr_ecc, ecc, ctl)
    register u_int  *wr_addr, wr_data, wr_ecc;
    register MemCtl *ctl;
    register MemEcc *ecc;
{
    register u_int save_ctl;

    save_ctl = ctl->wd;
    ecc->f.wreg = wr_ecc;
    ctl->wd  = save_ctl | EnaEccInSel;
    *wr_addr = wr_data;
    ctl->wd  = save_ctl;

    /*
     * this forces the compiler to place MemCtl write immediately after
     * bad data write.
     */
    save_ctl = 0;
}

