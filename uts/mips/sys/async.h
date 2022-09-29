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
/* $Header: async.h,v 1.2.3.2 90/05/10 06:04:54 wje Exp $ */
/* async control block */

#ifdef LANGUAGE_C
struct tty_atom {
    unsigned char       tty_text;
    unsigned char       tty_info;
};

/* tty_info bit definitions (valid only for receive, else always zero       */
/*      bit 0           parity error                                        */
/*      bit 1           framing error                                       */
/*      bit 2           uart overrun error                                  */
/*      bit 3           soft overrun error                                  */
/*      bit 7           "break" received                                    */


struct rcv {
    struct tty_atom     rcv_char;       /* used for simple I/O              */
    short               rcv_reladdr;    /* relative address of rcv buffer   */
    short               rcv_len;        /* length of receive buffer         */
    short               rcv_in;         /* index into buffer (iop)          */
    short               rcv_out;        /* index into buffer (R2000)        */
};

struct xmt {
    struct tty_atom     xmt_char;       /* used for simple I/O              */
    short               xmt_reladdr;    /* relative address of xmit buffer  */
    short               xmt_len;        /* length of transmit string        */
};

struct async {
        short           async_command;
        short           async_status;
                /*      bits 0..3       command status                      */
                /*      bit 4           comm error                          */
                /*      bit 5           receive data available              */
                /*      bit 6           transmitter ready                   */
                /*      bit 7           transmitter empty                   */
                /*      bit 8           output blocked (hardware)           */
                /*      bit 9           output blocked (software)           */
                /*      bit 10          input block request (hardware)      */
                /*      bit 11          input block request (software)      */
                /*      bit 15          interrupt pending                   */

        short           async_RS232C;
                /*                      baudrate        (see below)         */
                /*                      stopbits        (1, 1.5, 2, sync)   */
                /*                      data length     (5, 6, 7, 8)        */
                /*                      mode            simple/normal       */

        short           async_modem_out;
        short           async_modem_in;

        short           async_vtim;
        short           async_vmin;
        short           pad1;   /* structure alignment for 32bit machines */
        long            async_mappad;

        short           async_flow;
/*      bit 0           hardware output flow control (DTR)              */
/*      bit 1           hardware output flow control (RTS)              */
/*      bit 2           software output flow control                    */
/*      bit 3           sw out flow, restart on anything                */
/*      bit 4           hardware input flow control (DSR)               */
/*      bit 5           hardware input flow control (CTS)               */
/*      bit 6           software input flow control                     */
/*      bit 7           < eat x-on and x-off characters >               */

        unsigned char   async_x_on;
        unsigned char   async_x_off;
        unsigned short  async_lit_c;
        char            pad2[2];

        struct rcv      async_receive;
        struct xmt      async_transmit;

        short           async_nmaps;
        short           async_maps[1];
};
#endif /* LANGUAGE_C */




/* baudrates */
#define         ASYNC_BAUD_F    0
#define         ASYNC_BAUD_M    (0x1F << ASYNC_BAUD_F)
#  define       ASYNC_B0        (0x00 << ASYNC_BAUD_F)
#  define       ASYNC_B50       (0x01 << ASYNC_BAUD_F)
#  define       ASYNC_B75       (0x02 << ASYNC_BAUD_F)
#  define       ASYNC_B110      (0x03 << ASYNC_BAUD_F)
#  define       ASYNC_B134      (0x04 << ASYNC_BAUD_F)
#  define       ASYNC_B150      (0x05 << ASYNC_BAUD_F)
#  define       ASYNC_B200      (0x06 << ASYNC_BAUD_F)
#  define       ASYNC_B300      (0x07 << ASYNC_BAUD_F)
#  define       ASYNC_B600      (0x08 << ASYNC_BAUD_F)
#  define       ASYNC_B1200     (0x09 << ASYNC_BAUD_F)
#  define       ASYNC_B1800     (0x0A << ASYNC_BAUD_F)
#  define       ASYNC_B2400     (0x0B << ASYNC_BAUD_F)
#  define       ASYNC_B4800     (0x0C << ASYNC_BAUD_F)
#  define       ASYNC_B9600     (0x0D << ASYNC_BAUD_F)
#  define       ASYNC_B19200    (0x0E << ASYNC_BAUD_F)
#  define       ASYNC_B38400    (0x0F << ASYNC_BAUD_F)
/* 0x10 .. 0x1F reserved */

/* stop bits */
#define         ASYNC_STOP_F    8
#define         ASYNC_STOP_M    (0x03 << ASYNC_STOP_F)
#  define       ASYNC_STOP0     (0x00 << ASYNC_STOP_F)
#  define       ASYNC_STOP1     (0x01 << ASYNC_STOP_F)
#  define       ASYNC_STOP15    (0x02 << ASYNC_STOP_F)
#  define       ASYNC_STOP2     (0x03 << ASYNC_STOP_F)

/* data length */
#define         ASYNC_LENGTH_F  10
#define         ASYNC_DATA_M    (0x03 << ASYNC_LENGTH_F)
#  define       ASYNC_DATA5     (0x00 << ASYNC_LENGTH_F)
#  define       ASYNC_DATA6     (0x01 << ASYNC_LENGTH_F)
#  define       ASYNC_DATA7     (0x02 << ASYNC_LENGTH_F)
#  define       ASYNC_DATA8     (0x03 << ASYNC_LENGTH_F)

/* mode */
#define         ASYNC_MODE_F    12
#define         ASYNC_MODE_M    (0x01 << ASYNC_MODE_F)
#  define       ASYNC_BSMODE    (0x00 << ASYNC_MODE_F)
#  define       ASYNC_SILO      (0x01 << ASYNC_MODE_F)

/* parity */
#define         ASYNC_PARITY_F  13
#define         ASYNC_PAR_M     (0x07 << ASYNC_PARITY_F)
#  define       ASYNC_NO_PAR    (0x00 << ASYNC_PARITY_F)
#  define       ASYNC_EVEN_PAR  (0x01 << ASYNC_PARITY_F)
#  define       ASYNC_ODD_PAR   (0x02 << ASYNC_PARITY_F)
#  define       ASYNC_MARK_PAR  (0x03 << ASYNC_PARITY_F)
#  define       ASYNC_SPACE_PAR (0x04 << ASYNC_PARITY_F)

/* async_modem_in */
#define         ASYNC_CTS       (0x01 << 0)
#define         ASYNC_DCD       (0x01 << 1)

/* async_modem_out */
#define         ASYNC_DTR       (0x01 << 0)
#define         ASYNC_RTS       (0x01 << 1)
/* send break bit (handle like a modem control line) */
#define			ASYNC_SBRK_F	2
#define			ASYNC_SBRK_M	(0x01 << ASYNC_SBRK_F)
#define			ASYNC_SBRK		(0x01 << ASYNC_SBRK_F)


/*
 * commands:
 *      bits 0-7        nop
 *                      initialize channel
 *                      change parameter        (doesn't interrupt data flow)
 *                      transmit
 *                      acknowledge reception
 *                      abort transmit
 *
 *      bit 8           interrupt when transmitter ready
 *      bit 9           interrupt on receive error
 *      bit 10          interrupt when receive data available AND (vtim | vmin)
 *      bit 11          interrupt on modem line change (exclusive of flow cntl)
 *      bit 12          interrupt on change in flow control
 *
 *      bit 13          reset error status bit
 *
 */
#define         ASYNC_CMD_F     0
#define         ASYNC_CMD_M     (0x1F << ASYNC_CMD_F)
#  define ASYNC_NOP             (0x00 << ASYNC_CMD_F)
#  define ASYNC_INIT            (0x01 << ASYNC_CMD_F)
#  define ASYNC_PARAM           (0x02 << ASYNC_CMD_F)
#  define ASYNC_XMIT            (0x03 << ASYNC_CMD_F)
#  define ASYNC_RCV_ACK         (0x04 << ASYNC_CMD_F)
#  define ASYNC_STOP            (0x05 << ASYNC_CMD_F)
#  define ASYNC_IFLUSH          (0x06 << ASYNC_CMD_F)
#  define ASYNC_OFLUSH          (0x07 << ASYNC_CMD_F)
#  define ASYNC_FLOW            (0x08 << ASYNC_CMD_F)
#  define ASYNC_RESET_ERROR     (0x09 << ASYNC_CMD_F)

#define         ASYNC_INTR_F    5
#define         ASYNC_INTR_M    (0x7F << ASYNC_INTR_F)
#  define ASYNC_IXMIT_READY     (0x01 << 0 << ASYNC_INTR_F)
#  define ASYNC_IXMIT_EMPTY     (0x01 << 1 << ASYNC_INTR_F)
#  define ASYNC_IRECVERR        (0x01 << 2 << ASYNC_INTR_F)
#  define ASYNC_IRECV           (0x01 << 3 << ASYNC_INTR_F)
#  define ASYNC_IMODEM          (0x01 << 4 << ASYNC_INTR_F)
#  define ASYNC_IFLOW           (0x01 << 5 << ASYNC_INTR_F)
#  define ASYNC_ICMDRDY         (0x01 << 6 << ASYNC_INTR_F)

#define         ASYNC_MOD_F     12
#define         ASYNC_MOD_M     (0x01 << ASYNC_MOD_F)
#  define ASYNC_RESERR          (0x01 << 0 << ASYNC_MOD_F)

/*
 * status:
 *
 */


#define         ASYNC_STAT_F    0
#define         ASYNC_STAT_M    (0x0F << ASYNC_STAT_F)
#  define       ASYNC_STAT_SUCCESS      (0x00 << ASYNC_CMD_F)
#  define       ASYNC_STAT_NOTIMPL      (0x01 << ASYNC_CMD_F)
#  define       ASYNC_STAT_MAP          (0x02 << ASYNC_CMD_F)
#  define       ASYNC_STAT_UNKNOWN      (0x03 << ASYNC_CMD_F)

#define         ASYNC_STAT_BITS 4
#  define       ASYNC_STAT_ERROR        (0x01 << 0 << ASYNC_STAT_BITS)
#  define       ASYNC_STAT_RXRDY        (0x01 << 1 << ASYNC_STAT_BITS)
#  define       ASYNC_STAT_TXRDY        (0x01 << 2 << ASYNC_STAT_BITS)
#  define       ASYNC_STAT_TXEMP        (0x01 << 3 << ASYNC_STAT_BITS)
#  define       ASYNC_STAT_OBH          (0x01 << 4 << ASYNC_STAT_BITS)
#  define       ASYNC_STAT_OBS          (0x01 << 5 << ASYNC_STAT_BITS)
#  define       ASYNC_STAT_IBH          (0x01 << 6 << ASYNC_STAT_BITS)
#  define       ASYNC_STAT_IBS          (0x01 << 7 << ASYNC_STAT_BITS)
#  define		ASYNC_STAT_BIP			(0x01 << 8 << ASYNC_STAT_BITS)

/* from tty_atom.tty_info */
#define ASYNC_PARITY_ERR	(1<<0)	/* parity error 		*/
#define ASYNC_FRAME_ERR 	(1<<1)	/* framing error		*/
#define ASYNC_UART_OV 		(1<<2)	/* uart overrun 		*/
#define ASYNC_SOFT_OV 		(1<<3)	/* soft overrun 		*/
#define ASYNC_BREAK 		(1<<7)	/* "break" received	 	*/

/* async_flow */
#define ASYNC_HW_DTR    (1<<0)  /* hardware output flow control (DTR)   */
#define ASYNC_HW_RTS    (1<<1)  /* hardware output flow control (RTS)   */
#define ASYNC_SW_OFLOW  (1<<2)  /* software output flow control         */
#define ASYNC_SW_XANY   (1<<3)  /* sw out flow, restart on anything     */
#define ASYNC_HW_DSR    (1<<4)  /* hardware input flow control (DSR)    */
#define ASYNC_HW_CTS    (1<<5)  /* hardware input flow control (CTS)    */
#define ASYNC_SW_IFLOW  (1<<6)  /* software input flow control          */
#define ASYNC_EAT_FLOW  (1<<7)  /* eat x-on and x-off characters        */
/* __EOF__ */
