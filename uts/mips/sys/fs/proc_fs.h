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
/* $Header: proc_fs.h,v 1.1.1.2 90/05/10 06:17:47 wje Exp $ */

/*
 * /proc vnode filesystem definitions.
 */


/*
* /proc filesystem state information.  Private.  Keep out.
*/

struct	proc_state {
	int proc_init;		/* Boolean: has initialization code executed */
	int proc_mounted;	/* Boolean says if /proc has been mounted */
	struct vfs * proc_vfsp;	/* the vfs for /proc			*/
};

/* The focus of activity in the /proc filesystem
* is the process-inode or pinode.
*/

typedef struct pinode {
	struct pinode	*pi_next;
	struct vnode	pi_vnode;
	int		pi_id;		/* PID of process or PROCROOT	*/
	int		pi_mode;	/* mode of "file"		*/
	int		pi_xstat;	/* saved exit status		*/
} pinode_t;

/*
 * Macros to convert between vnode pointers and pinode pointers
 */

#define	PTOV(PP)	((struct vnode *)( & (PP)->pi_vnode))
#define	VTOP(VP)	((pinode_t *)((VP)->v_data))

#define	PROCROOT	30001	/* PID value for root vnode of /proc	*/

#define	PROCDSIZ	16	/* we use fixed 16 bytes for dir entries */

#define	PROCBLKSIZ	1024	/* the /proc filesystem block size	*/

#define	TRUE		1
#define	FALSE		0
