#ident "$Header: gencom.c,v 1.2 90/01/16 16:57:30 huang Exp $"
/* $Copyright$ */

#include "sys/errno.h"
#include "sys/param.h"
#include "sys/inode.h"
#include "sys/fs.h"
#include "sys/dir.h"
#include "netinet/in.h"
#include "machine/dvh.h"
#include "saio/tpd.h"
#include "saio/ei.h"

main()
{
	union {
		struct	inode ui_ino;		/* inode, if disk file */
		struct	tp_entry ui_te;		/* tape dir, if boottape */
		struct	ether_info ui_ei;	/* enet addresses, if net */
		struct	volume_directory ui_vd;	/* dvh directory entry */
	} inode_size;
	union {
		struct	tp_entry ui_te;		/* tape dir, if boottape */
		struct	ether_info ui_ei;	/* enet addresses, if net */
		struct	volume_directory ui_vd;	/* dvh directory entry */
	} p_inode_size;
	union {
		struct fs ui_fs;	/* file system super block info */
		struct tp_dir ui_tp;	/* tape directory info */
	} fs_size;
	union {
		struct tp_dir ui_tp;	/* tape directory info */
	} p_fs_size;
	printf ("inode_size %d, p_inode_size %d, fs_size %d, p_fs_size %d\n",
		sizeof (inode_size), sizeof (p_inode_size),
		sizeof (fs_size), sizeof (p_fs_size));
}
