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
#ident	"$Header: libutil.c,v 1.3.2.3 90/05/10 02:51:01 wje Exp $"

#include <stdio.h>
#include <ctype.h>
#include <nlist.h>

#include <filehdr.h>
#include <scnhdr.h>
#include <syms.h>
#include <ldfcn.h>
#include "../libutil.h"

extern long lseek();

struct nlist lib_nl[] = {
#define	X_STRING    0
	{ "id_string", (long)0, (short)0, (short)0 },
	{ (char *)0, (long)0, (short)0, (short)0 },
};

char *data_section_names[] = {
  	".rdata",
	".data",
	".sdata",
	NULL };

/*
* Procedure: check_kernel_id
*
* Description:
* This internal procedure reads the text file designated by "kernel_lfn"
* from which the system was booted and compares the values of the kernel
* symbol "id-string" appearing in the boot file. Zero is returned on success
* and non-zero indicates the type of failure error return code.
*/
int 
check_kernel_id (memfd, id_offset, kernel_lfn)
	int	memfd;		/* fd for /dev/kmem */
	long	id_offset;	/* offset to variable id */
        char    *kernel_lfn;	/* booted kernel filename */
{
	LDFILE *file;
	int	file_id_addr;
	char	file_id[MAX_KERNEL_ID_LEN + 1];
	int	kernel_id_addr;
	char	kernel_id[MAX_KERNEL_ID_LEN + 1];
	int	cnt, error, nbytes;

	if (id_offset == 0)
	 return LIB_ERR_BADOFFSET; /* invalid offset failure */

	if ((file = ldopen (kernel_lfn, NULL)) == NULL) {
	  /* Can't open kernel text file */
	  return LIB_ERR_KFILEOPEN;
	};

	id_offset &= 0x7fffffff;
	if (read_kernel_text (file, id_offset, (char *) &file_id_addr,
			      sizeof(int), &cnt)) {
		ldclose(file);
		return LIB_ERR_KTEXTREAD; /* Failed reading kernel text seg*/
	};

	if (cnt != sizeof(file_id_addr)) { /* Can't locate data in kernel */
		ldclose(file);
                return LIB_ERR_KNODATA;

	};

	file_id_addr &= 0x7fffffff;
	if (read_kernel_text (file, file_id_addr, file_id,
			      MAX_KERNEL_ID_LEN,&cnt)) {
		ldclose(file);
		return LIB_ERR_KTEXTREAD; /* Failed reading kernel text seg */
	};

	if (cnt < 0) { /* Can't locate data in kernel text file */
		ldclose(file);
		return LIB_ERR_KNODATA;
	};	

	ldclose (file);

	file_id[cnt] = 0;
	file_id[MAX_KERNEL_ID_LEN] = 0;

	error = lseek (memfd, id_offset, 0); /* read */
	if (error == -1) return LIB_ERR_KLSEEK; /* lseek failure error */

	nbytes = read (memfd, (char *)&kernel_id_addr,
		       (int)sizeof(kernel_id_addr));
	if (nbytes != sizeof(kernel_id_addr))
	  return LIB_ERR_SHORTREAD; /* failure short read*/

	kernel_id_addr &= 0x7fffffff;
	error = lseek (memfd, kernel_id_addr,0);
	if (error == -1) return LIB_ERR_KLSEEK; /* lseek failure error */

	nbytes = read (memfd, kernel_id, MAX_KERNEL_ID_LEN);
	if (nbytes != MAX_KERNEL_ID_LEN)
	  return LIB_ERR_SHORTREAD; /* failure short read*/

	kernel_id[MAX_KERNEL_ID_LEN] = 0;
	
	if (strcmp (file_id, kernel_id)) { /* id strings don't match */
	  return LIB_ERR_NOMATCH;
	};
	return LIB_SUCCESS; /* success as id strings match */
}

/*
* Procedure: correct_kernel
*
* Description:
* This externally advertised procedure looks up the kernel symbol "id-string"
* associated with the kernel name and compares the address/offset of this
* string in memory via /dev/kmem against the address of the string body in
* the code file. In addition the two string bodies are compared for equality.
* The procedure returns the following values:
*
* (1) Non-zero indicates failure. The following cases are covered:
*     The string addresses mismatch, the string bodies miscompare, the kernel
*     symbol wasn't found or some fileio operation failed.
* (2) zero indicates success.
*/
int 
correct_kernel ()
{
	int failure, memfd;

        failure = 1;
        lib_nl[X_STRING].n_type = lib_nl[X_STRING].n_value = 0;
	nlist ("/unix", lib_nl);

        if ( (lib_nl[X_STRING].n_type == 0) || (lib_nl[X_STRING].n_value == 0))
	 { /* either wrong kernel or didn't find symbol */
         return failure;
	 }

	memfd = open ("/dev/kmem", 0);
	if (memfd < 0) return failure;

        failure = check_kernel_id (memfd, (long)lib_nl[X_STRING].n_value, 
				"/unix");
        close (memfd);
        return failure;
}

/*
* Procedure: read_kernel_text
*
* Description:
* This internal procedure performs the actual reads of the text file which
* file handle is designated by "file". It returns zero to indicate the
* data_offset was found in the file and 1 otherwise.
*/
int 
read_kernel_text (file, data_offset, buf, buflen, cntp)
	LDFILE *file;
	int	data_offset;
	char	*buf;
	int	buflen;
	int	*cntp;
{
	SCNHDR  datahdr;
	int 	i;

	for (i = 0; data_section_names[i] != NULL; i++) {
		if (ldnshread(file,data_section_names[i],&datahdr) != SUCCESS)
			continue;

		if ((data_offset < (datahdr.s_vaddr & 0x7fffffff)) ||
		    (data_offset >= ((datahdr.s_vaddr & 0x7fffffff) +
					datahdr.s_size)))
			continue;

		if (ldnsseek(file,data_section_names[i]) != SUCCESS ||
		    FSEEK(file,datahdr.s_scnptr + (data_offset -
			 (datahdr.s_vaddr & 0x7fffffff)),BEGINNING) != 0)
			continue;

		*cntp = FREAD (buf,1, buflen, file);
		if (*cntp <= 0) break;
		return(0);
	};
 	/* Cannot locate data in kernel text file */
	return(1);
}

