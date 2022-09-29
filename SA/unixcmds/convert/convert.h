#ident "$Header: convert.h,v 1.1.11.1 90/07/18 16:42:57 huang Exp $"
/* $Header: convert.h,v 1.1.11.1 90/07/18 16:42:57 huang Exp $ */
/* $Log:	convert.h,v $
 * Revision 1.1.11.1  90/07/18  16:42:57  huang
 * Branch GENESIS_BETA1 off of trunk.
 * 
 * Revision 1.1  87/08/18  16:52:54  mdove
 * Initial revision
 *  */

#define TRUE 1
#define FALSE 0
#define OK 0
#define NOT_OK 1

/*
 * Control flags for match routine
 */
#define IGNORECASE 1
#define EXACTMATCH 2

/*
 * Standard type definitions
 */
typedef unsigned long address;

/*
 * Object file type control structures
 */

enum object_file_type { BERKELEY = 0, COFF = 1, MIPS = 2 };

struct object_file_type_table_descriptor {

	short magic;
	enum object_file_type type;
};

struct object_file_descriptor {

	enum object_file_type type;
	int (*initialize)();
	int (*read)();
	int (*close)();
};

/*
 * Output format control structures
 */

struct format_descriptor {

	int blocking_factor;
	int (*initialize)();
	int (*convert)();
	int (*write)();
	int (*terminate)();

};

/*
 * Physical address of at which to load next byte
 */
extern unsigned long start_address;
extern unsigned long load_address;
extern int byte_number;
extern char *progname;
extern struct object_file_type_table_descriptor object_file_type_table[];
extern struct object_file_descriptor object_files[];
extern struct object_file_descriptor *object;
extern struct format_descriptor formats[];
extern struct format_descriptor *format;
extern int my_sex;			/* Host byte sex */
extern int swap;			/* True if headers and object of opposite sex */
