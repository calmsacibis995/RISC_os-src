/* Unexec for MIPS (including IRIS4D).
   Note that the GNU project considers support for MIPS operation
   a peripheral activity which should not be allowed to divert effort
   from development of the GNU system.  Changes in this code will be
   installed when users send them in, but aside from that
   we don't plan to think about it, or about whether other Emacs
   maintenance might break it.

   Copyright (C) 1988 Free Software Foundation, Inc.

		       NO WARRANTY

  BECAUSE THIS PROGRAM IS LICENSED FREE OF CHARGE, WE PROVIDE ABSOLUTELY
NO WARRANTY, TO THE EXTENT PERMITTED BY APPLICABLE STATE LAW.  EXCEPT
WHEN OTHERWISE STATED IN WRITING, FREE SOFTWARE FOUNDATION, INC,
RICHARD M. STALLMAN AND/OR OTHER PARTIES PROVIDE THIS PROGRAM "AS IS"
WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY
AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
CORRECTION.

 IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW WILL RICHARD M.
STALLMAN, THE FREE SOFTWARE FOUNDATION, INC., AND/OR ANY OTHER PARTY
WHO MAY MODIFY AND REDISTRIBUTE THIS PROGRAM AS PERMITTED BELOW, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY LOST PROFITS, LOST MONIES, OR
OTHER SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR
DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY THIRD PARTIES OR
A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS) THIS
PROGRAM, EVEN IF YOU HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES, OR FOR ANY CLAIM BY ANY OTHER PARTY.

		GENERAL PUBLIC LICENSE TO COPY

  1. You may copy and distribute verbatim copies of this source file
as you receive it, in any medium, provided that you conspicuously and
appropriately publish on each copy a valid copyright notice "Copyright
(C) 1987 Free Software Foundation, Inc."; and include following the
copyright notice a verbatim copy of the above disclaimer of warranty
and of this License.  You may charge a distribution fee for the
physical act of transferring a copy.

  2. You may modify your copy or copies of this source file or
any portion of it, and copy and distribute such modifications under
the terms of Paragraph 1 above, provided that you also do the following:

    a) cause the modified files to carry prominent notices stating
    that you changed the files and the date of any change; and

    b) cause the whole of any work that you distribute or publish,
    that in whole or in part contains or is a derivative of this
    program or any part thereof, to be licensed at no charge to all
    third parties on terms identical to those contained in this
    License Agreement (except that you may choose to grant more extensive
    warranty protection to some or all third parties, at your option).

    c) You may charge a distribution fee for the physical act of
    transferring a copy, and you may at your option offer warranty
    protection in exchange for a fee.

Mere aggregation of another unrelated program with this program (or its
derivative) on a volume of a storage or distribution medium does not bring
the other program under the scope of these terms.

  3. You may copy and distribute this program (or a portion or derivative
of it, under Paragraph 2) in object code or executable form under the terms
of Paragraphs 1 and 2 above provided that you also do one of the following:

    a) accompany it with the complete corresponding machine-readable
    source code, which must be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    b) accompany it with a written offer, valid for at least three
    years, to give any third party free (except for a nominal
    shipping charge) a complete machine-readable copy of the
    corresponding source code, to be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    c) accompany it with the information you received as to where the
    corresponding source code may be obtained.  (This alternative is
    allowed only for noncommercial distribution and only if you
    received the program in object code or executable form alone.)

For an executable file, complete source code means all the source code for
all modules it contains; but, as a special exception, it need not include
source code for modules which are standard libraries that accompany the
operating system on which the executable file runs.

  4. You may not copy, sublicense, distribute or transfer this program
except as expressly provided under this License Agreement.  Any attempt
otherwise to copy, sublicense, distribute or transfer this program is void and
your rights to use the program under this License agreement shall be
automatically terminated.  However, parties who have received computer
software programs from you with this License Agreement will not have
their licenses terminated so long as such parties remain in full compliance.

  5. If you wish to incorporate parts of this program into other free
programs whose distribution conditions are different, write to the Free
Software Foundation at 675 Mass Ave, Cambridge, MA 02139.  We have not yet
worked out a simple rule that can be stated here, but we will often permit
this.  We will be guided by the two goals of preserving the free status of
all derivatives of our free software and of promoting the sharing and reuse of
software.


In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#include <varargs.h>
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <sym.h>

#ifdef IRIS_4D
#include "getpagesize.h"
#include <fcntl.h>
#endif

static void fatal_unexec ();

#define READ(_fd, _buffer, _size, _error_message, _error_arg) \
	errno = EEOF; \
	if (read(_fd, _buffer, _size) != _size) \
	  fatal_unexec(_error_message, _error_arg);

#define WRITE(_fd, _buffer, _size, _error_message, _error_arg) \
	if (write(_fd, _buffer, _size) != _size) \
	  fatal_unexec(_error_message, _error_arg);

#define SEEK(_fd, _position, _error_message, _error_arg) \
	errno = EEOF; \
	if (lseek(_fd, _position, L_SET) != _position) \
	  fatal_unexec(_error_message, _error_arg);

#define private static

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];
#define EEOF -1

struct headers {
    struct filehdr fhdr;
    struct aouthdr aout;
    struct scnhdr text_section;
    struct scnhdr rdata_section;
    struct scnhdr data_section;
    struct scnhdr sdata_section;
    struct scnhdr sbss_section;
    struct scnhdr bss_section;
};

/* Define name of label for entry point for the dumped executable.  */

#ifndef DEFAULT_ENTRY_ADDRESS
#define DEFAULT_ENTRY_ADDRESS __start
#endif

unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
  int new, old;
  int pagesize, brk;
  int newsyms, symrel;
  int nread;
  struct headers hdr;
#define BUFSIZE 8192
  char buffer[BUFSIZE];

  old = open (a_name, O_RDONLY, 0);
  if (old < 0) fatal_unexec("openning %s", a_name);

  new = creat (new_name, 0666);
  if (new < 0) fatal_unexec("creating %s", new_name);

  hdr = *((struct headers *)TEXT_START);
  if (hdr.fhdr.f_magic != MIPSELMAGIC
      && hdr.fhdr.f_magic != MIPSEBMAGIC) {
      fprintf(stderr, "unexec: input file magic number is %x, not %x or %x.\n",
	      hdr.fhdr.f_magic, MIPSELMAGIC, MIPSEBMAGIC);
      exit(1);
  }
  if (hdr.fhdr.f_opthdr != sizeof(hdr.aout)) {
      fprintf(stderr, "unexec: input a.out header is %d bytes, not %d.\n",
	      hdr.fhdr.f_opthdr, sizeof(hdr.aout));
      exit(1);
  }
#if 0
  if (hdr.aout.magic != ZMAGIC
      && hdr.aout.magic != NMAGIC
      && hdr.aout.magic != OMAGIC) {
      fprintf(stderr, "unexec: input file a.out magic number is %o, not %o, %o, or %o.\n",
	      hdr.aout.magic, ZMAGIC, NMAGIC, OMAGIC);
      exit(1);
  }
#else
  if (hdr.aout.magic != ZMAGIC) {
      fprintf(stderr, "unexec: input file a.out magic number is %o, not %o.\n",
	      hdr.aout.magic, ZMAGIC);
      exit(1);
  }
#endif
  if (hdr.fhdr.f_nscns != 6) {
      fprintf(stderr, "unexec: %d sections instead of 6.\n", hdr.fhdr.f_nscns);
  }
#define CHECK_SCNHDR(field, name, flags) \
  if (strcmp(hdr.field.s_name, name) != 0) { \
      fprintf(stderr, "unexec: %s section where %s expected.\n", \
	      hdr.field.s_name, name); \
      exit(1); \
  } \
  else if (hdr.field.s_flags != flags) { \
      fprintf(stderr, "unexec: %x flags where %x expected in %s section.\n", \
	      hdr.field.s_flags, flags, name); \
  }
  CHECK_SCNHDR(text_section,  _TEXT,  STYP_TEXT);
  CHECK_SCNHDR(rdata_section, _RDATA, STYP_RDATA);
  CHECK_SCNHDR(data_section,  _DATA,  STYP_DATA);
  CHECK_SCNHDR(sdata_section, _SDATA, STYP_SDATA);
  CHECK_SCNHDR(sbss_section,  _SBSS,  STYP_SBSS);
  CHECK_SCNHDR(bss_section,   _BSS,   STYP_BSS);

  pagesize = getpagesize();
  brk = (sbrk(0) + pagesize - 1) & (-pagesize);
  hdr.aout.dsize = brk - DATA_START;
  hdr.aout.bsize = 0;
  if (entry_address == 0) {
    extern DEFAULT_ENTRY_ADDRESS();
    hdr.aout.entry = (unsigned)DEFAULT_ENTRY_ADDRESS;
  }
  else {
    hdr.aout.entry = entry_address;
  }
  hdr.aout.bss_start = hdr.aout.data_start + hdr.aout.dsize;
  hdr.rdata_section.s_size = data_start - DATA_START;
  hdr.data_section.s_vaddr = data_start;
  hdr.data_section.s_paddr = data_start;
  hdr.data_section.s_size = brk - DATA_START;
  hdr.data_section.s_scnptr = hdr.rdata_section.s_scnptr
				+ hdr.rdata_section.s_size;
  hdr.sdata_section.s_vaddr = hdr.data_section.s_vaddr
				+ hdr.data_section.s_size;
  hdr.sdata_section.s_paddr = hdr.sdata_section.s_paddr;
  hdr.sdata_section.s_size = 0;
  hdr.sdata_section.s_scnptr = hdr.data_section.s_scnptr
				+ hdr.data_section.s_size;
  hdr.sbss_section.s_vaddr = hdr.sdata_section.s_vaddr
				+ hdr.sdata_section.s_size;
  hdr.sbss_section.s_paddr = hdr.sbss_section.s_vaddr;
  hdr.sbss_section.s_size = 0;
  hdr.sbss_section.s_scnptr = hdr.sdata_section.s_scnptr
				+ hdr.sdata_section.s_size;
  hdr.bss_section.s_vaddr = hdr.sbss_section.s_vaddr
				+ hdr.sbss_section.s_size;
  hdr.bss_section.s_paddr = hdr.bss_section.s_vaddr;
  hdr.bss_section.s_size = 0;
  hdr.bss_section.s_scnptr = hdr.sbss_section.s_scnptr
				+ hdr.sbss_section.s_size;

  WRITE(new, TEXT_START, hdr.aout.tsize,
	"writing text section to %s", new_name);
  WRITE(new, DATA_START, hdr.aout.dsize,
	"writing text section to %s", new_name);

  SEEK(old, hdr.fhdr.f_symptr, "seeking to start of symbols in %s", a_name);
  errno = EEOF;
  nread = read(old, buffer, BUFSIZE);
  if (nread < sizeof(HDRR)) fatal_unexec("reading symbols from %s", a_name);
#define symhdr ((pHDRR)buffer)
  newsyms = hdr.aout.tsize + hdr.aout.dsize;
  symrel = newsyms - hdr.fhdr.f_symptr;
  hdr.fhdr.f_symptr = newsyms;
  symhdr->cbLineOffset += symrel;
  symhdr->cbDnOffset += symrel;
  symhdr->cbPdOffset += symrel;
  symhdr->cbSymOffset += symrel;
  symhdr->cbOptOffset += symrel;
  symhdr->cbAuxOffset += symrel;
  symhdr->cbSsOffset += symrel;
  symhdr->cbSsExtOffset += symrel;
  symhdr->cbFdOffset += symrel;
  symhdr->cbRfdOffset += symrel;
  symhdr->cbExtOffset += symrel;
#undef symhdr
  do {
      if (write(new, buffer, nread) != nread)
	fatal_unexec("writing symbols to %s", new_name);
      nread = read(old, buffer, BUFSIZE);
      if (nread < 0) fatal_unexec("reading symbols from %s", a_name);
#undef BUFSIZE
  } while (nread != 0);

  SEEK(new, 0, "seeking to start of header in %s", new_name);
  WRITE(new, &hdr, sizeof(hdr),
	"writing header of %s", new_name);

  close(old);
  close(new);
  mark_x(new_name);
}

/*
 * mark_x
 *
 * After succesfully building the new a.out, mark it executable
 */
static
mark_x (name)
     char *name;
{
  struct stat sbuf;
  int um = umask (777);
  umask (um);
  if (stat (name, &sbuf) < 0)
    fatal_unexec ("getting protection on %s", name);
  sbuf.st_mode |= 0111 & ~um;
  if (chmod (name, sbuf.st_mode) < 0)
    fatal_unexec ("setting protection on %s", name);
}

static void
fatal_unexec (s, va_alist)
    va_dcl
{
  va_list ap;
  if (errno == EEOF)
    fputs ("unexec: unexpected end of file, ", stderr);
  else if (errno < sys_nerr)
    fprintf (stderr, "unexec: %s, ", sys_errlist[errno]);
  else
    fprintf (stderr, "unexec: error code %d, ", errno);
  va_start (ap);
  _doprnt (s, ap, stderr);
  fputs (".\n", stderr);
  exit (1);
}
