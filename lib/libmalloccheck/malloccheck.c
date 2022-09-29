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
#ident	"$Header: malloccheck.c,v 1.1.2.2 90/05/10 02:50:47 wje Exp $"
/* This code provides versions of malloc, free, and realloc that do
   extensive checking to help catch bugs.

   1. Blocks are surrounded by magic words to check for bashing.
   2. The block is initialized non-zero to catch programs that
      assume zero'd data.
   3. Blocks are never reused.
   4. Blocks are overwritten on free to catch programs that reference
      them after freeing.
   5. Freeing an already free block is checked.
   6. Realloc of freed blocks is flagged, as is realloc(NULL), to help
      catch non-portable usage.
   7. Calling malloc_status(0) at the end of the program will check
      for problems that haven't come up during the normal malloc/free
      checking.  Some statistics are printed.  malloc_status(1) does
      this and additionally prints blocks that have not been freed
      (catch your memory leaks).
   8. Setting malloctrace to an address allows you to set a
      breakpoint to see who allocates that address.
*/

#include <varargs.h>
#include <stdio.h>

extern char *sbrk();
extern int _doprnt(char *, va_list, FILE *);

static void error();
#define ESIZE 0
#define EFULL 1
#define EFREE 2
#define EMAGIC 3
#define EALLOCD 4
#define ENULL 5
#define EREFREE 6
#define NERR 7

static char *next;		/* next address to hand out */
static char *end;		/* end of current sbrk'd region */

#define MAXSIZE (16<<20)

#define MAGIC1 0x87ccccc1
#define MAGIC2 0x87ccccc2
#define MAGIC3 0x87ccccc3
#define MAGIC4 0x87ccccc4
#define MAGIC5 0x87ccccc5

static char *malloctrace;	/* for debugging set to address of block
				   and set breakpoint on fprintf. */
#define MAXAREA 1000

static struct {
  char *address;
  unsigned size;
} area[MAXAREA];
static unsigned narea;

static unsigned bytes_inuse;
static unsigned maximum_bytes_inuse;


char *
malloc (n)
    register int n;
{
  register unsigned b;
  register char *p, *q;

  if (n <= 0 || n >= MAXSIZE) {
    error (ESIZE, "Warning: malloc(%d).  Will return NULL.", n);
    return NULL;
  }
  b = (n + 5*sizeof(int) + 7) & -8;
retry:
  p = next;
  if (p+b > end) {
    register unsigned incr;
    register char *more;
    incr = b < 65536 ? 65536 : (b+4191) & -4192;
    more = sbrk(incr);
    if (more == (char *)-1) {
      error (EFULL, "Warning: sbrk(%d) failed.  Will return NULL.", incr);
      return NULL;
    }
    if (more == end) {
      area[narea-1].size += incr;
    }
    else {
      if (((unsigned)more & 7) != 0) {
	/* sbrk memory not double-word aligned!! */
	/* try to fix */
	if (sbrk((unsigned)more & 7) != more + incr) {
	  /* couldn't fix */
	  incr -= (unsigned)more & 7;
	}
	more = (char *)(((unsigned)more + 7) & -8);
      }
      area[narea].address = more;
      area[narea].size = incr;
      narea += 1;
      next = more;
    }
    end = more + incr;
    goto retry;
  }
  next = p+b;
  ((int*)p)[0] = n;
  ((int*)p)[1] = MAGIC1;
  q = (char *)((int*)p + 2);
  memset(q, 0xa5, n);
  if (b-5*sizeof(int)-n != 0) {
    memset(q+n, 0x5a, b-5*sizeof(int)-n);
  }
  ((int*)next)[-3] = MAGIC2;
  ((int*)next)[-2] = MAGIC3;
  ((int*)next)[-1] = n;
  if (next != end) {
    ((int*)next)[0] = MAGIC5;
  }
  bytes_inuse += n;
  if (bytes_inuse > maximum_bytes_inuse) {
    maximum_bytes_inuse = bytes_inuse;
  }
  if (q == malloctrace) {
    fprintf (stderr, "Info: malloc(%d) returning %08x.\n", n, q);
  }
  return q;
}

static unsigned realloc_flag;

void
free (p)
    register char *p;
{
  register int n, i;
  register unsigned b;
  register char *q;

  if (p == NULL) {
    error (ENULL, "Error: free(NULL).");
    return;
  }

  if (((int*)p)[-1] != MAGIC1) {
    if (((int*)p)[-1] == MAGIC4) {
      if (realloc_flag) {
	error (EREFREE, "Error: realloc of free block at %08x.", p);
      }
      else {
	error (EFREE, "Error: freeing block at %08x again.", p);
      }
    }
    else {
      error (EMAGIC, "Error: check word at %08x preceding block at %08x\n\tbashed from %08x to %08x.",
	     (int*)p-1, p, MAGIC1, ((int*)p)[-1]);
    }
    return;
  }
  n = ((int*)p)[-2];
  if (n <= 0 || n >= MAXSIZE) {
    error (EMAGIC,  "Error: preceding size word at %08x for block at %08x bashed to %08x.",
	   (int*)p-2, p, n);
    return;
  }
  b = (n + 5*sizeof(int) + 7) & -8;
  q = p - 2*sizeof(int) + b;
  if (((int*)q)[-3] != MAGIC2) {
    error (EMAGIC, "Error: check word at %08x following block at %08x\n\tbashed from %08x to %08x.",
	   (int*)p-3, p, MAGIC2, ((int*)q)[-3]);
    return;
  }
  if (((int*)q)[-2] != MAGIC3) {
    error (EMAGIC, "Error: check word at %08x following block at %08x\n\tbashed from %08x to %08x.",
	   (int*)p-2, p, MAGIC3, ((int*)q)[-2]);
    return;
  }
  if (((int*)q)[-1] != n) {
    error (EMAGIC, "Error: trailer size word at %08x for block at %08x bashed to %08x.",
	   (int*)q-1, p, ((int*)q)[-1]);
    return;
  }
  q = p + n;
  for (i = b-5*sizeof(int)-n; i > 0; i -= 1) {
    if (*q++ != 0x5a) {
      error (EMAGIC, "Error: pad byte at %08x of block at %08x bashed from 5a to %02x.",
	     q-1, p, q[-1]);
      break;
    }
  }
  ((int*)p)[-1] = MAGIC4;
  memset (p, 0x5a, n);
  bytes_inuse -= n;
}

char *
realloc (p, n)
    register char *p;
    register int n;
{
  register char *q;
  register int o;

  q = malloc (n);
  if (p == NULL) {
    error (ENULL, "Error: realloc(NULL, %d).", n);
  }
  else {
    o = ((int*)p)[-2];
    if (o > n) o = n;
    memcpy (q, p, o);
    realloc_flag = 1;
    free (p);
    realloc_flag = 0;
  }
  return q;
}

void
malloc_status (level)
    register unsigned level;
{
  register unsigned i;
  register unsigned allocated_blocks, allocated_bytes;
  register unsigned free_blocks, free_bytes;
  register unsigned total_blocks, total_bytes;

  allocated_blocks = 0;
  allocated_bytes = 0;
  free_blocks = 0;
  free_bytes = 0;

  for (i = 0; i < narea; i += 1) {
    register char *p = area[i].address;
    register char *e = p + area[i].size;
    if (i == narea-1) {
      e = next;
    }
    while (p < e) {
      register int n;
      register unsigned b;
      register char *q;

      if (((int*)p)[1] == MAGIC5) break;

      if (((int*)p)[1] != MAGIC1 && ((int*)p)[1] != MAGIC4) {
	error (EMAGIC, "Error: check word at %08x preceding block at %08x\n\tbashed from %08x to %08x.",
	       (int*)p+1, (int*)p+2, MAGIC1, ((int*)p)[1]);
	return;
      }
      n = ((int*)p)[0];
      if (n <= 0 || n >= MAXSIZE || p + n > e) {
	error (EMAGIC,  "Error: preceding size word at %08x for block at %08x bashed to %08x.",
	       p, (int*)p+2, n);
	return;
      }
      b = (n + 5*sizeof(int) + 7) & -8;
      q = p + b;
      if (((int*)q)[-3] != MAGIC2) {
	error (EMAGIC, "Error: check word at %08x following block at %08x\n\tbashed from %08x to %08x.",
	       (int*)q-3, (int*)p+2, MAGIC2, ((int*)q)[-3]);
	return;
      }
      if (((int*)q)[-2] != MAGIC3) {
	error (EMAGIC, "Error: check word at %08x following block at %08x\n\tbashed from %08x to %08x.",
	       (int*)q-2, (int*)p+2, MAGIC3, ((int*)q)[-2]);
	return;
      }
      if (((int*)q)[-1] != n) {
	error (EMAGIC, "Error: trailer size word at %08x for block at %08x bashed to %08x.",
	       (int*)q-1, (int*)p+2, ((int*)q)[-1]);
	return;
      }
      q = p + 2*sizeof(int) + n;
      for (i = b-5*sizeof(int)-n; i > 0; i -= 1) {
	if (*q++ != 0x5a) {
	  error (EMAGIC, "Error: pad byte at %08x of block at %08x bashed from 5a to %02x.",
		 q-1, (int*)p+2, q[-1]);
	  break;
	}
      }
      if (((int*)p)[1] == MAGIC1) {
	allocated_blocks += 1;
	allocated_bytes += n;
	if (level > 0) {
	  error (EALLOCD, "Info: %5u-byte block at %08x still allocated.",
		 n, (int*)p+2);
	}
      }
      else {
	register char *r;
	free_blocks += 1;
	free_bytes += n;
	for (q = p + 2*sizeof(int), r = q + n; q != r; ) {
	  if (*q++ != 0x5a) {
	    error (EMAGIC, "Error: block at %08x written after free; byte bashed from 5a to %02x.", (int*)p+2, q[-1]);
	    break;
	  }
	}
      }
      p += b;
    }
  }
  total_bytes = free_bytes + allocated_bytes;
  total_blocks = free_blocks + allocated_blocks;
  fprintf (stderr, "Info: allocated: %10u bytes (%5.1f%%) in %7u blocks (%5.1f%%)\n",
	   allocated_bytes, (double)allocated_bytes/(double)total_bytes*100.0,
	   allocated_blocks, (double)allocated_blocks/(double)total_blocks*100.0);
  fprintf (stderr, "Info: free:      %10u bytes (%5.1f%%) in %7u blocks (%5.1f%%)\n",
	   free_bytes, (double)free_bytes/(double)total_bytes*100.0,
	   free_blocks, (double)free_blocks/(double)total_blocks*100.0);
  fprintf (stderr, "Info: %u maximum bytes in use (%.1f%%).\n",
	   maximum_bytes_inuse,
	   (double)maximum_bytes_inuse/(double)total_bytes*100.0);
}

char *
alloc_new (n)
    register int n;
{
  return malloc (n);
}

void
alloc_dispose (p)
    register char *p;
{
  free (p);
}

char *
xrealloc (n)
    register int n;
{
  return realloc (n);
}

char *
xmalloc (n)
    register int n;
{
  return malloc (n);
}

void
xfree (p)
    register char *p;
{
  free (p);
}

static unsigned errorcount[NERR];

/* VARARGS 2 */
static void
error (code, format, va_alist)
    int code;
    char *format;
    va_dcl
{
  va_list ap;
  errorcount[code] += 1;
  if (errorcount[code] <= 10) {
    va_start(ap);
    _doprnt(format, ap, stderr);
    putc('\n', stderr);
    if (errorcount[code] == 10) {
      fprintf (stderr, "...will squelch future such errors\n");
    }
  }
}
