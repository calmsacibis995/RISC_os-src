#
# |-----------------------------------------------------------|
# | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restricted Rights Legend                         |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.3.2.3 90/05/07 20:28:43 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=_setjmp.o abort.o abs.o atod.o atof.o bcmp.o bcopy.o bzero.o \
	disktab.o dtoa.o dwmultu.o ecvt.o execute_br.o \
	fabs.o ffs.o fp_class.o fp_control.o fp_instr.o fpi_sigfpe.o \
	frexp.o index.o insque.o ldexp.o loadstore.o modf.o remque.o \
	rindex.o setjmperr.o strcat.o strcmp.o \
	strcpy.o strlen.o tenscale.o emulate_br.o setjmp.o \
	sigbus.o

SRCS	=_setjmp.s abort.s abs.s atod.s atof.c bcmp.s bcopy.s bzero.s \
	disktab.c dtoa.s dwmultu.s ecvt.c execute_br.s \
	fabs.s ffs.s fp_class.s fp_control.s fp_instr.s fpi_sigfpe.c \
	frexp.s index.s insque.s ldexp.s loadstore.s modf.s remque.s \
	rindex.s setjmperr.c strcat.s strcmp.s \
	strcpy.s strlen.c tenscale.s emulate_br.c setjmp.s \
	sigbus.c

HDRS	=

WHOAMI	=libc_gen.mips.a
INTEGDIR=../libc

all install: $(INTEGDIR)/$(WHOAMI)

$(INTEGDIR)/$(WHOAMI): $(WHOAMI)
	-rm -f $(INTEGDIR)/$(WHOAMI)
	cp $(WHOAMI) $(INTEGDIR)

$(WHOAMI): $(OBJS)
	-rm -f $(WHOAMI)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)

.s.o:
	$(CC) $(CFLAGS) -c $<

fpi_sigfpe.o: fpi_sigfpe.c
	$(CC) $(CFLAGS) -float -c $<
