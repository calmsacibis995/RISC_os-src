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
# $Header: Makefile,v 1.7.2.3 90/05/10 01:23:00 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=$(GNUM) -systype sysv $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(GNUM) -systype sysv $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=abs.o atod.o bcmp.o cuexit.o dwmultu.o emulate_br.o execute_br.o \
	fabs.o fp_class.o fp_instr.o fpi_sigfpe.o frexp.o ldexp.o \
	loadstore.o memcmp.o memcpy.o memset.o modf.o psignal.o setjmp.o \
	sigbus.o siglist.o strcat.o strcmp.o strcpy.o strlen.o tenscale.o \
	fp_control.o dtoa.o machine_info.o

SRCS	=abs.s atod.s bcmp.s cuexit.c dwmultu.s emulate_br.c execute_br.s \
	fabs.s fp_class.s fp_instr.s fpi_sigfpe.c frexp.s ldexp.s \
	loadstore.s memcmp.s memcpy.s memset.s modf.s psignal.c setjmp.s \
	sigbus.c siglist.c strcat.s strcmp.s strcpy.s strlen.s tenscale.s \
	fp_control.s dtoa.s machine_info.c

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

fpi_sigfpe.o: fpi_sigfpe.c
	$(CC) $(CFLAGS) -float -c fpi_sigfpe.c
