#ident "$Header: Makefile,v 1.6 89/11/08 09:12:00 huang Exp $"
#	%Q%	%I%	%M%
#
# Copyright 1985 by MIPS Computer Systems, Inc.
#
# dbgmon Makefile
#
VERSION=MIPS
ASOPTS= -g
CCOPTS=	-g
LDOPTS=
ENDIAN= -EB
TYPE=$(VERSION) DBG

include ../../Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(PI_RELEASE)
include ../Mkinc.dbgmon

# DO NOT DELETE THIS LINE (or the line after this one)

csu.mips.o: ../csu.mips.s ../../include/machine/regdef.h
csu.mips.o: ../../include/machine/cpu.h ../../include/machine/cpu_board.h
csu.mips.o: ../../include/machine/asm.h ../../include/prom/entrypt.h
csu.mips.o: ../../include/saio/saio.h ../../include/saio/saioctl.h
csu.mips.o: ../../include/dbgmon/dbgmon.h
faultasm.o: ../faultasm.s ../../include/sys/signal.h
faultasm.o: ../../include/machine/regdef.h ../../include/machine/cpu.h
faultasm.o: ../../include/machine/cpu_board.h ../../include/machine/asm.h
faultasm.o: ../../include/machine/bc.h ../../include/machine/ctlspace.h
faultasm.o: ../../include/saio/saioctl.h ../../include/dbgmon/dbgmon.h
promlink.o: ../promlink.s ../../include/prom/entrypt.h
promlink.o: ../../include/machine/regdef.h ../../include/machine/asm.h
promlink.o: ../../include/machine/cpu.h
tlb.o: ../tlb.s ../../include/machine/regdef.h ../../include/machine/cpu.h
tlb.o: ../../include/machine/cpu_board.h ../../include/machine/asm.h
brkpt.o: ../brkpt.c ../../include/machine/cpu.h ../../include/saio/setjmp.h
brkpt.o: ../../include/saio/parser.h ../../include/saio/saioctl.h
brkpt.o: ../../include/sys/dkio.h ../../include/sys/gen_ioctl.h
brkpt.o: ../../include/dbgmon/dbgmon.h ../../include/dbgmon/inst.h
commands.o: ../commands.c ../../include/sys/types.h ../../include/sys/file.h
commands.o: ../../include/machine/cpu_board.h ../../include/machine/cpu.h
commands.o: ../../include/saio/stringlist.h ../../include/saio/parser.h
commands.o: ../../include/saio/saioctl.h ../../include/sys/dkio.h
commands.o: ../../include/sys/gen_ioctl.h ../../include/prom/entrypt.h
commands.o: ../../include/dbgmon/dbgmon.h ../../include/saio/ctype.h
ctype.o: ../ctype.c ../../include/saio/ctype.h
dbgmon.o: ../dbgmon.c ../../include/sys/types.h ../../include/sys/file.h
dbgmon.o: ../../include/saio/setjmp.h ../../include/saio/parser.h
dbgmon.o: ../../include/saio/saioctl.h ../../include/sys/dkio.h
dbgmon.o: ../../include/sys/gen_ioctl.h ../../include/prom/entrypt.h
dbgmon.o: ../../include/dbgmon/dbgmon.h ../../include/machine/cpu_board.h
disMips.o: ../disMips.c ../../include/dbgmon/dbgmon.h
disMips.o: ../../include/dbgmon/inst.h ../../include/saio/setjmp.h
disMips.o: ../../include/machine/cpu_board.h
fault.o: ../fault.c ../../include/sys/param.h ../../include/machine/param.h
fault.o: ../../include/signal.h ../../include/sys/types.h
fault.o: ../../include/machine/cpu.h ../../include/machine/cpu_board.h
fault.o: ../../include/saio/saioctl.h ../../include/sys/dkio.h
fault.o: ../../include/sys/gen_ioctl.h
machdep.o: ../machdep.c ../../include/machine/cpu.h
machdep.o: ../../include/machine/cpu_board.h ../../include/prom/entrypt.h
machdep.o: ../../include/saio/saioctl.h ../../include/sys/dkio.h
machdep.o: ../../include/sys/gen_ioctl.h ../../include/dbgmon/dbgmon.h
rdebug.o: ../rdebug.c ../../include/sys/errno.h ../../include/sys/types.h
rdebug.o: ../../include/sys/file.h ../../include/sys/wait.h
rdebug.o: ../../include/sys/signal.h ../../include/sys/ptrace.h
rdebug.o: ../../include/machine/cpu.h ../../include/saio/setjmp.h
rdebug.o: ../../include/saio/protoio.h ../../include/saio/protocol.h
rdebug.o: ../../include/saio/stringlist.h ../../include/saio/saioctl.h
rdebug.o: ../../include/sys/dkio.h ../../include/sys/gen_ioctl.h
rdebug.o: ../../include/saio/debug.h ../../include/prom/entrypt.h
rdebug.o: ../../include/dbgmon/dbgmon.h
regdesc.o: ../regdesc.c ../../include/machine/cpu.h
regdesc.o: ../../include/saio/saioctl.h ../../include/sys/dkio.h
regdesc.o: ../../include/sys/gen_ioctl.h
calc.o: ../calc.c ../../include/dbgmon/calc.h
