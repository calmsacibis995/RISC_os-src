#ident "$Header: Makefile,v 1.5.2.1 90/07/18 16:21:21 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/M120/MIPS/PSAIO_BO/Makefile
#

MACHINE=M120
VERSION=MIPS
BINTYPE=BO

DEVSRC= device/mips_2400dev.c
DEVOBJ= mips_2400dev.o
LIB=	libpsa.a

CCOPTS= -O $(PROMOPTS)
ASOPTS= $(PROMOPTS)
ENDIAN= -EB
MACH_DEFS= -DINTREPID
TYPE=$(VERSION) PROM OPT
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R2400_ID)$(M120_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
