#ident "$Header: Makefile,v 1.4.2.1 90/07/18 16:22:54 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/M2000/MIPS/PSAIO_BO/Makefile
#

MACHINE=M1000
VERSION=MIPS
BINTYPE=BO

DEVSRC= device/mips_3200dev.c
DEVOBJ= mips_3200dev.o
LIB=	libpsa.a

CCCOPTS= -DR3200
CASOPTS= -DR3200

CCOPTS= -O $(PROMOPTS)
ASOPTS= $(PROMOPTS)
ENDIAN= -EB
TYPE=$(VERSION) PROM OPT
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R3200_ID)$(M2000_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
