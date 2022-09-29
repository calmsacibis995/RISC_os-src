#ident "$Header: Makefile,v 1.7.2.1 90/07/18 16:24:24 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/Rx3230/MIPS/PSAIO_BO/Makefile
#

MACHINE=Rx3230
VERSION=MIPS
BINTYPE=BO

DEVSRC= device/mips_3030dev.c
DEVOBJ= mips_3030dev.o
LIB=	libpsa.a

CCCOPTS	= -DR3030
CASOPTS	= -DR3030

CCOPTS= -O $(PROMOPTS)
ASOPTS= $(PROMOPTS)
ENDIAN= -EB
TYPE=$(VERSION) PROM OPT
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R3030_ID)$(Rx3230_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
