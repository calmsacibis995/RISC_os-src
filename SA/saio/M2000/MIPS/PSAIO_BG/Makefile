#ident "$Header: Makefile,v 1.4.2.1 90/07/18 16:22:41 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/M2000/MIPS/PSAIO_BG/Makefile
#

MACHINE=M2000
VERSION=MIPS
BINTYPE=BG

DEVSRC= device/mips_3200dev.c
DEVOBJ= mips_3200dev.o
LIB=	libpsa.a

CCCOPTS= -DR3200
CASOPTS= -DR3200

CCOPTS= -g $(PROMOPTS)
ASOPTS= -g $(PROMOPTS)
ENDIAN= -EB
TYPE=$(VERSION) PROM DBG
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R3200_ID)$(M2000_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
