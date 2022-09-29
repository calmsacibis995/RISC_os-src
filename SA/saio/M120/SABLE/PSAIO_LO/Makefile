#ident "$Header: Makefile,v 1.5.2.1 90/07/18 16:22:12 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/M120/SABLE/PSAIO_LO/Makefile
#

MACHINE=M120
VERSION=SABLE
BINTYPE=LO

DEVSRC= device/sable_2400dev.c
DEVOBJ= sable_2400dev.o
LIB=	libpsa.a

CCOPTS= -O $(PROMOPTS)
ASOPTS= $(PROMOPTS)
ENDIAN= -EL
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
