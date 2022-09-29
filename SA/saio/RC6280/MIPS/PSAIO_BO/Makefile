#ident "$Header: Makefile,v 1.5.2.1 90/07/18 16:29:58 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/RC6280/MIPS/PSAIO_BO/Makefile
#

MACHINE=RC6280
VERSION=MIPS
BINTYPE=BO

DEVSRC= device/mips_6300dev.c
DEVOBJ= mips_6300dev.o
LIB=	libpsa.a

CCOPTS= -O $(PROMOPTS)
ASOPTS= $(PROMOPTS)
ENDIAN= -EB
TYPE=$(VERSION) PROM OPT
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R6300_ID)$(RC6280_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
