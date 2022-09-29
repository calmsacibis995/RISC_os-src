#ident "$Header: Makefile,v 1.4.2.1 90/07/18 16:19:42 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../saio/M1000/MIPS/PSAIO_BO/Makefile
#

MACHINE=M1000
VERSION=MIPS
BINTYPE=BO

DEVSRC= device/mips_2300dev.c
DEVOBJ= mips_2300dev.o
LIB=	libpsa.a

CCOPTS= -O $(PROMOPTS)
ASOPTS= $(PROMOPTS)
ENDIAN= -EB
TYPE=$(VERSION) PROM OPT
DEPTH=../../../..

include ../../../../Mkinc.common_defines
include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R2300_ID)$(M1000_RELEASE)
include ../../../$(SAIOMKINC)

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
