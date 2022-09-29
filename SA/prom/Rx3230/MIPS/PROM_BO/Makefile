#ident "$Header: Makefile,v 1.8.2.1 90/07/18 15:49:21 huang Exp $"
#	%Q%	%I%	%M%
#
# ..../prom/Rx3230/MIPS/PROM_BO/Makefile for prom monitor
#

MACHINE=Rx3230
VERSION=MIPS
BINTYPE=BO

DEVSRC= saio/device/mips_3030dev.c
DEVOBJ= mips_3030dev.o
LIBPSA=../../../../saio/$(MACHINE)/$(VERSION)/PSAIO_$(BINTYPE)/libpsa.a
LIBPON= ../../../../diags/pon/$(VERSION)_3030_PON_$(BINTYPE)/libpon.a

CCCOPTS	= -DR3030
CASOPTS	= -DR3030
CONVFLAGS = -i

CCOPTS=	-O
ASOPTS=
LDOPTS=
ENDIAN=	-EB
PROMOPTS= -G 0
LDPROMOPTS= -G 0
TYPE=$(VERSION) OPT
DEPTH=../../../..

include $(DEPTH)/Mkinc.release
RELEASE = $(MAJOR_REV_ID).$(R3030_ID)$(Rx3230_RELEASE)
include ../../../Mkinc.prom

# DO NOT DELETE THIS LINE (or the line after this one)

include Makedepend

FRC:
