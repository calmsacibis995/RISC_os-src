#ident	"$Header: Makefile,v 1.20.1.5 90/05/10 19:01:02 wje Exp $"
#
# Notes: Makefile for the assembly language
#	generate assembler definitions from the header files,
#	cat all the .s and assemble
#

#
# Common Definitions
#
DEPTH=..
LABEL=ml
include $(DEPTH)/kcommondefs

#
# Compile Time Options
#

#
# Local Definitions
#
# the following is a problem with the chip, the flag says pass -mfc0 to pass
# b of the assembler (-W)
# CHIPBUG=-Wb,-mfc0
CHIPBUG=


SFILES = locore.s process.s usercopy.s searchdir.s softfp.s nofphw.s \
	emu_instr.s coproc_ctl.s fp_intr.s hwcopy.s

CSRCS	=genassym.c $(BOOTAREA)/assym.s $(BOOTAREA)/clocore.s

DRVSRCS= $(CSRCS)

# add this to commondefs:
HCC	= COMP_HOST_ROOT=/ COMP_TARGET_ROOT=/ /usr/bin/cc
HCCLIB	=

#
# Targets/Rules
#

default:	all

rules:	$(MAKERULESMARK)

$(MAKERULESMARK):
	touch $(MAKERULESMARK)

all:	$(BOOTAREA)/locore.o

include $(MAKEDEPEND)

ml-locore-o: 
	$(MAKE) $(MAKEARGS) $(BOOTAREA)/locore.o

ml-assym-s:
	$(MAKE) $(MAKEARGS) $(BOOTAREA)/assym.s

$(BOOTAREA)/locore.o:$(BOOTAREA)/clocore.o
	rm -f $(BOOTAREA)/locore.o
	ln $(BOOTAREA)/clocore.o $(BOOTAREA)/locore.o

$(BOOTAREA)/clocore.s:$(BOOTAREA)/assym.s $(SFILES) $(FRC)
	cat $(BOOTAREA)/assym.s $(SFILES) > $@

$(BOOTAREA)/clocore.o:$(BOOTAREA)/clocore.s
	$(CC) $(CFLAGS) $(CHIPBUG) -o $@ -c $(BOOTAREA)/clocore.s

$(BOOTAREA)/assym.s:$(BOOTAREA)/genassym
	$? > $@
	[ -d $(BOOTAREA)/ml ] || mkdir -p $(BOOTAREA)/ml ; \
	[ -r $(BOOTAREA)/ml/assym.s ] || \
		ln -s ../assym.s $(BOOTAREA)/ml/assym.s

# we do this in two steps so that we have genassym.o so the dependencies work
$(BOOTAREA)/genassym.o:genassym.c
	$(HCC) -DMIPSEB $(CFLAGS) -Wb,-r3000 -I$(DEPTH) -o $@ -c genassym.c

$(BOOTAREA)/genassym:$(BOOTAREA)/genassym.o
	$(HCC) -DMIPSEB $(CFLAGS) -I$(DEPTH) $(BOOTAREA)/genassym.o \
	$(HCCLIB) -o $@ 

include $(KCOMMONRULES)
