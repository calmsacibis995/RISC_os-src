#ident	"$Header: Makefile,v 1.10.6.10.1.2.1.2 90/10/16 09:57:52 beacker Exp $"

DEPTH=..
LABEL=io
include $(DEPTH)/kcommondefs

LIBNAME=$(BOOTAREA)/io.a

BSD_FLAGS=-I$(BOOTAREA)/ml

LIBCSRCS	=gba.c dma.c sgset.c stream.c misc.c ss.c \
	teCommands.c teDevColor8.c teDev_r2030.c teDevc8_r3030.c teDevice.c \
	teFont.c teParse.c tePutChar.c

I2000_CSRCS = fl.c kbd.c nvram.c graf.c iop.c sd.c uart.c \
	iop_clock.c lp.c i2000_console.c i2000_mem.c \
	ggetchar.c mouse.c general_tod.c mouse_dc.c keymap_i2000.c

CSRCS=mem.c rd.c gentty.c dkip.c ts.c xm.c shm.c ipc.c dkvj.c \
	monitor.c clone.c prf.c sem.c msg.c sduart.c spty.c stty_ld.c  \
	r2400_ints.c r2400_mem.c r6000_ints.c r6000mem.c r3030_ints.c \
	xyl712.c cdsio.c ikc.c cp.c c8.c tth.c sabledsk.c ssablecons.c \
	scsi.c spc.c spc_poll.c udc.c vme.c vme_rb3125.c vme_r6000.c \
	hd146818.c mk48t02.c r3200mem.c r2300mem.c console.c r6000_console.c \
	sable_console.c graf_dummy.c todc.c todc_r6000.c todc_r3030.c \
	leds.c klog.c common_scsi.c slip.c scc.c cons3030.c r3030_mem.c \
	mono.c fl8272.c r3030_mono.c r3030_font.c bitmap.c rb3125_console.c \
	r3030_scsi.c asc_poll.c asc.c pkbd.c graf_r3030.c keymap_r3030.c \
	rb3125_scsi.c timod.c tirdwr.c

SSRCS=wbflush_r2000.s wbflush_r2400.s wbflush_r3000.s wbflush_r6000.s	\
	prom_entries_std.s prom_entries_i2000.s	prom_entries_r6000.s	\
	cache_std.s cache_r6000.s			\
	spl_std.s spl_i2000.s spl_r6000.s spl_r3030.s	\
	tlb_std.s tlb_r6000.s
PTSRCS=spty.c

SRCS=$(LIBCSRCS) $(PTSRCS)

DRVSRCS=$(CSRCS) $(I2000_CSRCS) $(SSRCS)

PTLIBS= $(BOOTAREA)/pts.a $(BOOTAREA)/ptc.a
#
# Targets/Rules
#
default:	all

rules:	$(MAKERULES)

$(MAKERULES):	$(MAKERULESMARK)

$(MAKERULESMARK): Makefile
	touch $(MAKERULESMARK) ; \
	rm -rf $(MKRULESTMP) ; \
	$(MKRULESDRV) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' OBJS $(CSRCS) $(I2000_CSRCS) $(SSRCS) \
		> $(MKRULESTMP) && \
	$(MKRULES) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' LIBOBJS $(LIBCSRCS) \
		>> $(MKRULESTMP) && \
	$(MKRULES) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' PTOBJS $(PTSRCS) \
		>> $(MKRULESTMP) && \
	mv $(MKRULESTMP) $(MAKERULES)

include $(MAKEDEPEND)

include $(MAKERULES)

DRVOBJS= $(OBJS) $(PTLIBS)

all: $(DRVMARK) $(LIBMARK)

#
# When adding drivers on sourceless systems there may be .o's 
# with no .c's.
# 
# This "$(CSRCS):" quiets make.  The $(MAKERULES) rules build .o's from .c's
# if the .c's exist and have been changed.  They complain if neither the .o
# nor the .c exists.
#
# This a hack.
# 
$(CSRCS):

$(I2000_CSRCS):

$(SSRCS):

$(LIBCSRCS):

$(BOOTAREA)/ptc.a:	$(PTOBJS)
	rm -f $@; $(AR) qsc $@ $(PTOBJS)

$(BOOTAREA)/pts.a:	$(BOOTAREA)/ptc.a
	rm -f $@; ln $? $@

include $(KCOMMONRULES)
