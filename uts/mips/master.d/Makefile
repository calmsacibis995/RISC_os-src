#
# |-----------------------------------------------------------|
# | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restricted Rights Legend                         |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.27.1.8.1.2.1.2 90/11/07 15:06:46 beacker Exp $
#ident	"$Header: Makefile,v 1.27.1.8.1.2.1.2 90/11/07 15:06:46 beacker Exp $"


DEPTH=..
LABEL=master.d
include $(DEPTH)/kcommondefs

#
# By default, get mboot from /etc.  If a special mboot is required,
# set the MBOOT macro on the make command line.
#
MBOOT=$(ETCDIR)/mboot

MASTERD	=./master.d
SYSGEN	=$(MASTERD)/sysgen.sh
SYSTEM_I=$(MASTERD)/system.gen
SYSTEM_O=$(MASTERD)/system

MASTERS = \
	bsd cache_std cache_r6000 cdsio clone common_scsi		\
	console cp c8 decnet disp dkvj dkip du fl gentty ggetchar	\
	graf graf_dummy gri_dev hd146818 if_en if_enp	\
	if_egl if_la if_lance if_lacomm					\
	ikc io iop iop_clock ipc i2000_console i2000_mem		\
	kbd klog leds ll lp machaddr mem mirror monitor mk48t02 mouse	\
	mouse_dc msg nm nvram prf prom_entries_i2000 prom_entries_r6000	\
	prom_entries_std ptc pts rd				\
	r2300mem r2400_ints r2400_mem r3200mem				\
	r6000_console r6000_ints r6000mem				\
	sabledsk sable_console scsi sd sduart sem shm socket		\
	span spc spc_poll spl_i2000 spl_r6000 spl_std split ssablecons	\
	stripe stty_ld stubs tlb_std tlb_r6000 todc ts tth uart udc	\
	vdisk vme vme_r6000 keymap_r3030 graf_r3030 spl_r3030		\
	keymap_i2000 wbflush_r2400 wbflush_r2000 wbflush_r3000		\
	vme_rb3125 if_la_rb3125						\
	wbflush_r6000 xm xyl712

include $(BOOTAREA)/master_$(X)_Makedepend

default: $(BOOTAREA)/master.$(X).o

$(BOOTAREA)/master.$(X).o:
	$(CCF) -I. -c -o $@ $(BOOTAREA)/master.$(X).c

$(BOOTAREA)/master.$(X).c: $(MASTERS) kernel.$(X) sysgen.$(X) Makefile kernel_common
	-rm -f kernel; ln -s kernel.$(X) kernel
	$(SHELL) sysgen.sh	-i sysgen.$(X) \
				-l $(MBOOT) \
				-m . \
				-s system.$(X) \
				-b $(BOOTAREA) \
				$(CONFIG)
	-rm -f kernel $(BOOTAREA)/objlist.$(X)
	mv $(BOOTAREA)/objlist $(BOOTAREA)/objlist.$(X)
	-rm -f $@ ; mv master.c $@
	cp /dev/null $(MDEPENDTMP);\
	INCL_TMP=`echo "$(DESTROOT)/usr/include" | sed 's,//*,/,g'`; \
	$(CCF) -I. -M $@ | \
	awk -f $(DEPTH)/mkdepend.awk FRC="$$(FRC)" \
	    INC="$$INCL_TMP" INCDIR='$$(DESTROOT)/usr/include' \
	    - | sed -e 's"^[a-zA-Z0-9]"$(BOOTAREA)/&"' >> $(MDEPENDTMP) ;\
	mv $(MDEPENDTMP) $(BOOTAREA)/master_$(X)_Makedepend

$(BOOTAREA)/kernel_common: kernel_common
	rm -f $@ ; cp kernel_common $@
