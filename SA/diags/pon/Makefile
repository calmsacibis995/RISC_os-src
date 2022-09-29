#ident "$Header: Makefile,v 1.20.1.1 90/07/18 14:28:03 huang Exp $"

include ../../Mkinc.common_defines

MKARGS=		MAKE=$(MAKE)

BINDIRS=	MIPS_PON_BO MIPS_PON_BG \
		SABLE_PON_BO SABLE_PON_BG SABLE_PON_LO SABLE_PON_LG\
		MIPS_6000_PON_BO SABLE_6000_PON_BO \
		MIPS_3030_PON_BG MIPS_3030_PON_BO \
		SABLE_3030_PON_BG SABLE_3030_PON_BO \
		MIPS_3125_PON_BG MIPS_3125_PON_BO \
		SABLE_3125_PON_BG SABLE_3125_PON_BO 
 
BLDDIRS=	MIPS_3125_PON_BO

LIST=		Makefile \
		Mkinc.pon \
		Mkinc.pon.6000 \
		Mkinc.pon.3030 \
		Mkinc.pon.3125 \
		MIPS_PON/Makefile \
		pon.h \
		dma.h \
		kbd.h \
		ncr53c94.h \
		pon_allexc.s \
		pon_atreg.c \
		pon_banner.s \
		pon_buzzer.s \
		pon_cache1.s \
		pon_cache2.s \
		pon_cache3.s \
		pon_cache4.s \
		pon_chain1.c \
		pon_chain2.c \
		pon_dblk_refill.s \
		pon_iblk_refill.s \
		pon_duart.s \
		pon_duarts.c \
		pon_enetprom.s \
		pon_fp1.s \
		pon_fp2.s \
		pon_idprom.s \
		pon_imr.c \
		pon_kbd.c \
		pon_lance.c \
		pon_leds.s \
		pon_mem.s \
		pon_ncr94.c \
		pon_nvram.c \
		pon_parity.c \
		pon_scr.c \
		pon_rambo.c \
		pon_scsi.c \
		pon_subr.s \
		pon_timer.c \
		pon_timers.c \
		pon_tlb.s \
		pon_udcslave.c \
		pon_vm.c \
		pon_wb.s \
		pon_ecc.c \
		pon_vme.c

default:	all

all depend remake proms:
		@-if [ $@ = remake ]; \
		then \
			$(MAKE) tags $(MAKEARGS); \
		fi
		@for i in $(BLDDIRS); \
		do \
			echo "	XX Make $@ for pon/$$i XXXXXX"; \
			(cd $$i; $(MAKE) $@ $(MAKEARGS)); \
		done
		@echo diags/pon make $@ done

tags:		FRC
		-rm -f tags
		P=`pwd`; D=`basename $$P`; ctags ../$$D/*.[csh] >/dev/null 2>&1

clean clean_proms:
		@for i in $(BINDIRS); \
		do \
			echo "	XX Make $@ for pon/$$i XXXXXX"; \
			(cd $$i; $(MAKE) clean $(MAKEARGS)); \
		done
		@echo diags/pon make $@ done

clobber clobber_proms:
		@-if [ $@ = clobber ]; \
		then \
			rm -f err; \
		fi
		@for i in $(BINDIRS); \
		do \
			echo "	XX Make $@ for pon/$$i XXXXXX"; \
			(cd $$i; $(MAKE) clobber $(MAKEARGS)); \
		done
		@echo diags/pon make $@ done

print:
		$(IF_SYSV) \
			pr $(LIST) | lp; \
		$(ENDIF)
		$(IF_BSD) \
			lpr -p -J diags -C pon $(LIST); \
		$(ENDIF)
		@echo diags/pon make print done

FRC:
