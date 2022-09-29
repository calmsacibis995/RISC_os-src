#ident "$Header: Makefile,v 1.1.6.2 90/07/26 14:07:02 huang Exp $"

#
# Makefile for 3030 pon diagnostics 
#
VERSION=SABLE
#
# pon_vm.c contains setjmp and longjmp calls so we must compile
# it without the global optimization of O2.  It's easier to just
# compile the entire library -01.
#
CCOPTS=	-g -DR3030
ASOPTS= -g -DR3030 -w -Wb,-f1
ENDIAN=	-EB
TYPE=$(VERSION) OPT
DEPTH=../../..

include $(DEPTH)/Mkinc.release
RELEASE= $(MAJOR_REV_ID).$(PI_RELEASE)
include ../Mkinc.pon.3030

# DO NOT DELETE THIS LINE (or the line after this one)

pon_asm_ecc.o: ../pon_asm_ecc.s ../../../include/machine/regdef.h
pon_asm_ecc.o: ../../../include/machine/cpu.h
pon_asm_ecc.o: ../../../include/machine/cpu_board.h
pon_asm_ecc.o: ../../../include/machine/asm.h
pon_asm_ecc.o: ../../../include/machine/mem_board.h ../pon_ecc.h
pon_cache1.o: ../pon_cache1.s ../../../include/machine/asm.h
pon_cache1.o: ../../../include/machine/regdef.h
pon_cache1.o: ../../../include/machine/mach_ops.h
pon_cache1.o: ../../../include/machine/cp0.h ../../../include/machine/cpu.h
pon_cache1.o: ../pon.h
pon_cache2.o: ../pon_cache2.s ../../../include/machine/asm.h
pon_cache2.o: ../../../include/machine/regdef.h
pon_cache2.o: ../../../include/machine/mach_ops.h
pon_cache2.o: ../../../include/machine/cp0.h ../../../include/machine/cpu.h
pon_cache2.o: ../pon.h ../../../include/machine/cpu_board.h
pon_cache2.o: ../../../include/machine/cpu.h
pon_cache3.o: ../pon_cache3.s ../../../include/machine/asm.h
pon_cache3.o: ../../../include/machine/regdef.h
pon_cache3.o: ../../../include/machine/mach_ops.h
pon_cache3.o: ../../../include/machine/cp0.h ../../../include/machine/cpu.h
pon_cache3.o: ../pon.h
pon_cache4.o: ../pon_cache4.s ../../../include/machine/asm.h
pon_cache4.o: ../../../include/machine/regdef.h
pon_cache4.o: ../../../include/machine/mach_ops.h
pon_cache4.o: ../../../include/machine/cp0.h ../../../include/machine/cpu.h
pon_cache4.o: ../pon.h
pon_subr_2000.o: ../pon_subr_2000.s ../../../include/machine/asm.h
pon_subr_2000.o: ../../../include/machine/regdef.h
pon_subr_2000.o: ../../../include/machine/mach_ops.h
pon_subr_2000.o: ../../../include/machine/cp0.h ../../../include/machine/cpu.h
pon_tlb.o: ../pon_tlb.s ../../../include/machine/cpu.h
pon_tlb.o: ../../../include/machine/cpu_board.h ../../../include/machine/asm.h
pon_tlb.o: ../../../include/machine/regdef.h ../../../include/machine/param.h
pon_wb.o: ../pon_wb.s ../../../include/mips/asm.h
pon_wb.o: ../../../include/mips/regdef.h ../../../include/mips/mach_ops.h
pon_wb.o: ../../../include/mips/cp0.h ../../../include/mips/cpu.h
pon_wb.o: ../../../include/mips/cpu_board.h ../pon.h
pon_allexc.o: ../pon_allexc.s ../../../include/machine/asm.h
pon_allexc.o: ../../../include/machine/dregdef.h
pon_allexc.o: ../../../include/machine/mach_ops.h
pon_allexc.o: ../../../include/machine/cp0.h ../../../include/machine/cpu.h
pon_allexc.o: ../../../include/machine/orgmacs.h
pon_allexc.o: ../../../include/machine/cpu_board.h
pon_allexc.o: ../../../include/machine/cpu.h ../pon.h
pon_banner.o: ../pon_banner.s ../../../include/machine/asm.h
pon_banner.o: ../../../include/machine/regdef.h
pon_dblk_refill.o: ../pon_dblk_refill.s ../../../include/machine/asm.h
pon_dblk_refill.o: ../../../include/machine/regdef.h
pon_dblk_refill.o: ../../../include/machine/cpu.h
pon_dblk_refill.o: ../../../include/machine/cpu_board.h ../pon.h
pon_duart.o: ../pon_duart.s ../../../include/machine/cp0.h
pon_duart.o: ../../../include/machine/cpu.h
pon_duart.o: ../../../include/machine/cpu_board.h
pon_duart.o: ../../../include/machine/duart.h ../../../include/machine/asm.h
pon_duart.o: ../../../include/machine/regdef.h
pon_duart.o: ../../../include/machine/hd146818.h
pon_duart.o: ../../../include/machine/mk48t02.h ../pon.h
pon_duart.o: ../../../include/prom/prom.h
pon_enetprom.o: ../pon_enetprom.s ../../../include/mips/asm.h
pon_enetprom.o: ../../../include/mips/regdef.h ../../../include/mips/cpu.h
pon_enetprom.o: ../../../include/mips/cpu_board.h ../pon.h
pon_fp1.o: ../pon_fp1.s ../../../include/machine/dregdef.h
pon_fp1.o: ../../../include/machine/standard.h
pon_fp1.o: ../../../include/machine/cpu_board.h ../../../include/machine/cp0.h
pon_fp1.o: ../../../include/machine/cpu.h ../../../include/machine/delaymacs.h
pon_fp1.o: ../../../include/machine/asm.h ../pon.h
pon_fp2.o: ../pon_fp2.s ../../../include/machine/dregdef.h
pon_fp2.o: ../../../include/machine/asm.h ../../../include/machine/standard.h
pon_fp2.o: ../../../include/machine/cpu_board.h ../../../include/machine/cp0.h
pon_fp2.o: ../../../include/machine/cpu.h ../../../include/machine/delaymacs.h
pon_fp2.o: ../pon.h
pon_iblk_refill.o: ../pon_iblk_refill.s ../../../include/machine/asm.h
pon_iblk_refill.o: ../../../include/machine/regdef.h
pon_iblk_refill.o: ../../../include/machine/cpu.h
pon_iblk_refill.o: ../../../include/machine/cpu_board.h ../pon.h
pon_idprom.o: ../pon_idprom.s ../../../include/mips/asm.h
pon_idprom.o: ../../../include/mips/regdef.h ../../../include/mips/cpu.h
pon_idprom.o: ../../../include/mips/cpu_board.h ../pon.h
pon_leds.o: ../pon_leds.s ../../../include/mips/dregdef.h
pon_leds.o: ../../../include/mips/cpu.h ../../../include/mips/cpu_board.h
pon_leds.o: ../../../include/mips/standard.h ../../../include/mips/delaymacs.h
pon_leds.o: ../../../include/mips/asm.h ../pon.h
pon_mem.o: ../pon_mem.s ../../../include/machine/regdef.h
pon_mem.o: ../../../include/machine/asm.h ../../../include/machine/bc.h
pon_mem.o: ../../../include/machine/cpu_board.h ../../../include/machine/cp0.h
pon_mem.o: ../../../include/machine/cpu.h ../../../include/machine/param.h
pon_mem.o: ../pon.h ../../../include/prom/prom.h
pon_lomem.o: ../pon_lomem.s ../../../include/machine/regdef.h
pon_lomem.o: ../../../include/machine/asm.h
pon_lomem.o: ../../../include/machine/cpu_board.h
pon_lomem.o: ../../../include/machine/cp0.h ../../../include/machine/cpu.h
pon_lomem.o: ../../../include/machine/param.h ../pon.h
pon_lomem.o: ../../../include/prom/prom.h
pon_subr.o: ../pon_subr.s ../../../include/machine/asm.h
pon_subr.o: ../../../include/machine/bc.h ../../../include/machine/regdef.h
pon_subr.o: ../../../include/machine/mach_ops.h
pon_subr.o: ../../../include/machine/duart.h ../../../include/machine/cp0.h
pon_subr.o: ../../../include/machine/cpu.h ../../../include/machine/cpu_board.h
pon_subr.o: ../../../include/machine/mem_board.h
pon_subr.o: ../../../include/mipsvme/vmereg.h
pon_subr.o: ../../../include/machine/mk48t02.h
pon_subr.o: ../../../include/machine/hd146818.h ../pon.h
pon_subr.o: ../../../include/prom/prom.h
pon_ecc.o: ../pon_ecc.c ../../../include/sys/types.h
pon_ecc.o: ../../../include/machine/cpu.h ../../../include/machine/mem_board.h
pon_ecc.o: ../../../include/machine/cpu_board.h
pon_ecc.o: ../../../include/machine/r3250reg.h
pon_ecc.o: ../../../include/mipsvme/vmereg.h ../../../include/saio/setjmp.h
pon_ecc.o: ../pon_ecc.h ../pon.h
pon_lance.o: ../pon_lance.c ../../../include/sys/types.h
pon_lance.o: ../../../include/machine/cpu.h
pon_lance.o: ../../../include/machine/cpu_board.h
pon_lance.o: ../../../include/machine/asm.h ../pon.h
pon_lance.o: ../../../include/prom/prom.h
pon_parity.o: ../pon_parity.c ../../../include/sys/types.h
pon_parity.o: ../../../include/machine/cpu.h
pon_parity.o: ../../../include/machine/cpu_board.h
pon_parity.o: ../../../include/saio/setjmp.h ../pon.h
pon_scr.o: ../pon_scr.c ../../../include/sys/types.h
pon_scr.o: ../../../include/machine/cpu.h ../../../include/machine/cpu_board.h
pon_scr.o: ../pon.h
pon_scsi.o: ../pon_scsi.c ../../../include/sys/param.h
pon_scsi.o: ../../../include/machine/param.h ../../../include/signal.h
pon_scsi.o: ../../../include/sys/types.h ../../../include/mips/cpu.h
pon_scsi.o: ../../../include/mips/cp0.h ../../../include/mips/cpu.h
pon_scsi.o: ../../../include/mips/cpu_board.h ../../../include/mips/am9516.h
pon_scsi.o: ../../../include/mips/dvh.h ../../../include/mips/mb87030.h
pon_scsi.o: ../../../include/mips/scsi.h ../pon.h
pon_timers.o: ../pon_timers.c ../../../include/sys/types.h
pon_timers.o: ../../../include/machine/cpu.h
pon_timers.o: ../../../include/machine/cpu_board.h
pon_timers.o: ../../../include/machine/asm.h
pon_timers.o: ../../../include/machine/hd146818.h
pon_timers.o: ../../../include/machine/mk48t02.h
pon_timers.o: ../../../include/machine/i8254clock.h
pon_timers.o: ../../../include/machine/timer.h ../pon.h
pon_timers.o: ../../../include/prom/prom.h
pon_udcslave.o: ../pon_udcslave.c ../../../include/sys/param.h
pon_udcslave.o: ../../../include/machine/param.h ../../../include/signal.h
pon_udcslave.o: ../../../include/sys/types.h ../../../include/mips/cpu.h
pon_udcslave.o: ../../../include/mips/cpu_board.h
pon_udcslave.o: ../../../include/mips/am9516.h ../pon.h
pon_vme.o: ../pon_vme.c ../../../include/sys/types.h
pon_vme.o: ../../../include/machine/cpu.h ../../../include/machine/cpu_board.h
pon_vme.o: ../../../include/machine/mem_board.h
pon_vme.o: ../../../include/machine/r3250reg.h
pon_vme.o: ../../../include/mipsvme/vmereg.h ../pon_ecc.h ../pon.h
pon_vm.o: ../pon_vm.c ../../../include/sys/param.h
pon_vm.o: ../../../include/machine/param.h ../../../include/signal.h
pon_vm.o: ../../../include/sys/types.h ../../../include/machine/cpu.h
pon_vm.o: ../../../include/machine/cpu_board.h ../../../include/saio/setjmp.h
pon_vm.o: ../pon.h
pon_atreg.o: ../pon_atreg.c ../../../include/sys/types.h
pon_atreg.o: ../../../include/machine/cpu_board.h
pon_atreg.o: ../../../include/machine/cpu.h ../pon.h
pon_chain1.o: ../pon_chain1.c ../../../include/sys/types.h
pon_chain1.o: ../../../include/machine/cpu_board.h
pon_chain1.o: ../../../include/machine/cpu.h ../../../include/saio/saioctl.h
pon_chain1.o: ../../../include/machine/am9516.h ../../../include/machine/scsi.h
pon_chain1.o: ../pon.h
pon_chain2.o: ../pon_chain2.c ../../../include/sys/types.h
pon_chain2.o: ../../../include/machine/cpu_board.h
pon_chain2.o: ../../../include/machine/cpu.h ../../../include/saio/saioctl.h
pon_chain2.o: ../../../include/machine/am9516.h ../../../include/machine/scsi.h
pon_chain2.o: ../pon.h
pon_duarts.o: ../pon_duarts.c ../../../include/sys/types.h
pon_duarts.o: ../../../include/machine/duart.h ../../../include/machine/asm.h
pon_duarts.o: ../../../include/machine/cpu_board.h ../pon.h
pon_imr.o: ../pon_imr.c ../../../include/sys/types.h
pon_imr.o: ../../../include/mips/cpu.h ../../../include/mips/cpu_board.h
pon_imr.o: ../pon.h
pon_nvram.o: ../pon_nvram.c ../../../include/sys/param.h
pon_nvram.o: ../../../include/machine/param.h ../../../include/signal.h
pon_nvram.o: ../../../include/sys/types.h ../../../include/machine/regdef.h
pon_nvram.o: ../../../include/machine/asm.h
pon_nvram.o: ../../../include/machine/cpu_board.h
pon_nvram.o: ../../../include/machine/cpu.h ../../../include/machine/asm.h
pon_nvram.o: ../../../include/machine/mk48t02.h ../pon.h
pon_nvram.o: ../../../include/prom/prom.h
