/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: error.c,v 1.4.2.2 90/05/09 16:18:53 wje Exp $"

#include <sys/types.h>
#include "lboot.h"
#include "error.h"

struct errortab errortab[] = {
	{ ER1, _PANIC_, "memory overflow" },
	{ ER2, _PANIC_, "MAXCNTL exceeded" },
	{ ER3, _PANIC_, "Undefined expression element" },
	{ ER5, _PANIC_, "flexname too long" },
	{ ER6, _PANIC_, "Unsupported relocation type" },
	{ ER7, _PERROR_|_RETURN_, NULL },
	{ ER8, _RETURN_, "VTOC read failed" },
	{ ER9, _RETURN_, "VTOC does not exist or is damaged" },
	{ ER10, _RETURN_, "I/O ERROR: disk=%s block=%d count=%d" },
	{ ER12, _RETURN_, "Driver %s: not processed by mkboot(1M)" },
	{ ER13, _RETURN_, "Driver %s: major number greater than 127" },
	{ ER14, _RETURN_, "Driver %s: missing section .text" },
	{ ER15, _RETURN_, "Driver %s: not a valid object file" },
	{ ER16, _RETURN_, "%s: No drivers" },
	{ ER17, _RETURN_, "LBE ignored at board code %d; LBE must be at board code 14 or 15" },
	{ ER18, _RETURN_, "INCLUDE: %s; driver not found" },
	{ ER19, _RETURN_, "INCLUDE: %s; driver is EXCLUDED" },
	{ ER20, _RETURN_, "INCLUDE: %s; device not equipped" },
	{ ER21, _RETURN_, "EXCLUDE: %s; driver is INCLUDED" },
	{ ER22, _RETURN_, "%s: dependent driver %s not available" },
	{ ER23, _RETURN_, "%s: dependent driver %s is EXCLUDED" },
	{ ER24, _RETURN_, "%s: device not equipped for dependent driver %s" },
	{ ER25, _RETURN_, "%s: data initializer #C(%s) unknown; zero assumed" },
	{ ER26, _RETURN_, "%s: data initializer #D(%s) unknown; zero assumed" },
	{ ER27, _RETURN_, "%s: data initializer #M(%s) unknown; zero assumed" },
	{ ER28, _RETURN_, "%s: data initializer &%s cannot be resolved" },
	{ ER29, _RETURN_, "%s: data initializer #%s unknown; zero assumed" },
	{ ER30, _RETURN_, "%s: data initializer %s unknown; zero assumed" },
	{ ER31, _RETURN_, "%s: warning: not executable" },
	{ ER32, _RETURN_, "%s: not MAC32 magic" },
	{ ER33, _RETURN_, "%s: no section headers" },
	{ ER34, _RETURN_, "No drivers available, absolute BOOT program must be used" },
	{ ER35, _RETURN_, "%s: device not configured (LBE %d, board code %d)" },
	{ ER36, _RETURN_, "%s: required driver is EXCLUDED" },
	{ ER37, _RETURN_, "%s: flagged as ONCE only; #C set to 1" },
#ifndef mips
	{ ER38, _RETURN_, "Driver not found for %s device (board slot %d)" },
#else mips
	{ ER38, _RETURN_, "Driver not found for %s device" },
#endif mips
	{ ER39, _RETURN_, "Driver not found for %s device (LBE %d, board code %d)" },
	{ ER40, _DYNAMIC_, "Device %s previously configured on LBE (board code 0x%X) at ELB board code %d" },
	{ ER41, _DYNAMIC_, "Device %s previously configured at board slot %d" },
	{ ER42, _DYNAMIC_, "Device %s (board slot %d) not configured" },
	{ ER43, _DYNAMIC_, "Device %s (LBE %d, board code %d) not configured" },
	{ ER44, _RETURN_, "No section loaded at virtual address zero; interrupt vectors are inaccessible" },
	{ ER45, _RETURN_, "%s: already defined" },
	{ ER46, _RETURN_, "Section %s(%s) overlaps %s(%s)" },
	{ ER47, _RETURN_, "%s: not flagged as KERNEL by mkboot(1M)" },
	{ ER48, _RETURN_, "%s: previously allocated" },
	{ ER49, _RETURN_, "%s: truncated read" },
	{ ER50, _RETURN_, "%s: routine %s: unknown id; RNULL assumed" },
	{ ER51, _RETURN_, "\"%s\" does not exist" },
	{ ER52, _RETURN_, "\"%s\": not object file and not ascii text file" },
	{ ER53, _RETURN_, "System: line %d; %s" },
	{ ER54, _RETURN_, "System: %s" },
	{ ER55, _RETURN_, "Section %s(%s) loaded below MAINSTORE address" },
	{ ER56, _RETURN_, "Section %s(%s) loaded beyond end of MAINSTORE" },
	{ ER57, _RETURN_, "Section %s(%s) overlaps boot program" },
	{ ER58, _RETURN_, "%s: invalid object file" },
	{ ER59, _RETURN_, "%s: truncated string table" },
	{ ER60, _RETURN_, "%s: no symbols" },
	{ ER61, _RETURN_, "External symbol %s is undefined; set to zero" },
	{ ER62, _RETURN_, "%s: already allocated" },
	{ ER63, _RETURN_, "%s: already defined" },
	{ ER64, _RETURN_, "%s: previously defined" },
	{ ER65, _RETURN_, "%s: routine %s() not found" },
	{ ER66, _RETURN_, "%s: illegal character string initialization; zero assumed" },
	{ ER67, _RETURN_, "%s: character string initializer truncated" },
	{ ER68, _RETURN_, "System: line %d: too long" },
	{ ER69, _RETURN_, "System: line too long" },
	{ ER70, _RETURN_, "Parameter %s multiply defined" },
	{ ER71, _RETURN_, "          %s: %s = %d" },
	{ ER72, _RETURN_, "          %s: %s = %d  (%s EXCLUDED, parameter ignored)" },
	{ ER73, _RETURN_, "          %s: %s = %d  (set to zero)" },
	{ ER74, _RETURN_, "          %s: %s = \"%s\"" },
	{ ER75, _RETURN_, "          %s: %s = \"%s\"  (%s EXCLUDED, parameter ignored)" },
	{ ER76, _RETURN_, "          %s: %s = \"%s\"  (set to zero)" },
	{ ER77, _DYNAMIC_, "%s configured for more memory than available - use /etc/system\n" },
	{ ER78, _DYNAMIC_, "%s configured for less memory than available\n" },
	{ 0 } };
