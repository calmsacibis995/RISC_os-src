;/* --------------------------------------------------- */
;/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
;/* | All Rights Reserved.                            | */
;/* --------------------------------------------------- */
;/* $Header: named.boot,v 1.3.2.1 89/11/26 22:52:22 wje Exp $ */

;
;  @(#)named.boot.slave	1.13	(Berkeley)  87/07/21
;
;  boot file for secondary name server
; Note that there should be one primary entry for each SOA record.
;
;
sortlist 10.0.0.0 

directory	/etc/named.d

; type    domain		source host/file		backup file

cache     .			root.cache
secondary Berkeley.EDU		128.32.137.8 128.32.137.3	ucbhosts.bak
secondary 32.128.IN-ADDR.ARPA	128.32.137.8 128.32.137.3	ucbhosts.rev.bak
primary   0.0.127.IN-ADDR.ARPA	named.local

