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
/* $Header: funcs.h,v 1.4.2.2 90/05/09 17:56:50 wje Exp $ */

	public void edit ();
	public void next_file ();
	public void prev_file ();
	public void quit ();
	public void init_option ();
	public void toggle_option ();
	public void scan_option ();
	public void forward ();
	public void backward ();
	public void repaint ();
	public void jump_forw ();
	public void jump_back ();
	public void jump_percent ();
	public void jump_loc ();
	public void init_mark ();
	public void setmark ();
	public void lastmark ();
	public void gomark ();
	public int get_back_scroll ();
	public void search ();
	public void end_logfile ();
	public int ch_seek ();
	public int ch_end_seek ();
	public int ch_beg_seek ();
	public POSITION ch_length ();
	public POSITION ch_tell ();
	public int ch_forw_get ();
	public int ch_back_get ();
	public void ch_init ();
	public POSITION position ();
	public void add_forw_pos ();
	public void add_back_pos ();
	public void pos_clear ();
	public int onscreen ();
	public POSITION forw_line ();
	public POSITION back_line ();
	public void put_line ();
	public int control_char ();
	public int carat_char ();
	public void flush ();
	public void dropout ();
	public void putchr ();
	public void putstr ();
	public void error ();
	public void raw_mode ();
	public void get_term ();
	public void init ();
	public void deinit ();
	public void home ();
	public void add_line ();
	public void lower_left ();
	public void bell ();
	public void vbell ();
	public void clear ();
	public void clear_eol ();
	public void so_enter ();
	public void so_exit ();
	public void ul_enter ();
	public void ul_exit ();
	public void bo_enter ();
	public void bo_exit ();
	public void backspace ();
	public void putbs ();
	public char * eq_message ();
	public char * pr_string ();
	public void prewind ();
	public int pappend ();
	public POSITION forw_raw_line ();
	public POSITION back_raw_line ();
	public void init_signals ();
	public void  psignals ();
	public void lsystem ();
	public char * glob ();
	public char * glob ();
	public char * bad_file ();
	public char * bad_file ();
	public char * errno_message ();
	public char * errno_message ();
	public void help ();
	public void open_getchr ();
	public int getchr ();
	public void commands ();
