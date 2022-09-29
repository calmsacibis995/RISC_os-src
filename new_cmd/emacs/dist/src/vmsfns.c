/* VMS subprocess and command interface.
   Copyright (C) 1987, 1988 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

/* Written by Mukesh Prasad.  */

/*
 * INTERFACE PROVIDED BY EMACS FOR VMS SUBPROCESSES:
 *
 *	Emacs provides the following functions:
 *
 *      "spawn-subprocess", which takes as arguments:
 *
 *	  (i)   an integer to identify the spawned subprocess in future
 *              operations,
 *	  (ii)  A function to process input from the subprocess, and
 *	  (iii) A function to be called upon subprocess termination.
 *
 *      First argument is required.  If second argument is missing or nil,
 *      the default action is to insert all received messages at the current
 *      location in the current buffer.  If third argument is missing or nil,
 *      no action is taken upon subprocess termination.
 *	The input-handler is called as
 *		(input-handler num string)
 *	where num is the identifying integer for the subprocess and string
 *	is a string received from the subprocess.  exit-handler is called
 *	with the identifying integer as the argument.
 *
 *      "send-command-to-subprocess" takes two arguments:
 *
 *	  (i)   Subprocess identifying integer.
 *        (ii)  String to send as a message to the subprocess.
 *
 *      "stop-subprocess" takes the subprocess identifying integer as
 *      argument.
 *
 *  Implementation is done by spawning an asynchronous subprocess, and
 *  communicating to it via mailboxes.
 */

#ifdef VMS

#include <stdio.h>
#include <ctype.h>
#undef NULL

#include "config.h"
#include "lisp.h"
#include <descrip.h>
#include <dvidef.h>
#include <prvdef.h>
/* #include <clidef.h> */
#include <iodef.h>
#include <ssdef.h>
#include <errno.h>

#ifdef VMS4_4   /* I am being cautious; perhaps this exists in older versions */
#include <jpidef.h>
#endif

/* #include <syidef.h> */

#define	CLI$M_NOWAIT	1	/* clidef.h is missing from C library */
#define	SYI$_VERSION	4096	/* syidef.h is missing from C library */
#define	JPI$_CLINAME	522	/* JPI$_CLINAME is missing from jpidef.h */
#define	JPI$_MASTER_PID	805	/* JPI$_MASTER_PID missing from jpidef.h */
#define	LIB$_NOSUCHSYM	1409892 /* libclidef.h missing */

#define	MSGSIZE	160		/* Maximum size for mailbox operations */

/* IO status block for mailbox operations.  */
struct mbx_iosb
{
  short status;
  short size;
  int   pid;
};

/* Structure for maintaining linked list of subprocesses.  */
struct process_list
{
  int name;			/* Numeric identifier for subprocess */
  int process_id;		/* VMS process address */
  int process_active;		/* 1 iff process has not exited yet */
  int mbx_chan;			/* Mailbox channel to write to process */
  struct mbx_iosb iosb;		/* IO status block for write operations */
  Lisp_Object input_handler;	/* Input handler for subprocess */
  Lisp_Object exit_handler;	/* Exit handler for subprocess */
  struct process_list * next;	/* Linked list chain */
};

/* Structure for privilege list.  */
struct privilege_list
{
  char * name;
  int  mask;
};

/* Structure for finding VMS related information.  */
struct vms_objlist
{
  char * name;			/* Name of object */
  Lisp_Object (* objfn)();	/* Function to retrieve VMS object */
};

static int exit_ast ();		/* Called upon subprocess exit */
static int create_mbx ();	/* Creates mailbox */
static void mbx_msg ();		/* Writes null terminated string to mbx */
static void write_to_mbx ();	/* Writes message to string */
static void start_mbx_input ();	/* Queues I/O request to mailbox */

static int input_mbx_chan = 0;	/* Channel to read subprocess input on */
static char input_mbx_name[20];
				/* Storage for mailbox device name */
static struct dsc$descriptor_s input_mbx_dsc;
				/* Descriptor for mailbox device name */
static struct process_list * process_list = 0;
				/* Linked list of subprocesses */
static char mbx_buffer[MSGSIZE];
				/* Buffer to read from subprocesses */
static struct mbx_iosb input_iosb;
				/* IO status block for mailbox reads */

int have_process_input,		/* Non-zero iff subprocess input pending */
    process_exited;		/* Non-zero iff suprocess exit pending */

/* List of privilege names and mask offsets */
static struct privilege_list priv_list[] = {

    { "ACNT",		PRV$V_ACNT },
    { "ALLSPOOL",	PRV$V_ALLSPOOL },
    { "ALTPRI",		PRV$V_ALTPRI },
    { "BUGCHK",		PRV$V_BUGCHK },
    { "BYPASS",		PRV$V_BYPASS },
    { "CMEXEC",		PRV$V_CMEXEC },
    { "CMKRNL",		PRV$V_CMKRNL },
    { "DETACH",		PRV$V_DETACH },
    { "DIAGNOSE",	PRV$V_DIAGNOSE },
    { "DOWNGRADE",	PRV$V_DOWNGRADE }, /* Isn't VMS as low as you can go?  */
    { "EXQUOTA",	PRV$V_EXQUOTA },
    { "GRPPRV",		PRV$V_GRPPRV },
    { "GROUP",		PRV$V_GROUP },
    { "GRPNAM",		PRV$V_GRPNAM },
    { "LOG_IO",		PRV$V_LOG_IO },
    { "MOUNT",		PRV$V_MOUNT },
    { "NETMBX",		PRV$V_NETMBX },
    { "NOACNT",		PRV$V_NOACNT },
    { "OPER",		PRV$V_OPER },
    { "PFNMAP",		PRV$V_PFNMAP },
    { "PHY_IO",		PRV$V_PHY_IO },
    { "PRMCEB",		PRV$V_PRMCEB },
    { "PRMGBL",		PRV$V_PRMGBL },
    { "PRMJNL",		PRV$V_PRMJNL },
    { "PRMMBX",		PRV$V_PRMMBX },
    { "PSWAPM",		PRV$V_PSWAPM },
    { "READALL",	PRV$V_READALL },
    { "SECURITY",	PRV$V_SECURITY },
    { "SETPRI",		PRV$V_SETPRI },
    { "SETPRV",		PRV$V_SETPRV },
    { "SHARE", 		PRV$V_SHARE },
    { "SHMEM",		PRV$V_SHMEM },
    { "SYSGBL",		PRV$V_SYSGBL },
    { "SYSLCK",		PRV$V_SYSLCK },
    { "SYSNAM",		PRV$V_SYSNAM },
    { "SYSPRV",		PRV$V_SYSPRV },
    { "TMPJNL",		PRV$V_TMPJNL },
    { "TMPMBX",		PRV$V_TMPMBX },
    { "UPGRADE",	PRV$V_UPGRADE },
    { "VOLPRO",		PRV$V_VOLPRO },
    { "WORLD",		PRV$V_WORLD },

    };

static Lisp_Object
          vms_account(), vms_cliname(), vms_owner(), vms_grp(), vms_image(),
          vms_parent(), vms_pid(), vms_prcnam(), vms_terminal(), vms_uic_int(),
	  vms_uic_str(), vms_username(), vms_version_fn(), vms_trnlog(),
	  vms_symbol(), vms_proclist();

/* Table of arguments to Fvms_object, and the handlers that get the data.  */

static struct vms_objlist vms_object [] = {
    { "ACCOUNT",	vms_account },	/* Returns account name as a string */
    { "CLINAME",	vms_cliname },	/* Returns CLI name (string) */
    { "OWNER",		vms_owner },	/* Returns owner process's PID (int) */
    { "GRP",		vms_grp },	/* Returns group number of UIC (int) */
    { "IMAGE",		vms_image },	/* Returns executing image (string) */
    { "PARENT",		vms_parent },	/* Returns parent proc's PID (int) */
    { "PID",		vms_pid },	/* Returns process's PID (int) */
    { "PRCNAM",		vms_prcnam },	/* Returns process's name (string) */
    { "TERMINAL",	vms_terminal },	/* Returns terminal name (string) */
    { "UIC",		vms_uic_int },	/* Returns UIC as integer */
    { "UICGRP",		vms_uic_str },	/* Returns UIC as string */
    { "USERNAME",	vms_username },	/* Returns username (string) */
    { "VERSION",	vms_version_fn },/* Returns VMS version (string) */
    { "LOGICAL",	vms_trnlog },	/* Translates VMS logical name */
    { "DCL-SYMBOL",	vms_symbol },	/* Translates DCL symbol */
    { "PROCLIST",	vms_proclist },	/* Returns list of all PIDs on system */
    };
   
Lisp_Object Qdefault_subproc_input_handler;

extern int process_ef;		/* Event flag for subprocess operations */

DEFUN ("default-subprocess-input-handler",
  Fdefault_subproc_input_handler, Sdefault_subproc_input_handler,
  2, 2, 0,
  "Default input handler for input from spawned subprocesses.")
  (name, input)
     Lisp_Object name, input;
{
  /* Just insert in current buffer */
  InsCstr (XSTRING (input)->data, XSTRING (input)->size);
  InsCstr ("\n", 1);
}

DEFUN ("spawn-subprocess", Fspawn_subprocess, Sspawn_subprocess, 1, 3, 0,
  "Spawns an asynchronous VMS suprocess for command processing.")
  (name, input_handler, exit_handler)
     Lisp_Object name, input_handler, exit_handler;
{
  int status;
  char output_mbx_name[20];
  struct dsc$descriptor_s output_mbx_dsc;
  struct process_list * ptr, * p;

  CHECK_NUMBER (name, 0);
  if (! input_mbx_chan)
    {
      if (! create_mbx (&input_mbx_dsc, input_mbx_name, &input_mbx_chan, 1))
	return Qnil;
      start_mbx_input ();
    }
  ptr = 0;
  for (p = process_list; p; p = p->next)
    if (p->name == XFASTINT (name)) 
      {
	ptr = p;
	if (p->process_active)
	  return Qt;
	else
	  sys$dassgn (p->mbx_chan);
	break;
      }
  if (! ptr)
    ptr = xmalloc (sizeof (struct process_list));
  if (! create_mbx (&output_mbx_dsc, output_mbx_name, &ptr->mbx_chan, 2))
    {
      free (ptr);
      return Qnil;
    }
  if (NULL (input_handler))
    input_handler = Qdefault_subproc_input_handler;
  ptr->input_handler = input_handler;
  ptr->exit_handler = exit_handler;
  message ("Creating subprocess...");
  status = lib$spawn (0, &output_mbx_dsc, &input_mbx_dsc, &CLI$M_NOWAIT, 0,
                      &ptr->process_id, 0, 0, exit_ast, &ptr->process_active);
  if (! (status & 1))
    {
      sys$dassgn (ptr->mbx_chan);
      free (ptr);
      error ("Unable to spawn subprocess");
      return Qnil;
    }
  ptr->name = XFASTINT (name);
  ptr->next = process_list;
  ptr->process_active = 1;
  process_list = ptr;
  message ("Creating subprocess...done");
  return Qt;
}

static void
mbx_msg (ptr, msg)
     struct process_list *ptr;
     char *msg;
{
  write_to_mbx (ptr, msg, strlen (msg));
}

DEFUN ("send-command-to-subprocess",
  Fsend_command_to_subprocess, Ssend_command_to_subprocess, 2, 2,
  "sSend command to subprocess: \nsSend subprocess %s command: ",
  "Send to VMS subprocess named NAME the string COMMAND.")
  (name, command)
     Lisp_Object name, command;
{
  struct process_list * ptr;

  CHECK_NUMBER (name, 0);
  CHECK_STRING (command, 1);
  for (ptr = process_list; ptr; ptr = ptr->next)
    if (XFASTINT (name) == ptr->name)
      {
	write_to_mbx (ptr, XSTRING (command)->data,
		      XSTRING (command)->size);
	return Qt;
      }
  return Qnil;
}

DEFUN ("stop-subprocess", Fstop_subprocess, Sstop_subprocess, 1, 1,
  "sStop subprocess: ", "Stop VMS subprocess named NAME.")
  (name)
     Lisp_Object name;
{
  struct process_list * ptr;

  CHECK_NUMBER (name, 0);
  for (ptr = process_list; ptr; ptr = ptr->next)
    if (XFASTINT (name) == ptr->name)
      {
	ptr->exit_handler = Qnil;
	if (sys$delprc (&ptr->process_id, 0) & 1)
	  ptr->process_active = 0;
	return Qt;
      }
  return Qnil;
}

static int
exit_ast (active)
     int * active;
{
  process_exited = 1;
  *active = 0;
  sys$setef (process_ef);
}

/* Process to handle input on the input mailbox.
 * Searches through the list of processes until the matching PID is found,
 * then calls its input handler.
 */

process_command_input ()
{
  struct process_list * ptr;
  char * msg;
  int msglen;
  Lisp_Object expr;

  msg = mbx_buffer;
  msglen = input_iosb.size;
  /* Hack around VMS oddity of sending extraneous CR/LF characters for
   * some of the commands (but not most).
   */
  if (msglen > 0 && *msg == '\r')
    {
      msg++;
      msglen--;
    }
  if (msglen > 0 && msg[msglen - 1] == '\n')
    msglen--;
  if (msglen > 0 && msg[msglen - 1] == '\r')
    msglen--;
  /* Search for the subprocess in the linked list.
   */
  expr = Qnil;
  for (ptr = process_list; ptr; ptr = ptr->next)
    if (ptr->process_id == input_iosb.pid)
      {
	expr = Fcons (ptr->input_handler,
		      Fcons (make_number (ptr->name),
			     Fcons (make_string (msg, msglen),
				    Qnil)));
	break;
      }
  have_process_input = 0;
  start_mbx_input ();
  if (! NULL (expr))
    Feval (expr);
}

/* Searches process list for any processes which have exited.  Calls their
 * exit handlers and removes them from the process list.
 */

process_exit ()
{
  struct process_list * ptr, * prev, * next;

  process_exited = 0;
  prev = 0;
  ptr = process_list;
  while (ptr)
    {
      if (! ptr->process_active)
	{
	  next = ptr->next;
	  if (prev)
	    prev->next = next;
	  else
	    process_list = next;
	  if (! NULL (ptr->exit_handler))
	    Feval (Fcons (ptr->exit_handler, Fcons (make_number (ptr->name),
						    Qnil)));
	  sys$dassgn (ptr->mbx_chan);
	  free (ptr);
	}
      prev = ptr;
      ptr = next;
    }
}

/* Called at emacs exit.
 */

kill_vms_processes ()
{
  struct process_list * ptr;

  for (ptr = process_list; ptr; ptr = ptr->next)
    if (ptr->process_active)
      {
	sys$dassgn (ptr->mbx_chan);
	sys$delprc (&ptr->process_id, 0);
      }
  sys$dassgn (input_mbx_chan);
  process_list = 0;
  input_mbx_chan = 0;
}

/* Creates a temporary mailbox and retrieves its device name in 'buf'.
 * Makes the descriptor pointed to by 'dsc' refer to this device.
 * 'buffer_factor' is used to allow sending messages asynchronously
 * till some point.
 */

static int
create_mbx (dsc, buf, chan, buffer_factor)
     struct dsc$descriptor_s *dsc;
     char *buf;
     int *chan;
     int buffer_factor;
{
  int strval[2];
  int status;

  status = sys$crembx (0, chan, MSGSIZE, MSGSIZE * buffer_factor, 0, 0, 0);
  if (! (status & 1))
    {
      message ("Unable to create mailbox.  Need TMPMBX privilege.");
      return 0;
    }
  strval[0] = 16;
  strval[1] = buf;
  status = lib$getdvi (&DVI$_DEVNAM, chan, 0, 0, strval,
		       &dsc->dsc$w_length);
  if (! (status & 1))
    return 0;
  dsc->dsc$b_dtype = DSC$K_DTYPE_T;
  dsc->dsc$b_class = DSC$K_CLASS_S;
  dsc->dsc$a_pointer = buf;
  return 1;
}				/* create_mbx */

/* AST routine to be called upon receiving mailbox input.
 * Sets flag telling keyboard routines that input is available.
 */

static int
mbx_input_ast ()
{
  have_process_input = 1;
}

/* Issue a QIO request on the input mailbox.
 */
static void
start_mbx_input ()
{
  sys$qio (process_ef, input_mbx_chan, IO$_READVBLK, &input_iosb,
           mbx_input_ast, 0, mbx_buffer, sizeof (mbx_buffer),
	   0, 0, 0, 0);
}

/* Send a message to the subprocess input mailbox, without blocking if
 * possible.
 */
static void
write_to_mbx (ptr, buf, len)
     struct process_list *ptr;
     char *buf;
     int len;
{
  sys$qiow (0, ptr->mbx_chan, IO$_WRITEVBLK | IO$M_NOW, &ptr->iosb,
	    0, 0, buf, len, 0, 0, 0, 0);
}

DEFUN ("setprv", Fsetprv, Ssetprv, 1, 3, 0,
  "Set or reset a VMS privilege.  First arg is privilege name.\n\
Second arg is t or nil, indicating whether the privilege is to be\n\
set or reset.  Default is nil.  Returns t if success, nil if not.\n\
If third arg is non-nil, does not change privilege, but returns t\n\
or nil depending upon whether the privilege is already enabled.")
  (priv, value, getprv)
     Lisp_Object priv, value, getprv;
{
  int prvmask[2], prvlen, newmask[2];
  char * prvname;
  int found, i;
  struct privilege_list * ptr;

  CHECK_STRING (priv, 0);
  priv = Fupcase (priv);
  prvname = XSTRING (priv)->data;
  prvlen = XSTRING (priv)->size;
  found = 0;
  prvmask[0] = 0;
  prvmask[1] = 0;
  for (i = 0; i < sizeof (priv_list) / sizeof (priv_list[0]); i++)
    {
      ptr = &priv_list[i];
      if (prvlen == strlen (ptr->name) &&
	  bcmp (prvname, ptr->name, prvlen) == 0)
	{
	  if (ptr->mask >= 32)
	    prvmask[1] = 1 << (ptr->mask % 32);
	  else
	    prvmask[0] = 1 << ptr->mask;
	  found = 1;
	  break;
	}
    }
  if (! found)
    error ("Unknown privilege name %s", XSTRING (priv)->data);
  if (NULL (getprv))
    {
      if (sys$setprv (NULL (value) ? 0 : 1, prvmask, 0, 0) == SS$_NORMAL)
	return Qt;
      return Qnil;
    }
  /* Get old priv value */
  if (sys$setprv (0, 0, 0, newmask) != SS$_NORMAL)
    return Qnil;
  if ((newmask[0] & prvmask[0])
      || (newmask[1] & prvmask[1]))
    return Qt;
  return Qnil;
}

/* Retrieves VMS system information.  */

#ifdef VMS4_4  /* I don't know whether these functions work in old versions */

DEFUN ("vms-system-info", Fvms_system_info, Svms_system_info, 1, 3, 0,
  "Retrieve VMS process and system information.\n\
The first argument (a string) specifies the type of information desired.\n\
The other arguments depend on the type you select.\n\
For information about a process, the second argument is a process ID\n\
or a process name, with the current process as a default.\n\
These are the possibilities for the first arg (upper or lower case ok):\n\
    account     Returns account name\n\
    cliname     Returns CLI name\n\
    owner       Returns owner process's PID\n\
    grp         Returns group number\n\
    parent      Returns parent process's PID\n\
    pid         Returns process's PID\n\
    prcnam      Returns process's name\n\
    terminal    Returns terminal name\n\
    uic         Returns UIC number\n\
    uicgrp      Returns formatted [UIC,GRP]\n\
    username    Returns username\n\
    version     Returns VMS version\n\
    logical     Translates VMS logical name (second argument)\n\
    dcl-symbol  Translates DCL symbol (second argument)\n\
    proclist    Returns list of all PIDs on system (needs WORLD privilege)." )
  (type, arg1, arg2)
     Lisp_Object type, arg1, arg2;
{
  int i, typelen;
  char * typename;
  struct vms_objlist * ptr;

  CHECK_STRING (type, 0);
  type = Fupcase (type);
  typename = XSTRING (type)->data;
  typelen = XSTRING (type)->size;
  for (i = 0; i < sizeof (vms_object) / sizeof (vms_object[0]); i++)
    {
      ptr = &vms_object[i];
      if (typelen == strlen (ptr->name)
	  && bcmp (typename, ptr->name, typelen) == 0)
	return (* ptr->objfn)(arg1, arg2);
    }
  error ("Unknown object type %s", typename);
}

/* Given a reference to a VMS process, returns its process id.  */

static int
translate_id (pid, owner)
     Lisp_Object pid;
     int owner;		/* if pid is null/0, return owner.  If this
			 * flag is 0, return self. */
{
  int status, code, id, i, numeric, size;
  char * p;
  int prcnam[2];

  if (NULL (pid)
      || XTYPE (pid) == Lisp_String && XSTRING (pid)->size == 0
      || XTYPE (pid) == Lisp_Int && XFASTINT (pid) == 0)
    {
      code = owner ? JPI$_OWNER : JPI$_PID;
      status = lib$getjpi (&code, 0, 0, &id);
      if (! (status & 1))
	error ("Cannot find %s: %s",
	       owner ? "owner process" : "process id",
	       vmserrstr (status));
      return (id);
    }
  if (XTYPE (pid) == Lisp_Int)
    return (XFASTINT (pid));
  CHECK_STRING (pid, 0);
  pid = Fupcase (pid);
  size = XSTRING (pid)->size;
  p = XSTRING (pid)->data;
  numeric = 1;
  id = 0;
  for (i = 0; i < size; i++, p++)
    if (isxdigit (*p))
      {
	id *= 16;
	if (*p >= '0' && *p <= '9')
	  id += *p - '0';
	else
	  id += *p - 'A' + 10;
      }
    else
      {
	numeric = 0;
	break;
      }
  if (numeric)
    return (id);
  prcnam[0] = XSTRING (pid)->size;
  prcnam[1] = XSTRING (pid)->data;
  status = lib$getjpi (&JPI$_PID, 0, prcnam, &id);
  if (! (status & 1))
    error ("Cannot find process id: %s",
	   vmserrstr (status));
  return (id);
}				/* translate_id */

/* VMS object retrieval functions.  */

static Lisp_Object
getjpi (jpicode, arg, numeric)
     int jpicode;		/* Type of GETJPI information */
     Lisp_Object arg;
     int numeric;		/* 1 if numeric value expected */
{
  int id, status, numval;
  char str[128];
  int strdsc[2] = { sizeof (str), str };
  short strlen;

  id = translate_id (arg, 0);
  status = lib$getjpi (&jpicode, &id, 0, &numval, strdsc, &strlen);
  if (! (status & 1))
    error ("Unable to retrieve information: %s",
	   vmserrstr (status));
  if (numeric)
    return (make_number (numval));
  return (make_string (str, strlen));
}

static Lisp_Object
vms_account (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_ACCOUNT, arg1, 0);
}

static Lisp_Object
vms_cliname (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_CLINAME, arg1, 0);
}

static Lisp_Object
vms_grp (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_GRP, arg1, 1);
}

static Lisp_Object
vms_image (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_IMAGNAME, arg1, 0);
}

static Lisp_Object
vms_owner (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_OWNER, arg1, 1);
}

static Lisp_Object
vms_parent (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_MASTER_PID, arg1, 1);
}

static Lisp_Object
vms_pid (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_PID, arg1, 1);
}

static Lisp_Object
vms_prcnam (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_PRCNAM, arg1, 0);
}

static Lisp_Object
vms_terminal (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_TERMINAL, arg1, 0);
}

static Lisp_Object
vms_uic_int (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_UIC, arg1, 1);
}

static Lisp_Object
vms_uic_str (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_UIC, arg1, 0);
}

static Lisp_Object
vms_username (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return getjpi (JPI$_USERNAME, arg1, 0);
}

static Lisp_Object
vms_version_fn (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  char str[40];
  int status;
  int strdsc[2] = { sizeof (str), str };
  short strlen;

  status = lib$getsyi (&SYI$_VERSION, 0, strdsc, &strlen, 0, 0);
  if (! (status & 1))
    error ("Unable to obtain version: %s", vmserrstr (status));
  return (make_string (str, strlen));
}

static Lisp_Object
vms_trnlog (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  char str[100];
  int status, symdsc[2];
  int strdsc[2] = { sizeof (str), str };
  short length, level;

  CHECK_STRING (arg1, 0);
  symdsc[0] = XSTRING (arg1)->size;
  symdsc[1] = XSTRING (arg1)->data;
  status = lib$sys_trnlog (symdsc, &length, strdsc);
  if (! (status & 1))
    error ("Unable to translate logical name: %s", vmserrstr (status));
  if (status == SS$_NOTRAN)
    return (Qnil);
  return (make_string (str, length));
}

static Lisp_Object
vms_symbol (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  char str[100];
  int status, symdsc[2];
  int strdsc[2] = { sizeof (str), str };
  short length, level;

  CHECK_STRING (arg1, 0);
  symdsc[0] = XSTRING (arg1)->size;
  symdsc[1] = XSTRING (arg1)->data;
  status = lib$get_symbol (symdsc, strdsc, &length, &level);
  if (! (status & 1)) {
    if (status == LIB$_NOSUCHSYM)
      return (Qnil);
    else
      error ("Unable to translate symbol: %s", vmserrstr (status));
  }
  return (make_string (str, length));
}

static Lisp_Object
vms_proclist (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  Lisp_Object retval;
  int id, status, pid;

  retval = Qnil;
  pid = -1;
  for (;;)
    {
      status = lib$getjpi (&JPI$_PID, &pid, 0, &id);
      if (status == SS$_NOMOREPROC)
	break;
      if (! (status & 1))
	error ("Unable to get process ID: %s", vmserrstr (status));
      retval = Fcons (make_number (id), retval);
    }
  return (Fsort (retval, intern ("<")));
}

DEFUN ("shrink-to-icon", Fshrink_to_icon, Sshrink_to_icon, 0, 0, 0,
      "If emacs is running in a workstation window, shrink to an icon.")
     ()
{
  static char result[128];
  static $DESCRIPTOR (result_descriptor, result);
  static $DESCRIPTOR (tt_name, "TT:");
  static int chan = 0;
  static int buf = 0x9d + ('2'<<8) + ('2'<<16) + (0x9c<<24);
  int status;
  static int temp = JPI$_TERMINAL;

  status = lib$getjpi (&temp, 0, 0, 0, &result_descriptor, 0);
  if (status != SS$_NORMAL)
    error ("Unable to determine terminal type.");
  if (result[0] != 'W' || result[1] != 'T') /* see if workstation */
    error ("Can't shrink-to-icon on a non workstation terminal");
  if (!chan)				/* assign channel if not assigned */
    if ((status = sys$assign (&tt_name, &chan, 0, 0)) != SS$_NORMAL)
      error ("Can't assign terminal, %d", status);
  status = sys$qiow (0, chan, IO$_WRITEVBLK+IO$M_BREAKTHRU, 0, 0, 0,
		     &buf, 4, 0, 0, 0, 0);
  if (status != SS$_NORMAL)
    error ("Can't shrink-to-icon, %d", status);
}

#endif /* VMS4_4 */

init_vmsfns ()
{
  process_list = 0;
  input_mbx_chan = 0;
}

syms_of_vmsfns ()
{
  defsubr (&Sdefault_subproc_input_handler);
  defsubr (&Sspawn_subprocess);
  defsubr (&Ssend_command_to_subprocess);
  defsubr (&Sstop_subprocess);
  defsubr (&Ssetprv);
#ifdef VMS4_4
  defsubr (&Svms_system_info);
  defsubr (&Sshrink_to_icon);
#endif /* VMS4_4 */
  Qdefault_subproc_input_handler = intern ("default-subprocess-input-handler");
  staticpro (&Qdefault_subproc_input_handler);
}
#endif /* VMS */

