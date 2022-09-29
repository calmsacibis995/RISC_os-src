/* Definitions file for GNU emacs running on MIPS RISC/os 4.50 */

#include "s-bsd4-3.h"

#undef KERNEL_FILE
#define KERNEL_FILE "/unix"
#undef LDAV_SYMBOL
#define LDAV_SYMBOL "avenrun"
#undef START_FILES
#define START_FILES pre-crt0.o /bsd43/usr/lib/cmplrs/cc/crt1.o
#define LD_SWITCH_SYSTEM -L -L/bsd43/usr/lib -L/bsd43/usr/lib/cmplrs/cc
#define C_SWITCH_SYSTEM -systype bsd43
#define C_DEBUG_SWITCH -O0 -g
#define C_OPTIMIZE_SWITCH C_DEBUG_SWITCH
#undef LIB_STANDARD
#define LIB_STANDARD -lc /bsd43/usr/lib/cmplrs/cc/crtn.o
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -lcurses
