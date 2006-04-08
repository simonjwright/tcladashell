/*
 * tkmacro.c --
 *
 *    This file encapsulates calls to all tk.h macro functions into
 *    C function calls.  These can then be called from Ada.  This
 *    avoids having to translate the macro.
 */

#include "tk.h"

char * Tk_CallPathName (tkwin)
   register Tk_Window tkwin;
{
   return Tk_PathName (tkwin);
}

void Tk_CallMain (argc, argv, proc)
   int argc;
   char **argv;
   Tcl_AppInitProc *proc;
{
   Tk_MainEx (argc, argv, proc, Tcl_CreateInterp());
}
