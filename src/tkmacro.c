/*
 * tkmacro.c --
 *
 *    This file encapsulates calls to all tk.h macro functions into C
 *    function calls.  These can then be called from Ada.  This avoids
 *    having to translate the macro.
 *
 *  Copyright (c) 1999-2000 Terry J. Westley
 *
 *  Tash is free software; you can redistribute it and/or modify it
 *  under terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2, or (at your option)
 *  any later version. Tash is distributed in the hope that it will be
 *  useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *  PURPOSE. See the GNU General Public License for more details. You
 *  should have received a copy of the GNU General Public License
 *  distributed with Tash; see file COPYING. If not, write to
 *
 *          Free Software Foundation
 *          59 Temple Place - Suite 330
 *          Boston, MA 02111-1307, USA
 *
 *  As a special exception, if other files instantiate generics from
 *  this unit, or you link this unit with other files to produce an
 *  executable, this unit does not by itself cause the resulting
 *  executable to be covered by the GNU General Public License. This
 *  exception does not however invalidate any other reasons why the
 *  executable file might be covered by the GNU Public License.
 */

#if 0
#define __CYGWIN__
#endif

#include <tk.h>

char * Tk_CallPathName (Tk_Window tkwin)
{
   return Tk_PathName (tkwin);
}

void Tk_CallMain (int argc, char **argv, Tcl_AppInitProc *proc)
{
   Tk_MainEx (argc, argv, proc, Tcl_CreateInterp());
}
