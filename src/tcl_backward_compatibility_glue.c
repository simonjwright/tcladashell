/*
 * tcl_backward_compatibility_glue.c --
 *
 *    This file furnishes functions required for Tcl-8.4 that emulate
 *    the functions of newer Tcl versions.
 *
 *  Copyright (c) 2008 Oliver Kellogg <okellogg@users.sourceforge.net>
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

#include <tcl.h>
#include <stdlib.h>

#if (TCL_MAJOR_VERSION == 8) && (TCL_MINOR_VERSION < 5)

int
TclInfoExistsCmd
    (ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])
{
  Tcl_Obj *inner_objv[3];
  inner_objv[0] = Tcl_NewStringObj("info", -1);
  inner_objv[1] = objv[0];
  inner_objv[2] = objv[1];
  return Tcl_InfoObjCmd(dummy, interp, 3, inner_objv);
}

#endif

