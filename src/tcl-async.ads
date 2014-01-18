--------------------------------------------------------------------
--
--  tcl-async.ads -- This package supports asynchronous setting of Tcl
--  variables or array elements from Ada.
--
--  Copyright (c) Simon Wright <simon@pushface.org>
--
--  Tash is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version. Tash is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. You should have received a copy of the GNU General
--  Public License distributed with Tash; see file COPYING. If not, write to
--
--          Free Software Foundation
--          59 Temple Place - Suite 330
--          Boston, MA 02111-1307, USA
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
--  Tash is maintained at http://tcladashell.sourceforge.net/.
--
--------------------------------------------------------------------

package Tcl.Async is

   --  Must be called before any other subprogram in the package.
   procedure Register (Interp : Tcl_Interp);

   --  Called to set the named Tcl variable in the global scope to the
   --  given value.
   --
   --  The variable will be created if necessary.
   procedure Set (Tcl_Variable : String; Value : String);

   --  Called to set the indexed element of the named Tcl array in
   --  the global scope to the given value.
   --
   --  The array will be created if necessary.
   procedure Set (Tcl_Array : String; Index : String; Value : String);

end Tcl.Async;
