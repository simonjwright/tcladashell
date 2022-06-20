--  This package implements the "thin" binding to Tcl.Tk.
--
--  Copyright (C) 2019 Simon Wright <simon@pushrace.org>
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

package body Tcl.Tk is

   --  The subprograms here correspond to C macros. C functions are
   --  provided to invoke the macros. However, only Ada units can form
   --  part of the public interface of the library, and what where
   --  global symbols in the other units (including C units) are
   --  converted to local symbols and hence not visible to callers.
   --
   --  So we have to call the C functions from Ada code, rather than
   --  having pragma Import in the package spec.

   function Tk_PathName
     (tkwin : not null Tk_Window) return C.Strings.chars_ptr is
      function Tk_CallPathName
        (tkwin : not null Tk_Window) return C.Strings.chars_ptr;
      pragma Import (C, Tk_CallPathName, "Tk_CallPathName");
   begin
      return Tk_CallPathName (tkwin);
   end Tk_PathName;

   procedure Tk_Main
     (argc : C.int;
      argv : CArgv.Chars_Ptr_Ptr;
      proc : not null Tcl_AppInitProc) is
      procedure Tk_CallMain
        (argc : C.int;
         argv : CArgv.Chars_Ptr_Ptr;
         proc : not null Tcl_AppInitProc);
      pragma Import (C, Tk_CallMain, "Tk_CallMain");
   begin
      Tk_CallMain (argc, argv, proc);
   end Tk_Main;

end Tcl.Tk;
