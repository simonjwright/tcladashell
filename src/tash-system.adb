--------------------------------------------------------------------
--
--  Unit Name:    Tash.System body
--
--  File Name:    tash-system.adb
--
--  Purpose:      Provides objects and subprograms that access Tash
--               (really, Tcl) system information:
--
--                 - process id of the current process
--                 - platform information
--
--  Copyright (c) 2000 Terry J. Westley
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

with CHelper;
with System;

package body Tash.System is

   use type Interfaces.C.int;

   function Tcl_PidObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_PidObjCmd, "Tcl_PidObjCmd");

   function Pid return Process_ID is
      Objc   : constant Interfaces.C.int := 1;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Pid

      --  Use Tcl_PidObjCmd to get the process id of the current process
      -----------------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("pid");
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_PidObjCmd
           (dummy  => Standard.System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Check for errors, get result from
      --  interpreter, and convert to an integer.
      ------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Process_ID'Value (Result_String);
      end;

   end Pid;

end Tash.System;
