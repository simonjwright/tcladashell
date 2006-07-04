--------------------------------------------------------------------
--
--  Unit Name:    Tash.Platform body
--
--  File Name:    tash-platform.adb
--
--  Purpose:      Provides objects and subprograms that access Tash
--                (really, Tcl) platform information.
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

with Tcl.Ada;

package body Tash.Platform is

   function Byte_Order return String is
      Interp : Tcl.Tcl_Interp;
   begin --  Byte_Order
      Tash_Interp.Get (Interp);
      declare
         Result : constant String :=
            Tcl.Ada.Tcl_GetVar2 (Interp, "tcl_platform", "byteOrder");
      begin
         Tash_Interp.Release (Interp);
         return Result;
      end;
   end Byte_Order;

   function Machine return String is
      Interp : Tcl.Tcl_Interp;
   begin --  Machine
      Tash_Interp.Get (Interp);
      declare
         Result : constant String :=
            Tcl.Ada.Tcl_GetVar2 (Interp, "tcl_platform", "machine");
      begin
         Tash_Interp.Release (Interp);
         return Result;
      end;
   end Machine;

   function OS return String is
      Interp : Tcl.Tcl_Interp;
   begin --  OS
      Tash_Interp.Get (Interp);
      declare
         Result : constant String :=
            Tcl.Ada.Tcl_GetVar2 (Interp, "tcl_platform", "os");
      begin
         Tash_Interp.Release (Interp);
         return Result;
      end;
   end OS;

   function OS_Version return String is
      Interp : Tcl.Tcl_Interp;
   begin --  OS_Version
      Tash_Interp.Get (Interp);
      declare
         Result : constant String :=
            Tcl.Ada.Tcl_GetVar2 (Interp, "tcl_platform", "osVersion");
      begin
         Tash_Interp.Release (Interp);
         return Result;
      end;
   end OS_Version;

   function Platform return String is
      Interp : Tcl.Tcl_Interp;
   begin --  Platform
      Tash_Interp.Get (Interp);
      declare
         Result : constant String :=
            Tcl.Ada.Tcl_GetVar2 (Interp, "tcl_platform", "platform");
      begin
         Tash_Interp.Release (Interp);
         return Result;
      end;
   end Platform;

   function User return String is
      Interp : Tcl.Tcl_Interp;
   begin --  User
      Tash_Interp.Get (Interp);
      declare
         Result : constant String :=
            Tcl.Ada.Tcl_GetVar2 (Interp, "tcl_platform", "user");
      begin
         Tash_Interp.Release (Interp);
         return Result;
      end;
   end User;

end Tash.Platform;
