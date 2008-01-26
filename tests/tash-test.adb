--------------------------------------------------------------------
--
--  Unit Name:    Tash.Test body
--
--  File Name:    tash-test.adb
--
--  Purpose:      Provides Tash testing routines that apply to all
--               Tash types.
--
--
--  Copyright (c) 1999-2000 Terry J. Westley
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

with Ada.Integer_Text_IO;
with Ada.Text_IO;

package body Tash.Test is

   Case_Number : Natural := 0;
   Failed      : Boolean := False;

   procedure Set_Verbose (On : in Boolean) is
   begin --  Set_Verbose
      Verbose := On;
   end Set_Verbose;

   procedure Fail_Test_Case is
   begin --  Fail_Test_Case
      Failed := True;
   end Fail_Test_Case;

   procedure Incr_Test_Case_Number is
   begin --  Incr_Test_Case_Number
      Case_Number := Case_Number + 1;
   end Incr_Test_Case_Number;

   function Test_Case_Number return Natural is
   begin --  Test_Case_Number
      return Case_Number;
   end Test_Case_Number;

   procedure Test_Case
     (Description : in String;
      Result      : in Boolean;
      Failure_Msg : in String := "")
   is
   begin --  Test_Case
      Case_Number := Case_Number + 1;
      if not Result then
         Failed := True;
         Ada.Text_IO.Put ("Test Case");
         Ada.Integer_Text_IO.Put (Case_Number, Width => 4);
         Ada.Text_IO.Put_Line (" failed: " & Description);
         Ada.Text_IO.Put_Line ("   " & Failure_Msg);
      elsif Verbose then
         Ada.Text_IO.Put ("Test Case");
         Ada.Integer_Text_IO.Put (Case_Number, Width => 4);
         Ada.Text_IO.Put_Line (" passed: " & Description);
      end if;
   end Test_Case;

   function All_Test_Cases_Passed return Boolean is
   begin --  All_Test_Cases_Passed
      return not Failed;
   end All_Test_Cases_Passed;

end Tash.Test;
