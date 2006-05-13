--------------------------------------------------------------------
--
--  tash-lists-test.adb -- Routines for testing tash lists.
--
--  Copyright (c) 1998-2000 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Tash.Test;

package body Tash.Lists.Test is

   procedure Set_Verbose (On : in Boolean) is
   begin --  Set_Verbose
      Tash.Lists.Verbose := On;
   end Set_Verbose;

   procedure Test_Case
     (Description     : in String;
      Actual_Result   : in Tash.Lists.Tash_List;
      Expected_Result : in String;
      Expected_Type   : in String;
      Expected_Count  : in Natural;
      Expected_Length : in Natural)
   is
   begin --  Test_Case
      Tash.Test.Incr_Test_Case_Number;
      if Tash.Lists.To_String (Actual_Result) /= Expected_Result or
         Tash.Type_Of (Actual_Result) /= Expected_Type or
         Tash.Ref_Count (Actual_Result) /= Expected_Count or
         Tash.Lists.Length (Actual_Result) /= Expected_Length
      then
         Tash.Test.Fail_Test_Case;
         Ada.Text_IO.Put ("Test Case");
         Ada.Integer_Text_IO.Put (Tash.Test.Test_Case_Number, Width => 4);
         Ada.Text_IO.Put_Line (" failed: " & Description);
         Ada.Text_IO.Put_Line ("     actual=" & Internal_Rep (Actual_Result));
         Ada.Text_IO.Put_Line
           (" Length=" & Integer'Image (Tash.Lists.Length (Actual_Result)));
         Ada.Text_IO.Put ("   expected=(s=""" & Expected_Result);
         Ada.Text_IO.Put (""" t=" & Expected_Type);
         Ada.Text_IO.Put (" c=");
         Ada.Text_IO.Put
           (Ada.Strings.Fixed.Trim
               (Source => Integer'Image (Expected_Count),
                Side   => Ada.Strings.Left));
         Ada.Text_IO.Put (") Length=" & Integer'Image (Expected_Length));
         Ada.Text_IO.New_Line;
      elsif Tash.Lists.Verbose then
         Ada.Text_IO.Put ("Test Case");
         Ada.Integer_Text_IO.Put (Tash.Test.Test_Case_Number, Width => 4);
         Ada.Text_IO.Put_Line (" passed: " & Description);
         Ada.Text_IO.Put_Line
           ("                      " & Internal_Rep (Actual_Result));
      end if;
   end Test_Case;

end Tash.Lists.Test;
