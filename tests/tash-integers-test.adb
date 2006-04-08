with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Tash.Test;

package body Tash.Integers.Test is

   Verbose : Boolean := False;

   procedure Set_Verbose (On : in Boolean) is
   begin -- Set_Verbose
      Verbose := On;
   end Set_Verbose;

   procedure Test_Case (
      Description     : in String;
      Actual_Result   : in Tash.Integers.Tash_Integer;
      Expected_Result : in Long_Integer;
      Expected_Type   : in String;
      Expected_Count  : in Natural) is
   --
      Integer_Result : Long_Integer;
   begin -- Test_Case
      Tash.Test.Incr_Test_Case_Number;
      Integer_Result := Tash.Integers.To_Integer (Actual_Result);
      if Integer_Result /= Expected_Result or
            Tash.Type_Of (Actual_Result) /= Expected_Type or
            Tash.Ref_Count (Actual_Result) /= Expected_Count then
         Tash.Test.Fail_Test_Case;
         Ada.Text_IO.Put ("Test Case");
         Ada.Integer_Text_IO.Put (Tash.Test.Test_Case_Number, Width => 4);
         Ada.Text_IO.Put_Line (" failed: " & Description);
         Ada.Text_IO.Put_Line ("     actual=" &
            Internal_Rep (Actual_Result));
         Ada.Text_IO.Put ("   expected=(id=N/A");
         Ada.Text_IO.Put (", s=""");
         Ada.Text_IO.Put (
            Ada.Strings.Fixed.Trim (Source => Long_Integer'Image (Expected_Result),
                                    Side   => Ada.Strings.Left));
         Ada.Text_IO.Put (""" t=" & Expected_Type & " c=");
         Ada.Text_IO.Put (
            Ada.Strings.Fixed.Trim (Source => Integer'Image (Expected_Count),
                                    Side   => Ada.Strings.Left));
         Ada.Text_IO.Put_Line (")");
      elsif Verbose then
         Ada.Text_IO.Put ("Test Case");
         Ada.Integer_Text_IO.Put (Tash.Test.Test_Case_Number, Width => 4);
         Ada.Text_IO.Put_Line (" passed: " & Description);
         Ada.Text_IO.Put_Line ("                      " &
            Internal_Rep (Actual_Result));
      end if;
   end Test_Case;

end Tash.Integers.Test;
