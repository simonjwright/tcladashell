with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Tash.Test;

package body Tash.Arrays.Test is

   Verbose : Boolean := False;

   procedure Set_Verbose (On : in Boolean) is
   begin -- Set_Verbose
      Verbose := On;
   end Set_Verbose;

   procedure Put_Error
     (Description     : in String;
      Reason          : in String;
      Sorted_Result   : Tash.Lists.Tash_List;
      Expected_Result : in String;
      Expected_Count  : in Natural);
   procedure Put_Error
     (Description     : in String;
      Reason          : in String;
      Sorted_Result   : Tash.Lists.Tash_List;
      Expected_Result : in String;
      Expected_Count  : in Natural) is
   begin -- Put_Error
      Tash.Test.Fail_Test_Case;
      Ada.Text_IO.Put ("Test Case");
      Ada.Integer_Text_IO.Put (Tash.Test.Test_Case_Number, Width => 4);
      Ada.Text_IO.Put_Line (" failed: " & Description & ": " & Reason);
      Ada.Text_IO.Put_Line ("     actual=" &
         Tash.Lists.Internal_Rep (Sorted_Result));
      Ada.Text_IO.Put ("   expected=(s=""" & Expected_Result &
         """ t=N/A c=");
      Ada.Text_IO.Put (
         Ada.Strings.Fixed.Trim (Source => Integer'Image (Expected_Count),
                                 Side   => Ada.Strings.Left));
      Ada.Text_IO.Put_Line (")");
   end Put_Error;

   procedure Test_Case (
      Description     : in String;
      Actual_Result   : in Tash.Arrays.Tash_Array;
      Expected_Result : in String;
      Expected_Count  : in Natural) is
   --
      Sorted_Result : Tash.Lists.Tash_List :=
         Tash.Arrays.Get_Sorted_Elements (Actual_Result);
   begin -- Test_Case
      Tash.Test.Incr_Test_Case_Number;
      if Tash.Lists.To_String (Sorted_Result) /= Expected_Result then
         Put_Error (Description, "strings not equal",
                    Sorted_Result, Expected_Result, Expected_Count);
--    elsif not (Tash.Type_Of (Actual_Result) = "string" or
--               Tash.Type_Of (Actual_Result) = "") then
--       Put_Error (Description, "wrong type=" & Tash.Type_Of (Actual_Result),
--                  Sorted_Result, Expected_Result, Expected_Count);
      elsif Tash.Ref_Count (Actual_Result) /= Expected_Count then
         Put_Error (Description, "count not equal",
                    Sorted_Result, Expected_Result, Expected_Count);
      elsif Verbose then
         Ada.Text_IO.Put ("Test Case");
         Ada.Integer_Text_IO.Put (Tash.Test.Test_Case_Number, Width => 4);
         Ada.Text_IO.Put_Line (" passed: " & Description);
         Ada.Text_IO.Put_Line ("                      " &
            Tash.Lists.Internal_Rep (Sorted_Result));
      end if;
   end Test_Case;

end Tash.Arrays.Test;
