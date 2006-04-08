--------------------------------------------------------------------
--
-- freq2.adb -- This program demonstrates how the TASH Ada/Tcl interface
--              provides Tcl features for use in an Ada program.  This
--              version is different from freq.adb in that it uses the
--              thick Tcl binding (Tash packages) instead of the thin
--              binding (Tcl packages).
--
-- Copyright (c) 2000 Terry J. Westley
--
-- See the file "license.htm" for information on usage and
-- redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
-- This program reads lines from standard input and counts the number
-- of occurrences of each unique line.  Frequency counts are written
-- to standard out.
--
--------------------------------------------------------------------

with Ada.Text_IO;
with Tash.Arrays;
with Tash.Integer_Arrays;
with Tash.Lists;

procedure Freq2 is -- Frequency counter

   Count       : Natural;
   Freq_Counts : Tash.Arrays.Tash_Array;
   Input_Line  : String (1..1024);
   Last_Pos    : Natural;
   Indices     : Tash.Lists.Tash_List;

begin -- Freq2

   -- Read lines from standard input until end of file encountered,
   -- incrementing frequency count of each line.
   ----------------------------------------------------------------
   while not Ada.Text_IO.End_of_File loop
      Ada.Text_IO.Get_Line (Input_Line, Last_Pos);
      declare
         Index : constant String := Input_Line (1..Last_Pos);
      begin
         Count := Tash.Integer_Arrays.Get_Element (Freq_Counts, Index);
         Count := Count + 1;
         Tash.Integer_Arrays.Set_Element (Freq_Counts, Index, Count);
      exception
         when Tash.Arrays.Array_Error =>
            -- Index does not exist; set it to 1
            Tash.Integer_Arrays.Set_Element (Freq_Counts, Index, 1);
      end;
   end loop;

   -- Get the indices
   ------------------
   Indices := Tash.Arrays.Get_Indices (Freq_Counts);

   -- Iterate through every item and print it and its frequency count
   ------------------------------------------------------------------
   for I in 1..Tash.Lists.Length (Indices) loop
      declare
         Index : constant String := Tash.Lists.Get_Element (Indices, I);
      begin
         Count := Tash.Integer_Arrays.Get_Element (Freq_Counts, Index);
         Ada.Text_IO.Put_Line (Index & " " & Integer'Image (Count));
      end;
   end loop;

end Freq2;
