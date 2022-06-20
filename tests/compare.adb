--------------------------------------------------------------------
--
--  compare.adb -- This program compares two files.  If they are the
--                 same, it has no output.  If they are different, it
--                 reports the line number of the first difference,
--                 then terminates.
--
--  Copyright (c) 1998 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;
with Text_IO;

procedure Compare is

   File1          : Text_IO.File_Type;
   Char1          : Character;
   File1_Is_Stdin : Boolean := False;

   File2          : Text_IO.File_Type;
   Char2          : Character;
   File2_Is_Stdin : Boolean := False;

   function "&" (Left : String; Right : Text_IO.Count) return String;
   function "&" (Left : String; Right : Text_IO.Count) return String is
   begin --  "&"
      return Left & Text_IO.Count'Image (Right);
   end "&";

begin --  Compare

   --  Check that we have correct number of command line arguments
   ---------------------------------------------------------------
   if Argument_Count < 2 then
      Text_IO.Put_Line ("usage: compare file1 file2");
      Set_Exit_Status (Failure);
      return;
   end if;

   --  Get names of two files to be compared
   --  from the command line and open them.
   -----------------------------------------
   declare
      Name : constant String := Argument (1);
   begin
      if Name = "-" then
         File1_Is_Stdin := True;
      else
         Text_IO.Open (File1, Text_IO.In_File, Name);
      end if;
   exception
      when others =>
         Text_IO.Put_Line ("can't find or read file " & Name);
         Set_Exit_Status (Failure);
         return;
   end;

   declare
      Name : constant String := Argument (2);
   begin
      if Name = "-" then
         File2_Is_Stdin := True;
      else
         Text_IO.Open (File2, Text_IO.In_File, Name);
      end if;
   exception
      when others =>
         Text_IO.Put_Line ("can't find or read file " & Name);
         Set_Exit_Status (Failure);
         return;
   end;

   loop
      if File1_Is_Stdin then
         exit when Text_IO.End_Of_File;
         Text_IO.Get (Char1);
      else
         exit when Text_IO.End_Of_File (File1);
         Text_IO.Get (File1, Char1);
      end if;
      if File2_Is_Stdin then
         exit when Text_IO.End_Of_File;
         Text_IO.Get (Char2);
      else
         exit when Text_IO.End_Of_File (File2);
         Text_IO.Get (File2, Char2);
      end if;
      if Char1 /= Char2 then
         Text_IO.Put_Line
           (Text_IO.Name (File1) &
            " " &
            Text_IO.Name (File2) &
            " differ: char" &
            Text_IO.Col (File1) &
            ", line" &
            Text_IO.Line (File1));
         Set_Exit_Status (Failure);
         return;
      end if;
   end loop;

   Set_Exit_Status (Success);

end Compare;
