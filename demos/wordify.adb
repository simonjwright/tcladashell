--------------------------------------------------------------------
--
--  wordify.adb -- This program copies standard input to standard output,
--                removing punctuation and writing one word per output line.
--                A word is considered to be a sequence of letters, numbers,
--                and (sorta like Ada identifiers) underscores.
--
--  Copyright (c) 1998 Terry J. Westley
--  Copyright (c) 2006-2022 Simon Wright <simon@pushface.org>
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;

procedure Wordify is

   Line           : String (1 .. 1024);
   Length         : Natural;
   At_End_Of_Line : Boolean := True;

begin --  Wordify

   while not Ada.Text_IO.End_Of_File loop
      Ada.Text_IO.Get_Line (Line, Length);
      for I in  1 .. Length loop
         if Is_Alphanumeric (Line (I)) or else Line (I) = '_' then
            Ada.Text_IO.Put (Line (I));
            At_End_Of_Line := False;
         elsif not At_End_Of_Line then
            Ada.Text_IO.New_Line;
            At_End_Of_Line := True;
         end if;
      end loop;
   end loop;

end Wordify;
