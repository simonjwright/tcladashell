--  $Id$

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure CArgv.Test is
   Argv : Chars_Ptr_Ptr
     := Empty & "this" & "is" & "four" & "arguments";
begin
   for I in 0 .. 4 loop
      Put_Line (I'Img & " " & Arg (Argv, CNatural (I)));
   end loop;
exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));
end CArgv.Test;
