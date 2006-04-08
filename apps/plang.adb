--------------------------------------------------------------------
--
-- Unit Name:    PLang body
--
-- File Name:    plang.adb
--
-- Purpose:      This TASH demo program determines the programming
--               language of specified files.
--
-- Copyright (c) 1999-2000 Terry J. Westley
--
-- Tash is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free
-- Software Foundation; either version 2, or (at your option) any later
-- version. Tash is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. You should have received a copy of the GNU General
-- Public License distributed with Tash; see file COPYING. If not, write to
--
--          Software Foundation
--          59 Temple Place - Suite 330
--          Boston, MA 02111-1307, USA
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Tash is maintained by Terry Westley (http://www.adatcl.com).
--
--------------------------------------------------------------------------
--
-- PLang tries to determine the Programming LANGuage of each
-- file on the command line.  A "-" may be used in place of
-- one or all files to cause the program to read files names
-- from standard input, one file per line.
--
-- In this case, each output line contains one file name
-- and its programming language.
--
-- Execute the program with no command line arguments to get
-- a full explanation of what programming languages are
-- supported and how it is determined.
--
--------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Tash.File;
with Tash.Lists;
with Tash.Regexp;

procedure PLang is -- PLang

   use type Tash.Lists.Tash_List;
   use type Tash.Regexp.Tash_Regexp;

   Args      : aliased Tash.Lists.Tash_List;
   Verbose   : Boolean := False;
   Print_Why : Boolean := False;
   Line      : String (1..1000);
   Last      : Natural;

   -- Regular expressions
   ----------------------
   RE_White_Space    : Tash.Regexp.Tash_Regexp := +("[ " & ASCII.HT & "]");
   RE_Makefile       : Tash.Regexp.Tash_Regexp := +"[Mm]akefile";
   RE_Object         : Tash.Regexp.Tash_Regexp := +"^.(o|ali|exe)$";
   RE_C_Source       : Tash.Regexp.Tash_Regexp := +"^.[ch]$";
   RE_Ada_Source     : Tash.Regexp.Tash_Regexp := +"^.(ada|ads|adb)$";
   RE_Fortran_Source : Tash.Regexp.Tash_Regexp := +"^.(f|for)$";
   RE_HTML_Source    : Tash.Regexp.Tash_Regexp := +"^.(htm|html)$";
   RE_Bourne_Shell   : Tash.Regexp.Tash_Regexp := +"^(:|#)";
   RE_Exec_Line      : Tash.Regexp.Tash_Regexp := +"^[  ]*(#|$)";

   procedure Show_Usage is
      use Ada.Text_IO;
   begin -- Show_Usage
      Put_Line ("Usage:");
      Put_Line ("   plang [ (source-file|-|-why) ... ]");
      New_Line;
      Put_Line ("Command line arguments:");
      Put_Line ("   source-file : source file");
      New_Line;
      Put_Line ("   -           : get list of source files from");
      Put_Line ("                 standard input, one file per line");
      New_Line;
      Put_Line ("   -why        : print why program skipped a file or");
      Put_Line ("                 couldn't determine language.");
      New_Line;
      Put_Line ("Description:");
      Put_Line ("   Plang attempts to determine the programming language of");
      Put_Line ("   the files specified on the command line (or in standard");
      Put_Line ("   input).");
      New_Line;
      Put_Line ("   It recognizes the following languages based on file name");
      Put_Line ("   extension:");
      New_Line;
      Put_Line ("      Language     Extension(s)");
      Put_Line ("      --------     ------------");
      Put_Line ("      C            .c, .h");
      Put_Line ("      Ada          .ada, .ads, .adb");
      Put_Line ("      Tcl          .tcl");
      Put_Line ("      Perl         .pl");
      Put_Line ("      Emacs List   .el");
      Put_Line ("      Awk          .awk");
      Put_Line ("      Fortran      .f, .for");
      Put_Line ("      SQL          .sql");
      Put_Line ("      HTML         .htm, .html");
      Put_Line ("      XML          .xml");
      New_Line;
      Put_Line ("   If the file name matches the regular expression pattern,");
      Put_Line ("   ""[Mm]akefile"", it is considered to be a Makefile.");
      New_Line;
      Put_Line ("   If the first two bytes of the file are ""#!,"" the");
      Put_Line ("   language is determined by the program name in the path");
      Put_Line ("   following the ""#!.""  However, if the path following");
      Put_Line ("   the ""#!"" is ""/bin/sh,"" then the next non-blank,");
      Put_Line ("   non-comment line is examined for the string, ""exec.""");
      Put_Line ("   If found, then the language is determined by the program");
      Put_Line ("   name in the path following the ""exec.""");
      New_Line;
      Put_Line ("   This table shows what language is identified for the");
      Put_Line ("   specified program path:");
      New_Line;
      Put_Line ("      Language     Program(s)");
      Put_Line ("      --------     ------------");
      Put_Line ("      Tcl          tcl, tclsh, wish, expect");
      Put_Line ("      Perl         perl");
      Put_Line ("      C shell      csh");
      New_Line;
      Put_Line ("   If no ""exec"" line is found, then the language is");
      Put_Line ("   considered to be Bourne shell");
      New_Line;
      Put_Line ("Output:");
      Put_Line ("   Each input file name is printed to standard output along");
      Put_Line ("   with the programming language.  Binary files and those");
      Put_Line ("   whose file name ends in ~ are ignored.  All files for");
      Put_Line ("   which a programming language cannot be determined are");
      Put_Line ("   listed as ""unknown.""");
      New_Line;
   end Show_Usage;

   -- Extracts the name of a program from the first line
   -- of a Unix shell script (must start with #!) or from
   -- the exec line (usually the first non-comment line).
   -- Returns an empty list if no program was found.
   ------------------------------------------------------
   function Get_Program (Line : in String) return String is

   begin -- Get_Program

      -- Line must start with #! to contain a program name
      ----------------------------------------------------
      if Line'Length < 2 then
         return "";
      end if;
      if Line(Line'First..Line'First+1) /= "#!" then
         return "";
      end if;

      declare
         -- Split the line at space characters and get the
         -- first element as the path name of the program.
         -------------------------------------------------
         Program_Path_Name : constant String :=
            Tash.Lists.Head (Tash.Lists.Split (
               Line(Line'First+2..Line'Last), " "));
      begin
         -- Split the program path name at "/" characters
         -- and return the last element as the program name.
         ---------------------------------------------------
         -- return Tash.Lists.Tail (Tash.Lists.Split (Program_Path_Name, "/"));
         return Tash.Lists.Tail (Tash.File.Split (Program_Path_Name));
      end;

   end Get_Program;

   -- This procedure attempts to determine the programming language
   -- of a file.  Where we successfully determine the language, the
   -- file name and language are printed to standard out.
   -- If we can't tell what it is, we print the language as "unknown."
   -- Some files, such as those whose name ends in ~ are simply
   -- skipped and not evaluated at all.  In these cases, we just
   -- return without printing anything.
   --------------------------------------------------------------------
   procedure Print_Programming_Language (
      File_Name : in String) is

      Input_File : Ada.Text_IO.File_Type;
      Raw_Line   : String (1..1000);
      Last       : Natural;

   begin -- Print_Programming_Language

      -- skip this file if it ends in ~ or is empty
      ---------------------------------------------
      if File_Name = "" then
         if Print_Why then
            Ada.Text_IO.Put_Line (File_Name & ": skipped because is empty");
         end if;
         return;
      end if;
      if File_Name (File_Name'Last) = '~' then
         if Print_Why then
            Ada.Text_IO.Put_Line (File_Name & ": skipped because ends in ~");
         end if;
         return;
      end if;

      -- skip this file if it is a directory
      --------------------------------------
      if Tash.File.Is_Directory (File_Name) then
         if Print_Why then
            Ada.Text_IO.Put_Line (File_Name & ": skipped because is a directory");
         end if;
         return;
      end if;

      -- is it a makefile?
      --------------------
      if Tash.Regexp.Match (File_Name, RE_Makefile) then
         Ada.Text_IO.Put_Line (File_Name & ": makefile");
         return;
      end if;

      -- Can we determine language from the file extension?
      -----------------------------------------------------
      declare
         Extension : constant String := Tash.File.Extension (File_Name);
      begin
         if Verbose then
            Ada.Text_IO.Put_Line ("plang: ext=" & Extension);
         end if;

         if Extension'length > 0 then

            -- Check for file extensions of files to be skipped
            ---------------------------------------------------
            if Tash.Regexp.Match (Extension, RE_Object) then
               if Print_Why then
                  Ada.Text_IO.Put_Line (File_Name &
                     ": skipped because it is an object file (.o, .ali, or .exe extension)");
               end if;
               return;
            end if;

            -- Print languages of those extensions we recognize,
            -- then return immediately.
            ----------------------------------------------------
            if Tash.Regexp.Match (Extension, RE_C_Source) then
               Ada.Text_IO.Put_Line (File_Name & ": c");
               return;
            end if;
            if Tash.Regexp.Match (Extension, RE_Ada_Source) then
               Ada.Text_IO.Put_Line (File_Name & ": ada");
               return;
            end if;
            if Extension = ".tcl" then
               Ada.Text_IO.Put_Line (File_Name & ": tcl");
               return;
            end if;
            if Extension = ".pl" then
               Ada.Text_IO.Put_Line (File_Name & ": perl");
               return;
            end if;
            if Extension = ".el" then
               Ada.Text_IO.Put_Line (File_Name & ": emacs-lisp");
               return;
            end if;
            if Extension = ".awk" then
               Ada.Text_IO.Put_Line (File_Name & ": awk");
               return;
            end if;
            if Tash.Regexp.Match (Extension, RE_Fortran_Source) then
               Ada.Text_IO.Put_Line (File_Name & ": fortran");
               return;
            end if;
            if Extension = ".sql" then
               Ada.Text_IO.Put_Line (File_Name & ": sql");
               return;
            end if;
            if Tash.Regexp.Match (Extension, RE_HTML_Source) then
               Ada.Text_IO.Put_Line (File_Name & ": html");
               return;
            end if;
            if Extension = ".xml" then
               Ada.Text_IO.Put_Line (File_Name & ": xml");
               return;
            end if;

         end if;

      end;

      ------------------------------------------------
      -- We arrive here if there was no file extension
      -- or we didn't recognize it.
      ------------------------------------------------

      -- Open the file to investigate further what
      -- language this program is written in.
      --------------------------------------------
      Ada.Text_IO.Open (File => Input_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name);

      -- Get the first line of the file
      ---------------------------------
      Ada.Text_IO.Get_Line (File => Input_File,
                            Item => Raw_Line,
                            Last => Last);

      -- Get the program name (if any).  This will
      -- be in a pattern similar to "#!/bin/sh".
      --------------------------------------------
      declare
         Program : constant String := Get_Program (Raw_Line (1..Last));
      begin
         if Program'Length <= 0 then
            -- There was no program name on the first line.
            -- But, if the first line starts with a : or #, it's
            -- Bourne shell for sure.
            -----------------------------------------------------
            if Tash.Regexp.Match (Raw_Line (1..Last), RE_Bourne_Shell) then
               Ada.Text_IO.Put_Line (File_Name & ": sh");
               Ada.Text_IO.Close (Input_File);
               return;

            else
               -- The first line gives us no clue as to the
               -- language or how to discover the language.
               -- We won't learn any more by reading the rest
               -- of the file, so we just give up.
               ---------------------------------------------
               Ada.Text_IO.Close (Input_File);
               Ada.Text_IO.Put   (File_Name & ": unknown");
               if Print_Why then
                  Ada.Text_IO.Put_Line (" because it has no extension and" &
                     " isn't a scripting language we recognize");
               else
                  Ada.Text_IO.New_Line;
               end if;
               return;
            end if;

         elsif Program /= "sh" then
            -- We found a non-Bourne program name on the first
            -- line of the file.  We print the name of the
            -- program as the programming language.
            --------------------------------------------------
            Ada.Text_IO.Put_Line (File_Name & ": " & Program);
            Ada.Text_IO.Close (Input_File);
            return;
         end if;

         -- We reach here when we have a Bourne shell script.
         -- We must now read more of the file to check if this
         -- script execs another program.
         -------------------------------------------------------------------
         while not Ada.Text_IO.End_Of_File (Input_File) loop

            Ada.Text_IO.Get_Line (File => Input_File,
                                  Item => Raw_Line,
                                  Last => Last);

            if not Tash.Regexp.Match (Raw_Line (1..Last), RE_Exec_Line) then

               -- We have found the first executable line of a
               -- Bourne shell script.  Check if this script
               -- execs another program.
               -------------------------------------------------
               declare
                  Program : constant String := Get_Program (Raw_Line (1..Last));
               begin
                  if Program'Length <= 0 then

                     -- Does not exec another program.  We will
                     -- list it as a Bourne shell script.
                     ------------------------------------------
                     Ada.Text_IO.Put_Line (File_Name & ": sh");

                  else
                     -- Use the program name we've just found as
                     -- the name of the programming language.
                     -------------------------------------------
                     Ada.Text_IO.Put_Line (File_Name & ": " & Program);
                  end if;
               end;
            end if;
            Ada.Text_IO.Close (Input_File);
            return;
         end loop;

      end;

      Ada.Text_IO.Close (Input_File);

      -- The file has slipped through all our efforts to
      -- identify it.  We don't know what language it is.
      ---------------------------------------------------
      Ada.Text_IO.Put (File_Name & ": unknown");
      if Print_Why then
         Ada.Text_IO.Put_Line (" because it has no extension and" &
            " isn't a scripting language we recognize");
      else
         Ada.Text_IO.New_Line;
      end if;

     exception
        when E : others =>
           Ada.Text_IO.Put (File_Name & ": unknown");
           if Print_Why then
              Ada.Text_IO.Put_Line (" because exception " &
                 Ada.Exceptions.Exception_Name (E) & " occurred");
           else
              Ada.Text_IO.New_Line;
           end if;

   end Print_Programming_Language;

begin -- PLang

   -- Get all command line arguments and put them in a Tash list.
   -- If any command line argument is "-verbose," then set the
   -- verbose flag and don't put it in the argument list.
   -- If any command line argument is "-why," then set the
   -- print why flag and don't put it in the argument list.
   -------------------------------------------------------------
   for I in 1..Ada.Command_Line.Argument_Count loop
      declare
         Arg : constant String := Ada.Command_Line.Argument (I);
      begin
         if Arg = "-verbose" then
            Verbose := True;
         elsif Arg = "-why" then
            Print_Why := True;
         elsif Arg = "-" then
            while not Ada.Text_IO.End_Of_File (Ada.Text_IO.Standard_Input) loop
               Ada.Text_IO.Get_Line (Item => Line,
                                     Last => Last);
               Tash.Lists.Append (Args, Line (1..Last));
            end loop;
         else
            Tash.Lists.Append (Args, Arg);
         end if;
      end;
   end loop;

   -- display usage if there are no command line arguments
   -------------------------------------------------------
   if Tash.Lists.Length (Args) = 0 then
      Show_Usage;
      return;
   end if;

   -- Show command line arguments
   ------------------------------
   if Verbose then
      Ada.Text_IO.Put_Line ("plang " & Tash.Lists.To_String (Args));
   end if;

   -- Each iteration of this loop examines one file to
   -- determine and print its programming language.
   ---------------------------------------------------
   while not Tash.Lists.Is_Empty (Args) loop
      declare
         Arg : constant String := Tash.Lists.Head (Args);
      begin
         Tash.Lists.Pop (Args);
         Print_Programming_Language (Arg);
      end;
   end loop;

end PLang;

