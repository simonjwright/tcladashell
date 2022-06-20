--------------------------------------------------------------------
--
--  cargv.ads -- Create C-style "argv" vectors from strings and
--              Ada.Command_Line.
--
--  Copyright (c) 1995-2000 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--  This package provides the data type Chars_Ptr_Ptr which corresponds
--  to the char** of C and subprograms for creating and manipulating
--  arrays of C strings.
--
--------------------------------------------------------------------

with Interfaces.C.Pointers;
with Interfaces.C.Strings;

package CArgv is

   package C renames Interfaces.C;

   subtype CNatural is C.int range 0 .. C.int'Last;

   type Vector is array (CNatural range <>) of aliased C.Strings.chars_ptr;
   --  This is a C-style "argv" vector.

   package Argv_Pointer
   is new C.Pointers (Index => CNatural,
                      Element => C.Strings.chars_ptr,
                      Element_Array => Vector,
                      Default_Terminator => C.Strings.Null_Ptr);

   subtype Chars_Ptr_Ptr is Argv_Pointer.Pointer;
   --  This is C char **

   ---------------------------------------------------------------------
   --
   --  The following subprograms support converting command line
   --  arguments to C-style argc/argv command line arguments.
   --
   ---------------------------------------------------------------------

   procedure Create (Argc : out CNatural; Argv : out Chars_Ptr_Ptr);
   --  Create returns the command line arguments from Ada.Command_Line
   --  and converts them to a C-style, null-terminated argument vector.

   procedure Show (Argc : CNatural; Argv : Chars_Ptr_Ptr);
   --  Prints Argc and Argv to standard out.

   procedure Free (Argv : in out Chars_Ptr_Ptr);
   --  Free all space used by Argv.

   --  Example of getting Ada command line arguments and passing them
   --  to a C function requiring argc/argv arguments:
   --
   --    declare
   --       Argc : C.Int;
   --       Argv : CArgv.Chars_Ptr_Ptr;
   --    begin
   --       CArgv.Create (Argc, Argv);
   --       Tcl.Tcl_Concat (Argc, Argv);
   --       CArgv.Free (Argv);
   --    end;

   ---------------------------------------------------------------------
   --
   --  The following subprogram supports retrieving a command line
   --  argument from C-style argv command line arguments.
   --
   ---------------------------------------------------------------------

   function Arg (Argv : Chars_Ptr_Ptr; N : CNatural) return String;
   --  Returns the Nth argument from Argv.

   ---------------------------------------------------------------------
   --
   --  The following subprograms support creating C-style argc/argv
   --  argument vectors from strings.
   --
   ---------------------------------------------------------------------

   function Empty return Chars_Ptr_Ptr;
   --  An empty Chars_Ptr_Ptr, used for constructors.

   function "&" (Argv : Chars_Ptr_Ptr; Arg : String) return Chars_Ptr_Ptr;
   --  Construct a Chars_Ptr_Ptr using concat operation.

   function Argc (Argv : Chars_Ptr_Ptr) return CNatural;
   --  Returns the number of arguments in a Chars_Ptr_Ptr.

   --  Example of creating a Chars_Ptr_Ptr to pass to a C function requiring
   --  argc/argv arguments:
   --
   --    declare
   --       Argv : CArgv.Chars_Ptr_Ptr :=
   --          Empty & "this" & "is" & "four" & "arguments";
   --    begin
   --       Tcl.Tcl_Concat (CArgv.Argc (Argv), Argv);
   --       CArgv.Free (Argv);
   --    end;

end CArgv;
