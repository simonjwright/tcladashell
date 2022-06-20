--------------------------------------------------------------------
--
--  cargv.adb  --
--
--  Copyright (c) 1995-2000 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--  01-Sep-97   TJW   Fixed dangling pointer problem independently
--                    discovered by me and Brett Kettering (thanks, Brett!).
--
--  24-Mar-98   TJW   Again, Brett found one of my bugs, this time, in
--                    the "&" function.
--
--  8.x.08      sjw   Implemented Arg; tidied.
--
--------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

package body CArgv is

   --  To make operators visible:
   use type CNatural;
   use type C.Strings.chars_ptr;

   type Vector_Access is access Vector;

   Empty_Vector : Vector := (0 => C.Strings.Null_Ptr);

   procedure Create (Argc : out CNatural; Argv : out Chars_Ptr_Ptr) is
      Size : constant CNatural
        := CNatural (Ada.Command_Line.Argument_Count + 1);
      Vec  : Vector_Access := new Vector (0 .. Size);
   begin -- Create
      Vec (0) := C.Strings.New_String (Ada.Command_Line.Command_Name);
      for i in  1 .. Size - 1 loop
         Vec (i) :=
            C.Strings.New_String (Ada.Command_Line.Argument (Integer (i)));
      end loop;
      Vec (Size) := C.Strings.Null_Ptr;
      Argc       := Size;
      Argv       := Vec (Vec'First)'Access;
   end Create;

   procedure Show (Argc : CNatural; Argv : Chars_Ptr_Ptr) is
      Ptr : Chars_Ptr_Ptr := Argv;
      use Ada.Text_IO;
   begin --  Show
      Put_Line ("Argc :" & CNatural'Image (Argc));
      for i in  0 .. Argc - 1 loop
         Put (CNatural'Image (i) & " : ");
         if Ptr.all = C.Strings.Null_Ptr then
            Put_Line ("<null-ptr>");
         else
            Put_Line (C.Strings.Value (Ptr.all));
         end if;
         Argv_Pointer.Increment (Ptr);
      end loop;
   end Show;

   procedure Free (Argv : in out Chars_Ptr_Ptr) is
      pragma Warnings (Off, Argv);  -- logically in out
      Ptr : Chars_Ptr_Ptr := Argv;
   begin -- Free
      while Ptr.all /= C.Strings.Null_Ptr loop
         C.Strings.Free (Ptr.all);
         Argv_Pointer.Increment (Ptr);
      end loop;
      --  This is only a partial free because the argv array
      --  itself is not freed.
      --  Free_Vector (Argv);
   end Free;

   function Arg (Argv : Chars_Ptr_Ptr; N : CNatural) return String is
      L : constant CNatural := CNatural (Argv_Pointer.Virtual_Length (Argv));
   begin -- Arg
      if N >= L then
         raise Constraint_Error;
      end if;
      return C.Strings.Value (Argv_Pointer.Value (Argv) (N));
   end Arg;

   function Empty return Chars_Ptr_Ptr is
   begin -- Empty
      return Empty_Vector (Empty_Vector'First)'Access;
   end Empty;

   function "&" (Argv : Chars_Ptr_Ptr; Arg : String) return Chars_Ptr_Ptr is
      Size : constant CNatural :=
         CNatural (Argv_Pointer.Virtual_Length (Argv)) + 1;
      Vec  : Vector_Access := new Vector (0 .. Size);
   begin -- "&"

      --  Copy the existing argv.  Note that Vec(Size-1) should be
      --  the location of its null pointer.
      Vec (0 .. Size - 1) := Argv_Pointer.Value (Argv);

      --  Create the new string and put its pointer where the
      --  null pointer of the argv was.
      Vec (Size - 1) := C.Strings.New_String (Arg);

      --  Insert null pointer for the new argv
      Vec (Size) := C.Strings.Null_Ptr;

      return Vec (Vec'First)'Access;

   end "&";

   function Argc (Argv : Chars_Ptr_Ptr) return CNatural is
   begin -- Argc
      return CNatural (Argv_Pointer.Virtual_Length (Argv));
   end Argc;

end CArgv;
