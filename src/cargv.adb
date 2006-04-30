--------------------------------------------------------------------
--
-- cargv.adb --
--
--  Copyright (c) 1995-2000 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--  01-Sep-97   TJW   Fixed dangling pointer problem independently
--                   discovered by me and Brett Kettering (thanks, Brett!).
--
--  24-Mar-98   TJW   Again, Brett found one of my bugs, this time, in
--                   the "&" function.
--
--------------------------------------------------------------------

with Ada.Command_Line;
with Text_IO;

package body CArgv is

   --  To make operators visible:
   use type C.int;
   use type C.Strings.chars_ptr;

   type Vector_Access is access Vector;

   Empty_Vector : Vector := (0 => C.Strings.Null_Ptr);

   procedure Create (Argc : out C.int; Argv : out Chars_Ptr_Ptr) is
      Size : constant C.int := C.int (Ada.Command_Line.Argument_Count + 1);
      Vec  : Vector_Access  := new Vector (0 .. Size);
   begin --  Create
      Vec (0) := C.Strings.New_String (Ada.Command_Line.Command_Name);
      for i in  1 .. Size - 1 loop
         Vec (i) :=
            C.Strings.New_String (Ada.Command_Line.Argument (Integer (i)));
      end loop;
      Vec (Size) := C.Strings.Null_Ptr;
      Argc       := Size;
      Argv       := Vec (Vec'First)'Unchecked_Access;
   end Create;

   procedure Show (Argc : in C.int; Argv : in Chars_Ptr_Ptr) is
      Ptr : Chars_Ptr_Ptr := Argv;
   begin --  Show
      Text_IO.Put_Line ("Argc :" & C.int'Image (Argc));
      for i in  0 .. Argc - 1 loop
         Text_IO.Put (C.int'Image (i) & " : ");
         if Ptr.all = C.Strings.Null_Ptr then
            Text_IO.Put_Line ("<null-ptr>");
         else
            Text_IO.Put_Line (C.Strings.Value (Ptr.all));
         end if;
         Argv_Pointer.Increment (Ptr);
      end loop;
   end Show;

   procedure Free (Argv : in out Chars_Ptr_Ptr) is
      Ptr : Chars_Ptr_Ptr := Argv;
   begin --  Free
      while Ptr.all /= C.Strings.Null_Ptr loop
         C.Strings.Free (Ptr.all);
         Argv_Pointer.Increment (Ptr);
      end loop;
      --  This is only a partial free because the argv array
      --  itself is not freed.
      --  Free_Vector (Argv);
   end Free;

   function Empty return Chars_Ptr_Ptr is
   begin --  Empty
      return Empty_Vector (Empty_Vector'First)'Unchecked_Access;
   end Empty;

   function "&" (Argv : Chars_Ptr_Ptr; Arg : String) return Chars_Ptr_Ptr is
      Size : constant C.int :=
         C.int (Argv_Pointer.Virtual_Length (Argv)) + 1;
      Vec  : Vector_Access := new Vector (0 .. Size);

   begin --  "&"

      --  Copy the existing argv.  Note that Vec(Size-1) should be
      --  the location of its null pointer.
      Vec (0 .. Size - 1) := Argv_Pointer.Value (Argv);

      --  Create the new string and put its pointer where the
      --  null pointer of the argv was.
      Vec (Size - 1) := C.Strings.New_String (Arg);

      --  Insert null pointer for the new argv
      Vec (Size) := C.Strings.Null_Ptr;

      return Vec (Vec'First)'Unchecked_Access;

   end "&";

   function Argc (Argv : in Chars_Ptr_Ptr) return C.int is
   begin --  Argc
      return C.int (Argv_Pointer.Virtual_Length (Argv));
   end Argc;

end CArgv;
