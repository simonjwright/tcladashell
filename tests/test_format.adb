with Ada.Text_IO;
with Tash.Floats;
with Tash.Integers;
with Tash.Lists;
with Tash.Strings;

procedure Test_Format is

   use type Tash.Strings.Tash_String;
   use type Tash.Floats.Tash_Float;
   use type Tash.Integers.Tash_Integer;

   X : Tash.Strings.Tash_String;
   Y : Tash.Floats.Tash_Float;
   Z : Tash.Integers.Tash_Integer;

begin -- Test_Format

   -- Initialize some Tash objects to
   -- hold a string, float, and integer
   ------------------------------------
   X := +"a piece of pi";
   Y := +3.14159;
   Z := +89;

   -- Demonstrate use of C-style printf
   ------------------------------------
   Ada.Text_IO.Put_Line (Tash.Lists.Format (
      FormatString => "The result for %-30s is %10.2f (%08x)",
      Values       => Tash.Lists.To_Tash_List (X, Y, Z)));

   -- Demonstrate use of C-style printf without explictly
   -- creating the Tash objects: X, Y, and Z
   ------------------------------------------------------
   Ada.Text_IO.Put_Line (
      Tash.Lists.Format (
         "The result for %-30s is %10.2f (%08x)",
         Tash.Lists.To_Tash_List (+"a half piece of pi", +1.5708, +82)));

end Test_Format;
