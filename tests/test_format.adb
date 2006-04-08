with Ada.Text_IO;
with Tash.Float_Lists;
with Tash.Integer_Lists;
with Tash.Lists;

procedure Test_Format is

   use Tash.Float_Lists;
   use Tash.Integer_Lists;
   use Tash.Lists;

begin -- Test_Format

   -- Demonstrate use of C-style printf
   ------------------------------------
   Ada.Text_IO.Put_Line (Tash.Lists.Format (
      FormatString => "The result for %-30s is %10.2f (%08x)",
      Values       => +"{a piece of pi}" & 3.14159 & 89));

end Test_Format;
