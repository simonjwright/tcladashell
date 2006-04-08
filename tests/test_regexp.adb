with Ada.Command_Line;
with Ada.Strings;
with Ada.Tags;
with Ada.Text_IO;
with Tash.Floats.Test;
with Tash.Integers.Test;
with Tash.Lists.Test;
with Tash.Regexp;
with Tash.Strings.Test;
with Tash.Test;

procedure Test_Regexp is

   -- Make operators visible without qualification
   -----------------------------------------------
   use type Ada.Tags.Tag;
   use type Tash.Floats.Tash_Float;
   use type Tash.Integers.Tash_Integer;
   use type Tash.Lists.Tash_List;
   use type Tash.Strings.Tash_String;

   Verbose : Boolean := False;

begin -- Test_Regexp

   -------------------------------------------
   -- Check for -verbose command line argument
   -------------------------------------------
   GET_COMMAND_LINE_ARGUMENTS:
   for I in 1..Ada.Command_Line.Argument_Count loop
      if Ada.Command_Line.Argument (I) = "-verbose" then
         Verbose := True;
         exit GET_COMMAND_LINE_ARGUMENTS;
      end if;
   end loop GET_COMMAND_LINE_ARGUMENTS;

   Tash.Test.Set_Verbose          (On => Verbose);

   declare

      A : Tash.Lists.Tash_List;
      B : Tash.Lists.Tash_List;
      C : aliased Tash.Lists.Tash_List;
      D : Tash.Integers.Tash_Integer;
      F : Tash.Floats.Tash_Float;
      G : Tash.Strings.Tash_String;
      U : aliased Tash.Lists.Tash_List;

   begin

      -- prepare various Tash objects for testing Regexp
      --------------------------------------------------
      A := Tash.Lists.To_Tash_List (+3, +4);
      D := +52;
      C := Tash.Lists.To_Tash_List (
         D, +3.14159, Tash.Strings.To_Tash_String ("This is a string"), A);
      G := Tash.Strings.To_Tash_String ("This is a string");

      Tash.Test.Test_Case (
         Description     => "Simple regexp on an Ada string",
         Result          => Tash.Regexp.Match ("This is a string", "string"));

      Tash.Test.Test_Case (
         Description     => "Simple regexp on a Tash object",
         Result          => Tash.Regexp.Match (G, "string"));

      Tash.Test.Test_Case (
         Description     => "Simple regexp on a Tash list",
         Result          => Tash.Regexp.Match_Element (C, "string") = 3);

      Tash.Test.Test_Case (
         Description     => "Find 5 going forward",
         Result          => Tash.Regexp.Match_Element (C, "5") = 1);

      Tash.Test.Test_Case (
         Description     => "Find 5 going backward",
         Result          => Tash.Regexp.Match_Element (C, "5", Ada.Strings.Backward) = 2);

      Tash.Test.Test_Case (
         Description     => "Fail to find",
         Result          => Tash.Regexp.Match_Element (C, "xyz") = 0);

      declare
         Description : constant String := "Bad regular expression";
      begin
         Tash.Test.Test_Case (
            Description     => Description,
            Result          => Tash.Regexp.Match_Element (C, "(5") = 0);
      exception
         when Tash.Regexp.Regexp_Error =>
            Tash.Test.Test_Case (
               Description     => Description,
               Result          => True);
         when others =>
            Tash.Test.Test_Case (
               Description     => Description,
               Result          => False);
      end;

--      B := Tash.Regexp.Match ("/etc/rc2.d", "/([^/]+)\.([^/]+)$");
--Ada.Text_IO.Put_Line ("B=" & Tash.Lists.To_String (B));
--      Tash.Test.Test_Case (
--         Description => "Match and get subexpression in an Ada string",
--         Result      => Tash.Lists.To_String (B) = "/rc2.d rc2 d");

--      B := Tash.Regexp.Match_Element (C, "a str([ing]*)");
--      Tash.Test.Test_Case (
--         Description => "Match and get subexpression in a Tash list",
--         Result      => Tash.Lists.To_String (B) = "{a string} ing");

--      declare
--         Extension : Tash.Strings.Tash_String := Tash.Lists.Tail (
--            Tash.Regexp.Match ("test_regexp.adb", "\.(.*)$"));
--      begin
--         Tash.Test.Test_Case (
--            Description => "Use regexp match to get file extension",
--            Result      => Extension = "adb");
--      end;

   end;

   if Tash.Test.All_Test_Cases_Passed then
     Ada.Text_IO.Put_Line ("Test_Regexp PASSED");
   else
      Ada.Text_IO.Put_Line ("Test_Regexp FAILED");
   end if;

end Test_Regexp;
