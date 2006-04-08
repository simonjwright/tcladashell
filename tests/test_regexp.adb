with Ada.Command_Line;
with Ada.Strings;
with Ada.Text_IO;
with Tash.Integer_Lists;
with Tash.Float_Lists;
with Tash.Lists.Test;
with Tash.Regexp;
with Tash.Test;

procedure Test_Regexp is

   -- Make operators visible without qualification
   -----------------------------------------------
   use Tash.Float_Lists;
   use Tash.Integer_Lists;
   use type Tash.Lists.Tash_List;

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
      G : constant String := "This is a string";
      U : aliased Tash.Lists.Tash_List;

   begin

      -- prepare various Tash objects for testing Regexp
      --------------------------------------------------
      A := Tash.Integer_Lists.To_Tash_List (3) & 4;
      C := Tash.Integer_Lists.To_Tash_List (52) & 3.14159 &
              "This is a string" & A;

      Tash.Test.Test_Case (
         Description     => "Simple regexp on an Ada string",
         Result          => Tash.Regexp.Match ("This is a string", "string"));

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

      B := Tash.Regexp.Match ("/etc/rc2.d", "/([^/]+)\.([^/]+)$");
      Tash.Test.Test_Case (
         Description => "Match and get subexpression in an Ada string",
         Result      => Tash.Lists.To_String (B) = "/rc2.d rc2 d");

--      B := Tash.Regexp.Match_Element (C, "a str([ing]*)");
--      Tash.Test.Test_Case (
--         Description => "Match and get subexpression in a Tash list",
--         Result      => Tash.Lists.To_String (B) = "{a string} ing");

      declare
         Extension : constant String := Tash.Lists.Tail (
            Tash.Regexp.Match ("test.regexp.adb", "\.([^\.]*)$"));
      begin
         Tash.Test.Test_Case (
            Description => "Use regexp match to get file extension",
            Result      => Extension = "adb");
      end;

      Tash.Test.Test_Case (
         Description     => "Basic regular expression match succeeds",
         Result          => Tash.Regexp.Match (
            "abcccd", "abc*d", Mode => Tash.Regexp.Basic));

      Tash.Test.Test_Case (
         Description     => "Basic regular expression match fails",
         Result          => not Tash.Regexp.Match (
            "abcccd", "abc+d", Mode => Tash.Regexp.Basic));

      Tash.Test.Test_Case (
         Description     => "Extended regular expression match succeeds",
         Result          => Tash.Regexp.Match (
            "abcccd", "abc+d", Mode => Tash.Regexp.Extended));

      Tash.Test.Test_Case (
         Description     => "Advanced regular expression match succeeds",
         Result          => Tash.Regexp.Match (
            "digits: 012", "\d", Mode => Tash.Regexp.Advanced));

      Tash.Test.Test_Case (
         Description     => "Advanced regular expression match fails",
         Result          => not Tash.Regexp.Match (
            "no digits", "\d", Mode => Tash.Regexp.Advanced));

      Tash.Test.Test_Case (
         Description     => "Quoted regular expression match succeeds",
         Result          => Tash.Regexp.Match (
            "abcccd", "abc", Mode => Tash.Regexp.Quote));

      Tash.Test.Test_Case (
         Description     => "Quoted regular expression match fails",
         Result          => not Tash.Regexp.Match (
            "abcccd", "abc*", Mode => Tash.Regexp.Quote));

      Tash.Test.Test_Case (
         Description     => "Advanced, expanded regular expression match succeeds",
         Result          => Tash.Regexp.Match (
            "digits: 012", "\d #put comment here!", Expanded => True));

      Tash.Test.Test_Case (
         Description     => "Ignore case in regular expression match 1",
         Result          => Tash.Regexp.Match (
            "ALL CAPS", "all caps", Ignore_Case => True));

      Tash.Test.Test_Case (
         Description     => "Ignore case in regular expression match 2",
         Result          => Tash.Regexp.Match (
            "all caps", "ALL CAPS", Ignore_Case => True));

      Tash.Test.Test_Case (
         Description     => "Regular expression substitution",
         Result          => Tash.Regexp.Substitute (
            "abbd", "b+", "ccc") = "acccd");

      Tash.Test.Test_Case (
         Description     => "Substitute only first pattern",
         Result          => Tash.Regexp.Substitute (
            "abbdbbx", "b+", "ccc") = "acccdbbx");

      Tash.Test.Test_Case (
         Description     => "Substitute all patterns",
         Result          => Tash.Regexp.Substitute (
            "abbdbbx", "b+", "ccc", Sub_All => True) = "acccdcccx");

      Tash.Test.Test_Case (
         Description     => "Substitute first pattern using expanded pattern",
         Result          => Tash.Regexp.Substitute (
            "abbdbbx", "b+ #comment", "ccc", Expanded => True) = "acccdbbx");

      Tash.Test.Test_Case (
         Description     => "Substitute all patterns, ignoring case",
         Result          => Tash.Regexp.Substitute (
              "abbdBBx", "b+", "ccc", Sub_All => True, Ignore_Case => True)
            = "acccdcccx");

   end;

   if Tash.Test.All_Test_Cases_Passed then
     Ada.Text_IO.Put_Line ("Test_Regexp PASSED");
   else
      Ada.Text_IO.Put_Line ("Test_Regexp FAILED");
   end if;

end Test_Regexp;
