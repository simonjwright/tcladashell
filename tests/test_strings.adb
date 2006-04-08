with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Tash.Strings;
with Tash.Strings.Test;
with Tash.Test;

procedure Test_Strings is

   use type Tash.Strings.Tash_String; -- to make operators visible

   Verbose : Boolean := False;

begin -- Test_Strings

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

   Tash.Test.Set_Verbose         (On => Verbose);
   Tash.Strings.Test.Set_Verbose (On => Verbose);

   declare

      A : aliased Tash.Strings.Tash_String;
      B : Tash.Strings.Tash_String;
      C : Tash.Strings.Tash_String;

   begin

      -------------------------------------------------
      -- Test conversion of Ada strings to Tash strings
      -------------------------------------------------
      Tash.Strings.Test.Test_Case (
         "Convert uninitialized Tash string to Ada string", A, "", 0);

      A := Tash.Strings.Null_Tash_String;
      Tash.Strings.Test.Test_Case (
         "Convert null Tash string to Ada string", A, "", 0);

      A := +"";
      Tash.Strings.Test.Test_Case (
         "Convert empty Ada string to Tash string", A, "", 1);

      A := +"A";
      Tash.Strings.Test.Test_Case (
         "Convert Ada string to Tash string", A, "A", 1);

      A := +"AB";
      Tash.Strings.Test.Test_Case (
         "Convert Ada string to Tash string", A, "AB", 1);

      -------------------------------------------------
      -- Test appending to a Tash string
      -------------------------------------------------
      A := +"A";
      Tash.Strings.Append (A, A);
      Tash.Strings.Test.Test_Case (
         "Append a Tash string to itself", A, "AA", 1);

      B := +"B";
      Tash.Strings.Append (A, B);
      Tash.Strings.Test.Test_Case (
         "Append a Tash string to another", A, "AAB", 1);
      Tash.Strings.Test.Test_Case (
         "Assure appended string did not change", B, "B", 1);

      Tash.Strings.Append (A, "C");
      Tash.Strings.Test.Test_Case (
         "Append an Ada string to a Tash string", A, "AABC", 1);

      declare
         T : Tash.Strings.Tash_String;
         U : Tash.Strings.Tash_String;
      begin
         T := +"T";
         Tash.Strings.Append (U, T);
         Tash.Strings.Test.Test_Case (
            "Append a Tash string to an uninitialized Tash string",
            U, "T", 1);
      end;

      declare
         T : Tash.Strings.Tash_String;
         U : Tash.Strings.Tash_String;
      begin
         T := +"T";
         Tash.Strings.Append (T, U);
         Tash.Strings.Test.Test_Case (
            "Append an uninitialized Tash string to a Tash string",
            T, "T", 1);
      end;

      declare
         T : Tash.Strings.Tash_String;
         U : Tash.Strings.Tash_String;
      begin
         Tash.Strings.Append (U, T);
         Tash.Strings.Test.Test_Case (
            "Append an uninitialized Tash string to another " &
            "uninitialized Tash string", U, "", 0);
      end;

      declare
         U : Tash.Strings.Tash_String;
      begin
         Tash.Strings.Append (U, U);
         Tash.Strings.Test.Test_Case (
            "Append an uninitialized Tash string to itself", U, "", 0);
      end;

      declare
         U : Tash.Strings.Tash_String;
      begin
         Tash.Strings.Append (U, "T");
         Tash.Strings.Test.Test_Case (
            "Append an Ada string to an uninitialized Tash string",
            U, "T", 1);
      end;

      -------------------------------------------------
      -- Test Tash string concatenation
      -------------------------------------------------
      A := +"A";
      Tash.Strings.Test.Test_Case (
         "Concatenate a Tash string with itself", A & A, "AA", 1);

      B := +"B";
      Tash.Strings.Test.Test_Case (
         "Concatenate a Tash string and another", A & B, "AB", 1);

      Tash.Strings.Test.Test_Case (
         "Concatenate an Ada string and a Tash string", A & "C", "AC", 1);
      Tash.Strings.Test.Test_Case (
         "Concatenate a Tash string and an Ada string", "C" & A, "CA", 1);

      declare
         T : Tash.Strings.Tash_String;
         U : Tash.Strings.Tash_String;
      begin
         T := +"T";
         Tash.Strings.Test.Test_Case (
            "Concatenate a Tash string and an uninitialized Tash string",
            U & T, "T", 2);
      end;

      declare
         T : Tash.Strings.Tash_String;
         U : Tash.Strings.Tash_String;
      begin
         T := +"T";
         Tash.Strings.Test.Test_Case (
            "Concatenate an uninitialized Tash string and a Tash string",
            T & U, "T", 2);
      end;

      declare
         T : Tash.Strings.Tash_String;
         U : Tash.Strings.Tash_String;
      begin
         Tash.Strings.Test.Test_Case (
            "Concatenate an uninitialized Tash string and " &
            "another uninitialized Tash string", T & U, "", 0);
      end;

      declare
         U : Tash.Strings.Tash_String;
      begin
         Tash.Strings.Test.Test_Case (
            "Concatenate an uninitialized Tash string with itself",
            U & U, "", 0);
      end;

      declare
         U : Tash.Strings.Tash_String;
      begin
         Tash.Strings.Test.Test_Case (
            "Concatenate an Ada string to an uninitialized Tash string",
            U & "T", "T", 1);
         Tash.Strings.Test.Test_Case (
            "Concatenate an uninitialized Tash string and an Ada string",
            "T" & U, "T", 1);
      end;

      -- Next To Do: Tash.Strings.Element

   end;

   if Tash.Test.All_Test_Cases_Passed then
      Ada.Text_IO.Put_Line ("Test_Strings PASSED");
   else
      Ada.Text_IO.Put_Line ("Test_Strings FAILED");
   end if;

end Test_Strings;

