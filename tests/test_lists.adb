with Ada.Command_Line;
with Ada.Text_IO;
with Tash.Float_Lists;
with Tash.Integer_Lists;
with Tash.Lists.Test;
with Tash.Test;

procedure Test_Lists is

   package TL renames Tash.Lists;

   --  Make operators visible without qualification
   ------------------------------------------------
   use type Tash.Lists.Tash_List;
   use Tash.Integer_Lists;
   use Tash.Float_Lists;

   Verbose : Boolean := False;

begin --  Test_Lists

   -------------------------------------------
   --  Check for -verbose command line argument
   -------------------------------------------
   GET_COMMAND_LINE_ARGUMENTS : for I in  1 ..
        Ada.Command_Line.Argument_Count
   loop
      if Ada.Command_Line.Argument (I) = "-verbose" then
         Verbose := True;
      end if;
   end loop GET_COMMAND_LINE_ARGUMENTS;

   Tash.Test.Set_Verbose (On => Verbose);
   Tash.Lists.Test.Set_Verbose (On => Verbose);

   declare

      A : Tash.Lists.Tash_List;
      B : Tash.Lists.Tash_List;
      C : Tash.Lists.Tash_List;
      D : Tash.Lists.Tash_List;
      U : aliased Tash.Lists.Tash_List;

   begin

      -------------------------------------------------------
      --  Test Is_Null and Is_Empty on uninitialized Tash list
      -------------------------------------------------------

      Tash.Test.Test_Case
        (Description => "Uninitialized Tash list is null",
         Result      => Tash.Lists.Is_Null (U));

      Tash.Test.Test_Case
        (Description => "Uninitialized Tash list is empty",
         Result      => Tash.Lists.Is_Empty (U));

      Tash.Test.Test_Case
        (Description => "Convert uninitialized Tash list to Ada string",
         Result      => Tash.Lists.To_String (U) = "");

      Tash.Lists.Test.Test_Case
        (Description     =>
           "Check length and reference count of uninitialized Tash list",
         Actual_Result   => U,
         Expected_Result => "",
         Expected_Type   => "",
         Expected_Count  => 0,
         Expected_Length => 0);

      ---------------------------------------------------------------
      --  Test Is_Null and Is_Empty on Tash list set to Null_Tash_List
      ---------------------------------------------------------------

      A := Tash.Lists.Null_Tash_List;

      Tash.Test.Test_Case
        (Description => "Tash list initialized to Null_Tash_List is null",
         Result      => Tash.Lists.Is_Null (A));

      Tash.Test.Test_Case
        (Description => "Tash list initialized to Null_Tash_List is empty",
         Result      => Tash.Lists.Is_Empty (A));

      Tash.Test.Test_Case
        (Description =>
           "Convert Tash list initialized to Null_Tash_List to Ada string",
         Result      => Tash.Lists.To_String (A) = "");

      Tash.Lists.Test.Test_Case
        (Description     =>
"Check length and reference count of Tash list initialized to Null_Tash_List",
         Actual_Result   => A,
         Expected_Result => "",
         Expected_Type   => "",
         Expected_Count  => 0,
         Expected_Length => 0);

      -------------------------------------------------
      --  Test creation of Tash lists
      -------------------------------------------------

      A := Tash.Lists.To_Tash_List ("test");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("test")
      --
      ------------------------------------------------------------------

      Tash.Test.Test_Case
        (Description => "Verify that we have a string as an element",
         Result      => Tash.Lists.Element_Is_String (A, 1));
      Tash.Test.Test_Case
        (Description => "Verify that it is not a list element",
         Result      => not Tash.Lists.Element_Is_List (A, 1));
      Tash.Test.Test_Case
        (Description => "Verify that it is not an integer element",
         Result      => not Tash.Integer_Lists.Element_Is_Integer (A, 1));
      Tash.Test.Test_Case
        (Description => "Verify that it is not a float element",
         Result      => not Tash.Float_Lists.Element_Is_Float (A, 1));

      Tash.Lists.Test.Test_Case
        (Description     => "Create Tash list from a string with no blanks",
         Actual_Result   => A,
         Expected_Result => "test",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Test.Test_Case
        (Description => "Tash list is not null",
         Result      => not Tash.Lists.Is_Null (A));
      Tash.Test.Test_Case
        (Description => "Tash list is not empty",
         Result      => not Tash.Lists.Is_Empty (A));
      Tash.Test.Test_Case
        (Description => "Convert Tash list to Ada string",
         Result      => Tash.Lists.To_String (A) = "test");
      Tash.Test.Test_Case
        (Description => "Verify value of list element",
         Result      => Tash.Lists.Get_Element (A, 1) = "test");

      A := Tash.Lists.To_Tash_List ("another test");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("another", "test")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Create Tash list from a string with blanks",
         Actual_Result   => A,
         Expected_Result => "another test",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (A, 1) = "another");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (A, 2) = "test");

      A := Tash.Lists.To_Tash_List ("  {another}     {test}   ");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("another", "test")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Create Tash list from a string with braces",
         Actual_Result   => A,
         Expected_Result => "another test",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (A, 1) = "another");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (A, 2) = "test");

      A := Tash.Lists.To_Tash_List (" {another  test} ");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("another  test")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Create Tash list from a string with braces",
         Actual_Result   => A,
         Expected_Result => "{another  test}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (A, 1) = "another  test");

      declare
         Description : constant String :=
            "Create Tash list from a string with bad braces";
      begin
         A := Tash.Lists.To_Tash_List ("{another test");
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Lists.List_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
      end;

      Tash.Lists.Test.Test_Case
        (Description     => "Create Tash list should still be same as before",
         Actual_Result   => A,
         Expected_Result => "{another  test}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (A, 1) = "another  test");

      A := Tash.Lists.To_Tash_List (" \{another   test ");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("{another", "test")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     =>
           "Create Tash list from a string with escaped brace",
         Actual_Result   => A,
         Expected_Result => "\{another test",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (A, 1) = "{another");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (A, 2) = "test");

      A := +"use plus operator";

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("use", "plus", "operator")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     =>
           "Create Tash list from a string using plus operator",
         Actual_Result   => A,
         Expected_Result => "use plus operator",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (A, 1) = "use");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (A, 2) = "plus");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 3",
         Result      => Tash.Lists.Get_Element (A, 3) = "operator");

      A := +"test";
      B := +"concatenation";
      C := A & B;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("test")
      --    list(B) -> ("concatenation")
      --    list(C) -> ("test", "concatenation")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Test Tash list concatenation operator",
         Actual_Result   => C,
         Expected_Result => "test concatenation",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (C, 1) = "test");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (C, 2) = "concatenation");

      C := "another" & B;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("test")
      --    list(B) -> ("concatenation")
      --    list(C) -> ("another", "concatenation")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Test Tash list concatenation operator",
         Actual_Result   => C,
         Expected_Result => "another concatenation",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (C, 1) = "another");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (C, 2) = "concatenation");

      C := A & "another";

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("test")
      --    list(B) -> ("concatenation")
      --    list(C) -> ("test", "another")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Test Tash list concatenation operator",
         Actual_Result   => C,
         Expected_Result => "test another",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (C, 1) = "test");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (C, 2) = "another");

      C := (A & "multiple") & "concatenations";

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("test")
      --    list(B) -> ("concatention")
      --    list(C) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Test Tash list concatenation operator",
         Actual_Result   => C,
         Expected_Result => "test multiple concatenations",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (C, 1) = "test");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (C, 2) = "multiple");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 3",
         Result      => Tash.Lists.Get_Element (C, 3) = "concatenations");

      D := Tash.Lists.Copy (C);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("test")
      --    list(B) -> ("concatention")
      --    list(C) -> ("test", "multiple", "concatentions")
      --    list(D) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Copy a Tash list",
         Actual_Result   => D,
         Expected_Result => "test multiple concatenations",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (D, 1) = "test");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (D, 2) = "multiple");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 3",
         Result      => Tash.Lists.Get_Element (D, 3) = "concatenations");

      D := Tash.Lists.Duplicate (C);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("test")
      --    list(B) -> ("concatention")
      --    list(C) -> ("test", "multiple", "concatentions")
      --    list(D) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Duplicate a Tash list",
         Actual_Result   => D,
         Expected_Result => "test multiple concatenations",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);
      Tash.Test.Test_Case
        (Description => "Verify value of list element 1",
         Result      => Tash.Lists.Get_Element (D, 1) = "test");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 2",
         Result      => Tash.Lists.Get_Element (D, 2) = "multiple");
      Tash.Test.Test_Case
        (Description => "Verify value of list element 3",
         Result      => Tash.Lists.Get_Element (D, 3) = "concatenations");

      B := Tash.Lists.Split ("The quick brown fox jumps over the lazy dog");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("test")
      --    list(B) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the",  "lazy",  "dog")
      --    list(C) -> ("test", "multiple", "concatentions")
      --    list(D) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     =>
           "Make a Tash list by splitting a string at spaces",
         Actual_Result   => B,
         Expected_Result => "The quick brown fox jumps over the lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 9);

      B := Tash.Lists.Split (Str => "/usr/X/lib/X11", Split_At => "/");
      Tash.Lists.Test.Test_Case
        (Description     =>
           "Make a Tash list from splitting a unix path name",
         Actual_Result   => B,
         Expected_Result => "{} usr X lib X11",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 5);

      Tash.Test.Test_Case
        (Description => "Join the list to make Windows path name",
         Result      => Tash.Lists.Join (B, "\") = "\usr\X\lib\X11");

      B :=
         Tash.Lists.Split (Str => "/usr/X/lib/X11/pix map", Split_At => "/");
      Tash.Lists.Test.Test_Case
        (Description     =>
           "Check that Split does not split at embedded spaces",
         Actual_Result   => B,
         Expected_Result => "{} usr X lib X11 {pix map}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 6);

      B :=
         Tash.Lists.Split
           ("The quick brown fox jumps" &
            ASCII.CR &
            "over" &
            ASCII.LF &
            "the" &
            ASCII.HT &
            "lazy dog");
      Tash.Lists.Test.Test_Case
        (Description     => "Check default split characters",
         Actual_Result   => B,
         Expected_Result => "The quick brown fox jumps over the lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 9);

      B :=
         Tash.Lists.Split
           (" The  quick brown fox jumps" &
            ASCII.CR &
            "over" &
            ASCII.LF &
            "the" &
            ASCII.HT &
            "lazy dog  ");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("test")
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("test", "multiple", "concatentions")
      --    list(D) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     =>
           "Check leading, trailing, and adjacent split characters",
         Actual_Result   => B,
         Expected_Result =>
           "{} The {} quick brown fox jumps over the lazy dog {} {}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 13);

      A := Tash.Lists.Slice (B, 2, 2) & Tash.Lists.Slice (B, 4, 11);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog")
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("test", "multiple", "concatentions")
      --    list(D) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Create a new tash list using Slice",
         Actual_Result   => A,
         Expected_Result => "The quick brown fox jumps over the lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 9);

      C := Tash.Lists.Slice (A, 9, 11);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog")
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("dog")
      --    list(D) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Get a slice at the tail of a list",
         Actual_Result   => C,
         Expected_Result => "dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);

      Tash.Lists.Test.Test_Case
        (Description     => "Get a slice at head of list",
         Actual_Result   => Tash.Lists.Slice (A, 1, 3),
         Expected_Result => "The quick brown",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Test.Test_Case
        (Description => "An empty slice is a null list",
         Result      => Tash.Lists.Is_Null (Tash.Lists.Slice (A, 1, 0)));

      Tash.Test.Test_Case
        (Description => "An empty slice is a null list",
         Result      => Tash.Lists.Is_Null (Tash.Lists.Slice (A, 4, 3)));

      C := Tash.Lists.To_Tash_List ("bigboy bigBoy bigbang");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog")
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> (bigboy", "bigBoy", "bigbang")
      --    list(D) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "ASCII sort, increasing",
         Actual_Result   => Tash.Lists.Sort (C, TL.SM_ASCII, TL.Increasing),
         Expected_Result => "bigBoy bigbang bigboy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      D := Tash.Lists.Sort (C, TL.SM_ASCII, TL.Decreasing);
      Tash.Test.Test_Case
        (Description => "ASCII sort, decreasing",
         Result      => Tash.Lists.To_String (D) = "bigboy bigbang bigBoy");
      Tash.Lists.Test.Test_Case
        (Description     => "ASCII sort, decreasing",
         Actual_Result   => D,
         Expected_Result => "bigboy bigbang bigBoy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Lists.Test.Test_Case
        (Description     => "Dictionary sort, increasing",
         Actual_Result   =>
            Tash.Lists.Sort (C, TL.SM_Dictionary, TL.Increasing),
         Expected_Result => "bigbang bigBoy bigboy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Lists.Test.Test_Case
        (Description     => "Dictionary sort, decreasing",
         Actual_Result   =>
            Tash.Lists.Sort (C, TL.SM_Dictionary, TL.Decreasing),
         Expected_Result => "bigboy bigBoy bigbang",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      C := Tash.Integer_Lists.To_Tash_List (8);
      Tash.Integer_Lists.Append (C, 10);
      Tash.Integer_Lists.Append (C, 6);
      Tash.Integer_Lists.Append (C, 1);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog")
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> (8, 10, 6, 1)
      --    list(D) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Test.Test_Case
        (Description => "Verify that we have an integer element",
         Result      => Tash.Integer_Lists.Element_Is_Integer (C, 1));
      Tash.Test.Test_Case
        (Description => "Verify that it is not a list element",
         Result      => not Tash.Lists.Element_Is_List (C, 1));
      Tash.Test.Test_Case
        (Description => "Verify that it is not a string element",
         Result      => not Tash.Lists.Element_Is_String (C, 1));
      Tash.Test.Test_Case
        (Description => "Verify that it is not a float element",
         Result      => not Tash.Float_Lists.Element_Is_Float (C, 1));

      Tash.Lists.Test.Test_Case
        (Description     => "ASCII sort integers, increasing",
         Actual_Result   => Tash.Lists.Sort (C, TL.SM_ASCII, TL.Increasing),
         Expected_Result => "1 10 6 8",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Test.Test_Case
        (Description     => "Dictionary sort integers, increasing",
         Actual_Result   =>
            Tash.Lists.Sort (C, TL.SM_Dictionary, TL.Increasing),
         Expected_Result => "1 6 8 10",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Test.Test_Case
        (Description     => "Integer sort integers, increasing",
         Actual_Result   => Tash.Lists.Sort (C, TL.SM_Integer, TL.Increasing),
         Expected_Result => "1 6 8 10",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      C := Tash.Float_Lists.To_Tash_List (267.5);
      Tash.Float_Lists.Append (C, 1.25);
      Tash.Float_Lists.Append (C, 7.125);
      Tash.Float_Lists.Append (C, -0.03125);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog")
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> (267.5, 1.25, 7.125, -0.03125)
      --    list(D) -> ("test", "multiple", "concatentions")
      --
      ------------------------------------------------------------------

      Tash.Test.Test_Case
        (Description => "Verify that we have a float element",
         Result      => Tash.Float_Lists.Element_Is_Float (C, 1));
      Tash.Test.Test_Case
        (Description => "Verify that it is not a list element",
         Result      => not Tash.Lists.Element_Is_List (C, 1));
      Tash.Test.Test_Case
        (Description => "Verify that it is not a string element",
         Result      => not Tash.Lists.Element_Is_String (C, 1));
      Tash.Test.Test_Case
        (Description => "Verify that it is not an integer element",
         Result      => not Tash.Integer_Lists.Element_Is_Integer (C, 1));

      Tash.Lists.Test.Test_Case
        (Description     => "ASCII sort reals, increasing",
         Actual_Result   => Tash.Lists.Sort (C, TL.SM_ASCII, TL.Increasing),
         Expected_Result => "-0.03125 1.25 267.5 7.125",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Test.Test_Case
        (Description     => "ASCII sort reals, decreasing",
         Actual_Result   => Tash.Lists.Sort (C, TL.SM_ASCII, TL.Decreasing),
         Expected_Result => "7.125 267.5 1.25 -0.03125",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Test.Test_Case
        (Description     => "Real sort reals, increasing",
         Actual_Result   => Tash.Lists.Sort (C, TL.SM_Real, TL.Increasing),
         Expected_Result => "-0.03125 1.25 7.125 267.5",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Test.Test_Case
        (Description     => "Real sort reals, decreasing",
         Actual_Result   => Tash.Lists.Sort (C, TL.SM_Real, TL.Decreasing),
         Expected_Result => "267.5 7.125 1.25 -0.03125",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      declare
         Description : constant String := "Integer sort with bad string data";
      begin
         D := Tash.Lists.Sort (C, TL.SM_Integer, TL.Increasing);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Lists.List_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
      end;

      ------------------------------------------------------------------
      --  Test Get_Element
      ------------------------------------------------------------------

      declare
         Description : constant String :=
            "Getting an element beyond the end of a list raises List_Error";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Lists.Get_Element (C, 100) = "");
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Lists.List_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
      end;

      Tash.Test.Test_Case
        (Description => "Get a float element as a string",
         Result      => Tash.Lists.Get_Element (C, 1) = "267.5");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog")
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> (267.5, 1.25, 7.125, -0.03125)
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Append_List (A, D);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> (267.5, 1.25, 7.125, -0.03125)
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Test.Test_Case
        (Description => "Verify that we have a list as an element",
         Result      => Tash.Lists.Element_Is_List (A, 10));
      Tash.Test.Test_Case
        (Description => "Verify that it is not a string element",
         Result      => not Tash.Lists.Element_Is_String (A, 10));
      Tash.Test.Test_Case
        (Description => "Verify that it is not an integer element",
         Result      => not Tash.Integer_Lists.Element_Is_Integer (A, 10));
      Tash.Test.Test_Case
        (Description => "Verify that it is not a float element",
         Result      => not Tash.Float_Lists.Element_Is_Float (A, 10));

      Tash.Lists.Test.Test_Case
        (Description     => "Get an element which is a list",
         Actual_Result   => Tash.Lists.Get_Element (A, 10),
         Expected_Result => "bigboy bigbang bigBoy",
         Expected_Type   => "list",
         Expected_Count  => 3,
         Expected_Length => 3);

      Tash.Lists.Test.Test_Case
        (Description     => "Get a string element as a list",
         Actual_Result   => Tash.Lists.Get_Element (A, 1),
         Expected_Result => "The",
         Expected_Type   => "list",
         Expected_Count  => 2,
         Expected_Length => 1);

      ------------------------------------------------------------------
      --  Test Head and Tail
      ------------------------------------------------------------------

      Tash.Test.Test_Case
        (Description => "Get head element of a list, a string",
         Result      => Tash.Lists.Head (A) = "The");
      Tash.Test.Test_Case
        (Description => "Get head element of a list, a string",
         Result      => Tash.Lists.Head (C) = "267.5");
      Tash.Lists.Test.Test_Case
        (Description     => "Get head element of a list as a list",
         Actual_Result   => Tash.Lists.Head (A),
         Expected_Result => "The",
         Expected_Type   => "list",
         Expected_Count  => 2,
         Expected_Length => 1);

      Tash.Test.Test_Case
        (Description => "Get tail element of a list, a string",
         Result      => Tash.Lists.Tail (D) = "bigBoy");
      Tash.Test.Test_Case
        (Description => "Get tail element of a list, a string",
         Result      => Tash.Lists.Tail (C) = "-0.03125");
      Tash.Lists.Test.Test_Case
        (Description     => "Get tail element of a list, a list",
         Actual_Result   => Tash.Lists.Tail (A),
         Expected_Result => "bigboy bigbang bigBoy",
         Expected_Type   => "list",
         Expected_Count  => 3,
         Expected_Length => 3);

      ------------------------------------------------------------------
      --  Test Append
      ------------------------------------------------------------------

      C := Tash.Lists.Duplicate (D);
      Tash.Lists.Append (C, "submarine");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("bigboy", "bigbang", "bigBoy", "submarine")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Append a string to a list",
         Actual_Result   => C,
         Expected_Result => "bigboy bigbang bigBoy submarine",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Append_Elements (C, Tash.Lists.To_Tash_List ("high five"));

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("bigboy", "bigbang", "bigBoy", "submarine"
      --                "high", "five")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Append the elements of a list to another list",
         Actual_Result   => C,
         Expected_Result => "bigboy bigbang bigBoy submarine high five",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 6);

      ------------------------------------------------------------------
      --  Test Replace
      ------------------------------------------------------------------

      Tash.Lists.Replace_Element (C, 4, "hoagy");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("bigboy", "bigbang", "bigBoy", "hoagy"
      --                "high", "five")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace a list element",
         Actual_Result   => C,
         Expected_Result => "bigboy bigbang bigBoy hoagy high five",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 6);

      Tash.Lists.Replace_Element_With_Elements (C, 6, D);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("bigboy", "bigbang", "bigBoy", "hoagy"
      --                "high", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace a list element with individual elements",
         Actual_Result   => C,
         Expected_Result =>
           "bigboy bigbang bigBoy hoagy high bigboy bigbang bigBoy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);

      Tash.Lists.Replace_Element_With_List (C, 5, D);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("bigboy", "bigbang", "bigBoy", "hoagy"
      --                "bigboy", "bigbang", "bigBoy",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace a list element with a list",
         Actual_Result   => C,
         Expected_Result =>
"bigboy bigbang bigBoy hoagy {bigboy bigbang bigBoy} bigboy bigbang bigBoy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);

      Tash.Lists.Replace_Slice (C, 2, 8, "fish");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("bigboy", "fish")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace a slice with an element",
         Actual_Result   => C,
         Expected_Result => "bigboy fish",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      Tash.Lists.Replace_Slice_With_Elements
        (C,
         1,
         1,
         Tash.Lists.To_Tash_List ("a fine kettle of"));

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("a", "fine", "kettle", "of", "fish")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace a list element with individual elements",
         Actual_Result   => C,
         Expected_Result => "a fine kettle of fish",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 5);

      Tash.Lists.Replace_Slice_With_List
        (C,
         1,
         2,
         Tash.Lists.To_Tash_List ("one very fine"));

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> (("one", "very", "fine"), "kettle", "of", "fish")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace a list element with a list",
         Actual_Result   => C,
         Expected_Result => "{one very fine} kettle of fish",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Insert (A, 9, "fat");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> (("one", "very", "fine"), "kettle", "of", "fish")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Insert an element into a list",
         Actual_Result   => A,
         Expected_Result =>
"The quick brown fox jumps over the lazy fat dog {bigboy bigbang bigBoy}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 11);

      Tash.Lists.Insert_List (C, 3, D);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> (("one", "very", "fine"), "kettle",
      --                ("bigboy", "bigbang", "bigBoy"), "of", "fish")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Insert a list into a list",
         Actual_Result   => C,
         Expected_Result =>
           "{one very fine} kettle {bigboy bigbang bigBoy} of fish",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 5);

      Tash.Lists.Delete_Element (C, 3);
      Tash.Lists.Insert_Elements (C, 3, D);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> (("one", "very", "fine"), "kettle",
      --                "bigboy", "bigbang", "bigBoy", "of", "fish")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Insert a list into a list",
         Actual_Result   => C,
         Expected_Result =>
           "{one very fine} kettle bigboy bigbang bigBoy of fish",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 7);

      Tash.Lists.Delete_Element (C, 1);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy", "of", "fish")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Delete first element of a list",
         Actual_Result   => C,
         Expected_Result => "kettle bigboy bigbang bigBoy of fish",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 6);

      Tash.Lists.Delete_Element (C, Tash.Lists.Length (C));
      Tash.Lists.Delete_Element (C, Tash.Lists.Length (C));

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Delete last two elements of a list",
         Actual_Result   => C,
         Expected_Result => "kettle bigboy bigbang bigBoy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Delete_Element (C, Tash.Lists.Length (C) + 1);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog", "{}", "{}")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Delete an element past the end of a list",
         Actual_Result   => C,
         Expected_Result => "kettle bigboy bigbang bigBoy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Delete_Slice (B, 12, 13);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("{}", "The", "{}", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Delete a slice at the end of a list",
         Actual_Result   => B,
         Expected_Result =>
           "{} The {} quick brown fox jumps over the lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 11);

      Tash.Lists.Delete_Slice (B, 1, 3);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "dog")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Delete a slice at the head of a list",
         Actual_Result   => B,
         Expected_Result => "quick brown fox jumps over the lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);

      Tash.Lists.Delete_Slice (B, 2, 6);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("quick", "lazy", "dog")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Delete a slice in the middle of a list",
         Actual_Result   => B,
         Expected_Result => "quick lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Lists.Delete_Slice (B, 3, 6);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("quick", "lazy")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Delete a slice that goes past the end of a list",
         Actual_Result   => B,
         Expected_Result => "quick lazy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      Tash.Lists.Delete_Slice (B, 1, 0);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("quick", "lazy")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Delete an empty slice",
         Actual_Result   => B,
         Expected_Result => "quick lazy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      Tash.Lists.Delete_Element (B, 1);
      Tash.Lists.Push (B, "the");

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("the", "lazy")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Push a string onto head of a list",
         Actual_Result   => B,
         Expected_Result => "the lazy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      Tash.Lists.Push_List (B, Tash.Lists.To_Tash_List ("fox jumps over"));

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> (("fox", "jumps", "over"), "the", "lazy")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Push a list onto head of a list",
         Actual_Result   => B,
         Expected_Result => "{fox jumps over} the lazy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Lists.Push_Elements
        (B,
         Tash.Lists.To_Tash_List ("The quick brown"));

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("The", "quick", "brown",
      --                ("fox", "jumps", "over"), "the", "lazy")
      --    list(C) -> ("kettle", "bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Push a list onto head of a list",
         Actual_Result   => B,
         Expected_Result => "The quick brown {fox jumps over} the lazy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 6);

      declare
         E : Tash.Lists.Tash_List;
      begin
         Tash.Lists.Push (E, "string");
         Tash.Lists.Test.Test_Case
           (Description     =>
              "Push a string onto the head of an uninitialized list",
            Actual_Result   => E,
            Expected_Result => "string",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 1);
      end;

      Tash.Lists.Pop (U);
      Tash.Test.Test_Case
        (Description =>
           "Popping an element from an uninitialized list has no effect",
         Result      => Tash.Lists.Is_Null (U));

      Tash.Lists.Pop (C);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> ("The", "quick", "brown",
      --                ("fox", "jumps", "over"), "the", "lazy")
      --    list(C) -> ("bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Pop an element from a list",
         Actual_Result   => C,
         Expected_Result => "bigboy bigbang bigBoy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Lists.Replace_Slice_With_List
        (B,
         1,
         3,
         Tash.Lists.To_Tash_List ("The quick brown"));
      Tash.Lists.Test.Test_Case
        (Description     => "Replace a slice with a list",
         Actual_Result   => B,
         Expected_Result => "{The quick brown} {fox jumps over} the lazy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);
      Tash.Lists.Pop (B);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> (("fox", "jumps", "over"), "the", "lazy")
      --    list(C) -> ("bigboy", "bigbang", "bigBoy")
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Pop a list from a list",
         Actual_Result   => B,
         Expected_Result => "{fox jumps over} the lazy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Test.Test_Case
        (Description => "Compare two lists as strings for =",
         Result      => C = D);

      Tash.Test.Test_Case
        (Description => "Compare two lists as strings for /=",
         Result      => C /= B);

      Tash.Test.Test_Case
        (Description => "Compare two lists as strings for >=",
         Result      => C >= D);

      Tash.Test.Test_Case
        (Description => "Compare two lists as strings for <=",
         Result      => C <= D);

      Tash.Test.Test_Case
        (Description => "Compare two lists as strings for >",
         Result      => not (C > D));

      Tash.Test.Test_Case
        (Description => "Compare two lists as strings for <",
         Result      => not (C < D));

      --  Note the use of Ada string compare here, not "dictionary" order.
      Tash.Test.Test_Case
        (Description => "Compare two lists as strings for >",
         Result      => C > A);

      Tash.Test.Test_Case
        (Description => "Compare two lists as strings for <",
         Result      => A < C);

      C := +8;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> ("The", "quick", "brown", "fox", "jumps",
      --                "over", "the", "lazy", "fat", "dog",
      --                ("bigboy", "bigbang", "bigBoy"))
      --    list(B) -> (("fox", "jumps", "over"), "the", "lazy")
      --    list(C) -> (8)
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Create a tash list from an integer",
         Actual_Result   => C,
         Expected_Result => "8",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);

      A := +1;
      B := -2;
      C := A & B;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1)
      --    list(B) -> (-2)
      --    list(C) -> (1, -2)
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Concatenate two integer tash lists",
         Actual_Result   => C,
         Expected_Result => "1 -2",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      C := C & 3;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1)
      --    list(B) -> (-2)
      --    list(C) -> (1, -2, 3)
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Concatenate a list with an integer",
         Actual_Result   => C,
         Expected_Result => "1 -2 3",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      C := Integer'(-4) & C;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1)
      --    list(B) -> (-2)
      --    list(C) -> (-4, 1, -2, 3)
      --    list(D) -> ("bigboy", "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Concatenate an integer and a tash list",
         Actual_Result   => C,
         Expected_Result => "-4 1 -2 3",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Test.Test_Case
        (Description => "Get an integer element at head of a list",
         Result      => Tash.Integer_Lists.Get_Element (C, 1) = -4);

      Tash.Test.Test_Case
        (Description => "Get an integer element at tail of a list",
         Result      => Tash.Integer_Lists.Get_Element (C, 4) = 3);

      declare
         Description : constant String :=
            "Try to get an integer past the end of a list";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Integer_Lists.Get_Element (C, 5) = 0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Lists.List_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
      end;

      declare
         Description : constant String :=
           "Raise Constraint_Error by using Integer_Lists "
             & "to get a string element"
;
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Integer_Lists.Get_Element (D, 1) = 0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Constraint_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
      end;

      Tash.Test.Test_Case
        (Description => "Get an integer element at head of a list",
         Result      => Tash.Integer_Lists.Head (C) = -4);

      Tash.Test.Test_Case
        (Description => "Get an integer element at tail of a list",
         Result      => Tash.Integer_Lists.Tail (C) = 3);

      Tash.Integer_Lists.Replace_Element (D, 1, 38);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1)
      --    list(B) -> (-2)
      --    list(C) -> (-4, 1, -2, 3)
      --    list(D) -> (38, "bigbang", "bigBoy")
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace an integer element",
         Actual_Result   => D,
         Expected_Result => "38 bigbang bigBoy",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Integer_Lists.Replace_Slice (D, 2, 3, 83);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1)
      --    list(B) -> (-2)
      --    list(C) -> (-4, 1, -2, 3)
      --    list(D) -> (38, 83)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace slice of a list with an integer",
         Actual_Result   => D,
         Expected_Result => "38 83",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      Tash.Integer_Lists.Insert (D, 2, 45);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1)
      --    list(B) -> (-2)
      --    list(C) -> (-4, 1, -2, 3)
      --    list(D) -> (38, 45, 83)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Insert an integer into a list",
         Actual_Result   => D,
         Expected_Result => "38 45 83",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Integer_Lists.Push (D, 690);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1)
      --    list(B) -> (-2)
      --    list(C) -> (-4, 1, -2, 3)
      --    list(D) -> (690, 38, 45, 83)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Push an integer onto a list",
         Actual_Result   => D,
         Expected_Result => "690 38 45 83",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      C := +8.125;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1)
      --    list(B) -> (-2)
      --    list(C) -> (8.125)
      --    list(D) -> (690, 38, 45, 83)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Create a tash list from an float",
         Actual_Result   => C,
         Expected_Result => "8.125",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);

      A := +1.25;
      B := -2.375;
      C := A & B;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1.25)
      --    list(B) -> (-2.375)
      --    list(C) -> (1.25, -2.375)
      --    list(D) -> (690, 38, 45, 83)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Concatenate two float tash lists",
         Actual_Result   => C,
         Expected_Result => "1.25 -2.375",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      C := C & 3.4375;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1.25)
      --    list(B) -> (-2.375)
      --    list(C) -> (1.25, -2.375, 3.4375)
      --    list(D) -> (690, 38, 45, 83)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Concatenate a list with an float",
         Actual_Result   => C,
         Expected_Result => "1.25 -2.375 3.4375",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      C := Float'(-4.5) & C;

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1.25)
      --    list(B) -> (-2.375)
      --    list(C) -> (-4.5, 1.25, -2.375, 3.4375)
      --    list(D) -> (690, 38, 45, 83)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Concatenate an float and a tash list",
         Actual_Result   => C,
         Expected_Result => "-4.5 1.25 -2.375 3.4375",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Test.Test_Case
        (Description => "Get an float element at head of a list",
         Result      => Tash.Float_Lists.Get_Element (C, 1) = -4.5);

      Tash.Test.Test_Case
        (Description => "Get an float element at tail of a list",
         Result      => Tash.Float_Lists.Get_Element (C, 4) = 3.4375);

      declare
         Description : constant String :=
            "Try to get an float past the end of a list";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Float_Lists.Get_Element (C, 5) = 0.0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Lists.List_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
      end;

      declare
         Description : constant String :=
           "Raise Constraint_Error by using Float_Lists "
             & "to get an integer element";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Float_Lists.Get_Element (D, 1) = 0.0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Constraint_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
      end;

      Tash.Test.Test_Case
        (Description => "Get an float element at head of a list",
         Result      => Tash.Float_Lists.Head (C) = -4.5);

      Tash.Test.Test_Case
        (Description => "Get an float element at tail of a list",
         Result      => Tash.Float_Lists.Tail (C) = 3.4375);

      Tash.Float_Lists.Replace_Element (D, 2, 38.75);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1.25)
      --    list(B) -> (-2.375)
      --    list(C) -> (-4.5, 1.25, -2.375, 3.4375)
      --    list(D) -> (690, 38.75, 45, 83)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace a float element",
         Actual_Result   => D,
         Expected_Result => "690 38.75 45 83",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Float_Lists.Replace_Slice (D, 3, 4, 83.875);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1.25)
      --    list(B) -> (-2.375)
      --    list(C) -> (-4.5, 1.25, -2.375, 3.4375)
      --    list(D) -> (690 38.75, 83.875)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Replace slice of a list with a float",
         Actual_Result   => D,
         Expected_Result => "690 38.75 83.875",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Float_Lists.Insert (D, 2, 45.0);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1.25)
      --    list(B) -> (-2.375)
      --    list(C) -> (-4.5, 1.25, -2.375, 3.4375)
      --    list(D) -> (38.75, 45.0, 38.75, 83.875)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Insert a float into a list",
         Actual_Result   => D,
         Expected_Result => "690 45.0 38.75 83.875",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Float_Lists.Push (D, 369.0);

      -- Current State -------------------------------------------------
      --
      --    list(A) -> (1.25)
      --    list(B) -> (-2.375)
      --    list(C) -> (-4.5, 1.25, -2.375, 3.4375)
      --    list(D) -> (369.0, 690, 45.0, 38.75, 83.9)
      --
      ------------------------------------------------------------------

      Tash.Lists.Test.Test_Case
        (Description     => "Push a float onto a list",
         Actual_Result   => D,
         Expected_Result => "369.0 690 45.0 38.75 83.875",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 5);

   end;

   if Tash.Test.All_Test_Cases_Passed then
      Ada.Text_IO.Put_Line
        ("Test_Lists PASSED --" &
         Integer'Image (Tash.Test.Test_Case_Number) &
         " tests completed");
   else
      Ada.Text_IO.Put_Line ("Test_Lists FAILED");
   end if;

end Test_Lists;
