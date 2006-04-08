with Ada.Command_Line;
with Ada.Strings;
with Ada.Tags;
with Ada.Text_IO;
with Tash.Floats.Test;
with Tash.Integers.Test;
with Tash.Lists.Test;
with Tash.Strings.Test;
with Tash.Test;

procedure Test_Lists is

   -- Make operators visible without qualification
   -----------------------------------------------
   use type Ada.Tags.Tag;
   use type Tash.Floats.Tash_Float;
   use type Tash.Integers.Tash_Integer;
   use type Tash.Lists.Tash_List;
   use type Tash.Strings.Tash_String;

   Verbose : Boolean := False;
   D2      : Tash.Integers.Tash_Integer;

begin -- Test_Lists

   -------------------------------------------
   -- Check for -verbose command line argument
   -------------------------------------------
   GET_COMMAND_LINE_ARGUMENTS:
   for I in 1..Ada.Command_Line.Argument_Count loop
      if Ada.Command_Line.Argument (I) = "-verbose" then
         Verbose := True;
      end if;
   end loop GET_COMMAND_LINE_ARGUMENTS;

   Tash.Test.Set_Verbose          (On => Verbose);
   Tash.Lists.Test.Set_Verbose    (On => Verbose);
   Tash.Integers.Test.Set_Verbose (On => Verbose);
   Tash.Floats.Test.Set_Verbose   (On => Verbose);
   Tash.Strings.Test.Set_Verbose  (On => Verbose);

   declare

      A : Tash.Lists.Tash_List;
      B : Tash.Lists.Tash_List;
      C : aliased Tash.Lists.Tash_List;
      D : Tash.Integers.Tash_Integer;
      E : Tash.Strings.Tash_String;
      F : Tash.Floats.Tash_Float;
      G : Tash.Strings.Tash_String;
      I : Tash.Integers.Tash_Integer;
      U : aliased Tash.Lists.Tash_List;

   begin

      -------------------------------------------------
      -- Test creation of Tash lists
      -------------------------------------------------
      Tash.Lists.Test.Test_Case (
         Description     => "Convert uninitialized Tash list to Ada string",
         Actual_Result   => A,
         Expected_Result => "",
         Expected_Type   => "",
         Expected_Count  => 0,
         Expected_Length => 0);

      Tash.Test.Test_Case (
         Description     => "Uninitialized Tash list is null",
         Result          => Tash.Lists.Is_Null (A));

      Tash.Test.Test_Case (
         Description     => "Uninitialized Tash list is empty",
         Result          => Tash.Lists.Is_Empty (A));

      A := Tash.Lists.Null_Tash_List;
      Tash.Lists.Test.Test_Case (
         Description     => "Convert null Tash list to Ada string",
         Actual_Result   => A,
         Expected_Result => "",
         Expected_Type   => "",
         Expected_Count  => 0,
         Expected_Length => 0);
      Tash.Test.Test_Case (
         Description     => "Null Tash list is null",
         Result          => Tash.Lists.Is_Null (A));
      Tash.Test.Test_Case (
         Description     => "Null Tash list is empty",
         Result          => Tash.Lists.Is_Empty (A));

      A := Tash.Lists.To_Tash_List (D);
      Tash.Lists.Test.Test_Case (
         Description     => "Create Tash list from uninitialized Tash object",
         Actual_Result   => A,
         Expected_Result => "",
         Expected_Type   => "",
         Expected_Count  => 0,
         Expected_Length => 0);
      Tash.Test.Test_Case (
         Description     => "Tash list created from uninitialized Tash " &
                            "object is null",
         Result          => Tash.Lists.Is_Null (A));
      Tash.Test.Test_Case (
         Description     => "Tash list created from uninitialized Tash " &
                            "object is empty",
         Result          => Tash.Lists.Is_Empty (A));

      A := +D;
      Tash.Lists.Test.Test_Case (
         Description     => "Create Tash list from uninitialized Tash object",
         Actual_Result   => A,
         Expected_Result => "",
         Expected_Type   => "",
         Expected_Count  => 0,
         Expected_Length => 0);
      Tash.Test.Test_Case (
         Description     => "Tash list created from uninitialized Tash " &
                            "object is null",
         Result          => Tash.Lists.Is_Null (A));
      Tash.Test.Test_Case (
         Description     => "Tash list created from uninitialized Tash " &
                            "object is empty",
         Result          => Tash.Lists.Is_Empty (A));

      D := +3;
      A := Tash.Lists.To_Tash_List (D);
      Tash.Lists.Test.Test_Case (
         Description     => "Create Tash list from single Tash object",
         Actual_Result   => A,
         Expected_Result => "3",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list element",
         Actual_Result   => D,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 2);
      I := Tash.Lists.Element (A, 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list element",
         Actual_Result   => I,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 3);
      Tash.Test.Test_Case (
         Description     => "Tash list is not empty",
         Result          => not Tash.Lists.Is_Empty (A));

      A := +D;
      Tash.Lists.Test.Test_Case (
         Description     => "Create Tash list from single Tash object",
         Actual_Result   => A,
         Expected_Result => "3",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list element",
         Actual_Result   => D,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 3);
      I := Tash.Lists.Element (A, 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list element",
         Actual_Result   => I,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 3);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list element",
         Actual_Result   => Tash.Lists.Element (A, 1),
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      D2 := D;
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      D := +4;
      Tash.Lists.Append (List    => A,
                         Element => D);
      Tash.Lists.Test.Test_Case (
         Description     => "Append another element",
         Actual_Result   => A,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list element",
         Actual_Result   => D,
         Expected_Result => 4,
         Expected_Type   => "int",
         Expected_Count  => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list " &
                            "element 1",
         Actual_Result   => Tash.Lists.Element (A, 1),
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list " &
                            "element 2",
         Actual_Result   => Tash.Lists.Element (A, 2),
         Expected_Result => 4,
         Expected_Type   => "int",
         Expected_Count  => 3);
      Tash.Test.Test_Case (
         Description     => "Tash list is not empty",
         Result          => not Tash.Lists.Is_Empty (A));

      B := Tash.Lists.To_Tash_List (A, Expand => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Create Tash list by expanding a Tash list object",
         Actual_Result   => B,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Lists.Test.Test_Case (
         Description     => "Verify that expanded list has not changed",
         Actual_Result   => A,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list element",
         Actual_Result   => D,
         Expected_Result => 4,
         Expected_Type   => "int",
         Expected_Count  => 3);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list element",
         Actual_Result   => Tash.Lists.Element (A, 1),
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list element",
         Actual_Result   => Tash.Lists.Element (A, 2),
         Expected_Result => 4,
         Expected_Type   => "int",
         Expected_Count  => 4);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      B := Tash.Lists.To_Tash_List (A, Expand => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Create Tash list that contains a list as an " &
                            "element",
         Actual_Result   => B,
         Expected_Result => "{3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Lists.Test.Test_Case (
         Description     => "Verify that original list ref count was " &
                            "incremented",
         Actual_Result   => A,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 2,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 3);
      declare
         Description : constant String :=
           "Any element of an uninitialized list is null Tash object";
      begin
         E := Tash.Lists.Element (U, 1);
         Tash.Test.Test_Case (
            Description => Description,
            Result      => Tash.Strings.Is_Null (E));
         E := Tash.Lists.Element (U, 2);
         Tash.Test.Test_Case (
            Description => Description,
            Result      => Tash.Strings.Is_Null (E));
         E := Tash.Lists.Element (U, Integer'last);
         Tash.Test.Test_Case (
            Description => Description,
            Result      => Tash.Strings.Is_Null (E));
      end;

      -- Remove A from B so it's no longer shared
      B := Tash.Lists.Null_Tash_List;
      Tash.Test.Test_Case (
         Description => "Any element of a list beyond its length is null",
         Result      => Tash.Strings.Is_Null (Tash.Lists.Element (A, 3)));

      Tash.Test.Test_Case (
         Description => "Any element of a list beyond its length is a null",
         Result      => Tash.Strings.Is_Null (
                        Tash.Lists.Element (A, Integer'last)));

      Tash.Test.Test_Case (
         Description => "Head of an uninitialized list is null element",
         Result      => Tash.Strings.Is_Null (Tash.Lists.Head (U)));

      Tash.Test.Test_Case (
         Description => "Tail of an uninitialized list is null element",
         Result      => Tash.Strings.Is_Null (Tash.Lists.Tail (U)));

      Tash.Integers.Test.Test_Case (
         Description     => "Get head of a tash list",
         Actual_Result   => Tash.Lists.Head (A),
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      Tash.Integers.Test.Test_Case (
         Description     => "Get tail of a tash list",
         Actual_Result   => Tash.Lists.Tail (A),
         Expected_Result => 4,
         Expected_Type   => "int",
         Expected_Count  => 3);

      D := Tash.Integers.Null_Tash_Integer;
      Tash.Lists.Append (List    => A,
                         Element => D);
      Tash.Lists.Test.Test_Case (
         Description     => "Append a null element to a list",
         Actual_Result   => A,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 3);

      G := Tash.Strings.To_Tash_String ("");
      Tash.Lists.Append (List    => A,
                         Element => G);
      Tash.Lists.Test.Test_Case (
         Description     => "Append an empty element to a list",
         Actual_Result   => A,
         Expected_Result => "3 4 {}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      -- delete the empty element just added
      --------------------------------------
      Tash.Lists.Delete_Element (A, 3);
      Tash.Lists.Test.Test_Case (
         Description     => "Delete empty element at end of list",
         Actual_Result   => A,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      C := Tash.Lists.Null_Tash_List;
      D := +5;
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Test.Test_Case (
         Description     => "Append an element to a null list",
         Actual_Result   => C,
         Expected_Result => "5",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);

      C := Tash.Lists.Null_Tash_List;
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Append a list to a null list with no expansion",
         Actual_Result   => C,
         Expected_Result => "{3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 3);

      C := Tash.Lists.Null_Tash_List;
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Append a list to an empty list with expansion",
         Actual_Result   => C,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      D := +6;
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Test.Test_Case (
         Description     => "Append an element to a list",
         Actual_Result   => C,
         Expected_Result => "3 4 6",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Append a list to a list with no expansion",
         Actual_Result   => C,
         Expected_Result => "3 4 6 {3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Append a list to a list with expansion",
         Actual_Result   => C,
         Expected_Result => "3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 6);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      Tash.Lists.Delete_Element (List  => U,
                                 Index => 4);
      Tash.Test.Test_Case (
         Description     => "Delete an element from an empty list",
         Result          => Tash.Lists.Is_Empty (U));

      Tash.Lists.Delete_Element (List  => C,
                                 Index => 1);
      Tash.Lists.Test.Test_Case (
         Description     => "Delete first element from a list",
         Actual_Result   => C,
         Expected_Result => "4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 5);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      Tash.Lists.Delete_Element (List  => C,
                                 Index => Tash.Lists.Length (C));
      Tash.Lists.Test.Test_Case (
         Description     => "Delete last element from a list",
         Actual_Result   => C,
         Expected_Result => "4 6 {3 4} 3",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      Tash.Lists.Delete_Element (List  => C,
                                 Index => 2);
      Tash.Lists.Test.Test_Case (
         Description     => "Delete a middle element from a list",
         Actual_Result   => C,
         Expected_Result => "4 {3 4} 3",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Lists.Delete_Element (List  => C,
                                 Index => 2);
      Tash.Lists.Test.Test_Case (
         Description     => "Delete a list element from a list",
         Actual_Result   => C,
         Expected_Result => "4 3",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Lists.Test.Test_Case (
         Description     => "Verify value and ref count of list element " &
                            "just deleted",
         Actual_Result   => A,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      -- Create a list from which we can delete some slices.
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => False);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Create a list from which we can delete some slices",
         Actual_Result   => C,
         Expected_Result => "4 3 3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      Tash.Lists.Delete_Slice (List => U,
                               From => 1,
                               To   => 2);
      Tash.Test.Test_Case (
         Description     => "Delete a slice from an empty list",
         Result          => Tash.Lists.Is_Empty (U));

      Tash.Lists.Delete_Slice (List => C,
                               From => 1,
                               To   => 0);
      Tash.Lists.Test.Test_Case (
         Description     => "Delete empty range from a list",
         Actual_Result   => C,
         Expected_Result => "4 3 3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);

      Tash.Lists.Delete_Slice (List => C,
                               From => 1,
                               To   => 2);
      Tash.Lists.Test.Test_Case (
         Description     => "Delete slice from beginning of a list",
         Actual_Result   => C,
         Expected_Result => "3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 6);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      Tash.Lists.Delete_Slice (List => C,
                               From => Tash.Lists.Length (C) - 1,
                               To   => Tash.Lists.Length (C));
      Tash.Lists.Test.Test_Case (
         Description     => "Delete slice from end of a list",
         Actual_Result   => C,
         Expected_Result => "3 4 6 {3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      Tash.Lists.Delete_Slice (List => C,
                               From => 2,
                               To   => 3);
      Tash.Lists.Test.Test_Case (
         Description     => "Delete a slice from middle of a list",
         Actual_Result   => C,
         Expected_Result => "3 {3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      Tash.Lists.Delete_Slice (List => C,
                               From => 1,
                               To   => 2);
      Tash.Lists.Test.Test_Case (
         Description     => "Delete all elements of a list",
         Actual_Result   => C,
         Expected_Result => "",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 0);
      Tash.Lists.Test.Test_Case (
         Description     => "Verify value and ref count of list element just deleted",
         Actual_Result   => A,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 3);

      -- Create a list in which we can replace some slices.
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => False);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Create a list in which we can replace some slices",
         Actual_Result   => C,
         Expected_Result => "6 6 3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      Tash.Lists.Replace_Slice (List    => C,
                                From    => 2,
                                To      => 3,
                                Element => U,
                                Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace slice with an empty element",
         Actual_Result   => C,
         Expected_Result => "6 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 6);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Replace_Slice (List    => B,
                                From    => 1,
                                To      => 2,
                                Element => A,
                                Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace slice in an empty list; expand element",
         Actual_Result   => B,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Replace_Slice (List    => B,
                                From    => 1,
                                To      => 2,
                                Element => A,
                                Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace slice in an empty list; don't expand element",
         Actual_Result   => B,
         Expected_Result => "{3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      Tash.Lists.Replace_Slice (List    => C,
                                From    => 1,
                                To      => 3,
                                Element => A,
                                Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace slice in a list; expand element",
         Actual_Result   => C,
         Expected_Result => "3 4 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 5);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      Tash.Lists.Replace_Slice (List    => C,
                                From    => 2,
                                To      => 5,
                                Element => A,
                                Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace slice in a list; don't expand element",
         Actual_Result   => C,
         Expected_Result => "3 {3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      -- Create a list in which we can replace some elements.
      C := Tash.Lists.Null_Tash_List;
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => False);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Create a list in which we can replace some elements",
         Actual_Result   => C,
         Expected_Result => "6 6 3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      Tash.Lists.Replace_Element (List    => C,
                                  Index   => 2,
                                  Element => U,
                                  Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace element with an empty element",
         Actual_Result   => C,
         Expected_Result => "6 3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 7);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Replace_Element (List    => B,
                                  Index   => 1,
                                  Element => A,
                                  Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace element in an empty list; expand element",
         Actual_Result   => B,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Replace_Element (List    => B,
                                  Index   => 1,
                                  Element => A,
                                  Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace element in an empty list; don't expand element",
         Actual_Result   => B,
         Expected_Result => "{3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      Tash.Lists.Replace_Element (List    => C,
                                  Index   => 1,
                                  Element => A,
                                  Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace element in a list; expand element",
         Actual_Result   => C,
         Expected_Result => "3 4 3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      Tash.Lists.Replace_Element (List    => C,
                                  Index   => 2,
                                  Element => A,
                                  Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Replace element in a list; don't expand element",
         Actual_Result   => C,
         Expected_Result => "3 {3 4} 3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      -- Create a list in which we can replace some elements.
      C := Tash.Lists.Null_Tash_List;
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Append (List    => C,
                         Element => D);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => False);
      Tash.Lists.Append (List    => C,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Create a list in which we can replace some elements",
         Actual_Result   => C,
         Expected_Result => "6 6 3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 8);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Insert (List    => B,
                         Index   => 1,
                         Element => D,
                         Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Insert element into empty list",
         Actual_Result   => B,
         Expected_Result => "6",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Insert (List    => B,
                         Index   => 2,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Insert list into an empty list; expand list",
         Actual_Result   => B,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Insert (List    => B,
                         Index   => 3,
                         Element => A,
                         Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Insert list into an empty list; don't expand list",
         Actual_Result   => B,
         Expected_Result => "{3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      D := +7;
      Tash.Lists.Insert (List    => C,
                         Index   => 1,
                         Element => D,
                         Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Insert an element into front of list",
         Actual_Result   => C,
         Expected_Result => "7 6 6 3 4 6 {3 4} 3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 9);

      Tash.Lists.Insert (List    => C,
                         Index   => Tash.Lists.Length (C),
                         Element => D,
                         Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Insert an element before last element of list",
         Actual_Result   => C,
         Expected_Result => "7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 10);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 5);

      Tash.Lists.Insert (List    => C,
                         Index   => 1,
                         Element => A,
                         Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Insert a list before first element of list; expand",
         Actual_Result   => C,
         Expected_Result => "3 4 7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 12);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      Tash.Lists.Insert (List    => C,
                         Index   => 1,
                         Element => A,
                         Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Insert a list before first element of list; don't expand",
         Actual_Result   => C,
         Expected_Result => "{3 4} 3 4 7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 13);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Push (List    => B,
                       Element => D,
                       Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Push element into empty list",
         Actual_Result   => B,
         Expected_Result => "7",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Push (List    => B,
                       Element => A,
                       Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Push list into an empty list; expand list",
         Actual_Result   => B,
         Expected_Result => "3 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 7);

      B := Tash.Lists.Null_Tash_List;
      Tash.Lists.Push (List    => B,
                       Element => A,
                       Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Push list into an empty list; don't expand list",
         Actual_Result   => B,
         Expected_Result => "{3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      D := +8;
      Tash.Lists.Push (List    => C,
                       Element => D,
                       Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Push an element into list",
         Actual_Result   => C,
         Expected_Result => "8 {3 4} 3 4 7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 14);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 6);

      Tash.Lists.Push (List    => C,
                       Element => A,
                       Expand  => True);
      Tash.Lists.Test.Test_Case (
         Description     => "Push a list onto list; expand",
         Actual_Result   => C,
         Expected_Result => "3 4 8 {3 4} 3 4 7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 16);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 7);

      Tash.Lists.Push (List    => C,
                       Element => A,
                       Expand  => False);
      Tash.Lists.Test.Test_Case (
         Description     => "Push a list onto list; don't expand",
         Actual_Result   => C,
         Expected_Result => "{3 4} 3 4 8 {3 4} 3 4 7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 17);
      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of D2",
         Actual_Result   => D2,
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 7);

      Tash.Lists.Pop (U);
      Tash.Test.Test_Case (
         Description => "Pop element from an empty list",
         Result      => Tash.Lists.Is_Null (U));
      Tash.Test.Test_Case (
         Description => "Pop element from an empty list",
         Result      => U = Tash.Lists.Null_Tash_List);

      Tash.Lists.Pop (C);
      Tash.Lists.Test.Test_Case (
         Description     => "Pop list element from a list",
         Actual_Result   => C,
         Expected_Result => "3 4 8 {3 4} 3 4 7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 16);

      Tash.Lists.Test.Test_Case (
         Description     => "Verify first item was removed from list",
         Actual_Result   => C,
         Expected_Result => "3 4 8 {3 4} 3 4 7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 16);

      Tash.Lists.Pop (C);
      Tash.Lists.Test.Test_Case (
         Description     => "Pop element from a list",
         Actual_Result   => C,
         Expected_Result => "4 8 {3 4} 3 4 7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 15);

      Tash.Lists.Pop (C);
      Tash.Lists.Test.Test_Case (
         Description     => "Pop element from list",
         Actual_Result   => C,
         Expected_Result => "8 {3 4} 3 4 7 6 6 3 4 6 {3 4} 3 7 4",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 14);

      Tash.Test.Test_Case (
         Description => "C = C",
         Result      => C = C);

      Tash.Test.Test_Case (
         Description => "C <= C",
         Result      => C <= C);

      Tash.Test.Test_Case (
         Description => "C >= C",
         Result      => C >= C);

      Tash.Test.Test_Case (
         Description => "A /= C",
         Result      => A /= C);

      B := Tash.Lists.Null_Tash_List;
      D := +3;
      Tash.Lists.Append (List    => B,
                         Element => D);
      Tash.Lists.Append (List    => B,
                         Element => D);
      Tash.Test.Test_Case (
         Description => "A /= B",
         Result      => A /= B);
      Tash.Test.Test_Case (
         Description => "A > B",
         Result      => A > B);
      Tash.Test.Test_Case (
         Description => "A >= B",
         Result      => A >= B);
      Tash.Test.Test_Case (
         Description => "B < A",
         Result      => B < A);
      Tash.Test.Test_Case (
         Description => "B <= A",
         Result      => B <= A);

      declare
         G : Tash.Lists.Tash_List;
         H : Tash.Lists.Tash_List;
      begin
         Tash.Lists.Append (List    => G,
                            Element => +23);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 1 of checking list finalization",
            Actual_Result   => G,
            Expected_Result => "23",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 1);
         Tash.Lists.Append (List    => G,
                            Element => +89);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 2 of checking list finalization",
            Actual_Result   => G,
            Expected_Result => "23 89",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 2);
         Tash.Lists.Append (List    => H,
                            Element => G);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 3a of checking list finalization",
            Actual_Result   => H,
            Expected_Result => "{23 89}",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 1);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 3b of checking list finalization",
            Actual_Result   => G,
            Expected_Result => "23 89",
            Expected_Type   => "list",
            Expected_Count  => 2,
            Expected_Length => 2);
         Tash.Lists.Append (List    => H,
                            Element => G);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 4a of checking list finalization",
            Actual_Result   => H,
            Expected_Result => "{23 89} {23 89}",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 2);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 4b of checking list finalization",
            Actual_Result   => G,
            Expected_Result => "23 89",
            Expected_Type   => "list",
            Expected_Count  => 3,
            Expected_Length => 2);
         Tash.Lists.Append (List    => H,
                            Element => G);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 5a of checking list finalization",
            Actual_Result   => H,
            Expected_Result => "{23 89} {23 89} {23 89}",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 3);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 5b of checking list finalization",
            Actual_Result   => G,
            Expected_Result => "23 89",
            Expected_Type   => "list",
            Expected_Count  => 4,
            Expected_Length => 2);
         Tash.Lists.Delete_Element (List  => H,
                                    Index => 1);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 6a of checking list finalization",
            Actual_Result   => H,
            Expected_Result => "{23 89} {23 89}",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 2);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 6b of checking list finalization",
            Actual_Result   => G,
            Expected_Result => "23 89",
            Expected_Type   => "list",
            Expected_Count  => 3,
            Expected_Length => 2);
         H := Tash.Lists.Null_Tash_List;
         Tash.Lists.Test.Test_Case (
            Description     => "Step 7a of checking list finalization",
            Actual_Result   => H,
            Expected_Result => "",
            Expected_Type   => "",
            Expected_Count  => 0,
            Expected_Length => 0);
         Tash.Lists.Test.Test_Case (
            Description     => "Step 7b of checking list finalization",
            Actual_Result   => G,
            Expected_Result => "23 89",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 2);
      end;

      B := Tash.Lists.To_Tash_List ("");
      Tash.Lists.Test.Test_Case (
         Description     => "Make an empty Tash list from a string",
         Actual_Result   => B,
         Expected_Result => "",
         Expected_Type   => "",
         Expected_Count  => 0,
         Expected_Length => 0);

      B := Tash.Lists.To_Tash_List (
         "The quick brown fox jumps over the lazy dog");
      Tash.Lists.Test.Test_Case (
         Description     => "Make a Tash list from a string",
         Actual_Result   => B,
         Expected_Result => "The quick brown fox jumps over the lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 9);

      B := Tash.Lists.To_Tash_List (
         "     The   quick     brown     fox jumps    over the lazy dog      ");
      Tash.Lists.Test.Test_Case (
         Description     => "Make a Tash list from a string with extra spaces",
         Actual_Result   => B,
         Expected_Result => "The quick brown fox jumps over the lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 9);

      B := Tash.Lists.To_Tash_List (
         "{The quick brown fox} jumps over {the lazy dog}");
      Tash.Lists.Test.Test_Case (
         Description     => "Make a Tash list from a string with embedded spaces",
         Actual_Result   => B,
         Expected_Result => "{The quick brown fox} jumps over {the lazy dog}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      B := Tash.Lists.To_Tash_List (
         "{The {quick brown} fox} jumps over {the lazy dog}");
      Tash.Lists.Test.Test_Case (
         Description     => "Make a Tash list from a string with embedded lists",
         Actual_Result   => B,
         Expected_Result => "{The {quick brown} fox} jumps over {the lazy dog}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);

      B := Tash.Lists.To_Tash_List (
         "The \{quick brown\} fox jumps over {the lazy dog}");
      Tash.Lists.Test.Test_Case (
         Description     => "Make a Tash list from a string with escaped braces",
         Actual_Result   => B,
         Expected_Result => "The \{quick brown\} fox jumps over {the lazy dog}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 7);

      declare
         Description : constant String :=
            "Make a tash list from string with unmatched brace";
      begin
         B := Tash.Lists.To_Tash_List (
            "The quick brown fox jumps over {the lazy dog");
         Tash.Test.Test_Case (
            Description     => Description,
            Result          => False);
      exception
         when Tash.Lists.List_Error =>
            Tash.Test.Test_Case (
               Description     => Description,
               Result          => true);
      end;

      B := Tash.Lists.Split (
         "The quick brown fox jumps over the lazy dog");
      Tash.Lists.Test.Test_Case (
         Description     => "Make a Tash list by splitting a string at spaces",
         Actual_Result   => B,
         Expected_Result => "The quick brown fox jumps over the lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 9);

      B := Tash.Lists.Split (
         Str      => "/usr/X/lib/X11",
         Split_At => "/");
      Tash.Lists.Test.Test_Case (
         Description     => "Make a Tash list from splitting a unix path name",
         Actual_Result   => B,
         Expected_Result => "{} usr X lib X11",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 5);

      B := Tash.Lists.Split (
         Str      => "/usr/X/lib/X11/pix map",
         Split_At => "/");
      Tash.Lists.Test.Test_Case (
         Description     => "Check that Split does not split at embedded spaces",
         Actual_Result   => B,
         Expected_Result => "{} usr X lib X11 {pix map}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 6);

      B := Tash.Lists.Split (
         "The quick brown fox jumps" & ASCII.CR & "over" & ASCII.LF &
         "the" & ASCII.HT & "lazy dog");
      Tash.Lists.Test.Test_Case (
         Description     => "Check default split characters",
         Actual_Result   => B,
         Expected_Result => "The quick brown fox jumps over the lazy dog",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 9);

      B := Tash.Lists.Split (
         " The  quick brown fox jumps" & ASCII.CR & "over" & ASCII.LF &
         "the" & ASCII.HT & "lazy dog  ");
      Tash.Lists.Test.Test_Case (
         Description     => "Check leading, trailing, and adjacent split characters",
         Actual_Result   => B,
         Expected_Result => "{} The {} quick brown fox jumps over the lazy dog {} {}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 13);

      D := +52;
      Tash.Integers.Test.Test_Case (
         Description     => "Create a Tash integer",
         Actual_Result   => D,
         Expected_Result => 52,
         Expected_Type   => "int",
         Expected_Count  => 1);

      C := Tash.Lists.To_Tash_List (D, +3.14159,
                              Tash.Strings.To_Tash_String ("This is a string"), A);
      Tash.Lists.Test.Test_Case (
         Description     => "Create Tash list with heterogeneous elements",
         Actual_Result   => C,
         Expected_Result => "52 3.14159 {This is a string} {3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 4);
      Tash.Integers.Test.Test_Case (
         Description     => "Check reference count on Tash integer",
         Actual_Result   => D,
         Expected_Result => 52,
         Expected_Type   => "int",
         Expected_Count  => 2);

      Tash.Test.Test_Case (
         Description     => "Printf-style Format with extra list elements",
         Result          => Tash.Lists.Format ("D=%d  F=%7.5f G=%s", C) =
                            "D=52  F=3.14159 G=This is a string");
      Tash.Integers.Test.Test_Case (
         Description     => "Check reference count on Tash integer",
         Actual_Result   => D,
         Expected_Result => 52,
         Expected_Type   => "int",
         Expected_Count  => 2);

      Tash.Lists.Pop (C);
      Tash.Lists.Test.Test_Case (
         Description     => "Pop an integer from a Tash list",
         Actual_Result   => C,
         Expected_Result => "3.14159 {This is a string} {3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 3);

      Tash.Lists.Pop (C);
      Tash.Lists.Test.Test_Case (
         Description     => "Pop a float from a Tash list",
         Actual_Result   => C,
         Expected_Result => "{This is a string} {3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      Tash.Lists.Pop (C);
      Tash.Lists.Test.Test_Case (
         Description     => "Pop a string from a Tash list",
         Actual_Result   => C,
         Expected_Result => "{3 4}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 1);

      declare
         Formatted_String : constant String := Tash.Lists.Format (
            "D=%d  F=%4.2f G=%s",
            Tash.Lists.To_Tash_List (+45, +3.14,
                               Tash.Strings.To_Tash_String ("This is another string")));
      begin
         Tash.Test.Test_Case (
            Description     => "Another printf-style Format",
            Result          => Formatted_String = "D=45  F=3.14 G=This is another string");
      end;

      -- test some bad formats
      C := Tash.Lists.To_Tash_List (+52, +3.14159,
                              Tash.Strings.To_Tash_String ("This is a string"));
      Tash.Test.Test_Case (
         Description     => "Printf-style Format with no format %",
         Result          => Tash.Lists.Format ("none", C) = "none");
      declare
         Description : constant String :=
            "Printf-style Format with not enough list elements";
      begin
         Ada.Text_IO.Put_Line (Tash.Lists.Format (
            "D=%d  F=%7.5f G=%s H=%s I=%s", C));
         Tash.Test.Test_Case (
            Description     => Description,
            Result          => False);
      exception
         when Tash.Lists.Format_Error =>
            Tash.Test.Test_Case (
               Description     => Description,
               Result          => True);
         when others =>
            Tash.Test.Test_Case (
               Description     => Description,
               Result          => False);
      end;
      declare
         Description : constant String :=
            "Printf-style Format with wrong data type";
      begin
         Ada.Text_IO.Put_Line (Tash.Lists.Format (
            "D=%7.5f  F=%d G=%s", C));
         Tash.Test.Test_Case (
            Description     => Description,
            Result          => False);
      exception
         when Tash.Lists.Format_Error =>
            Tash.Test.Test_Case (
               Description     => Description,
               Result          => True);
         when others =>
            Tash.Test.Test_Case (
               Description     => Description,
               Result          => False);
      end;

      Tash.Integers.Test.Test_Case (
         Description     => "Verify value and reference count of list " &
                            "element 1",
         Actual_Result   => Tash.Lists.Element (A, 1),
         Expected_Result => 3,
         Expected_Type   => "int",
         Expected_Count  => 4);

      C := Tash.Lists.To_Tash_List (+52, +3.14159, +"This is a string");
      Tash.Lists.Test.Test_Case (
         Description     => "Get a slice at head of list",
         Actual_Result   => Tash.Lists.Slice (C, 1, 2),
         Expected_Result => "52 3.14159",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      Tash.Lists.Test.Test_Case (
         Description     => "Get a slice at tail of list",
         Actual_Result   => Tash.Lists.Slice (C, 2, 3),
         Expected_Result => "3.14159 {This is a string}",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      Tash.Test.Test_Case (
         Description     => "An empty slice is a null list",
         Result          => Tash.Lists.Is_Null (Tash.Lists.Slice (C, 1, 0)));

      Tash.Test.Test_Case (
         Description     => "An empty slice is a null list",
         Result          => Tash.Lists.Is_Null (Tash.Lists.Slice (C, 4, 3)));

      C := Tash.Lists.To_Tash_List (+52, +3.14159, +"This", +"is a string");
      Tash.Lists.Test.Test_Case (
         Description     => "Get a slice in the middle of a list",
         Actual_Result   => Tash.Lists.Slice (C, 2, 3),
         Expected_Result => "3.14159 This",
         Expected_Type   => "list",
         Expected_Count  => 1,
         Expected_Length => 2);

      declare
         use Tash.Lists;
      begin
         C := Tash.Lists.To_Tash_List (+"bigboy", +"bigBoy", +"bigbang");
         Tash.Lists.Test.Test_Case (
            Description     => "ASCII sort, increasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_ASCII, Increasing),
            Expected_Result => "bigBoy bigbang bigboy",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 3);

         Tash.Lists.Test.Test_Case (
            Description     => "ASCII sort, decreasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_ASCII, Decreasing),
            Expected_Result => "bigboy bigbang bigBoy",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 3);

         Tash.Lists.Test.Test_Case (
            Description     => "Dictionary sort, increasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_Dictionary, Increasing),
            Expected_Result => "bigbang bigBoy bigboy",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 3);

         Tash.Lists.Test.Test_Case (
            Description     => "Dictionary sort, decreasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_Dictionary, Decreasing),
            Expected_Result => "bigboy bigBoy bigbang",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 3);

         C := Tash.Lists.To_Tash_List (+8, +10, +6, +1);
         Tash.Lists.Test.Test_Case (
            Description     => "ASCII sort integers, increasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_ASCII, Increasing),
            Expected_Result => "1 10 6 8",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 4);

         Tash.Lists.Test.Test_Case (
            Description     => "Dictionary sort integers, increasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_Dictionary, Increasing),
            Expected_Result => "1 6 8 10",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 4);

         Tash.Lists.Test.Test_Case (
            Description     => "Integer sort integers, increasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_Integer, Increasing),
            Expected_Result => "1 6 8 10",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 4);

         C := Tash.Lists.To_Tash_List (+267.5, +1.2, +7.2, -0.001);
         Tash.Lists.Test.Test_Case (
            Description     => "ASCII sort reals, increasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_ASCII, Increasing),
            Expected_Result => "-0.001 1.2 267.5 7.2",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 4);

         Tash.Lists.Test.Test_Case (
            Description     => "ASCII sort reals, decreasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_ASCII, Decreasing),
            Expected_Result => "7.2 267.5 1.2 -0.001",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 4);

         Tash.Lists.Test.Test_Case (
            Description     => "Real sort reals, increasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_REAL, Increasing),
            Expected_Result => "-0.001 1.2 7.2 267.5",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 4);

         Tash.Lists.Test.Test_Case (
            Description     => "Real sort reals, decreasing",
            Actual_Result   => Tash.Lists.Sort (C, SM_REAL, Decreasing),
            Expected_Result => "267.5 7.2 1.2 -0.001",
            Expected_Type   => "list",
            Expected_Count  => 1,
            Expected_Length => 4);

         declare
            Description : constant String :=
               "Integer sort with bad string data";
         begin
            C := Tash.Lists.To_Tash_List (+"these", +"are", +"strings!");
            B := Tash.Lists.Sort (C, SM_INTEGER, Increasing);
            Tash.Test.Test_Case (
               Description     => Description,
               Result          => False);
         exception
            when Tash.Lists.List_Error =>
               Tash.Test.Test_Case (
                  Description     => Description,
                  Result          => True);
         end;

      end;

   end;

   Tash.Integers.Test.Test_Case (
      Description     => "Verify value and reference count of D2",
      Actual_Result   => D2,
      Expected_Result => 3,
      Expected_Type   => "int",
      Expected_Count  => 1);

   if Tash.Test.All_Test_Cases_Passed then
      Ada.Text_IO.Put_Line ("Test_Lists PASSED");
   else
      Ada.Text_IO.Put_Line ("Test_Lists FAILED");
   end if;

end Test_Lists;
