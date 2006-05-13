with Ada.Command_Line;
with Ada.Text_IO;
with Tash.Arrays.Test;
with Tash.Float_Arrays;
with Tash.Integer_Arrays;
with Tash.Lists;
with Tash.Test;

procedure Test_Arrays is

   package TL renames Tash.Lists;

   --  Make operators visible without qualification
   -----------------------------------------------
   use type Tash.Arrays.Tash_Array;
   use type Tash.Lists.Tash_List;

   Verbose : Boolean := False;

begin --  Test_Arrays

   -------------------------------------------
   --  Check for -verbose command line argument
   -------------------------------------------
   GET_COMMAND_LINE_ARGUMENTS : for I in  1 ..
        Ada.Command_Line.Argument_Count
   loop
      if Ada.Command_Line.Argument (I) = "-verbose" then
         Verbose := True;
         exit GET_COMMAND_LINE_ARGUMENTS;
      end if;
   end loop GET_COMMAND_LINE_ARGUMENTS;

   Tash.Test.Set_Verbose (On => Verbose);
   Tash.Arrays.Test.Set_Verbose (On => Verbose);

   declare

      A : Tash.Arrays.Tash_Array;
      B : Tash.Arrays.Tash_Array;
      C : Tash.Arrays.Tash_Array;
      L : Tash.Lists.Tash_List;
      U : aliased Tash.Arrays.Tash_Array;

   begin

      --------------------------------------------------------
      --  Test Is_Null and Is_Empty on uninitialized Tash array
      --------------------------------------------------------

      Tash.Test.Test_Case
        (Description => "Uninitialized Tash array is null",
         Result      => Tash.Arrays.Is_Null (U));

      Tash.Test.Test_Case
        (Description => "Uninitialized Tash array is empty",
         Result      => Tash.Arrays.Is_Empty (U));

      Tash.Test.Test_Case
        (Description => "Convert uninitialized Tash array to Ada string",
         Result      => Tash.Arrays.To_String (U) = "");

      -----------------------------------------------------------------
      --  Test Is_Null and Is_Empty on Tash array set to Null_Tash_Array
      -----------------------------------------------------------------

      A := Tash.Arrays.Null_Tash_Array;

      Tash.Test.Test_Case
        (Description => "Tash array initialized to Null_Tash_Array is null",
         Result      => Tash.Arrays.Is_Null (A));

      Tash.Test.Test_Case
        (Description => "Tash array initialized to Null_Tash_Array is empty",
         Result      => Tash.Arrays.Is_Empty (A));

      Tash.Test.Test_Case
        (Description =>
           "Convert Tash array initialized to Null_Tash_Array to Ada string",
         Result      => Tash.Arrays.To_String (A) = "");

      ----------------------------
      --  Test creating Tash arrays
      ----------------------------

      B :=
         Tash.Arrays.To_Tash_Array ("{New York} Albany Massachusetts Boston");
      Tash.Test.Test_Case
        (Description => "Create Tash array -- is not null",
         Result      => not Tash.Arrays.Is_Null (B));
      Tash.Test.Test_Case
        (Description => "Create Tash array -- is not empty",
         Result      => not Tash.Arrays.Is_Empty (B));
      Tash.Test.Test_Case
        (Description => "Create Tash array -- check length",
         Result      => Tash.Arrays.Length (B) = 2);

      L := Tash.Lists.Sort (Tash.Arrays.Get_Indices (B));
      Tash.Test.Test_Case
        (Description => "Create Tash array -- get indices",
         Result      => Tash.Lists.To_String (L) =
                        "Massachusetts {New York}");
      Tash.Arrays.Test.Test_Case
        (Description     => "Create Tash array -- check contents",
         Actual_Result   => B,
         Expected_Result => "Massachusetts Boston {New York} Albany",
         Expected_Count  => 1);
      Tash.Test.Test_Case
        (Description => "Create Tash array -- To_String",
         Result      => Tash.Arrays.To_String (B) =
                        "Massachusetts Boston {New York} Albany");

      -----------------------------------------
      --  Test getting array elements
      -----------------------------------------

      Tash.Test.Test_Case
        (Description => "Get an array element",
         Result      => Tash.Arrays.Get_Element (B, "Massachusetts") =
                        "Boston");

      Tash.Test.Test_Case
        (Description => "Get an array element",
         Result      => Tash.Arrays.Get_Element (B, "New York") = "Albany");

      declare
         Description : constant String :=
            "Get an element of an uninitialized array -- raise Array_Error";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Arrays.Get_Element (U, "xyz") = "");
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      declare
         Description : constant String :=
            "Get a non-existant element -- raise Array_Error";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Arrays.Get_Element (B, "xyz") = "");
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      -----------------------------------------
      --  Modify an existing element of an array
      -----------------------------------------

      Tash.Arrays.Set_Element (B, Index => "New York", Value => "Buffalo");
      Tash.Test.Test_Case
        (Description => "Modify existing element -- is not null",
         Result      => not Tash.Arrays.Is_Null (B));
      Tash.Test.Test_Case
        (Description => "Modify existing element -- is not empty",
         Result      => not Tash.Arrays.Is_Empty (B));
      Tash.Test.Test_Case
        (Description => "Modify existing element -- check length",
         Result      => Tash.Arrays.Length (B) = 2);

      L := Tash.Lists.Sort (Tash.Arrays.Get_Indices (B));
      Tash.Test.Test_Case
        (Description => "Modify existing element -- get indices",
         Result      => Tash.Lists.To_String (L) =
                        "Massachusetts {New York}");
      Tash.Arrays.Test.Test_Case
        (Description     => "Modify existing element -- check contents",
         Actual_Result   => B,
         Expected_Result => "Massachusetts Boston {New York} Buffalo",
         Expected_Count  => 1);
      Tash.Test.Test_Case
        (Description => "Modify existing element -- To_String",
         Result      => Tash.Arrays.To_String (B) =
                        "Massachusetts Boston {New York} Buffalo");

      -----------------------------------------
      --  Add a new element to an array
      -----------------------------------------

      Tash.Arrays.Set_Element
        (B,
         Index => "California",
         Value => "Sacramento");
      Tash.Test.Test_Case
        (Description => "Add new element -- is not null",
         Result      => not Tash.Arrays.Is_Null (B));
      Tash.Test.Test_Case
        (Description => "Add new element -- is not empty",
         Result      => not Tash.Arrays.Is_Empty (B));
      Tash.Test.Test_Case
        (Description => "Add new element -- check length",
         Result      => Tash.Arrays.Length (B) = 3);

      L := Tash.Lists.Sort (Tash.Arrays.Get_Indices (B));
      Tash.Test.Test_Case
        (Description => "Add new element -- get indices",
         Result      => Tash.Lists.To_String (L) =
                        "California Massachusetts {New York}");
      Tash.Arrays.Test.Test_Case
        (Description     => "Add new element -- check contents",
         Actual_Result   => B,
         Expected_Result => "California Sacramento " &
                            "Massachusetts Boston " &
                            "{New York} Buffalo",
         Expected_Count  => 1);
      Tash.Test.Test_Case
        (Description => "Add new element -- To_String",
         Result      => Tash.Arrays.To_String (B) =
                        "California Sacramento " &
                        "Massachusetts Boston " &
                        "{New York} Buffalo");

      -----------------------------------------
      --  Add several new elements to an array
      -----------------------------------------
      Tash.Arrays.Set_Elements
        (B,
         Tash.Lists.To_Tash_List
            ("{North Carolina} Raleigh Pennsylvania Harrisburg " &
             "{New Jersey} Trenton"));
      Tash.Test.Test_Case
        (Description => "Add several elements -- is not null",
         Result      => not Tash.Arrays.Is_Null (B));
      Tash.Test.Test_Case
        (Description => "Add several elements -- is not empty",
         Result      => not Tash.Arrays.Is_Empty (B));
      Tash.Test.Test_Case
        (Description => "Add several elements -- check length",
         Result      => Tash.Arrays.Length (B) = 6);

      L := Tash.Lists.Sort (Tash.Arrays.Get_Indices (B));
      Tash.Test.Test_Case
        (Description => "Add several elements -- get indices",
         Result      => Tash.Lists.To_String (L) =
                        "California Massachusetts " &
                        "{New Jersey} {New York} " &
                        "{North Carolina} Pennsylvania");
      Tash.Arrays.Test.Test_Case
        (Description     => "Add several elements -- check contents",
         Actual_Result   => B,
         Expected_Result => "California Sacramento " &
                            "Massachusetts Boston " &
                            "{New Jersey} Trenton " &
                            "{New York} Buffalo " &
                            "{North Carolina} Raleigh " &
                            "Pennsylvania Harrisburg",
         Expected_Count  => 1);
      Tash.Test.Test_Case
        (Description => "Add several elements -- To_String",
         Result      =>
           Tash.Arrays.To_String (B) =
           "{New Jersey} Trenton " &
           "Pennsylvania Harrisburg " &
           "{North Carolina} Raleigh " &
           "California Sacramento " &
           "Massachusetts Boston " &
           "{New York} Buffalo");

      ----------------------------------------
      --  Delete several elements from an array
      ----------------------------------------
      Tash.Arrays.Delete_Element (B, "Massachusetts");
      Tash.Arrays.Delete_Element (B, "New York");
      Tash.Test.Test_Case
        (Description => "Delete several elements -- is not null",
         Result      => not Tash.Arrays.Is_Null (B));
      Tash.Test.Test_Case
        (Description => "Delete several elements -- is not empty",
         Result      => not Tash.Arrays.Is_Empty (B));
      Tash.Test.Test_Case
        (Description => "Delete several elements -- check length",
         Result      => Tash.Arrays.Length (B) = 4);

      L := Tash.Lists.Sort (Tash.Arrays.Get_Indices (B));
      Tash.Test.Test_Case
        (Description => "Delete several elements -- get indices",
         Result      => Tash.Lists.To_String (L) =
                        "California {New Jersey} " &
                        "{North Carolina} Pennsylvania");
      Tash.Arrays.Test.Test_Case
        (Description     => "Delete several elements -- check contents",
         Actual_Result   => B,
         Expected_Result => "California Sacramento " &
                            "{New Jersey} Trenton " &
                            "{North Carolina} Raleigh " &
                            "Pennsylvania Harrisburg",
         Expected_Count  => 1);
      Tash.Test.Test_Case
        (Description => "Delete several elements -- To_String",
         Result      => Tash.Arrays.To_String (B) =
                        "{New Jersey} Trenton " &
                        "Pennsylvania Harrisburg " &
                        "{North Carolina} Raleigh " &
                        "California Sacramento");

      --------------------------------------
      --  Delete all the elements of an array
      --------------------------------------
      Tash.Arrays.Delete_Element (B, "New Jersey");
      Tash.Arrays.Delete_Element (B, "Pennsylvania");
      Tash.Arrays.Delete_Element (B, "North Carolina");
      Tash.Arrays.Delete_Element (B, "California");
      Tash.Test.Test_Case
        (Description => "Delete all elements -- is not null",
         Result      => not Tash.Arrays.Is_Null (B));
      Tash.Test.Test_Case
        (Description => "Delete all elements -- is empty",
         Result      => Tash.Arrays.Is_Empty (B));
      Tash.Test.Test_Case
        (Description => "Delete all elements -- check length",
         Result      => Tash.Arrays.Length (B) = 0);

      L := Tash.Lists.Sort (Tash.Arrays.Get_Indices (B));
      Tash.Test.Test_Case
        (Description => "Delete all elements -- get indices",
         Result      => Tash.Lists.To_String (L) = "");
      Tash.Arrays.Test.Test_Case
        (Description     => "Delete all elements -- check contents",
         Actual_Result   => B,
         Expected_Result => "",
         Expected_Count  => 1);
      Tash.Test.Test_Case
        (Description => "Delete all elements -- To_String",
         Result      => Tash.Arrays.To_String (B) = "");

      -------------------------------------------------
      --  Test creating Tash arrays with mal-formed data
      -------------------------------------------------

      declare
         Description : constant String :=
            "create Tash array with unmatched open brace";
      begin
         B :=
            Tash.Arrays.To_Tash_Array
              ("{New York Albany Massachusetts Boston");
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      declare
         Description : constant String :=
            "create Tash array with unmatched close brace";
      begin
         B :=
            Tash.Arrays.To_Tash_Array
              ("New York} Albany Massachusetts Boston");
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      declare
         Description : constant String :=
            "create Tash array with missing value";
      begin
         B := +"{New York} Albany Massachusetts";
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      -------------------------------------------------
      --  Test Exists function
      -------------------------------------------------

      B :=
         Tash.Arrays.To_Tash_Array
           ("{Empire State Building} {New York} {Faneuil Hall} Boston");

      Tash.Test.Test_Case
        (Description => "Element exists",
         Result      => Tash.Arrays.Exists (B, "Empire State Building"));

      Tash.Test.Test_Case
        (Description => "Element does not exist",
         Result      => not Tash.Arrays.Exists (B, "Sears Tower"));

      Tash.Test.Test_Case
        (Description => "Element does not exist; array is uninitialized",
         Result      => not Tash.Arrays.Exists (U, "Sears Tower"));

      Tash.Test.Test_Case
        (Description => "Element does not exist; array is null",
         Result      => not Tash.Arrays.Exists (A, "Sears Tower"));

      -------------------------------------------------
      --  Test Element_is_String function
      -------------------------------------------------

      Tash.Test.Test_Case
        (Description => "Element is a string",
         Result      =>
            Tash.Arrays.Element_Is_String (B, "Empire State Building"));

      Tash.Test.Test_Case
        (Description => "Element is not a list",
         Result      =>
           not Tash.Arrays.Element_Is_List (B, "Empire State Building"));

      Tash.Test.Test_Case
        (Description => "Element is not an integer",
         Result      =>
           not Tash.Integer_Arrays.Element_Is_Integer
                 (B,
                  "Empire State Building"));

      Tash.Test.Test_Case
        (Description => "Element is not a float",
         Result      =>
           not Tash.Float_Arrays.Element_Is_Float
                 (B,
                  "Empire State Building"));

      Tash.Test.Test_Case
        (Description => "Element is not a string; it doesn't not exist",
         Result      => not Tash.Arrays.Element_Is_String (B, "Sears Tower"));

      Tash.Test.Test_Case
        (Description => "Element is not a string; array is uninitialized",
         Result      => not Tash.Arrays.Element_Is_String (U, "Sears Tower"));

      Tash.Test.Test_Case
        (Description => "Element is not a string; array is null",
         Result      => not Tash.Arrays.Element_Is_String (A, "Sears Tower"));

      -------------------------------------------------
      --  Test getting multiple elements at once
      -------------------------------------------------

      L := Tash.Arrays.Get_Elements (B);
      Tash.Test.Test_Case
        (Description => "Get all elements of an array",
         Result      => Tash.Lists.To_String (L) =
           "{Faneuil Hall} Boston {Empire State Building} {New York}");

      L := Tash.Arrays.Get_Elements (B, Pattern => "*euil*");
      Tash.Test.Test_Case
        (Description =>
           "Get elements of an array whose indices match a pattern",
         Result      => Tash.Lists.To_String (L) = "{Faneuil Hall} Boston");

      L := Tash.Arrays.Get_Elements (B, Pattern => "*pire*");
      Tash.Test.Test_Case
        (Description =>
           "Get elements of an array whose indices match a pattern",
         Result      => Tash.Lists.To_String (L) =
                        "{Empire State Building} {New York}");

      -------------------------------------------------
      --  Test getting sorted elements
      -------------------------------------------------

      L :=
         Tash.Arrays.Get_Sorted_Elements
           (B,
            Mode  => TL.SM_ASCII,
            Order => TL.Increasing);
      Tash.Test.Test_Case
        (Description => "Get sorted elements of an array",
         Result      => Tash.Lists.To_String (L) =
           "{Empire State Building} {New York} {Faneuil Hall} Boston");

      L :=
         Tash.Arrays.Get_Sorted_Elements
           (B,
            Mode  => TL.SM_ASCII,
            Order => TL.Decreasing);
      Tash.Test.Test_Case
        (Description => "Get sorted elements of an array",
         Result      => Tash.Lists.To_String (L) =
           "{Faneuil Hall} Boston {Empire State Building} {New York}");

      -------------------------------------------------
      --  Test getting indices
      -------------------------------------------------

      Tash.Test.Test_Case
        (Description => "Get indices of an array",
         Result      => Tash.Arrays.Get_Indices (B) =
                        "{Faneuil Hall} {Empire State Building}");

      Tash.Test.Test_Case
        (Description => "Get indices of an array",
         Result      => Tash.Arrays.Get_Indices (B, "*euil*") =
                        "{Faneuil Hall}");

      Tash.Test.Test_Case
        (Description => "Get indices of an array",
         Result      => Tash.Arrays.Get_Indices (B, "Empi*") =
                        "{Empire State Building}");

      L := Tash.Arrays.Get_Indices (B);
      Tash.Test.Test_Case
        (Description => "Get indices of an array",
         Result      => Tash.Lists.To_String (L) =
                        "{Faneuil Hall} {Empire State Building}");

      L := Tash.Arrays.Get_Indices (B, "*euil*");
      Tash.Test.Test_Case
        (Description => "Get indices of an array",
         Result      => Tash.Lists.To_String (L) = "{Faneuil Hall}");

      L := Tash.Arrays.Get_Indices (B, "Emp*");
      Tash.Test.Test_Case
        (Description => "Get indices of an array",
         Result      => Tash.Lists.To_String (L) =
                        "{Empire State Building}");

      Tash.Arrays.PArray (B);

      ----------------------------
      --  Create integer Tash array
      ----------------------------

      C := Tash.Integer_Arrays.To_Tash_Array ("integer", 5);
      Tash.Test.Test_Case
        (Description => "Element is an integer",
         Result      => Tash.Integer_Arrays.Element_Is_Integer (C, "integer"));
      Tash.Test.Test_Case
        (Description => "Element is not a float",
         Result      => not Tash.Float_Arrays.Element_Is_Float (C, "integer"));
      Tash.Test.Test_Case
        (Description => "Verify integer element value",
         Result      => Tash.Integer_Arrays.Get_Element (C, "integer") = 5);

      -----------------------------------------
      --  Test getting integer array elements
      -----------------------------------------

      declare
         Description : constant String :=
           "Get an integer element of an uninitialized array "
             & "-- raise Array_Error";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Integer_Arrays.Get_Element (U, "xyz") = 0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      declare
         Description : constant String :=
            "Get a non-existant element -- raise Array_Error";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Integer_Arrays.Get_Element (C, "xyz") = 0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      declare
         Description : constant String :=
            "Get wrong element type -- raise Constraint_Error";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      =>
              Tash.Integer_Arrays.Get_Element (B, "{Faneuil Hall}") = 0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      -----------------------------------------
      --  Test if element is an integer
      -----------------------------------------

      Tash.Test.Test_Case
        (Description => "A non-existant element is not an integer element",
         Result      => not Tash.Integer_Arrays.Element_Is_Integer (C, "xyz"));

      -------------------------------------------------
      --  Modify an existing integer element of an array
      -------------------------------------------------

      Tash.Integer_Arrays.Set_Element (C, Index => "integer", Value => 100);
      Tash.Test.Test_Case
        (Description => "Modify existing element -- check length",
         Result      => Tash.Arrays.Length (C) = 1);
      Tash.Arrays.Test.Test_Case
        (Description     => "Modify existing element -- check contents",
         Actual_Result   => C,
         Expected_Result => "integer 100",
         Expected_Count  => 1);
      Tash.Test.Test_Case
        (Description => "Modify existing element -- To_String",
         Result      => Tash.Arrays.To_String (C) = "integer 100");

      -----------------------------------------
      --  Add a new integer element to an array
      -----------------------------------------

      Tash.Integer_Arrays.Set_Element (C, Index => "low", Value => 5);
      Tash.Integer_Arrays.Set_Element (C, Index => "high", Value => 500);
      Tash.Test.Test_Case
        (Description => "Add new integer element -- check length",
         Result      => Tash.Arrays.Length (C) = 3);

      L := Tash.Lists.Sort (Tash.Arrays.Get_Indices (C));
      Tash.Test.Test_Case
        (Description => "Add new integer element -- get indices",
         Result      => Tash.Lists.To_String (L) = "high integer low");

      L :=
         Tash.Arrays.Get_Sorted_Elements
           (C,
            Mode  => TL.SM_ASCII,
            Order => TL.Decreasing);
      Tash.Test.Test_Case
        (Description => "Get sorted elements of an array",
         Result      => Tash.Lists.To_String (L) =
                        "low 5 integer 100 high 500");

      Tash.Arrays.PArray (C);

      ----------------------------
      --  Create float Tash array
      ----------------------------

      C := Tash.Float_Arrays.To_Tash_Array ("float", 5.0);
      Tash.Test.Test_Case
        (Description => "Element is a float",
         Result      => Tash.Float_Arrays.Element_Is_Float (C, "float"));
      Tash.Test.Test_Case
        (Description => "Element is not an integer",
         Result      =>
           not Tash.Integer_Arrays.Element_Is_Integer (C, "float"));
      Tash.Test.Test_Case
        (Description => "Verify float element value",
         Result      => Tash.Float_Arrays.Get_Element (C, "float") = 5.0);

      -----------------------------------------
      --  Test getting float array elements
      -----------------------------------------

      declare
         Description : constant String :=
           "Get a float element of an uninitialized array "
             & "-- raise Array_Error";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Float_Arrays.Get_Element (U, "xyz") = 0.0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      declare
         Description : constant String :=
            "Get a non-existant element -- raise Array_Error";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      => Tash.Float_Arrays.Get_Element (C, "xyz") = 0.0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      declare
         Description : constant String :=
            "Get wrong element type -- raise Constraint_Error";
      begin
         Tash.Test.Test_Case
           (Description => Description,
            Result      =>
              Tash.Float_Arrays.Get_Element (B, "{Faneuil Hall}") = 0.0);
         Tash.Test.Test_Case (Description => Description, Result => False);
      exception
         when Tash.Arrays.Array_Error =>
            Tash.Test.Test_Case (Description => Description, Result => True);
         when others =>
            Tash.Test.Test_Case (Description => Description, Result => False);
      end;

      -----------------------------------------
      --  Test if element is a float
      -----------------------------------------

      Tash.Test.Test_Case
        (Description => "A non-existant element is not a float element",
         Result      => not Tash.Float_Arrays.Element_Is_Float (C, "xyz"));

      -------------------------------------------------
      --  Modify an existing float element of an array
      -------------------------------------------------

      Tash.Float_Arrays.Set_Element (C, Index => "float", Value => 100.5);
      Tash.Test.Test_Case
        (Description => "Modify existing element -- check length",
         Result      => Tash.Arrays.Length (C) = 1);
      Tash.Arrays.Test.Test_Case
        (Description     => "Modify existing element -- check contents",
         Actual_Result   => C,
         Expected_Result => "float 100.5",
         Expected_Count  => 1);
      Tash.Test.Test_Case
        (Description => "Modify existing element -- To_String",
         Result      => Tash.Arrays.To_String (C) = "float 100.5");

      -----------------------------------------
      --  Add a new float element to an array
      -----------------------------------------

      Tash.Float_Arrays.Set_Element (C, Index => "low", Value => 5.0);
      Tash.Float_Arrays.Set_Element (C, Index => "high", Value => 500.25);
      Tash.Test.Test_Case
        (Description => "Add new float element -- check length",
         Result      => Tash.Arrays.Length (C) = 3);

      L := Tash.Lists.Sort (Tash.Arrays.Get_Indices (C));
      Tash.Test.Test_Case
        (Description => "Add new float element -- get indices",
         Result      => Tash.Lists.To_String (L) = "float high low");

      L :=
         Tash.Arrays.Get_Sorted_Elements
           (C,
            Mode  => TL.SM_ASCII,
            Order => TL.Decreasing);
      Tash.Test.Test_Case
        (Description => "Get sorted elements of an array",
         Result      => Tash.Lists.To_String (L) =
                        "low 5.0 high 500.25 float 100.5");

      Tash.Arrays.PArray (C);

   end;

   if Tash.Test.All_Test_Cases_Passed then
      Ada.Text_IO.Put_Line
        ("Test_Arrays PASSED --" &
         Integer'Image (Tash.Test.Test_Case_Number) &
         " tests completed");
   else
      Ada.Text_IO.Put_Line ("Test_Arrays FAILED");
   end if;

end Test_Arrays;
