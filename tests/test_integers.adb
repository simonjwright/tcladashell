with Ada.Command_Line;
with Ada.Text_IO;
with Tash.Integers;
with Tash.Integers.Test;
with Tash.Test;

procedure Test_Integers is

   use type Tash.Integers.Tash_Integer; --  to make operators visible

   Verbose : Boolean := False;

begin --  Test_Integers

   --------------------------------------------
   --  Check for -verbose command line argument
   --------------------------------------------
   GET_COMMAND_LINE_ARGUMENTS : for I in  1 ..
        Ada.Command_Line.Argument_Count
   loop
      if Ada.Command_Line.Argument (I) = "-verbose" then
         Verbose := True;
         exit GET_COMMAND_LINE_ARGUMENTS;
      end if;
   end loop GET_COMMAND_LINE_ARGUMENTS;

   Tash.Test.Set_Verbose (On => Verbose);
   Tash.Integers.Test.Set_Verbose (On => Verbose);

   declare

      A : Tash.Integers.Tash_Integer;
      B : Tash.Integers.Tash_Integer;
      C : Tash.Integers.Tash_Integer;

   begin

      --------------------------------------------------------
      --  Test conversion of Ada integers to/from Tash integers
      --------------------------------------------------------
      Tash.Integers.Test.Test_Case
        ("Convert uninitialized Tash integer to Ada integer",
         A,
         0,
         "",
         0);

      A := Tash.Integers.Null_Tash_Integer;
      Tash.Integers.Test.Test_Case
        ("Convert null Tash integer to Ada integer",
         A,
         0,
         "",
         0);

      A := Tash.Integers.To_Tash_Integer (1);
      Tash.Integers.Test.Test_Case
        ("Convert Ada integer to Tash integer",
         A,
         1,
         "int",
         1);

      A := +2;
      Tash.Integers.Test.Test_Case
        ("Convert Ada integer to Tash integer",
         A,
         2,
         "int",
         1);

      A := -3;
      Tash.Integers.Test.Test_Case
        ("Convert Ada integer to Tash integer",
         A,
         -3,
         "int",
         1);

      A := +2;
      B := +2;
      Tash.Integers.Test.Test_Case
        ("Convert Tash integer to Ada integer",
         A,
         Tash.Integers.To_Integer (B),
         "int",
         1);

      A := -3;
      B := +3;
      Tash.Integers.Test.Test_Case
        ("Convert Tash integer to Ada integer",
         A,
         -Tash.Integers.To_Integer (B),
         "int",
         1);

      --  A := Tash.Strings.To_Tash_String ("0x10");
      --  Tash.Integers.Test.Test_Case (
      --   "Convert C-style hex integer to Ada integer", A, 16, "int", 1);

      --  A := Tash.Strings.To_Tash_String ("010");
      --  Tash.Integers.Test.Test_Case (
      --   "Convert C-style octal integer to Ada integer", A, 8, "int", 1);

      --------------------------------------------------------
      --  Test Ada and Tash integer comparisons
      --------------------------------------------------------

      declare
         T : Tash.Integers.Tash_Integer;
         U : Tash.Integers.Tash_Integer;
         Y : Tash.Integers.Tash_Integer;
         Z : Tash.Integers.Tash_Integer;
      begin
         T := +1;
         Z := +0;
         Y := -1;
         Tash.Test.Test_Case ("Uninitialized Tash integer  = 0", U = Z);
         Tash.Test.Test_Case ("Uninitialized Tash integer /= 1", U /= T);
         Tash.Test.Test_Case ("Uninitialized Tash integer <  1", U < T);
         Tash.Test.Test_Case ("Uninitialized Tash integer <= 0", U <= Z);
         Tash.Test.Test_Case ("Uninitialized Tash integer <= 1", U <= T);
         Tash.Test.Test_Case ("Uninitialized Tash integer > -1", U > Y);
         Tash.Test.Test_Case ("Uninitialized Tash integer >= 0", U >= Z);
         Tash.Test.Test_Case ("Uninitialized Tash integer >= -1", U >= Y);
         Tash.Test.Test_Case ("0    = Uninitialized Tash integer", Z = U);
         Tash.Test.Test_Case ("1   /= Uninitialized Tash integer", T /= U);
         Tash.Test.Test_Case ("-1  <  Uninitialized Tash integer", Y < U);
         Tash.Test.Test_Case ("0   <= Uninitialized Tash integer", Z <= U);
         Tash.Test.Test_Case ("-1  <= Uninitialized Tash integer", Y <= U);
         Tash.Test.Test_Case ("1   >  Uninitialized Tash integer", T > U);
         Tash.Test.Test_Case ("0   >= Uninitialized Tash integer", Z >= U);
         Tash.Test.Test_Case ("1   >= Uninitialized Tash integer", T >= U);
      end;

      A := +1;
      Tash.Test.Test_Case ("Tash integer  = itself", A = A);
      Tash.Test.Test_Case ("Tash integer <= itself", A <= A);
      Tash.Test.Test_Case ("Tash integer >= itself", A >= A);

      A := +1;
      B := +1;
      C := +2;
      Tash.Test.Test_Case ("Tash integer  = another", A = B);
      Tash.Test.Test_Case ("Tash integer /= another", A /= C);
      Tash.Test.Test_Case ("Tash integer <  another", A < C);
      Tash.Test.Test_Case ("Tash integer <= another", A <= B);
      Tash.Test.Test_Case ("Tash integer <= another", A <= C);
      Tash.Test.Test_Case ("Tash integer >  another", C > A);
      Tash.Test.Test_Case ("Tash integer >= another", B >= A);
      Tash.Test.Test_Case ("Tash integer >= another", C >= A);

      A := +1;
      Tash.Test.Test_Case ("Tash integer  = Ada integer", A = 1);
      Tash.Test.Test_Case ("Tash integer /= Ada integer", A /= 2);
      Tash.Test.Test_Case ("Tash integer <  Ada integer", A < 2);
      Tash.Test.Test_Case ("Tash integer <= Ada integer", A <= 1);
      Tash.Test.Test_Case ("Tash integer <= Ada integer", A <= 2);
      Tash.Test.Test_Case ("Tash integer >  Ada integer", A > 0);
      Tash.Test.Test_Case ("Tash integer >= Ada integer", A >= 1);
      Tash.Test.Test_Case ("Tash integer >= Ada integer", A >= 0);
      Tash.Test.Test_Case ("Ada integer   = Tash integer", 1 = A);
      Tash.Test.Test_Case ("Ada integer  /= Tash integer", 2 /= A);
      Tash.Test.Test_Case ("Ada integer  <  Tash integer", 0 < A);
      Tash.Test.Test_Case ("Ada integer  <= Tash integer", 1 <= A);
      Tash.Test.Test_Case ("Ada integer  <= Tash integer", 0 <= A);
      Tash.Test.Test_Case ("Ada integer  >  Tash integer", 2 > A);
      Tash.Test.Test_Case ("Ada integer  >= Tash integer", 1 >= A);
      Tash.Test.Test_Case ("Ada integer  >= Tash integer", 2 >= A);

      -------------------------------------------
      --  Test increment
      -------------------------------------------
      declare
         U : Tash.Integers.Tash_Integer;
      begin
         Tash.Integers.Incr (U);
         Tash.Integers.Test.Test_Case
           ("Increment uninitialized Tash integer",
            U,
            1,
            "int",
            1);
      end;

      A := +1;
      Tash.Integers.Incr (A);
      Tash.Integers.Test.Test_Case
        ("Increment Tash integer using default parameter",
         A,
         2,
         "int",
         1);

      Tash.Integers.Incr (A, 1);
      Tash.Integers.Test.Test_Case
        ("Increment Tash integer using explicit parameter",
         A,
         3,
         "int",
         1);

      Tash.Integers.Incr (A, By => -1);
      Tash.Integers.Test.Test_Case
        ("Increment Tash integer using negative parameter",
         A,
         2,
         "int",
         1);

      B := +2;
      Tash.Integers.Incr (A, B);
      Tash.Integers.Test.Test_Case
        ("Increment Tash integer using Tash integer parameter",
         A,
         4,
         "int",
         1);

      --------------------------------------------------------
      --  Test Tash integer arithmetic operations
      --------------------------------------------------------

      A := +5;
      Tash.Integers.Test.Test_Case
        ("Take abs of Tash integer",
         abs A,
         5,
         "int",
         1);

      A := -5;
      Tash.Integers.Test.Test_Case
        ("Take abs of Tash integer",
         abs A,
         5,
         "int",
         1);

      A := +1;
      B := +100;
      Tash.Integers.Test.Test_Case
        ("Add two Tash integers",
         A + B,
         101,
         "int",
         1);

      A := -1;
      B := -3;
      Tash.Integers.Test.Test_Case
        ("Add two negative Tash integers",
         A + B,
         -4,
         "int",
         1);

      A := +1;
      Tash.Integers.Test.Test_Case
        ("Add Tash integer and Ada  integer",
         A + 5,
         6,
         "int",
         1);
      Tash.Integers.Test.Test_Case
        ("Add Ada  integer and Tash integer",
         6 + A,
         7,
         "int",
         1);

      A := +100;
      B := +1;
      Tash.Integers.Test.Test_Case
        ("Subtract two Tash integers",
         A - B,
         99,
         "int",
         1);

      A := -1;
      B := -3;
      Tash.Integers.Test.Test_Case
        ("Subtract two negative Tash integers",
         A - B,
         2,
         "int",
         1);

      A := +1;
      Tash.Integers.Test.Test_Case
        ("Subtract Tash integer and Ada  integer",
         A - 5,
         -4,
         "int",
         1);
      Tash.Integers.Test.Test_Case
        ("Subtract Ada  integer and Tash integer",
         6 - A,
         5,
         "int",
         1);

      A := +3;
      B := +5;
      Tash.Integers.Test.Test_Case
        ("Multiply two Tash integers",
         A * B,
         15,
         "int",
         1);

      A := -3;
      B := -5;
      Tash.Integers.Test.Test_Case
        ("Multiply two negative Tash integers",
         A * B,
         15,
         "int",
         1);

      A := +3;
      Tash.Integers.Test.Test_Case
        ("Multiply Tash integer and Ada  integer",
         A * 5,
         15,
         "int",
         1);
      Tash.Integers.Test.Test_Case
        ("Multiply Ada  integer and Tash integer",
         6 * A,
         18,
         "int",
         1);

      for I in  Long_Integer'(10) .. Long_Integer'(15) loop
         A := +I;
         B := +5;
         Tash.Integers.Test.Test_Case
           ("Divide      two Tash integers",
            A / B,
            I / 5,
            "int",
            1);
         Tash.Integers.Test.Test_Case
           ("Find rem of two Tash integers",
            A rem B,
            I rem 5,
            "int",
            1);
         Tash.Integers.Test.Test_Case
           ("Find mod of two Tash integers",
            A mod B,
            I mod 5,
            "int",
            1);
         A := -I;
         Tash.Integers.Test.Test_Case
           ("Divide      two Tash integers",
            A / B,
            -I / 5,
            "int",
            1);
         Tash.Integers.Test.Test_Case
           ("Find rem of two Tash integers",
            A rem B,
            -I rem 5,
            "int",
            1);
         Tash.Integers.Test.Test_Case
           ("Find mod of two Tash integers",
            A mod B,
            (-I) mod 5,
            "int",
            1);
         A := +I;
         B := -5;
         Tash.Integers.Test.Test_Case
           ("Divide      two Tash integers",
            A / B,
            I / (-5),
            "int",
            1);
         Tash.Integers.Test.Test_Case
           ("Find rem of two Tash integers",
            A rem B,
            I rem (-5),
            "int",
            1);
         Tash.Integers.Test.Test_Case
           ("Find mod of two Tash integers",
            A mod B,
            I mod (-5),
            "int",
            1);
         A := -I;
         Tash.Integers.Test.Test_Case
           ("Divide      two Tash integers",
            A / B,
            (-I) / (-5),
            "int",
            1);
         Tash.Integers.Test.Test_Case
           ("Find rem of two Tash integers",
            A rem B,
            (-I) rem (-5),
            "int",
            1);
         Tash.Integers.Test.Test_Case
           ("Find mod of two Tash integers",
            A mod B,
            (-I) mod (-5),
            "int",
            1);
      end loop;

      A := +10;
      Tash.Integers.Test.Test_Case
        ("Divide      Tash integer and Ada  integer",
         A / 5,
         2,
         "int",
         1);
      Tash.Integers.Test.Test_Case
        ("Divide      Ada  integer and Tash integer",
         20 / A,
         2,
         "int",
         1);
      Tash.Integers.Test.Test_Case
        ("Find rem of Tash integer and Ada  integer",
         A rem 4,
         2,
         "int",
         1);
      Tash.Integers.Test.Test_Case
        ("Find rem of Ada  integer and Tash integer",
         21 rem A,
         1,
         "int",
         1);
      Tash.Integers.Test.Test_Case
        ("Find mod of Tash integer and Ada  integer",
         A mod 4,
         2,
         "int",
         1);
      Tash.Integers.Test.Test_Case
        ("Find mod of Ada  integer and Tash integer",
         21 mod A,
         1,
         "int",
         1);

      A := +3;
      Tash.Integers.Test.Test_Case
        ("Exponentiate Tash integer and Ada  integer",
         A ** 5,
         3 ** 5,
         "int",
         1);

   end;

   if Tash.Test.All_Test_Cases_Passed then
      Ada.Text_IO.Put_Line ("Test_Integers PASSED");
   else
      Ada.Text_IO.Put_Line ("Test_Integers FAILED");
   end if;

end Test_Integers;
