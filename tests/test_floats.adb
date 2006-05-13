with Ada.Command_Line;
with Ada.Text_IO;
with Tash.Floats;
with Tash.Floats.Test;
with Tash.Test;

procedure Test_Floats is

   use type Tash.Floats.Tash_Float; --  to make operators visible

   Verbose : Boolean := False;

begin --  Test_Floats

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
   Tash.Floats.Test.Set_Verbose (On => Verbose);

   declare

      A : Tash.Floats.Tash_Float;
      B : Tash.Floats.Tash_Float;
      C : Tash.Floats.Tash_Float;

   begin

      --------------------------------------------------------
      --  Test conversion of Ada floats to/from Tash floats
      --------------------------------------------------------
      Tash.Floats.Test.Test_Case
        ("Convert uninitialized Tash float to Ada float",
         A,
         0.0,
         "",
         0);

      A := Tash.Floats.Null_Tash_Float;
      Tash.Floats.Test.Test_Case
        ("Convert null Tash float to Ada float",
         A,
         0.0,
         "",
         0);

      A := Tash.Floats.To_Tash_Float (1.0);
      Tash.Floats.Test.Test_Case
        ("Convert Ada float to Tash float",
         A,
         1.0,
         "double",
         1);

      A := +2.0;
      Tash.Floats.Test.Test_Case
        ("Convert Ada float to Tash float",
         A,
         2.0,
         "double",
         1);

      A := -3.0;
      Tash.Floats.Test.Test_Case
        ("Convert Ada float to Tash float",
         A,
         -3.0,
         "double",
         1);

      A := +2.0;
      B := +2.0;
      Tash.Floats.Test.Test_Case
        ("Convert Tash float to Ada float",
         A,
         Tash.Floats.To_Float (B),
         "double",
         1);

      A := -3.0;
      B := +3.0;
      Tash.Floats.Test.Test_Case
        ("Convert Tash float to Ada float",
         A,
         -Tash.Floats.To_Float (B),
         "double",
         1);

      --------------------------------------------------------
      --  Test Ada and Tash float comparisons
      --------------------------------------------------------

      declare
         T : Tash.Floats.Tash_Float;
         U : Tash.Floats.Tash_Float;
         Y : Tash.Floats.Tash_Float;
         Z : Tash.Floats.Tash_Float;
      begin
         T := +1.0;
         Z := +0.0;
         Y := -1.0;
         Tash.Test.Test_Case ("Uninitialized Tash float  = 0.0", U = Z);
         Tash.Test.Test_Case ("Uninitialized Tash float /= 1.0", U /= T);
         Tash.Test.Test_Case ("Uninitialized Tash float <  1.0", U < T);
         Tash.Test.Test_Case ("Uninitialized Tash float <= 0.0", U <= Z);
         Tash.Test.Test_Case ("Uninitialized Tash float <= 1.0", U <= T);
         Tash.Test.Test_Case ("Uninitialized Tash float > -1.0", U > Y);
         Tash.Test.Test_Case ("Uninitialized Tash float >= 0.0", U >= Z);
         Tash.Test.Test_Case ("Uninitialized Tash float >= -1.0", U >= Y);
         Tash.Test.Test_Case ("0.0    = Uninitialized Tash float", Z = U);
         Tash.Test.Test_Case ("1.0   /= Uninitialized Tash float", T /= U);
         Tash.Test.Test_Case ("-1.0  <  Uninitialized Tash float", Y < U);
         Tash.Test.Test_Case ("0.0   <= Uninitialized Tash float", Z <= U);
         Tash.Test.Test_Case ("-1.0  <= Uninitialized Tash float", Y <= U);
         Tash.Test.Test_Case ("1.0   >  Uninitialized Tash float", T > U);
         Tash.Test.Test_Case ("0.0   >= Uninitialized Tash float", Z >= U);
         Tash.Test.Test_Case ("1.0   >= Uninitialized Tash float", T >= U);
      end;

      A := +1.0;
      Tash.Test.Test_Case ("Tash float  = itself", A = A);
      Tash.Test.Test_Case ("Tash float <= itself", A <= A);
      Tash.Test.Test_Case ("Tash float >= itself", A >= A);

      A := +1.0;
      B := +1.0;
      C := +2.0;
      Tash.Test.Test_Case ("Tash float  = another", A = B);
      Tash.Test.Test_Case ("Tash float /= another", A /= C);
      Tash.Test.Test_Case ("Tash float <  another", A < C);
      Tash.Test.Test_Case ("Tash float <= another", A <= B);
      Tash.Test.Test_Case ("Tash float <= another", A <= C);
      Tash.Test.Test_Case ("Tash float >  another", C > A);
      Tash.Test.Test_Case ("Tash float >= another", B >= A);
      Tash.Test.Test_Case ("Tash float >= another", C >= A);

      A := +1.0;
      Tash.Test.Test_Case ("Tash float  = Ada float", A = 1.0);
      Tash.Test.Test_Case ("Tash float /= Ada float", A /= 2.0);
      Tash.Test.Test_Case ("Tash float <  Ada float", A < 2.0);
      Tash.Test.Test_Case ("Tash float <= Ada float", A <= 1.0);
      Tash.Test.Test_Case ("Tash float <= Ada float", A <= 2.0);
      Tash.Test.Test_Case ("Tash float >  Ada float", A > 0.0);
      Tash.Test.Test_Case ("Tash float >= Ada float", A >= 1.0);
      Tash.Test.Test_Case ("Tash float >= Ada float", A >= 0.0);
      Tash.Test.Test_Case ("Ada float   = Tash float", 1.0 = A);
      Tash.Test.Test_Case ("Ada float  /= Tash float", 2.0 /= A);
      Tash.Test.Test_Case ("Ada float  <  Tash float", 0.0 < A);
      Tash.Test.Test_Case ("Ada float  <= Tash float", 1.0 <= A);
      Tash.Test.Test_Case ("Ada float  <= Tash float", 0.0 <= A);
      Tash.Test.Test_Case ("Ada float  >  Tash float", 2.0 > A);
      Tash.Test.Test_Case ("Ada float  >= Tash float", 1.0 >= A);
      Tash.Test.Test_Case ("Ada float  >= Tash float", 2.0 >= A);

      --------------------------------------------------------
      --  Test Tash float arithmetic operations
      --------------------------------------------------------

      A := +5.0;
      Tash.Floats.Test.Test_Case
        ("Take abs of Tash float",
         abs A,
         5.0,
         "double",
         1);

      A := -5.0;
      Tash.Floats.Test.Test_Case
        ("Take abs of Tash float",
         abs A,
         5.0,
         "double",
         1);

      A := +1.0;
      B := +100.0;
      Tash.Floats.Test.Test_Case
        ("Add two Tash floats",
         A + B,
         101.0,
         "double",
         1);

      A := -1.0;
      B := -3.0;
      Tash.Floats.Test.Test_Case
        ("Add two negative Tash floats",
         A + B,
         -4.0,
         "double",
         1);

      A := +1.0;
      Tash.Floats.Test.Test_Case
        ("Add Tash float and Ada  float",
         A + 5.0,
         6.0,
         "double",
         1);
      Tash.Floats.Test.Test_Case
        ("Add Ada  float and Tash float",
         6.0 + A,
         7.0,
         "double",
         1);

      A := +100.0;
      B := +1.0;
      Tash.Floats.Test.Test_Case
        ("Subtract two Tash floats",
         A - B,
         99.0,
         "double",
         1);

      A := -1.0;
      B := -3.0;
      Tash.Floats.Test.Test_Case
        ("Subtract two negative Tash floats",
         A - B,
         2.0,
         "double",
         1);

      A := +1.0;
      Tash.Floats.Test.Test_Case
        ("Subtract Tash float and Ada  float",
         A - 5.0,
         -4.0,
         "double",
         1);
      Tash.Floats.Test.Test_Case
        ("Subtract Ada  float and Tash float",
         6.0 - A,
         5.0,
         "double",
         1);

      A := +3.0;
      B := +5.0;
      Tash.Floats.Test.Test_Case
        ("Multiply two Tash floats",
         A * B,
         15.0,
         "double",
         1);

      A := -3.0;
      B := -5.0;
      Tash.Floats.Test.Test_Case
        ("Multiply two negative Tash floats",
         A * B,
         15.0,
         "double",
         1);

      A := +3.0;
      Tash.Floats.Test.Test_Case
        ("Multiply Tash float and Ada  float",
         A * 5.0,
         15.0,
         "double",
         1);
      Tash.Floats.Test.Test_Case
        ("Multiply Ada  float and Tash float",
         6.0 * A,
         18.0,
         "double",
         1);

      for I in  10 .. 15 loop
         A := +Long_Float (I);
         B := +5.0;
         Tash.Floats.Test.Test_Case
           ("Divide      two Tash floats",
            A / B,
            Long_Float (I) / 5.0,
            "double",
            1);
         A := -Long_Float (I);
         Tash.Floats.Test.Test_Case
           ("Divide      two Tash floats",
            A / B,
            Long_Float (-I) / 5.0,
            "double",
            1);
         A := +Long_Float (I);
         B := -5.0;
         Tash.Floats.Test.Test_Case
           ("Divide      two Tash floats",
            A / B,
            Long_Float (I) / (-5.0),
            "double",
            1);
         A := -Long_Float (I);
         Tash.Floats.Test.Test_Case
           ("Divide      two Tash floats",
            A / B,
            Long_Float (-I) / (-5.0),
            "double",
            1);
      end loop;

      A := +10.0;
      Tash.Floats.Test.Test_Case
        ("Divide      Tash float and Ada  float",
         A / 5.0,
         2.0,
         "double",
         1);
      Tash.Floats.Test.Test_Case
        ("Divide      Ada  float and Tash float",
         20.0 / A,
         2.0,
         "double",
         1);

      A := +3.0;
      Tash.Floats.Test.Test_Case
        ("Exponentiate Tash float and Ada  integer",
         A ** 5,
         3.0 ** 5,
         "double",
         1);

   end;

   if Tash.Test.All_Test_Cases_Passed then
      Ada.Text_IO.Put_Line ("Test_Floats PASSED");
   else
      Ada.Text_IO.Put_Line ("Test_Floats FAILED");
   end if;

end Test_Floats;
