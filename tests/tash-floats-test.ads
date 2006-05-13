--------------------------------------------------------------------
--
--  tash-floats-test.ads -- Test routines for Tash.Floats.
--
--  Copyright (c) 1998 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

package Tash.Floats.Test is

   procedure Set_Verbose (On : in Boolean);

   procedure Test_Case
     (Description     : in String;
      Actual_Result   : in Tash.Floats.Tash_Float;
      Expected_Result : in Long_Float;
      Expected_Type   : in String;
      Expected_Count  : in Natural);

end Tash.Floats.Test;
