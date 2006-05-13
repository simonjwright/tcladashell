--------------------------------------------------------------------
--
--  tash-arrays-test.ads -- Test routines for Tash.Arrays.
--
--  Copyright (c) 1999 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

package Tash.Arrays.Test is

   procedure Set_Verbose (On : in Boolean);

   procedure Test_Case
     (Description     : in String;
      Actual_Result   : in Tash.Arrays.Tash_Array;
      Expected_Result : in String;
      Expected_Count  : in Natural);
   --  Actual result is sorted via Tash.Arrays.Get_Sorted_Elements
   --  before comparing with Expected_Result.

end Tash.Arrays.Test;
