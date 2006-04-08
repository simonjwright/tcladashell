--------------------------------------------------------------------

--

-- tash-lists-test.ads -- Routines for testing Tash lists.

--

-- Copyright (c) 1998 Terry J. Westley

--

-- See the file "license.htm" for information on usage and

-- redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.

--

--------------------------------------------------------------------



package Tash.Lists.Test is



   procedure Set_Verbose (On : in Boolean);

   -- Turns verbose printing (to standard output) on or off.



   procedure Test_Case (

      Description     : in String;

      Actual_Result   : in Tash.Lists.Tash_List;

      Expected_Result : in String;

      Expected_Type   : in String;

      Expected_Count  : in Natural;

      Expected_Length : in Natural);

   -- Increment test case number and (if verbose is on)

   -- report results to standard output.



end Tash.Lists.Test;

