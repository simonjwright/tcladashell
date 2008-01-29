--------------------------------------------------------------------
--
--  tash-strings-test.ads -- Test routines for Tash.Strings.
--
--  Copyright (c) 1998 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

package Tash.Strings.Test is

   procedure Set_Verbose (On : in Boolean);

   procedure Test_Case (
      Description     : in String;
      Actual_Result   : in Tash.Strings.Tash_String;
      Expected_Result : in String;
      Expected_Count  : in Natural);

end Tash.Strings.Test;
