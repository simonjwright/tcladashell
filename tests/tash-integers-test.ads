--------------------------------------------------------------------
--
-- tash-integers-test.ads -- Test routines for Tash.Integers.
--
-- Copyright (c) 1998 Terry J. Westley
--
-- See the file "license.htm" for information on usage and
-- redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with Ada.Finalization;
with Interfaces.C;
with Tcl;

package Tash.Integers.Test is

   procedure Set_Verbose (On : in Boolean);

   procedure Test_Case (
      Description     : in String;
      Actual_Result   : in Tash.Integers.Tash_Integer;
      Expected_Result : in Long_Integer;
      Expected_Type   : in String;
      Expected_Count  : in Natural);

end Tash.Integers.Test;
