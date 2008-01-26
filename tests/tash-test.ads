--------------------------------------------------------------------
--
--  Unit Name:    Tash.Test spec
--
--  File Name:    tash-test.ads
--
--  Purpose:      Provides Tash testing routines that apply to all
--               Tash types.
--
--
--  Copyright (c) 1999-2000 Terry J. Westley
--
--  Tash is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version. Tash is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. You should have received a copy of the GNU General
--  Public License distributed with Tash; see file COPYING. If not, write to
--
--          Free Software Foundation
--          59 Temple Place - Suite 330
--          Boston, MA 02111-1307, USA
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
--  Tash is maintained at http://tcladashell.sourceforge.net/.
--
--------------------------------------------------------------------

package Tash.Test is

   procedure Set_Verbose (On : in Boolean);
   --  Turns verbose printing (to standard output) on or off.

   procedure Fail_Test_Case;
   --  Record the fact that a single test case failed.

   procedure Incr_Test_Case_Number;
   --  Increment the test case number.

   function Test_Case_Number return Natural;
   --  Get current value of test case number.

   procedure Test_Case
     (Description : in String;
      Result      : in Boolean;
      Failure_Msg : in String := "");
   --  Increment test case number and (if verbose is on)
   --  report results to standard output.  If Result is False,
   --  print Failure_Msg.

   function All_Test_Cases_Passed return Boolean;
   --  Determine whether all test cases considered so far
   --  have passed or not.

end Tash.Test;
