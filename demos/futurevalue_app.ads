------------------------------------------------
--
-- futurevalue_app.ads --
--    This program demonstrates how the TASH Ada/Tk interface
--    provides Tk features for use in an Ada program.
--
--    It implements a simple GUI for computing Future Value
--    of a series of fixed monthly investments.
--
--  Copyright (c) 2017-2022 Simon Wright <simon@pushface.org>
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
--
------------------------------------------------

with Interfaces.C;
with Tcl;

package FutureValue_App is

   function Init (Interp : Tcl.Tcl_Interp) return Interfaces.C.int;
   pragma Convention (C, Init);

end FutureValue_App;
