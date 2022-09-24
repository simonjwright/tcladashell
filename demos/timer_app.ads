------------------------------------------------
--
--  timer_app.ads -- This program demonstrates how the TASH Ada/Tk interface
--                    provides Tk features for use in an Ada program.
--
--  Copyright (c) 2017-2022 Simon Wright <simon@pushface.org>
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
--
--  This program was adapted from the demo program distributed with Tk.
--  It provides a simple stop watch timer facility.
--
------------------------------------------------

with Interfaces.C;
with Tcl;

package Timer_App is

   function Init (Interp : Tcl.Tcl_Interp) return Interfaces.C.int;
   pragma Convention (C, Init);

end Timer_App;
