------------------------------------------------
--
-- futurevalue.adb --
--    This program demonstrates how the TASH Ada/Tk interface
--    provides Tk features for use in an Ada program.
--
--    It implements a simple GUI for computing Future Value
--    of a series of fixed monthly investments.
--
--  Copyright (c) 1997 Terry J. Westley
--  Copyright (c) 2017-2022 Simon Wright <simon@pushface.org>
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
--
------------------------------------------------

with CArgv;
with Interfaces.C;
with Tcl.Tk;

with FutureValue_App;

procedure FutureValue is
   Argc : Interfaces.C.int;
   Argv : CArgv.Chars_Ptr_Ptr;
begin
   --  Get command-line arguments and put them into C-style "argv,"
   --  as required by Tcl_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl/Tk (and never return!)
   Tcl.Tk.Tk_Main (Argc, Argv, FutureValue_App.Init'Access);
end FutureValue;
