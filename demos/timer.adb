------------------------------------------------
--
--  timer.adb -- This program demonstrates how the TASH Ada/Tk interface
--              provides Tk features for use in an Ada program.
--
--  Copyright (c) 2017-2022 Simon Wright <simon@pushface.org>
--  Copyright (c) 1996-1999 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
--
--  This program was adapted from the demo program distributed with Tk.
--  It provides a simple stop watch timer facility.
--
------------------------------------------------

with CArgv;
with Interfaces.C;
with Tcl.Tk;

with Timer_App;

procedure Timer is
   Argc : Interfaces.C.int;
   Argv : CArgv.Chars_Ptr_Ptr;
begin
   --  Get command-line arguments and put them into C-style "argv,"
   --  as required by Tcl_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl/Tk (and never return!)
   Tcl.Tk.Tk_Main (Argc, Argv, Timer_App.Init'Access);
end Timer;
