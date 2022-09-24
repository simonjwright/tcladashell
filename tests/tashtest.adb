--------------------------------------------------------------------
--
--  tashtest.adb -- This program is an alternate version of tash which
--                  includes several new Tcl commands used to test the
--                  TASH Ada/Tcl interface.
--
--  Copyright (c) 1995-1997 Terry J. Westley
--  Copyright (c) 2006-2022  Simon Wright
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with CArgv;
with Tcl;     use Tcl;
with TestApp;

procedure TaShTest is --  Tcl Ada SHell Test

   --  Argc and Argv include the command name
   Argc : C.int;
   Argv : CArgv.Chars_Ptr_Ptr;

begin --  TaShTest

   --  Get command-line arguments and put them into C-style "argv,"
   --  as required by Tcl_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl (and never return!)
   Tcl_Main (Argc, Argv, TestApp.Init'Access);

end TaShTest;
