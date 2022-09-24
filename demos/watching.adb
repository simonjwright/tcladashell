--  Copyright 2017-2022 Simon Wright <simon@pushface.org>
--
--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This program (with watching.tcl) demonstrates the use of
--  Tcl_SetVar[2]() to get an Ada-domain value back into the Tcl
--  domain without the use of polling.

with CArgv;
with Interfaces.C;
with Tcl;

with Watching_Support;

procedure Watching is

   --  Argc and Argv include the command name
   Argc : Interfaces.C.int;
   Argv : CArgv.Chars_Ptr_Ptr;

begin

   --  Get command-line arguments and put them into C-style "argv",
   --  as required by Tcl_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl (and never return!)
   Tcl.Tcl_Main (Argc, Argv, Watching_Support.Init'Unrestricted_Access);

end Watching;
