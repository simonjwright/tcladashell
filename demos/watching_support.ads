--  Copyright 2017-2022 Simon Wright <simon@pushface.org>
--
--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This program (with watching.tcl) demonstrates the use of
--  Tcl_SetVar[2]() to get an Ada-domain value back into the Tcl
--  domain without the use of polling.

with Interfaces.C;
with Tcl;

package Watching_Support is

   function Init (Interp : Tcl.Tcl_Interp) return Interfaces.C.int;
   pragma Convention (C, Init);

end Watching_Support;
