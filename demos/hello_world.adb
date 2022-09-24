--------------------------------------------------------------------
--
--  Unit Name:    Hello_World body
--
--  File Name:    hello_world.adb
--
--  Purpose:      This procedure is a dummy main unit which withs
--                all the packages to be included in the TASH
--                library so that gnatmake can build all units.
--
--                It also serves as a quick test that everything
--                compiled OK.
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
--          Software Foundation
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

with Ada.Text_IO;
with CArgv;
with Interfaces.C;
with Tcl.Ada;
with Tcl.Tk.Ada;

procedure Hello_World is --  Hello_World

   use type Interfaces.C.int;

   package CreateCommands is new Tcl.Ada.Generic_Command (Integer);

   Argc         : Interfaces.C.int;
   Argv         : CArgv.Chars_Ptr_Ptr;
   Interp       : Tcl.Tcl_Interp;
   Hello_Button : Tcl.Tk.Ada.Button;
   Exit_Button  : Tcl.Tk.Ada.Button;
   Command      : Tcl.Tcl_Command;
   pragma Unreferenced (Command);

   function Hello_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int;
   pragma Convention (C, Hello_Command);
   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will print "Hello World" when the "Hello World"
   --  button is pressed.

   function Hello_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int
   is
      pragma Unreferenced (ClientData, Interp, Argc, Argv);
   begin --  Hello_Command
      Ada.Text_IO.Put_Line ("Hello: Welcome to my TASH world!");
      return Tcl.TCL_OK;
   end Hello_Command;

   function Exit_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int;
   pragma Convention (C, Exit_Command);
   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will terminate the program when pressed.

   function Exit_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int
   is
      pragma Unreferenced (ClientData, Argc, Argv);
      Result : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin --  Exit_Command
      Result := Tcl.Ada.Tcl_Eval (Interp, "destroy .");
      return Tcl.Ada.Tcl_Eval (Interp, "exit");
   end Exit_Command;

begin --  Hello_World

   --  Get command-line arguments and put them into C-style "argv"
   --------------------------------------------------------------
   CArgv.Create (Argc, Argv);

   --  Tcl needs to know the path name of the executable
   --  otherwise Tcl.Tcl_Init below will fail.
   ----------------------------------------------------
   Tcl.Tcl_FindExecutable (Argv.all);

   --  Create one Tcl interpreter
   -----------------------------
   Interp := Tcl.Tcl_CreateInterp;

   --  Initialize Tcl
   -----------------
   if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        ("Hello_World: Tcl.Tcl_Init failed: " &
         Tcl.Ada.Tcl_GetStringResult (Interp));
      return;
   end if;

   --  Initialize Tk
   ----------------
   if Tcl.Tk.Tk_Init (Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line ("Cannot run GUI version of hello_world: ");
      Ada.Text_IO.Put_Line ("   " & Tcl.Ada.Tcl_GetStringResult (Interp));
      Ada.Text_IO.Put_Line ("Hello: Welcome to my TASH world!");
      return;
   end if;

   --  Set the Tk context so that we may use shortcut Tk
   --  calls that require reference to the interpreter.
   ----------------------------------------------------
   Tcl.Tk.Ada.Set_Context (Interp);

   --  Create several new Tcl commands to call Ada subprograms
   ----------------------------------------------------------
   Command :=
      CreateCommands.Tcl_CreateCommand
        (Interp,
         "Hello",
         Hello_Command'Access,
         0,
         null);
   Command :=
      CreateCommands.Tcl_CreateCommand
        (Interp,
         "Exit",
         Exit_Command'Access,
         0,
         null);

   --  Create and pack the Hello button
   -----------------------------------
   Hello_Button := Tcl.Tk.Ada.Create (".hello", "-text Hello -command Hello");
   Tcl.Tk.Ada.Pack (Hello_Button, "-side left -fill both -expand yes");

   --  Create and pack the Exit button
   ----------------------------------
   Exit_Button := Tcl.Tk.Ada.Create (".exit", "-text Exit -command Exit");
   Tcl.Tk.Ada.Pack (Exit_Button, "-side left -fill both -expand yes");

   --  Loop inside Tk, waiting for commands to execute.
   --  When there are no windows left, Tcl.Tk.Tk_MainLoop returns and we exit.
   --------------------------------------------------------------------------
   Tcl.Tk.Tk_MainLoop;

end Hello_World;
