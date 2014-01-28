------------------------------------------------
--
--  timer.adb -- This program demonstrates how the TASH Ada/Tk interface
--              provides Tk features for use in an Ada program.
--
--  Copyright (c) 1996-1999 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
--
--  This program was adapted from the demo program distributed with Tk.
--  It provides a simple stop watch timer facility.
--
------------------------------------------------

with Ada.Command_Line;
with CArgv;
with Interfaces.C.Strings;
with Tcl;
with Tcl.Ada;
with Tcl.Tk;
with Tcl.Tk.Ada;
with Text_IO;

procedure Timer is --  Timer

   package C renames Interfaces.C;
   package CreateCommands is new Tcl.Ada.Generic_Command (Integer);

   function "=" (Left, Right : in C.int) return Boolean renames C. "=";

   type Timer_Type is delta 0.01 digits 8 range 0.0 .. 999999.99;

   Command_Name : constant String :=
      Ada.Command_Line.Command_Name;
   Executable   : aliased Interfaces.C.char_array :=
      Interfaces.C.To_C (Command_Name);
   Interp       : Tcl.Tcl_Interp;
   Counter      : Tcl.Tk.Ada.Label;
   Start_Button : Tcl.Tk.Ada.Button;
   Stop_Button  : Tcl.Tk.Ada.Button;
   Time_Value   : Timer_Type                      := 0.0;
   Stopped      : Boolean                         := True;
   Command      : Tcl.Tcl_Command;
   pragma Unreferenced (Command);

   --  Update the window by displaying the current value of the timer.
   -------------------------------------------------------------------
   procedure Update;
   procedure Update is
   begin --  Update
      Tcl.Tk.Ada.configure
        (Counter,
         "-text " & Timer_Type'Image (Time_Value));
   end Update;

   --  Increment the timer by one "tick."  A tick is 50 milliseconds
   --  (or 5 hundredths of a second).
   -----------------------------------------------------------------
   procedure Tick;
   procedure Tick is

   begin --  Tick

      --  if the timer is stopped, do not increment
      --  its value or reschedule tick for future execution.
      ------------------------------------------------------
      if Stopped then
         return;
      end if;

      --  Schedule tick to be called again in 50 milliseconds.
      --------------------------------------------------------
      Tcl.Tk.Ada.After (50, "tick");

      --  Increment the timer value
      ------------------------------
      Time_Value := Time_Value + 0.05;

      --  Update the timer display.
      ----------------------------
      Update;

   end Tick;

   function Tick_Command
     (ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.int;
      Argv       : in CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, Tick_Command);

   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will increment the timer.
   -------------------------------------------------------------
   function Tick_Command
     (ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.int;
      Argv       : in CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData, Interp, Argc, Argv);
   begin --  Tick_Command
      Tick;
      return Tcl.TCL_OK;
   end Tick_Command;

   function Start_Command
     (ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.int;
      Argv       : in CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, Start_Command);

   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will start the timer if it is currently stopped.  Also,
   --  change the Stop button (currently labeled "Reset") to display "Stop."
   -------------------------------------------------------------------------
   function Start_Command
     (ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.int;
      Argv       : in CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData, Interp, Argc, Argv);
   begin --  Start_Command
      if Stopped then
         Stopped := False;
         Tcl.Tk.Ada.configure (Stop_Button, "-text Stop -command Stop");
         Tick;
         Tcl.Tk.Ada.Set_Trace (False);
      end if;
      return Tcl.TCL_OK;
   end Start_Command;

   function Stop_Command
     (ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.int;
      Argv       : in CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, Stop_Command);

   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will stop incrementing the timer.  Also, relabel the
   --  Stop button to be a Reset button.
   --------------------------------------------------------------
   function Stop_Command
     (ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.int;
      Argv       : in CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData, Interp, Argc, Argv);
   begin --  Stop_Command
      Tcl.Tk.Ada.Set_Trace (True);
      Stopped := True;
      Tcl.Tk.Ada.configure (Stop_Button, "-text Reset -command Reset");
      return Tcl.TCL_OK;
   end Stop_Command;

   --  Reset the timer's value to 0.0 and update the display.
   ----------------------------------------------------------
   procedure Reset;
   procedure Reset is
   begin --  Reset
      Time_Value := 0.0;
      Stopped    := True;
      Update;
   end Reset;

   function Reset_Command
     (ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.int;
      Argv       : in CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, Reset_Command);

   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will reset the timer to 0.0 and update the display.
   -------------------------------------------------------------
   function Reset_Command
     (ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.int;
      Argv       : in CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData, Interp, Argc, Argv);
   begin --  Reset_Command
      Reset;
      return Tcl.TCL_OK;
   end Reset_Command;

begin --  Timer

   Tcl.Tk.Ada.Set_Trace (True);

   --  Tcl needs to know the path name of the executable
   --  otherwise Tcl.Tcl_Init below will fail.
   -----------------------------------------------------
   Tcl.Tcl_FindExecutable
     (Interfaces.C.Strings.To_Chars_Ptr (Executable'Unchecked_Access));

   --  Create one Tcl interpreter
   ------------------------------
   Interp := Tcl.Tcl_CreateInterp;

   --  Initialize Tcl
   ------------------
   if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
      Text_IO.Put_Line
        ("Timer: Tcl.Tcl_Init failed: " & Tcl.Ada.Tcl_GetResult (Interp));
      return;
   end if;

   --  Initialize Tk
   -----------------
   if Tcl.Tk.Tk_Init (Interp) = Tcl.TCL_ERROR then
      Text_IO.Put_Line
        ("Timer: Tcl.Tk.Tk_Init failed: " & Tcl.Ada.Tcl_GetResult (Interp));
      return;
   end if;

   --  Create several new Tcl commands to call Ada subprograms.
   ------------------------------------------------------------
   Command :=
      CreateCommands.Tcl_CreateCommand
        (Interp,
         "tick",
         Tick_Command'Access,
         0,
         null);
   Command :=
      CreateCommands.Tcl_CreateCommand
        (Interp,
         "Start",
         Start_Command'Access,
         0,
         null);
   Command :=
      CreateCommands.Tcl_CreateCommand
        (Interp,
         "Stop",
         Stop_Command'Access,
         0,
         null);
   Command :=
      CreateCommands.Tcl_CreateCommand
        (Interp,
         "Reset",
         Reset_Command'Access,
         0,
         null);

   --  Set the Tk context so that we may use shortcut Tk calls
   --  that require reference to the interpreter.
   -----------------------------------------------------------
   Tcl.Tk.Ada.Set_Context (Interp);

   --  Create and pack the counter text widget
   -------------------------------------------
   Counter :=
      Tcl.Tk.Ada.Create (".counter", "-text 0.00 -relief raised -width 10");
   Tcl.Tk.Ada.Pack (Counter, "-side bottom -fill both");

   --  Create and pack the Start button
   ------------------------------------
   Start_Button := Tcl.Tk.Ada.Create (".start", "-text Start -command Start");
   Tcl.Tk.Ada.Pack (Start_Button, "-side left -fill both -expand yes");

   --  Create and pack the Stop button
   -----------------------------------
   Stop_Button := Tcl.Tk.Ada.Create (".stop", "-text Reset -command Reset");
   Tcl.Tk.Ada.Pack (Stop_Button, "-side left -fill both -expand yes");

   --  Bind ^C and ^Q keys to exit
   -------------------------------
   Tcl.Tk.Ada.Bind_To_Main_Window (Interp, "<Control-c>", "{destroy .;exit}");
   Tcl.Tk.Ada.Bind_To_Main_Window (Interp, "<Control-q>", "{destroy .;exit}");

   --  Reset timer value to 0.0
   ----------------------------
   Reset;

   --  Loop inside Tk, waiting for commands to execute.
   --  When there are no windows left, Tcl.Tk.Tk_MainLoop
   --  returns and we exit.
   ------------------------------------------------------
   Tcl.Tk.Tk_MainLoop;

end Timer;
