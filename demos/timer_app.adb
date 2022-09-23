------------------------------------------------
--
--  timer_app.adb -- This program demonstrates how the TASH Ada/Tk interface
--                    provides Tk features for use in an Ada program.
--
--  Copyright (c) 2017-2022 Simon Wright <simon@pushface.org>
--  Copyright (c) 1997 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
--
--  This program was adapted from the demo program distributed with Tk.
--  It provides a simple stop watch timer facility.
--
------------------------------------------------

--  This package provides the Init function required for the demo
--  program Timer.

with CArgv;
with Tcl.Ada;
with Tcl.Tk;
with Tcl.Tk.Ada;

package body Timer_App is

   use Tcl;

   type Timer_Type is delta 0.01 digits 8 range 0.0 .. 999999.99;

   Counter      : Tcl.Tk.Ada.Label;
   Start_Button : Tcl.Tk.Ada.Button;
   Stop_Button  : Tcl.Tk.Ada.Button;
   Time_Value   : Timer_Type := 0.0;
   Stopped      : Boolean    := True;

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
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int;
   pragma Convention (C, Tick_Command);

   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will increment the timer.
   -------------------------------------------------------------
   function Tick_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int
   is
      pragma Unreferenced (ClientData, Interp, Argc, Argv);
   begin --  Tick_Command
      Tick;
      return Tcl.TCL_OK;
   end Tick_Command;

   function Start_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int;
   pragma Convention (C, Start_Command);

   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will start the timer if it is currently stopped.  Also,
   --  change the Stop button (currently labeled "Reset") to display "Stop."
   -------------------------------------------------------------------------
   function Start_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int
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
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int;
   pragma Convention (C, Stop_Command);

   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will stop incrementing the timer.  Also, relabel the
   --  Stop button to be a Reset button.
   --------------------------------------------------------------
   function Stop_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int
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
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int;
   pragma Convention (C, Reset_Command);

   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will reset the timer to 0.0 and update the display.
   -------------------------------------------------------------
   function Reset_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       Interfaces.C.int
   is
      pragma Unreferenced (ClientData, Interp, Argc, Argv);
   begin --  Reset_Command
      Reset;
      return Tcl.TCL_OK;
   end Reset_Command;

   function Init (Interp : Tcl.Tcl_Interp)
                 return Interfaces.C.int
   is
      package CreateCommands is new Tcl.Ada.Generic_Command (Integer);
      Command      : Tcl.Tcl_Command;
      pragma Unreferenced (Command);
      use type Interfaces.C.int;
   begin
      Tcl.Tk.Ada.Set_Trace (True);

      --  Initialize Tcl
      if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      --  Initialize Tk
      if Tcl.Tk.Tk_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      --  Create several new Tcl commands to call Ada subprograms.
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
      Tcl.Tk.Ada.Set_Context (Interp);

      --  Create and pack the counter text widget
      Counter :=
        Tcl.Tk.Ada.Create (".counter", "-text 0.00 -relief raised -width 10");
      Tcl.Tk.Ada.Pack (Counter, "-side bottom -fill both");

      --  Create and pack the Start button
      Start_Button := Tcl.Tk.Ada.Create (".start",
                                         "-text Start -command Start");
      Tcl.Tk.Ada.Pack (Start_Button, "-side left -fill both -expand yes");

      --  Create and pack the Stop button
   -----------------------------------
      Stop_Button := Tcl.Tk.Ada.Create (".stop", "-text Reset -command Reset");
      Tcl.Tk.Ada.Pack (Stop_Button, "-side left -fill both -expand yes");

      --  Bind ^C and ^Q keys to exit
   -------------------------------
      Tcl.Tk.Ada.Bind_To_Main_Window (Interp,
                                      "<Control-c>",
                                      "{destroy .;exit}");
      Tcl.Tk.Ada.Bind_To_Main_Window (Interp,
                                      "<Control-q>",
                                      "{destroy .;exit}");

      --  Reset timer value to 0.0
   ----------------------------
      Reset;

      return Tcl.TCL_OK;

   end Init;

end Timer_App;
