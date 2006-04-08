------------------------------------------------
--
-- futurevalue.adb --
--    This program demonstrates how the TASH Ada/Tk interface
--    provides Tk features for use in an Ada program.
--
--    It implements a simple GUI for computing Future Value
--    of a series of fixed monthly investments.
--
-- Copyright (c) 1997 Terry J. Westley
--
-- See the file "license.htm" for information on usage and
-- redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
--
------------------------------------------------

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with CArgv;
with CHelper;
with Interfaces.C.Strings;
with Tcl;
with Tcl.Ada;
with Tcl.Tk;
with Tcl.Tk.Ada;
with Text_IO;

procedure FutureValue is

   package C   renames Interfaces.C;
   package ASU renames Ada.Strings.Unbounded;
   package CreateCommands is new Tcl.Ada.Generic_Command (Integer);

   function "=" (Left, Right : in C.Int) return Boolean renames C."=";

   -- use a decimal type for ease in formatting for display
   --------------------------------------------------------
   type Money is delta 0.01 digits 14;

   Command_Name : String := Ada.Command_Line.Command_Name;
   Executable   : aliased Interfaces.C.Char_Array :=
      Interfaces.C.To_C (Command_Name);
   Interp       : Tcl.Tcl_Interp;
   Command      : Tcl.Tcl_Command;
   Frame        : Tcl.Tk.Ada.Frame;
   Label        : Tcl.Tk.Ada.Label;
   Button       : Tcl.Tk.Ada.Button;
   Amt_Entry    : Tcl.Tk.Ada.EntryWidget;
   Rate_Entry   : Tcl.Tk.Ada.EntryWidget;
   Yrs_Entry    : Tcl.Tk.Ada.EntryWidget;
   Result       : Tcl.Tk.Ada.Label;

   function Compute_Future_Value_Command (
      ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.Int;
      Argv       : in CArgv.Chars_Ptr_Ptr
   ) return C.Int;
   pragma Convention (C, Compute_Future_Value_Command);

   -- Declare a procedure, suitable for creating a Tcl command,
   -- which will compute the Future Value.
   ------------------------------------------------------------
   function Compute_Future_Value_Command (
      ClientData : in Integer;
      Interp     : in Tcl.Tcl_Interp;
      Argc       : in C.Int;
      Argv       : in CArgv.Chars_Ptr_Ptr
   ) return C.Int is

      Amount       : Money;
      Future_Value : Money;
      Annual_Rate  : Float;
      Rate         : Float;
      Years        : Integer;
      Months       : Integer;

   begin -- Compute_Future_Value_Command

      -- get the monthly investment amount from its text entry field,
      -- evaluate it in case it is an expression,
      -- and make sure it is not less than zero
      ---------------------------------------------------------------
      Amount := Money (
         Tcl.Ada.Tcl_ExprDouble (Interp, Tcl.Tk.Ada.Get (Amt_Entry)));
      if Amount < 0.0 then
         return Tcl.TCL_OK;
      end if;

      -- get the annual interest rate from its text entry field
      -- evaluate it in case it is an expression,
      -- and make sure it is not less than zero
      ---------------------------------------------------------
      Annual_Rate := Float (
         Tcl.Ada.Tcl_ExprDouble (Interp, Tcl.Tk.Ada.Get (Rate_Entry)));
      if Annual_Rate < 0.0 then
         return Tcl.TCL_OK;
      end if;

      -- get the number of years from its text entry field
      -- evaluate it in case it is an expression,
      -- and make sure it is not less than zero
      ----------------------------------------------------
      Years := Integer (
         Tcl.Ada.Tcl_ExprLong (Interp, Tcl.Tk.Ada.Get (Yrs_Entry)));
      if Years < 0 then
         return Tcl.TCL_OK;
      end if;

      -- compute the monthly interest rate
      ------------------------------------
      Rate := Annual_Rate / 1200.0;

      -- compute the number of months
      -------------------------------
      Months := Years * 12;

      -- compute future value with the formula:
      --                              n
      --                       (1 + i)  - 1
      -- Future Value = (M) * --------------
      --                             i
      --
      -- where M = Monthly savings
      --       i = interest per month
      --       n = number of months
      -----------------------------------------
      Future_Value :=
         Money (Float (Amount) * ((1.0 + Rate)**Months - 1.0)/Rate);

      -- put the future value into the result label
      ---------------------------------------------
      Tcl.Tk.Ada.Configure (Result, "-text " & Money'image (Future_Value));

      -- return TCL_OK to keep Tcl happy
      ----------------------------------
      return Tcl.TCL_OK;

   exception
      when others => return Tcl.TCL_OK;

   end Compute_Future_Value_Command;

begin -- FutureValue

   Tcl.Tk.Ada.Set_Trace (False);

   -- Tcl needs to know the path name of the executable
   -- otherwise Tcl.Tcl_Init below will fail.
   ----------------------------------------------------
   Tcl.Tcl_FindExecutable (Interfaces.C.Strings.To_Chars_Ptr (
      Executable'Unchecked_access));

   -- Create one Tcl interpreter
   -----------------------------
   Interp := Tcl.Tcl_CreateInterp;

   -- Initialize Tcl
   -----------------
   if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
      Text_IO.Put_Line ("FutureValue: Tcl_Init failed: " &
         Tcl.Ada.Tcl_GetResult (Interp));
      return;
   end if;

   -- Initialize Tk
   ----------------
   if Tcl.Tk.Tk_Init (Interp) = Tcl.TCL_ERROR then
      Text_IO.Put_Line ("FutureValue: Tcl.Tk.Tk_Init failed: " &
         Tcl.Ada.Tcl_GetResult (Interp));
      return;
   end if;

   -- Create a new Tcl command to compute future value.
   ----------------------------------------------------
   Command := CreateCommands.Tcl_CreateCommand (
      Interp, "computeFutureValue",
      Compute_Future_Value_Command'access,  0, NULL);

   -- Set the Tk context so that we may use shortcut Tk calls
   -- that do not require reference to the interpreter.
   ----------------------------------------------------------
   Tcl.Tk.Ada.Set_Context (Interp);

   -- Create a frame, label and entry field for savings amount
   -----------------------------------------------------------
   Tcl.Tk.Ada.Create (Frame, ".amt", "-bd 2");
   Tcl.Tk.Ada.Pack   (Frame, "-side top -fill x");
   Tcl.Tk.Ada.Create (Amt_Entry, ".amt.entry", "-width 20 -bg white");
   Tcl.Tk.Ada.Pack   (Amt_Entry, "-side right");
   Tcl.Tk.Ada.Create (Label, ".amt.label", "-text ""Monthly Savings Amount:""");
   Tcl.Tk.Ada.Pack   (Label, "-side right");

   -- Initialize savings amount
   ----------------------------
   Tcl.Ada.Tcl_Eval (Interp, ".amt.entry insert end 100");

   -- Create a frame, label and entry field for interest rate
   ----------------------------------------------------------
   Tcl.Tk.Ada.Create (Frame, ".rate", "-bd 2");
   Tcl.Tk.Ada.Pack   (Frame, "-side top -fill x");
   Tcl.Tk.Ada.Create (Rate_Entry, ".rate.entry", "-width 20 -bg white");
   Tcl.Tk.Ada.Pack   (Rate_Entry, "-side right");
   Tcl.Tk.Ada.Create (Label, ".rate.label", "-text ""Annual Interest Rate:""");
   Tcl.Tk.Ada.Pack   (Label, "-side right");

   -- Initialize interest rate
   ---------------------------
   Tcl.Ada.Tcl_Eval (Interp, ".rate.entry insert end 0.10");

   -- Create a frame, label and entry field for number of years
   ------------------------------------------------------------
   Tcl.Tk.Ada.Create (Frame, ".yrs", "-bd 2");
   Tcl.Tk.Ada.Pack   (Frame, "-side top -fill x");
   Tcl.Tk.Ada.Create (Yrs_Entry, ".yrs.entry", "-width 20 -bg white");
   Tcl.Tk.Ada.Pack   (Yrs_Entry, "-side right");
   Tcl.Tk.Ada.Create (Label, ".yrs.label", "-text ""Number of Years:""");
   Tcl.Tk.Ada.Pack   (Label, "-side right");

   -- Initialize savings amount
   ----------------------------
   Tcl.Ada.Tcl_Eval (Interp, ".yrs.entry insert end 10");

   -- Create a frame, button, and result label for computed result
   ---------------------------------------------------------------
   Tcl.Tk.Ada.Create (Frame, ".fv", "-bd 2");
   Tcl.Tk.Ada.Pack   (Frame, "-side top -fill x");
   Tcl.Tk.Ada.Create (Result, ".fv.result", "-width 20 -relief sunken");
   Tcl.Tk.Ada.Pack   (Result, "-side right");
   Tcl.Tk.Ada.Create (Button, ".fv.button",
      "-text ""Compute Future Value:"" -command computeFutureValue -pady 1");
   Tcl.Tk.Ada.Pack   (Button, "-side right");

   -- Add a window title
   ---------------------
   Tcl.Ada.Tcl_Eval (Interp, "wm title . ""Future Value of Savings""");

   -- Set focus to the first entry field
   -------------------------------------
   Tcl.Ada.Tcl_Eval (Interp, "focus .amt.entry");

   -- bind Return to the button
   ----------------------------
   Tcl.Tk.Ada.Bind (Button, "<Return>", "computeFutureValue");

   -- Loop inside Tk, waiting for commands to execute.
   -- When there are no windows left, Tcl.Tk.Tk_MainLoop
   -- returns and we exit.
   -----------------------------------------------------
   Tcl.Tk.Tk_MainLoop;

end FutureValue;
