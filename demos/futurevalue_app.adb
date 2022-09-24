------------------------------------------------
--
-- futurevalue_app.adb --
--    This program demonstrates how the TASH Ada/Tk interface
--    provides Tk features for use in an Ada program.
--
--    It implements a simple GUI for computing Future Value
--    of a series of fixed monthly investments.
--
--  Copyright (c) 2017-2022 Simon Wright <simon@pushface.org>
--  Copyright (c) 1997 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and  a DISCLAIMER OF ALL WARRANTIES.
--
------------------------------------------------

with CArgv;
with Tcl.Ada;
with Tcl.Tk;
with Tcl.Tk.Ada;

package body FutureValue_App is

   --  use a decimal type for ease in formatting for display
   type Money is delta 0.01 digits 14;

   Frame        : Tcl.Tk.Ada.Frame;
   Label        : Tcl.Tk.Ada.Label;
   Button       : Tcl.Tk.Ada.Button;
   Amt_Entry    : Tcl.Tk.Ada.EntryWidget;
   Rate_Entry   : Tcl.Tk.Ada.EntryWidget;
   Yrs_Entry    : Tcl.Tk.Ada.EntryWidget;
   Result       : Tcl.Tk.Ada.Label;

   function Compute_Future_Value_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
     return       Interfaces.C.int;
   pragma Convention (C, Compute_Future_Value_Command);

   --  Declare a procedure, suitable for creating a Tcl command,
   --  which will compute the Future Value.
   -------------------------------------------------------------
   function Compute_Future_Value_Command
     (ClientData : Integer;
      Interp     : Tcl.Tcl_Interp;
      Argc       : Interfaces.C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
     return       Interfaces.C.int
   is
      pragma Unreferenced (ClientData, Argc, Argv);
      Amount       : Money;
      Future_Value : Money;
      Annual_Rate  : Float;
      Rate         : Float;
      Years        : Integer;
      Months       : Integer;

   begin --  Compute_Future_Value_Command

      --  get the monthly investment amount from its text entry field,
      --  evaluate it in case it is an expression,
      --  and make sure it is not less than zero
      ----------------------------------------------------------------
      Amount :=
        Money (Tcl.Ada.Tcl_ExprDouble (Interp, Tcl.Tk.Ada.get (Amt_Entry)));
      if Amount < 0.0 then
         return Tcl.TCL_OK;
      end if;

      --  get the annual interest rate from its text entry field
      --  evaluate it in case it is an expression,
      --  and make sure it is not less than zero
      ----------------------------------------------------------
      Annual_Rate :=
        Float (Tcl.Ada.Tcl_ExprDouble (Interp, Tcl.Tk.Ada.get (Rate_Entry)));
      if Annual_Rate < 0.0 then
         return Tcl.TCL_OK;
      end if;

      --  get the number of years from its text entry field
      --  evaluate it in case it is an expression,
      --  and make sure it is not less than zero
      -----------------------------------------------------
      Years :=
        Integer (Tcl.Ada.Tcl_ExprLong (Interp, Tcl.Tk.Ada.get (Yrs_Entry)));
      if Years < 0 then
         return Tcl.TCL_OK;
      end if;

      --  compute the monthly interest rate
      -------------------------------------
      Rate := Annual_Rate / 1200.0;

      --  compute the number of months
      -------------------------------
      Months := Years * 12;

      --  compute future value with the formula:
      --                              n
      --                       (1 + i)  - 1
      -- Future Value = (M) * --------------
      --                             i
      --
      --  where M = Monthly savings
      --       i = interest per month
      --       n = number of months
      ------------------------------------------
      Future_Value :=
        Money (Float (Amount) * ((1.0 + Rate) ** Months - 1.0) / Rate);

      --  put the future value into the result label
      ----------------------------------------------
      Tcl.Tk.Ada.configure (Result, "-text " & Money'Image (Future_Value));

      --  return TCL_OK to keep Tcl happy
      -----------------------------------
      return Tcl.TCL_OK;

   exception
      when others =>
         return Tcl.TCL_OK;

   end Compute_Future_Value_Command;

   function Init (Interp : Tcl.Tcl_Interp) return Interfaces.C.int is
      package CreateCommands is new Tcl.Ada.Generic_Command (Integer);
      Command      : Tcl.Tcl_Command;
      pragma Unreferenced (Command);
      use type Interfaces.C.int;
   begin

      Tcl.Tk.Ada.Set_Trace (False);

      --  Initialize Tcl
      if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      --  Initialize Tk
      if Tcl.Tk.Tk_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      --  Create a new Tcl command to compute future value.
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Interp,
           "computeFutureValue",
           Compute_Future_Value_Command'Access,
           0,
           null);

      --  Set the Tk context so that we may use shortcut Tk calls that
      --  do not require reference to the interpreter.
      Tcl.Tk.Ada.Set_Context (Interp);

      --  Create a frame, label and entry field for savings amount
      Tcl.Tk.Ada.Create (Frame, ".amt", "-bd 2");
      Tcl.Tk.Ada.Pack (Frame, "-side top -fill x");
      Tcl.Tk.Ada.Create (Amt_Entry, ".amt.entry", "-width 20 -bg white");
      Tcl.Tk.Ada.Pack (Amt_Entry, "-side right");
      Tcl.Tk.Ada.Create
        (Label,
         ".amt.label",
         "-text ""Monthly Savings Amount:""");
      Tcl.Tk.Ada.Pack (Label, "-side right");

      --  Initialize savings amount
      Tcl.Ada.Tcl_Eval (Interp, ".amt.entry insert end 100");

      --  Create a frame, label and entry field for interest rate
      Tcl.Tk.Ada.Create (Frame, ".rate", "-bd 2");
      Tcl.Tk.Ada.Pack (Frame, "-side top -fill x");
      Tcl.Tk.Ada.Create (Rate_Entry, ".rate.entry", "-width 20 -bg white");
      Tcl.Tk.Ada.Pack (Rate_Entry, "-side right");
      Tcl.Tk.Ada.Create
        (Label,
         ".rate.label",
         "-text ""Annual Interest Rate:""");
      Tcl.Tk.Ada.Pack (Label, "-side right");

      --  Initialize interest rate
      Tcl.Ada.Tcl_Eval (Interp, ".rate.entry insert end 8");

      --  Create a frame, label and entry field for number of years
      Tcl.Tk.Ada.Create (Frame, ".yrs", "-bd 2");
      Tcl.Tk.Ada.Pack (Frame, "-side top -fill x");
      Tcl.Tk.Ada.Create (Yrs_Entry, ".yrs.entry", "-width 20 -bg white");
      Tcl.Tk.Ada.Pack (Yrs_Entry, "-side right");
      Tcl.Tk.Ada.Create (Label, ".yrs.label", "-text ""Number of Years:""");
      Tcl.Tk.Ada.Pack (Label, "-side right");

      --  Initialize savings amount
      Tcl.Ada.Tcl_Eval (Interp, ".yrs.entry insert end 30");

      --  Create a frame, button, and result label for computed result
      Tcl.Tk.Ada.Create (Frame, ".fv", "-bd 2");
      Tcl.Tk.Ada.Pack (Frame, "-side top -fill x");
      Tcl.Tk.Ada.Create (Result, ".fv.result", "-width 20 -relief sunken");
      Tcl.Tk.Ada.Pack (Result, "-side right");
      Tcl.Tk.Ada.Create
        (Button,
         ".fv.button",
         "-text ""Compute Future Value:"""
           & " -command computeFutureValue -pady 1");
      Tcl.Tk.Ada.Pack (Button, "-side right");

      --  Add a window title
      Tcl.Ada.Tcl_Eval (Interp, "wm title . ""Future Value of Savings""");

      --  Set focus to the first entry field
      Tcl.Ada.Tcl_Eval (Interp, "focus .amt.entry");

      --  bind Return to the button
      Tcl.Tk.Ada.Bind (Button, "<Return>", "computeFutureValue");

      return Tcl.TCL_OK;

   end Init;

end FutureValue_App;
