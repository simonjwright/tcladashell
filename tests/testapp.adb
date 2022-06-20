--------------------------------------------------------------------
--
-- testapp.adb --
--
--  Copyright (c) 1995-1997 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with Ada.Strings.Fixed;
with CArgv;
with CHelper;
with Interfaces.C.Strings;
with Tcl.Ada;
with Unchecked_Deallocation;

package body TestApp is

   use Tcl.Ada;

   function "+" (Left, Right : C.int) return C.int renames C. "+";
   function "-" (Left, Right : C.int) return C.int renames C. "-";
   function "=" (Left, Right : C.int) return Boolean renames C. "=";

   package CreateCommands is new Generic_Command (Integer);

   type Counter_Rec is record
      Value : Integer := 0;
   end record;
   pragma Convention (C, Counter_Rec);
   type Counter_Ptr is access all Counter_Rec;
   pragma Convention (C, Counter_Ptr);

   procedure Free_Counter is new Unchecked_Deallocation (
      Object => Counter_Rec,
      Name => Counter_Ptr);

   package CounterCommands is new Generic_Command (Counter_Ptr);

   Counter_Id : Natural := 0;

   --     procedure Put_Argv
   --       (Msg  : in String;
   --        Argc : in C.int;
   --        Argv : in CArgv.Chars_Ptr_Ptr)
   --     is
   --     begin --  Put_Argv
   --        Text_IO.Put_Line
   --          (Msg &
   --           ": c=" &
   --           C.int'Image (Argc) &
   --           "  v=" &
   --           CHelper.Value (Tcl_Merge (Argc, Argv)));
   --     end Put_Argv;

   --     procedure Put_Vector (Msg : in String; Vec : in CArgv.Vector) is
   --     begin --  Put_Vector
   --        Text_IO.Put_Line (Msg & ":");
   --        for I in  Vec'First .. Vec'Last loop
   --           Text_IO.Put_Line
   --             ("  " & C.int'Image (I) & " = " & CHelper.Value (Vec (I)));
   --        end loop;
   --     end Put_Vector;

   function EqCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, EqCmd);

   function EqCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData);
      --  From Section 30.2 of _Tcl_and_the_Tk_Toolkit_ by John Ousterhout.
      --  Compares two arguments for equality using string comparision.
      --  Returns 1 if equal, 0 if not.
      Vec : CArgv.Vector (0 .. Argc);
   begin --  EqCmd
      if Argc /= 3 then
         Tcl_SetResult (Interp, "wrong # args");
         return TCL_ERROR;
      end if;
      Vec := CArgv.Argv_Pointer.Value (Argv);
      if CHelper. "=" (Vec (1), Vec (2)) then
         Tcl_SetResult (Interp, "1");
      else
         Tcl_SetResult (Interp, "0");
      end if;
      return TCL_OK;
   end EqCmd;

   function ConcatCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, ConcatCmd);

   function ConcatCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData);
      --  From Section 30.4 of _Tcl_and_the_Tk_Toolkit_ by John Ousterhout.
      --  This is a simple implementation of the concat command using
      --  AppendResult.
      Vec : CArgv.Vector (0 .. Argc);
   begin --  ConcatCmd
      if Argc = 1 then
         return TCL_OK;
      end if;
      Vec := CArgv.Argv_Pointer.Value (Argv);
      --    Put_Argv   ("ConcatCmd", Argc, Argv);
      --    Put_Vector ("ConcatCmd", Vec);
      Tcl_AppendResult (Interp, Vec (1));
      for i in  2 .. Argc - 1 loop
         Tcl_AppendResult (Interp, C.Strings.New_String (" "));
         Tcl_AppendResult (Interp, Vec (i));
      end loop;
      return TCL_OK;
   end ConcatCmd;

   function ListCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, ListCmd);

   function ListCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData);
      --  From Section 30.4 of _Tcl_and_the_Tk_Toolkit_ by John Ousterhout.
      --  This is a simple implementation of the list command using
      --  AppendElement.
      Vec : CArgv.Vector (0 .. Argc);
   begin --  ListCmd
      Vec := CArgv.Argv_Pointer.Value (Argv);
      for i in  1 .. Argc - 1 loop
         Tcl_AppendElement (Interp, Vec (i));
      end loop;
      return TCL_OK;
   end ListCmd;

   function "&" (Left : String; Right : Integer) return String;
   function "&" (Left : String; Right : Integer) return String is
   begin --  "&"
      return Left &
             Ada.Strings.Fixed.Trim (Integer'Image (Right), Ada.Strings.Left);
   end "&";

   procedure DeleteCounter (Counter : Counter_Ptr);
   pragma Convention (C, DeleteCounter);

   procedure DeleteCounter (Counter : Counter_Ptr) is
      --
      Local_Counter : Counter_Ptr := Counter;
   begin --  DeleteCounter
      Free_Counter (Local_Counter);
   end DeleteCounter;

   function ObjectCmd
     (Counter : Counter_Ptr;
      Interp  : Tcl_Interp;
      Argc    : C.int;
      Argv    : CArgv.Chars_Ptr_Ptr)
      return    C.int;
   pragma Convention (C, ObjectCmd);

   function ObjectCmd
     (Counter : Counter_Ptr;
      Interp  : Tcl_Interp;
      Argc    : C.int;
      Argv    : CArgv.Chars_Ptr_Ptr)
      return    C.int
   is
      --  From Section 30.5 of _Tcl_and_the_Tk_Toolkit_ by John Ousterhout.
      --  This is an Ada implementation of the counter object command.  It
      --  demonstrates the use of Client Data and deletion callbacks.
      Vec : CArgv.Vector (0 .. Argc);
   begin --  ObjectCmd
      if Argc /= 2 then
         Tcl_SetResult (Interp, "wrong # args");
         return TCL_ERROR;
      end if;
      Vec := CArgv.Argv_Pointer.Value (Argv);
      declare
         Command : constant String := CHelper.Value (Vec (1));
      begin
         if Command = "get" then
            Tcl_SetResult (Interp, Integer'Image (Counter.Value));
         elsif Command = "next" then
            Counter.Value := Counter.Value + 1;
         else
            Tcl_AppendResult
              (Interp,
               C.Strings.New_String
                  ("bad counter command """ &
                   Command &
                   """: should be get or next"));
            return TCL_ERROR;
         end if;
      end;
      return TCL_OK;
   end ObjectCmd;

   function CounterCmd
     (ClientData : Counter_Ptr;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, CounterCmd);

   function CounterCmd
     (ClientData : Counter_Ptr;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData, Argv);
      --  From Section 30.5 of _Tcl_and_the_Tk_Toolkit_ by John Ousterhout.
      --  This is an Ada implementation of the Object command.  It demonstrates
      --  the use of Client Data and deletion callbacks.
      Counter : Counter_Ptr;
      Command : Tcl_Command;
      pragma Unreferenced (Command);
   begin --  CounterCmd
      if Argc /= 1 then
         Tcl_SetResult (Interp, "wrong # args");
         return TCL_ERROR;
      end if;
      Counter := new Counter_Rec;
      Tcl_SetResult (Interp, "ctr" & Counter_Id);
      Counter_Id := Counter_Id + 1;
      Command    :=
         CounterCommands.Tcl_CreateCommand
           (Interp,
            Tcl.Tcl_GetStringResult (Interp),
            ObjectCmd'Access,
            Counter,
            DeleteCounter'Access);
      return TCL_OK;
   end CounterCmd;

   function SumCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, SumCmd);

   function SumCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData);
      --  From Section 32.1 of _Tcl_and_the_Tk_Toolkit_ by John Ousterhout.
      --  Adds its two integer arguments.
      Vec         : CArgv.Vector (0 .. Argc);
      Left, Right : aliased C.int;
   begin --  SumCmd
      if Argc /= 3 then
         Tcl_SetResult (Interp, "wrong # args");
         return TCL_ERROR;
      end if;
      Vec := CArgv.Argv_Pointer.Value (Argv);
      begin
         Left := C.int'Value (CHelper.Value (Vec (1)));
      exception
         when others =>
            if Tcl_GetInt (Interp, Vec (1), Left'Unchecked_Access) /=
               TCL_OK
            then
               return TCL_ERROR;
            end if;
      end;
      begin
         Right := C.int'Value (CHelper.Value (Vec (2)));
      exception
         when others =>
            if Tcl_GetInt (Interp, Vec (2), Right'Unchecked_Access) /=
               TCL_OK
            then
               return TCL_ERROR;
            end if;
      end;
      Tcl_SetResult (Interp, C.int'Image (Left + Right));
      return TCL_OK;
   end SumCmd;

   function ExprCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int;
   pragma Convention (C, ExprCmd);

   function ExprCmd
     (ClientData : Integer;
      Interp     : Tcl_Interp;
      Argc       : C.int;
      Argv       : CArgv.Chars_Ptr_Ptr)
      return       C.int
   is
      pragma Unreferenced (ClientData);
      --  From Section 32.2 of _Tcl_and_the_Tk_Toolkit_ by John Ousterhout.
      --  This is a simple implementation of the expr command using ExprString.
      Vec : CArgv.Vector (0 .. Argc);
   begin --  ExprCmd
      if Argc /= 2 then
         Tcl_SetResult (Interp, "wrong # args");
         return TCL_ERROR;
      end if;
      Vec := CArgv.Argv_Pointer.Value (Argv);
      return Tcl_ExprString (Interp, Vec (1));
   end ExprCmd;

   function Init (Interp : Tcl_Interp) return C.int is
      Command : Tcl_Command;
      pragma Unreferenced (Command);
   begin --  Init

      if Tcl_Init (Interp) = TCL_ERROR then
         return TCL_ERROR;
      end if;

      --  Call the init procedures for included packages.  Each call should
      --  look like this:
      --
      --  if Mod.Init(interp) = TCL_ERROR then
      --    return TCL_ERROR;
      --  end if;
      --
      --  where "Mod" is the name of the module.

      --  Call CreateCommand for application-specific commands, if
      --  they weren't already created by the init procedures called above.

      Command :=
         CreateCommands.Tcl_CreateCommand
           (Interp,
            "eq",
            EqCmd'Access,
            0,
            null);

      Command :=
         CreateCommands.Tcl_CreateCommand
           (Interp,
            "concat",
            ConcatCmd'Access,
            0,
            null);

      Command :=
         CreateCommands.Tcl_CreateCommand
           (Interp,
            "list",
            ListCmd'Access,
            0,
            null);

      Command :=
         CounterCommands.Tcl_CreateCommand
           (Interp,
            "counter",
            CounterCmd'Access,
            null,
            null);

      Command :=
         CreateCommands.Tcl_CreateCommand
           (Interp,
            "sum",
            SumCmd'Access,
            0,
            null);

      Command :=
         CreateCommands.Tcl_CreateCommand
           (Interp,
            "simple_expr",
            ExprCmd'Access,
            0,
            null);

      --  Specify a user-specific startup file to invoke if the application
      --  is run interactively.  Typically the startup file is "~/.apprc"
      --  where "app" is the name of the application.  If this line is deleted
      --  then no user-specific startup file will be run under any conditions.

      declare
      begin
         return Tcl.TCL_OK;
      exception
         when others =>
            return Tcl.TCL_ERROR;
      end;

   end Init;

end TestApp;
