--  Copyright 2014 Simon Wright <simon@pushface.org>
--
--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  This program (with watching.tcl) demonstrates the use of
--  Tcl_SetVar[2]() to get an Ada-domain value back into the Tcl
--  domain without the use of polling.

with CArgv;
with Interfaces.C.Strings;
with Tcl.Ada;

procedure Watching is

   --  Argc and Argv include the command name
   Argc : Interfaces.C.int;
   Argv : CArgv.Chars_Ptr_Ptr;

   function Init (Interp : in Tcl.Tcl_Interp) return Interfaces.C.int;
   pragma Convention (C, Init);

   --  Handy wrapper for C.Strings.Free, so it can be used to free
   --  results.
   procedure Freeproc (BlockPtr : in Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Freeproc);

   function Square
     (Client_Data : in Integer;
      Interp : in Tcl.Tcl_Interp;
      Argc : in Interfaces.C.int;
      Argv : in CArgv.Chars_Ptr_Ptr) return Interfaces.C.int;
   pragma Convention (C, Square);

   function Init (Interp : in Tcl.Tcl_Interp) return Interfaces.C.int is

      package CreateCommands is new Tcl.Ada.Generic_Command (Integer);
      Command : Tcl.Tcl_Command;
      pragma Unreferenced (Command);

      use type Interfaces.C.int;

   begin

      if Tcl.Tcl_Init (Interp) = Tcl.TCL_ERROR then
         return Tcl.TCL_ERROR;
      end if;

      Command := CreateCommands.Tcl_CreateCommand
        (Interp,
         "square",
         Square'Unrestricted_Access,
         0,
         null);

      return Tcl.TCL_OK;

   end Init;

   procedure Freeproc (BlockPtr : in Interfaces.C.Strings.chars_ptr)
   is
      Tmp : Interfaces.C.Strings.chars_ptr := BlockPtr;
   begin
      Interfaces.C.Strings.Free (Tmp);
   end Freeproc;

   function Square
     (Client_Data : in Integer;
      Interp : in Tcl.Tcl_Interp;
      Argc : in Interfaces.C.int;
      Argv : in CArgv.Chars_Ptr_Ptr) return Interfaces.C.int
   is
      pragma Unreferenced (Client_Data);
      Input : Integer;
      Squared : Integer;
      use type Interfaces.C.int;
   begin
      pragma Assert (Argc = 2, "'square' requires one integer argument");

      Input := Integer'Value
        (Interfaces.C.Strings.Value
          (CArgv.Argv_Pointer.Value (Argv) (1)));
      Squared := Input * Input;

      Tcl.Tcl_SetResult
        (Interp,
         Interfaces.C.Strings.New_String (Integer'Image (Squared)),
         Freeproc'Unrestricted_Access);

      declare
         Result : constant String :=
            Tcl.Ada.Tcl_SetVar2
              (Interp,
               "tellback",
               "42",
               Integer'Image (Squared),
               Tcl.TCL_GLOBAL_ONLY);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      return Tcl.TCL_OK;
   end Square;

begin

   --  Get command-line arguments and put them into C-style "argv",
   --  as required by Tcl_Main.
   CArgv.Create (Argc, Argv);

   --  Start Tcl (and never return!)
   Tcl.Tcl_Main (Argc, Argv, Init'Unrestricted_Access);

end Watching;
