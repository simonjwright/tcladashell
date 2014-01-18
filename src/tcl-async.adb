--------------------------------------------------------------------
--
--  tcl-async.adb -- This package supports asynchronous setting of Tcl
--  variables or array elements from Ada.
--
--  Copyright (c) Simon Wright <simon@pushface.org>
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
--          Free Software Foundation
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.Threads;
with Interfaces.C.Strings;
with System;

package body Tcl.Async is

   --  This record holds a proposed update until the Tcl event loop is
   --  ready for it to be processed.
   type Update is record
      Variable : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Index    : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Value    : Standard.Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Proposed updates are held in a Vector.
   package Update_Vectors is new Standard.Ada.Containers.Vectors (Positive,
                                                                  Update);

   --  Updates are proposed by Ada code which may be running in a
   --  separate thread from that used by the Tcl event loop.
   protected Update_Manager is

      --  Propose an update.
      procedure Set (New_Update : Update);

      --  Retrieve an update; blocks if there are none waiting.
      entry Get (New_Update : out Update);

   private
      --  The queue of proposed updates.
      Updates : Update_Vectors.Vector;
   end Update_Manager;

   protected body Update_Manager is

      procedure Set (New_Update : Update)
      is
      begin
         Updates.Append (New_Update);
      end Set;

      entry Get (New_Update : out Update) when Integer (Updates.Length) /= 0
      is
      begin
         New_Update := Updates.First_Element;
         Updates.Delete_First;
      end Get;

   end Update_Manager;

   --  Stores a token indicating the registered handler (which will be
   --  called when the Tcl event loop has been informed, by
   --  Tcl_AsyncMark(), that there is data to be procssed).
   Async_Handler : Tcl_AsyncHandler;

   --  Stores the Tcl Interpreter to be used if the Async Handler
   --  isn't given one.
   Default_Interpreter : Tcl_Interp;

   --  The handler to be registered.
   function Async_Proc (Dummy  : ClientData;
                        Interp : Tcl_Interp;
                        Code   : Interfaces.C.int)
                       return Interfaces.C.int;
   pragma Convention (C, Async_Proc);

   procedure Register (Interp : Tcl_Interp)
   is
      pragma Assert (Async_Handler = null);
   begin
      --  We don't use any client data.
      Async_Handler := Tcl_AsyncCreate (proc => Async_Proc'Access,
                                        data => Null_ClientData);
      Default_Interpreter := Interp;
   end Register;

   procedure Set (Tcl_Variable : String; Value : String)
   is
      pragma Assert (Async_Handler /= null);
      use Standard.Ada.Strings.Unbounded;
   begin
      Update_Manager.Set ((Variable => To_Unbounded_String (Tcl_Variable),
                           Index    => Null_Unbounded_String,
                           Value    => To_Unbounded_String (Value)));
      --  Tell Tcl there's work to be done.
      Tcl_AsyncMark (Async_Handler);
   end Set;

   procedure Set (Tcl_Array : String; Index : String; Value : String)
   is
      pragma Assert (Async_Handler /= null);
      use Standard.Ada.Strings.Unbounded;
   begin
      Update_Manager.Set ((Variable => To_Unbounded_String (Tcl_Array),
                           Index    => To_Unbounded_String (Index),
                           Value    => To_Unbounded_String (Value)));
      --  Tell Tcl there's work to be done.
      Tcl_AsyncMark (Async_Handler);
   end Set;

   --  Called in the Tcl event loop's context to action the proposed
   --  updates.
   function Async_Proc
     (Dummy  : ClientData;
      Interp : Tcl_Interp;
      Code   : Interfaces.C.int)
     return Interfaces.C.int
   is
      pragma Unreferenced (Dummy);
      Next_Update : Update;
      Interpreter : Tcl_Interp := Interp;
      use Standard.Ada.Strings.Unbounded;
      use Interfaces.C, Interfaces.C.Strings;
   begin
      --  We may not be in a thread already registered with the Ada
      --  RTS; make it so.
      declare
         Ada_Id : System.Address;
         pragma Unreferenced (Ada_Id);
      begin
         Ada_Id := GNAT.Threads.Register_Thread;
      end;

      if Interpreter = null then
         Interpreter := Default_Interpreter;
      end if;

      --  We don't want to rely on there being as many calls of this
      --  procedure as there were Sets.
      loop

         select
            Update_Manager.Get (Next_Update);
         else
            --  There were no outstanding updates.
            exit;
         end select;

         if Length (Next_Update.Index) = 0 then
            --  We're setting a variable.
            declare
               C_Variable : aliased Interfaces.C.char_array
                 := To_C (To_String (Next_Update.Variable));
               C_Value : aliased Interfaces.C.char_array
                 := To_C (To_String (Next_Update.Value));
               Result : Interfaces.C.Strings.chars_ptr;
               pragma Unreferenced (Result);
            begin
               Result :=
                 Tcl_SetVar
                   (Interpreter,
                    To_Chars_Ptr (C_Variable'Unchecked_Access,
                                  Nul_Check => True),
                    To_Chars_Ptr (C_Value'Unchecked_Access,
                                  Nul_Check => True),
                    Tcl.TCL_GLOBAL_ONLY);
               --  What to do if Result is null (it failed)?
            end;
         else
            --  We're setting an array element.
            declare
               C_Variable : aliased Interfaces.C.char_array
                 := To_C (To_String (Next_Update.Variable));
               C_Index : aliased Interfaces.C.char_array
                 := To_C (To_String (Next_Update.Index));
               C_Value : aliased Interfaces.C.char_array
                 := To_C (To_String (Next_Update.Value));
               Result : Interfaces.C.Strings.chars_ptr;
               pragma Unreferenced (Result);
            begin
               Result :=
                 Tcl_SetVar2
                   (Interpreter,
                    To_Chars_Ptr (C_Variable'Unchecked_Access,
                                  Nul_Check => True),
                    To_Chars_Ptr (C_Index'Unchecked_Access,
                                  Nul_Check => True),
                    To_Chars_Ptr (C_Value'Unchecked_Access,
                                  Nul_Check => True),
                    Tcl.TCL_GLOBAL_ONLY);
               --  What to do if Result is null (it failed)?
            end;
         end if;
      end loop;

      --  From the man page, "It is almost always a bad idea for an
      --  asynchronous event handler to modify interp->result or
      --  return a code different from its code argument."
      return Code;
   end Async_Proc;

end Tcl.Async;
