------------------------------------------------------------------
--
--  Unit Name:    Tash.File_IO body
--
--  File Name:    tash-file_io.adb
--
--  Purpose:      Provides file input/output routines.
--
--  Copyright (c) 2000 Terry J. Westley
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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with CHelper;
with Interfaces.C.Strings;
with Ada.IO_Exceptions;
with System;

package body Tash.File_IO is

   function To_File_Mode_List (Mode : in File_Mode_List) return String;
   function Get_Encoding (File : in Tash_File) return Encoding_Type;
   procedure Set_Encoding (File : in Tash_File; Encoding : in Encoding_Type);
   procedure Set_EOF_Char (File : in Tash_File; Char : in String);
   function Get_EOF_In_Char (File : in Tash_File) return String;
   function Get_EOF_Out_Char (File : in Tash_File) return String;
   procedure Set_EOF_Char (File     : in Tash_File;
                           In_Char  : in String;
                           Out_Char : in String);
   procedure Get_Translation_Mode (File     : in Tash_File;
                                   In_Mode  : out Translation_Mode;
                                   Out_Mode : out Translation_Mode);
   procedure Set_Translation_Mode (File     : in Tash_File;
                                   In_Mode  : in Translation_Mode;
                                   Out_Mode : in Translation_Mode);
   pragma Unreferenced (Get_Encoding,
                        Set_Encoding,
                        Get_EOF_In_Char,
                        Get_EOF_Out_Char,
                        Set_EOF_Char,
                        Get_Translation_Mode,
                        Set_Translation_Mode); -- XXX what are they for, then?

   use type Interfaces.C.int;

   function Tcl_OpenObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_OpenObjCmd, "Tcl_OpenObjCmd");

   function Tcl_CloseObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_CloseObjCmd, "Tcl_CloseObjCmd");

   function Tcl_FlushObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_FlushObjCmd, "Tcl_FlushObjCmd");

   function Tcl_PidObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_PidObjCmd, "Tcl_PidObjCmd");

   function Tcl_GetsObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_GetsObjCmd, "Tcl_GetsObjCmd");

   function Tcl_ReadObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_ReadObjCmd, "Tcl_ReadObjCmd");

   function Tcl_PutsObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_PutsObjCmd, "Tcl_PutsObjCmd");

   function Tcl_FconfigureObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_FconfigureObjCmd, "Tcl_FconfigureObjCmd");

   function Tcl_EofObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_EofObjCmd, "Tcl_EofObjCmd");

   --   procedure Put_Objv (Prefix : in String; Objv : in Tcl.Tcl_Obj_Array) is
   --   begin -- Put_Objv
   --      Ada.Text_IO.Put (Prefix);
   --      for I in Objv'Range loop
   --         Ada.Text_IO.Put (" " & Tash.Image (Objv(I)));
   --      end loop;
   --      Ada.Text_IO.New_Line;
   --   end Put_Objv;

   function To_File_Mode_List (Mode : in File_Mode_List) return String is
      --
      Mode_String : Ada.Strings.Unbounded.Unbounded_String;
   begin --  To_File_Mode_List
      for I in  Mode'Range loop
         Ada.Strings.Unbounded.Append
           (Mode_String,
            " " & File_Mode'Image (Mode ((I))));
      end loop;
      return Ada.Strings.Unbounded.To_String (Mode_String);
   end To_File_Mode_List;

   function To_String (File : in Tash_File) return String is
   begin --  To_String
      if Is_Null (File) then
         return "";
      else
         return CHelper.Value (Tcl.Tcl_GetString (File.Obj));
      end if;
   end To_String;

   procedure Open
     (File : in out Tash_File;
      Name : in String;
      Mode : in File_Mode_List := Read;
      Perm : in Natural        := 8#0666#)
   is

      Objc   : constant Interfaces.C.int := 4;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Open

      --  Use Tcl_OpenObjCmd to open a file
      ------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("open");
      Objv (2) := Tash.To_Tcl_Obj (Name);
      Objv (3) := Tash.To_Tcl_Obj (To_File_Mode_List (Mode));
      Objv (4) := Tash.To_Tcl_Obj (Perm);
      --  Put_Objv ("TFIO.Open:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_OpenObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      declare
         Result_String  : constant String                 :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         Result_CString : aliased Interfaces.C.char_array :=
            Interfaces.C.To_C (Result_String);
         New_Obj        : Tcl.Tcl_Obj;
      begin
         --  check for errors
         -------------------
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;

         --  set value of the returned Tash_File object to
         --  the result string, that is, the channel id
         ------------------------------------------------
         New_Obj :=
            Tcl.Tcl_NewStringObj
              (Interfaces.C.Strings.To_Chars_Ptr
                  (Result_CString'Unchecked_Access),
               Interfaces.C.int (Result_String'Length));
         Tcl.Tcl_IncrRefCount (New_Obj);
         File := (Ada.Finalization.Controlled with Obj => New_Obj);
      end;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Open;

   procedure Close (File : in out Tash_File) is

      Objc   : constant Interfaces.C.int := 2;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Close

      --  Use Tcl_CloseObjCmd to close a file
      ------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("close");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      --  Put_Objv ("TFIO.Close:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_CloseObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Check for errors
      -------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
      end;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Close;

   function End_Of_File (File : in Tash_File) return Boolean is
      Objc   : constant Interfaces.C.int := 2;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  End_Of_File

      --  Use Tcl_EofObjCmd to determine whether
      --  a file is at end of file or not
      ------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("eof");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_EofObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and return it.
      ---------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Result_String = "1";
      end;

   end End_Of_File;

   procedure Flush (File : in out Tash_File) is

      Objc   : constant Interfaces.C.int := 2;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Flush

      --  Use Tcl_FlushObjCmd to flush a file
      ------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("flush");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      --  Put_Objv ("TFIO.Flush:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FlushObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Check for errors
      -------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
      end;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Flush;

   function Pid (File : in Tash_File) return Tash.Lists.Tash_List is
      Objc     : constant Interfaces.C.int := 2;
      Objv     : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result   : Interfaces.C.int;
      Interp   : Tcl.Tcl_Interp;
      Pid_List : Tash.Lists.Tash_List;
      pragma Unreferenced (Result);  --  XXX why not?

   begin --  Pid

      --  Use Tcl_PidObjCmd to get the process ids of a process pipeline
      -----------------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("pid");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_PidObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter, convert to a
      --  TASH list and return it.
      ---------------------------------------------
      Tash.Tash_Object (Pid_List).Obj :=
         Tcl.Tcl_DuplicateObj (Tcl.Tcl_GetObjResult (Interp));
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);
      Tcl.Tcl_IncrRefCount (Tash.Tash_Object (Pid_List).Obj);
      return Pid_List;

   end Pid;

   procedure New_Line (File : in Tash_File; Count : in Positive := 1) is
   begin --  New_Line
      for I in  1 .. Count loop
         Put_Line (File, "");
      end loop;
   end New_Line;

   procedure Get (File : in Tash_File; Item : out String) is

      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get

      if End_Of_File (File) then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      --  Use Tcl_ReadObjCmd to read a file
      ------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("read");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj (Item'Length);
      --  Put_Objv ("TFIO.Get:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_ReadObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get characters read from the file and check for errors
      ---------------------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         Item (Item'First .. Item'First + Result_String'Length - 1) :=
           Result_String;
      end;

   end Get;

   procedure Get_Line
     (File : in Tash_File;
      Item : out String;
      Last : out Natural)
   is

      Objc   : constant Interfaces.C.int := 2;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Line

      if End_Of_File (File) then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      --  Use Tcl_GetsObjCmd to read a file
      ------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("gets");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      --  Put_Objv ("TFIO.Get_Line:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_GetsObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Check for errors
      -------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         if Result_String'Length <= Item'Length then
            Last                      := Item'First +
                                         Result_String'Length -
                                         1;
            Item (Item'First .. Last) := Result_String;
         else
            Last := Item'Length;
            Item := Result_String (1 .. Last);
         end if;
      end;

   end Get_Line;

   procedure Put (File : in Tash_File; Item : in String) is

      Objc   : constant Interfaces.C.int := 4;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Put

      --  Use Tcl_PutsObjCmd to write to a file
      ----------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("puts");
      Objv (2) := Tash.To_Tcl_Obj ("-nonewline");
      Objv (3) := Tash.To_Tcl_Obj (To_String (File));
      Objv (4) := Tash.To_Tcl_Obj (Item);
      --  Put_Objv ("TFIO.Put:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_PutsObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Check for errors
      -------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
      end;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Put;

   procedure Put_Line (File : in Tash_File; Item : in String) is

      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Put_Line

      --  Use Tcl_PutsObjCmd to write to a file
      ----------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("puts");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj (Item);
      --  Put_Objv ("TFIO.Put_Line:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_PutsObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Check for errors
      -------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
      end;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Put_Line;

   function Get_Blocking_Mode (File : in Tash_File) return Boolean is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Blocking_Mode

      --  Use Tcl_FconfigureObjCmd to determine whether
      --  a file is in nonblocking mode
      ------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-blocking");
      --  Put_Objv ("TFIO.GBM:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and return it.
      ---------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Result_String = "1";
      end;

   end Get_Blocking_Mode;

   function Get_Buffering_Mode (File : in Tash_File) return Buffering_Mode is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Buffering_Mode

      --  Use Tcl_FconfigureObjCmd to get buffering mode of a file
      -----------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-buffering");
      --  Put_Objv ("TFIO.GBM:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and return it.
      ---------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Buffering_Mode'Value (Result_String);
      end;

   end Get_Buffering_Mode;

   procedure Set_Buffering_Mode
     (File : in Tash_File;
      Mode : in Buffering_Mode)
   is

      Objc   : constant Interfaces.C.int := 4;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Set_Buffering_Mode

      --  Use Tcl_FconfigureObjCmd to set buffering mode of a file
      -----------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-buffering");
      Objv (4) :=
         Tash.To_Tcl_Obj
           (Ada.Characters.Handling.To_Lower (Buffering_Mode'Image (Mode)));
      --  Put_Objv ("TFIO.SBM:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and check for error.
      ---------------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
      end;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_Buffering_Mode;

   function Get_Buffer_Size (File : in Tash_File) return Buffer_Size is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Buffer_Size

      --  Use Tcl_FconfigureObjCmd to get buffer size of a file
      --------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-buffersize");
      --  Put_Objv ("TFIO.GBS:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and return it.
      ---------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Buffer_Size'Value (Result_String);
      end;

   end Get_Buffer_Size;

   procedure Set_Buffer_Size (File : in Tash_File; Size : in Buffer_Size) is

      Objc   : constant Interfaces.C.int := 4;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Set_Buffer_Size

      --  Use Tcl_FconfigureObjCmd to set buffering mode of a file
      -----------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-buffersize");
      Objv (4) := Tash.To_Tcl_Obj (Integer'Image (Size));
      --  Put_Objv ("TFIO.SBS:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and check for error.
      ---------------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
      end;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_Buffer_Size;

   function Get_Encoding (File : in Tash_File) return Encoding_Type is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Encoding

      --  Use Tcl_FconfigureObjCmd to get encoding type of a file
      ----------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-encoding");
      --  Put_Objv ("TFIO.GE:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and return it.
      ---------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         begin
            return Encoding_Type'Value (Result_String);
         exception
            when Constraint_Error =>
               return Normal;
         end;
      end;

   end Get_Encoding;

   procedure Set_Encoding
     (File     : in Tash_File;
      Encoding : in Encoding_Type)
   is
      pragma Unreferenced (File, Encoding);
   begin --  Set_Encoding
      null;
   end Set_Encoding;

   function Get_EOF_Char (File : in Tash_File) return String is
      Objc      : constant Interfaces.C.int := 3;
      Objv      : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result    : Interfaces.C.int;
      Interp    : Tcl.Tcl_Interp;
      InterpObj : Tcl.Tcl_Obj;
      ListElem  : aliased Tcl.Tcl_Obj;

   begin --  Get_EOF_Char

      --  Use Tcl_FconfigureObjCmd to get eof char of a file
      ----------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-eofchar");
      --  Put_Objv ("TFIO.GEC:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Check for errors
      -------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
      end;

      --  Get result from interpreter as a list, then
      --  return the first element of the list.
      ----------------------------------------------
      InterpObj := Tcl.Tcl_GetObjResult (Interp);
      Result    :=
         Tcl.Tcl_ListObjIndex
           (interp    => Interp,
            listPtr   => InterpObj,
            index     => Interfaces.C.int (0),
            objPtrPtr => ListElem'Unchecked_Access);
      Tash_Interp.Assert (Interp, Result, Tash.Tcl_Error'Identity);

      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetString (ListElem));
      begin
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         if Result_String'Length = 0 then
            return Result_String;
         else
            return Result_String (1 .. 1);
         end if;
      end;

   end Get_EOF_Char;

   function Get_EOF_Char (File : in Tash_File) return Character is
      --
      EOF : constant String := Get_EOF_Char (File);
   begin --  Get_EOF_Char
      return EOF (EOF'First);
   end Get_EOF_Char;

   function EOF_Char_Is_Empty_String (File : in Tash_File) return Boolean is
   begin --  EOF_Char_Is_Empty_String
      return Get_EOF_Char (File) = "";
   end EOF_Char_Is_Empty_String;

   procedure Set_EOF_Char (File : in Tash_File; Char : in String) is

      Objc   : constant Interfaces.C.int := 4;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Set_EOF_Char

      --  Use Tcl_FconfigureObjCmd to set eof char of a file
      ----------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-eofchar");
      Objv (4) := Tash.To_Tcl_Obj (Char);
      --  Put_Objv ("TFIO.SEC:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and return it.
      ---------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
      end;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_EOF_Char;

   procedure Set_EOF_Char (File : in Tash_File; Char : in Character) is
   begin --  Set_EOF_Char
      Set_EOF_Char (File, "" & Char);
   end Set_EOF_Char;

   procedure Set_EOF_Char_To_Empty_String (File : in Tash_File) is
   begin --  Set_EOF_Char_To_Empty_String
      Set_EOF_Char (File, "");
   end Set_EOF_Char_To_Empty_String;

   function Get_EOF_In_Char (File : in Tash_File) return String is
      pragma Unreferenced (File);
   begin --  Get_EOF_In_Char
      return "";
   end Get_EOF_In_Char;

   function Get_EOF_Out_Char (File : in Tash_File) return String is
   pragma Unreferenced (File);
   begin --  Get_EOF_Out_Char
      return "";
   end Get_EOF_Out_Char;

   procedure Set_EOF_Char
     (File     : in Tash_File;
      In_Char  : in String;
      Out_Char : in String)
   is
      pragma Unreferenced (File, In_Char, Out_Char);
   begin --  Set_EOF_Char
      null;
   end Set_EOF_Char;

   function Get_Translation_Mode
     (File : in Tash_File)
      return Translation_Mode
   is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Translation_Mode

      --  Use Tcl_FconfigureObjCmd to get translation mode of a file
      -------------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-translation");
      --  Put_Objv ("TFIO.GTM:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and return it.
      ---------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Translation_Mode'Value (Result_String);
      end;

   end Get_Translation_Mode;

   procedure Set_Translation_Mode
     (File : in Tash_File;
      Mode : in Translation_Mode)
   is

      Objc   : constant Interfaces.C.int := 4;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Set_Translation_Mode

      --  Use Tcl_FconfigureObjCmd to set translation mode of a file
      -------------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("fconfigure");
      Objv (2) := Tash.To_Tcl_Obj (To_String (File));
      Objv (3) := Tash.To_Tcl_Obj ("-translation");
      Objv (4) :=
         Tash.To_Tcl_Obj
           (Ada.Characters.Handling.To_Lower (Translation_Mode'Image (Mode)));
      --  Put_Objv ("TFIO.STM:", Objv);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FconfigureObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter and check for error.
      ---------------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end if;
      end;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_Translation_Mode;

   procedure Get_Translation_Mode
     (File     : in Tash_File;
      In_Mode  : out Translation_Mode;
      Out_Mode : out Translation_Mode)
   is
   pragma Unreferenced (File);
   begin --  Get_Translation_Mode
      In_Mode  := Auto;
      Out_Mode := Auto;
   end Get_Translation_Mode;

   procedure Set_Translation_Mode
     (File     : in Tash_File;
      In_Mode  : in Translation_Mode;
      Out_Mode : in Translation_Mode)
   is
      pragma Unreferenced (File, In_Mode, Out_Mode);
   begin --  Set_Translation_Mode
      null;
   end Set_Translation_Mode;

end Tash.File_IO;
