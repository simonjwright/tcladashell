-------------------------------------------------------------------
--
--  Unit Name:    Tash.File body
--
--  File Name:    tash-file.adb
--
--  Purpose:      Provides file information and manipulation routines.
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with CHelper;
with System;
with Tash.Arrays;

package body Tash.File is

   use type Interfaces.C.int;

   function Match (Source, Pattern : in String) return Boolean;
   function To_Calendar_Time
     (Seconds_Since_Epoch : in String)
      return                Ada.Calendar.Time;
   function To_Epoch_Time (Date : in Ada.Calendar.Time) return String;
   function To_Attribute_String (Attr : in Attribute) return String;

   function Tcl_FileObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_FileObjCmd, "Tcl_FileObjCmd");

   function Tcl_CdObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_CdObjCmd, "Tcl_CdObjCmd");

   function Tcl_GlobObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_GlobObjCmd, "Tcl_GlobObjCmd");

   function Tcl_PwdObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_PwdObjCmd, "Tcl_PwdObjCmd");

   function Match (Source, Pattern : in String) return Boolean is
   begin --  Match
      return Ada.Strings.Fixed.Index (Source, Pattern) > 0;
   end Match;
   pragma Inline (Match);

   Epoch : constant Ada.Calendar.Time
     := Ada.Calendar.Time_Of (Year => 1970,
                              Month => 1,
                              Day => 1,
                              Seconds => 0.0);

   --  Convert time in seconds since the epoch to Ada Calendar time.
   ----------------------------------------------------------------
   function To_Calendar_Time
     (Seconds_Since_Epoch : in String)
      return                Ada.Calendar.Time
   is
      use type Ada.Calendar.Time;
   begin --  To_Calendar_Time
      return Epoch + Duration'Value (Seconds_Since_Epoch);
   end To_Calendar_Time;

   --  Convert Ada calendar time to time in seconds since the epoch.
   ----------------------------------------------------------------
   function To_Epoch_Time (Date : in Ada.Calendar.Time) return String
   is
      use type Ada.Calendar.Time;
      Duration_Since_Epoch : constant Duration := Date - Epoch;
      Seconds_Since_Epoch : constant Integer := Integer (Duration_Since_Epoch);
      Result : constant String := Integer'Image (Seconds_Since_Epoch);
   begin --  To_Epoch_Time
      --  Do not return the leading space produced by 'Image
      return Result (2 .. Result'Last);
   end To_Epoch_Time;

   function Get_Access_Time (Name : String) return Ada.Calendar.Time is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Access_Time

      --  Use Tcl_FileObjCmd to get the access time of a file.
      -------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("atime");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter (in seconds since
      --  the epoch) and convert it to Ada calendar time.
      --------------------------------------------------
      declare
         Access_Time : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            if Match (Access_Time, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Access_Time);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Access_Time);
            end if;
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return To_Calendar_Time (Access_Time);
      end;

   end Get_Access_Time;

   procedure Set_Access_Time (Name : String; Date : Ada.Calendar.Time) is

      Objc        : constant Interfaces.C.int := 4;
      Objv        : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result      : Interfaces.C.int;
      Interp      : Tcl.Tcl_Interp;
      Access_Time : constant String  := To_Epoch_Time (Date);

   begin --  Set_Access_Time

      --  Use Tcl_FileObjCmd to set the access time of a file.
      -------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("atime");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Objv (4) := Tash.To_Tcl_Obj (Access_Time);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  check for errors
      -------------------
      if Result = Tcl.TCL_ERROR then
         declare
            Result_String : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            if Match (Result_String, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Result_String);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end;
      end if;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_Access_Time;

   function To_Attribute_String (Attr : in Attribute) return String is
   begin --  To_Attribute_String
      case Attr is
         when SystemAttr =>
            return "-system";
         when FType =>
            return "-type";
         when others =>
            return "-" &
                   Ada.Characters.Handling.To_Lower (Attribute'Image (Attr));
      end case;
   end To_Attribute_String;

   function Get_Attribute
     (Name : in String;
      Attr : in Attribute)
      return String
   is
      Objc   : constant Interfaces.C.int := 4;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Attribute

      --  Use Tcl_FileObjCmd to get the specified attrubute
      ----------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("attributes");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Objv (4) := Tash.To_Tcl_Obj (To_Attribute_String (Attr));
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Check for errors, get result from interpreter and return it
      --------------------------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            if Match (Result_String, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Result_String);
            elsif Match (Result_String, "bad option") then
               Tcl.Tcl_ResetResult (Interp);
               Tash_Interp.Release (Interp);
               return "";
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Result_String;
      end;

   end Get_Attribute;

   procedure Set_Attribute
     (Name  : in String;
      Attr  : in Attribute;
      Value : in String)
   is

      Objc   : constant Interfaces.C.int := 5;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Set_Attribute

      --  Use Tcl_FileObjCmd to set the attributes of a file
      -----------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("attributes");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Objv (4) := Tash.To_Tcl_Obj (To_Attribute_String (Attr));
      Objv (5) := Tash.To_Tcl_Obj (Value);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  check for errors
      -------------------
      if Result = Tcl.TCL_ERROR then
         declare
            Result_String : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            if Match (Result_String, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Result_String);
            elsif Match (Result_String, "bad option") then
               null;
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end;
      end if;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_Attribute;

   procedure Copy
     (Source : in String;
      Target : in String;
      Force  : in Boolean := False)
   is

      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Copy

      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);

      --  Use Tcl_FileObjCmd to copy the file
      --------------------------------------
      if Force then
         declare
            Objc : constant Interfaces.C.int := 5;
            Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         begin
            Objv (1) := Tash.To_Tcl_Obj ("file");
            Objv (2) := Tash.To_Tcl_Obj ("copy");
            Objv (3) := Tash.To_Tcl_Obj ("-force");
            Objv (4) := Tash.To_Tcl_Obj (Source);
            Objv (5) := Tash.To_Tcl_Obj (Target);
            Result   :=
               Tcl_FileObjCmd
                 (dummy  => System.Null_Address,
                  interp => Interp,
                  objc   => Objc,
                  objv   => Objv);
            for I in  Objv'Range loop
               Tcl.Tcl_DecrRefCount (Objv (I));
            end loop;
         end;
      else
         declare
            Objc : constant Interfaces.C.int := 4;
            Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         begin
            Objv (1) := Tash.To_Tcl_Obj ("file");
            Objv (2) := Tash.To_Tcl_Obj ("copy");
            Objv (3) := Tash.To_Tcl_Obj (Source);
            Objv (4) := Tash.To_Tcl_Obj (Target);
            Result   :=
               Tcl_FileObjCmd
                 (dummy  => System.Null_Address,
                  interp => Interp,
                  objc   => Objc,
                  objv   => Objv);
            for I in  Objv'Range loop
               Tcl.Tcl_DecrRefCount (Objv (I));
            end loop;
         end;
      end if;

      --  check for errors
      -------------------
      if Result = Tcl.TCL_ERROR then
         declare
            Result_String : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            if Match (Result_String, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Result_String);
            elsif Match (Result_String, "file already exists") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  File_Already_Exists'Identity,
                  Result_String);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end;
      end if;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Copy;

   procedure Delete (Name : in String; Force : in Boolean := False) is

      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Delete

      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);

      --  Use Tcl_FileObjCmd to delete the file
      ----------------------------------------
      if Force then
         declare
            Objc : constant Interfaces.C.int := 4;
            Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         begin
            Objv (1) := Tash.To_Tcl_Obj ("file");
            Objv (2) := Tash.To_Tcl_Obj ("delete");
            Objv (3) := Tash.To_Tcl_Obj ("-force");
            Objv (4) := Tash.To_Tcl_Obj (Name);
            Result   :=
               Tcl_FileObjCmd
                 (dummy  => System.Null_Address,
                  interp => Interp,
                  objc   => Objc,
                  objv   => Objv);
            for I in  Objv'Range loop
               Tcl.Tcl_DecrRefCount (Objv (I));
            end loop;
         end;
      else
         declare
            Objc : constant Interfaces.C.int := 3;
            Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         begin
            Objv (1) := Tash.To_Tcl_Obj ("file");
            Objv (2) := Tash.To_Tcl_Obj ("delete");
            Objv (3) := Tash.To_Tcl_Obj (Name);
            Result   :=
               Tcl_FileObjCmd
                 (dummy  => System.Null_Address,
                  interp => Interp,
                  objc   => Objc,
                  objv   => Objv);
            for I in  Objv'Range loop
               Tcl.Tcl_DecrRefCount (Objv (I));
            end loop;
         end;
      end if;

      --  check for errors
      -------------------
      if Result = Tcl.TCL_ERROR then
         declare
            Result_String : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            if Match (Result_String, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Result_String);
            elsif Match (Result_String, "file already exists") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  File_Already_Exists'Identity,
                  Result_String);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end;
      end if;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Delete;

   function Directory_Name (Name : in String) return String is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Directory_Name

      --  Use Tcl_FileObjCmd to get the directory_name of a file
      ---------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("dirname");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
         return Result_String;
      end;

   end Directory_Name;

   function Exists (Name : in String) return Boolean is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Exists

      --  Use Tcl_FileObjCmd to determine whether a file exists
      --------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("exists");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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

   end Exists;

   function Executable (Name : in String) return Boolean is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Executable

      --  Use Tcl_FileObjCmd to determine whether a file is executable
      ---------------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("executable");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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

   end Executable;

   function Extension (Name : in String) return String is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Extension

      --  Use Tcl_FileObjCmd to get the extension of a file
      ---------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("extension");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
         return Result_String;
      end;

   end Extension;

   function Is_Directory (Name : in String) return Boolean is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Is_Directory

      --  Use Tcl_FileObjCmd to determine whether a file is a directory
      ----------------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("isdirectory");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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

   end Is_Directory;

   function Is_File (Name : in String) return Boolean is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Is_File

      --  Use Tcl_FileObjCmd to determine whether a file is a regular file
      -------------------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("isfile");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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

   end Is_File;

   function Join
     (Name1 : in String;
      Name2 : in String := "";
      Name3 : in String := "";
      Name4 : in String := "";
      Name5 : in String := "";
      Name6 : in String := "";
      Name7 : in String := "";
      Name8 : in String := "";
      Name9 : in String := "")
      return  String
   is
      Objc   : Interfaces.C.int := 11;
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Join

      if Name9 = "" then
         Objc := Objc - 1;
         if Name8 = "" then
            Objc := Objc - 1;
            if Name7 = "" then
               Objc := Objc - 1;
               if Name6 = "" then
                  Objc := Objc - 1;
                  if Name5 = "" then
                     Objc := Objc - 1;
                     if Name4 = "" then
                        Objc := Objc - 1;
                        if Name3 = "" then
                           Objc := Objc - 1;
                           if Name2 = "" then
                              Objc := Objc - 1;
                              if Name1 = "" then
                                 return "";
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;

      --  Use Tcl_FileObjCmd to join the path name elements
      ----------------------------------------------------
      declare
         Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
      begin
         Objv (1) := Tash.To_Tcl_Obj ("file");
         Objv (2) := Tash.To_Tcl_Obj ("join");
         Objv (3) := Tash.To_Tcl_Obj (Name1);
         if Objc > 3 then
            Objv (4) := Tash.To_Tcl_Obj (Name2);
            if Objc > 4 then
               Objv (5) := Tash.To_Tcl_Obj (Name3);
               if Objc > 5 then
                  Objv (6) := Tash.To_Tcl_Obj (Name4);
                  if Objc > 6 then
                     Objv (7) := Tash.To_Tcl_Obj (Name5);
                     if Objc > 7 then
                        Objv (8) := Tash.To_Tcl_Obj (Name6);
                        if Objc > 8 then
                           Objv (9) := Tash.To_Tcl_Obj (Name7);
                           if Objc > 9 then
                              Objv (10) := Tash.To_Tcl_Obj (Name8);
                              if Objc > 10 then
                                 Objv (11) := Tash.To_Tcl_Obj (Name9);
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
         Tash_Interp.Get (Interp);
         Tcl.Tcl_ResetResult (Interp);
         Result :=
            Tcl_FileObjCmd
              (dummy  => System.Null_Address,
               interp => Interp,
               objc   => Objc,
               objv   => Objv);
         for I in  Objv'Range loop
            Tcl.Tcl_DecrRefCount (Objv (I));
         end loop;
      end;

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
         return Result_String;
      end;

   end Join;

   function Join (Name_List : in Tash.Lists.Tash_List) return String is
      Length : constant Natural := Tash.Lists.Length (Name_List);
      Objc   : constant Interfaces.C.int := 2 + Interfaces.C.int (Length);
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Join

      --  Use Tcl_FileObjCmd to join the path name elements
      ----------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("join");
      for I in  1 .. Length loop
         Objv (2 + Interfaces.C.int (I))  :=
            Tash.To_Tcl_Obj (Tash.Lists.Get_Element (Name_List, I));
      end loop;
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Check for errors, get result from interpreter and return it
      --------------------------------------------------------------
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
         return Result_String;
      end;

   end Join;

   procedure Get_Link_Status
     (Name   : in String;
      Status : in out Tash.Arrays.Tash_Array);
   --  Returns status information for the specified link (or for the file,
   --  if Name is a file rather than a link).  Array elements include atime,
   --  ctime, dev, gid, ino, mode, mtime, nlink, size, type, and uid.  Each
   --  element except type is a decimal string with the value of the
   --  corresponding field from the stat return structure; see the manual
   --  entry for stat(2) for details on the meanings of the values.  The
   --  type element gives the type of the file in the same form returned
   --  by the function Tash.File.Type.
   pragma Unreferenced (Get_Link_Status);  --  XXX what is it for?

   procedure Get_Link_Status
     (Name   : in String;
      Status : in out Tash.Arrays.Tash_Array)
   is

      Objc   : constant Interfaces.C.int := 4;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Link_Status

      --  Create Status as a Tash_Array if it is null
      ----------------------------------------------
      if Tash.Arrays.Is_Null (Status) then
         Tash.Arrays.Set_Element (Status, "index", "value");
         Tash.Arrays.Delete_Element (Status, "index");
      end if;

      --  Use Tcl_FileObjCmd to get the lstat data
      -------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("lstat");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Objv (4) := Tash.To_Tcl_Obj (Tash.Arrays.Internal_Name (Status));
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  check for errors
      -------------------
      if Result = Tcl.TCL_ERROR then
         declare
            Result_String : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            if Match (Result_String, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Result_String);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end;
      end if;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Get_Link_Status;

   procedure Make_Directory (Name : in String) is

      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Make_Directory

      --  Use Tcl_FileObjCmd to create the directory
      ---------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("mkdir");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  check for errors
      -------------------
      if Result = Tcl.TCL_ERROR then
         declare
            Result_String : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            if Match (Result_String, "file already exists") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  File_Already_Exists'Identity,
                  Result_String);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end;
      end if;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Make_Directory;

   function Get_Modified_Time (Name : String) return Ada.Calendar.Time is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Modified_Time

      --  Use Tcl_FileObjCmd to get the modified time of a file.
      ---------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("mtime");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter (in seconds since
      --  the epoch) and convert it to Ada calendar time.
      --------------------------------------------------
      declare
         Modified_Time : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            if Match (Modified_Time, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Modified_Time);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Modified_Time);
            end if;
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return To_Calendar_Time (Modified_Time);
      end;

   end Get_Modified_Time;

   procedure Set_Modified_Time (Name : String; Date : Ada.Calendar.Time) is

      Objc          : constant Interfaces.C.int := 4;
      Objv          : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result        : Interfaces.C.int;
      Interp        : Tcl.Tcl_Interp;
      Modified_Time : constant String  := To_Epoch_Time (Date);

   begin --  Set_Modified_Time

      --  Use Tcl_FileObjCmd to set the modified time of a file.
      ---------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("mtime");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Objv (4) := Tash.To_Tcl_Obj (Modified_Time);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  check for errors
      -------------------
      if Result = Tcl.TCL_ERROR then
         declare
            Result_String : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            if Match (Result_String, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Result_String);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end;
      end if;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Set_Modified_Time;

   function Native_Name (Name : in String) return String is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Native_Name

      --  Use Tcl_FileObjCmd to get the native name of a file
      ------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("nativename");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
         return Result_String;
      end;

   end Native_Name;

   function Owned (Name : in String) return Boolean is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Owned

      --  Use Tcl_FileObjCmd to determine whether a file owned
      --------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("owned");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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

   end Owned;

   function Get_Path_Type (Name : in String) return Path_Type is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_Path_Type

      --  Use Tcl_FileObjCmd to get the path type of a file
      ----------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("pathtype");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
         return Path_Type'Value (Result_String);
      end;

   end Get_Path_Type;

   function Readable (Name : in String) return Boolean is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Readable

      --  Use Tcl_FileObjCmd to determine whether a file readable
      ----------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("readable");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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

   end Readable;

   function Is_Link (Name : in String) return Boolean is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Is_Link

      --  Use Tcl_FileObjCmd to determine whether a file is a link
      --  by calling "file readlink"
      -----------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("readlink");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);
      return Result = Tcl.TCL_OK;

   end Is_Link;

   function Read_Link (Name : in String) return String is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Read_Link

      if not Exists (Name) then
         raise No_Such_File;
      end if;

      --  Use Tcl_FileObjCmd to get the file name of a link
      ----------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("readlink");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
            Tcl.Tcl_ResetResult (Interp);
            Tash_Interp.Release (Interp);
            return Name;
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Result_String;
      end;

   end Read_Link;

   procedure Rename
     (Source : in String;
      Target : in String;
      Force  : in Boolean := False)
   is

      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Rename

      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);

      --  Use Tcl_FileObjCmd to rename the file
      ----------------------------------------
      if Force then
         declare
            Objc : constant Interfaces.C.int := 5;
            Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         begin
            Objv (1) := Tash.To_Tcl_Obj ("file");
            Objv (2) := Tash.To_Tcl_Obj ("rename");
            Objv (3) := Tash.To_Tcl_Obj ("-force");
            Objv (4) := Tash.To_Tcl_Obj (Source);
            Objv (5) := Tash.To_Tcl_Obj (Target);
            Result   :=
               Tcl_FileObjCmd
                 (dummy  => System.Null_Address,
                  interp => Interp,
                  objc   => Objc,
                  objv   => Objv);
            for I in  Objv'Range loop
               Tcl.Tcl_DecrRefCount (Objv (I));
            end loop;
         end;
      else
         declare
            Objc : constant Interfaces.C.int := 4;
            Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         begin
            Objv (1) := Tash.To_Tcl_Obj ("file");
            Objv (2) := Tash.To_Tcl_Obj ("rename");
            Objv (3) := Tash.To_Tcl_Obj (Source);
            Objv (4) := Tash.To_Tcl_Obj (Target);
            Result   :=
               Tcl_FileObjCmd
                 (dummy  => System.Null_Address,
                  interp => Interp,
                  objc   => Objc,
                  objv   => Objv);
            for I in  Objv'Range loop
               Tcl.Tcl_DecrRefCount (Objv (I));
            end loop;
         end;
      end if;

      --  check for errors
      -------------------
      if Result = Tcl.TCL_ERROR then
         declare
            Result_String : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            if Match (Result_String, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Result_String);
            elsif Match (Result_String, "file already exists") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  File_Already_Exists'Identity,
                  Result_String);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end;
      end if;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Rename;

   function Root_Name (Name : in String) return String is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Root_Name

      --  Use Tcl_FileObjCmd to get the root name of a file
      ----------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("rootname");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
         return Result_String;
      end;

   end Root_Name;

   function Size (Name : in String) return Natural is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Size

      --  Use Tcl_FileObjCmd to get the size of a file
      -----------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("size");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
           (dummy  => System.Null_Address,
            interp => Interp,
            objc   => Objc,
            objv   => Objv);
      for I in  Objv'Range loop
         Tcl.Tcl_DecrRefCount (Objv (I));
      end loop;

      --  Get result from interpreter, convert to an
      --  integer and return it.
      ---------------------------------------------
      declare
         Result_String : constant String :=
            CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
      begin
         if Result = Tcl.TCL_ERROR then
            if Match (Result_String, "no such file") then
               Tash_Interp.Raise_Exception
                 (Interp,
                  No_Such_File'Identity,
                  Result_String);
            else
               Tash_Interp.Raise_Exception
                 (Interp,
                  Tash.Tcl_Error'Identity,
                  Result_String);
            end if;
         end if;
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         return Natural'Value (Result_String);
      end;

   end Size;

   function Split (Name : in String) return Tash.Lists.Tash_List is
      Objc      : constant Interfaces.C.int := 3;
      Objv      : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result    : Interfaces.C.int;
      Interp    : Tcl.Tcl_Interp;
      Path_List : Tash.Lists.Tash_List;
      pragma Unreferenced (Result);  --  XXX why not?

   begin --  Split

      --  Use Tcl_FileObjCmd to get the split a file name
      --------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("split");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
      Tash.Tash_Object (Path_List).Obj :=
         Tcl.Tcl_DuplicateObj (Tcl.Tcl_GetObjResult (Interp));
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);
      Tcl.Tcl_IncrRefCount (Tash.Tash_Object (Path_List).Obj);
      return Path_List;

   end Split;

   function Tail (Name : in String) return String is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Tail

      --  Use Tcl_FileObjCmd to get the tail of a file
      -----------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("tail");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
         return Result_String;
      end;

   end Tail;

   function Get_File_Type (Name : in String) return File_Type is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Get_File_Type

      --  Use Tcl_FileObjCmd to get the file type of a file
      ------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("type");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
         return File_Type'Value (Result_String);
      end;

   end Get_File_Type;

   function Volume return Tash.Lists.Tash_List is
      Objc      : constant Interfaces.C.int := 2;
      Objv      : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result    : Interfaces.C.int;
      Interp    : Tcl.Tcl_Interp;
      Path_List : Tash.Lists.Tash_List;
      pragma Unreferenced (Result);  --  XXX why not?

   begin --  Volume

      --  Use Tcl_FileObjCmd to get the volumes on this system
      -------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("volume");
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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
      Tash.Tash_Object (Path_List).Obj :=
         Tcl.Tcl_DuplicateObj (Tcl.Tcl_GetObjResult (Interp));
      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);
      Tcl.Tcl_IncrRefCount (Tash.Tash_Object (Path_List).Obj);
      return Path_List;

   end Volume;

   function Writable (Name : in String) return Boolean is
      Objc   : constant Interfaces.C.int := 3;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Writable

      --  Use Tcl_FileObjCmd to determine whether a file writable
      ----------------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("file");
      Objv (2) := Tash.To_Tcl_Obj ("writable");
      Objv (3) := Tash.To_Tcl_Obj (Name);
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_FileObjCmd
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

   end Writable;

   procedure Change_Directory (Name : in String := "") is

      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Change_Directory

      --  Use Tcl_CdObjCmd to change the directory
      -------------------------------------------
      if Name = "" then
         declare
            Objc : constant Interfaces.C.int := 1;
            Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         begin
            Objv (1) := Tash.To_Tcl_Obj ("cd");
            Tash_Interp.Get (Interp);
            Tcl.Tcl_ResetResult (Interp);
            Result :=
               Tcl_CdObjCmd
                 (dummy  => System.Null_Address,
                  interp => Interp,
                  objc   => Objc,
                  objv   => Objv);
            for I in  Objv'Range loop
               Tcl.Tcl_DecrRefCount (Objv (I));
            end loop;
         end;
      else
         declare
            Objc : constant Interfaces.C.int := 2;
            Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         begin
            Objv (1) := Tash.To_Tcl_Obj ("cd");
            Objv (2) := Tash.To_Tcl_Obj (Name);
            Tash_Interp.Get (Interp);
            Tcl.Tcl_ResetResult (Interp);
            Result :=
               Tcl_CdObjCmd
                 (dummy  => System.Null_Address,
                  interp => Interp,
                  objc   => Objc,
                  objv   => Objv);
            for I in  Objv'Range loop
               Tcl.Tcl_DecrRefCount (Objv (I));
            end loop;
         end;
      end if;

      --  check for errors
      -------------------
      if Result = Tcl.TCL_ERROR then
         declare
            Result_String : constant String :=
               CHelper.Value (Tcl.Tcl_GetStringResult (Interp));
         begin
            Tash_Interp.Raise_Exception
              (Interp,
               Tash.Tcl_Error'Identity,
               Result_String);
         end;
      end if;

      Tcl.Tcl_ResetResult (Interp);
      Tash_Interp.Release (Interp);

   end Change_Directory;

   function Match
     (Pattern     : in String;
      Directory   : in String := "";
      Path_Prefix : in String := "";
      Type_List   : in String := "")
      return        Tash.Lists.Tash_List
   is
      Objc      : Interfaces.C.int := 3;
      Result    : Interfaces.C.int;
      Interp    : Tcl.Tcl_Interp;
      Types     : Tash.Lists.Tash_List;
      File_List : Tash.Lists.Tash_List;
      pragma Unreferenced (Result);  -- XXX why not?

   begin --  Match

      --  compute number of arguments for glob command
      -----------------------------------------------
      if Directory /= "" then
         Objc := Objc + 2;
      end if;
      if Path_Prefix /= "" then
         Objc := Objc + 2;
      end if;
      if Type_List /= "" then
         Types := Tash.Lists.To_Tash_List (Type_List);
         Objc  := Objc + 1 + Interfaces.C.int (Tash.Lists.Length (Types));
      end if;

      --  Use Tcl_GlobObjCmd to match file names
      -----------------------------------------
      declare
         Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         I    : Interfaces.C.int := 3;
      begin
         Objv (1) := Tash.To_Tcl_Obj ("glob");
         Objv (2) := Tash.To_Tcl_Obj ("-nocomplain");
         if Directory /= "" then
            Objv (I)     := Tash.To_Tcl_Obj ("-directory");
            Objv (I + 1) := Tash.To_Tcl_Obj (Directory);
            I            := I + 2;
         end if;
         if Path_Prefix /= "" then
            Objv (I)     := Tash.To_Tcl_Obj ("-path");
            Objv (I + 1) := Tash.To_Tcl_Obj (Path_Prefix);
            I            := I + 2;
         end if;
         if Type_List /= "" then
            Objv (I) := Tash.To_Tcl_Obj ("-types");
            I        := I + 1;
            for J in  1 .. Tash.Lists.Length (Types) loop
               Objv (I) :=
                  Tash.To_Tcl_Obj (Tash.Lists.Get_Element (Types, J));
               I        := I + 1;
            end loop;
         end if;
         Objv (I) := Tash.To_Tcl_Obj (Pattern);

         Tash_Interp.Get (Interp);
         Tcl.Tcl_ResetResult (Interp);
         Result :=
            Tcl_GlobObjCmd
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
         Tash.Tash_Object (File_List).Obj :=
            Tcl.Tcl_DuplicateObj (Tcl.Tcl_GetObjResult (Interp));
         Tcl.Tcl_ResetResult (Interp);
         Tash_Interp.Release (Interp);
         Tcl.Tcl_IncrRefCount (Tash.Tash_Object (File_List).Obj);
         return File_List;

      end;

   end Match;

   function Current_Working_Directory return String is
      Objc   : constant Interfaces.C.int := 1;
      Objv   : Tcl.Tcl_Obj_Array (1 .. Objc);
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;

   begin --  Current_Working_Directory

      --  Use Tcl_PwdObjCmd to get current working directory
      -----------------------------------------------------
      Objv (1) := Tash.To_Tcl_Obj ("pwd");
      Tash_Interp.Get (Interp);
      Tcl.Tcl_ResetResult (Interp);
      Result :=
         Tcl_PwdObjCmd
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
         return Result_String;
      end;

   end Current_Working_Directory;

end Tash.File;
