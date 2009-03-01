--------------------------------------------------------------------
--
--  Unit Name:    Tash.Regexp body
--
--  File Name:    tash-regexp.adb
--
--  Purpose:      Provides regular expression pattern matching
--               on Tash strings and lists.
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

with CHelper;
with Interfaces.C.Strings;
with System;
with Tcl.Ada;

package body Tash.Regexp is

   function To_CString
     (TString : in Tash_Regexp) return Interfaces.C.Strings.chars_ptr;
   function CFlags
     (Mode        : in Regexp_Mode;
      Expanded    : in Boolean;
      Ignore_Case : in Boolean;
      Line        : in Boolean;
      Line_Stop   : in Boolean;
      Line_Anchor : in Boolean) return Interfaces.C.int;
   function Match
     (Source      : in Tcl.Tcl_Obj;
      Pattern     : in Tash_Regexp;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False) return Boolean;

   use type Ada.Strings.Direction;
   use type Interfaces.C.int;
   use type Interfaces.C.long;
   use type Interfaces.C.size_t;
   use type Interfaces.C.Strings.chars_ptr;
   use type Tcl.Tcl_RegExp;

   function To_CString
     (TString : in Tash_Regexp)
      return    Interfaces.C.Strings.chars_ptr
   is
      --
      Length : aliased Interfaces.C.int;
   begin --  To_CString
      if Is_Null (TString) then
         return Interfaces.C.Strings.Null_Ptr;
      else
         return Tcl.Tcl_GetStringFromObj (TString.Obj, Length'Access);
      end if;
   end To_CString;

   function CFlags
     (Mode        : in Regexp_Mode;
      Expanded    : in Boolean;
      Ignore_Case : in Boolean;
      Line        : in Boolean;
      Line_Stop   : in Boolean;
      Line_Anchor : in Boolean)
      return        Interfaces.C.int
   is
      --
      Flags : Interfaces.C.int;
   begin --  CFlags
      case Mode is
         when Basic =>
            Flags := Tcl.TCL_REG_BASIC;
         when Extended =>
            Flags := Tcl.TCL_REG_EXTENDED;
         when Advanced =>
            Flags := Tcl.TCL_REG_ADVANCED;
         when Quote =>
            Flags := Tcl.TCL_REG_QUOTE;
      end case;
      if Expanded then
         Flags := Flags + Tcl.TCL_REG_EXPANDED;
      end if;
      if Ignore_Case then
         Flags := Flags + Tcl.TCL_REG_NOCASE;
      end if;
      if Line then
         Flags := Flags + Tcl.TCL_REG_NEWLINE;
      else
         if Line_Stop then
            Flags := Flags + Tcl.TCL_REG_NLSTOP;
         end if;
         if Line_Anchor then
            Flags := Flags + Tcl.TCL_REG_NLANCH;
         end if;
      end if;
      return Flags;
   end CFlags;

   procedure RegDump (R : Tcl.Tcl_RegExp);
   pragma Import (C, RegDump, "regdump");
   pragma Unreferenced (RegDump);  --  XXX what is it for, then?

   -------------------------------------------------
   --  Converting a string to a Tash_Regexp is
   --  merely converting it to a Tcl object.  Later,
   --  when the object is first used as a pattern,
   --  the pattern will be compiled and cached in
   --  the object.
   -------------------------------------------------

   function To_Tash_Regexp (Str : in String) return Tash_Regexp is
      --
      C_TString : aliased Interfaces.C.char_array := Interfaces.C.To_C (Str);
      New_Obj   : Tcl.Tcl_Obj;
   begin --  To_Tash_Regexp
      New_Obj :=
         Tcl.Tcl_NewStringObj
           (Interfaces.C.Strings.To_Chars_Ptr (C_TString'Unchecked_Access),
            Interfaces.C.int (Str'Length));
      Tcl.Tcl_IncrRefCount (New_Obj);
      return (Ada.Finalization.Controlled with Obj => New_Obj);
   end To_Tash_Regexp;

   function "+" (Str : in String) return Tash_Regexp is
   begin --  "+"
      return To_Tash_Regexp (Str);
   end "+";
   pragma Inline ("+");

   function To_String (Regexp : in Tash_Regexp) return String is
   begin --  To_String
      if Is_Null (Regexp) then
         return "";
      else
         return CHelper.Value (To_CString (Regexp));
      end if;
   end To_String;

   function Match
     (Source      : in Tcl.Tcl_Obj;
      Pattern     : in Tash_Regexp;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False)
      return        Boolean
   is
      --
      Flags  : Interfaces.C.int;
      Regexp : Tcl.Tcl_RegExp;
      Result : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;
   begin --  Match
      Flags :=
         CFlags
           (Mode        => Mode,
            Expanded    => Expanded,
            Ignore_Case => Ignore_Case,
            Line        => Line,
            Line_Stop   => Line_Stop,
            Line_Anchor => Line_Anchor);
      Tash_Interp.Get (Interp);
      Regexp := Tcl.Tcl_GetRegExpFromObj (Interp, Pattern.Obj, Flags);
      if Regexp = null then
         Tash_Interp.Raise_Exception (Interp, Regexp_Error'Identity);
      end if;
      Result := Tcl.Tcl_RegExpExecObj (Interp, Regexp, Source, 0, 0, 0);
      if Result < 0 then
         Tash_Interp.Raise_Exception (Interp, Regexp_Error'Identity);
      end if;
      Tash_Interp.Release (Interp);
      return Result = 1;
   end Match;

   function Match
     (Source      : in String;
      Pattern     : in String;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False)
      return        Boolean
   is
   begin --  Match
      return Match
               (Source      => Tash.To_Tcl_Obj (Source),
                Pattern     => To_Tash_Regexp (Pattern),
                Mode        => Mode,
                Expanded    => Expanded,
                Ignore_Case => Ignore_Case,
                Line        => Line,
                Line_Stop   => Line_Stop,
                Line_Anchor => Line_Anchor);
   end Match;

   function Match
     (Source      : in String;
      Pattern     : in Tash_Regexp;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False)
      return        Boolean
   is
   begin --  Match
      return Match
               (Source      => Tash.To_Tcl_Obj (Source),
                Pattern     => Pattern,
                Mode        => Mode,
                Expanded    => Expanded,
                Ignore_Case => Ignore_Case,
                Line        => Line,
                Line_Stop   => Line_Stop,
                Line_Anchor => Line_Anchor);
   end Match;

   function Match_Element
     (Source      : in Tash.Lists.Tash_List;
      Pattern     : in String;
      Going       : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mode        : in Regexp_Mode           := Advanced;
      Expanded    : in Boolean               := False;
      Ignore_Case : in Boolean               := False;
      Line        : in Boolean               := False;
      Line_Stop   : in Boolean               := False;
      Line_Anchor : in Boolean               := False)
      return        Natural
   is
   begin --  Match_Element
      return Match_Element
               (Source      => Source,
                Pattern     => To_Tash_Regexp (Pattern),
                Going       => Going,
                Mode        => Mode,
                Expanded    => Expanded,
                Ignore_Case => Ignore_Case,
                Line        => Line,
                Line_Stop   => Line_Stop,
                Line_Anchor => Line_Anchor);
   end Match_Element;

   function Match_Element
     (Source      : in Tash.Lists.Tash_List;
      Pattern     : in Tash_Regexp;
      Going       : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mode        : in Regexp_Mode           := Advanced;
      Expanded    : in Boolean               := False;
      Ignore_Case : in Boolean               := False;
      Line        : in Boolean               := False;
      Line_Stop   : in Boolean               := False;
      Line_Anchor : in Boolean               := False)
      return        Natural
   is
   begin --  Match_Element
      if Going = Ada.Strings.Forward then
         for I in  1 .. Tash.Lists.Length (Source) loop
            if Match
                 (Tash.Lists.Get_Element (Source, I),
                  Pattern,
                  Mode,
                  Expanded,
                  Ignore_Case,
                  Line,
                  Line_Stop,
                  Line_Anchor)
            then
               return I;
            end if;
         end loop;
      else
         for I in reverse  1 .. Tash.Lists.Length (Source) loop
            if Match
                 (Tash.Lists.Get_Element (Source, I),
                  Pattern,
                  Mode,
                  Expanded,
                  Ignore_Case,
                  Line,
                  Line_Stop,
                  Line_Anchor)
            then
               return I;
            end if;
         end loop;
      end if;
      return 0;
   end Match_Element;

   function Match
     (Source      : in String;
      Pattern     : in Tash_Regexp;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False)
      return        Tash.Lists.Tash_List
   is
      Flags      : Interfaces.C.int;
      Interp     : Tcl.Tcl_Interp;
      Regexp     : Tcl.Tcl_RegExp;
      Result     : Interfaces.C.int;
      InfoRec    : aliased Tcl.Tcl_RegExpInfo_Rec;
      Matches    : Tcl.Tcl_RegExpIndices;
      Substrings : Tash.Lists.Tash_List;

   begin --  Match

      Flags :=
         CFlags
           (Mode        => Mode,
            Expanded    => Expanded,
            Ignore_Case => Ignore_Case,
            Line        => Line,
            Line_Stop   => Line_Stop,
            Line_Anchor => Line_Anchor);

      Tash_Interp.Get (Interp);

      --  get regular expression
      -------------------------
      Regexp := Tcl.Tcl_GetRegExpFromObj (Interp, Pattern.Obj, Flags);
      if Regexp = null then
         Tash_Interp.Raise_Exception (Interp, Regexp_Error'Identity);
      end if;

      --  perform regexp match
      -----------------------
      Result :=
         Tcl.Tcl_RegExpExecObj
           (Interp,
            Regexp,
            Tash.To_Tcl_Obj (Source),
            0,
            -1,
            0);
      if Result < 0 then
         Tash_Interp.Raise_Exception (Interp, Regexp_Error'Identity);
      else
         Tash_Interp.Release (Interp);
      end if;

      --  get information about this match
      -----------------------------------
      Tcl.Tcl_RegExpGetInfo (Regexp, InfoRec'Unchecked_Access);
      Matches := InfoRec.matches;

      --  Each iteration of this loop gets the next matching
      --  substring and appends it to Substrings.
      -----------------------------------------------------
      for I in  1 .. InfoRec.nsubs + 1 loop
         declare
            Substring : constant String :=
               Source (
               Positive (Matches.Start + 1) .. Positive (Matches.E_n_d));
         begin
            Tash.Lists.Append (Substrings, Substring);
         end;
         Tcl.Tcl_RegExpIndices_Pointers.Increment (Matches);
      end loop;

      return Substrings;

   end Match;

   function Match
     (Source      : in String;
      Pattern     : in String;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False)
      return        Tash.Lists.Tash_List
   is
   begin --  Match
      return Match
               (Source      => Source,
                Pattern     => To_Tash_Regexp (Pattern),
                Mode        => Mode,
                Expanded    => Expanded,
                Ignore_Case => Ignore_Case,
                Line        => Line,
                Line_Stop   => Line_Stop,
                Line_Anchor => Line_Anchor);
   end Match;

   function Tcl_RegsubObjCmd
     (dummy  : in Tcl.ClientData;
      interp : in Tcl.Tcl_Interp;
      objc   : in Interfaces.C.int;
      objv   : in Tcl.Tcl_Obj_Array)
      return   Interfaces.C.int;
   pragma Import (C, Tcl_RegsubObjCmd, "Tcl_RegsubObjCmd");

   function Substitute
     (Source      : in String;
      Pattern     : in String;
      Sub_Spec    : in String;
      Sub_All     : in Boolean := False;
      Expanded    : in Boolean := False;
      Ignore_Case : in Boolean := False;
      Line        : in Boolean := False;
      Line_Stop   : in Boolean := False;
      Line_Anchor : in Boolean := False)
      return        String
   is
      Objc   : Interfaces.C.int;
      Interp : Tcl.Tcl_Interp;
      Result : Interfaces.C.int;

   begin --  Substitute

      --  Count how many object arguments we'll
      --  have when calling Tcl_RegsubObjCmd.
      ----------------------------------------
      Objc := 6;
      if Sub_All then
         Objc := Objc + 1;
      end if;
      if Expanded then
         Objc := Objc + 1;
      end if;
      if Ignore_Case then
         Objc := Objc + 1;
      end if;
      if Line then
         Objc := Objc + 1;
      end if;
      if Line_Stop then
         Objc := Objc + 1;
      end if;
      if Line_Anchor then
         Objc := Objc + 1;
      end if;

      declare
         Objv : Tcl.Tcl_Obj_Array (1 .. Objc);
         I    : Interfaces.C.int := 1;
      begin

         --  Create object arguments for
         --  call to Tcl_RegesubObjCmd.
         ------------------------------
         Objv (I) := Tash.To_Tcl_Obj ("regsub");
         I        := I + 1;
         if Sub_All then
            Objv (I) := Tash.To_Tcl_Obj ("-all");
            I        := I + 1;
         end if;
         if Expanded then
            Objv (I) := Tash.To_Tcl_Obj ("-expanded");
            I        := I + 1;
         end if;
         if Ignore_Case then
            Objv (I) := Tash.To_Tcl_Obj ("-nocase");
            I        := I + 1;
         end if;
         if Line then
            Objv (I) := Tash.To_Tcl_Obj ("-line");
            I        := I + 1;
         end if;
         if Line_Stop then
            Objv (I) := Tash.To_Tcl_Obj ("-linestop");
            I        := I + 1;
         end if;
         if Line_Anchor then
            Objv (I) := Tash.To_Tcl_Obj ("-lineanchor");
            I        := I + 1;
         end if;
         Objv (I) := Tash.To_Tcl_Obj ("--");
         I        := I + 1;
         Objv (I) := Tash.To_Tcl_Obj (Pattern);
         I        := I + 1;
         Objv (I) := Tash.To_Tcl_Obj (Source);
         I        := I + 1;
         Objv (I) := Tash.To_Tcl_Obj (Sub_Spec);
         I        := I + 1;

         --  Create a variable in which regsub will store the
         --  modified string.  Store the name of this variable
         --  in the last object parameter for regsub.
         ----------------------------------------------------
         Tash_Interp.Get (Interp);
         Tcl.Ada.Tcl_SetVar (Interp, "_TASH_Regsub_Result", "");
         Objv (I) := Tash.To_Tcl_Obj ("_TASH_Regsub_Result");

         --  Perform the substitution(s) and check for errors
         ---------------------------------------------------
         Tcl.Tcl_ResetResult (Interp);
         Result :=
            Tcl_RegsubObjCmd
              (dummy  => System.Null_Address,
               interp => Interp,
               objc   => Objc,
               objv   => Objv);
         Tash_Interp.Assert (Interp, Result, Regexp_Error'Identity);

         --  Get and return the regsub'd string
         -------------------------------------
         declare
            Regsub_Result : constant String :=
               Tcl.Ada.Tcl_GetVar (Interp, "_TASH_Regsub_Result");
         begin
            Tcl.Ada.Tcl_UnsetVar (Interp, "_TASH_Regsub_Result");
            Tcl.Tcl_ResetResult (Interp);
            Tash_Interp.Release (Interp);
            return Regsub_Result;
         end;

      end;

   end Substitute;

end Tash.Regexp;
