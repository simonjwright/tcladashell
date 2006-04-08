--------------------------------------------------------------------
--
-- Unit Name:    Tash.Regexp body
--
-- File Name:    tash-regexp.adb
--
-- Purpose:      Provides regular expression pattern matching
--               on Tash strings and lists.
--
-- Copyright (c) 1999 Terry J. Westley
--
-- Tash is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free
-- Software Foundation; either version 2, or (at your option) any later
-- version. Tash is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. You should have received a copy of the GNU General
-- Public License distributed with Tash; see file COPYING. If not, write to
--
--          Software Foundation
--          59 Temple Place - Suite 330
--          Boston, MA 02111-1307, USA
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Tash is maintained by Terry Westley (http://www.adatcl.com).
--
--------------------------------------------------------------------

with CHelper;
with Interfaces.C.Strings;
with Tash.Strings;
with Tash.Test;

package body Tash.Regexp is

   use type Ada.Strings.Direction;
   use type Interfaces.C.Int;
   use type Interfaces.C.Size_T;
   use type Interfaces.C.Strings.Chars_Ptr;
   use type Tcl.Tcl_RegExp;

   function To_CString (
      TString : in Tash_Regexp) return Interfaces.C.Strings.Chars_Ptr is
   --
      Length : aliased Interfaces.C.Int;
   begin -- To_CString
      if Is_Null (TString) then
         return Interfaces.C.Strings.Null_Ptr;
      else
         return Tcl.Tcl_GetStringFromObj (TString.Obj, Length'Access);
      end if;
   end To_CString;

   procedure RegDump (R : Tcl.Tcl_Regexp);
   pragma Import (C, RegDump, "regdump");

   -------------------------------------------------
   -- Converting a string to a Tash_Regexp is
   -- merely converting it to a Tcl object.  Later,
   -- when the object is first used as a pattern,
   -- the pattern will be compiled and cached in
   -- the object.
   -------------------------------------------------

   function To_Tash_Regexp (
      Str : in String) return Tash_Regexp is
   --
      C_TString : aliased Interfaces.C.Char_Array := Interfaces.C.To_C (Str);
      New_Obj   : Tcl.Tcl_Obj;
   begin -- To_Tash_Regexp
      New_Obj := Tcl.Tcl_NewStringObj (
         Interfaces.C.Strings.To_Chars_Ptr (C_TString'Unchecked_access),
         Interfaces.C.Int (Str'length));
      Tcl.Tcl_IncrRefCount (New_Obj);
      return (Ada.Finalization.Controlled with Obj => New_Obj);
   end To_Tash_Regexp;

   function "+" (
      Str : in String) return Tash_Regexp is
   begin -- "+"
      return To_Tash_Regexp (Str);
   end "+";
   pragma Inline ("+");

   function To_Tash_Regexp (
      TString : in Tash.Strings.Tash_String) return Tash_Regexp is
   --
      New_Obj : Tcl.Tcl_Obj;
   begin -- To_Tash_Regexp
      New_Obj := Tcl.Tcl_DuplicateObj (TString.Obj);
      Tcl.Tcl_IncrRefCount (New_Obj);
      return (Ada.Finalization.Controlled with Obj => New_Obj);
   end To_Tash_Regexp;

   function To_String (
      Regexp : in Tash_Regexp) return String is
   begin -- To_String
      if Is_Null (Regexp) then
         return "";
      else
         return CHelper.Value (To_CString (Regexp));
      end if;
   end To_String;

   function Match (
      Source      : in String;
      Pattern     : in String;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False) return Boolean is
   begin -- Match
      return Match (
         Source      => Tash.Strings.To_Tash_String (Source),
         Pattern     => To_Tash_Regexp (Pattern),
         Mode        => Mode,
         Expanded    => Expanded,
         Ignore_Case => Ignore_Case,
         Line        => Line,
         Line_Stop   => Line_Stop,
         Line_Anchor => Line_Anchor);
   end Match;

   function Match (
      Source      : in String;
      Pattern     : in Tash_Regexp;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False) return Boolean is
   begin -- Match
      return Match (
         Source      => Tash.Strings.To_Tash_String (Source),
         Pattern     => Pattern,
         Mode        => Mode,
         Expanded    => Expanded,
         Ignore_Case => Ignore_Case,
         Line        => Line,
         Line_Stop   => Line_Stop,
         Line_Anchor => Line_Anchor);
   end Match;

   function Match (
      Source      : in Tash.Strings.Tash_String;
      Pattern     : in String;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False) return Boolean is
   begin -- Match
      return Match (
         Source      => Source,
         Pattern     => To_Tash_Regexp (Pattern),
         Mode        => Mode,
         Expanded    => Expanded,
         Ignore_Case => Ignore_Case,
         Line        => Line,
         Line_Stop   => Line_Stop,
         Line_Anchor => Line_Anchor);
   end Match;

   function CFlags (
      Mode        : in Regexp_Mode;
      Expanded    : in Boolean;
      Ignore_Case : in Boolean;
      Line        : in Boolean;
      Line_Stop   : in Boolean;
      Line_Anchor : in Boolean) return Interfaces.C.Int is
   --
      Flags  : Interfaces.C.Int;
   begin -- CFlags
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

   function Match (
      Source      : in Tash.Strings.Tash_String;
      Pattern     : in Tash_Regexp;
      Mode        : in Regexp_Mode := Advanced;
      Expanded    : in Boolean     := False;
      Ignore_Case : in Boolean     := False;
      Line        : in Boolean     := False;
      Line_Stop   : in Boolean     := False;
      Line_Anchor : in Boolean     := False) return Boolean is
   --
      Flags  : Interfaces.C.Int;
      Regexp : Tcl.Tcl_RegExp;
      Result : Interfaces.C.Int;
      Interp : Tcl.Tcl_Interp;
   begin -- Match
      Flags := CFlags (
         Mode        => Mode,
         Expanded    => Expanded,
         Ignore_Case => Ignore_Case,
         Line        => Line,
         Line_Stop   => Line_Stop,
         Line_Anchor => Line_Anchor);
      Tash_Interp.Get (Interp);
      Regexp := Tcl.Tcl_GetRegExpFromObj (Interp, Pattern.Obj, Flags);
      if Regexp = Null then
         Tash_Interp.Raise_Exception (Interp, Regexp_Error'Identity);
      end if;
      Result := Tcl.Tcl_RegExpExecObj (
         Interp, Regexp, Source.Obj, 0, 0, 0);
      if Result < 0 then
         Tash_Interp.Raise_Exception (Interp, Regexp_Error'Identity);
      end if;
      Tash_Interp.Release (Interp);
      return Result = 1;
   end Match;

   function Match_Element (
      Source      : in Tash.Lists.Tash_List;
      Pattern     : in String;
      Going       : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mode        : in Regexp_Mode           := Advanced;
      Expanded    : in Boolean               := False;
      Ignore_Case : in Boolean               := False;
      Line        : in Boolean               := False;
      Line_Stop   : in Boolean               := False;
      Line_Anchor : in Boolean               := False) return Natural is
   begin -- Match_Element
      return Match_Element (
         Source      => Source,
         Pattern     => To_Tash_Regexp (Pattern),
         Going       => Going,
         Mode        => Mode,
         Expanded    => Expanded,
         Ignore_Case => Ignore_Case,
         Line        => Line,
         Line_Stop   => Line_Stop,
         Line_Anchor => Line_Anchor);
   end Match_Element;

   function Match_Element (
      Source      : in Tash.Lists.Tash_List;
      Pattern     : in Tash_Regexp;
      Going       : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mode        : in Regexp_Mode           := Advanced;
      Expanded    : in Boolean               := False;
      Ignore_Case : in Boolean               := False;
      Line        : in Boolean               := False;
      Line_Stop   : in Boolean               := False;
      Line_Anchor : in Boolean               := False) return Natural is
   begin -- Match_Element
      if Going = Ada.Strings.Forward then
         for I in 1..Tash.Lists.Length (Source) loop
            if Match (Tash.Lists.Element (Source, I), Pattern,
                      Mode, Expanded, Ignore_Case, Line,
                      Line_Stop, Line_Anchor) then
               return I;
            end if;
         end loop;
      else
         for I in reverse 1..Tash.Lists.Length (Source) loop
            if Match (Tash.Lists.Element (Source, I), Pattern,
                      Mode, Expanded, Ignore_Case, Line,
                      Line_Stop, Line_Anchor) then
               return I;
            end if;
         end loop;
      end if;
      return 0;
   end Match_Element;

end Tash.Regexp;
