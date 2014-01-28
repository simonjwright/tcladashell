--------------------------------------------------------------------
--
--  Unit Name:    Tash.Strings spec
--
--  File Name:    tash-strings.ads
--
--  Purpose:      Defines the Tash string type and provides
--               operations supporting string manipulation
--               in the style of Ada.Strings packages.
--
--  Copyright (c) 1999 Terry J. Westley
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
--
--  For those more familiar with Tcl string handling functions than
--  the style of Ada.Strings packages, here is a translation table:
--
--    Tcl command               Ada subprogram
--    -----------               --------------
--    format                    Tash.Lists.Format
--    regexp                    Tash.Regexp
--    regsub                    Regsub
--    scan                      Scan
--    string compare            "=", "<", "<=", ">", ">="
--    string first              Index
--    string index              Element
--    string last
--    string length             Length
--    string match              Match
--    string range              Slice
--    string tolower
--    string toupper
--    string trim               Trim
--    string trimleft           Trim
--    string trimright          Trim
--
--------------------------------------------------------------------

with Ada.Strings.Maps;

package Tash.Strings is

   --------------------------------------------------------
   --  A Tash string is derived from a Tash object.
   --
   --  When used in an expression, an uninitialized Tash
   --  string evaluates to an empty string.
   --------------------------------------------------------

   type Tash_String is new Tash.Tash_Object with null record;

   Null_Tash_String : constant Tash_String;

   -----------------------------------------------
   --  Inherits Is_Null function from Tash.  It
   --  returns True if Obj has not been initialized
   --  or has been set to Null_Tash_String.
   -----------------------------------------------

   --  function Is_Null (
   --   Obj : in Tash_String) return Boolean;

   -----------------------------------------------
   --  Returns True if Is_Null(TString) is true or
   --  Length(TString) = 0.
   -----------------------------------------------

   function Is_Empty (TString : in Tash_String) return Boolean;

   -----------------------------------------------
   --  Convert an Ada string to a Tash string
   -----------------------------------------------

   function To_Tash_String (Str : in String) return Tash_String;

   function "+" (Str : in String) return Tash_String;

   --------------------------------------------------------
   --  Convert a Tash string to a string.  Returns an emtpy
   --  string for an uninitialized Tash_String.
   --------------------------------------------------------

   function To_String (TString : in Tash_String) return String;

   --------------------------------------------------------
   --  Get the length of a Tash string.
   --------------------------------------------------------

   function Length (TString : in Tash_String) return Natural;

   --------------------------------------------------------
   --  Append to a Tash string.
   --------------------------------------------------------

   procedure Append
     (TString  : in out Tash_String;
      New_Item : in Tash_String);

   procedure Append (TString : in out Tash_String; New_Item : in String);

   --------------------------------------------------------
   --  Concatenate strings
   --------------------------------------------------------

   function "&"
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Tash_String;

   function "&" (Left : in Tash_String; Right : String) return Tash_String;

   function "&" (Left : String; Right : in Tash_String) return Tash_String;

   --------------------------------------------------------
   --  Get the Index'th character element from a Tash string
   --------------------------------------------------------

   function Element
     (TString : in Tash_String;
      Index   : in Positive)
      return    Character;

   --------------------------------------------------------
   --  Replace the Index'th character element in a Tash string
   --------------------------------------------------------

   procedure Replace_Element
     (TString : in Tash_String;
      Index   : in Positive;
      By      : in Character);

   --------------------------------------------------------
   --  Get the slice in Low..High from a Tash string
   --------------------------------------------------------

   function Slice
     (TString : in Tash_String;
      Low     : in Positive;
      High    : in Natural)
      return    String;

   --------------------------------------------------------
   --  Compare Tash and standard Ada strings
   --------------------------------------------------------

   function "="
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean;

   function "=" (Left : in Tash_String; Right : in String) return Boolean;

   function "=" (Left : in String; Right : in Tash_String) return Boolean;

   function "<"
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean;

   function "<" (Left : in Tash_String; Right : in String) return Boolean;

   function "<" (Left : in String; Right : in Tash_String) return Boolean;

   function "<="
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean;

   function "<=" (Left : in Tash_String; Right : in String) return Boolean;

   function "<=" (Left : in String; Right : in Tash_String) return Boolean;

   function ">"
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean;

   function ">" (Left : in Tash_String; Right : in String) return Boolean;

   function ">" (Left : in String; Right : in Tash_String) return Boolean;

   function ">="
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean;

   function ">=" (Left : in Tash_String; Right : in String) return Boolean;

   function ">=" (Left : in String; Right : in Tash_String) return Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (TString : in Tash_String;
      Pattern : in String;
      Going   : in Ada.Strings.Direction              := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.Identity)
      return    Natural;

   function Index
     (TString : in Tash_String;
      Pattern : in String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural;

   function Index
     (TString : in Tash_String;
      Set     : in Ada.Strings.Maps.Character_Set;
      Test    : in Ada.Strings.Membership := Ada.Strings.Inside;
      Going   : in Ada.Strings.Direction  := Ada.Strings.Forward)
      return    Natural;

   function Index_Non_Blank
     (TString : in Tash_String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward)
      return    Natural;

   function Count
     (TString : in Tash_String;
      Pattern : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.Identity)
      return    Natural;

   function Count
     (TString : in Tash_String;
      Pattern : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural;

   function Count
     (TString : in Tash_String;
      Set     : in Ada.Strings.Maps.Character_Set)
      return    Natural;

   procedure Find_Token
     (TString : in Tash_String;
      Set     : in Ada.Strings.Maps.Character_Set;
      Test    : in Ada.Strings.Membership;
      First   : out Positive;
      Last    : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (TString : in Tash_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping)
      return    Tash_String;

   procedure Translate
     (TString : in out Tash_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping);

   function Translate
     (TString : in Tash_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    Tash_String;

   procedure Translate
     (TString : in out Tash_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (TString : in Tash_String;
      Low     : in Positive;
      High    : in Natural;
      By      : in String)
      return    Tash_String;

   procedure Replace_Slice
     (TString : in out Tash_String;
      Low     : in Positive;
      High    : in Natural;
      By      : in String);

   function Insert
     (TString  : in Tash_String;
      Before   : in Positive;
      New_Item : in String)
      return     Tash_String;

   procedure Insert
     (TString  : in out Tash_String;
      Before   : in Positive;
      New_Item : in String);

   function Overwrite
     (TString  : in Tash_String;
      Position : in Positive;
      New_Item : in String)
      return     Tash_String;

   procedure Overwrite
     (TString  : in out Tash_String;
      Position : in Positive;
      New_Item : in String);

   function Delete
     (TString : in Tash_String;
      From    : in Positive;
      Through : in Natural)
      return    Tash_String;

   procedure Delete
     (TString : in out Tash_String;
      From    : in Positive;
      Through : in Natural);

   function Trim
     (TString : in Tash_String;
      Side    : in Ada.Strings.Trim_End)
      return    Tash_String;

   procedure Trim
     (TString : in out Tash_String;
      Side    : in Ada.Strings.Trim_End);

   function Trim
     (TString : in Tash_String;
      Left    : in Ada.Strings.Maps.Character_Set;
      Right   : in Ada.Strings.Maps.Character_Set)
      return    Tash_String;

   procedure Trim
     (TString : in out Tash_String;
      Left    : in Ada.Strings.Maps.Character_Set;
      Right   : in Ada.Strings.Maps.Character_Set);

   function Head
     (TString : in Tash_String;
      Count   : in Natural;
      Pad     : in Character := Ada.Strings.Space)
      return    Tash_String;

   procedure Head
     (TString : in out Tash_String;
      Count   : in Natural;
      Pad     : in Character := Ada.Strings.Space);

   function Tail
     (TString : in Tash_String;
      Count   : in Natural;
      Pad     : in Character := Ada.Strings.Space)
      return    Tash_String;

   procedure Tail
     (TString : in out Tash_String;
      Count   : in Natural;
      Pad     : in Character := Ada.Strings.Space);

   function "*"
     (Left  : in Natural;
      Right : in Character)
      return  Tash_String;

   function "*" (Left : in Natural; Right : in String) return Tash_String;

   function "*"
     (Left  : in Natural;
      Right : in Tash_String)
      return  Tash_String;

private

   Null_Tash_String : constant Tash_String := (Ada.Finalization.Controlled
                                               with Obj => null);

   Verbose : Boolean := False;

end Tash.Strings;
