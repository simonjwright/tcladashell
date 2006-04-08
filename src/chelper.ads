--------------------------------------------------------------------
--
-- chelper.ads -- Provides additional C data types not in Interfaces.C.
--                Also provides for the type Interfaces.C.Strings those
--                operations available for manipulating Ada strings in
--                Ada.Strings.Fixed.
--
-- Copyright (c) 1995-1997 Terry J. Westley
--
-- See the file "license.htm" for information on usage and
-- redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with Ada.Strings.Maps;
with Interfaces.C.Strings;

package CHelper is

   package C renames Interfaces.C;

   type Int_Ptr      is access all C.Int;               -- int *
   type Int_Ptr_Ptr  is access all Int_Ptr;             -- int **
   type Long_Ptr     is access all C.Long;              -- long *
   type Double_Ptr   is access all C.Double;            -- double *
   type Int_Array    is array (C.Int range <>) of C.Int;

   function To_C (Str : in String) return C.Strings.Chars_Ptr
      renames C.Strings.New_String;

   function "&" (Left, Right : in C.Strings.Chars_Ptr) return String;

   function "&" (
      Left  : in C.Strings.Chars_Ptr;
      Right : String) return String;

   function "&" (
      Left  : String;
      Right : in C.Strings.Chars_Ptr) return String;

   function Length (Source : C.Strings.Chars_Ptr) return Natural;
   function Length (Source : C.Strings.Chars_Ptr) return C.Int;

   function Value (Item : in C.Strings.Chars_Ptr) return String;
   pragma Inline (Value);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   procedure Append
     (Source   : in out C.Strings.Chars_Ptr;
      New_Item : in C.Strings.Chars_Ptr);

   procedure Append
     (Source   : in out C.Strings.Chars_Ptr;
      New_Item : in String);

   procedure Append
     (Source   : in out C.Strings.Chars_Ptr;
      New_Item : in Character);

   function Element
     (Source : in C.Strings.Chars_Ptr;
      Index  : in Positive)
      return   Character;

   procedure Replace_Element
     (Source : in out C.Strings.Chars_Ptr;
      Index  : in Positive;
      By     : Character);

   function Slice
     (Source : in C.Strings.Chars_Ptr;
      Low    : in Positive;
      High   : in Natural)
      return   String;

   function "=" (Left, Right : in C.Strings.Chars_Ptr) return Boolean;

   function "="
     (Left  : in C.Strings.Chars_Ptr;
      Right : in String)
      return  Boolean;

   function "="
     (Left  : in String;
      Right : in C.Strings.Chars_Ptr)
      return  Boolean;

   function "<" (Left, Right : in C.Strings.Chars_Ptr) return Boolean;

   function "<"
     (Left  : in C.Strings.Chars_Ptr;
      Right : in String)
      return  Boolean;

   function "<"
     (Left  : in String;
      Right : in C.Strings.Chars_Ptr)
      return  Boolean;

   function "<=" (Left, Right : in C.Strings.Chars_Ptr) return Boolean;

   function "<="
     (Left  : in C.Strings.Chars_Ptr;
      Right : in String)
      return  Boolean;

   function "<="
     (Left  : in String;
      Right : in C.Strings.Chars_Ptr)
      return  Boolean;

   function ">" (Left, Right : in C.Strings.Chars_Ptr) return Boolean;

   function ">"
     (Left  : in C.Strings.Chars_Ptr;
      Right : in String)
      return  Boolean;

   function ">"
     (Left  : in String;
      Right : in C.Strings.Chars_Ptr)
      return  Boolean;

   function ">=" (Left, Right : in C.Strings.Chars_Ptr) return Boolean;

   function ">="
     (Left  : in C.Strings.Chars_Ptr;
      Right : in String)
      return  Boolean;

   function ">="
     (Left  : in String;
      Right : in C.Strings.Chars_Ptr)
      return  Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source   : in C.Strings.Chars_Ptr;
      Pattern  : in String;
      Going    : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping  : in Ada.Strings.Maps.Character_Mapping :=
                                             Ada.Strings.Maps.Identity)
      return     Natural;

   function Index
     (Source   : in C.Strings.Chars_Ptr;
      Pattern  : in String;
      Going    : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping  : in Ada.Strings.Maps.Character_Mapping_Function)
      return     Natural;

   function Index
     (Source : in C.Strings.Chars_Ptr;
      Set    : in Ada.Strings.Maps.Character_Set;
      Test   : in Ada.Strings.Membership := Ada.Strings.Inside;
      Going  : in Ada.Strings.Direction  := Ada.Strings.Forward)
      return   Natural;

   function Index_Non_Blank
     (Source : in C.Strings.Chars_Ptr;
      Going  : in Ada.Strings.Direction := Ada.Strings.Forward)
      return   Natural;

   function Count
     (Source  : in C.Strings.Chars_Ptr;
      Pattern : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity)
      return    Natural;

   function Count
     (Source   : in C.Strings.Chars_Ptr;
      Pattern  : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping_Function)
      return     Natural;

   function Count
     (Source : in C.Strings.Chars_Ptr;
      Set    : in Ada.Strings.Maps.Character_Set)
      return   Natural;

   procedure Find_Token
     (Source : in C.Strings.Chars_Ptr;
      Set    : in Ada.Strings.Maps.Character_Set;
      Test   : in Ada.Strings.Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : in C.Strings.Chars_Ptr;
      Mapping : in Ada.Strings.Maps.Character_Mapping)
      return    C.Strings.Chars_Ptr;

   procedure Translate
     (Source  : in out C.Strings.Chars_Ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping);

   function Translate
     (Source  : in C.Strings.Chars_Ptr;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    C.Strings.Chars_Ptr;

   procedure Translate
     (Source  : in out C.Strings.Chars_Ptr;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : in C.Strings.Chars_Ptr;
      Low    : in Positive;
      High   : in Natural;
      By     : in String)
      return   C.Strings.Chars_Ptr;

   procedure Replace_Slice
     (Source   : in out C.Strings.Chars_Ptr;
      Low      : in Positive;
      High     : in Natural;
      By       : in String);

   function Insert
     (Source   : in C.Strings.Chars_Ptr;
      Before   : in Positive;
      New_Item : in String)
      return     C.Strings.Chars_Ptr;

   procedure Insert
     (Source   : in out C.Strings.Chars_Ptr;
      Before   : in Positive;
      New_Item : in String);

   function Overwrite
     (Source   : in C.Strings.Chars_Ptr;
      Position : in Positive;
      New_Item : in String)
      return     C.Strings.Chars_Ptr;

   procedure Overwrite
     (Source    : in out C.Strings.Chars_Ptr;
      Position  : in Positive;
      New_Item  : in String);

   function Delete
     (Source  : in C.Strings.Chars_Ptr;
      From    : in Positive;
      Through : in Natural)
      return    C.Strings.Chars_Ptr;

   procedure Delete
     (Source  : in out C.Strings.Chars_Ptr;
      From    : in Positive;
      Through : in Natural);

   function Trim
     (Source : in C.Strings.Chars_Ptr;
      Side   : in Ada.Strings.Trim_End)
      return   C.Strings.Chars_Ptr;

   procedure Trim
     (Source : in out C.Strings.Chars_Ptr;
      Side   : in Ada.Strings.Trim_End);

   function Trim
     (Source : in C.Strings.Chars_Ptr;
      Left   : in Ada.Strings.Maps.Character_Set;
      Right  : in Ada.Strings.Maps.Character_Set)
      return   C.Strings.Chars_Ptr;

   procedure Trim
     (Source : in out C.Strings.Chars_Ptr;
      Left   : in Ada.Strings.Maps.Character_Set;
      Right  : in Ada.Strings.Maps.Character_Set);

   function Head
     (Source : in C.Strings.Chars_Ptr;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space)
      return   C.Strings.Chars_Ptr;

   procedure Head
     (Source : in out C.Strings.Chars_Ptr;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space);

   function Tail
     (Source : in C.Strings.Chars_Ptr;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space)
      return   C.Strings.Chars_Ptr;

   procedure Tail
     (Source : in out C.Strings.Chars_Ptr;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space);

   function "*"
     (Left  : in Natural;
      Right : in Character)
      return  C.Strings.Chars_Ptr;

   function "*"
     (Left  : in Natural;
      Right : in String)
      return  C.Strings.Chars_Ptr;

   function "*"
     (Left  : in Natural;
      Right : in C.Strings.Chars_Ptr)
      return  C.Strings.Chars_Ptr;

end CHelper;
