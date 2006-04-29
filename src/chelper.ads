--------------------------------------------------------------------
--
--  chelper.ads -- Provides additional C data types not in Interfaces.C.
--                 Also provides for the type Interfaces.C.Strings those
--                 operations available for manipulating Ada strings in
--                 Ada.Strings.Fixed.
--
--  Copyright (c) 1995-2000 Terry J. Westley
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--------------------------------------------------------------------

with Ada.Strings.Maps;
with Interfaces.C.Strings;

package CHelper is

   package C renames Interfaces.C;

   type Int_Ptr is access all C.int;               -- int *
   type Int_Ptr_Ptr is access all Int_Ptr;             -- int **
   type Long_Ptr is access all C.long;              -- long *
   type Double_Ptr is access all C.double;            -- double *
   type Int_Array is array (C.int range <>) of C.int;

   function To_C (Str : in String) return C.Strings.chars_ptr renames
     C.Strings.New_String;

   function "&" (Left, Right : in C.Strings.chars_ptr) return String;

   function "&"
     (Left  : in C.Strings.chars_ptr;
      Right : String)
      return  String;

   function "&"
     (Left  : String;
      Right : in C.Strings.chars_ptr)
      return  String;

   function Length (Source : C.Strings.chars_ptr) return Natural;
   function Length (Source : C.Strings.chars_ptr) return C.int;

   function Value (Item : in C.Strings.chars_ptr) return String;
   pragma Inline (Value);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   procedure Append
     (Source   : in out C.Strings.chars_ptr;
      New_Item : in C.Strings.chars_ptr);

   procedure Append
     (Source   : in out C.Strings.chars_ptr;
      New_Item : in String);

   procedure Append
     (Source   : in out C.Strings.chars_ptr;
      New_Item : in Character);

   function Element
     (Source : in C.Strings.chars_ptr;
      Index  : in Positive)
      return   Character;

   procedure Replace_Element
     (Source : in out C.Strings.chars_ptr;
      Index  : in Positive;
      By     : Character);

   function Slice
     (Source : in C.Strings.chars_ptr;
      Low    : in Positive;
      High   : in Natural)
      return   String;

   function "=" (Left, Right : in C.Strings.chars_ptr) return Boolean;

   function "="
     (Left  : in C.Strings.chars_ptr;
      Right : in String)
      return  Boolean;

   function "="
     (Left  : in String;
      Right : in C.Strings.chars_ptr)
      return  Boolean;

   function "<" (Left, Right : in C.Strings.chars_ptr) return Boolean;

   function "<"
     (Left  : in C.Strings.chars_ptr;
      Right : in String)
      return  Boolean;

   function "<"
     (Left  : in String;
      Right : in C.Strings.chars_ptr)
      return  Boolean;

   function "<=" (Left, Right : in C.Strings.chars_ptr) return Boolean;

   function "<="
     (Left  : in C.Strings.chars_ptr;
      Right : in String)
      return  Boolean;

   function "<="
     (Left  : in String;
      Right : in C.Strings.chars_ptr)
      return  Boolean;

   function ">" (Left, Right : in C.Strings.chars_ptr) return Boolean;

   function ">"
     (Left  : in C.Strings.chars_ptr;
      Right : in String)
      return  Boolean;

   function ">"
     (Left  : in String;
      Right : in C.Strings.chars_ptr)
      return  Boolean;

   function ">=" (Left, Right : in C.Strings.chars_ptr) return Boolean;

   function ">="
     (Left  : in C.Strings.chars_ptr;
      Right : in String)
      return  Boolean;

   function ">="
     (Left  : in String;
      Right : in C.Strings.chars_ptr)
      return  Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : in C.Strings.chars_ptr;
      Pattern : in String;
      Going   : in Ada.Strings.Direction              := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.Identity)
      return    Natural;

   function Index
     (Source  : in C.Strings.chars_ptr;
      Pattern : in String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural;

   function Index
     (Source : in C.Strings.chars_ptr;
      Set    : in Ada.Strings.Maps.Character_Set;
      Test   : in Ada.Strings.Membership := Ada.Strings.Inside;
      Going  : in Ada.Strings.Direction  := Ada.Strings.Forward)
      return   Natural;

   function Index_Non_Blank
     (Source : in C.Strings.chars_ptr;
      Going  : in Ada.Strings.Direction := Ada.Strings.Forward)
      return   Natural;

   function Count
     (Source  : in C.Strings.chars_ptr;
      Pattern : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.Identity)
      return    Natural;

   function Count
     (Source  : in C.Strings.chars_ptr;
      Pattern : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural;

   function Count
     (Source : in C.Strings.chars_ptr;
      Set    : in Ada.Strings.Maps.Character_Set)
      return   Natural;

   procedure Find_Token
     (Source : in C.Strings.chars_ptr;
      Set    : in Ada.Strings.Maps.Character_Set;
      Test   : in Ada.Strings.Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : in C.Strings.chars_ptr;
      Mapping : in Ada.Strings.Maps.Character_Mapping)
      return    C.Strings.chars_ptr;

   procedure Translate
     (Source  : in out C.Strings.chars_ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping);

   function Translate
     (Source  : in C.Strings.chars_ptr;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    C.Strings.chars_ptr;

   procedure Translate
     (Source  : in out C.Strings.chars_ptr;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : in C.Strings.chars_ptr;
      Low    : in Positive;
      High   : in Natural;
      By     : in String)
      return   C.Strings.chars_ptr;

   procedure Replace_Slice
     (Source : in out C.Strings.chars_ptr;
      Low    : in Positive;
      High   : in Natural;
      By     : in String);

   function Insert
     (Source   : in C.Strings.chars_ptr;
      Before   : in Positive;
      New_Item : in String)
      return     C.Strings.chars_ptr;

   procedure Insert
     (Source   : in out C.Strings.chars_ptr;
      Before   : in Positive;
      New_Item : in String);

   function Overwrite
     (Source   : in C.Strings.chars_ptr;
      Position : in Positive;
      New_Item : in String)
      return     C.Strings.chars_ptr;

   procedure Overwrite
     (Source   : in out C.Strings.chars_ptr;
      Position : in Positive;
      New_Item : in String);

   function Delete
     (Source  : in C.Strings.chars_ptr;
      From    : in Positive;
      Through : in Natural)
      return    C.Strings.chars_ptr;

   procedure Delete
     (Source  : in out C.Strings.chars_ptr;
      From    : in Positive;
      Through : in Natural);

   function Trim
     (Source : in C.Strings.chars_ptr;
      Side   : in Ada.Strings.Trim_End)
      return   C.Strings.chars_ptr;

   procedure Trim
     (Source : in out C.Strings.chars_ptr;
      Side   : in Ada.Strings.Trim_End);

   function Trim
     (Source : in C.Strings.chars_ptr;
      Left   : in Ada.Strings.Maps.Character_Set;
      Right  : in Ada.Strings.Maps.Character_Set)
      return   C.Strings.chars_ptr;

   procedure Trim
     (Source : in out C.Strings.chars_ptr;
      Left   : in Ada.Strings.Maps.Character_Set;
      Right  : in Ada.Strings.Maps.Character_Set);

   function Head
     (Source : in C.Strings.chars_ptr;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space)
      return   C.Strings.chars_ptr;

   procedure Head
     (Source : in out C.Strings.chars_ptr;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space);

   function Tail
     (Source : in C.Strings.chars_ptr;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space)
      return   C.Strings.chars_ptr;

   procedure Tail
     (Source : in out C.Strings.chars_ptr;
      Count  : in Natural;
      Pad    : in Character := Ada.Strings.Space);

   function "*"
     (Left  : in Natural;
      Right : in Character)
      return  C.Strings.chars_ptr;

   function "*"
     (Left  : in Natural;
      Right : in String)
      return  C.Strings.chars_ptr;

   function "*"
     (Left  : in Natural;
      Right : in C.Strings.chars_ptr)
      return  C.Strings.chars_ptr;

end CHelper;
