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

   type Int_Ptr is access all C.int;                --  int *
   type Int_Ptr_Ptr is access all Int_Ptr;              --  int **
   type Long_Ptr is access all C.long;               --  long *
   type Double_Ptr is access all C.double;             --  double *
   type Int_Array is array (C.int range <>) of C.int;

   function To_C (Str : String) return C.Strings.chars_ptr renames
     C.Strings.New_String;

   function "&" (Left, Right : C.Strings.chars_ptr) return String;

   function "&"
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  String;

   function "&"
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  String;

   function Length (Source : C.Strings.chars_ptr) return Natural;
   function Length (Source : C.Strings.chars_ptr) return C.int;

   function Value (Item : C.Strings.chars_ptr) return String;
   pragma Inline (Value);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   procedure Append
     (Source   : in out C.Strings.chars_ptr;
      New_Item : C.Strings.chars_ptr);

   procedure Append
     (Source   : in out C.Strings.chars_ptr;
      New_Item : String);

   procedure Append
     (Source   : in out C.Strings.chars_ptr;
      New_Item : Character);

   function Element
     (Source : C.Strings.chars_ptr;
      Index  : Positive)
      return   Character;

   procedure Replace_Element
     (Source : in out C.Strings.chars_ptr;
      Index  : Positive;
      By     : Character);

   function Slice
     (Source : C.Strings.chars_ptr;
      Low    : Positive;
      High   : Natural)
      return   String;

   function "=" (Left, Right : C.Strings.chars_ptr) return Boolean;

   function "="
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean;

   function "="
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean;

   function "<" (Left, Right : C.Strings.chars_ptr) return Boolean;

   function "<"
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean;

   function "<"
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean;

   function "<=" (Left, Right : C.Strings.chars_ptr) return Boolean;

   function "<="
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean;

   function "<="
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean;

   function ">" (Left, Right : C.Strings.chars_ptr) return Boolean;

   function ">"
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean;

   function ">"
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean;

   function ">=" (Left, Right : C.Strings.chars_ptr) return Boolean;

   function ">="
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean;

   function ">="
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : C.Strings.chars_ptr;
      Pattern : String;
      Going   : Ada.Strings.Direction              := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.Identity)
      return    Natural;

   function Index
     (Source  : C.Strings.chars_ptr;
      Pattern : String;
      Going   : Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural;

   function Index
     (Source : C.Strings.chars_ptr;
      Set    : Ada.Strings.Maps.Character_Set;
      Test   : Ada.Strings.Membership := Ada.Strings.Inside;
      Going  : Ada.Strings.Direction  := Ada.Strings.Forward)
      return   Natural;

   function Index_Non_Blank
     (Source : C.Strings.chars_ptr;
      Going  : Ada.Strings.Direction := Ada.Strings.Forward)
      return   Natural;

   function Count
     (Source  : C.Strings.chars_ptr;
      Pattern : String;
      Mapping : Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.Identity)
      return    Natural;

   function Count
     (Source  : C.Strings.chars_ptr;
      Pattern : String;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural;

   function Count
     (Source : C.Strings.chars_ptr;
      Set    : Ada.Strings.Maps.Character_Set)
      return   Natural;

   procedure Find_Token
     (Source : C.Strings.chars_ptr;
      Set    : Ada.Strings.Maps.Character_Set;
      Test   : Ada.Strings.Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : C.Strings.chars_ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping)
      return    C.Strings.chars_ptr;

   procedure Translate
     (Source  : in out C.Strings.chars_ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping);

   function Translate
     (Source  : C.Strings.chars_ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
      return    C.Strings.chars_ptr;

   procedure Translate
     (Source  : in out C.Strings.chars_ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : C.Strings.chars_ptr;
      Low    : Positive;
      High   : Natural;
      By     : String)
      return   C.Strings.chars_ptr;

   procedure Replace_Slice
     (Source : in out C.Strings.chars_ptr;
      Low    : Positive;
      High   : Natural;
      By     : String);

   function Insert
     (Source   : C.Strings.chars_ptr;
      Before   : Positive;
      New_Item : String)
      return     C.Strings.chars_ptr;

   procedure Insert
     (Source   : in out C.Strings.chars_ptr;
      Before   : Positive;
      New_Item : String);

   function Overwrite
     (Source   : C.Strings.chars_ptr;
      Position : Positive;
      New_Item : String)
      return     C.Strings.chars_ptr;

   procedure Overwrite
     (Source   : in out C.Strings.chars_ptr;
      Position : Positive;
      New_Item : String);

   function Delete
     (Source  : C.Strings.chars_ptr;
      From    : Positive;
      Through : Natural)
      return    C.Strings.chars_ptr;

   procedure Delete
     (Source  : in out C.Strings.chars_ptr;
      From    : Positive;
      Through : Natural);

   function Trim
     (Source : C.Strings.chars_ptr;
      Side   : Ada.Strings.Trim_End)
      return   C.Strings.chars_ptr;

   procedure Trim
     (Source : in out C.Strings.chars_ptr;
      Side   : Ada.Strings.Trim_End);

   function Trim
     (Source : C.Strings.chars_ptr;
      Left   : Ada.Strings.Maps.Character_Set;
      Right  : Ada.Strings.Maps.Character_Set)
      return   C.Strings.chars_ptr;

   procedure Trim
     (Source : in out C.Strings.chars_ptr;
      Left   : Ada.Strings.Maps.Character_Set;
      Right  : Ada.Strings.Maps.Character_Set);

   function Head
     (Source : C.Strings.chars_ptr;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space)
      return   C.Strings.chars_ptr;

   procedure Head
     (Source : in out C.Strings.chars_ptr;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space);

   function Tail
     (Source : C.Strings.chars_ptr;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space)
      return   C.Strings.chars_ptr;

   procedure Tail
     (Source : in out C.Strings.chars_ptr;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space);

   function "*"
     (Left  : Natural;
      Right : Character)
      return  C.Strings.chars_ptr;

   function "*"
     (Left  : Natural;
      Right : String)
      return  C.Strings.chars_ptr;

   function "*"
     (Left  : Natural;
      Right : C.Strings.chars_ptr)
      return  C.Strings.chars_ptr;

end CHelper;
