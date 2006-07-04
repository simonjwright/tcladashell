--------------------------------------------------------------------
--
--  Unit Name:    Tash.Strings body
--
--  File Name:    tash-strings.adb
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

with Ada.Strings.Fixed;
with CHelper;
with Interfaces.C.Strings;

package body Tash.Strings is

   use type Interfaces.C.int;

   function To_CString
     (TString : in Tash_String)
      return    Interfaces.C.Strings.chars_ptr;
--     function "+" (TString : in Tash_String) return String;

   function Is_Empty (TString : in Tash_String) return Boolean is
   begin --  Is_Empty
      return Is_Null (TString) or else Length (TString) = 0;
   end Is_Empty;
   pragma Inline (Is_Empty);

   function To_Tash_String (Str : in String) return Tash_String is
      --
      C_TString : aliased Interfaces.C.char_array := Interfaces.C.To_C (Str);
      New_Obj   : Tcl.Tcl_Obj;
   begin --  To_Tash_String
      New_Obj :=
         Tcl.Tcl_NewStringObj
           (Interfaces.C.Strings.To_Chars_Ptr (C_TString'Unchecked_Access),
            Interfaces.C.int (Str'Length));
      Tcl.Tcl_IncrRefCount (New_Obj);
      return (Ada.Finalization.Controlled with Obj => New_Obj);
   end To_Tash_String;

   function "+" (Str : in String) return Tash_String is
   begin --  "+"
      return To_Tash_String (Str);
   end "+";
   pragma Inline ("+");

   function To_CString
     (TString : in Tash_String)
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

   function To_String (TString : in Tash_String) return String is
   begin --  To_String
      if Is_Null (TString) then
         return "";
      else
         return CHelper.Value (To_CString (TString));
      end if;
   end To_String;

--     function "+" (TString : in Tash_String) return String is
--     begin --  "+"
--        return To_String (TString);
--     end "+";

   function Length (TString : Tash_String) return Natural is
      Str    : Interfaces.C.Strings.chars_ptr;
      pragma Unreferenced (Str);
      Length : aliased Interfaces.C.int;
   begin --  Length
      if Is_Null (TString) then
         return 0;
      end if;
      Str := Tcl.Tcl_GetStringFromObj (TString.Obj, Length'Access);
      return Natural (Length);
   end Length;

   procedure Append
     (TString  : in out Tash_String;
      New_Item : in Tash_String)
   is
   begin --  Append
      if Is_Empty (New_Item) then
         return;
      end if;
      Append (TString, To_String (New_Item));
   end Append;

   procedure Append (TString : in out Tash_String; New_Item : in String) is
   begin --  Append
      if New_Item'Length = 0 then
         return;
      elsif Is_Empty (TString) then
         TString := To_Tash_String (New_Item);
         return;
      else
         declare
            C_TString : aliased Interfaces.C.char_array :=
               Interfaces.C.To_C (New_Item);
         begin
            Tcl.Tcl_AppendToObj
              (TString.Obj,
               Interfaces.C.Strings.To_Chars_Ptr (C_TString'Unchecked_Access),
               Interfaces.C.int (New_Item'Length));
         end;
      end if;
   end Append;

   function "&"
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Tash_String
   is
      --
      New_String : Tash_String;
   begin --  "&"
      if Is_Empty (Left) then
         return Right;
      elsif Is_Empty (Right) then
         return Left;
      else
         New_String := To_Tash_String (To_String (Left));
         Append (New_String, Right);
         return New_String;
      end if;
   end "&";

   function "&"
     (Left  : in Tash_String;
      Right : in String)
      return  Tash_String
   is
      --
      New_String : Tash_String;
   begin --  "&"
      if Is_Empty (Left) then
         return To_Tash_String (Right);
      elsif Right'Length = 0 then
         return To_Tash_String (To_String (Left));
      else
         New_String := To_Tash_String (To_String (Left));
         Append (New_String, Right);
         return New_String;
      end if;
   end "&";

   function "&"
     (Left  : in String;
      Right : in Tash_String)
      return  Tash_String
   is
      --
      New_String : Tash_String;
   begin --  "&"
      if Left'Length = 0 then
         return To_Tash_String (To_String (Right));
      elsif Is_Empty (Right) then
         return To_Tash_String (Left);
      else
         New_String := To_Tash_String (Left);
         Append (New_String, Right);
         return New_String;
      end if;
   end "&";

   function Element
     (TString : in Tash_String;
      Index   : in Positive)
      return    Character
   is
   begin --  Element
      return CHelper.Element (To_CString (TString), Index);
   end Element;

   procedure Replace_Element
     (TString : in Tash_String;
      Index   : in Positive;
      By      : Character)
   is
      --  XXX how can this possibly work?
--        Str : Interfaces.C.Strings.chars_ptr;
      Dodgy : exception;
   begin --  Replace_Element
      raise Dodgy;
--        CHelper.Replace_Element (Str, Index, By);
   end Replace_Element;

   function Slice
     (TString : in Tash_String;
      Low     : in Positive;
      High    : in Natural)
      return    String
   is
   begin --  Slice
      return CHelper.Slice (To_CString (TString), Low, High);
   end Slice;

   function "="
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean
   is
   begin --  "="
      return CHelper. "=" (To_CString (Left), To_CString (Right));
   end "=";

   function "=" (Left : in Tash_String; Right : in String) return Boolean is
   begin --  "="
      return CHelper. "=" (To_CString (Left), Right);
   end "=";

   function "=" (Left : in String; Right : in Tash_String) return Boolean is
   begin --  "="
      return CHelper. "=" (Left, To_CString (Right));
   end "=";

   function "<"
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean
   is
   begin --  "<"
      return CHelper. "<" (To_CString (Left), To_CString (Right));
   end "<";

   function "<" (Left : in Tash_String; Right : in String) return Boolean is
   begin --  "<"
      return CHelper. "<" (To_CString (Left), Right);
   end "<";

   function "<" (Left : in String; Right : in Tash_String) return Boolean is
   begin --  "<"
      return CHelper. "<" (Left, To_CString (Right));
   end "<";

   function "<="
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean
   is
   begin --  "<="
      return CHelper. "<=" (To_CString (Left), To_CString (Right));
   end "<=";

   function "<=" (Left : in Tash_String; Right : in String) return Boolean is
   begin --  "<="
      return CHelper. "<=" (To_CString (Left), Right);
   end "<=";

   function "<=" (Left : in String; Right : in Tash_String) return Boolean is
   begin --  "<="
      return CHelper. "<=" (Left, To_CString (Right));
   end "<=";

   function ">"
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean
   is
   begin --  ">"
      return CHelper. ">" (To_CString (Left), To_CString (Right));
   end ">";

   function ">" (Left : in Tash_String; Right : in String) return Boolean is
   begin --  ">"
      return CHelper. ">" (To_CString (Left), Right);
   end ">";

   function ">" (Left : in String; Right : in Tash_String) return Boolean is
   begin --  ">"
      return CHelper. ">" (Left, To_CString (Right));
   end ">";

   function ">="
     (Left  : in Tash_String;
      Right : in Tash_String)
      return  Boolean
   is
   begin --  ">="
      return CHelper. ">=" (To_CString (Left), To_CString (Right));
   end ">=";

   function ">=" (Left : in Tash_String; Right : in String) return Boolean is
   begin --  ">="
      return CHelper. ">=" (To_CString (Left), Right);
   end ">=";

   function ">=" (Left : in String; Right : in Tash_String) return Boolean is
   begin --  ">="
      return CHelper. ">=" (Left, To_CString (Right));
   end ">=";

   function Index
     (TString : in Tash_String;
      Pattern : in String;
      Going   : in Ada.Strings.Direction              := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping :=
        Ada.Strings.Maps.Identity)
      return    Natural
   is
   begin --  Index
      return Ada.Strings.Fixed.Index
               (Tash.Strings.To_String (TString),
                Pattern,
                Going,
                Mapping);
   end Index;

   function Index
     (TString : in Tash_String;
      Pattern : in String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural
   is
   begin --  Index
      return Ada.Strings.Fixed.Index
               (Tash.Strings.To_String (TString),
                Pattern,
                Going,
                Mapping);
   end Index;

   function Index
     (TString : in Tash_String;
      Set     : in Ada.Strings.Maps.Character_Set;
      Test    : in Ada.Strings.Membership := Ada.Strings.Inside;
      Going   : in Ada.Strings.Direction  := Ada.Strings.Forward)
      return    Natural
   is
   begin --  Index
      return Ada.Strings.Fixed.Index
               (Tash.Strings.To_String (TString),
                Set,
                Test,
                Going);
   end Index;

   function Index_Non_Blank
     (TString : in Tash_String;
      Going   : in Ada.Strings.Direction := Ada.Strings.Forward)
      return    Natural
   is
   begin --  Index_Non_Blank
      return Ada.Strings.Fixed.Index_Non_Blank
               (Tash.Strings.To_String (TString),
                Going);
   end Index_Non_Blank;

   function Count
     (TString : in Tash_String;
      Pattern : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping :=
        Ada.Strings.Maps.Identity)
      return    Natural
   is
   begin --  Count
      return Ada.Strings.Fixed.Count
               (Tash.Strings.To_String (TString),
                Pattern,
                Mapping);
   end Count;

   function Count
     (TString : in Tash_String;
      Pattern : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural
   is
   begin --  Count
      return Ada.Strings.Fixed.Count
               (Tash.Strings.To_String (TString),
                Pattern,
                Mapping);
   end Count;

   function Count
     (TString : in Tash_String;
      Set     : in Ada.Strings.Maps.Character_Set)
      return    Natural
   is
   begin --  Count
      return Ada.Strings.Fixed.Count (Tash.Strings.To_String (TString), Set);
   end Count;

   procedure Find_Token
     (TString : in Tash_String;
      Set     : in Ada.Strings.Maps.Character_Set;
      Test    : in Ada.Strings.Membership;
      First   : out Positive;
      Last    : out Natural)
   is
   begin --  Find_Token
      Ada.Strings.Fixed.Find_Token
        (Tash.Strings.To_String (TString),
         Set,
         Test,
         First,
         Last);
   end Find_Token;

   function Translate
     (TString : in Tash_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping)
      return    Tash_String
   is
   begin --  Translate
      return To_Tash_String
               (Ada.Strings.Fixed.Translate
                   (Tash.Strings.To_String (TString),
                    Mapping));
   end Translate;

   procedure Translate
     (TString : in out Tash_String;
      Mapping : Ada.Strings.Maps.Character_Mapping)
   is
   begin --  Translate
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Translate
               (Tash.Strings.To_String (TString),
                Mapping));
   end Translate;

   function Translate
     (TString : in Tash_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
      return    Tash_String
   is
   begin --  Translate
      return To_Tash_String
               (Ada.Strings.Fixed.Translate
                   (Tash.Strings.To_String (TString),
                    Mapping));
   end Translate;

   procedure Translate
     (TString : in out Tash_String;
      Mapping : in Ada.Strings.Maps.Character_Mapping_Function)
   is
   begin --  Translate
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Translate
               (Tash.Strings.To_String (TString),
                Mapping));
   end Translate;

   function Replace_Slice
     (TString : in Tash_String;
      Low     : in Positive;
      High    : in Natural;
      By      : in String)
      return    Tash_String
   is
   begin --  Replace_Slice
      return To_Tash_String
               (Ada.Strings.Fixed.Replace_Slice
                   (Tash.Strings.To_String (TString),
                    Low,
                    High,
                    By));
   end Replace_Slice;

   procedure Replace_Slice
     (TString : in out Tash_String;
      Low     : in Positive;
      High    : in Natural;
      By      : in String)
   is
   begin --  Replace_Slice
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Replace_Slice
               (Tash.Strings.To_String (TString),
                Low,
                High,
                By));
   end Replace_Slice;

   function Insert
     (TString  : in Tash_String;
      Before   : in Positive;
      New_Item : in String)
      return     Tash_String
   is
   begin --  Insert
      return To_Tash_String
               (Ada.Strings.Fixed.Insert
                   (Tash.Strings.To_String (TString),
                    Before,
                    New_Item));
   end Insert;

   procedure Insert
     (TString  : in out Tash_String;
      Before   : in Positive;
      New_Item : in String)
   is
   begin --  Insert
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Insert
               (Tash.Strings.To_String (TString),
                Before,
                New_Item));
   end Insert;

   function Overwrite
     (TString  : in Tash_String;
      Position : in Positive;
      New_Item : in String)
      return     Tash_String
   is
   begin --  Overwrite
      return To_Tash_String
               (Ada.Strings.Fixed.Overwrite
                   (Tash.Strings.To_String (TString),
                    Position,
                    New_Item));
   end Overwrite;

   procedure Overwrite
     (TString  : in out Tash_String;
      Position : in Positive;
      New_Item : in String)
   is
   begin --  Overwrite
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Overwrite
               (Tash.Strings.To_String (TString),
                Position,
                New_Item));
   end Overwrite;

   function Delete
     (TString : in Tash_String;
      From    : in Positive;
      Through : in Natural)
      return    Tash_String
   is
   begin --  Delete
      return To_Tash_String
               (Ada.Strings.Fixed.Delete
                   (Tash.Strings.To_String (TString),
                    From,
                    Through));
   end Delete;

   procedure Delete
     (TString : in out Tash_String;
      From    : in Positive;
      Through : in Natural)
   is
   begin --  Delete
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Delete
               (Tash.Strings.To_String (TString),
                From,
                Through));
   end Delete;

   function Trim
     (TString : in Tash_String;
      Side    : in Ada.Strings.Trim_End)
      return    Tash_String
   is
   begin --  Trim
      return To_Tash_String
               (Ada.Strings.Fixed.Trim
                   (Tash.Strings.To_String (TString),
                    Side));
   end Trim;

   procedure Trim
     (TString : in out Tash_String;
      Side    : in Ada.Strings.Trim_End)
   is
   begin --  Trim
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Trim (Tash.Strings.To_String (TString), Side));
   end Trim;

   function Trim
     (TString : in Tash_String;
      Left    : in Ada.Strings.Maps.Character_Set;
      Right   : in Ada.Strings.Maps.Character_Set)
      return    Tash_String
   is
   begin --  Trim
      return To_Tash_String
               (Ada.Strings.Fixed.Trim
                   (Tash.Strings.To_String (TString),
                    Left,
                    Right));
   end Trim;

   procedure Trim
     (TString : in out Tash_String;
      Left    : in Ada.Strings.Maps.Character_Set;
      Right   : in Ada.Strings.Maps.Character_Set)
   is
   begin --  Trim
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Trim
               (Tash.Strings.To_String (TString),
                Left,
                Right));
   end Trim;

   function Head
     (TString : in Tash_String;
      Count   : in Natural;
      Pad     : in Character := Ada.Strings.Space)
      return    Tash_String
   is
   begin --  Head
      return To_Tash_String
               (Ada.Strings.Fixed.Head
                   (Tash.Strings.To_String (TString),
                    Count,
                    Pad));
   end Head;

   procedure Head
     (TString : in out Tash_String;
      Count   : in Natural;
      Pad     : in Character := Ada.Strings.Space)
   is
   begin --  Head
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Head
               (Tash.Strings.To_String (TString),
                Count,
                Pad));
   end Head;

   function Tail
     (TString : in Tash_String;
      Count   : in Natural;
      Pad     : in Character := Ada.Strings.Space)
      return    Tash_String
   is
   begin --  Tail
      return To_Tash_String
               (Ada.Strings.Fixed.Tail
                   (Tash.Strings.To_String (TString),
                    Count,
                    Pad));
   end Tail;

   procedure Tail
     (TString : in out Tash_String;
      Count   : in Natural;
      Pad     : in Character := Ada.Strings.Space)
   is
   begin --  Tail
      TString :=
         To_Tash_String
           (Ada.Strings.Fixed.Tail
               (Tash.Strings.To_String (TString),
                Count,
                Pad));
   end Tail;

   function "*"
     (Left  : in Natural;
      Right : in Character)
      return  Tash_String
   is
   begin
      return To_Tash_String (Ada.Strings.Fixed. "*" (Left, Right));
   end "*";

   function "*" (Left : in Natural; Right : in String) return Tash_String is
   begin
      return To_Tash_String (Ada.Strings.Fixed. "*" (Left, Right));
   end "*";

   function "*"
     (Left  : in Natural;
      Right : in Tash_String)
      return  Tash_String
   is
   begin
      return To_Tash_String
               (Ada.Strings.Fixed. "*" (Left, Tash.Strings.To_String (Right)));
   end "*";

end Tash.Strings;
