--------------------------------------------------------------------
--
-- chelper.adb --
--
--  Copyright (c) 1995-2000 Terry J. Westley
--  Copyright (c) 2006-2020 Simon Wright <simon@pushface.org>
--
--  See the file "license.htm" for information on usage and
--  redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
--
--------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

package body CHelper is

   package ASF renames Ada.Strings.Fixed;

   use type C.int;
   use type C.size_t;

   function Value (Item : C.Strings.chars_ptr) return String is
   begin --  Value
      --  We use fully qualified C.Strings."=" to avoid recursive
      --  call that would result from calling CHelper."=".
      if C.Strings. "=" (Item, C.Strings.Null_Ptr) then
         return "";
      else
         return C.Strings.Value (Item);
      end if;
   end Value;

   function "&" (Left, Right : C.Strings.chars_ptr) return String is
   begin --  "&"
      return Value (Left) & Value (Right);
   end "&";

   function "&"
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  String
   is
   begin --  "&"
      return Value (Left) & Right;
   end "&";

   function "&"
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  String
   is
   begin --  "&"
      return Left & Value (Right);
   end "&";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : C.Strings.chars_ptr) return Boolean is
   begin --  "="
      return Value (Left) = Value (Right);
   end "=";

   function "="
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean
   is
   begin --  "="
      return Value (Left) = Right;
   end "=";

   function "="
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean
   is
   begin --  "="
      return Left = Value (Right);
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : C.Strings.chars_ptr) return Boolean is
   begin --  "<"
      return Value (Left) < Value (Right);
   end "<";

   function "<"
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean
   is
   begin --  "<"
      return Value (Left) < Right;
   end "<";

   function "<"
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean
   is
   begin --  "<"
      return Left < Value (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : C.Strings.chars_ptr) return Boolean is
   begin --  "<="
      return Value (Left) <= Value (Right);
   end "<=";

   function "<="
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean
   is
   begin --  "<="
      return Value (Left) <= Right;
   end "<=";

   function "<="
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean
   is
   begin --  "<="
      return Left <= Value (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : C.Strings.chars_ptr) return Boolean is
   begin --  ">"
      return Value (Left) > Value (Right);
   end ">";

   function ">"
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean
   is
   begin --  ">"
      return Value (Left) > Right;
   end ">";

   function ">"
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean
   is
   begin --  ">"
      return Left > Value (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : C.Strings.chars_ptr) return Boolean is
   begin --  ">="
      return Value (Left) >= Value (Right);
   end ">=";

   function ">="
     (Left  : C.Strings.chars_ptr;
      Right : String)
      return  Boolean
   is
   begin --  ">="
      return Value (Left) >= Right;
   end ">=";

   function ">="
     (Left  : String;
      Right : C.Strings.chars_ptr)
      return  Boolean
   is
   begin --  ">="
      return Left >= Value (Right);
   end ">=";

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Character)
      return  C.Strings.chars_ptr
   is
      Result : constant C.Strings.char_array_access :=
         new C.char_array (1 .. C.size_t (Left + 1));
   begin --  "*"
      Result.all := C.To_C (ASF. "*" (Left, Right), Append_Nul => True);
      return C.Strings.To_Chars_Ptr (Result);
   end "*";

   function "*" (Left : Natural; Right : String) return C.Strings.chars_ptr is
      Result : constant C.Strings.char_array_access :=
         new C.char_array (1 .. C.size_t (Left * Right'Length + 1));
   begin --  "*"
      Result.all := C.To_C (ASF. "*" (Left, Right), Append_Nul => True);
      return C.Strings.To_Chars_Ptr (Result);
   end "*";

   function "*"
     (Left  : Natural;
      Right : C.Strings.chars_ptr)
      return  C.Strings.chars_ptr
   is
      Result : constant C.Strings.char_array_access :=
         new C.char_array (1 .. C.size_t (Left * Length (Right) + 1));
   begin --  "*"
      Result.all :=
         C.To_C (ASF. "*" (Left, Value (Right)), Append_Nul => True);
      return C.Strings.To_Chars_Ptr (Result);
   end "*";

   -----------
   -- Count --
   -----------

   function Count
     (Source  : C.Strings.chars_ptr;
      Pattern : String;
      Mapping : Ada.Strings.Maps.Character_Mapping :=
        Ada.Strings.Maps.Identity)
      return    Natural
   is
   begin --  Count
      return Ada.Strings.Fixed.Count (Value (Source), Pattern, Mapping);
   end Count;

   function Count
     (Source  : C.Strings.chars_ptr;
      Pattern : String;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural
   is
   begin --  Count
      return Ada.Strings.Fixed.Count (Value (Source), Pattern, Mapping);
   end Count;

   function Count
     (Source : C.Strings.chars_ptr;
      Set    : Ada.Strings.Maps.Character_Set)
      return   Natural
   is
   begin --  Count
      return Ada.Strings.Fixed.Count (Value (Source), Set);
   end Count;

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : C.Strings.chars_ptr;
      From    : Positive;
      Through : Natural)
      return    C.Strings.chars_ptr
   is
   begin --  Delete
      return C.Strings.New_String
               (Ada.Strings.Fixed.Delete (Value (Source), From, Through));
   end Delete;

   procedure Delete
     (Source  : in out C.Strings.chars_ptr;
      From    : Positive;
      Through : Natural)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Delete
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Delete (Value (Source), From, Through));
      C.Strings.Free (Temp);
   end Delete;

   -------------
   -- Element --
   -------------

   function Element
     (Source : C.Strings.chars_ptr;
      Index  : Positive)
      return   Character
   is
   begin --  Element
      if Index <= Length (Source) then
         return Value (Source) (Index);
      else
         raise Ada.Strings.Index_Error;
      end if;
   end Element;

   ----------------
   -- Find_Token --
   ----------------

   procedure Find_Token
     (Source : C.Strings.chars_ptr;
      Set    : Ada.Strings.Maps.Character_Set;
      Test   : Ada.Strings.Membership;
      First  : out Positive;
      Last   : out Natural)
   is
   begin --  Find_Token
      Ada.Strings.Fixed.Find_Token (Value (Source), Set, Test, First, Last);
   end Find_Token;

   ----------
   -- Head --
   ----------

   function Head
     (Source : C.Strings.chars_ptr;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space)
      return   C.Strings.chars_ptr
   is
   begin --  Head
      return C.Strings.New_String
               (Ada.Strings.Fixed.Head (Value (Source), Count, Pad));
   end Head;

   procedure Head
     (Source : in out C.Strings.chars_ptr;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Head
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Head (Value (Source), Count, Pad));
      C.Strings.Free (Temp);
   end Head;

   -----------
   -- Index --
   -----------

   function Index
     (Source  : C.Strings.chars_ptr;
      Pattern : String;
      Going   : Ada.Strings.Direction              := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping :=
        Ada.Strings.Maps.Identity)
      return    Natural
   is
   begin --  Index
      return Ada.Strings.Fixed.Index
               (Value (Source),
                Pattern,
                Going,
                Mapping);
   end Index;

   function Index
     (Source  : C.Strings.chars_ptr;
      Pattern : String;
      Going   : Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
      return    Natural
   is
   begin --  Index
      return Ada.Strings.Fixed.Index
               (Value (Source),
                Pattern,
                Going,
                Mapping);
   end Index;

   function Index
     (Source : C.Strings.chars_ptr;
      Set    : Ada.Strings.Maps.Character_Set;
      Test   : Ada.Strings.Membership := Ada.Strings.Inside;
      Going  : Ada.Strings.Direction  := Ada.Strings.Forward)
      return   Natural
   is
   begin --  Index
      return Ada.Strings.Fixed.Index (Value (Source), Set, Test, Going);
   end Index;

   function Index_Non_Blank
     (Source : C.Strings.chars_ptr;
      Going  : Ada.Strings.Direction := Ada.Strings.Forward)
      return   Natural
   is
   begin --  Index_Non_Blank
      return Ada.Strings.Fixed.Index_Non_Blank (Value (Source), Going);
   end Index_Non_Blank;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : C.Strings.chars_ptr;
      Before   : Positive;
      New_Item : String)
      return     C.Strings.chars_ptr
   is
   begin --  Insert
      return C.Strings.New_String
               (Ada.Strings.Fixed.Insert (Value (Source), Before, New_Item));
   end Insert;

   procedure Insert
     (Source   : in out C.Strings.chars_ptr;
      Before   : Positive;
      New_Item : String)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Insert
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Insert (Value (Source), Before, New_Item));
      C.Strings.Free (Temp);
   end Insert;

   ------------
   -- Length --
   ------------

   function Length (Source : C.Strings.chars_ptr) return Natural is
   begin --  Length
      return Natural (C.Strings.Strlen (Source));
   end Length;

   function Length (Source : C.Strings.chars_ptr) return C.int is
   begin --  Length
      return C.int (C.Strings.Strlen (Source));
   end Length;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : C.Strings.chars_ptr;
      Position : Positive;
      New_Item : String)
      return     C.Strings.chars_ptr
   is
   begin --  Overwrite
      return C.Strings.New_String
               (Ada.Strings.Fixed.Overwrite
                   (Value (Source),
                    Position,
                    New_Item));
   end Overwrite;

   procedure Overwrite
     (Source   : in out C.Strings.chars_ptr;
      Position : Positive;
      New_Item : String)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Overwrite
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Overwrite (Value (Source), Position, New_Item));
      C.Strings.Free (Temp);
   end Overwrite;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Source : in out C.Strings.chars_ptr;
      Index  : Positive;
      By     : Character)
   is
   begin --  Replace_Element
      if Index <= Length (Source) then
         C.Strings.Update (Source, C.size_t (Index), By & "", Check => False);
      else
         raise Ada.Strings.Index_Error;
      end if;
   end Replace_Element;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source : C.Strings.chars_ptr;
      Low    : Positive;
      High   : Natural;
      By     : String)
      return   C.Strings.chars_ptr
   is
   begin --  Replace_Slice
      return C.Strings.New_String
               (Ada.Strings.Fixed.Replace_Slice
                   (Value (Source),
                    Low,
                    High,
                    By));
   end Replace_Slice;

   procedure Replace_Slice
     (Source : in out C.Strings.chars_ptr;
      Low    : Positive;
      High   : Natural;
      By     : String)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Replace_Slice
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Replace_Slice (Value (Source), Low, High, By));
      C.Strings.Free (Temp);
   end Replace_Slice;

   -----------
   -- Slice --
   -----------

   function Slice
     (Source : C.Strings.chars_ptr;
      Low    : Positive;
      High   : Natural)
      return   String
   is
      Result : String (1 .. High - Low + 1);

   begin --  Slice
      Result := Value (Source) (Low .. High);
      return Result;
   end Slice;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : C.Strings.chars_ptr;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space)
      return   C.Strings.chars_ptr
   is
   begin --  Tail
      return C.Strings.New_String
               (Ada.Strings.Fixed.Tail (Value (Source), Count, Pad));
   end Tail;

   procedure Tail
     (Source : in out C.Strings.chars_ptr;
      Count  : Natural;
      Pad    : Character := Ada.Strings.Space)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Tail
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Tail (Value (Source), Count, Pad));
      C.Strings.Free (Temp);
   end Tail;

   ------------
   -- Append --
   ------------

   procedure Append
     (Source   : in out C.Strings.chars_ptr;
      New_Item : C.Strings.chars_ptr)
   is
      S_Length : constant C.size_t           := C.Strings.Strlen (Source);
      T_Length : constant C.size_t           :=
         S_Length + C.Strings.Strlen (New_Item);
      Result : constant C.Strings.char_array_access :=
         new C.char_array (1 .. T_Length + 1);
   begin --  Append
      Result.all (1 .. S_Length + 1)            := C.Strings.Value (Source);
      Result.all (S_Length + 1 .. T_Length + 1) := C.Strings.Value (New_Item);
      C.Strings.Free (Source);
      Source := C.Strings.To_Chars_Ptr (Result);
   end Append;

   procedure Append
     (Source   : in out C.Strings.chars_ptr;
      New_Item : String)
   is
      S_Length : constant C.size_t           := C.Strings.Strlen (Source);
      Length   : constant C.size_t           := S_Length + New_Item'Length;
      Result   : constant C.Strings.char_array_access :=
         new C.char_array (1 .. Length + 1);
   begin --  Append
      Result.all (1 .. S_Length + 1)          := C.Strings.Value (Source);
      Result.all (S_Length + 1 .. Length + 1) := C.To_C (New_Item);
      C.Strings.Free (Source);
      Source := C.Strings.To_Chars_Ptr (Result);
   end Append;

   procedure Append
     (Source   : in out C.Strings.chars_ptr;
      New_Item : Character)
   is
      S_Length : constant C.size_t           := C.Strings.Strlen (Source);
      Length   : constant C.size_t           := S_Length + 1;
      Result   : constant C.Strings.char_array_access :=
         new C.char_array (1 .. Length + 1);
   begin --  Append
      Result.all (1 .. S_Length + 1) := C.Strings.Value (Source);
      Result.all (Length)            := C.To_C (New_Item);
      Result.all (Length + 1)        := C.To_C (Ada.Characters.Latin_1.NUL);
      C.Strings.Free (Source);
      Source := C.Strings.To_Chars_Ptr (Result);
   end Append;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : C.Strings.chars_ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping)
      return    C.Strings.chars_ptr
   is
   begin --  Translate
      return C.Strings.New_String
               (Ada.Strings.Fixed.Translate (Value (Source), Mapping));
   end Translate;

   procedure Translate
     (Source  : in out C.Strings.chars_ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Translate
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Translate (Value (Source), Mapping));
      C.Strings.Free (Temp);
   end Translate;

   function Translate
     (Source  : C.Strings.chars_ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
      return    C.Strings.chars_ptr
   is
   begin --  Translate
      return C.Strings.New_String
               (Ada.Strings.Fixed.Translate (Value (Source), Mapping));
   end Translate;

   procedure Translate
     (Source  : in out C.Strings.chars_ptr;
      Mapping : Ada.Strings.Maps.Character_Mapping_Function)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Translate
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Translate (Value (Source), Mapping));
      C.Strings.Free (Temp);
   end Translate;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : C.Strings.chars_ptr;
      Side   : Ada.Strings.Trim_End)
      return   C.Strings.chars_ptr
   is
   begin --  Trim
      return C.Strings.New_String
               (Ada.Strings.Fixed.Trim (Value (Source), Side));
   end Trim;

   procedure Trim
     (Source : in out C.Strings.chars_ptr;
      Side   : Ada.Strings.Trim_End)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Trim
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Trim (Value (Source), Side));
      C.Strings.Free (Temp);
   end Trim;

   function Trim
     (Source : C.Strings.chars_ptr;
      Left   : Ada.Strings.Maps.Character_Set;
      Right  : Ada.Strings.Maps.Character_Set)
      return   C.Strings.chars_ptr
   is
   begin --  Trim
      return C.Strings.New_String
               (Ada.Strings.Fixed.Trim (Value (Source), Left, Right));
   end Trim;

   procedure Trim
     (Source : in out C.Strings.chars_ptr;
      Left   : Ada.Strings.Maps.Character_Set;
      Right  : Ada.Strings.Maps.Character_Set)
   is
      Temp : C.Strings.chars_ptr := Source;
   begin --  Trim
      Source :=
         C.Strings.New_String
           (Ada.Strings.Fixed.Trim (Value (Source), Left, Right));
      C.Strings.Free (Temp);
   end Trim;

end CHelper;
