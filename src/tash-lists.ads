-------------------------------------------------------------------
--
--  Unit Name:   Tash.Lists spec
--
--  File Name:   tash-lists.ads
--
--  Purpose:     Defines the Tash list type and operations on it.
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
--
--        Tcl Command Cross Reference
--
--    Tcl command               TASH Ada subprogram
--    -----------               -------------------
--    format                    Format
--    lappend                   Append
--    lindex                    Element, Element_Obj
--    linsert                   Insert
--    list                      To_Tash_List
--    llength                   Length
--    lrange                    Slice
--    lreplace                  Replace_Element, Replace_Slice
--                              Delete_Element, Delete_Slice
--    lsearch                   Tash.Regexp.Match
--    lsort                     Sort
--    split                     Split
--    join                      Join
--
--        TclX Command Cross Reference
--
--    TclX command              TASH Ada subprogram
--    ------------              -------------------
--    lempty                    Is_Empty
--    lvarpop                   Head, Pop
--    lvarpush                  Push
--
--------------------------------------------------------------------

with Ada.Finalization;

package Tash.Lists is

   ----------------------------------------------------------
   --  A Tash list is composed of zero or more elements in an
   --  ordered list.  Each element may be either a scalar type
   --  (see the generics below for handling list elements of
   --  integer and float) or a non-scalar type.
   --
   --  Currently, two non-scalar element types are supported:
   --  strings and lists.  Since an element may be a Tash list,
   --  you can construct lists of lists.
   --
   --  Elements of different types may to stored in a list.
   -----------------------------------------------------------

   type Tash_List is new Tash.Tash_Object with null record;

   Null_Tash_List : constant Tash_List;

   List_Error : exception;

   -----------------------------------------------
   --  Inherits Is_Null function from Tash.  It
   --  returns True if List has not been initialized
   --  or has been set to Null_Tash_List.
   -----------------------------------------------

   --  function Is_Null (
   --   List : in Tash_List) return Boolean;

   --------------------------------------------------------
   --  Is_Empty returns True if Is_Null(List) is true or
   --  Length(List) = 0.
   --------------------------------------------------------

   function Is_Empty (List : in Tash_List) return Boolean;

   --------------------------------------------------------
   --  Create a Tash list from a string.  This breaks a string
   --  up into list.  Each element of the list is a string
   --  taken from a sequence of non-blank characters in the
   --  input string.
   --
   --  You may enclose phrases which contain blanks in a pair
   --  of braces ({}).  Each such phrase will be a single
   --  element in the returned list.  The braces will not be
   --  part of the element.
   --
   --  Raises List_Error if a string that contains an
   --  opening brace does not also containing a closing brace.
   --
   --  To escape a brace so that it is part of an element and
   --  not a marker for a start of an element, precede it with
   --  a '\' character.
   -------------------------------------------------------------

   function To_Tash_List (Str : in String) return Tash_List;

   function "+" (Str : in String) return Tash_List;

   -------------------------------------------------------------
   --  Concatenate Tash lists and strings to create a new Tash list.
   -------------------------------------------------------------

   function "&"
     (Left  : in Tash_List;
      Right : in Tash_List)
      return  Tash_List;

   function "&" (Left : in String; Right : in Tash_List) return Tash_List;

   function "&" (Left : in Tash_List; Right : in String) return Tash_List;

   --------------------------------------------------------
   --  Create a new list which is a copy of the original.
   --  The resulting list is a new list, but the elements
   --  "point" to the elements of the original list.
   --
   --  Some call this a "shallow" copy.
   --------------------------------------------------------

   function Copy (List : in Tash_List) return Tash_List;

   --------------------------------------------------------
   --  Create a new list which is a copy of the original.
   --  The resulting list is a new list AND each element
   --  is a new element.
   --
   --  Some call this a "deep" copy.
   --------------------------------------------------------

   function Duplicate (List : in Tash_List) return Tash_List;

   --------------------------------------------------------
   --  Convert a Tash list to a string.
   --------------------------------------------------------

   function To_String (List : in Tash_List) return String;

   -------------------------------------------------------------
   --  Create a Tash list by splitting the string, Str, into
   --  elements separated at each occurrence of any character in
   --  Split_At.  Adjacent Split_At characters in Str
   --  will result in the list containing empty elements.  If
   --  Split_At is an empty string, then the resulting list
   --  will consist of one element for each character in Str.
   --
   --  An example is splitting an unix path name into its
   --  components:
   --
   --  List := Split (Str      => "/usr/X/lib/X11",
   --                Split_At => "/");
   --
   --  Note that, since there is a leading split character, this
   --  call will return a five element list, of which the first
   --  is empty.  Since a empty element is represented as the
   --  string, "{}", calling Tash.To_String (List) results in
   --  the string, "{} usr X lib X11".
   -------------------------------------------------------------

   function Split
     (Str      : in String;
      Split_At : in String := " " & ASCII.CR & ASCII.HT & ASCII.LF)
      return     Tash_List;

   ----------------------------------------------------------------
   --  Join is the inverse of Split:  it creates a string by joining
   --  all the elements of a list with JoinString separating each
   --  adjacent element.
   ----------------------------------------------------------------

   function Join
     (List       : in Tash_List;
      JoinString : in String := " ")
      return       String;

   ----------------------------------------
   --  Create a new Tash list from a range
   --  of elements of an existing list.
   ----------------------------------------

   function Slice
     (List  : in Tash_List;
      First : in Positive;
      Last  : in Natural)
      return  Tash_List;

   --------------------------------------------------------
   --  Create a new Tash list by sorting the input list.
   --  Uses the Tcl lsort command which utilizes a merge sort
   --  algorithm.
   --------------------------------------------------------

   type Sort_Mode is (SM_ASCII, SM_Dictionary, SM_Integer, SM_Real);
   --  SM_ASCII Sort_Mode treats the list elements as strings
   --  and sorts them in ASCII collating sequence.  SM_Dictionary
   --  mode is similar to ASCII but ignores case except as a
   --  tie-breaker and numbers compare as integers, not characters.
   --  For example, in Dictionary mode, "bigBoy" sorts between "bigbang"
   --  and  "bigboy;"  x10y  sorts between x9y and x11y.  In SM_Integer
   --  and SM_Real mode, list elements are treated as integers and reals
   --  (Ada type Long_Float), respectively, before sorting.

   type Ordering is (Increasing, Decreasing);

   function Sort
     (List  : in Tash_List;
      Mode  : in Sort_Mode := SM_ASCII;
      Order : in Ordering  := Increasing)
      return  Tash_List;

   ----------------------------------------
   --  Get the number of elements in the list.
   --  Returns 0 if Is_Empty (List) is true.
   ----------------------------------------

   function Length (List : in Tash_List) return Natural;

   --------------------------------------------------------
   --  Get a specific list element.
   --
   --  Any list element can be represented as a string, so
   --  there is never a type mismatch when returning a list
   --  element as a string.  If you specifically need to know
   --  whether an element is a string or not, call Element_is_String.
   --
   --  The elements of a list are indexed starting at 1.
   --
   --  Raises List_Error if Index is greater than Length (List).
   --------------------------------------------------------

   function Get_Element
     (List  : in Tash_List;
      Index : in Positive)
      return  String;

   --------------------------------------------------------
   --  Returns the requested element as a list.
   --------------------------------------------------------

   function Get_Element
     (List  : in Tash_List;
      Index : in Positive)
      return  Tash_List;

   --------------------------------------------------------
   --  Find out whether a list element is a string or a list
   --------------------------------------------------------

   function Element_Is_String
     (List  : in Tash_List;
      Index : in Positive)
      return  Boolean;

   function Element_Is_List
     (List  : in Tash_List;
      Index : in Positive)
      return  Boolean;

   --------------------------------------------------------
   --  Get the first or last element of the list without
   --  modifying the list.
   --------------------------------------------------------

   function Head (List : in Tash_List) return String;

   function Head (List : in Tash_List) return Tash_List;

   function Tail (List : in Tash_List) return String;

   function Tail (List : in Tash_List) return Tash_List;

   --------------------------------------------------------
   --  Append a string as a single element to a Tash list.
   --------------------------------------------------------

   procedure Append (List : in out Tash_List; Element : in String);

   --------------------------------------------------------
   --  Append the elements of one list to another.
   --  The length of the resulting list is equal to
   --  Length (List) + Length (Elements).
   --------------------------------------------------------

   procedure Append_Elements
     (List     : in out Tash_List;
      Elements : in Tash_List);

   --------------------------------------------------------
   --  Append a list as an element of another list.
   --  The length of the resulting list is equal to
   --  Length (List) + 1.
   --------------------------------------------------------

   procedure Append_List (List : in out Tash_List; Element : in Tash_List);

   --------------------------------------------------------
   --  Replace the Index'th element in a Tash list with a
   --  string or a list or the elements of a list.
   --------------------------------------------------------

   procedure Replace_Element
     (List    : in out Tash_List;
      Index   : in Positive;
      Element : in String);

   procedure Replace_Element_With_List
     (List    : in out Tash_List;
      Index   : in Positive;
      Element : in Tash_List);

   procedure Replace_Element_With_Elements
     (List     : in out Tash_List;
      Index    : in Positive;
      Elements : in Tash_List);

   --------------------------------------------------------
   --  Replace the slice From..to of a Tash list with a
   --  string or a list or the elements of a list.
   --
   --  If To < From, has the effect of inserting Element
   --  in List before From.
   --------------------------------------------------------

   procedure Replace_Slice
     (List    : in out Tash_List;
      From    : in Positive;
      To      : in Natural;
      Element : in String);

   procedure Replace_Slice_With_List
     (List    : in out Tash_List;
      From    : in Positive;
      To      : in Natural;
      Element : in Tash_List);

   procedure Replace_Slice_With_Elements
     (List     : in out Tash_List;
      From     : in Positive;
      To       : in Natural;
      Elements : in Tash_List);

   --------------------------------------------------------
   --  Insert a string or a list or the elements of a list
   --  into a Tash list before the Index'th element.
   --------------------------------------------------------

   procedure Insert
     (List    : in out Tash_List;
      Index   : in Positive;
      Element : in String);

   procedure Insert_List
     (List    : in out Tash_List;
      Index   : in Positive;
      Element : in Tash_List);

   procedure Insert_Elements
     (List     : in out Tash_List;
      Index    : in Positive;
      Elements : in Tash_List);

   --------------------------------------------------------
   --  Deletes an element or slice from a Tash list.
   --  Has no effect in any of the following situations:
   --   1) List is null or empty
   --   2) Length is less than Index or From
   --   3) To is less than From
   --------------------------------------------------------

   procedure Delete_Element (List : in out Tash_List; Index : in Positive);

   procedure Delete_Slice
     (List : in out Tash_List;
      From : in Positive;
      To   : in Natural);

   --------------------------------------------------------
   --  Push and Pop treat a Tash list as a stack.
   --------------------------------------------------------
   --  Push inserts a string or a list or the elements of a
   --  list before the first element of List.
   --  Pop removes the first element and "throws it away."
   --  It is identical to Delete_Element with Index = 1.
   --------------------------------------------------------

   procedure Push (List : in out Tash_List; Element : in String);

   procedure Push_List (List : in out Tash_List; Element : in Tash_List);

   procedure Push_Elements
     (List     : in out Tash_List;
      Elements : in Tash_List);

   procedure Pop (List : in out Tash_List);

   --------------------------------------------------------
   --  Generic for handling Integer list elements
   --------------------------------------------------------

   generic
      type Item is range <>;
   package Generic_Integer_Lists is

      --------------------------------------------------------
      --  Create a Tash list with one element, an integer.
      --------------------------------------------------------

      function To_Tash_List (Num : in Item) return Tash_List;

      function "+" (Num : in Item) return Tash_List;

      function "-" (Num : in Item) return Tash_List;

      -----------------------------------------------------------------
      --  Concatenate Tash lists and integers to create a new Tash list.
      -----------------------------------------------------------------

      function "&" (Left : in Item; Right : in Tash_List) return Tash_List;

      function "&" (Left : in Tash_List; Right : in Item) return Tash_List;

      --------------------------------------------------------
      --  Get a specific list element.
      --
      --  Raises List_Error if Index is greater than Length (List).
      --
      --  Raises Constraint_Error if the Index'th element of the
      --  list does not match the return type.
      --------------------------------------------------------

      function Get_Element
        (List  : in Tash_List;
         Index : in Positive)
         return  Item;

      --------------------------------------------------------
      --  Find out whether a list element is an integer
      --------------------------------------------------------

      function Element_Is_Integer
        (List  : in Tash_List;
         Index : in Positive)
         return  Boolean;

      --------------------------------------------------------
      --  Get the first or last element of the list without
      --  modifying the list.
      --------------------------------------------------------

      function Head (List : in Tash_List) return Item;

      function Tail (List : in Tash_List) return Item;

      --------------------------------------------------------
      --  Append an integer as a single element to a Tash list.
      --
      --  Raises List_Error when List to be appended to
      --  is shared (referenced by another list).
      --------------------------------------------------------

      procedure Append (List : in out Tash_List; Element : in Item);

      ---------------------------------------------------------------
      --  Replace the Index'th element in a Tash list with an integer.
      ---------------------------------------------------------------

      procedure Replace_Element
        (List    : in out Tash_List;
         Index   : in Positive;
         Element : in Item);

      -------------------------------------------------------------
      --  Replace the slice From..to of a Tash list with an integer.
      --
      --  If To < From, has the effect of inserting Element
      --  in List before From.
      -------------------------------------------------------------

      procedure Replace_Slice
        (List    : in out Tash_List;
         From    : in Positive;
         To      : in Natural;
         Element : in Item);

      -----------------------------------------------------------------
      --  Insert an integer into a Tash list before the Index'th element.
      -----------------------------------------------------------------

      procedure Insert
        (List    : in out Tash_List;
         Index   : in Positive;
         Element : in Item);

      ------------------------------------------------------------
      --  Push inserts an integer before the first element of List.
      ------------------------------------------------------------

      procedure Push (List : in out Tash_List; Element : in Item);

   end Generic_Integer_Lists;

   --------------------------------------------------------
   --  Generic for handling float list elements
   --------------------------------------------------------

   generic
      type Item is digits <>;
   package Generic_Float_Lists is

      --------------------------------------------------------
      --  Create a Tash list with one element, a float.
      --------------------------------------------------------

      function To_Tash_List (Num : in Item) return Tash_List;

      function "+" (Num : in Item) return Tash_List;

      function "-" (Num : in Item) return Tash_List;

      -----------------------------------------------------------------
      --  Concatenate Tash lists and floats to create a new Tash list.
      -----------------------------------------------------------------

      function "&" (Left : in Item; Right : in Tash_List) return Tash_List;

      function "&" (Left : in Tash_List; Right : in Item) return Tash_List;

      --------------------------------------------------------
      --  Get a specific list element.
      --
      --  Raises List_Error if Index is greater than Length (List).
      --
      --  Raises Constraint_Error if the Index'th element of the
      --  list does not match the return type.
      --------------------------------------------------------

      function Get_Element
        (List  : in Tash_List;
         Index : in Positive)
         return  Item;

      --------------------------------------------------------
      --  Find out whether a list element is a float
      --------------------------------------------------------

      function Element_Is_Float
        (List  : in Tash_List;
         Index : in Positive)
         return  Boolean;

      --------------------------------------------------------
      --  Get the first or last element of the list without
      --  modifying the list.
      --------------------------------------------------------

      function Head (List : in Tash_List) return Item;

      function Tail (List : in Tash_List) return Item;

      --------------------------------------------------------
      --  Append a float as a single element to a Tash list.
      --
      --  Raises List_Error when List to be appended to
      --  is shared (referenced by another list).
      --------------------------------------------------------

      procedure Append (List : in out Tash_List; Element : in Item);

      ---------------------------------------------------------------
      --  Replace the Index'th element in a Tash list with a float.
      ---------------------------------------------------------------

      procedure Replace_Element
        (List    : in out Tash_List;
         Index   : in Positive;
         Element : in Item);

      -------------------------------------------------------------
      --  Replace the slice From..to of a Tash list with a float.
      --
      --  If To < From, has the effect of inserting Element
      --  in List before From.
      -------------------------------------------------------------

      procedure Replace_Slice
        (List    : in out Tash_List;
         From    : in Positive;
         To      : in Natural;
         Element : in Item);

      -----------------------------------------------------------------
      --  Insert a float into a Tash list before the Index'th element.
      -----------------------------------------------------------------

      procedure Insert
        (List    : in out Tash_List;
         Index   : in Positive;
         Element : in Item);

      ------------------------------------------------------------
      --  Push inserts a float before the first element of List.
      ------------------------------------------------------------

      procedure Push (List : in out Tash_List; Element : in Item);

   end Generic_Float_Lists;

   ----------------------------------------------------------
   --  Compare string representations of Tash lists.  Uses the
   --  same collating sequence as comparing two Ada strings.
   ----------------------------------------------------------

   function "=" (Left : in Tash_List; Right : in Tash_List) return Boolean;

   function "<" (Left : in Tash_List; Right : in Tash_List) return Boolean;

   function "<=" (Left : in Tash_List; Right : in Tash_List) return Boolean;

   function ">" (Left : in Tash_List; Right : in Tash_List) return Boolean;

   function ">=" (Left : in Tash_List; Right : in Tash_List) return Boolean;

   --------------------------------------------------------
   --  Replace % sequences in formatString with corresponding
   --  Values.  This is similar to C's printf.
   --
   --  Raises Format_Error when there are too few elements in
   --  the list or % data types do not match.
   --
   --  MEMORY LEAK WARNING: If Format_Error is raised because
   --  % data types do not match the corresponding list element,
   --  there is likely to be a memory leak.
   --------------------------------------------------------

   Format_Error : exception;

   function Format
     (FormatString : in String;
      Values       : in Tash_List)
      return         String;

   --------------------------------------------------------
   --  Following subprograms are used for examining the
   --  internal representation of a element.  They are
   --  primarily used in verification of the Tash interface.
   --------------------------------------------------------

   function Internal_Rep (List : in Tash_List) return String;
   --  Returns an image of the internal representation,
   --  including the string representation, identity, and
   --  reference count of the whole list and of each element.

private

   Null_Tash_List : constant Tash_List := (Ada.Finalization.Controlled with
                                           Obj => null);

   Verbose : Boolean := False;

end Tash.Lists;
