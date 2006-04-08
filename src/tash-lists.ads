-------------------------------------------------------------------
--
-- Unit Name:    Tash.Lists spec
--
-- File Name:    tash-lists.ads
--
-- Purpose:      Defines the Tash list type which may
--               contain any Tash object, including lists.
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
with Tash.Floats;
with Tash.Integers;
with Tash.Strings;
with Tcl;

package Tash.Lists is

   --------------------------------------------------------
   -- A Tash list is composed of zero or more Tash objects.
   -- Since a Tash list is a Tash object, you can construct
   -- lists of lists.  As each Tash object is added to and
   -- removed from a list, its reference count is updated.
   --------------------------------------------------------

   type Tash_List is new Tash.Tash_Object with null record;

   Null_Tash_List : constant Tash_List;

   List_Error     : exception;

   -----------------------------------------------
   -- Inherits Is_Null function from Tash.  It
   -- returns True if TList has not been initialized
   -- or has been set to Null_Tash_List.
   -----------------------------------------------

   -- function Is_Null (
   --   TList : in Tash_List) return Boolean;

   --------------------------------------------------------
   -- Returns True if Is_Null(TList) is true or
   -- Length(TList) = 0.
   --------------------------------------------------------

   function Is_Empty (
      List : in Tash_List) return Boolean;

   --------------------------------------------------------
   -- Create a Tash list from a string.  This breaks a string
   -- up into list elements separated by a sequence of spaces.
   --
   -- Those elements which have embedded spaces must be enclosed
   -- within a pair of braces ({}).  In this case, the whole
   -- sequence of characters within the braces will be one list
   -- element.  The braces will not be part of the element.
   --
   -- Raises List_Error if a string that contains an
   -- opening brace does not also containing a closing brace.
   --
   -- To escape a brace so that it is part of a string and
   -- not a marker for a start of a string, precede it with
   -- a '\' character.
   -------------------------------------------------------------

   function To_Tash_List (
      Str : in String) return Tash_List;

   --------------------------------------------------------
   -- Convert a Tash list to a string.
   --
   -- Recall that all non-string Tash data types have a dual
   -- representation.  At any time, you may fetch either the
   -- string representation or the native (i.e. List)
   -- representation.  The string representation is updated
   -- to correspond with the native data type only when the
   -- string is fetched.
   --------------------------------------------------------

   function To_String (
      TList : in Tash_List) return String;

   --------------------------------------------------------
   -- Create a new list.
   --
   -- Where any of Element1..Element9 is a list and Expand
   -- is True, each element of the parameter is an element
   -- of the returned list.
   --
   -- Where any of Element1..Element9 is a list and Expand
   -- is False, the list parameter is an element of the
   -- returned list.  This, you now have a list of lists.
   --
   -- Where any of Element1..Element9 are null, the element
   -- is not appended to the list.
   --
   -- Operator "+" is equivalent to the following call:
   --    Create (Element, Expand => False);
   --------------------------------------------------------

   function To_Tash_List (
      Element1 : in Tash.Tash_Object'Class;
      Element2 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element3 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element4 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element5 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element6 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element7 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element8 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Element9 : in Tash.Tash_Object'Class := Tash.Strings.Null_Tash_String;
      Expand   : in Boolean                := False)
         return Tash_List;

   function "+" (
      Element : in Tash.Tash_Object'Class) return Tash_List;

   -------------------------------------------------------------
   -- Create a Tash list by splitting the string, Str, into
   -- elements separated at each occurrence of any character in
   -- Split_At.  Adjacent Split_At characters in Str
   -- will result in the list containing empty elements.  If
   -- Split_At is an empty string, then the resulting list
   -- will consist of one element for each character in Str.
   --
   -- An example is splitting an unix path name into its
   -- components:
   --
   -- List := Split (Str      => "/usr/X/lib/X11",
   --                Split_At => "/");
   --
   -- Note that, since there is a leading split character, this
   -- call will return a five element list, of which the first
   -- is empty.  Since a empty element is represented as the
   -- string, "{}", calling Tash.To_String (List) results in
   -- the string, "{} usr X lib X11".
   --
   -- In this particular situation (splitting unix path names)
   -- see Pop below for removing the first element.
   -------------------------------------------------------------

   function Split (
      Str      : in String;
      Split_At : in String := " " & ASCII.CR & ASCII.HT & ASCII.LF)
         return Tash_List;

   function Split (
      TObject  : in Tash.Tash_Object'Class;
      Split_At : in String := " " & ASCII.CR & ASCII.HT & ASCII.LF)
         return Tash_List;

   ----------------------------------------
   -- Create a new Tash list from a range
   -- of elements of an existing list.
   ----------------------------------------

   function Slice (
      List  : in Tash_List;
      First : in Positive;
      Last  : in Natural) return Tash_List;

   --------------------------------------------------------
   -- Create a new Tash list by sorting the input list.
   -- Uses the Tcl lsort command which utilizes a merge sort
   -- algorithm.
   --------------------------------------------------------

   type Sort_Mode is (SM_ASCII, SM_Dictionary, SM_Integer, SM_Real);
   -- SM_ASCII Sort_Mode treats the list elements as strings
   -- and sorts them in ASCII collating sequence.  SM_Dictionary
   -- mode is similar to ASCII but ignores case except as a
   -- tie-breaker and numbers compare as integers, not characters.
   -- For example, in Dictionary mode, "bigBoy" sorts between "bigbang"
   -- and  "bigboy;"  x10y  sorts between x9y and x11y.  In SM_Integer
   -- and SM_Real mode, list elements are treated as integers and reals
   -- (Ada type Long_Float), respectively, before sorting.

   type Ordering is (Increasing, Decreasing);

   function Sort (
      List  : in Tash_List;
      Mode  : in Sort_Mode := SM_ASCII;
      Order : in Ordering  := Increasing) return Tash_List;

   ----------------------------------------
   -- Get length of a list.
   -- Returns 0 if Is_Empty (List) is true.
   ----------------------------------------

   function Length (
      List : in Tash_List) return Natural;

   --------------------------------------------------------
   -- Get a specific list element.  If you don't know the
   -- data type of the returned element, you can use either
   -- Element that returns a string or Element_Obj.
   -- Raises List_Error if the requested list item data type
   -- doesn't match the return type.
   --------------------------------------------------------

   function Element (
      List  : in Tash_List;
      Index : in Positive) return Tash.Floats.Tash_Float;

   function Element (
      List  : in Tash_List;
      Index : in Positive) return Tash.Integers.Tash_Integer;

   function Element (
      List  : in Tash_List;
      Index : in Positive) return Tash.Strings.Tash_String;

   function Element_Obj (
      List  : in Tash_List;
      Index : in Positive) return Tash.Tash_Object'Class;

   --------------------------------------------------------
   -- To iterate over a Tash list, use the
   -- Length and Element subprograms:
   --
   -- declare
   --    List : Tash_List;
   --    Elem : Element_Ref;
   -- begin
   --    for i in 1..Length (List) loop
   --       Elem := Element (List, i);
   --    end loop;
   -- end;
   --------------------------------------------------------

   --------------------------------------------------------
   -- Get the first or last element of the list without
   -- modifying the list.
   --------------------------------------------------------

   function Head (
      List : in Tash_List) return Tash.Integers.Tash_Integer;

   function Head (
      List : in Tash_List) return Tash.Floats.Tash_Float;

   function Head (
      List : in Tash_List) return Tash.Strings.Tash_String;

   function Head_Obj (
      List : in Tash_List) return Tash.Tash_Object'Class;

   function Tail (
      List : in Tash_List) return Tash.Integers.Tash_Integer;

   function Tail (
      List : in Tash_List) return Tash.Floats.Tash_Float;

   function Tail (
      List : in Tash_List) return Tash.Strings.Tash_String;

   function Tail_Obj (
      List : in Tash_List) return Tash.Tash_Object'Class;

   --------------------------------------------------------
   -- Append an element or a list to a Tash list.
   --
   -- If Element is a list and Expand is True, each
   -- element of Element is appended to List.
   --
   -- If Element is a list and Expand is False, Element
   -- is appended as a list.  You now have a list of lists.
   --
   -- If Element is not a list, Expand has no effect.
   --
   -- Raises List_Error when List to be appended to
   -- is shared (reference by another list).
   --------------------------------------------------------

   procedure Append (
      List    : in out Tash_List;
      Element : in     Tash.Tash_Object'Class;
      Expand  : in     Boolean := False);

   --------------------------------------------------------
   -- Replace the Index'th element in a Tash list with an
   -- element or list.  The elements of a list are indexed
   -- starting with element index 1.
   --
   -- If Element is a list and Expand is True, each
   -- element of Element is appended to List.
   --
   -- If Element is a list and Expand is False, Element
   -- is appended as a list.  You now have a list of lists.
   --
   -- If Element is not a list, Expand has no effect.
   --------------------------------------------------------

   procedure Replace_Element (
      List    : in out Tash_List;
      Index   : in     Positive;
      Element : in     Tash.Tash_Object'Class;
      Expand  : in     Boolean := False);

   --------------------------------------------------------
   -- Replace a slice of a Tash list with an element or list.
   --
   -- If Element is a list and Expand is True, each
   -- element of Element is appended to List.
   --
   -- If Element is a list and Expand is False, Element
   -- is appended as a list.  You now have a list of lists.
   --
   -- If Element is not a list, Expand has no effect.
   --
   -- If To < From, has the effect of inserting Element
   -- in List before From.
   --------------------------------------------------------

   procedure Replace_Slice (
      List    : in out Tash_List;
      From    : in     Positive;
      To      : in     Natural;
      Element : in     Tash.Tash_Object'Class;
      Expand  : in     Boolean := False);

   --------------------------------------------------------
   -- Insert an element or list into a Tash list before
   -- the Index'th element.  The elements of a list are
   -- indexed starting with element index 1.
   --
   -- If Element is a list and Expand is True, each
   -- element of Element is inserted into List.
   --
   -- If Element is a list and Expand is False, Element
   -- is inserted as a list.  You now have a list of lists.
   --
   -- If Element is not a list, Expand has no effect.
   --------------------------------------------------------

   procedure Insert (
     List    : in out Tash_List;
     Index   : in     Positive;
     Element : in     Tash.Tash_Object'Class;
     Expand  : in     Boolean := False);

   --------------------------------------------------------
   -- Deletes an element or slice from a Tash list.
   -- Has no effect in any of the following situations:
   --   1) List is null or empty
   --   2) Length is less than Index or From
   --   3) To is less than From
   --------------------------------------------------------

   procedure Delete_Element (
      List  : in out Tash_List;
      Index : in     Positive);

   procedure Delete_Slice   (
      List  : in out Tash_List;
      From  : in     Positive;
      To    : in     Natural);

   --------------------------------------------------------
   -- Push and Pop treat a Tash list as a stack.
   --
   -- Push inserts Element before the first element of List.
   --
   -- If Element is a list and Expand is True, each
   -- element of Element is inserted before the first
   -- element of List.
   --
   -- If Element is a list and Expand is False, Element
   -- is inserted as a list element before the first
   -- element of List.  You now have a list of lists.
   --
   -- If Element is not a list, Expand has no effect.
   --
   -- Pop removes the first element and "throws it away."
   -- It is identical to Delete_Element with Index = 1.
   --------------------------------------------------------

   procedure Push (
      List    : in out Tash_List;
      Element : in     Tash.Tash_Object'Class;
      Expand  : in     Boolean := False);

   procedure Pop (
      List : in out Tash_List);

   --------------------------------------------------------
   -- Compare Tash lists as if they had first been converted
   -- to strings.
   --------------------------------------------------------

   function "="  (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean;

   function "<"  (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean;

   function "<=" (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean;

   function ">"  (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean;

   function ">=" (
      Left  : in Tash_List;
      Right : in Tash_List) return Boolean;

   --------------------------------------------------------
   -- Replace % sequences in formatString with corresponding
   -- Values.  This is similar to C's printf.
   --
   -- Raises Format_Error when there are too few elements in
   -- the list or % data types do not match.
   --
   -- MEMORY LEAK WARNING: If Format_Error is raised because
   -- % data types do not match the corresponding list element,
   -- there is likely to be a memory leak.
   --------------------------------------------------------

   Format_Error : exception;

   function Format (
      FormatString : in String;
      Values       : in Tash_List) return String;

   --------------------------------------------------------
   -- Following subprograms are used for examining the
   -- internal representation of a Tash object.  They are
   -- primarily used in verification of the Tash interface.
   --------------------------------------------------------

   function Internal_Rep (
      TList : in Tash_List) return String;
   -- Returns an image of the internal representation,
   -- including the string representation, identity, and
   -- reference count of the whole list and of each element.

private

   Null_Tash_List : constant Tash_List := (
      Ada.Finalization.Controlled with Obj => Tcl.Null_Tcl_Obj);

   Verbose  : Boolean := False;

end Tash.Lists;
