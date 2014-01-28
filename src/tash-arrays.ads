--------------------------------------------------------------------
--
--  Unit Name:    Tash.Arrays spec
--
--  File Name:    tash-arrays.ads
--
--  Purpose:      Defines the Tash array type which is
--                an associative array whose indices are
--                strings and contents may be any Tash data type.
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

with Ada.Finalization;
with Tash.Lists;

package Tash.Arrays is

   ---------------------------------------------------------
   --  A Tash array is composed of zero or more elements each
   --  indexed by a string, like an associative array in awk
   --  or perl.  Each element may be either a scalar type (see
   --  the generics below for handling array elements of integer
   --  and float) or a non-scalar type.
   --
   --  Currently, two non-scalar element types are supported:
   --  strings and Tash lists.
   --
   --  Elements of different types may to stored in an array.
   --------------------------------------------------------

   type Tash_Array is new Tash.Tash_Object with null record;

   Null_Tash_Array : constant Tash_Array;

   Array_Error : exception;

   -----------------------------------------------
   --  Inherits Is_Null function from Tash.  It
   --  returns True if TArray has not been initialized
   --  or has been set to Null_Tash_Array.
   -----------------------------------------------

   --  function Is_Null (
   --   TArray : in Tash_Array) return Boolean;

   --------------------------------------------------------
   --  Is_Empty returns True if Is_Null(TArray) is true or
   --  Length(TArray) = 0.
   --------------------------------------------------------

   function Is_Empty (TArray : in Tash_Array) return Boolean;

   -------------------------------------------------------------------
   --  Create a Tash array from a string.  The
   --  string must contain index/value pairs
   --  separated by white space.  Any index or value
   --  which contains white space must be enclosed
   --  in braces ({}).
   --
   --  Example:
   --
   --  State_Capitals := To_Tash_Array (
   --    "{New York} Albany Massachusetts Boston";
   --         ^        ^         ^          ^
   --         |        |         |          +--> Value of 2nd element
   --         |        |         +-------------> Index of 2nd element
   --         |        +-----------------------> Value of 1st element
   --         +--------------------------------> Index of 1st element
   --
   --  Raises Array_Error if there is not a matching
   --  value for each index.
   ------------------------------------------------------------------

   function To_Tash_Array (Str : in String) return Tash_Array;

   function "+" (Str : in String) return Tash_Array;

   -------------------------------------------------------------
   --  There is no capability to concatenate Tash arrays because
   --  of the problem of dealing with duplicate indices in the
   --  two arrays.  You must handle this yourself.
   -------------------------------------------------------------

   --------------------------------------------------------
   --  Create a new array which is a copy of the original.
   --  The resulting array is a new array, but the elements
   --  "point" to the elements of the original array.
   --
   --  Some call this a "shallow" copy.
   --------------------------------------------------------

   --   function Copy (
   --      TArray   : in Tash_Array) return Tash_Array;

   --------------------------------------------------------
   --  Create a new array which is a copy of the original.
   --  The resulting array is a new array AND each element
   --  is a new element.
   --
   --  Some call this a "deep" copy.
   --------------------------------------------------------

   --   function Duplicate (
   --      TArray   : in Tash_Array) return Tash_Array;

   --------------------------------------------------------
   --  Convert a Tash array to a string.
   --------------------------------------------------------

   function To_String (TArray : in Tash_Array) return String;

   -------------------------------------------------------
   --  Procedure PArray executes the Tcl "parray" command
   --  which prints the specified array to standard output.
   -------------------------------------------------------

   procedure PArray (TArray : in Tash_Array);

   ----------------------------------------
   --  Get the number of elements in the array.
   --  Returns 0 if Is_Empty (Array) is true.
   ----------------------------------------

   function Length (TArray : in Tash_Array) return Natural;

   --------------------------------------------------------
   --  Get one array element with the specified Index.
   --
   --  Raises Array_Error if the array is null or empty or
   --  if no element exists in the array with the specified
   --  Index.
   --
   --  Use the function, Exists, to determine whether an
   --  element with a specific Index exists.
   --------------------------------------------------------

   function Get_Element
     (TArray : in Tash_Array;
      Index  : in String)
      return   String;

   function Get_Element
     (TArray : in Tash_Array;
      Index  : in String)
      return   Tash.Lists.Tash_List;

   --------------------------------------------------------
   --  Find out whether an array element with the specified
   --  Index exists.
   --------------------------------------------------------

   function Exists
     (TArray : in Tash_Array;
      Index  : in String)
      return   Boolean;

   --------------------------------------------------------
   --  Find out whether an array element is a string or a list
   --------------------------------------------------------

   function Element_Is_String
     (TArray : in Tash_Array;
      Index  : in String)
      return   Boolean;

   function Element_Is_List
     (TArray : in Tash_Array;
      Index  : in String)
      return   Boolean;

   --------------------------------------------------------
   --  Returns a list containing index/value pairs of elements.
   --  The order is undefined.  If Pattern is empty, then all
   --  elements of the array are returned.
   --
   --  If Pattern is not empty, then those elements whose indices
   --  match the pattern using glob-style (not regular expression)
   --  pattern matching are returned.  The pattern must match the
   --  whole index, not just a part.  For example, to match the
   --  "in" in both "string" and "integer", use "*in*" as the pattern.
   --
   --  If the array is null or empty, returns Tash.Lists.Null_Tash_List.
   --------------------------------------------------------

   function Get_Elements
     (TArray  : in Tash_Array;
      Pattern : in String := "")
      return    Tash.Lists.Tash_List;

   --------------------------------------------------------
   --  Returns a list containing index/value pairs of elements.
   --  The array is sorted on its indices based on Mode and Order.
   --  If Pattern is empty, then all elements of the array are
   --  returned.
   --
   --  If Pattern is not empty, then those elements whose indices
   --  match the pattern using glob-style (not regular expression)
   --  pattern matching are returned.  The pattern must match the
   --  whole index, not just a part.  For example, to match the
   --  "in" in both "string" and "integer", use "*in*" as the pattern.
   --
   --  If the array is null or empty, returns Tash.Lists.Null_Tash_List.
   --------------------------------------------------------

   function Get_Sorted_Elements
     (TArray  : in Tash_Array;
      Pattern : in String               := "";
      Mode    : in Tash.Lists.Sort_Mode := Tash.Lists.SM_ASCII;
      Order   : in Tash.Lists.Ordering  := Tash.Lists.Increasing)
      return    Tash.Lists.Tash_List;

   --------------------------------------------------------
   --  Get the indices of an array.  If Pattern is empty, then
   --  all indices of the array are returned.  If Pattern is not
   --  empty, then those indices that match the pattern using
   --  glob-style (not regular expression) pattern matching are
   --  returned.  If the array is null or empty, returns an
   --  empty list or empty string.
   --
   --  Use Tash.Lists.Sort to sort the resulting list if desired.
   --------------------------------------------------------

   function Get_Indices
     (TArray  : in Tash_Array;
      Pattern : in String := "")
      return    String;

   function Get_Indices
     (TArray  : in Tash_Array;
      Pattern : in String := "")
      return    Tash.Lists.Tash_List;

   --------------------------------------------------------
   --  Set the value of an array element.  If the Index'th
   --  element does not exist, will create a new element.
   --  If it does exist, will overwrite its value.
   --  If the array, TArray, does not exist, it will be created.
   --------------------------------------------------------

   procedure Set_Element
     (TArray : in out Tash_Array;
      Index  : in String;
      Value  : in String);

   procedure Set_Element
     (TArray : in out Tash_Array;
      Index  : in String;
      Value  : in Tash.Lists.Tash_List);

   --------------------------------------------------------
   --  Set the value of many array elements.  List must be
   --  in the form as would be returned by Get_Elements
   --  where each odd-numbered list element is an array
   --  index and each even-numbered list element is the
   --  corresponding array element value.
   --------------------------------------------------------

   procedure Set_Elements
     (TArray : in out Tash_Array;
      List   : in Tash.Lists.Tash_List);

   --------------------------------------------------------
   --  Delete an element, If the Index'th element does not
   --  exist, this procedure has no effect.  Note that the
   --  deleted object itself does not go away unless its
   --  reference count is decremented to zero.
   --------------------------------------------------------

   procedure Delete_Element (TArray : in Tash_Array; Index : in String);

   --------------------------------------------------------
   --  Generic for handling Integer list elements
   --------------------------------------------------------

   generic
      type Item is range <>;
   package Generic_Integer_Arrays is

      --------------------------------------------------------
      --  Create a Tash array with one element, an integer.
      --------------------------------------------------------

      function To_Tash_Array
        (Index : in String;
         Num   : in Item)
         return  Tash_Array;

      --------------------------------------------------------
      --  Get a specific array element.
      --
      --  Raises Array_Error if no element exists with the
      --  specified Index.
      --
      --  Raises Constraint_Error if the Index'th element of the
      --  array does not match the return type.
      --------------------------------------------------------

      function Get_Element
        (TArray : in Tash_Array;
         Index  : in String)
         return   Item;

      --------------------------------------------------------
      --  Find out whether a array element is an integer
      --------------------------------------------------------

      function Element_Is_Integer
        (TArray : in Tash_Array;
         Index  : in String)
         return   Boolean;

      ---------------------------------------------------------------
      --  Set the value of an array element.  If the Index'th
      --  element does not exist, will create a new element.
      --  If it does exist, will overwrite its value.
      --  If the array, TArray, does not exist, it will be created.
      ---------------------------------------------------------------

      procedure Set_Element
        (TArray : in out Tash_Array;
         Index  : in String;
         Value  : in Item);

   end Generic_Integer_Arrays;

   --------------------------------------------------------
   --  Generic for handling Float list elements
   --------------------------------------------------------

   generic
      type Item is digits <>;
   package Generic_Float_Arrays is

      --------------------------------------------------------
      --  Create a Tash array with one element, a float.
      --------------------------------------------------------

      function To_Tash_Array
        (Index : in String;
         Num   : in Item)
         return  Tash_Array;

      --------------------------------------------------------
      --  Get a specific array element.
      --
      --  Raises Array_Error if no element exists with the
      --  specified Index.
      --
      --  Raises Constraint_Error if the Index'th element of the
      --  array does not match the return type.
      --------------------------------------------------------

      function Get_Element
        (TArray : in Tash_Array;
         Index  : in String)
         return   Item;

      --------------------------------------------------------
      --  Find out whether a array element is a float
      --------------------------------------------------------

      function Element_Is_Float
        (TArray : in Tash_Array;
         Index  : in String)
         return   Boolean;

      ---------------------------------------------------------------
      --  Set the value of an array element.  If the Index'th
      --  element does not exist, will create a new element.
      --  If it does exist, will overwrite its value.
      --  If the array, TArray, does not exist, it will be created.
      ---------------------------------------------------------------

      procedure Set_Element
        (TArray : in out Tash_Array;
         Index  : in String;
         Value  : in Item);

   end Generic_Float_Arrays;

   --------------------------------------------------------
   --  Following subprograms are used for examining the
   --  internal representation of a element.  They are
   --  primarily used in verification of the Tash interface.
   --------------------------------------------------------

   function Internal_Rep (TArray : in Tash_Array) return String;
   --  Returns an image of the internal representation,
   --  including the string representation, identity, and
   --  reference count of the whole array and of each element.

   function Internal_Name (TArray : in Tash_Array) return String;
   --  Returns the Tcl array variable in which TArray is stored.

private

   procedure Finalize (TArray : in out Tash_Array);

   Null_Tash_Array : constant Tash_Array := (Ada.Finalization.Controlled with
                                             Obj => null);

   Verbose : Boolean := False;

end Tash.Arrays;
