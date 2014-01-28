--------------------------------------------------------------------
--
--  Unit Name:    Tash.Integers spec
--
--  File Name:    tash-integers.ads
--
--  Purpose:      This package exports a Tash integer type along with
--               its operations.
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

with Ada.Finalization;

package Tash.Integers is

   --------------------------------------------------------
   --  A Tash integer is derived from a Tash object.
   --
   --  When used in any expression, an uninitialized Tash_Integer
   --  has the value of 0.
   --------------------------------------------------------

   type Tash_Integer is new Tash.Tash_Object with null record;

   Null_Tash_Integer : constant Tash_Integer;

   -----------------------------------------------
   --  Inherits Is_Null function from Tash.  It
   --  returns True if TInteger has not been initialized
   --  or has been set to Null_Tash_Integer.
   -----------------------------------------------

   --  function Is_Null (
   --   TInteger : in Tash_Integer) return Boolean;

   -----------------------------------------------
   --  Returns True if Is_Null(TInteger) is true or
   --  To_Integer(TInteger) = 0.
   -----------------------------------------------

   function Is_Zero (TInteger : in Tash_Integer) return Boolean;

   -----------------------------------------------
   --  Convert a string to a Tash integer.
   --  Raises Constraint_Error if number is invalid
   --  or outside the range of Tash_Integer.
   -----------------------------------------------

   function To_Tash_Integer (Str : in String) return Tash_Integer;

   --------------------------------------------------------
   --  Convert a Tash integer to a string.  Returns "0" for
   --  an uninitialized Tash_Integer.
   --
   --  Recall that all non-string Tash data types have a dual
   --  representation.  At any time, you may fetch either the
   --  string representation or the native (i.e. Integer)
   --  representation.  The string representation is updated
   --  to correspond with the native data type only when the
   --  string is fetched.
   --------------------------------------------------------

   function To_String (TInteger : in Tash_Integer) return String;

   --------------------------------------------------------
   --  Convert Ada integers to and from Tash integers.
   --------------------------------------------------------

   subtype Tash_Integer_Range is Long_Integer range
      Long_Integer (Interfaces.C.long'First) ..
      Long_Integer (Interfaces.C.long'Last);
   --  This subtype will not compile if standard Ada long_integer range
   --  is smaller than Tash long_integer range.  It is also used to assure
   --  that we don't attempt to convert Ada long_integers outside the Tash
   --  long_integer range to/from Tash long_integers.

   function To_Tash_Integer
     (LInteger : in Tash_Integer_Range)
      return     Tash_Integer;

   function "+" (LInteger : in Tash_Integer_Range) return Tash_Integer;

   function "-" (LInteger : in Tash_Integer_Range) return Tash_Integer;

   function To_Integer
     (TInteger : in Tash_Integer)
      return     Tash_Integer_Range;

   --------------------------------------------------------
   --  Increment a Tash integer.  If TInteger is uninitialized,
   --  it will be set to 1.
   --
   --  Note different formal argument name for the increment
   --  amount in these two subprograms.  This helps to disambiguate
   --  the following call when Tash.Integers."-" is visible:
   --
   --    Incr (A, -1);
   --
   --  Instead, use one of the following calls:
   --
   --    Incr (A, By => -1);
   --    Incr (A, Integer'(-1));
   --
   --------------------------------------------------------

   procedure Incr
     (TInteger : in out Tash_Integer;
      By       : in Tash_Integer_Range := 1);

   procedure Incr
     (TInteger : in out Tash_Integer;
      Amount   : in Tash_Integer);

   --------------------------------------------------------
   --  Compare Tash and standard Ada integers
   --------------------------------------------------------

   function "="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean;

   function "="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean;

   function "="
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean;

   function "<"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean;

   function "<"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean;

   function "<"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean;

   function "<="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean;

   function "<="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean;

   function "<="
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean;

   function ">"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean;

   function ">"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean;

   function ">"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean;

   function ">="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Boolean;

   function ">="
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Boolean;

   function ">="
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Boolean;

   --------------------------------------------------------
   --  Tash integer arithmetic operations
   --------------------------------------------------------

   function "abs" (Right : in Tash_Integer) return Tash_Integer;

   function "+"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "+"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer;

   function "+"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "-"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "-"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer;

   function "-"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "*"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "*"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer;

   function "*"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "/"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "/"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer;

   function "/"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "rem"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "rem"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer;

   function "rem"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "mod"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "mod"
     (Left  : in Tash_Integer;
      Right : in Tash_Integer_Range)
      return  Tash_Integer;

   function "mod"
     (Left  : in Tash_Integer_Range;
      Right : in Tash_Integer)
      return  Tash_Integer;

   function "**"
     (Left  : in Tash_Integer;
      Right : in Natural)
      return  Tash_Integer;

private

   Null_Tash_Integer : constant Tash_Integer :=
     (Ada.Finalization.Controlled with
      Obj => null);

   Verbose : Boolean := False;

end Tash.Integers;
