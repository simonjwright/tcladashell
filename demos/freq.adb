--------------------------------------------------------------------
--
-- freq.adb -- This program demonstrates how the TASH Ada/Tcl interface
--             provides Tcl features for use in an Ada program.
--
-- Copyright (c) 1995-1997 Terry J. Westley
--
-- See the file "license.htm" for information on usage and
-- redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
-- This program reads lines from standard input and counts the number
-- of occurrences of each unique line.  Frequency counts are written
-- to standard out.
--
--------------------------------------------------------------------

with Ada.Text_IO;
with CHelper;
with Interfaces.C.Strings;
with Tcl;
with Tcl.Ada;

procedure Freq is -- Frequency counter

   use Tcl;

   package C renames Interfaces.C;
   package Hash is new Tcl.Ada.Generic_Hash (Integer); use Hash;

   function "=" (Left, Right : in Tcl_HashEntry)
      return Boolean renames Tcl."=";
   function "=" (Left, Right : in C.Int)
      return Boolean renames C."=";

   Line          : C.Strings.Chars_Ptr := C.Strings.Null_Ptr;
   Freq_Count    : Integer;
   Item          : C.Strings.Chars_Ptr;
   Hash_Table    : aliased Tcl_HashTable_Rec;
   Freq_Hash     : Tcl_HashTable := Hash_Table'Unchecked_Access;
   Entry_Ptr     : Tcl_HashEntry;
   Is_New_Entry  : aliased C.Int;
   Search_Rec    : aliased Tcl_HashSearch_rec;
   Search        : Tcl_HashSearch := Search_Rec'Unchecked_Access;

   procedure Get_Line (Line : in out C.Strings.Chars_Ptr) is
   -- This procedure gets a line from standard input and converts
   -- it to a "C" string.
      Input_Line : String (1..1024);
      Length     : Natural;
   begin -- Get_Line
      Ada.Text_IO.Get_Line (Input_Line, Length);
      C.Strings.Free (Line);
      Line := C.Strings.New_String (Input_Line (1..Length));
   end Get_Line;

begin -- Freq

   -- create a hash table for holding frequency counts
   Tcl_InitHashTable (Freq_Hash, TCL_STRING_KEYS);

   -- read lines from standard input until
   -- end of file encountered
   while not Ada.Text_IO.End_of_File loop
      Get_Line (Line);
      -- create (or find, if already created) an entry for this line
      Entry_Ptr := Tcl_CreateHashEntry (
         Freq_Hash, Line, Is_New_Entry'Access);
      if Is_New_Entry = 1 then
         Freq_Count := 1;
      else
         -- get the frequency count from the hash
         Freq_Count := Tcl_GetHashValue (Entry_Ptr) + 1;
      end if;
      -- Store the updated frequency count in the table.
      -- WARNING: We take advantage of the fact that an integer is the
      -- same size as a C pointer and store the count in the table,
      -- rather than a pointer to it.
      Tcl_SetHashValue (Entry_Ptr, Freq_Count);
   end loop;

   -- iterate through every item and print it and its frequency count
   Entry_Ptr := Tcl_FirstHashEntry (Freq_Hash, Search);
   while not Is_Null (Entry_Ptr) loop
      Freq_Count := Tcl_GetHashValue (Entry_Ptr);
      Item       := Tcl_GetHashKey (Freq_Hash, Entry_Ptr);
      Ada.Text_IO.Put_Line (CHelper.Value (Item) & Integer'image (Freq_Count));
      Entry_Ptr  := Tcl_NextHashEntry (Search);
   end loop;

   -- delete the frequency counter hash table
   Tcl_DeleteHashTable (Freq_Hash);

end Freq;
