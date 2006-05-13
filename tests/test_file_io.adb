with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Text_IO;
with Tash.File;
with Tash.File_IO;
with Tash.Lists;
with Tash.Platform;
with Tash.Test;

procedure Test_File_Io is

   use type Tash.File.File_Type;
   use type Tash.File.Path_Type;
   use type Tash.Lists.Tash_List;
   use type Tash.File_IO.Buffering_Mode;
   use type Tash.File_IO.Translation_Mode;

   Verbose        : Boolean         := False;
   Test_File      : Tash.File_IO.Tash_File;
   Test_File_Name : constant String := "test_file.txt";
   Platform       : constant String := Tash.Platform.Platform;
   K              : constant := 1024;

begin --  Test_File_Io

   --------------------------------------------
   --  Check for -verbose command line argument
   --------------------------------------------
   GET_COMMAND_LINE_ARGUMENTS : for I in  1 ..
        Ada.Command_Line.Argument_Count
   loop
      if Ada.Command_Line.Argument (I) = "-verbose" then
         Verbose := True;
         exit GET_COMMAND_LINE_ARGUMENTS;
      end if;
   end loop GET_COMMAND_LINE_ARGUMENTS;

   Tash.Test.Set_Verbose (On => Verbose);

   --  create a new file
   --------------------
   Tash.File.Delete (Test_File_Name, Force => True);
   Tash.File_IO.Open
     (File => Test_File,
      Name => Test_File_Name,
      Mode => Tash.File_IO.Write_Trunc);
   Tash.Test.Test_Case
     (Description => "create a new file",
      Result      => Tash.File.Exists (Test_File_Name));
   declare
      File_String : constant String := Tash.File_IO.To_String (Test_File);
   begin
      Tash.Test.Test_Case
        (Description => "create a new file",
         Result      => File_String (1 .. 4) = "file");
   end;

   --  test that file is in buffering mode Full
   -------------------------------------------
   Tash.Test.Test_Case
     (Description => "test that file is in buffering mode Full",
      Result      => Tash.File_IO.Get_Buffering_Mode (Test_File) =
                     Tash.File_IO.Full);

   --  set buffering mode to Line
   -----------------------------
   Tash.File_IO.Set_Buffering_Mode (Test_File, Tash.File_IO.Line);
   Tash.Test.Test_Case
     (Description => "test that file is in buffering mode Line",
      Result      => Tash.File_IO.Get_Buffering_Mode (Test_File) =
                     Tash.File_IO.Line);

   --  set buffering mode to None
   -----------------------------
   Tash.File_IO.Set_Buffering_Mode (Test_File, Tash.File_IO.None);
   Tash.Test.Test_Case
     (Description => "test that file is in buffering mode None",
      Result      => Tash.File_IO.Get_Buffering_Mode (Test_File) =
                     Tash.File_IO.None);

   --  set buffering mode to Full
   -----------------------------
   Tash.File_IO.Set_Buffering_Mode (Test_File, Tash.File_IO.Full);
   Tash.Test.Test_Case
     (Description => "test that file is in buffering mode Full",
      Result      => Tash.File_IO.Get_Buffering_Mode (Test_File) =
                     Tash.File_IO.Full);

   --  get file buffer size
   -----------------------
   Tash.Test.Test_Case
     (Description => "get file buffer size",
      Result      => Tash.File_IO.Get_Buffer_Size (Test_File) = 4096);

   --  set file buffer size
   -----------------------
   Tash.File_IO.Set_Buffer_Size (Test_File, 32 * K);
   Tash.Test.Test_Case
     (Description => "set file buffer size",
      Result      => Tash.File_IO.Get_Buffer_Size (Test_File) = 32 * K);

   --  get EOF character of a file
   ------------------------------
   Tash.Test.Test_Case
     (Description => "get EOF character of a file",
      Result      => Tash.File_IO.Get_EOF_Char (Test_File) = "");
   Tash.Test.Test_Case
     (Description => "get EOF character of a file",
      Result      => Tash.File_IO.EOF_Char_Is_Empty_String (Test_File));

   --  set EOF character of a file
   ------------------------------
   Tash.File_IO.Set_EOF_Char (Test_File, Ada.Characters.Latin_1.EM);
   Tash.Test.Test_Case
     (Description => "get EOF character of a file",
      Result      => Tash.File_IO.Get_EOF_Char (Test_File) =
                     Ada.Characters.Latin_1.EM);

   --  set EOF character of a file
   ------------------------------
   Tash.File_IO.Set_EOF_Char_To_Empty_String (Test_File);
   Tash.Test.Test_Case
     (Description => "get EOF character of a file",
      Result      => Tash.File_IO.EOF_Char_Is_Empty_String (Test_File));

   --  test that file is in translation mode CRLF
   ---------------------------------------------
   if Platform = "windows" then
      Tash.Test.Test_Case
        (Description => "test that file is in translation mode CRLF",
         Result      => Tash.File_IO.Get_Translation_Mode (Test_File) =
                        Tash.File_IO.CRLF);
   else
      Tash.Test.Test_Case
        (Description => "test that file is in translation mode LF",
         Result      => Tash.File_IO.Get_Translation_Mode (Test_File) =
                        Tash.File_IO.LF);
   end if;

   --  set translation mode to LF
   -----------------------------
   Tash.File_IO.Set_Translation_Mode (Test_File, Tash.File_IO.LF);
   Tash.Test.Test_Case
     (Description => "test that file is in translation mode LF",
      Result      => Tash.File_IO.Get_Translation_Mode (Test_File) =
                     Tash.File_IO.LF);

   --  set translation mode to CRLF
   -------------------------------
   Tash.File_IO.Set_Translation_Mode (Test_File, Tash.File_IO.CRLF);
   Tash.Test.Test_Case
     (Description => "test that file is in translation mode CRLF",
      Result      => Tash.File_IO.Get_Translation_Mode (Test_File) =
                     Tash.File_IO.CRLF);

   --  Write to the file
   --------------------
   Tash.File_IO.Put (Test_File, "Line 1");
   Tash.File_IO.Flush (Test_File);
   Tash.File_IO.New_Line (Test_File, 3);
   Tash.File_IO.Put_Line (Test_File, "Line 4");

   --  close the file
   -----------------
   Tash.File_IO.Close (Test_File);

   --  open a file for appending
   ----------------------------
   Tash.File_IO.Open
     (File => Test_File,
      Name => Test_File_Name,
      Mode => Tash.File_IO.Write_Append);

   --  get file buffer size
   -----------------------
   Tash.Test.Test_Case
     (Description => "get file buffer size",
      Result      => Tash.File_IO.Get_Buffer_Size (Test_File) = 4096);

   --  set file buffer size
   -----------------------
   Tash.File_IO.Set_Buffer_Size (Test_File, 32 * K);
   Tash.Test.Test_Case
     (Description => "set file buffer size",
      Result      => Tash.File_IO.Get_Buffer_Size (Test_File) = 32 * K);

   --  Write to the file
   --------------------
   Tash.File_IO.Put_Line (Test_File, "Line 5");

   --  close the file
   -----------------
   Tash.File_IO.Close (Test_File);

   --  open a file for reading
   --------------------------
   Tash.File_IO.Open
     (File => Test_File,
      Name => Test_File_Name,
      Mode => Tash.File_IO.Read);

   --  get file buffer size
   -----------------------
   Tash.Test.Test_Case
     (Description => "get file buffer size",
      Result      => Tash.File_IO.Get_Buffer_Size (Test_File) = 4096);

   --  set file buffer size
   -----------------------
   Tash.File_IO.Set_Buffer_Size (Test_File, 32 * K);
   Tash.Test.Test_Case
     (Description => "set file buffer size",
      Result      => Tash.File_IO.Get_Buffer_Size (Test_File) = 32 * K);

   --  Read from the file
   ----------------------
   declare
      Line : String (1 .. 100);
      Last : Natural;
   begin

      Tash.File_IO.Get (Test_File, Line (1 .. 4));
      Tash.Test.Test_Case
        (Description => "read from a file -- first 4 characters",
         Result      => Line (1 .. 4) = "Line",
         Failure_Msg => Line (1 .. 4));

      Tash.File_IO.Get_Line (Test_File, Line (5 .. Line'Last), Last);
      Tash.Test.Test_Case
        (Description => "read from a file -- Line 1",
         Result      => Line (1 .. Last) = "Line 1",
         Failure_Msg => Line (1 .. Last));

      Tash.File_IO.Get_Line (Test_File, Line, Last);
      Tash.Test.Test_Case
        (Description => "read from a file -- Line 2",
         Result      => Line (1 .. Last) = "",
         Failure_Msg => Line (1 .. Last));

      Tash.File_IO.Get_Line (Test_File, Line, Last);
      Tash.Test.Test_Case
        (Description => "read from a file -- Line 3",
         Result      => Line (1 .. Last) = "",
         Failure_Msg => Line (1 .. Last));

      Tash.File_IO.Get_Line (Test_File, Line, Last);
      Tash.Test.Test_Case
        (Description => "read from a file -- Line 4",
         Result      => Line (1 .. Last) = "Line 4",
         Failure_Msg => Line (1 .. Last));

      Tash.File_IO.Get_Line (Test_File, Line, Last);
      Tash.Test.Test_Case
        (Description => "read from a file -- Line 5",
         Result      => Line (1 .. Last) = "Line 5",
         Failure_Msg => Line (1 .. Last));

   end;

   --  test that file is in non-blocking mode
   -----------------------------------------
   Tash.Test.Test_Case
     (Description => "test that file is in non-blocking mode",
      Result      => Tash.File_IO.Get_Blocking_Mode (Test_File));

   --  close the file
   -----------------
   Tash.File_IO.Close (Test_File);

   --  create and read from a process pipeline
   ------------------------------------------
   declare
      Line : String (1 .. 100);
      Last : Natural;
   begin
      Tash.File_IO.Open
        (File => Test_File,
         Name => "| gnatkr test_file_io.adb",
         Mode => Tash.File_IO.Read);
      Tash.Test.Test_Case
        (Description => "file is not at EOF",
         Result      => not Tash.File_IO.End_Of_File (Test_File));
      Tash.File_IO.Get_Line (Test_File, Line, Last);
      Tash.Test.Test_Case
        (Description => "create and read from a process pipeline",
         Result      => Line (1 .. Last) = "tesfilio.adb",
         Failure_Msg => Line (1 .. Last));
      while not Tash.File_IO.End_Of_File (Test_File) loop
         Tash.File_IO.Get_Line (Test_File, Line, Last);
      end loop;
      Tash.Test.Test_Case
        (Description => "file is at EOF",
         Result      => Tash.File_IO.End_Of_File (Test_File));
      Tash.File_IO.Close (Test_File);
   end;

   if Tash.Test.All_Test_Cases_Passed then
      Ada.Text_IO.Put_Line
        ("Test_File_IO PASSED --" &
         Integer'Image (Tash.Test.Test_Case_Number) &
         " tests completed");
   else
      Ada.Text_IO.Put_Line ("Test_File_IO FAILED");
   end if;

end Test_File_Io;
