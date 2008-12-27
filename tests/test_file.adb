with Ada.Calendar;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Tash.File;
with Tash.Lists;
with Tash.Platform;
with Tash.Test;

procedure Test_File is

   use type Ada.Calendar.Time;
   use type Tash.File.File_Type;
   use type Tash.File.Path_Type;
   use type Tash.Lists.Tash_List;

   Verbose            : Boolean         := False;
   Test_File          : Ada.Text_IO.File_Type;
   Test_File_Name     : constant String := "test_file.txt";
   Platform           : constant String := Tash.Platform.Platform;
   New_Access_Time    : Ada.Calendar.Time;
   File_Access_Time   : Ada.Calendar.Time;
   New_Modified_Time  : Ada.Calendar.Time;
   File_Modified_Time : Ada.Calendar.Time;

begin --  Test_File

   -------------------------------------------
   --  Check for -verbose command line argument
   -------------------------------------------
   GET_COMMAND_LINE_ARGUMENTS : for I in  1 ..
        Ada.Command_Line.Argument_Count
   loop
      if Ada.Command_Line.Argument (I) = "-verbose" then
         Verbose := True;
         exit GET_COMMAND_LINE_ARGUMENTS;
      end if;
   end loop GET_COMMAND_LINE_ARGUMENTS;

   Tash.Test.Set_Verbose (On => Verbose);

   --  Create a file we can use for testing access time
   ---------------------------------------------------
   Ada.Text_IO.Create
     (File => Test_File,
      Mode => Ada.Text_IO.Out_File,
      Name => Test_File_Name);
   Ada.Text_IO.Put_Line
     (Test_File,
      "Test getting and setting file access time");
   Ada.Text_IO.Close (Test_File);

   --  test file exists function
   ----------------------------
   Tash.Test.Test_Case
     (Description => "test file exists function",
      Result      => Tash.File.Exists (Test_File_Name));

   --  test file does not exist function
   ------------------------------------
   Tash.Test.Test_Case
     (Description => "test file does not exist function",
      Result      => not Tash.File.Exists ("file_does_not_exist.txt"));

   --  Set the access time of a file
   --------------------------------
   New_Access_Time :=
      Ada.Calendar.Time_Of
        (Year    => 2000,
         Month   => 1,
         Day     => 1,
         Seconds => 0.0);
   Tash.File.Set_Access_Time
     (Name => Test_File_Name,
      Date => New_Access_Time);

   --  Get the access time of a file
   --------------------------------
   File_Access_Time := Tash.File.Get_Access_Time (Test_File_Name);
   Tash.Test.Test_Case
     (Description => "get and set access time of a newly created file",
      Result      => New_Access_Time = File_Access_Time);

   --  Set the access time of a non-existent file
   ---------------------------------------------
   declare
      Description : constant String :=
         "set access time of a non-existent file";
   begin
      Tash.File.Set_Access_Time
        (Name => "no_such_file.txt",
         Date => New_Access_Time);
      Tash.Test.Test_Case (Description => Description, Result => False);
   exception
      when Tash.File.No_Such_File =>
         Tash.Test.Test_Case (Description => Description, Result => True);
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  Get the access time of a non-existent file
   ---------------------------------------------
   declare
      Description : constant String :=
         "get access time of a non-existent file";
   begin
      File_Access_Time := Tash.File.Get_Access_Time ("no_such_file.txt");
      Tash.Test.Test_Case (Description => Description, Result => False);
   exception
      when Tash.File.No_Such_File =>
         Tash.Test.Test_Case (Description => Description, Result => True);
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  Get the LongName attribute of a file
   ---------------------------------------
   declare
      Long_Name : constant String :=
         Tash.File.Get_Attribute
           (Name => Test_File_Name,
            Attr => Tash.File.LongName);
   begin
      if Platform = "windows" then
         Tash.Test.Test_Case
           (Description => "get the LongName attribute of a file",
            Result      => Long_Name = Test_File_Name);
      else
         Tash.Test.Test_Case
           (Description => "get the LongName attribute of a file",
            Result      => Long_Name = "");
      end if;
   end;

   --  Get the ShortName attribute of a file
   ---------------------------------------
   declare
      Short_Name : constant String :=
         Tash.File.Get_Attribute
           (Name => Test_File_Name,
            Attr => Tash.File.ShortName);
   begin
      if Platform = "windows" then
         Tash.Test.Test_Case
           (Description => "get the ShortName attribute of a file",
            Result      => Short_Name = "TEST_F~1.TXT");
      else
         Tash.Test.Test_Case
           (Description => "get the ShortName attribute of a file",
            Result      => Short_Name = "");
      end if;
   end;

   --  check that the file is writable
   --------------------------------------
   Tash.Test.Test_Case
     (Description => "check that the test file is writable",
      Result      => Tash.File.Writable (Test_File_Name));

   --  Make the test file read only
   -------------------------------
   if Platform = "windows" then
      Tash.Test.Test_Case
        (Description => "check that the test file is not read only",
         Result      => True);
      Tash.File.Set_Attribute
        (Name  => Test_File_Name,
         Attr  => Tash.File.ReadOnly,
         Value => "1");
      Tash.Test.Test_Case
        (Description => "make the test file read only",
         Result      =>
           Tash.File.Get_Attribute (Test_File_Name, Tash.File.ReadOnly) =
           "1");
   else
      Tash.File.Set_Attribute
        (Name  => Test_File_Name,
         Attr  => Tash.File.Permissions,
         Value => "00444");
      Tash.Test.Test_Case
        (Description => "make the test file read only",
         Result      =>
           Tash.File.Get_Attribute (Test_File_Name, Tash.File.Permissions) =
           "00444",
         Failure_Msg =>
            Tash.File.Get_Attribute (Test_File_Name, Tash.File.Permissions));
   end if;

   --  check that the file is not writable
   --------------------------------------
   Tash.Test.Test_Case
     (Description => "check that the test file is not writable",
      Result      => not Tash.File.Writable (Test_File_Name));

   --  check that a non-existent file is not writable
   -------------------------------------------------
   Tash.Test.Test_Case
     (Description => "check that a non-existent file is not writable",
      Result      => not Tash.File.Writable ("no_such_file.txt"));

   --  Put file back to allow writes so that the Text_IO.Create
   --  above will succeed next time we execute this program.
   -----------------------------------------------------------
   Tash.File.Set_Attribute
     (Name  => Test_File_Name,
      Attr  => Tash.File.ReadOnly,
      Value => "0");
   Tash.File.Set_Attribute
     (Name  => Test_File_Name,
      Attr  => Tash.File.Permissions,
      Value => "00644");

   --  copy a file
   --------------
   Tash.File.Copy (Test_File_Name, "test_file2.txt");
   Tash.Test.Test_Case
     (Description => "copy a file",
      Result      => Tash.File.Exists ("test_file2.txt"));

   --  copy file again to test File_Already_Exists exception
   --------------------------------------------------------
   declare
      Description : constant String :=
         "copy file again to test File_Already_Exists exception";
   begin
      Tash.File.Copy (Test_File_Name, "test_file2.txt");
      Tash.Test.Test_Case (Description => Description, Result => False);
   exception
      when Tash.File.File_Already_Exists =>
         Tash.Test.Test_Case (Description => Description, Result => True);
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  copy file again to test Force = True
   ---------------------------------------
   declare
      Description : constant String := "copy file again to test Force = True";
   begin
      Tash.File.Copy (Test_File_Name, "test_file2.txt", Force => True);
      Tash.Test.Test_Case (Description => Description, Result => True);
   exception
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  delete the file just copied
   ------------------------------
   Tash.File.Delete ("test_file2.txt");
   Tash.Test.Test_Case
     (Description => "delete a file",
      Result      => not Tash.File.Exists ("test_file2.txt"));

   --  get directory name of a file
   -------------------------------
   Tash.Test.Test_Case
     (Description => "get directory name of a file",
      Result      => Tash.File.Directory_Name (Test_File_Name) = ".");

   --  get directory name of a directory
   ------------------------------------
   Tash.Test.Test_Case
     (Description => "get directory name of a directory",
      Result      => Tash.File.Directory_Name ("../tests") = "..");

   --  test file is executable function
   -----------------------------------
   if Platform = "windows" then
      Tash.Test.Test_Case
        (Description => "test file is executable function",
         Result      => Tash.File.Executable ("test_file.exe"));
   else
      Tash.Test.Test_Case
        (Description => "test file is executable function",
         Result      => Tash.File.Executable ("test_file"));
   end if;

   --  get file extension
   ---------------------
   Tash.Test.Test_Case
     (Description => "get file extension",
      Result      => Tash.File.Extension ("test_file.exe") = ".exe");
   Tash.Test.Test_Case
     (Description => "get file extension",
      Result      => Tash.File.Extension (Test_File_Name) = ".txt");
   Tash.Test.Test_Case
     (Description => "get file extension",
      Result      => Tash.File.Extension ("test_file") = "");
   Tash.Test.Test_Case
     (Description => "get file extension",
      Result      => Tash.File.Extension ("test.file.exe") = ".exe");

   --  test whether a file is a directory
   -----------------------------
   Tash.Test.Test_Case
     (Description => "test whether a file is a directory",
      Result      => not Tash.File.Is_Directory (Test_File_Name));

   --  test whether a directory is a directory
   ------------------------------------------
   Tash.Test.Test_Case
     (Description => "test whether a directory is a directory",
      Result      => Tash.File.Is_Directory ("../src"));

   --  test whether a non-existent file is a directory
   --------------------------------------------------
   Tash.Test.Test_Case
     (Description => "test whether a non-existent file is a directory",
      Result      => not Tash.File.Is_Directory ("no_such_file.txt"));

   --  test whether a file is a regular file
   ----------------------------------------
   Tash.Test.Test_Case
     (Description => "test whether a file is a regular file",
      Result      => Tash.File.Is_File (Test_File_Name));

   --  test whether a directory is a regular file
   ---------------------------------------------
   Tash.Test.Test_Case
     (Description => "test whether a directory is a regular file",
      Result      => not Tash.File.Is_File ("../src"));

   --  test whether a non-existent file is a regular file
   -----------------------------------------------------
   Tash.Test.Test_Case
     (Description => "test whether a non-existent file is a regular file",
      Result      => not Tash.File.Is_File ("no_such_file.txt"));

   --  join path elements into a file name
   --------------------------------------
   Tash.Test.Test_Case
     (Description => "join path elements into a file name 1",
      Result      => Tash.File.Join ("/") = "/");
   Tash.Test.Test_Case
     (Description => "join path elements into a file name 2",
      Result      => Tash.File.Join ("//") = "/");
   Tash.Test.Test_Case
     (Description => "join path elements into a file name 3",
      Result      =>
        Tash.File.Join ("1", "2", "3", "4", "5", "6", "7", "8", "9") =
        "1/2/3/4/5/6/7/8/9");
   Tash.Test.Test_Case
     (Description => "join path elements into a file name 4",
      Result      =>
        Tash.File.Join ("/1", "2", "3", "4", "5", "6", "7", "8", "9") =
        "/1/2/3/4/5/6/7/8/9");
   Tash.Test.Test_Case
     (Description => "join path elements into a file name 5",
      Result      =>
        Tash.File.Join ("1", "2", "3", "4", "5", "6", "/7", "8", "9") =
        "/7/8/9");
   Tash.Test.Test_Case
     (Description => "join path elements into a file name 6",
      Result      => Tash.File.Join (".") = ".");
   Tash.Test.Test_Case
     (Description => "join path elements into a file name 7",
      Result      => Tash.File.Join ("./") = ".");
   Tash.Test.Test_Case
     (Description => "join path elements into a file name 8",
      Result      => Tash.File.Join (Tash.Lists.To_Tash_List ("/usr X lib")) =
                     "/usr/X/lib");

   --  get link status
   ------------------
   --  declare
   --   Status : Tash.Arrays.Tash_Array;
   --  begin
   --   Tash.File.Get_Link_Status (Test_File_Name, Status);
   --   Tash.Arrays.PArray (Status);
   --  end;

   --  create a directory
   ---------------------
   declare
      New_Dir     : constant String := "new_directory";
      Dir_Hier    : constant String := "dir/hier";
      Description : constant String :=
         "create a directory whose name already exists as a file";
   begin
      Tash.File.Make_Directory (New_Dir);
      Tash.Test.Test_Case
        (Description => "create a directory -- exists",
         Result      => Tash.File.Exists (New_Dir));
      Tash.Test.Test_Case
        (Description => "create a directory -- is a directory",
         Result      => Tash.File.Is_Directory (New_Dir));
      Tash.File.Make_Directory (Dir_Hier);
      Tash.Test.Test_Case
        (Description => "create a nested directory -- exists",
         Result      => Tash.File.Exists (Dir_Hier));
      Tash.Test.Test_Case
        (Description => "create a nested directory -- is a directory",
         Result      => Tash.File.Is_Directory (Dir_Hier));
      if Platform = "windows" then
         Tash.File.Make_Directory ("test_file.exe");
      else
         Tash.File.Make_Directory ("test_file");
      end if;
      Tash.Test.Test_Case (Description => Description, Result => False);
   exception
      when Tash.File.File_Already_Exists =>
         Tash.Test.Test_Case (Description => Description, Result => True);
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  Set the modified time of a file
   --------------------------------
   New_Modified_Time :=
      Ada.Calendar.Time_Of
        (Year    => 2000,
         Month   => 5,
         Day     => 6,
         Seconds => 7000.0);
   Tash.File.Set_Modified_Time
     (Name => Test_File_Name,
      Date => New_Modified_Time);

   --  Get the modified time of a file
   --------------------------------
   File_Modified_Time := Tash.File.Get_Modified_Time (Test_File_Name);
   Tash.Test.Test_Case
     (Description => "get and set modified time of a newly created file",
      Result      => New_Modified_Time = File_Modified_Time);

   --  Set the modified time of a non-existent file
   ---------------------------------------------
   declare
      Description : constant String :=
         "set modified time of a non-existent file";
   begin
      Tash.File.Set_Modified_Time
        (Name => "no_such_file.txt",
         Date => New_Modified_Time);
      Tash.Test.Test_Case (Description => Description, Result => False);
   exception
      when Tash.File.No_Such_File =>
         Tash.Test.Test_Case (Description => Description, Result => True);
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  Get the modified time of a non-existent file
   ---------------------------------------------
   declare
      Description : constant String :=
         "get modified time of a non-existent file";
   begin
      File_Modified_Time := Tash.File.Get_Modified_Time ("no_such_file.txt");
      Tash.Test.Test_Case (Description => Description, Result => False);
   exception
      when Tash.File.No_Such_File =>
         Tash.Test.Test_Case (Description => Description, Result => True);
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  get native name of a file
   ----------------------------
   declare
      Native_Name : constant String :=
         Tash.File.Native_Name ("./" & Test_File_Name);
   begin
      if Platform = "windows" then
         Tash.Test.Test_Case
           (Description => "get native name of a file",
            Result      => Native_Name = ".\" & Test_File_Name);
      else
         Tash.Test.Test_Case
           (Description => "get native name of a file",
            Result      => Native_Name = "./" & Test_File_Name);
      end if;
   end;

   --  test file owned function
   ----------------------------
   Tash.Test.Test_Case
     (Description => "test file owned function",
      Result      => Tash.File.Owned (Test_File_Name));

   --  test file does not exist function
   ------------------------------------
   Tash.Test.Test_Case
     (Description => "test file does not exist function",
      Result      => not Tash.File.Owned ("file_does_not_exist.txt"));

   --  get path type of absolute file
   ---------------------------------
   if Platform = "windows" then
      Tash.Test.Test_Case
        (Description => "get path type of absolute file",
         Result      => Tash.File.Get_Path_Type ("C:/windows") =
                        Tash.File.Absolute);
   else
      Tash.Test.Test_Case
        (Description => "get path type of absolute file",
         Result      => Tash.File.Get_Path_Type ("/tmp") =
                        Tash.File.Absolute);
   end if;

   --  get path type of relative file
   ---------------------------------
   if Platform = "windows" then
      Tash.Test.Test_Case
        (Description => "get path type of relative file",
         Result      => Tash.File.Get_Path_Type ("..\src") =
                        Tash.File.Relative);
   else
      Tash.Test.Test_Case
        (Description => "get path type of relative file",
         Result      => Tash.File.Get_Path_Type ("../src") =
                        Tash.File.Relative);
   end if;

   --  get path type of relative file
   ---------------------------------
   Tash.Test.Test_Case
     (Description => "get path type of relative file",
      Result      => Tash.File.Get_Path_Type ("windows") =
                     Tash.File.Relative);

   --  get path type of volume relative file
   ----------------------------------------
   if Platform = "windows" then
      Tash.Test.Test_Case
        (Description => "get path type of volume relative file",
         Result      => Tash.File.Get_Path_Type ("/windows") =
                        Tash.File.VolumeRelative);
   end if;

   --  test file readable function
   ----------------------------
   Tash.Test.Test_Case
     (Description => "test file readable function",
      Result      => Tash.File.Readable (Test_File_Name));

   --  test file does not exist function
   ------------------------------------
   Tash.Test.Test_Case
     (Description => "test file does not exist function",
      Result      => not Tash.File.Readable ("file_does_not_exist.txt"));

   --  Windows shortcut is not a symbolic link
   ------------------------------------------
   Tash.Test.Test_Case
     (Description => "Windows shortcut is not a symbolic link",
      Result      => not Tash.File.Is_Link ("Shortcut.txt"));

   --  read link of Windows Shortcut
   --------------------------------
   Tash.Test.Test_Case
     (Description => "read link of Windows Shortcut",
      Result      => Tash.File.Read_Link ("Shortcut.txt.lnk") =
                     "Shortcut.txt.lnk");

   --  read link of a regular file
   ------------------------------
   Tash.Test.Test_Case
     (Description => "read link of a regular file",
      Result      => Tash.File.Read_Link (Test_File_Name) = Test_File_Name);

   --  read link of a non-existent file
   -----------------------------------
   declare
      Description : constant String := "read link of a non-existent file";
   begin
      Tash.Test.Test_Case
        (Description => Description,
         Result      => Tash.File.Read_Link ("no_such_file.txt") = "");
   exception
      when Tash.File.No_Such_File =>
         Tash.Test.Test_Case (Description => Description, Result => True);
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  copy a file so we can rename it
   ----------------------------------
   Tash.File.Copy (Test_File_Name, "test_file2.txt");
   Tash.Test.Test_Case
     (Description => "copy a file so we can rename it",
      Result      => Tash.File.Exists ("test_file2.txt"));

   --  rename a file
   ----------------
   Tash.File.Rename ("test_file2.txt", "test_file3.txt");
   Tash.Test.Test_Case
     (Description => "rename a file",
      Result      => not Tash.File.Exists ("test_file2.txt"));
   Tash.Test.Test_Case
     (Description => "rename a file",
      Result      => Tash.File.Exists ("test_file3.txt"));

   --  rename file again to test File_Already_Exists exception
   --------------------------------------------------------
   declare
      Description : constant String :=
         "rename file again to test File_Already_Exists exception";
   begin
      Tash.File.Copy (Test_File_Name, "test_file2.txt");
      Tash.File.Rename ("test_file2.txt", "test_file3.txt");
      Tash.Test.Test_Case (Description => Description, Result => False);
   exception
      when Tash.File.File_Already_Exists =>
         Tash.Test.Test_Case (Description => Description, Result => True);
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  rename file again to test Force = True
   -----------------------------------------
   Tash.File.Rename ("test_file2.txt", "test_file3.txt", Force => True);
   Tash.Test.Test_Case
     (Description => "rename file again to test Force = True",
      Result      => Tash.File.Exists ("test_file3.txt"));

   --  delete the file just renamed
   ------------------------------
   Tash.File.Delete ("test_file3.txt");
   Tash.Test.Test_Case
     (Description => "delete a file",
      Result      => not Tash.File.Exists ("test_file3.txt"));

   --  get file root name
   ---------------------
   Tash.Test.Test_Case
     (Description => "get file root name",
      Result      => Tash.File.Root_Name ("test_file.exe") = "test_file");
   Tash.Test.Test_Case
     (Description => "get file root name",
      Result      => Tash.File.Root_Name (Test_File_Name) = "test_file");
   Tash.Test.Test_Case
     (Description => "get file root name",
      Result      => Tash.File.Root_Name ("test_file") = "test_file");
   Tash.Test.Test_Case
     (Description => "get file root name",
      Result      => Tash.File.Root_Name ("./test.file.exe") =
                     "./test.file");
   Tash.Test.Test_Case
     (Description => "get file root name",
      Result      => Tash.File.Root_Name ("../tests/test_file.exe") =
                     "../tests/test_file");
   Tash.Test.Test_Case
     (Description => "get file root name",
      Result      => Tash.File.Root_Name ("C:\windows") = "C:\windows");

   --  get size of a file
   ---------------------
   if Platform = "windows" then
      Tash.Test.Test_Case
        (Description => "get size of a file",
         Result      => Tash.File.Size (Test_File_Name) = 43);
   else
      Tash.Test.Test_Case
        (Description => "get size of a file",
         Result      => Tash.File.Size (Test_File_Name) = 42);
   end if;

   --  get size of non-existent file
   --------------------------------
   declare
      Description : constant String := "get size of non-existent file";
   begin
      Tash.Test.Test_Case
        (Description => Description,
         Result      => Tash.File.Size ("no_such_file.txt") = 0);
   exception
      when Tash.File.No_Such_File =>
         Tash.Test.Test_Case (Description => Description, Result => True);
      when An_Unexpected_Error_Occurred : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information
               (An_Unexpected_Error_Occurred));
         Tash.Test.Test_Case (Description => Description, Result => False);
   end;

   --  split a file name into its path components
   ---------------------------------------------
   Tash.Test.Test_Case
     (Description => "split a file name into its path components 1",
      Result      => Tash.File.Split (Test_File_Name) =
                     Tash.Lists.To_Tash_List (Test_File_Name));
   Tash.Test.Test_Case
     (Description => "split a file name into its path components 2",
      Result      => Tash.File.Split ("../test_file.txt") =
                     Tash.Lists.To_Tash_List (".. test_file.txt"));
   Tash.Test.Test_Case
     (Description => "split a file name into its path components 3",
      Result      => Tash.File.Split ("../tests/test_file.txt") =
                     Tash.Lists.To_Tash_List (".. tests test_file.txt"));
   Tash.Test.Test_Case
     (Description => "split a file name into its path components 4",
      Result      => Tash.File.Split ("/tash/tash834b/tests/test_file.txt") =
                     Tash.Lists.To_Tash_List
                        ("/ tash tash834b tests test_file.txt"));

   --  get tail of a file name
   --------------------------
   Tash.Test.Test_Case
     (Description => "get tail of a file name",
      Result      => Tash.File.Tail ("C:/tash/tash834b/tests/test_file.txt") =
                     "test_file.txt");
   Tash.Test.Test_Case
     (Description => "get tail of a file name",
      Result      => Tash.File.Tail ("./test_file.txt") = "test_file.txt");
   Tash.Test.Test_Case
     (Description => "get tail of a file name",
      Result      => Tash.File.Tail ("../tests/test_file.txt") =
                     "test_file.txt");

   --  get file type of a file
   --------------------------
   Tash.Test.Test_Case
     (Description => "get file type of a file",
      Result      => Tash.File.Get_File_Type (Test_File_Name) =
                     Tash.File.File);

   --  get file type of a directory
   -------------------------------
   Tash.Test.Test_Case
     (Description => "get file type of a directory",
      Result      => Tash.File.Get_File_Type ("dir") = Tash.File.Directory);
   Tash.Test.Test_Case
     (Description => "get file type of a directory",
      Result      => Tash.File.Get_File_Type ("dir/hier") =
                     Tash.File.Directory);

   --  get list of volumes on this computer
   ---------------------------------------
   if Platform = "windows" then
      Tash.Test.Test_Case
        (Description => "get list of volumes on this computer",
         Result      => Tash.Lists.Slice (Tash.File.Volume, 1, 3) =
                        Tash.Lists.To_Tash_List ("A:/ C:/ D:/"),
         Failure_Msg => "Expected: ""A:/ C:/ D:/""  Got: """ &
                        Tash.Lists.To_String
                           (Tash.Lists.Slice (Tash.File.Volume, 1, 3)) &
                        """");
   else
      Tash.Test.Test_Case
        (Description => "get list of volumes on this computer",
         Result      => Tash.File.Volume = Tash.Lists.To_Tash_List ("/"),
         Failure_Msg => "Expected: ""/""  Got: """ &
                        Tash.Lists.To_String (Tash.File.Volume) &
                        """");
   end if;

   declare
      Cwd : constant String := Tash.File.Current_Working_Directory;
   begin

      --  change directory
      -------------------
      Tash.File.Change_Directory ("dir");
      Tash.Test.Test_Case
        (Description => "change directory 1",
         Result      => Tash.File.Pwd = Tash.File.Join (Cwd, "dir"));
      Tash.File.Change_Directory ("hier");
      Tash.Test.Test_Case
        (Description => "change directory 2",
         Result      => Tash.File.Pwd = Tash.File.Join (Cwd, "dir", "hier"));

      --  change back to original tests directory
      ------------------------------------------
      Tash.File.Change_Directory (Cwd);
      Tash.Test.Test_Case
        (Description => "get current working directory",
         Result      => Tash.File.Current_Working_Directory = Cwd);

   end;

   --  simple file name match
   --------------------------
   --  If this test is run in a tree checked out from CVS, there will
   --  be CVS directories to be accounted for. Note that the order in
   --  which directory entries is returned is OS-dependent; Linux is a
   --  straight ASCII sort, MacOS X is case-independent, Solaris
   --  appears not to sort.
   declare
      Directories : constant Tash.Lists.Tash_List
        := Tash.Lists.Sort (Tash.File.Match ("../demos/*.tcl"));
      Expected : constant String :=
        "../demos/freq.tcl" &
        " " &
        "../demos/futurevalue.tcl" &
        " " &
        "../demos/testfreq.tcl" &
        " " &
        "../demos/timer.tcl";
   begin
      Tash.Test.Test_Case
        (Description => "simple file name match",
         Result      => Directories = Tash.Lists.To_Tash_List (Expected),
         Failure_Msg => "Expected: """ &
           Expected &
           """  Got: """ &
           Tash.Lists.To_String (Directories) &
           """");
   end;

   Tash.Test.Test_Case
     (Description => "simple file name match",
      Result      => Tash.File.Match ("*.tcl") =
                     Tash.Lists.To_Tash_List ("tashtest.tcl"));

   --  specify directory in file name match
   ----------------------------------------
   declare
      Directories : constant Tash.Lists.Tash_List
        := Tash.Lists.Sort (Tash.File.Match ("*.tcl",
                                             Directory => "../demos"));
      Expected : constant String :=
        "../demos/freq.tcl" &
        " " &
        "../demos/futurevalue.tcl" &
        " " &
        "../demos/testfreq.tcl" &
        " " &
        "../demos/timer.tcl";
   begin
      Tash.Test.Test_Case
        (Description => "specify directory in file name match",
         Result      => Directories = Tash.Lists.To_Tash_List (Expected),
         Failure_Msg => "Expected: """ &
           Expected &
           """  Got: """ &
           Tash.Lists.To_String (Directories) &
           """");
   end;

   --  specify path prefix in file name match
   ------------------------------------------
   declare
      Files : constant Tash.Lists.Tash_List
        := Tash.Lists.Sort (Tash.File.Match ("*.tcl",
                                             Path_Prefix => "../demos/"));
      Expected : constant String :=
        "../demos/freq.tcl" &
        " " &
        "../demos/futurevalue.tcl" &
        " " &
        "../demos/testfreq.tcl" &
        " " &
        "../demos/timer.tcl";
   begin
      Tash.Test.Test_Case
        (Description => "specify path prefix in file name match",
         Result      => Files = Tash.Lists.To_Tash_List (Expected),
         Failure_Msg => "Expected: """ &
           Expected &
           """  Got: """ &
           Tash.Lists.To_String (Files) &
           """");
   end;

   --  find all directories in current directory
   ---------------------------------------------
   declare
      Directories : Tash.Lists.Tash_List
         := Tash.Lists.Sort (Tash.File.Match ("*", Type_List => "d"));
   begin
      case Tash.Lists.Length (Directories) is
         when 2 => null;
         when 3 => Directories := Tash.Lists.Slice (Directories, 2, 3);
         when others => null;  -- will fail
      end case;
      Tash.Test.Test_Case
        (Description => "find all directories in current directory",
         Result      =>
           Directories =
           Tash.Lists.To_Tash_List ("dir new_directory"),
         Failure_Msg => "Expected: ""dir new_directory""  Got: """ &
           Tash.Lists.To_String (Directories) &
           """");
   end;

   if Tash.Test.All_Test_Cases_Passed then
      Ada.Text_IO.Put_Line
        ("Test_File PASSED --" &
         Integer'Image (Tash.Test.Test_Case_Number) &
         " tests completed");
   else
      Ada.Text_IO.Put_Line ("Test_File FAILED");
   end if;

end Test_File;
