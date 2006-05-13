with Ada.Text_IO;
with Tash.Platform;

procedure Test_Platform is

begin --  Test_Platform

   Ada.Text_IO.Put_Line ("Byte_Order: " & Tash.Platform.Byte_Order);
   Ada.Text_IO.Put_Line ("Machine:    " & Tash.Platform.Machine);
   Ada.Text_IO.Put_Line ("OS:         " & Tash.Platform.OS);
   Ada.Text_IO.Put_Line ("OS_Version: " & Tash.Platform.OS_Version);
   Ada.Text_IO.Put_Line ("Platform:   " & Tash.Platform.Platform);
   Ada.Text_IO.Put_Line ("User:       " & Tash.Platform.User);

end Test_Platform;
