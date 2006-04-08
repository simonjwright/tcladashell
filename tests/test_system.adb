with Ada.Command_Line;

with Ada.Text_IO;

with Tash.System;

with Tash.Test;



procedure Test_System is



   use type Tash.System.Process_ID;



   Verbose : Boolean := False;



begin -- Test_System



   -------------------------------------------

   -- Check for -verbose command line argument

   -------------------------------------------

   GET_COMMAND_LINE_ARGUMENTS:

   for I in 1..Ada.Command_Line.Argument_Count loop

      if Ada.Command_Line.Argument (I) = "-verbose" then

         Verbose := True;

         exit GET_COMMAND_LINE_ARGUMENTS;

      end if;

   end loop GET_COMMAND_LINE_ARGUMENTS;



   Tash.Test.Set_Verbose (On => Verbose);



   -- get the process id of the current process

   --------------------------------------------

   declare

      Pid : Tash.System.Process_ID;

   begin

      Pid := Tash.System.Pid;

      Tash.Test.Test_Case (

         Description => "get the process id of the current process",

         Result      => Pid > 0,

         Failure_Msg => Tash.System.Process_ID'Image (Pid));

   end;



   <<Finish>>

   if Tash.Test.All_Test_Cases_Passed then

     Ada.Text_IO.Put_Line ("Test_System PASSED --" &

        Integer'Image (Tash.Test.Test_Case_Number) & " tests completed");

   else

      Ada.Text_IO.Put_Line ("Test_System FAILED");

   end if;



end Test_System;

