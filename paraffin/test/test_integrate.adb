------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                      T E S T _ I N T E G R A T E                         --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore                    --
--                                                                          --
--  Paraffin is free software;  you can  redistribute it  and/or modify it  --
--  under  terms of the  GNU General Public License  as  published  by the  --
--  Free Software  Foundation;  either version 2,  or (at your option) any  --
--  later  version.  Paraffin is  distributed in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU  --
--  General Public License for  more details.  You should have  received a  --
--  copy of the GNU General Public License distributed with Paraffin;  see  --
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,   --
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.         --
--                                                                          --
--  As a  special exception, if other files  instantiate generics from      --
--  this unit,  or you link this  unit with other files  to produce an      --
--  executable,  this unit  does  not by  itself  cause the  resulting      --
--  executable to be covered by  the GNU General Public License.  This      --
--  exception does  not however invalidate  any other reasons  why the      --
--  executable file might be covered by the GNU Public License.             --
------------------------------------------------------------------------------

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Command_Line;         use Ada;
with Ada.Real_Time;
with Ada.Calendar.Formatting;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Long_Float_Text_IO;   use Ada.Long_Float_Text_IO;
with System.Storage_Elements;

with Parallel.Recursion;
with Parallel_Test_Harness;
with Integrate_Sequential;
with Integrate_Recursive_Work_Seeking;
with Integrate_Recursive_Work_Sharing;
with Integrate_Recursive_Stack_Safe_Work_Seeking;

procedure Test_Integrate
is
   Start_Time : Real_Time.Time := Real_Time.Clock;
   Lower, Upper : Long_Float;
   Last_Lower, Last_Upper : Positive;

   procedure Display_Usage;

   function Sequential_Integrate is new Integrate_Sequential
     (Real => Long_Float);

   function Work_Sharing_Integrate is new Integrate_Recursive_Work_Sharing
     (Real => Long_Float);

   function Work_Seeking_Integrate is new Integrate_Recursive_Work_Seeking
     (Real => Long_Float);

   function Stack_Safe_Work_Seeking_Integrate is new
     Integrate_Recursive_Stack_Safe_Work_Seeking
       (Real => Long_Float);

   procedure Display_Usage is
   begin
      New_Line;
      Put_Line ("Usage:");
      New_Line;
      Put_Line ("test_integrate lower_bound upper_bound");
      Put_Line ("   where:");
      Put_Line ("       bounds are floating point values and lower < upper");
      New_Line;
      Put_Line ("Environment Variables:");
      New_Line;
      Put_Line ("  DEFAULT_WORKER_COUNT ; " &
                "Number of workers to use for each test");
      Put_Line ("  MAX_STACK_DEPTH : " &
                "No. of recursive calls before stack limited deferrals occur");
      Put_Line ("  WORKER_STORAGE_SIZE : " &
                "Stack size for workers");
      Put_Line ("  PARALLEL_DEBUG_ENABLED : " &
                "If compiled with pragma debug enabled, " &
                "logs debug info to console");
      New_Line;
      Put_Line ("Suggested inputs: ");
      Put_Line ("     test_integrate 1.0000 1.00001");
      New_Line;
      Put_Line ("NOTE: Larger ranges of values can take significantly");
      Put_Line ("         longer to run. If the program appears to be");
      Put_Line ("         hung, it likely is that the range specified is");
      Put_Line ("         too large, and is taking too long to complete.");
      New_Line;

   end Display_Usage;

begin

   if Command_Line.Argument_Count /= 2 then
      Display_Usage;
      return;
   end if;

   New_Line;
   Put_Line ("************* Parallel Framework Test *************");
   Put_Line ("  Physical Processors=" &
             Parallel.CPU_Count'Image (Parallel.Available_CPUs));
   Put_Line ("  DEFAULT_WORKER_COUNT=" &
             Parallel.Positive_Worker_Count'Image
               (Parallel.Default_Worker_Count));
   Put_Line ("  MAX_STACK_DEPTH=" &
             Parallel.Recursion.Stack_Percentage_Type'Image
               (Parallel_Test_Harness.Maximum_Recursive_Stack_Depth));
   Put_Line ("  WORKER_STORAGE_SIZE=" &
             System.Storage_Elements.Storage_Count'Image
               (Parallel_Test_Harness.Worker_Storage_Size));
   Put_Line ("  PARALLEL_DEBUG_ENABLED=" &
             Boolean'Image
               (Parallel_Test_Harness.Debug_Logging));
   Put_Line ("***************************************************");
   New_Line;

   Get (From => Command_Line.Argument (1),
        Item => Lower,
        Last => Last_Lower);

   Get (From => Command_Line.Argument (2),
        Item => Upper,
        Last => Last_Upper);

   pragma Assert (Lower < Upper);

   Put
     ("** Sequential Recursive Integrate of Sqrt from " &
      Command_Line.Argument (1) & " to " &
      Command_Line.Argument (2) &
      " is ");

   Put
     (Item => Sequential_Integrate
        (Numerics.Long_Elementary_Functions.Sqrt'Access, Lower, Upper));

   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Start_Time := Real_Time.Clock;

   Put
     ("** Work Sharing Recursive Integrate of Sqrt from " &
      Command_Line.Argument (1) & " to " &
      Command_Line.Argument (2) &
      " is ");

   Put
     (Item => Work_Sharing_Integrate
        (Numerics.Long_Elementary_Functions.Sqrt'Access, Lower, Upper));

   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Start_Time := Real_Time.Clock;

   Put
     ("** Work Seeking Recursive Integrate of Sqrt from " &
      Command_Line.Argument (1) & " to " &
      Command_Line.Argument (2) &
      " is ");

   Put
     (Item => Work_Seeking_Integrate
        (Numerics.Long_Elementary_Functions.Sqrt'Access, Lower, Upper));

   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Start_Time := Real_Time.Clock;

   Put
     ("** Stack Safe Work Seeking Recursive Integrate of Sqrt from " &
      Command_Line.Argument (1) & " to " &
      Command_Line.Argument (2) &
      " is ");

   Put
     (Item => Stack_Safe_Work_Seeking_Integrate
        (Numerics.Long_Elementary_Functions.Sqrt'Access, Lower, Upper));

   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

end Test_Integrate;
