------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                     T E S T _ Q U I C K S O R T                          --
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
with Ada.Numerics.Discrete_Random;

with Quicksort_Sequential;
with Quicksort_Work_Sharing;
with Quicksort_Work_Seeking;
with Quicksort_Stack_Safe_Work_Seeking;
with Parallel_Test_Harness;
with Parallel.Recursion;
with System.Storage_Elements;

procedure Test_Quicksort is

   Print_Array : Boolean := False;
   Reset : Boolean := True;
   Run_Sequential : Boolean := True;

   package Random_Number is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Short_Integer);

   subtype Index_Type is
     Natural range 1 .. Natural'Value (Command_Line.Argument (1));

   type Array_Type is array (Index_Type range <>) of Short_Integer;

   procedure Sequential_Quicksort is
     new Quicksort_Sequential (Index_Type => Index_Type,
                               Element_Type => Short_Integer,
                               Array_Type => Array_Type);

   procedure Work_Sharing_Quicksort is
     new Quicksort_Work_Sharing (Index_Type => Index_Type,
                                 Element_Type => Short_Integer,
                                 Array_Type => Array_Type);

   procedure Work_Seeking_Quicksort is
     new Quicksort_Work_Seeking (Index_Type => Index_Type,
                                 Element_Type => Short_Integer,
                                 Array_Type => Array_Type);

   procedure Stack_Safe_Work_Seeking_Quicksort is
     new Quicksort_Stack_Safe_Work_Seeking (Index_Type => Index_Type,
                                            Element_Type => Short_Integer,
                                            Array_Type => Array_Type);

   procedure Print_List (List : Array_Type) is
   begin
      for I in List'Range loop
         Put (Short_Integer'Image (List (I)) & ' ');
      end loop;
   end Print_List;

   Generator : Random_Number.Generator;

   --  GNAT Compiler bug, false positive for warning
   pragma Warnings (Off, "*List*could be declared constant");
   --  Allocate the list from the heap, to allow larger arrays that could not
   --  be used on the default stack.
   List : access Array_Type := new Array_Type (Index_Type);
   pragma Warnings (On, "*List*could be declared constant");

   Start_Time : Real_Time.Time;
   Elapsed : Duration;
   Stack_Deferalls : Parallel.Recursion.Stack_Limit_Count;

   procedure Print_Usage is
   begin
      New_Line;
      Put_Line ("Usage:");
      New_Line;
      Put_Line
        ("   test_quicksort <number_of_elements> " &
         "   {verbose=FALSE {reset=TRUE {run_sequential=FALSE}}}");

      Put_Line
        ("      where 'verbose' is TRUE to indicate verbose, FALSE otherwise");
      Put_Line
        ("      where 'reset' is true if random numbers generator is reset");
      Put_Line
        ("      where 'run_seq' is true if sequential test is to be run");

      New_Line;
      Put_Line ("Environment Variables:");
      New_Line;
      Put_Line ("  DEFAULT_WORKER_COUNT ; " &
                "Number of workers to use for each test");
      Put_Line ("  USE_AFFINITY : " &
                "Lock each worker to a specific physical processor");
      Put_Line ("  WORK_BUDGET_FOR_EVEN_LOADS : " &
                "No. of times work seeker may seek work, " &
                "for evenly balanced loads");
      Put_Line ("     -1 means unlimited ");
      Put_Line ("  MAX_STACK_LIMIT : " &
                "Percentage of stack before stack safe work deferrals occur");
      Put_Line ("  WORKER_STORAGE_SIZE : " &
                "Stack size for workers");
      Put_Line ("  PARALLEL_DEBUG_ENABLED : " &
                "If compiled with pragma debug enabled, " &
                "logs debug info to console");

      New_Line;
      Put_Line ("Suggested inputs: ");
      Put_Line ("     test_quicksort 1_000_000");

   end Print_Usage;

begin

   if Command_Line.Argument_Count = 0 then
      Print_Usage;
      return;
   end if;

   if Command_Line.Argument_Count > 1 then
      Print_Array := Boolean'Value (Command_Line.Argument (2));
      if Command_Line.Argument_Count > 2 then
         Reset := Boolean'Value (Command_Line.Argument (3));
         if Command_Line.Argument_Count = 4 then
            Run_Sequential := Boolean'Value (Command_Line.Argument (4));
         else
            Print_Usage;
            return;
         end if;
      end if;
   end if;

   Parallel_Test_Harness.Setup_Tests;

   New_Line;
   Put_Line ("************* Parallel Framework Test *************");
   Put_Line ("  Physical Processors=" &
             Parallel.CPU_Count'Image (Parallel.Available_CPUs));
   Put_Line ("  DEFAULT_WORKER_COUNT=" &
             Parallel.Positive_Worker_Count'Image
               (Parallel.Default_Worker_Count));
   Put_Line ("  USE_AFFINITY=" &
             Boolean'Image (Parallel_Test_Harness.Use_Affinity));
   Put_Line ("  WORK_BUDGET_FOR_EVEN_LOADS=" &
             Parallel.Work_Budget_Limit'Image
               (Parallel_Test_Harness.Work_Budget_For_Even_Loads));
   Put_Line ("  MAX_STACK_LIMIT=" &
             Natural'Image
               (Parallel_Test_Harness.Maximum_Recursive_Stack_Limit));
   Put_Line ("  WORKER_STORAGE_SIZE=" &
             System.Storage_Elements.Storage_Count'Image
               (Parallel_Test_Harness.Worker_Storage_Size));
   Put_Line ("  PARALLEL_DEBUG_ENABLED=" &
             Boolean'Image
               (Parallel_Test_Harness.Debug_Logging));

   New_Line;
   Put_Line ("(- Quicksort Tests -)");
   New_Line;

   if not Run_Sequential then
      goto Skip_Sequential;
   end if;

   if Reset then
      --  Start from a repeatable sequence
      Random_Number.Reset (Generator, Initiator => 0);
   end if;

   for I in List'Range loop
      List (I) := Random_Number.Random (Generator);
   end loop;

   if Print_Array then
      Put_Line ("The list before sorting is:");
      Print_List (List.all);
      New_Line;
      New_Line;
   end if;

   Put ("** Sequential Quicksort of " &
        Command_Line.Argument (1) &
        " Naturals.");

   Start_Time := Real_Time.Clock;

   Sequential_Quicksort (List.all);

   Elapsed := Real_Time.To_Duration
     (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time));

   Put_Line
     (" Elapsed = " &
      Calendar.Formatting.Image (Elapsed_Time          => Elapsed,
                                 Include_Time_Fraction => True));

   if Print_Array then
      Put_Line ("The list after sorting quicksort algorithm:");
      Print_List (List.all);
      New_Line;
      New_Line;
   end if;

   <<Skip_Sequential>>

   --  Work Sharing

   if Reset then
      --  Start from a repeatable sequence
      Random_Number.Reset (Generator, Initiator => 0);
   end if;

   for I in List'Range loop
      List (I) := Random_Number.Random (Generator);
   end loop;

   if Print_Array then
      Put_Line ("The list before sorting is:");
      Print_List (List.all);
      New_Line;
      New_Line;
   end if;

   Put ("** Work Sharing Quicksort of " &
        Command_Line.Argument (1) &
        " Naturals.");

   Start_Time := Real_Time.Clock;

   Work_Sharing_Quicksort (List.all);

   Elapsed := Real_Time.To_Duration
     (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time));

   Put_Line
     (" Elapsed = " &
      Calendar.Formatting.Image (Elapsed_Time          => Elapsed,
                                 Include_Time_Fraction => True));

   if Print_Array then
      Put_Line ("The list after sorting quicksort algorithm:");
      Print_List (List.all);
   end if;

   --  Work Seeking

   if Reset then
      --  Start from a repeatable sequence
      Random_Number.Reset (Generator, Initiator => 0);
   end if;

   for I in List'Range loop
      List (I) := Random_Number.Random (Generator);
   end loop;

   if Print_Array then
      Put_Line ("The list before sorting is:");
      Print_List (List.all);
      New_Line;
      New_Line;
   end if;

   Put ("** Work Seeking Quicksort of " &
        Command_Line.Argument (1) &
        " Naturals.");

   Start_Time := Real_Time.Clock;

   Work_Seeking_Quicksort (List.all);

   Elapsed := Real_Time.To_Duration
     (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time));

   Put_Line
     (" Elapsed = " &
      Calendar.Formatting.Image (Elapsed_Time          => Elapsed,
                                 Include_Time_Fraction => True));

   if Print_Array then
      Put_Line ("The list after sorting quicksort algorithm:");
      Print_List (List.all);
   end if;

   --  Stack Safe Work Seeking

   if Reset then
      --  Start from a repeatable sequence
      Random_Number.Reset (Generator, Initiator => 0);
   end if;

   for I in List'Range loop
      List (I) := Random_Number.Random (Generator);
   end loop;

   if Print_Array then
      Put_Line ("The list before sorting is:");
      Print_List (List.all);
      New_Line;
      New_Line;
   end if;

   Put ("** Stack Safe Work Seeking Quicksort of " &
        Command_Line.Argument (1) &
        " Naturals.");

   Start_Time := Real_Time.Clock;

   Stack_Safe_Work_Seeking_Quicksort (List.all, Stack_Deferalls);

   Elapsed := Real_Time.To_Duration
     (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time));

   Put_Line
     (" Elapsed = " &
      Calendar.Formatting.Image (Elapsed_Time          => Elapsed,
                                 Include_Time_Fraction => True) &
      ", Deferrals = " &
      Parallel.Recursion.Stack_Limit_Count'Image (Stack_Deferalls));

   if Print_Array then
      Put_Line ("The list after sorting quicksort algorithm:");
      Print_List (List.all);
   end if;

exception
   when others =>
      Print_Usage;
      raise;
end Test_Quicksort;
