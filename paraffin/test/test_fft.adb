------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                            T E S T _ F F T                               --
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
with Ada.Long_Float_Text_IO;        use Ada.Long_Float_Text_IO;
with Ada.Command_Line;         use Ada;
with Ada.Real_Time;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
with  Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics;

with FFT_Unoptimized_Sequential;
with FFT_Sequential;
with FFT_Work_Sharing;
with FFT_Work_Seeking;

with Parallel_Test_Harness;
with Parallel;
--  with System.Storage_Elements;

procedure Test_FFT is

   Run_Sequential : Boolean := True;

   P : constant Natural := Natural'Value (Command_Line.Argument (1));
   N : constant Positive := 2**P;
   Print_Array : constant Boolean := (P <= 8);
   Scale_Results : Boolean := True;

   subtype Index_Type is Natural range 0 .. (2 * N) - 1;

   type Array_Type is array (Index_Type range <>) of Long_Float;

   package Unoptimized_Sequential is new
     FFT_Unoptimized_Sequential
       (Index_Type => Index_Type,
        Element_Type => Long_Float,
        Array_Type => Array_Type);

   package Sequential is new
     FFT_Sequential
       (Index_Type => Index_Type,
        Element_Type => Long_Float,
        Array_Type => Array_Type);

   package Work_Sharing is new
     FFT_Work_Sharing
       (Index_Type => Index_Type,
        Element_Type => Long_Float,
        Array_Type => Array_Type);

   package Work_Seeking is new
     FFT_Work_Seeking
       (Index_Type => Index_Type,
        Element_Type => Long_Float,
        Array_Type => Array_Type);

   procedure Print_List (List : Array_Type) is
   begin
      for I in 0 .. N - 1 loop
         Put (Item => List (2 * I), Aft => 4, Exp => 0);
         Put (", ");
         Put (Item => List (2 * I + 1), Aft => 4, Exp => 0);
         Put_Line (",");
      end loop;
   end Print_List;

   procedure Print_Usage is
   begin

      New_Line;
      Put_Line ("Usage:");
      New_Line;
      Put_Line ("   test_fft <P> {scale=TRUE {run_sequential=FALSE}}");
      Put_Line ("      where 2**P data elements will be used");
      Put_Line ("      where 'scale' is TRUE to indicate results" &
                  " are scaled, ");
      Put_Line ("        by the size of the array, FALSE otherwise");
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
                "Percentage of stack before " &
                      "stack safe work deferrals occur");
      Put_Line ("  WORKER_STORAGE_SIZE : " &
                "Stack size for workers");
      Put_Line ("  PARALLEL_DEBUG_ENABLED : " &
                "If compiled with pragma debug enabled, " &
                "logs debug info to console");

      New_Line;
      Put_Line ("Suggested inputs: ");
      Put_Line ("     test_fft 20");

   end Print_Usage;

   --  GNAT Compiler bug, false positive for warning
   pragma Warnings (Off, "*List*could be declared constant");
   --  Allocate the list from the heap, to allow larger arrays that could not
   --  be used on the default stack.
   List : access Array_Type := new Array_Type (Index_Type);
   pragma Warnings (On, "*List*could be declared constant");

   Start_Time : Real_Time.Time;
   Elapsed : Duration;

begin

   if Command_Line.Argument_Count = 0 then
      Print_Usage;
      return;
   end if;

   if Command_Line.Argument_Count > 1 then
      Scale_Results := Boolean'Value (Command_Line.Argument (2));
      if Command_Line.Argument_Count > 2 then
         if Command_Line.Argument_Count = 3 then
            Run_Sequential := Boolean'Value (Command_Line.Argument (3));
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
   Put_Line ("  PARALLEL_DEBUG_ENABLED=" &
             Boolean'Image
               (Parallel_Test_Harness.Debug_Logging));
   Put_Line ("  Min_Sequential_Length=" &
             Integer'Image (Work_Sharing.Min_Sequential_Length));
   Put_Line ("  N=" &
             Integer'Image (2**Integer'Value (Command_Line.Argument (1))));

   New_Line;
   Put_Line ("(- FFT Tests -)");
   New_Line;

   if not Run_Sequential then
      goto Skip_Sequential;
   end if;

   --  Load a sine wave for input processing
   --  Even elements are the real value, odd elements are the imaginary value
   for I in 0 .. N - 1 loop
      List (I * 2) := Sin (2.0 * Pi * Long_Float (I) / Long_Float (N));
      List (I * 2 + 1) := 0.0;
   end loop;

   if Print_Array then
      Put_Line ("The list before sorting is:");
      Print_List (List.all);
      New_Line;
      New_Line;
   end if;

   Put ("** Unoptimized Sequential FFT of 2**" &
        Command_Line.Argument (1) & " Float values");

   Start_Time := Real_Time.Clock;

   Unoptimized_Sequential.FFT (List.all, Scale_Results);

   Elapsed := Real_Time.To_Duration
     (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time));

   Put_Line
     (" Elapsed = " &
      Calendar.Formatting.Image (Elapsed_Time          => Elapsed,
                                 Include_Time_Fraction => True));

   if Print_Array then
      Put_Line ("The list after generating the FFT:");
      Print_List (List.all);
      New_Line;
      New_Line;

      if Scale_Results then
         --  Now perform the reverse transform
         Unoptimized_Sequential.FFT (List.all, False);

         Put_Line ("The list after inverse FFT transform:");
         Print_List (List.all);
         New_Line;
         New_Line;
      end if;
   end if;

   --  Load a sine wave for input processing
   --  Even elements are the real value, odd elements are the imaginary value
   for I in 0 .. N - 1 loop
      List (I * 2) := Sin (2.0 * Pi * Long_Float (I) / Long_Float (N));
      List (I * 2 + 1) := 0.0;
   end loop;

   if Print_Array then
      Put_Line ("The list before sorting is:");
      Print_List (List.all);
      New_Line;
      New_Line;
   end if;

   Put ("** Sequential FFT of 2**" &
        Command_Line.Argument (1) & " Float values");

   Start_Time := Real_Time.Clock;

   Sequential.FFT (List.all, Scale_Results);

   Elapsed := Real_Time.To_Duration
     (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time));

   Put_Line
     (" Elapsed = " &
      Calendar.Formatting.Image (Elapsed_Time          => Elapsed,
                                 Include_Time_Fraction => True));

--     Sequential.FFT (List.all, False);
--
--     --  Check Results
--     for I in 1 .. N - 1 loop
--        if Long_Float'Rounding (List (List'Last - 1 - (I - 1) * 2) * 1000.0)
--          / 1000.0 /=
--          Long_Float'Rounding (Sin (2.0 * Pi * Long_Float (I)
--               / Long_Float (N)) * 1000.0) / 1000.0 then
--           Put ("I=" & Integer'Image (I));
--           Put (", L(I)=");
--        Put (Item => List (List'Last - 1 - 2 * (I - 1)), Aft => 4, Exp => 0);
--           Put (", Sin=");
--           Put (Item => Sin (2.0 * Pi * Long_Float (I) / Long_Float (N)),
--                Aft => 4, Exp => 0);
--           New_Line;
--        end if;
--
--        if Long_Float'Rounding (List (List'Last - (I - 1) * 2)) /= 0.0 then
--           Put ("L(I+1) /= 0:");
--           Put (Item => List (List'Last - 2 * I), Aft => 4, Exp => 0);
--           New_Line;
--        end if;
--
--     end loop;

   if Print_Array then
      Put_Line ("The list after generating the FFT:");
      Print_List (List.all);
      New_Line;
      New_Line;

      if Scale_Results then
         --  Now perform the reverse transform
         Sequential.FFT (List.all, False);

         Put_Line ("The list after inverse FFT transform:");
         Print_List (List.all);
         New_Line;
         New_Line;
      end if;
   end if;

   <<Skip_Sequential>>

   --  Work Sharing

   --  Load a sine wave for input processing
   --  Even elements are the real value, odd elements are the imaginary value
   for I in 0 .. N - 1 loop
      List (I * 2) := Sin (2.0 * Pi * Long_Float (I) / Long_Float (N));
      List (I * 2 + 1) := 0.0;
   end loop;

   if Print_Array then
      Put_Line ("The list before sorting is:");
      Print_List (List.all);
      New_Line;
      New_Line;
   end if;

   Put ("** Work Sharing FFT of 2**" &
        Command_Line.Argument (1) & " Float values");

   Start_Time := Real_Time.Clock;

   Work_Sharing.FFT (List.all, P, Scale_Results);

   Elapsed := Real_Time.To_Duration
     (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time));

   Put_Line
     (" Elapsed = " &
      Calendar.Formatting.Image (Elapsed_Time          => Elapsed,
                                 Include_Time_Fraction => True));

--     Work_Sharing.FFT (List.all, False);
--
--     --  Check Results
--     for I in 1 .. N - 1 loop
--        if Long_Float'Rounding (List (List'Last - 1 - (I - 1) * 2) * 1000.0)
--          / 1000.0 /=
--          Long_Float'Rounding (Sin (2.0 * Pi * Long_Float (I)
--               / Long_Float (N)) * 1000.0) / 1000.0 then
--           Put ("I=" & Integer'Image (I));
--           Put (", L(I)=");
--       Put (Item => List (List'Last - 1 - 2 * (I - 1)), Aft => 4, Exp => 0);
--           Put (", Sin=");
--           Put (Item => Sin (2.0 * Pi * Long_Float (I) / Long_Float (N)),
--                Aft => 4, Exp => 0);
--           New_Line;
--        end if;
--
--        if Long_Float'Rounding (List (List'Last - (I - 1) * 2)) /= 0.0 then
--           Put ("L(I+1) /= 0:");
--           Put (Item => List (List'Last - 2 * I), Aft => 4, Exp => 0);
--           New_Line;
--        end if;
--
--     end loop;

   if Print_Array then
      Put_Line ("The list after generating the FFT:");
      Print_List (List.all);
      New_Line;
      New_Line;

      if Scale_Results then
         --  Now perform the reverse transform
         Work_Sharing.FFT (List.all, P, False);

         Put_Line ("The list after inverse FFT transform:");
         Print_List (List.all);
         New_Line;
         New_Line;
      end if;
   end if;

   --  Work Seeking

   --  Load a sine wave for input processing
   --  Even elements are the real value, odd elements are the imaginary value
   for I in 0 .. N - 1 loop
      List (I * 2) := Sin (2.0 * Pi * Long_Float (I) / Long_Float (N));
      List (I * 2 + 1) := 0.0;
   end loop;

   if Print_Array then
      Put_Line ("The list before sorting is:");
      Print_List (List.all);
      New_Line;
      New_Line;
   end if;

   Put ("** Work Seeking FFT of 2**" &
        Command_Line.Argument (1) & " Float values");

   Start_Time := Real_Time.Clock;

   Work_Seeking.FFT (List.all, P, Scale_Results);

   Elapsed := Real_Time.To_Duration
     (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time));

   Put_Line
     (" Elapsed = " &
      Calendar.Formatting.Image (Elapsed_Time          => Elapsed,
                                 Include_Time_Fraction => True));

--     Work_Seeking.FFT (List.all, False);
--
--   --  Check Results
--     for I in 1 .. N - 1 loop
--        if Long_Float'Rounding (List (List'Last - 1 - (I - 1) * 2) * 1000.0)
--          / 1000.0 /=
--          Long_Float'Rounding (Sin (2.0 * Pi * Long_Float (I)
--               / Long_Float (N)) * 1000.0) / 1000.0 then
--           Put ("I=" & Integer'Image (I));
--           Put (", L(I)=");
--       Put (Item => List (List'Last - 1 - 2 * (I - 1)), Aft => 4, Exp => 0);
--           Put (", Sin=");
--           Put (Item => Sin (2.0 * Pi * Long_Float (I) / Long_Float (N)),
--                Aft => 4, Exp => 0);
--           New_Line;
--        end if;
--
--        if Long_Float'Rounding (List (List'Last - (I - 1) * 2)) /= 0.0 then
--           Put ("L(I+1) /= 0:");
--           Put (Item => List (List'Last - 2 * I), Aft => 4, Exp => 0);
--           New_Line;
--        end if;
--
--     end loop;

   if Print_Array then
      Put_Line ("The list after generating the FFT:");
      Print_List (List.all);
      New_Line;
      New_Line;

      if Scale_Results then
         --  Now perform the reverse transform
         Work_Seeking.FFT (List.all, P, False);

         Put_Line ("The list after inverse FFT transform:");
         Print_List (List.all);
         New_Line;
         New_Line;
      end if;
   end if;

exception

   when E : others =>
      Put_Line ("Caught Exception: " & Exceptions.Exception_Information (E));
      Print_Usage;
      raise;
end Test_FFT;
