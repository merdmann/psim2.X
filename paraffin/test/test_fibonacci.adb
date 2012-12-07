------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                      T E S T _ F I B O N A C C I                         --
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

--  with Fibonacci;  -- Work-Sharing Recursive
with Fibonacci_Elementary_Work_Seeking;
with Fibonacci_Elementary_Depth_Work_Seeking;
with Fibonacci_Sequential_Recursive;
with Fibonacci_Sequential_Iterative;
with Fibonacci_Composite_Sequential_Recursive;
with Fibonacci_Composite_Work_Seeking;
with Fibonacci_Composite_Work_Sharing;
with Fibonacci_Elementary_Work_Sharing;
with Fibonacci_Stack_Safe_Work_Seeking;
with Fibonacci_Composite_Stack_Safe_Work_Seeking;
with Sequential_Double_Fibonacci;
with Double_Fibonacci;
with Parallel.Recursion;
with Parallel_Test_Harness;

procedure Test_Fibonacci is
   Start_Time : Real_Time.Time := Real_Time.Clock;
   Deferral_Count : aliased Parallel.Recursion.Stack_Limit_Count;
   Result : Natural;
begin

   New_Line;
   Put_Line ("(- Functional Fibonacci Integer Reduction -)");
   New_Line;

   Put ("** Sequential Iterative Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Put_Line (Natural'Image
        (Fibonacci_Sequential_Iterative
           (Integer'Value (Command_Line.Argument (2)))) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Sequential Recursive Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Start_Time := Real_Time.Clock;

   Put_Line
     (Natural'Image
        (Fibonacci_Sequential_Recursive
           (Integer'Value (Command_Line.Argument (2)))) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Recursive Elementary Work Sharing Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Start_Time := Real_Time.Clock;

   Put_Line
     (Natural'Image
        (Fibonacci_Elementary_Work_Sharing
           (Integer'Value (Command_Line.Argument (2)))) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Recursive Elementary Work Seeking Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Start_Time := Real_Time.Clock;

   Put_Line
     (Natural'Image
        (Fibonacci_Elementary_Work_Seeking
           (Integer'Value (Command_Line.Argument (2)))) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Recursive Elementary Work Seeking Depth Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Start_Time := Real_Time.Clock;

   Put_Line
     (Natural'Image
        (Fibonacci_Elementary_Depth_Work_Seeking
           (Integer'Value (Command_Line.Argument (2)))) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Recursive Stack Safe Elementary Work Seeking Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Start_Time := Real_Time.Clock;

   Put
     (Natural'Image
        (Fibonacci_Stack_Safe_Work_Seeking
           (Integer'Value (Command_Line.Argument (2)),
            Deferral_Count'Access)));

   Put_Line
     (", Stack Limit Reached" &
      Parallel.Recursion.Stack_Limit_Count'Image (Deferral_Count) &
      " times , Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   New_Line;
   Put_Line ("(- Procedural Fibonacci Integer Reduction -)");
   New_Line;

   Put ("** Sequential Composite Recursive Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Result := 0;
   Start_Time := Real_Time.Clock;

   Fibonacci_Composite_Sequential_Recursive
     (Integer'Value (Command_Line.Argument (2)),
      Result);

   Put_Line
     (Natural'Image (Result) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Recursive Composite Work Sharing Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Start_Time := Real_Time.Clock;

   Put_Line
     (Natural'Image
        (Fibonacci_Composite_Work_Sharing
           (Integer'Value (Command_Line.Argument (2)))) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Recursive Composite Work Seeking Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Start_Time := Real_Time.Clock;

   Put_Line
     (Natural'Image
        (Fibonacci_Composite_Work_Seeking
           (Integer'Value (Command_Line.Argument (2)))) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Recursive Stack Safe Composite Work Seeking Fibonacci of " &
        Command_Line.Argument (2) &
        " is");

   Start_Time := Real_Time.Clock;

   Fibonacci_Composite_Stack_Safe_Work_Seeking
     (Integer'Value (Command_Line.Argument (2)),
      Deferral_Count,
      Result);

   Put_Line
     (Natural'Image (Result) &
      ", Stack Limit Reached" &
      Parallel.Recursion.Stack_Limit_Count'Image (Deferral_Count) &
      " times , Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   if Parallel_Test_Harness.Run_Double_Fibonacci_Tests then

      New_Line;
      Put_Line ("(- Multiway Recursion - Double Fibonacci -)");
      New_Line;

      Put ("** Recursive Sequential Double Fibonacci of " &
           Command_Line.Argument (2) &
           " is");

      Start_Time := Real_Time.Clock;

      Put_Line
        (Natural'Image
           (Sequential_Double_Fibonacci
              (Integer'Value (Command_Line.Argument (2)))) &
         ", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                       (Left => Real_Time.Clock, Right => Start_Time)),
             Include_Time_Fraction => True));

      Put ("** Recursive Work Seeking Double Fibonacci of " &
           Command_Line.Argument (2) &
           " is");

      Start_Time := Real_Time.Clock;

      Put_Line
        (Natural'Image
           (Double_Fibonacci (Integer'Value (Command_Line.Argument (2)))) &
         ", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left => Real_Time.Clock, Right => Start_Time)),
             Include_Time_Fraction => True));

   end if;

end Test_Fibonacci;
