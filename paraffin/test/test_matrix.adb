------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                            T E S T _ M A T R I X                         --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore, Jon Squire         --
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
with Ada.Calendar;             use Ada.Calendar;
with Ada.Command_Line;         use Ada;
with Ada.Numerics.Real_Arrays; use Ada.Numerics.Real_Arrays;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

with Parallel; use Parallel;
with Sequential_Gauss_Jordan;
with Work_Sharing_Gauss_Jordan;
with Parallel_Test_Harness;

procedure Test_Matrix
is
   subtype Real is Float;

   N          : Positive := 500; -- number of equations
   Start_Time : Duration;
   Verbose : Boolean  := False;
   Parallel_Only : Boolean := False;
   Float_Generator  : Generator;

   procedure Check (A : Real_Matrix; X : Real_Vector; Y : Real_Vector) is
      Sum  : Real := 0.0;
      Maxe : Real := 0.0;
      Diff : Real;
      Tmp  : Real;
   begin
      for I in 1 .. N loop
         Tmp := 0.0;
         for J in 1 .. N loop
            Tmp := Tmp + A (I, J) * X (J);
         end loop;
         Tmp  := Tmp - Y (I);
         Diff := abs (Tmp);
         if Diff > Maxe then
            Maxe := Diff;
         end if;
         Sum := Sum + Diff;
      end loop; -- while
      Put_Line
        ("check N=" &
         Integer'Image (N) &
         ", maxerr:=" &
         Real'Image (Maxe));
   end Check;

   -----------------------------------------------------------------------

   procedure Print_Inputs (A : Real_Matrix; Y : Real_Vector) is
   begin
         for I in 1 .. 5 loop
            for J in 1 .. 5 loop
               Put_Line
                 ("A" &
                  Integer'Image (I) &
                  "," &
                  Integer'Image (J) &
                  ")=" &
                  Real'Image (A (I, J)));
            end loop;
            Put_Line ("Y(" & Integer'Image (I) & Real'Image (Y (I)));
         end loop;
   end Print_Inputs;

   -----------------------------------------------------------------------

   procedure Print_Usage is
   begin

      New_Line;
      Put_Line ("Usage:");
      New_Line;
      Put_Line ("   test_matrix {-v} {-p} <N>");
      Put_Line ("      where -v indicates verbose mode ");
      Put_Line ("      where -p indicates parallel tests only");
      Put_Line ("      where an N x N matrix will be used");
      Put_Line ("          (default Value of N is" & Positive'Image (N) & ")");

      New_Line;
      Put_Line ("Environment Variables:");
      New_Line;
      Put_Line ("  DEFAULT_WORKER_COUNT ; " &
                "Number of workers to use for each test");
      Put_Line ("  PARALLEL_DEBUG_ENABLED : " &
                "If compiled with pragma debug enabled, " &
                "logs debug info to console");

      New_Line;
      Put_Line ("Suggested inputs: ");
      Put_Line ("     test_matrix 500");

   end Print_Usage;

begin

   Validate_Parameters :
   declare
      N_Assigned : Boolean := False;
   begin
      for I in 1 .. Command_Line.Argument_Count loop
         if Command_Line.Argument (I) = "-v" then
            Verbose := True;
         elsif Command_Line.Argument (I) = "-p" then
            Parallel_Only := True;
         elsif not N_Assigned then
            N := Positive'Value (Command_Line.Argument (I));
            N_Assigned := True;
         else
            Print_Usage;
            return;
         end if;
      end loop;
   exception
      when Constraint_Error =>
         Put_Line ("Invalid Inputs");
         Print_Usage;
         return;
   end Validate_Parameters;

   New_Line;
   Put_Line ("Number of equations N=" & Integer'Image (N));

   Parallel_Test_Harness.Setup_Tests;

   New_Line;
   Put_Line ("************* Parallel Framework Test *************");
   Put_Line ("  Physical Processors=" &
             Parallel.CPU_Count'Image (Parallel.Available_CPUs));
   Put_Line ("  DEFAULT_WORKER_COUNT=" &
             Parallel.Positive_Worker_Count'Image
               (Parallel.Default_Worker_Count));
   Put_Line ("  PARALLEL_DEBUG_ENABLED=" &
             Boolean'Image
               (Parallel_Test_Harness.Debug_Logging));

   New_Line;
   Put_Line ("(- Matrix Tests -)");
   New_Line;

   if Parallel_Only then
      goto Skip_Sequential;
   end if;

   Sequential_Block : declare
      X : Real_Vector (1 .. N);
      Y : Real_Vector (1 .. N);
      A : Real_Matrix (1 .. N, 1 .. N); -- A * X = Y
   begin
      New_Line;
      Put_Line ("Sequential Gauss Jordan");

      --  Generate Random Inputs
      for I in 1 .. N loop
         for J in 1 .. N loop
            A (I, J) := Random (Float_Generator);
         end loop;
         Y (I)  := Random (Float_Generator);
      end loop;

      if Verbose then
         Print_Inputs (A, Y);
      end if;

      Start_Time := Calendar.Seconds (Calendar.Clock);
      X          := Sequential_Gauss_Jordan (A, Y);
      Put_Line
        ("wall time=" &
         Duration'Image (Calendar.Seconds (Calendar.Clock) - Start_Time) &
         " seconds, N=" &
         Integer'Image (N) &
         ", processors=1");

      if Verbose then
         Check (A, X, Y);
      end if;

   end Sequential_Block; -- declare

   <<Skip_Sequential>>
   New_Line;
   Put_Line ("Work Sharing Gauss Jordan");

   Parallel_Block : declare
      X : Real_Vector (1 .. N);
      Y : Real_Vector (1 .. N);
      A : Real_Matrix (1 .. N, 1 .. N); -- A * X = Y
   begin
      --  Generate Random Inputs
      for I in 1 .. N loop
         for J in 1 .. N loop
            A (I, J) := Random (Float_Generator);
         end loop;
         Y (I)  := Random (Float_Generator);
      end loop;

      if Verbose then
         Print_Inputs (A, Y);
      end if;

      Start_Time := Calendar.Seconds (Calendar.Clock);

      X := Work_Sharing_Gauss_Jordan (A, Y);

      Put_Line
        ("wall time=" &
         Duration'Image (Calendar.Seconds (Calendar.Clock) - Start_Time) &
         " seconds, N=" &
         Integer'Image (N) &
         ", processors=" &
         Parallel.CPU_Id_Type'Image (Parallel.Available_CPUs) &
         ", workers=" &
         Parallel.Positive_Worker_Count'Image (Parallel.Default_Worker_Count));

      if Verbose then
         Check (A, X, Y);
      end if;

   end Parallel_Block; -- declare

   New_Line;
end Test_Matrix;
