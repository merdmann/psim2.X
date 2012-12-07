------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                      F L O A T _ A D D I T I O N                         --
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

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line;        use Ada;
with Ada.Real_Time;
with Ada.Calendar.Formatting;

with Float_Addition_Reducer;
with Work_Seeking_Float_Addition_Reducer;
with Work_Stealing_Float_Addition_Reducer;
with Parallel_Test_Harness;                use Parallel_Test_Harness;
with Parallel; use Parallel;

procedure Float_Addition is
   Sum        : Float             := 0.0;
   Iterations : constant Positive :=
      Positive'Value (Command_Line.Argument (1));
   Start_Time : Real_Time.Time    := Real_Time.Clock;
begin
   Put ("** Sequential Elementary Float Sum = ");
   for I in 1 .. Iterations loop
      Sum := Sum + Float (I) * 0.00001;
   end loop;

   Float_Text_IO.Put (Item => Sum);
   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Sharing Elementary Float Sum = ");
   Sum        := 0.0;
   Start_Time := Real_Time.Clock;

   --  for I in 1 .. 100_000_000 with Float_Addition_Reducer <Sum> loop
   --    Sum := Sum + (I * 0.00001);
   --  end loop
   --
   Parallel_For_Loop3 : declare
      procedure Iteration (Start, Finish : Integer; Sum : in out Float) is
      begin
         for I in Start .. Finish loop
            Sum := Sum + Float (I) * 0.00001;
         end loop;
      end Iteration;
   begin
      Float_Addition_Reducer
        (From         => 1,
         To           => Iterations,
         Process      => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item         => Sum,
         Use_Affinity => Parallel_Test_Harness.Use_Affinity);
   end Parallel_For_Loop3;

   Float_Text_IO.Put (Item => Sum);
   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Seeking Parallel Elementary Float Sum = ");
   Sum        := 0.0;
   Start_Time := Real_Time.Clock;
   --  for I in 1 .. 100_000_000 with Float_Addition_Reducer <Sum> loop
   --    Sum := Sum + (I * 0.00001);
   --  end loop
   declare

      procedure Iteration
        (Start, Finish  : Integer;
         Sum    : in out Float)
      is
      begin
         for I in Start .. Finish loop
            Sum := Sum + Float (I) * 0.00001;
         end loop;
      end Iteration;
   begin
      Work_Seeking_Float_Addition_Reducer
        (From          => 1,
         To            => Iterations,
         Process       => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Minimum_Seek  => Parallel_Test_Harness.Minimum_Seek,
         Item          => Sum,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Even_Loads,
         Use_Affinity  => Parallel_Test_Harness.Use_Affinity);
   end;

   Float_Text_IO.Put (Item => Sum);
   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Stealing Parallel Elementary Float Sum = ");
   Sum        := 0.0;
   Start_Time := Real_Time.Clock;
   --  for I in 1 .. 100_000_000 with Float_Addition_Reducer <Sum> loop
   --    Sum := Sum + (I * 0.00001);
   --  end loop
   declare

      procedure Iteration
        (Start, Finish : Integer;
         Sum    : in out Float) is
      begin
         for I in Start .. Finish loop
            Sum := Sum + Float (I) * 0.00001;
         end loop;
      end Iteration;
   begin
      Work_Stealing_Float_Addition_Reducer
        (From          => 1,
         To            => Iterations,
         Process       => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item          => Sum,
         Minimum_Steal => Parallel_Test_Harness.Minimum_Steal,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Even_Loads);
   end;

   Float_Text_IO.Put (Item => Sum);
   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));
end Float_Addition;
