------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--             M O N O I D _ F U N C T I O N _ A D D I T I O N              --
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
with Ada.Command_Line;        use Ada;
with Ada.Real_Time;
with Ada.Calendar.Formatting;
with Interfaces;

with Parallel_Test_Harness;                         use Parallel_Test_Harness;
with Parallel;                                      use Parallel;
with Integer_Monoids.Adder;
with Integer_Monoid_Addition_Reducer;
with Work_Seeking_Integer_Monoid_Addition_Reducer;
with Work_Stealing_Integer_Monoid_Addition_Reducer;

procedure Monoid_Function_Addition is
   Sum        : Integer_Monoids.Adder.Addition_Monoid;
   Start_Time : Real_Time.Time;
begin

   Put ("** Work Sharing Non-Limited Integer Monoid Sum = ");

   Start_Time := Real_Time.Clock;

   --  for I in 1 .. 100_000_000 loop
   --    Sum := Sum + I;
   --  end loop
   --  pragma Parallel_Loop (Result => Sum, Reducer => "+", Reducer => 0);
   Parallel_For_Loop2 : declare
      procedure Iteration
        (Start, Finish : Interfaces.Unsigned_32;
         Sum           : in out Integer_Monoids.Adder.Addition_Monoid)
      is
         use type Integer_Monoids.Adder.Addition_Monoid;
      begin
         for I in Start .. Finish loop
            Sum := Sum + I;
         end loop;
      end Iteration;
   begin
      Integer_Monoid_Addition_Reducer
        (From         => 1,
         To           => Interfaces.Unsigned_32'Value
           (Command_Line.Argument (1)),
         Process      => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item         => Sum,
         Use_Affinity => Parallel_Test_Harness.Use_Affinity);
   end Parallel_For_Loop2;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum.Value) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Seeking Parallel Non-Limited Integer Monoid Sum = ");

   Sum.Reset;
   Start_Time := Real_Time.Clock;

   --  for I in 1 .. 100_000_000
   --         with Work_Seeking_Integer_Monoid_Addition_Reducer <Sum> loop
   --    Sum := Sum + I;
   --  end loop
   Parallel_For_Loop2aa : declare

      procedure Iteration
        (Start, Finish  : Interfaces.Unsigned_32;
         Sum    : in out Integer_Monoids.Adder.Addition_Monoid)
      is
         use type Integer_Monoids.Adder.Addition_Monoid;
      begin
         for I in Start .. Finish loop
            Sum := Sum + I;
         end loop;
      end Iteration;
   begin
      Work_Seeking_Integer_Monoid_Addition_Reducer
        (From          => 1,
         To            => Interfaces.Unsigned_32'Value
           (Command_Line.Argument (1)),
         Process       => Iteration'Access,
         Item          => Sum,
         Minimum_Seek  => Parallel_Test_Harness.Minimum_Seek,
         Worker_Count => Parallel.Default_Worker_Count,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Even_Loads,
         Use_Affinity  => Parallel_Test_Harness.Use_Affinity);
   end Parallel_For_Loop2aa;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum.Value) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Stealing Parallel Non-Limited Integer Monoid Sum = ");

   Sum.Reset;
   Start_Time := Real_Time.Clock;

   --  for I in 1 .. 100_000_000
   --         with Work_Stealing_Integer_Monoid_Addition_Reducer <Sum> loop
   --    Sum := Sum + I;
   --  end loop
   Parallel_For_Loop3aa : declare

      procedure Iteration
        (Start, Finish : Interfaces.Unsigned_32;
         Sum    : in out Integer_Monoids.Adder.Addition_Monoid)
      is
         use type Integer_Monoids.Adder.Addition_Monoid;
      begin
         for I in Start .. Finish loop
            Sum := Sum + I;
         end loop;
      end Iteration;
   begin
      Work_Stealing_Integer_Monoid_Addition_Reducer
        (From          => 1,
         To            => Interfaces.Unsigned_32'Value
           (Command_Line.Argument (1)),
         Process       => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item          => Sum,
         Minimum_Steal => Parallel_Test_Harness.Minimum_Steal,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Even_Loads);
   end Parallel_For_Loop3aa;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum.Value) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

end Monoid_Function_Addition;
