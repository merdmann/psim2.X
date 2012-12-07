------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--            M O N O I D _ P R O C E D U R E _ A D D I T I O N             --
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

with Parallel_Test_Harness;
use Parallel_Test_Harness;
with Parallel;
use Parallel;
with Composite_Integer_Monoids.Adder;
with Composite_Integer_Monoid_Addition_Reducer;
with Work_Seeking_Composite_Integer_Monoid_Addition_Reducer;
with Work_Stealing_Composite_Integer_Monoid_Addition_Reducer;

procedure Monoid_Procedure_Addition is
   Sum        : Composite_Integer_Monoids.Adder.Addition_Monoid;
   Start_Time : Real_Time.Time;
   Loop_End   : constant Interfaces.Unsigned_32 :=
      Interfaces.Unsigned_32'Value (Command_Line.Argument (1));
begin
   Put ("** Sequential Integer Composite Monoid Sum = ");

   Start_Time := Real_Time.Clock;

   for I in 1 .. Loop_End loop
      Sum.Add (I);
   end loop;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum.Value) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Sharing Integer Composite Monoid Sum = ");

   Sum.Reset;
   Start_Time := Real_Time.Clock;

   --  for I in 1 .. 100_000_000 with Limited_Integer_Monoid_Addition_Reducer
   --<Sum> loop
   --    Sum.Add (I);
   --  end loop

   declare
      procedure Iteration
        (Start, Finish : Interfaces.Unsigned_32;
         Sum           : in out
           Composite_Integer_Monoids.Adder.Addition_Monoid)
      is
         use type Composite_Integer_Monoids.Adder.Addition_Monoid;
      begin
         for I in Start .. Finish loop
            Sum.Add (I);
         end loop;
      end Iteration;
   begin
      Composite_Integer_Monoid_Addition_Reducer
        (From         => 1,
         To           => Interfaces.Unsigned_32'Value
           (Command_Line.Argument (1)),
         Process      => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item         => Sum,
         Use_Affinity => Parallel_Test_Harness.Use_Affinity);
   end;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum.Value) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Seeking Parallel Integer Composite Monoid Sum = ");

   Sum.Reset;
   Start_Time := Real_Time.Clock;
   --  for I in 1 .. 100_000_000 with
   --      Work_Seeking_Limited_Integer_Monoid_Addition_Reducer <Sum> loop
   --    Sum.Add (I);
   --  end loop
   Parallel_For_Loop2a2 : declare

      procedure Iteration
        (Start, Finish  : Interfaces.Unsigned_32;
         Sum    : in out Composite_Integer_Monoids.Adder.Addition_Monoid)
      is
         use type Composite_Integer_Monoids.Adder.Addition_Monoid;
      begin
         for I in Start .. Finish loop
            Sum.Add (I);
         end loop;
      end Iteration;
   begin
      Work_Seeking_Composite_Integer_Monoid_Addition_Reducer
        (From          => 1,
         To            => Interfaces.Unsigned_32'Value
           (Command_Line.Argument (1)),
         Process       => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item          => Sum,
         Minimum_Seek  => Parallel_Test_Harness.Minimum_Seek,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Even_Loads,
         Use_Affinity  => Parallel_Test_Harness.Use_Affinity);
   end Parallel_For_Loop2a2;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum.Value) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Stealing Parallel Integer Composite Monoid Sum = ");

   Sum.Reset;
   Start_Time := Real_Time.Clock;
   --  for I in 1 .. 100_000_000 with
   --      Work_Stealing_Limited_Integer_Monoid_Addition_Reducer <Sum> loop
   --    Sum.Add (I);
   --  end loop
   declare

      procedure Iteration
        (Start, Finish : Interfaces.Unsigned_32;
         Sum    : in out Composite_Integer_Monoids.Adder.Addition_Monoid)
      is
         use type Composite_Integer_Monoids.Adder.Addition_Monoid;
      begin
         for I in Start .. Finish loop
            Sum.Add (I);
         end loop;
      end Iteration;
   begin
      Work_Stealing_Composite_Integer_Monoid_Addition_Reducer
        (From          => 1,
         To            => Interfaces.Unsigned_32'Value
           (Command_Line.Argument (1)),
         Process       => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item          => Sum,
         Minimum_Steal => Parallel_Test_Harness.Minimum_Steal,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Even_Loads);
   end;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum.Value) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));
end Monoid_Procedure_Addition;
