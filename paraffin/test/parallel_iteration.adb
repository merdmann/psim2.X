------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                 P A R A L L E L _ I T E R A T I O N                      --
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

with Ada.Text_IO;                            use Ada.Text_IO;
with Ada.Command_Line;                       use Ada;
with Ada.Real_Time;
with Ada.Calendar.Formatting;
with Parallel; use Parallel;
with Integer_Iterate;
with Work_Seeking_Integer_Iterate;
with Work_Stealing_Integer_Iterate;
with Parallel_Test_Harness; use Parallel_Test_Harness;
with Ada.Unchecked_Deallocation;

procedure Parallel_Iteration is
   Start_Time : Real_Time.Time;
   type Bit_Array is array (Integer range <>) of Boolean;
   for Bit_Array'Component_Size use 1;
   type Bit_Array_Access is access Bit_Array;
   procedure Free_Bits is new Unchecked_Deallocation
     (Object => Bit_Array,
      Name => Bit_Array_Access);
   Number : constant Integer := Integer'Value (Command_Line.Argument (1));
   Bits : Bit_Array_Access := new Bit_Array (1 .. Number);
begin

   Put ("** Parallel Integer Work Sharing Iterate");

   Start_Time := Real_Time.Clock;

   --  for I in 1 .. 1_000_000 with Integer_Iterate <> loop
   --     Bit_Array (I) := True;
   --  end loop;

   declare
      procedure Iteration
        (Start, Finish : Integer) is
      begin
         for I in Start .. Finish loop
            Bits (I) := True;
         end loop;
      end Iteration;
   begin
      Integer_Iterate
        (From         => 1,
         To           => Number,
         Worker_Count => Parallel.Default_Worker_Count,
         Process      => Iteration'Access,
         Use_Affinity => Parallel_Test_Harness.Use_Affinity);
   end;

   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Parallel Integer Work Seeking Iterate");

   Start_Time := Real_Time.Clock;

   declare

      procedure Iteration (Start, Finish : Integer) is
      begin
         for I in Start .. Finish loop
            Bits (I) := False;
         end loop;
      end Iteration;
   begin
      Work_Seeking_Integer_Iterate
        (From          => 1,
         To            => Number,
         Worker_Count => Parallel.Default_Worker_Count,
         Process       => Iteration'Access,
         Minimum_Seek => Parallel_Test_Harness.Minimum_Seek,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Uneven_Loads,
         Use_Affinity  => Parallel_Test_Harness.Use_Affinity);
   end;

   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Parallel Integer Work Stealing Iterate");

   Start_Time := Real_Time.Clock;

   declare
      procedure Iteration
        (Start, Finish : Integer) is
      begin
         for I in Start .. Finish loop
            Bits (I) := True;
         end loop;
      end Iteration;
   begin
      Work_Stealing_Integer_Iterate
        (From        => 1,
         To          => Number,
         Process     => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Minimum_Steal => Parallel_Test_Harness.Minimum_Steal,
         Work_Budget =>
            Parallel_Test_Harness.Work_Budget_For_Uneven_Loads);
   end;

   Put_Line
     (", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-"
                   (Left  => Real_Time.Clock,
                    Right => Start_Time)),
          Include_Time_Fraction => True));

   Free_Bits (Bits);
end Parallel_Iteration;
