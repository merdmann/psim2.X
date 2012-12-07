------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                   W O R K _ S E E K I N G _ C A S E                      --
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
with Interfaces;
with Parallel_Test_Harness;
with Parallel;                               use Parallel;
with Integer_Addition_Reducer;
with Work_Seeking_Integer_Addition_Reducer;
with Work_Stealing_Integer_Addition_Reducer;

procedure Work_Seeking_Case is
   Sum        : Interfaces.Unsigned_32        := 0;
   Start_Time : Real_Time.Time;
begin

   if not Parallel_Test_Harness.Run_Unbalanced_Iteration_Tests then
      return;
   end if;

   Put ("** Work Sharing Unbalanced Parallel Integer Sum = ");

   Start_Time := Real_Time.Clock;

   --  for I in 1 .. 100_000_000 with Integer_Addition_Reducer <Sum> loop
   --    declare
   --       X : Integer := 0;
   --    for J in 1 .. I loop
   --       X := X + J;
   --    end loop;
   --    Sum := Sum + X;
   --  end loop
   --
   declare
      procedure Iteration
        (Start, Finish : Interfaces.Unsigned_32;
         Sum : in out Interfaces.Unsigned_32) is
      begin
         for I in Start .. Finish loop
            declare
               X : Interfaces.Unsigned_32 := 0;
               use type Interfaces.Unsigned_32;
            begin
               for J in 1 .. I loop
                  X := X + J;
               end loop;
               Sum := Sum + X;
            end;
         end loop;
      end Iteration;
   begin
      Integer_Addition_Reducer
        (From         => 1,
         To           => Interfaces.Unsigned_32'Value
           (Command_Line.Argument (1)),
         Process      => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item         => Sum,
         Use_Affinity => Parallel_Test_Harness.Use_Affinity);
   end;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Seeking Unbalanced Parallel Integer Sum = ");

   Sum        := 0;
   Start_Time := Real_Time.Clock;
   --  for I in 1 .. 100_000_000 with Integer_Addition_Reducer <Sum> loop
   --    declare
   --       X : Integer := 0;
   --    for J in 1 .. I loop
   --       X := X + J;
   --    end loop;
   --    Sum := Sum + X;
   --  end loop
   declare

      procedure Iteration
        (Start, Finish  : Interfaces.Unsigned_32;
         Sum    : in out Interfaces.Unsigned_32)
      is
      begin
         for I in Start .. Finish loop
            declare
               X : Interfaces.Unsigned_32 := 0;
               use type Interfaces.Unsigned_32;
            begin
               for J in 1 .. I loop
                  X := X + J;
               end loop;
               Sum := Sum + X;
            end;
         end loop;
      end Iteration;
   begin
      Work_Seeking_Integer_Addition_Reducer
        (From          => 1,
         To            => Interfaces.Unsigned_32'Value
           (Command_Line.Argument (1)),
         Process       => Iteration'Access,
         Item          => Sum,
         Worker_Count => Parallel.Default_Worker_Count,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Uneven_Loads,
         Use_Affinity  => Parallel_Test_Harness.Use_Affinity);
   end;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Work Stealing Unbalanced Parallel Integer Sum = ");

   Sum        := 0;
   Start_Time := Real_Time.Clock;
   --  for I in 1 .. 100_000_000 with Integer_Addition_Reducer <Sum> loop
   --    declare
   --       X : Integer := 0;
   --    for J in 1 .. I loop
   --       X := X + J;
   --    end loop;
   --    Sum := Sum + X;
   --  end loop
   declare

      procedure Iteration
        (Start, Finish : Interfaces.Unsigned_32;
         Sum    : in out Interfaces.Unsigned_32)
      is
      begin
         for I in Start .. Finish loop
            declare
               X : Interfaces.Unsigned_32          := 0;
               use type Interfaces.Unsigned_32;
            begin
               for J in 1 .. I loop
                  X := X + J;
               end loop;
               Sum := Sum + X;
            end;
         end loop;
      end Iteration;
   begin
      Work_Stealing_Integer_Addition_Reducer
        (From          => 1,
         To            => Interfaces.Unsigned_32'Value
           (Command_Line.Argument (1)),
         Process       => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item          => Sum,
         Minimum_Steal => Parallel_Test_Harness.Minimum_Steal,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Uneven_Loads);
   end;

   Put_Line
     (Interfaces.Unsigned_32'Image (Sum) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

end Work_Seeking_Case;
