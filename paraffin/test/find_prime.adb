------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                         F I N D _ P R I M E                              --
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

with Ada.Text_IO;                              use Ada.Text_IO;
with Ada.Real_Time;                            use Ada;
with Ada.Calendar.Formatting;
with Integer_Iterate;
with Work_Seeking_Integer_Iterate;
with Work_Stealing_Integer_Iterate;
with Parallel; use Parallel;
with Parallel_Test_Harness;                    use Parallel_Test_Harness;

procedure Find_Prime is
   Number   : constant Integer := 78479719;
   Is_Prime : Boolean          := True;
   pragma Atomic (Is_Prime);
   Factor : Integer;
   pragma Atomic (Factor);
   Start_Time    : Real_Time.Time;
begin
   Put ("** Parallel Integer Work Sharing Find: Is Prime = ");

   Start_Time := Real_Time.Clock;

   --  Prime_Loop :
   --  for I in 1 .. Number / 4  with Integer_Iterate <> loop
   --    if Number rem I * 2 + 1 = 0 then
   --        Is_Prime := False;
   --        Factor := I * 2 + 1;
   --    end if;
   --    exit Prime_Loop when not Is_Prime;
   --  end loop Prime_Loop;

   Parallel_Find_Loop6 : declare
      procedure Iteration
        (Start, Finish : Integer) is
      begin
         Prime_Loop : for I in Start .. Finish loop
            if Number rem (I * 2 + 1) = 0 then
               Is_Prime := False;
               Factor   := I * 2 + 1;
            end if;
            exit Prime_Loop when not Is_Prime;
         end loop Prime_Loop;
      end Iteration;
   begin
      Integer_Iterate
        (From         => 1,
         To           => Number / 4,
         Process      => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Use_Affinity => Parallel_Test_Harness.Use_Affinity);
   end Parallel_Find_Loop6;

   Put_Line
     (Boolean'Image (Is_Prime) &
      ", Factor = " &
      Integer'Image (Factor) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Parallel Integer Work Seeking Find: Is Prime = ");

   Is_Prime   := True;
   Start_Time := Real_Time.Clock;
   --  Prime_Loop :
   --  for I in 1 .. Number / 4  with Work_Seeking_Integer_Iterate <> loop
   --    if Number rem I * 2 + 1 = 0 then
   --        Is_Prime := False;
   --        Factor := I * 2 + 1;
   --    end if;
   --    exit Prime_Loop when not Is_Prime;
   --  end loop Prime_Loop;

   Parallel_Find_Loop7 : declare
      procedure Iteration (Start, Finish : Integer) is
      begin
         Prime_Loop : for I in Start .. Finish loop
            if Number rem (I * 2 + 1) = 0 then
               Is_Prime := False;
               Factor   := I * 2 + 1;
            end if;
            exit Prime_Loop when not Is_Prime;
         end loop Prime_Loop;
      end Iteration;
   begin
      Work_Seeking_Integer_Iterate
        (From          => 1,
         To            => Number / 4,
         Worker_Count => Parallel.Default_Worker_Count,
         Process       => Iteration'Access,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Uneven_Loads,
         Use_Affinity  => Parallel_Test_Harness.Use_Affinity);
   end Parallel_Find_Loop7;

   Put_Line
     (Boolean'Image (Is_Prime) &
      ", Factor = " &
      Integer'Image (Factor) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

   Put ("** Parallel Integer Work Stealing Find: Is Prime = ");

   Is_Prime   := True;
   Start_Time := Real_Time.Clock;
   --  Prime_Loop :
   --  for I in 1 .. Number / 4  with Work_Seeking_Integer_Iterate <> loop
   --    if Number rem I * 2 + 1 = 0 then
   --        Is_Prime := False;
   --        Factor := I * 2 + 1;
   --    end if;
   --    exit Prime_Loop when not Is_Prime;
   --  end loop Prime_Loop;

   Parallel_Find_Loop8 : declare
      procedure Iteration
        (Start, Finish : Integer) is
      begin
         Prime_Loop :
         for I in Start .. Finish loop
            if Number rem (I * 2 + 1) = 0 then
               Is_Prime := False;
               Factor   := I * 2 + 1;
            end if;
            exit Prime_Loop when not Is_Prime;
         end loop Prime_Loop;
      end Iteration;
   begin
      Work_Stealing_Integer_Iterate
        (From          => 1,
         To            => Number / 4,
         Process       => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Minimum_Steal => Parallel_Test_Harness.Minimum_Steal,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Uneven_Loads);
   end Parallel_Find_Loop8;

   Put_Line
     (Boolean'Image (Is_Prime) &
      ", Factor = " &
      Integer'Image (Factor) &
      ", Elapsed = " &
      Calendar.Formatting.Image
         (Elapsed_Time          =>
             Real_Time.To_Duration
               (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time)),
          Include_Time_Fraction => True));

end Find_Prime;
