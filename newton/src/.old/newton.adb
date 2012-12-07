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

with Parallel; use Parallel;
with Work_Seeking_Integer_Iterate;
with Ada.Unchecked_Deallocation;
with Parameters;

with System.Storage_Elements;

procedure Newton is

   Start_Time : Real_Time.Time;
   Elapsed : Duration;

   type Bit_Array is array (Integer range <>) of Boolean;
   for Bit_Array'Component_Size use 1;
   type Bit_Array_Access is access Bit_Array;

   procedure Free_Bits is new Unchecked_Deallocation
     (Object => Bit_Array,
      Name => Bit_Array_Access);

   Number : constant Integer := Integer'Value (Command_Line.Argument (1));
   Bits : Bit_Array_Access := new Bit_Array (1 .. Number);

   procedure Print_Usage is
   begin
      New_Line;
   end Print_Usage;

begin
   if Command_Line.Argument_Count = 0 then
      Print_Usage;
      return;
   end if;

   New_Line;
   Put_Line ("Physical Processors=" &
         Parallel.CPU_Count'Image (Parallel.Available_CPUs));
   Put_Line ("DEFAULT_WORKER_COUNT=" &
         Parallel.Positive_Worker_Count'Image (Parallel.Default_Worker_Count) );
   New_Line;

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
         Minimum_Seek => Parameters.Minimum_Seek,
         Work_Budget   => Parameters.Work_Budget_For_Uneven_Loads,
         Use_Affinity  => Parameters.Use_Affinity);
   end;

   Elapsed := Real_Time.To_Duration
     (Real_Time."-" (Left => Real_Time.Clock, Right => Start_Time));

   Put_Line
     (" Elapsed = " &
      Calendar.Formatting.Image (Elapsed_Time          => Elapsed,
                                 Include_Time_Fraction => True));
exception
   when others =>
      Print_Usage;
      raise;
end Newton;
