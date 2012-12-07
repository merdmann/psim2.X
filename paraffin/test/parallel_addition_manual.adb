------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--             P A R A L L E L _ A D D I T I O N _ M A N U A L              --
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

--  This file demonstrates the minimum level of effort to code parallelism
--  for a simple loop, without using the generics. This example assumes a
--  fixed number of processors, to generalize for any number of processors
--  would require more effort. A work seeking or work stealing approach
--  would raise the level of effort even further. The point is, this is too
--  much effort to manually parallelize loops, compared to using the generics.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada;
procedure Parallel_Addition_Manual is

   type Modular_Integer is mod 2 ** 32;

   task type Worker is
      entry Initialize (Start_Index, Finish_Index : Integer);
      entry Total (Result : out Modular_Integer);
   end Worker;

   task body Worker is
      Start, Finish : Integer;
      Sum : Modular_Integer := 0;
   begin

      accept Initialize (Start_Index, Finish_Index : Integer)
      do
         Start := Start_Index;
         Finish := Finish_Index;
      end Initialize;

      for I in Start .. Finish loop
         Sum := Sum + Modular_Integer (I);
      end loop;

      accept Total (Result : out Modular_Integer) do
         Result := Sum;
      end Total;
   end Worker;

   Number_Of_Processors : constant := 2;
   Workers : array (1 .. Number_Of_Processors) of Worker;
   Results : array (1 .. Number_Of_Processors) of Modular_Integer;
   Overall_Result : Modular_Integer;
   Iterations : constant Integer := Integer'Value (Command_Line.Argument (1));
begin
   Workers (1).Initialize
     (1, Iterations / Number_Of_Processors);
   Workers (2).Initialize
     ((Iterations / Number_Of_Processors) + 1, Iterations);
   Workers (1).Total (Results (1));
   Workers (2).Total (Results (2));

   Overall_Result := Results (1) + Results (2);
   Put_Line ("** Manual Parallel Sum =" &
             Modular_Integer'Image (Overall_Result));
end Parallel_Addition_Manual;
