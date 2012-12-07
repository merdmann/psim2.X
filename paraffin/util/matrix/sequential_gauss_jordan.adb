------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                       S E Q U E N T I A L
--                     G A U S S _ J O R D A N                        --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                  Copyright (C) 2012, Bradley J. Moore, Jon Squire                   --
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
--  solve.adb  solve simultaneous equations shared memory, parallel tasks
--  solve real linear equations for X where Y = A * X
--  method: Gauss-Jordan elimination using maximum pivot
--  usage:  X := Solve (A,Y);
--    Translated to java by : Jon Squire , 26 March 2003
--    First written by Jon Squire December 1959 for IBM 650, translated to
--    other languages  e.g. Fortran converted to Ada converted to C
--    then converted to java, then made parallel October 2008
--    method : guass-jordan elimination using maximum element for pivot.
--    Adapted for use testing parallel generics by Brad Moore March 2010

with Ada.Numerics.Real_Arrays;   use Ada.Numerics.Real_Arrays;
with Ada.Text_IO;                use Ada;

function Sequential_Gauss_Jordan
  (A    : Real_Matrix;
   Y    : Real_Vector)
   return Real_Vector
is

   subtype Row is Integer range 1 .. A'Length (1);
   subtype Col is Integer range 1 .. A'Length (2) + 1;

   type Max_Pivot is
      record
         Abs_Pivot : Float := 0.0;
         Pivot_Row : Row := Row'First;
      end record;

   Pivot : Max_Pivot;
   K_Row : Integer renames Pivot.Pivot_Row;

   B            : Real_Matrix (Row, Col);  --  working matrix
   Pivot_Row    : array (Row) of Row'Base
     := (others => -1); --  pivot row if not -1

   Finished_Row : array (Row) of Boolean := (others => False);

   Result     : Real_Vector (Row);

begin -- Sequential_Solve

   --  Copy input to work area

   for I in Row'Range loop
      for J in 1 .. Col'Pred (Col'Last) loop
         B (I, J) := A (I, J);
      end loop;
      B (I, Col'Last) := Y (I);
   end loop;

   --  start column processing find max pivot, wait, reduce. wait
   for K_Col in 1 .. Col'Pred (Col'Last) loop

      --  find max of tasks max pivot
      --  set smax_pivot(myid)
      --  set spivot(myid)

      Pivot.Abs_Pivot := 0.0;  -- singular caught elsewhere
      for I in Row'Range loop
         if not Finished_Row (I) and then
              abs (B (I, K_Col)) > Pivot.Abs_Pivot then
            Pivot
              := Max_Pivot'(Abs_Pivot => abs (B (I, K_Col)),
                            Pivot_Row => I);
         end if;
      end loop;

      --  have pivot, record for X at end
      Pivot_Row (K_Col) := K_Row;
      Finished_Row (K_Row) := True;

      --  check for near singular
      if Pivot.Abs_Pivot < 1.0E-12 then

         for J in Col'Succ (K_Col) .. Col'Last + 1 loop
            B (K_Row, J) := 0.0;
         end loop;

         pragma Debug
           (True,
            Text_IO.Put_Line
              ("redundant row (singular) " &
               Integer'Image (Pivot.Pivot_Row)));

         --  singular, set row to zero, including solution
      else

         --  reduce about pivot
         for J in Col'Succ (K_Col) .. Col'Last loop
            B (K_Row, J) := B (K_Row, J) / B (K_Row, K_Col);
         end loop;

         --  inner reduction loop
         for I in Row'Range loop
            if I /= K_Row then
               for J in Col'Succ (K_Col) .. Col'Last loop
                  B (I, J) := B (I, J) - B (I, K_Col) * B (K_Row, J);
               end loop;
            end if;
         end loop;

      end if;
   end loop; -- N times, then terminate

   --  build  Result  for return, unscrambling rows
   for I in Row'Range loop
      Result (I) := B (Pivot_Row (I), Col'Last);
   end loop;

   return Result;

end Sequential_Gauss_Jordan;
