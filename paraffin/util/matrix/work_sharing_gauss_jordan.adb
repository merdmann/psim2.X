------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                     W O R K _ S H A R I N G
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

--  Gauss_Jordan.adb  solve simultaneous equations shared memory,
--  parallel tasks
--  solve real linear equations for X where Y = A * X
--  method: Gauss-Jordan elimination using maximum pivot
--  usage:  X := Gauss_Jordan (A,Y);
--    Translated to java by : Jon Squire , 26 March 2003
--    First written by Jon Squire December 1959 for IBM 650, translated to
--    other languages  e.g. Fortran converted to Ada converted to C
--    then converted to java, then made parallel October 2008
--    method : guass-jordan elimination using maximum element for pivot.
--    Adapted for use with Paraffin by Brad Moore March 2010

--   Notes: This program utilizes Annex G of the Ada ARM.
--   To compile, you will need to install libraries for BLAS and LAPACK.
--   Also, you may need to create the library gnalasup if it doesn't exist.
--   This library is just a repackaging of the BLAS and LAPACK libraries.
--      mkdir temp
--      cd temp
--      cp /usr/lib/libblas.a .
--      cp /usr/lib/liblapack.a .
--      ar -x libblas.a
--      ar -x liblapack.a
--      ar -r libgnalasup.a *.o
--
--      then copy the library to where libgnala.a resides. eg.
--      cp libgnalasup.a /usr/gnat/lib/gcc/x86_64-pc-linux-gnu/4.5.3/adalib
--

with Ada.Numerics.Real_Arrays;   use Ada.Numerics.Real_Arrays;
with Parallel.Iteration.Work_Sharing;
with Parallel.Synchronous_Barriers; use Parallel.Synchronous_Barriers;

function Work_Sharing_Gauss_Jordan
  (A    : Real_Matrix;
   Y    : Real_Vector;
   Workers : Parallel.Positive_Worker_Count := Parallel.Default_Worker_Count)
   return Real_Vector
is
   subtype Row is Integer range 1 .. A'Length (1);
   subtype Col is Integer range 1 .. A'Length (2) + 1;

   procedure Row_Iterate is new Parallel.Iteration.Work_Sharing
     (Iteration_Index_Type => Row);

   type Max_Pivot is
      record
         Abs_Pivot : Float;
         Pivot_Row : Row;
      end record;

   Identity_Pivot : constant Max_Pivot
     := (Abs_Pivot => 0.0, Pivot_Row => Row'First);

   Pivot : Max_Pivot := Identity_Pivot;
   K_Row : Integer renames Pivot.Pivot_Row;

   --  working matrix
   B            : Real_Matrix (Row, Col);

   --  pivot row if not -1
   Pivot_Row    : array (Row) of Row'Base := (others => -1);

   Worker_Pivots : array (1 .. Workers) of Max_Pivot;

   Finished_Row : array (Row) of Boolean := (others => False);

   Barr1, Barr2 : Synchronous_Barrier (Number_Waiting => Workers);

   Result     : Real_Vector (Row);

   procedure Iteration
     (Start, Finish : Row)
   is
      Released_Last : Boolean;
      use type Parallel.Positive_Worker_Count;
      Worker : constant Parallel.Positive_Worker_Count :=
        Parallel.Positive_Worker_Count
          (1 + (Start - 1) / (Row'Last / Positive (Workers)));
   begin

      --  Copy inputs to work space
      for I in Start .. Finish loop
         for J in 1 .. Col'Pred (Col'Last) loop
            B (I, J) := A (I, J);
         end loop;

         B (I, Col'Last) := Y (I);
      end loop;

      for K_Col in 1 .. Col'Pred (Col'Last) loop

         --  for the current column, find the row with the maximum value
         --  within the worker's subset range of rows

         Worker_Pivots (Worker) := (Pivot_Row => Start, Abs_Pivot => 0.0);

         for I in Start .. Finish loop
            if not Finished_Row (I) and then
            abs (B (I, K_Col)) > Worker_Pivots (Worker).Abs_Pivot then

               Worker_Pivots (Worker) :=
                 (Pivot_Row => I,
                  Abs_Pivot => abs (B (I, K_Col)));

            end if;
         end loop;

         Wait_For_Release (Barrier       => Barr1,
                           Released_Last => Released_Last);

         --  Sequential Section

         if Released_Last then

            --  Note: One might be tempted to apply the Paraffin
            --  generics to this sequential code. It significantly
            --  slows down the execution rather than provide
            --  performance benefits. The reason is that this code
            --  is called iteratively from within an outer loop.
            --  The overhead of launching worker tasks each time through
            --  the loop far outweighs the parallelism benefits

            --  Now find the maximum value for the current column across
            --  all worker results. (i.e. all rows)
            Pivot := Identity_Pivot;
            for I in Worker_Pivots'Range loop
               if Worker_Pivots (I).Abs_Pivot > Pivot.Abs_Pivot then
                  Pivot := Worker_Pivots (I);
               end if;
            end loop;

            --  Save the pivot row for each column as it is needed
            --  at the end to build the result vector.
            Pivot_Row (K_Col) := K_Row;
            Finished_Row (K_Row) := True;

            --  Ignore the row if it is not significant
            if Pivot.Abs_Pivot < 1.0E-12 then

               for J in Col'Succ (K_Col) .. Col'Last loop
                  B (K_Row, J) := 0.0;
               end loop;

            else
               --  Set column to unity by dividing row by column value
               for J in Col'Succ (K_Col) .. Col'Last loop
                  B (K_Row, J) := B (K_Row, J) / B (K_Row, K_Col);
               end loop;

            end if;
         end if;  -- Released_Last

         --  End of Sequential Section

         Wait_For_Release (Barrier => Barr2);

         --  eliminate coefficients for column in other rows by subtracting
         --  pivot row multiplied by column coefficent
         for I in Start .. Finish loop
            if I /= K_Row then
               for J in Col'Succ (K_Col) .. Col'Last loop
                  B (I, J) := B (I, J) - B (I, K_Col) * B (K_Row, J);
               end loop;
            end if;
         end loop;

      end loop;

   end Iteration;

begin -- Gauss_Jordan

   Row_Iterate
     (Process => Iteration'Access,
      Worker_Count => Workers);

   --  build  Result  for return, unscrambling rows
   for I in Row'Range loop
      Result (I) := B (Pivot_Row (I), Col'Last);
   end loop;

   return Result;

end Work_Sharing_Gauss_Jordan;
