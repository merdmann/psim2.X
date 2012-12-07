------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--              Q U I C K S O R T _ S E Q U E N T I A L                 --
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
procedure Quicksort_Sequential (Container : in out Array_Type)
is
   procedure Swap (L, R : Index_Type);
   pragma Inline (Swap);

   procedure Quicksort (Left, Right : Index_Type)
   is
      I : Index_Type'Base := Left;
      J : Index_Type'Base := Right;
      Pivot : constant Element_Type
        := Container
          (Index_Type'Val
               (Index_Type'Pos (Container'First) +
                ((Index_Type'Pos (Left) + Index_Type'Pos (Right)) / 2) - 1));

   begin

      while I <= J loop

         while Container (I) < Pivot loop
            I := Index_Type'Succ (I);
         end loop;

         while Pivot < Container (J) loop
            J := Index_Type'Pred (J);
         end loop;

         if I <= J then

            Swap (I, J);

            I := Index_Type'Succ (I);
            J := Index_Type'Pred (J);
         end if;

      end loop;

      if Left < J then
         Quicksort (Left, J);
      end if;

      if I < Right then
         Quicksort (I, Right);
      end if;
   end Quicksort;

   procedure Swap (L, R : Index_Type) is
      Temp : constant Element_Type := Container (L);
   begin
      Container (L) := Container (R);
      Container (R) := Temp;
   end Swap;

begin
   Quicksort (Container'First, Container'Last);
end Quicksort_Sequential;
