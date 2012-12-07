------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                         Q U I C K S O R T                             --
--                     W O R K - S E E K I N G                         --
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
with Parallel.Recursion.Work_Seeking;
use Parallel;

with Parallel_Test_Harness;

procedure Quicksort_Work_Seeking (Container : in out Array_Type)
is

   Other_Workers : aliased Parallel.Work_Seeking_State;

   type Bounds_Type is
      record
         Left, Right : Index_Type;
      end record;

   package Recursive_Quicksort is
     new Parallel.Recursion.Work_Seeking
       (Work_Type => Bounds_Type);

   Dispatcher : aliased
     Recursive_Quicksort.Recursion_Dispatcher_Access := null;

   procedure Swap (L, R : Index_Type);
   pragma Inline (Swap);

   procedure Quicksort (Bounds : Bounds_Type)
   is
      I : Index_Type'Base := Bounds.Left;
      J : Index_Type'Base := Bounds.Right;
      Pivot : constant Element_Type
        := Container
          (Index_Type'Val
               (Index_Type'Pos (Container'First) +
                ((Index_Type'Pos (Bounds.Left) +
                     Index_Type'Pos (Bounds.Right)) / 2) - 1));
      Offered : Boolean := False;
      Minimum_Work_Offer : constant := 5000;
   begin -- Quicksort

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

      if Bounds.Left < J then

         if Other_Workers.Seeking_Work
           and then (Index_Type'Pos (J) -
                       Index_Type'Pos (Bounds.Left) > Minimum_Work_Offer) then
            Dispatcher.Recurse ((Bounds.Left, J));
            Offered := True;
         else
            Quicksort ((Bounds.Left, J));
         end if;

      end if;

      if I < Bounds.Right then
         if Other_Workers.Seeking_Work and then not Offered
           and then (Index_Type'Pos (J) -
                       Index_Type'Pos (Bounds.Left)) > Minimum_Work_Offer then
            Dispatcher.Recurse ((I, Bounds.Right));
         else
            Quicksort ((I, Bounds.Right));
         end if;
      end if;
   end Quicksort;

   procedure Swap (L, R : Index_Type) is
      Temp : constant Element_Type := Container (L);
   begin
      Container (L) := Container (R);
      Container (R) := Temp;
   end Swap;

begin
   Recursive_Quicksort.Execute
     (Item => (Container'First, Container'Last),
      Other_Workers => Other_Workers'Access,
      Dispatcher => Dispatcher'Access,
      Process => Quicksort'Access,
      Storage_Size => Parallel_Test_Harness.Worker_Storage_Size,
      --  Priority => Dynamic_Priorities.Get_Priority;
      --  Worker_Count => Default_Worker_Count;
      Work_Budget => Parallel_Test_Harness.Work_Budget_For_Even_Loads,
      Use_Affinity => Parallel_Test_Harness.Use_Affinity);
end Quicksort_Work_Seeking;
