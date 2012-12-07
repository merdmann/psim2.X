------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . I T E R A T I O N .                   --
--                        W O R K _ S H A R I N G                           --
--                 P R O C E D U R A L _ R E D U C T I O N                  --
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

with Ada.Text_IO; use Ada.Text_IO;
with Parallel.Procedural_Reducing_Linked_List;

procedure Parallel.Iteration.Work_Sharing_Procedural_Reduction
  (From    : Iteration_Index_Type := Iteration_Index_Type'First;
   To      : Iteration_Index_Type := Iteration_Index_Type'Last;
   Worker_Count : Worker_Count_Type := Use_Optimal_Worker_Count;
   Storage_Size : System.Storage_Elements.Storage_Count :=
     Default_Worker_Storage_Size;
   Priority : System.Priority := Dynamic_Priorities.Get_Priority;
   Use_Affinity : Boolean := False;
   Process : not null access procedure
     (Start, Finish : Iteration_Index_Type;
      Item  : in out Element_Type);
   Item    : in out Element_Type)
is

   Iterations : constant Positive := Positive'Base
     (Iteration_Index_Type'Pos (To) - Iteration_Index_Type'Pos (From)) + 1;

   --  If the amount of work < number of workers, then set the worker
   --  count to match the amount of work
   Effective_Workers : constant Positive_Worker_Count :=
     Effective_Worker_Count (Worker_Count, Iterations);

   subtype Effective_Worker_Id is Worker_Id range 1 .. Effective_Workers;

   package Reducing_List is new Procedural_Reducing_Linked_List
     (Element_Type,
      Reducer,
      Identity_Value);

   Reduction_List : Reducing_List.List :=
     Reducing_List.Create (Effective_Workers, Priority);

   task type Worker
     (Work_Id : Effective_Worker_Id := Effective_Worker_Id'First;
      Start_Index,
      End_Index : Iteration_Index_Type := Iteration_Index_Type'First)
   is
      pragma Storage_Size (Storage_Size);
      pragma Priority (Priority);
   end Worker;

   task body Worker is
      Value                   : Element_Type := Identity_Value;
   begin

      if Use_Affinity then
         declare
            Worker_Affinity : Affinity_Type := (others => False);
         begin

            --  Set the Affinity for the Worker
            Worker_Affinity
              (((CPU_Id_Type'Base (Work_Id) - 1)
               rem Available_CPUs) + 1) := True;

            Set_Affinity (Worker_Affinity);

         end;
      end if;

      Process (Start_Index, End_Index, Value);

      Reducing_List.Reduce
        (Container => Reduction_List,
         Item    => Value,
         Position  => Reducing_List.To_Cursor (Worker => Work_Id));

   end Worker;

   Start_Index         : Iteration_Index_Type := From;
   End_Index           : Iteration_Index_Type;

   Iterations_Per_Task : constant Positive :=
     Iterations / Positive (Effective_Workers);

   Remainder           : Natural :=
     Iterations rem Positive (Effective_Workers);

   Reduction_Result : Element_Type;

   Next_Worker : Effective_Worker_Id := Effective_Worker_Id'First;

   function Create_Worker return Worker is
   begin

      if Remainder = 0 then
         End_Index := Iteration_Index_Type'Val
           (Iteration_Index_Type'Pos (Start_Index) +
            (Iterations_Per_Task - 1));
      else
         End_Index := Iteration_Index_Type'Val
           (Iteration_Index_Type'Pos (Start_Index) + Iterations_Per_Task);

         Remainder := Remainder - 1;
      end if;

      pragma Debug
        (Debug_Logging,
         Put_Line ("Creating Worker" &
           Effective_Worker_Id'Image (Next_Worker) &
           ", Start=" & Iteration_Index_Type'Image (Start_Index) &
          ", Finish=" & Iteration_Index_Type'Image (End_Index)));

      return New_Worker : Worker
        (Work_Id => Next_Worker,
         Start_Index => Start_Index,
         End_Index => End_Index)
      do

         if Next_Worker < Effective_Worker_Id'Last then
            Next_Worker := Next_Worker + 1;
            Start_Index := Iteration_Index_Type'Succ (End_Index);
         end if;
      end return;
   end Create_Worker;

begin

   declare
      Workers : constant array (Effective_Worker_Id'Range) of Worker :=
        (others => Create_Worker);
      pragma Unreferenced (Workers);
   begin
      null;
   end;

   --  Blocking call until reduction is complete, though it will be
   --  by the time we get here.
   Reducing_List.Result (Reduction_List, Reduction_Result);

   Reducer (Item, Reduction_Result);
   pragma Unreferenced (Reduction_Result);
end Parallel.Iteration.Work_Sharing_Procedural_Reduction;
