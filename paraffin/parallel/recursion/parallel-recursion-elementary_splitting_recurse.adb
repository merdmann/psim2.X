------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . R E C U R S I O N .                   --
--         E L E M E N T A R Y _ S P L I T T I N G _ R E C U R S E          --
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

with Ada.Task_Identification; use Ada;
with Ada.Text_IO; use Ada.Text_IO;

function Parallel.Recursion.Elementary_Splitting_Recurse
  (Item : Work_Type;
   Splitting : not null access Split_Routine;
--   Recursion : not null access Recursion_Routine;
--     Sequential_Process : not null access
--       function (Item : Work_Type) return Result_Type;
--     Parallel_Process : not null access
--       function (Item : Work_Type;
--                 Subcontractors : access Worker_Count_Type)
--            return Result_Type;
   Pre_Split_Process : not null access
     function (Item : Work_Type;
               Subcontractors : Worker_Count_Type) return Result_Type;
   Post_Split_Process : not null access
     procedure (Item : Work_Type;
                Subcontractors : access Worker_Count_Type;
                Split_Result : Result_Type);
   Storage_Size : System.Storage_Elements.Storage_Count :=
     Default_Worker_Storage_Size;
   Priority : System.Priority := Dynamic_Priorities.Get_Priority;
   Worker_Count : Positive_Worker_Count := Default_Worker_Count;
   Use_Affinity : Boolean := False) return Result_Type
is

   protected type Work_Seeker is
      pragma Priority (Priority);

      entry Wait_For_Worker_Initialization;

      entry Request_Work
        (Worker : Worker_Id;
         Item : out Work_Type;
         Split_Result : out Result_Type;
         Subs : out Worker_Count_Type;
         Done : out Boolean);

      procedure Offer_Work
        (Item : Work_Type;
         Split_Result : Result_Type;
         Subs : Worker_Count_Type);

      procedure Master_Finished;

   private
      entry Wait_For_Later_Offer
        (Worker : Worker_Id;
         Item : out Work_Type;
         Split_Result : out Result_Type;
         Subs : out Worker_Count_Type;
         Done : out Boolean);

      entry Wait_For_Work
        (Worker : Worker_Id;
         Item : out Work_Type;
         Split_Result : out Result_Type;
         Subs : out Worker_Count_Type;
         Done : out Boolean);

      Unemployed_Workers : Worker_Count_Type := 0;
      Work_Offered : Boolean := False;
      All_Work_Complete : Boolean := False;
      Master_Is_Done : Boolean := False;
      Assignment_Value : Work_Type;
      Assignment_Result : Result_Type;
      Assignment_Subs : Worker_Count_Type;
      Next_Waiting_Worker : Worker_Id;
   end Work_Seeker;

   protected body Work_Seeker is

      procedure Master_Finished is
      begin
         Master_Is_Done := True;
         if Unemployed_Workers = Worker_Count then
            All_Work_Complete := True;
         end if;
      end Master_Finished;

      entry Request_Work
        (Worker : Worker_Id;
         Item : out Work_Type;
         Split_Result : out Result_Type;
         Subs : out Worker_Count_Type;
         Done : out Boolean) when Standard.True is
      begin
         Unemployed_Workers := Unemployed_Workers + 1;

         if Unemployed_Workers = Worker_Count
           and then Master_Is_Done then
            All_Work_Complete := True;
            Done := True;
            return;
         end if;

         if Wait_For_Work'Count = 0 then
            Next_Waiting_Worker := Worker;
            requeue Wait_For_Work;
         else
            requeue Wait_For_Later_Offer;
         end if;
      end Request_Work;

      entry Wait_For_Later_Offer
        (Worker : Worker_Id;
         Item : out Work_Type;
         Split_Result : out Result_Type;
         Subs : out Worker_Count_Type;
         Done : out Boolean)
        when Wait_For_Work'Count = 0 or else All_Work_Complete is
      begin
         if All_Work_Complete then
            Done := True;
         else
            Next_Waiting_Worker := Worker;
            requeue Wait_For_Work;
         end if;
      end Wait_For_Later_Offer;

      pragma Warnings (Off, "*Worker*is not referenced");
      entry Wait_For_Work
        (Worker : Worker_Id;
         Item : out Work_Type;
         Split_Result : out Result_Type;
         Subs : out Worker_Count_Type;
         Done : out Boolean)
        when Work_Offered or else All_Work_Complete is
      begin
         if All_Work_Complete then
            Done := True;
         else
            Done := False;
            Item := Assignment_Value;
            Split_Result := Assignment_Result;
            Subs := Assignment_Subs;

            --  The worker accepting the work counts as a subcontractor, and
            --  so we need to decrement the subcontractor count.
            if Subs = 1 then
               Subs := 0;
            else

               --  A single subcontractor is going to switch to sequential
               --  recursion, then exit, so consider that worker as remaining
               --  unemployed. It will never offer work, or request work.
               --  In all other cases, the worker is now employed, so track
               --  this.
               Unemployed_Workers := Unemployed_Workers - 1;
            end if;

            Work_Offered := False;
         end if;
      end Wait_For_Work;
      pragma Warnings (On, "*Worker*is not referenced");

      entry Wait_For_Worker_Initialization
        when Wait_For_Work'Count +
          Wait_For_Later_Offer'Count = Worker_Count is
      begin
         null;
      end Wait_For_Worker_Initialization;

      procedure Offer_Work
        (Item : Work_Type;
         Split_Result : Result_Type;
         Subs : Worker_Count_Type) is
      begin
         --  Others looking for work
         if Wait_For_Work'Count > 0 and then not All_Work_Complete then
            Work_Offered := True;
            Assignment_Value := Item;
            Assignment_Result := Split_Result;
            Assignment_Subs := Subs;
         end if;
      end Offer_Work;
   end Work_Seeker;

   Scheduler : Work_Seeker;

   task type Worker
     (Work_Id : Worker_Id := Worker_Id'First)
   is
      pragma Storage_Size (Storage_Size);
      pragma Priority (Priority);
   end Worker;

   task body Worker is
      Value : Work_Type;
      Result : Result_Type;
      Done : Boolean := False;

      Subcontractors : aliased Worker_Count_Type;
   begin -- Worker

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

      pragma Debug
        (Debug_Logging, Put_Line
           (Task_Identification.Image
              (T => Task_Identification.Current_Task) &
            " Requesting Work"));

      --  Execution completed, look for work
      --  from other tasks if possible
      Scheduler.Request_Work
        (Item => Value,
         Split_Result => Result,
         Subs => Subcontractors,
         Worker => Work_Id,
         Done => Done);

      if not Done then

         --  BJM.  This unit needs some updating to get up to speed with changes to other
         --  units. SO far, this unit isnt used anywhere, as it hasn't been found to improve
         --  performance in any known scenario.
         --  Worker_Attributes.Set_Value (Val => (Work_Id, Subcontractors));
         Post_Split_Process (Value, Subcontractors'Access, Result);

      end if;

      pragma Debug
        (Debug_Logging, Put_Line
           (Task_Identification.Image
                 (T => Task_Identification.Current_Task) & " exiting"));
   end Worker;

   Next_Worker : Worker_Id := Worker_Id'First;

   function Create_Worker return Worker is
   begin
      return New_Worker : Worker
        (Work_Id => Next_Worker)
      do
         if Next_Worker < Worker_Id'Last then
            Next_Worker := Next_Worker + 1;
         end if;
      end return;
   end Create_Worker;

   type Worker_Array is array (Positive_Worker_Count range <>) of Worker;

   Workers : constant Worker_Array := (1 .. Worker_Count => Create_Worker);
   pragma Unreferenced (Workers);

--     function Recurse
--       (Item : Work_Type;
--        Subcontractors : access Worker_Count_Type;
--        Split : Positive_Worker_Count;
--        Of_Splits : Positive_Worker_Count) return Result_Type
--     is
--        --  Check to see if we can further divide the work
--        Subcontractor_Count : aliased Worker_Count_Type
--          := Subcontractors.all / Of_Splits;
--
--     begin
--
--        --  Assign any leftover subcontractors
--        if Split <= Subcontractors.all rem Of_Splits then
--           Subcontractor_Count := Subcontractor_Count + 1;
--        end if;
--
--        --  The last split is run under the current task, rather than
--        --  attempt to offer to a new worker
--        if Split = Of_Splits then
--           --  If there is only one subcontractor left, this worker takes
--           --  the work, and proceeds sequentially. Otherwise, there is
--           --  still potential for farming out work to subcontractors, so
--           --  we proceed in parallel.
--           if Subcontractor_Count <= 1 then
--              return Sequential_Process (Item);
--           else
--              return Parallel_Process (Item, Subcontractor_Count'Access);
--           end if;
--        else
--           if Subcontractor_Count = 0 then
--              return Sequential_Process (Item);
--           else
--
--              if Subcontractor_Count = 1 then
--                 Subcontractor_Count := 0;
--              end if;
--
--              return Pre_Split_Process (Item, Subcontractor_Count);
--           end if;
--        end if;
--
--     end Recurse;

   procedure Split
     (Work : Work_Type;
      Subcontractors : Worker_Count_Type;
      Partial_Result : Result_Type) is
   begin

      Scheduler.Offer_Work
        (Work,
         Partial_Result,
         Subcontractors);

   end Split;

   Result : Result_Type;

begin -- Elementar_Splitting_Recurse

   Splitting.all := Split'Unrestricted_Access;
   pragma Compile_Time_Warning
     (Portability_Warnings, "Unrestricted_Access is non-portable");

   Scheduler.Wait_For_Worker_Initialization;

   Result := Pre_Split_Process (Item, Worker_Count);
   Scheduler.Master_Finished;
   return Result;
end Parallel.Recursion.Elementary_Splitting_Recurse;
