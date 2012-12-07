------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . R E C U R S I O N .                   --
--    W O R K _ S H A R I N G _ F U N C T I O N A L _ R E D U C T I O N     --
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

with Ada.Assertions; use Ada.Assertions;

package body Parallel.Recursion.Work_Sharing_Functional_Reduction is

   function Execute
     (Item : Work_Type;
      Dispatcher : not null access Recursion_Dispatcher_Access;
      Process : not null access
        function (Item : Work_Type) return Result_Type;
      Parallel_Process : not null access
        function (Item : Work_Type;
                  Subcontractors : Worker_Count_Type) return Result_Type;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Worker_Count : Positive_Worker_Count := Default_Worker_Count;
      Use_Affinity : Boolean := False) return Result_Type
   is

      Reduction_List : Reducing_List.List :=
        Reducing_List.Create (Worker_Count,
                              Priority);

      protected type Work_Seeker is
         pragma Priority (Priority);

         entry Wait_For_Worker_Initialization;

         entry Request_Work
           (Item : out Work_Type;
            Subs : out Worker_Count_Type;
            Done : out Boolean);

         procedure Offer_Work
           (Item : Work_Type;
            Subs : Worker_Count_Type;
            Work_Accepted : out Boolean);

         procedure Master_Finished;

      private
         entry Wait_For_Later_Offer
           (Item : out Work_Type;
            Subs : out Worker_Count_Type;
            Done : out Boolean);

         entry Wait_For_Work
           (Item : out Work_Type;
            Subs : out Worker_Count_Type;
            Done : out Boolean);

         Unemployed_Workers : Worker_Count_Type := 0;
         Work_Offered : Boolean := False;
         All_Work_Complete : Boolean := False;
         Master_Is_Done : Boolean := False;
         Assignment_Value : Work_Type;
         Assignment_Subs : Worker_Count_Type;
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
           (Item : out Work_Type;
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
               requeue Wait_For_Work;
            else
               requeue Wait_For_Later_Offer;
            end if;
         end Request_Work;

         entry Wait_For_Later_Offer
           (Item : out Work_Type;
            Subs : out Worker_Count_Type;
            Done : out Boolean)
           when Wait_For_Work'Count = 0 or else All_Work_Complete is
         begin
            if All_Work_Complete then
               Done := True;
            else
               requeue Wait_For_Work;
            end if;
         end Wait_For_Later_Offer;

         entry Wait_For_Work
           (Item : out Work_Type;
            Subs : out Worker_Count_Type;
            Done : out Boolean)
           when Work_Offered or else All_Work_Complete is
         begin
            if All_Work_Complete then
               Done := True;
            else
               Done := False;
               Item := Assignment_Value;
               Subs := Assignment_Subs;

               --  The worker accepting the work counts as a subcontractor, and
               --  so we need to decrement the subcontractor count.
               if Subs = 1 then
                  Subs := 0;
               else

                  --  A single subcontractor is going to switch to sequential
                  --  recursion, then exit, so consider that worker as
                  --  remaining unemployed. It will never offer work, or
                  --  request work.
                  --  In all other cases, the worker is now employed, so track
                  --  this.
                  Unemployed_Workers := Unemployed_Workers - 1;
               end if;

               Work_Offered := False;
            end if;
         end Wait_For_Work;

         entry Wait_For_Worker_Initialization
           when Wait_For_Work'Count +
             Wait_For_Later_Offer'Count = Worker_Count is
         begin
            null;
         end Wait_For_Worker_Initialization;

         procedure Offer_Work
           (Item : Work_Type;
            Subs : Worker_Count_Type;
            Work_Accepted : out Boolean) is
         begin
            --  Others looking for work
            if Wait_For_Work'Count > 0 and then not All_Work_Complete then

               Work_Offered := True;
               Assignment_Value := Item;
               Assignment_Subs := Subs;
               Work_Accepted := True;
            else
               --  Nobody looking for work or no work to offer
               Work_Accepted := False;
            end if;
         end Offer_Work;

      end Work_Seeker;

      Scheduler : Work_Seeker;

      type Result_Array is
        array (Positive_Worker_Count range <>) of Result_Type;

      Results : Result_Array (1 .. Worker_Count) :=
        (others => Identity_Value);

      task type Worker (Work_Id : Worker_Id := Worker_Id'First) is
         pragma Storage_Size (Storage_Size);
         pragma Priority (Priority);
      end Worker;

      function Create_Worker return Worker;
      procedure Initial_Recurse (Item : Work_Type);

      task body Worker is
         Value : Work_Type;
         Done : Boolean := False;
         Subcontractors : Worker_Count_Type;

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

         Work_Loop : loop

            --  Execution completed, look for work
            --  from other tasks if possible
            Scheduler.Request_Work
              (Item => Value,
               Subs => Subcontractors,
               Done => Done);

            exit Work_Loop when Done;

            if Subcontractors = 0 then
               Results (Work_Id) := Process (Value);
               exit Work_Loop;
            end if;

            Results (Work_Id) := Parallel_Process (Value, Subcontractors);

         end loop Work_Loop;

         Reducing_List.Reduce (Container => Reduction_List,
                               Item      => Results (Work_Id),
                               Position  => Reducing_List.To_Cursor (Work_Id));

      end Worker;

      Next_Worker : Worker_Id := Worker_Id'First;

      function Create_Worker return Worker is
      begin
         return New_Worker : Worker (Work_Id => Next_Worker) do
            if Next_Worker < Worker_Id'Last then
               Next_Worker := Next_Worker + 1;
            end if;
         end return;
      end Create_Worker;

      procedure Initial_Recurse (Item : Work_Type)
      is
         Work_Accepted : Boolean := False;
      begin

         --  Initial Donor is not a worker
         Scheduler.Offer_Work
           (Item,
            Worker_Count,
            Work_Accepted);

         Assert (Work_Accepted);

      end Initial_Recurse;

      pragma Warnings
        (Off,
         "*formal parameter *Dispatcher* is not referenced");

      type Internal_Dispatcher_Type is
        new Recursion_Dispatcher with null record;

      overriding function Recurse
        (Dispatcher     : Internal_Dispatcher_Type;
         Item : Work_Type;
         Split : Positive_Worker_Count;
         Of_Splits : Positive_Worker_Count;
         Subcontractors : Worker_Count_Type) return Result_Type
      is

         Work_Accepted : Boolean := False;
         --  Check to see if we can further divide the work
         Subcontractor_Count : Worker_Count_Type
           := Subcontractors / Of_Splits;

      begin

         --  Assign any leftover subcontractors
         if Split <= Subcontractors rem Of_Splits then
            Subcontractor_Count := Subcontractor_Count + 1;
         end if;

         if Subcontractors > 0 then

            --  The last split is run under the current task, rather than
            --  attempt to offer to a new worker
            if Split = Of_Splits then
               --  If there is only one subcontractor left, this worker takes
               --  the work, and proceeds sequentially. Otherwise, there is
               --  still potential for farming out work to subcontractors,
               --  so we proceed in parallel.
               if Subcontractor_Count <= 1 then
                  return Process (Item);
               else
                  return Parallel_Process (Item, Subcontractor_Count);
               end if;
            else

               Scheduler.Offer_Work
                 (Item,
                  Subcontractor_Count,
                  Work_Accepted);

               if Work_Accepted then
                  return Identity_Value;
               end if; --  Work_Accepted
            end if; --  Split = Of_Splits
         end if; --  Workers_Are_Idle ...

         --  If we get here, it's likely because the only worker available is
         --  the one looking for subcontractors, in this case, we consider
         --  that the work is assigned to the calling worker, and we no longer
         --  need to worry about assigning work to subcontractors
         return Process (Item);
      end Recurse;

      pragma Warnings
        (On,
         "*formal parameter *Dispatcher* is not referenced");

      --  Allow client to call recursion routine
      Internal_Dispatcher : aliased Internal_Dispatcher_Type;
      Final_Reduction_Value : Result_Type := Identity_Value;
   begin

      --  Allow client to call recursion routine
      Dispatcher.all := Internal_Dispatcher'Unchecked_Access;

      declare
         type Worker_Array is
           array (Positive_Worker_Count range <>) of Worker;

         Workers : constant Worker_Array :=
           (1 .. Worker_Count => Create_Worker);
         pragma Unreferenced (Workers);
      begin

         Scheduler.Wait_For_Worker_Initialization;

         Initial_Recurse (Item);
         Scheduler.Master_Finished;
      end;

      Reducing_List.Result (Container        => Reduction_List,
                            Reduction_Result => Final_Reduction_Value);

      Dispatcher.all := null;

      return Final_Reduction_Value;
   end Execute;
end Parallel.Recursion.Work_Sharing_Functional_Reduction;
