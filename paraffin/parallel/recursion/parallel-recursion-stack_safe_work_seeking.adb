------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . R E C U R S I O N .                   --
--                          S T A C K _ S A F E _                           --
--                        W O R K _ S E E K I N G                         --
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

with Ada.Unchecked_Deallocation;
with Ada.Assertions; use Ada.Assertions;

package body Parallel.Recursion.Stack_Safe_Work_Seeking is
   procedure Execute
     (Item            : Work_Type;
      Other_Workers   : not null access Work_Seeking_State;
      Dispatcher      : not null access Recursion_Dispatcher_Access;
      Process         : not null access procedure
        (Item        : Work_Type;
         Stack_Limit : System.Address);
      Max_Depth       : Stack_Percentage_Type                 :=
        Default_Maximum_Stack_Depth;
      Storage_Size    : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      Priority        : System.Priority                       :=
         Dynamic_Priorities.Get_Priority;
      Worker_Count    : Positive_Worker_Count                 :=
         Default_Worker_Count;
      Work_Budget     : Work_Budget_Limit                     :=
        Unlimited_Work_Budget;
      Use_Affinity    : Boolean                               := False;
      Stack_Deferrals : out Stack_Limit_Count)
   is

      type Deferred_Work_Type;
      type Deferred_Work_Access is access Deferred_Work_Type;

      type Deferred_Work_Type is record
         Item : Work_Type;
         Next : Deferred_Work_Access;
      end record;

      procedure Delete_Deferred_Work is new Ada.Unchecked_Deallocation (
         Object => Deferred_Work_Type,
         Name => Deferred_Work_Access);

      protected type Work_Seeker is
         pragma Priority (Priority);

         entry Wait_For_Worker_Initialization;

         entry Request_Work
           (Item        : out Work_Type;
            Work_Budget : in out Work_Budget_Limit;
            Done        : out Boolean);

         procedure Offer_Work
           (Item          : Work_Type;
            Work_Accepted : out Boolean);

         procedure Defer_Work (Item : Work_Type);

         procedure Master_Finished;

      private
         entry Wait_For_Later_Offer
           (Item        : out Work_Type;
            Work_Budget : in out Work_Budget_Limit;
            Done        : out Boolean);
         --  BJM FIXME Do we really need this entry?

         entry Wait_For_Work
           (Item        : out Work_Type;
            Work_Budget : in out Work_Budget_Limit;
            Done        : out Boolean);

         entry Wait_For_Deferred
           (Item        : out Work_Type;
            Work_Budget : in out Work_Budget_Limit;
            Done        : out Boolean);
         --  BJM FIXME Do we really need this entry?

         Expired_Workers    : Natural              := 0;
         Unemployed_Workers : Worker_Count_Type    := 0;
         Work_Offered       : Boolean              := False;
         All_Work_Complete  : Boolean              := False;
         Master_Is_Done     : Boolean              := False;
         Assignment_Value   : Work_Type;
         Deferred_Head      : Deferred_Work_Access := null;
         Deferred_Tail      : Deferred_Work_Access := null;
         Deferred_Length    : Natural              := 0;
      end Work_Seeker;

      protected body Work_Seeker is

         procedure Defer_Work (Item : Work_Type) is
            New_Deferred : constant Deferred_Work_Access :=
               new Deferred_Work_Type'(Item, Next => null);
         begin

            if Deferred_Tail = null then
               Deferred_Tail := New_Deferred;
               Deferred_Head := New_Deferred;
            else
               Deferred_Tail.Next := New_Deferred;
               Deferred_Tail      := New_Deferred;
            end if;
            Deferred_Length := Deferred_Length + 1;

         end Defer_Work;

         procedure Master_Finished is
         begin
            Master_Is_Done := True;
            if Unemployed_Workers = Worker_Count
              and then Deferred_Length = 0
            then
               All_Work_Complete := True;
            end if;
         end Master_Finished;

         entry Request_Work
           (Item        : out Work_Type;
            Work_Budget : in out Work_Budget_Limit;
            Done        : out Boolean)
           when Standard.True is
         begin
            Unemployed_Workers := Unemployed_Workers + 1;

            if Unemployed_Workers = Worker_Count
              and then Deferred_Length = 0
              and then Master_Is_Done
            then
               All_Work_Complete := True;
               Done              := True;
               return;
            else
               Done := False;
            end if;

            --  The initial request for work does not
            --  count against the work budget.
            --  A negative initial Work Budget
            --  means an unlimited work budget. Otherwise,
            --  a transition from 1 to 0 means the budget
            --  has been fully depleted.
            if Work_Budget >= 1 then

               Work_Budget := Work_Budget - 1;

               if Work_Budget = 0 then
                  Expired_Workers := Expired_Workers + 1;
               end if;
            end if;

            if Deferred_Length > 0 then

               Item               := Deferred_Head.Item;
               Unemployed_Workers := Unemployed_Workers - 1;

               declare
                  Temp : Deferred_Work_Access := Deferred_Head;
               begin
                  Deferred_Head := Deferred_Head.Next;
                  Delete_Deferred_Work (Temp);
                  Deferred_Length := Deferred_Length - 1;
                  if Deferred_Length = 0 then
                     Deferred_Tail := null;
                  end if;
               end;

            else
               if Work_Budget = 0 then
                  requeue Wait_For_Deferred;
               end if;

               Other_Workers.all := (Seeking_Work => True);

               if Wait_For_Work'Count = 0 then
                  requeue Wait_For_Work;
               else
                  requeue Wait_For_Later_Offer;
               end if;

            end if;

         end Request_Work;

         entry Wait_For_Later_Offer
           (Item        : out Work_Type;
            Work_Budget : in out Work_Budget_Limit;
            Done        : out Boolean)
           when Wait_For_Work'Count = 0 or else All_Work_Complete is
         begin
            if All_Work_Complete then
               Done := True;
            else
               requeue Wait_For_Work;
            end if;
         end Wait_For_Later_Offer;

         pragma Warnings (Off, "*Work_Budget* is not referenced*");
         entry Wait_For_Work
           (Item        : out Work_Type;
            Work_Budget : in out Work_Budget_Limit;
            Done        : out Boolean)
           when Work_Offered or else All_Work_Complete is
         begin
            if All_Work_Complete then
               Done := True;
            else
               Done := False;

               --  A Work Offer
               Item               := Assignment_Value;
               Unemployed_Workers := Unemployed_Workers - 1;

               --  BJM FIXME Shouldn't this also consider Wait_For_Work'Count?
               --  It depends whether the first requeue from
               --  Wait_For_Later_Offer Shuts down other queued tasks, or if
               --  they all get moved to Wait_For_Work
               Other_Workers.all :=
                 (Seeking_Work => Wait_For_Later_Offer'Count > 0);

               Work_Offered := False;

            end if;
         end Wait_For_Work;

         entry Wait_For_Deferred
           (Item        : out Work_Type;
            Work_Budget : in out Work_Budget_Limit;
            Done        : out Boolean)
           when Deferred_Length > 0 or else All_Work_Complete is
         begin
            if All_Work_Complete then
               Done := True;
            else
               Done := False;

               Item := Deferred_Head.Item;

               Unemployed_Workers := Unemployed_Workers - 1;

               declare
                  Temp : Deferred_Work_Access := Deferred_Head;
               begin
                  Deferred_Head := Deferred_Head.Next;
                  Delete_Deferred_Work (Temp);
                  Deferred_Length := Deferred_Length - 1;
                  if Deferred_Length = 0 then
                     Deferred_Tail := null;
                  end if;
               end;

            end if;
         end Wait_For_Deferred;
         pragma Warnings (On, "*Work_Budget* is not referenced*");

         entry Wait_For_Worker_Initialization
           when Wait_For_Work'Count + Wait_For_Later_Offer'Count
             = Worker_Count is
         begin
            null;
         end Wait_For_Worker_Initialization;

         procedure Offer_Work
           (Item          : Work_Type;
            Work_Accepted : out Boolean) is
         begin
            --  Others looking for work
            if Wait_For_Work'Count > 0 and then not All_Work_Complete then

               Work_Offered     := True;
               Assignment_Value := Item;
               Work_Accepted    := True;

            else
               --  Nobody looking for work or no work to offer
               Work_Accepted := False;
            end if;
         end Offer_Work;

      end Work_Seeker;

      Scheduler : Work_Seeker;

      task type Worker (Work_Id : Worker_Id := Worker_Id'First) is
         pragma Storage_Size (Storage_Size);
         pragma Priority (Priority);
      end Worker;

      task body Worker is
         Value : Work_Type;
         Done  : Boolean := False;

         Work_Count : Integer := Work_Budget;

         use type System.Storage_Elements.Storage_Offset;

         function Get_Stack_Limit_Offset
           (Limit : Stack_Percentage_Type)
            return  System.Storage_Elements.Storage_Offset is
         begin
            return Worker'Storage_Size / 100 *
                   System.Storage_Elements.Storage_Offset (Limit);
         end Get_Stack_Limit_Offset;

         --  Use the address of the first declaration as the bottom of the
         --  stack
         Stack_Limit : constant System.Address :=
            Done'Address - Get_Stack_Limit_Offset (Max_Depth);

      begin -- Worker

         --  If a work budget was specified, we dont count the initial
         --  work loading against the budget. You always get at least one
         --  crack at bat.
         if Work_Count > 0 then
            Work_Count := Work_Count + 1;
         end if;

         if Use_Affinity then
            declare
               Worker_Affinity : Affinity_Type := (others => False);
            begin

               --  Set the Affinity for the Worker
               Worker_Affinity
                 (((CPU_Id_Type'Base (Work_Id) - 1) rem
                    Available_CPUs) + 1) := True;

               Set_Affinity (Worker_Affinity);

            end;
         end if;

         Work_Loop : loop

            --  Execution completed, look for work
            --  from other tasks if possible
            Scheduler.Request_Work
              (Item        => Value,
               Work_Budget => Work_Count,
               Done        => Done);

            exit Work_Loop when Done;

            Process (Value, Stack_Limit);

         end loop Work_Loop;

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
         Scheduler.Offer_Work (Item, Work_Accepted);

         Assert (Work_Accepted);

      end Initial_Recurse;

      type Internal_Dispatcher_Type is new
        Recursion_Dispatcher with null record;

      pragma Warnings
        (Off,
         "*formal parameter *Dispatcher* is not referenced");

      overriding procedure Recurse
        (Dispatcher  : Internal_Dispatcher_Type;
         Item        : Work_Type;
         Stack_Limit : System.Address)
      is
         Work_Accepted : Boolean := False;
         use type System.Address;
      begin

         if Other_Workers.Seeking_Work then

            Scheduler.Offer_Work (Item, Work_Accepted);

            if Work_Accepted then
               return;
            end if;  --  Work_Accepted
         elsif Work_Accepted'Address <= Stack_Limit then

            Scheduler.Defer_Work (Item);

            Stack_Deferrals := Stack_Deferrals + 1;

            return;

         end if; --  Workers_Are_Idle ...

         Process (Item, Stack_Limit);
      end Recurse;

      pragma Warnings
        (On,
         "*formal parameter *Dispatcher* is not referenced");

      --  Allow client to call recursion routine
      Internal_Dispatcher : aliased Internal_Dispatcher_Type;

   begin -- Stack_Limited_Work_Seeking_Recurse

      Stack_Deferrals := 0;

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

      Dispatcher.all := null;

   end Execute;
end Parallel.Recursion.Stack_Safe_Work_Seeking;
