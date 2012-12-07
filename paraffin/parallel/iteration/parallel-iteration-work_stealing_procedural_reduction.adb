------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . I T E R A T I O N .                   --
--                       W O R K _ S T E A L I N G _                        --
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

with Parallel.Procedural_Reducing_Linked_List;
with Ada.Numerics.Discrete_Random; use Ada.Numerics;

procedure Parallel.Iteration.Work_Stealing_Procedural_Reduction
  (From : Iteration_Index_Type := Iteration_Index_Type'First;
   To : Iteration_Index_Type := Iteration_Index_Type'Last;
   Worker_Count : Worker_Count_Type := Use_Optimal_Worker_Count;
   Minimum_Steal : Natural := Select_Minimum_Steal;
   Storage_Size : System.Storage_Elements.Storage_Count :=
     Default_Worker_Storage_Size;
   Priority : Worker_Priority := Dynamic_Priorities.Get_Priority;
   Work_Budget : Work_Budget_Limit := Unlimited_Work_Budget;
   Use_Affinity : Boolean := False;
   Process   : not null access procedure
     (Start, Finish : Iteration_Index_Type;
      Item : in out Element_Type);
   Item : in out Element_Type)
is

   Iterations : constant Positive := Positive'Base
     (Iteration_Index_Type'Pos (To) - Iteration_Index_Type'Pos (From)) + 1;

   Local_Minimum_Steal : constant Natural := Select_Minimum_Steal_Value
     (Workers     =>  Worker_Count,
      Iterations    => Iterations,
      Minimum_Steal => Minimum_Steal);

   Work_Chunks : constant Positive := Iterations / Local_Minimum_Steal +
     Boolean'Pos (Iterations mod Local_Minimum_Steal > 0);

   --  If the amount of work < number of workers, then set the worker
   --  count to match the amount of work
   Effective_Workers : constant Positive_Worker_Count :=
     Effective_Worker_Count
       (Worker_Count, Work_Chunks);

   subtype Effective_Worker_Id is Worker_Id range 1 .. Effective_Workers;

   type Worker_Progress_Type is
      record
         Iterator : aliased Atomic_Loop_Index_Type;
         Last_Index : aliased Atomic_Loop_Index_Type;
      end record;

   package Random_Worker is new Discrete_Random
     (Result_Subtype => Effective_Worker_Id);

   Random_Worker_Generator : Random_Worker.Generator;

   type Worker_Progress_Array is
     array (Effective_Worker_Id) of Worker_Progress_Type;
   Worker_Progress : Worker_Progress_Array;
   pragma Volatile (Worker_Progress);

   package Reducing_List is new Procedural_Reducing_Linked_List
     (Element_Type,
      Reducer,
      Identity_Value);

   Reduction_List : Reducing_List.List
     := Reducing_List.Create (Worker_Count => Effective_Workers,
                              Priority     => Priority);

   type Idle_State_Array is array (Effective_Worker_Id) of Boolean;

   protected Work_Stealing_Manager is

      pragma Priority (Priority);

      entry Wait_For_Workers_To_Start;

      entry Request_Work
        (Start, Finish : out Work_Stealing_Loop_Index;
         Stealer : Effective_Worker_Id;
         Work_Budget : in out Work_Budget_Limit;
         Done : out Boolean);

      procedure Offer_Work
        (Start, Finish : Work_Stealing_Loop_Index;
         Donor : Effective_Worker_Id;
         Idle_Worker : out Effective_Worker_Id);

   private
      entry Wait_For_Later_Offer
        (Start, Finish : out Work_Stealing_Loop_Index;
         Stealer : Effective_Worker_Id;
         Work_Budget : in out Work_Budget_Limit;
         Done : out Boolean);

      entry Wait_For_Work
        (Start, Finish : out Work_Stealing_Loop_Index;
         Stealer : Effective_Worker_Id;
         Work_Budget : in out Work_Budget_Limit;
         Done : out Boolean);

      Expired_Workers : Natural := 0;
      Work_Offered : Boolean := False;
      All_Work_Complete : Boolean := False;
      Assignment_Start, Assignment_Finish : Work_Stealing_Loop_Index;
      Initial_Victim : Worker_Count_Type;
      Current_Victim : Effective_Worker_Id'Base := 0;
      Next_Waiting_Worker : Effective_Worker_Id;
      Idlers : Idle_State_Array := (others => False);
      Release_The_Hounds : Boolean := False;
   end Work_Stealing_Manager;

   protected body Work_Stealing_Manager is

      entry Wait_For_Workers_To_Start
        when Wait_For_Workers_To_Start'Count =
          Natural (Effective_Worker_Id'Last)
      or else Release_The_Hounds is
      begin
         Release_The_Hounds := True;
      end Wait_For_Workers_To_Start;

      entry Request_Work
        (Start, Finish : out Work_Stealing_Loop_Index;
         Stealer : Effective_Worker_Id;
         Work_Budget : in out Work_Budget_Limit;
         Done : out Boolean) when Standard.True
      is
         Budget_Expired : Boolean := False;
      begin
         Idlers (Stealer) := True;

         if Work_Budget >= 0 then
            Work_Budget := Work_Budget - 1;
            if Work_Budget < 0 then
               Budget_Expired := True;
            end if;
         end if;

         if All_Work_Complete then
            Done := True;
         else
            if Budget_Expired then
               Expired_Workers := Expired_Workers + 1;

               if Expired_Workers = Natural (Effective_Worker_Id'Last) then
                  All_Work_Complete := True;
               end if;

               Done := True;
            else
               if Wait_For_Work'Count = 0 then
                  Next_Waiting_Worker := Stealer;
                  Initial_Victim := 0;
                  requeue Wait_For_Work;
               else
                  requeue Wait_For_Later_Offer;
               end if;
            end if;
         end if;
      end Request_Work;

      entry Wait_For_Later_Offer
        (Start, Finish : out Work_Stealing_Loop_Index;
         Stealer : Effective_Worker_Id;
         Work_Budget : in out Work_Budget_Limit;
         Done : out Boolean)
        when Wait_For_Work'Count = 0 or else All_Work_Complete is
         --  Workers queue here if there is already a worker queued
         --  on the Wait_For_Work entry
      begin
         if All_Work_Complete then
            Done := True;
         else
            Next_Waiting_Worker := Stealer;
            Initial_Victim := 0;
            requeue Wait_For_Work;
         end if;
      end Wait_For_Later_Offer;

      entry Wait_For_Work
        (Start, Finish : out Work_Stealing_Loop_Index;
         Stealer : Effective_Worker_Id;
         Work_Budget : in out Work_Budget_Limit;
        Done : out Boolean)
        when Initial_Victim = 0 or else
             Work_Offered or else
             Idlers (Current_Victim) or else
             All_Work_Complete is
      begin
         if All_Work_Complete then
            Done := True;
         else
            if Initial_Victim = 0 then
               Initial_Victim := Random_Worker.Random
                     (Gen => Random_Worker_Generator);
               Current_Victim := Initial_Victim;

               --  If you're the last worker, you're not going
               --  to be able to steal from anyone. You're done.
               if Expired_Workers = Natural (Effective_Worker_Id'Last) - 1 then
                  Done := True;
               end if;

            elsif Work_Offered then
               Start := Assignment_Start;
               Finish := Assignment_Finish;
               Work_Offered := False;
               Idlers (Stealer) := False;
               Current_Victim := 0;
               Done := False;

               return;
            elsif Idlers (Current_Victim) then
               Current_Victim :=
                 (Current_Victim rem Effective_Worker_Id'Last) + 1;
               if Current_Victim = Initial_Victim then
                  Done := True;
               end if;
            end if;

            Done := False;

            while not Done loop
               if Current_Victim /= Stealer then

                  if Worker_Progress (Current_Victim).Iterator.Value <
                    Worker_Progress (Current_Victim).Last_Index.Value
                    - Local_Minimum_Steal then

                     --  Attempt to cause the worker to exit early
                     --  by tricking it into thinking it has completed
                     --  its iterations.
                     Worker_Progress (Current_Victim).Last_Index
                        := Worker_Progress (Current_Victim).Iterator;

                     --  Wait for the Victim to either
                     --  offer work, or request more work
                     --  if it escaped.
                     requeue Wait_For_Work;
                  end if;
               end if;

               --  Still looking? Advance to next worker
               Current_Victim :=
                 (Current_Victim rem Effective_Worker_Id'Last) + 1;

               if Current_Victim = Initial_Victim then
                  Done := True;
               end if;
            end loop;

            Expired_Workers := Expired_Workers + 1;
            if Expired_Workers = Natural (Effective_Worker_Id'Last) then
               All_Work_Complete := True;
            end if;

         end if;
      end Wait_For_Work;

      procedure Offer_Work
        (Start, Finish : Work_Stealing_Loop_Index;
         Donor : Effective_Worker_Id;
         Idle_Worker : out Effective_Worker_Id) is
      begin

         Idle_Worker := Next_Waiting_Worker;
         Assignment_Start := Start;
         Assignment_Finish := Finish;
         Work_Offered := True;

            --  Insert the new node into the reduction list
            Reducing_List.Insert_Right
              (Container => Reduction_List,
               Item      => Reducing_List.To_Cursor
                 (Worker => Idle_Worker),
               Position  => Reducing_List.To_Cursor
                 (Worker => Donor));

      end Offer_Work;
   end Work_Stealing_Manager;

   task type Worker
     (Work_Id : Effective_Worker_Id := Effective_Worker_Id'First)
   is
      pragma Storage_Size (Storage_Size);
      pragma Priority (Priority);
   end Worker;

   task body Worker is
      Previous_Last_Index : Work_Stealing_Loop_Index
        := Worker_Progress (Work_Id).Last_Index.Value;
      Temp : Work_Stealing_Loop_Index;
      Value                   : Element_Type := Identity_Value;
      Done : Boolean := False;
      Stealer : Effective_Worker_Id;
      Work_Count : Integer := Work_Budget;

      procedure Iteration
        (Iter   : in out Atomic_Loop_Index_Type;
         Finish : Atomic_Loop_Index_Type;
         Item    : in out Element_Type)
      is
         Chunk_Start, Chunk_Finish : Iteration_Index_Type;
      begin
         while Iter <= Finish loop

            Chunk_Start := Iteration_Index_Type'Val
              (Iteration_Index_Type'Pos (From) +
               (Iter.Value - 1) * Local_Minimum_Steal);

            if Iter.Value = Work_Chunks then
               Chunk_Finish := To;
            else
               Chunk_Finish := Iteration_Index_Type'Val
                 (Iteration_Index_Type'Pos (Chunk_Start)
                  + Local_Minimum_Steal - 1);
            end if;

            Process (Chunk_Start, Chunk_Finish, Item);

            Parallel.Iteration.Next (Iter);
         end loop;
      end Iteration;

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

      Work_Stealing_Manager.Wait_For_Workers_To_Start;

      Work_Loop : while not Done loop

         Iteration
           (Worker_Progress (Work_Id).Iterator,
            Worker_Progress (Work_Id).Last_Index,
            Value);

         if Worker_Progress (Work_Id).Last_Index.Value
           /= Previous_Last_Index then

            Worker_Progress (Work_Id).Last_Index.Value :=
              (Previous_Last_Index -
                 Worker_Progress (Work_Id).Iterator.Value) / 2 +
               Worker_Progress (Work_Id).Iterator.Value;

            --  Need to set Previous_Last_Index before we make an Offer
            --  because someone might steal work before we get to store
            --  the Previous_Last_Index otherwise.
            Temp := Previous_Last_Index;
            Previous_Last_Index := Worker_Progress (Work_Id).Last_Index.Value;

            --  Execution cut short,
            --  another task must be trying to Seek work
            Work_Stealing_Manager.Offer_Work
              (Start => Work_Stealing_Loop_Index'Succ
                (Worker_Progress (Work_Id).Last_Index.Value),
               Finish => Temp,
               Donor => Work_Id,
               Idle_Worker => Stealer);

         else

            pragma Assert
              (Worker_Progress (Work_Id).Iterator.Value >=
               Worker_Progress (Work_Id).Last_Index.Value);

            --  Now do the real reduction
            Reducing_List.Reduce
              (Container => Reduction_List,
               Item    => Value,
               Position  => Reducing_List.To_Cursor
                 (Worker => Work_Id));

            --  Execution completed, look for work
            --  from other tasks if possible
            Work_Stealing_Manager.Request_Work
              (Start     => Worker_Progress (Work_Id).Iterator.Value,
               Finish    => Previous_Last_Index,
               Stealer => Work_Id,
               Work_Budget => Work_Count,
               Done => Done);

            Worker_Progress (Work_Id).Last_Index.Value := Previous_Last_Index;

            if not Done then
               --  Resetting Value, since we are starting a new work task
               Value := Identity_Value;
            end if;
         end if;
      end loop Work_Loop;

   end Worker;

   Start_Index         : Work_Stealing_Loop_Index := 1;
   End_Index           : Work_Stealing_Loop_Index;

   Iterations_Per_Task : constant Positive :=
     Work_Chunks / Positive (Effective_Workers);

   Remainder           : Natural :=
     Work_Chunks rem Positive (Effective_Workers);

   Reduction_Result : Element_Type;

   Next_Worker : Effective_Worker_Id := Effective_Worker_Id'First;

   function Create_Worker return Worker is
   begin
      if Remainder = 0 then
         End_Index := Start_Index + Iterations_Per_Task - 1;
      else
         End_Index := Start_Index + Iterations_Per_Task;

         Remainder := Remainder - 1;
      end if;

      Worker_Progress (Next_Worker) :=
        (Iterator => (Value => Start_Index),
         Last_Index => (Value => End_Index));

      return New_Worker : Worker
        (Work_Id => Next_Worker)
      do
         if Next_Worker < Effective_Worker_Id'Last then
            Next_Worker := Next_Worker + 1;
            Start_Index := Integer'Succ (End_Index);
         end if;
      end return;
   end Create_Worker;

begin

   declare
      Workers : constant array (Effective_Worker_Id'Range) of Worker
        := (others => Create_Worker);
      pragma Unreferenced (Workers);
   begin
      null;
   end;

   --  Blocking call until reduction is complete
   Reducing_List.Result (Reduction_List, Reduction_Result);

   Reducer (Item, Reduction_Result);
   pragma Unreferenced (Reduction_Result);
end Parallel.Iteration.Work_Stealing_Procedural_Reduction;
