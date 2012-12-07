------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--               P A R A L L E L _ T E S T _ H A R N E S S                  --
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
with Ada.Exceptions; use Ada;
with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Environment_Variables;

package body Parallel_Test_Harness is

   Affinity_Usage : Boolean := False;

   --  Effectively disable work seeking, each Worker only runs once.
   --  Even for even loads, work seeking seems to be better than
   --  not work seeking
   Parallelism_Count_For_Even_Loads : Integer := -1;

   --  Unlimited Parallelism, each Worker runs as many times as needed
   Parallelism_Count_For_Uneven_Loads : Integer := -1;

   Parallel_Tree_Parallelism_Depth : Natural := 20;

   Only_Test_Fibonacci : Boolean := False;
   Fibonacci_Parallelism_Limit : Integer := 22;
   Fibonacci_Parallelism_Depth_Limit : Integer := 10;

   Run_Non_Recursion_Binary_Tree : Boolean := False;
   Run_Unbalanced_Iteration : Boolean := True;
   Run_Double_Fibonacci : Boolean := False;

   Max_Stack_Limit : Natural := Natural'Last;
   Max_Stack_Depth : Parallel.Recursion.Stack_Percentage_Type :=
     Parallel.Recursion.Default_Maximum_Stack_Depth;

   Min_Steal : Natural := 0;
   Min_Seek : Natural := 0;
   Debug_Logging_Enabled : Boolean := False;

   Storage_Size_For_Worker_Tasks : System.Storage_Elements.Storage_Count :=
     16#200_000#;

   protected Grim_Reaper is
      procedure Last_Gasp
        (C : Cause_Of_Termination;
         T : Task_Id;
         X : Exceptions.Exception_Occurrence);
   end Grim_Reaper;

   protected body Grim_Reaper is
      procedure Last_Gasp
        (C : Cause_Of_Termination;
         T : Task_Id;
         X : Exceptions.Exception_Occurrence) is
      begin
         case C is
            when Normal =>
               null;

            when Abnormal =>
               New_Line;
               Put_Line ("Something bad happened to task");
               Put (Task_Identification.Image (T));
               Put_Line (Exceptions.Exception_Information (X));

            when Unhandled_Exception =>
               New_Line;
               Put_Line ("Unhandled exception in task: " &
                         Task_Identification.Image (T) &
                         Exceptions.Exception_Information (X));
         end case;
      end Last_Gasp;
   end Grim_Reaper;

   function Debug_Logging return Boolean is
   begin
      return Debug_Logging_Enabled;
   end Debug_Logging;

   function Fibonacci_Offer_Depth return Natural is
   begin
      return Fibonacci_Parallelism_Depth_Limit;
   end Fibonacci_Offer_Depth;

   function Fibonacci_Offer_Limit return Integer is
   begin
      return Fibonacci_Parallelism_Limit;
   end Fibonacci_Offer_Limit;

   function Maximum_Recursive_Stack_Depth
     return Parallel.Recursion.Stack_Percentage_Type is
   begin
      return Max_Stack_Depth;
   end Maximum_Recursive_Stack_Depth;

   function Maximum_Recursive_Stack_Limit return Natural is
   begin
      return Max_Stack_Limit;
   end Maximum_Recursive_Stack_Limit;

   function Minimum_Seek return Natural is
   begin
      return Min_Seek;
   end Minimum_Seek;

   function Minimum_Steal return Natural is
   begin
      return Min_Steal;
   end Minimum_Steal;

   function Only_Run_Fibonacci_Tests return Boolean is
   begin
      return Only_Test_Fibonacci;
   end Only_Run_Fibonacci_Tests;

   function Parallel_Tree_Parallelism_Limit return Natural is
   begin
      return Parallel_Tree_Parallelism_Depth;
   end Parallel_Tree_Parallelism_Limit;

   function Parallelism_Enabled (Loop_Name : String) return Boolean is
   begin

      if Environment_Variables.Exists (Loop_Name) then
         return Boolean'Value (Environment_Variables.Value (Loop_Name));
      else
         return True;
      end if;
   end Parallelism_Enabled;

   function Run_Double_Fibonacci_Tests return Boolean is
   begin
      return Run_Double_Fibonacci;
   end Run_Double_Fibonacci_Tests;

   function Run_Non_Recursive_Parallel_Tree return Boolean is
   begin
      return Run_Non_Recursion_Binary_Tree;
   end Run_Non_Recursive_Parallel_Tree;

   function Run_Unbalanced_Iteration_Tests return Boolean is
   begin
      return Run_Unbalanced_Iteration;
   end Run_Unbalanced_Iteration_Tests;

   procedure Setup_Tests is
   begin

      --  Parallel.Set_Affinity (Affinity => Parallel.All_CPUs);
      Set_Dependents_Fallback_Handler
        (Handler => Grim_Reaper.Last_Gasp'Access);

   end Setup_Tests;

   function Use_Affinity return Boolean is
   begin
      return Affinity_Usage;
   end Use_Affinity;

   function Work_Budget_For_Even_Loads return Integer is
   begin
      return Parallelism_Count_For_Even_Loads;
   end Work_Budget_For_Even_Loads;

   function Work_Budget_For_Uneven_Loads return Integer is
   begin
      return Parallelism_Count_For_Uneven_Loads;
   end Work_Budget_For_Uneven_Loads;

   function Worker_Storage_Size
     return System.Storage_Elements.Storage_Count is
   begin
      return Storage_Size_For_Worker_Tasks;
   end Worker_Storage_Size;

   Affinity_Variable : constant String := "USE_AFFINITY";
   Even_Load_Work_Budget_Variable : constant String
     := "WORK_BUDGET_FOR_EVEN_LOADS";
   Uneven_Load_Work_Budget_Variable : constant String
     := "WORK_BUDGET_FOR_UNEVEN_LOADS";
   Fibonacci_Tests_Only_Variable : constant String
     := "ONLY_TEST_FIBONACCI";
   Fibonacci_Parallelism_Limit_Variable : constant String
     := "FIBONACCI_PARALLELISM_LIMIT";
   Fibonacci_Parallelism_Depth_Limit_Variable : constant String
     := "FIBONACCI_PARALLELISM_DEPTH_LIMIT";
   Run_Unbalanced_Iteration_Tests_Variable : constant String
     := "RUN_UNBALANCED_ITERATION";
   Run_Double_Fibonacci_Variable : constant String
     := "RUN_DOUBLE_FIBONACCI";
   Non_Recursion_Binary_Tree_Variable : constant String
     := "NON_RECURSIVE_BINARY_TREE";
   Parallel_Tree_Parallelism_Limit_Variable : constant String
     := "PARALLEL_TREE_PARALLELISM_DEPTH";

   --  Note: This variable is also is tested in Parallel.adb. Dont change the
   --  name here, without also changing it there as well.
   Debug_Logging_Variable : constant String := "PARALLEL_DEBUG_ENABLED";

   --  Maximum number of recursive calls before deferring work
   Max_Stack_Depth_Variable : constant String
     := "MAX_STACK_DEPTH";

   --  Percentage of stack used before deferring work
   Max_Stack_Limit_Variable : constant String
     := "MAX_STACK_LIMIT";

   --  Size of Workers stack space
   Worker_Storage_Size_Variable : constant String
     := "WORKER_STORAGE_SIZE";

   Min_Steal_Variable : constant String
     := "MIN_STEAL";

   Min_Seek_Variable : constant String
     := "MIN_SEEK";
begin
   if Environment_Variables.Exists (Affinity_Variable) then
      Affinity_Usage := Boolean'Value
        (Environment_Variables.Value (Affinity_Variable));
   end if;

   if Environment_Variables.Exists (Even_Load_Work_Budget_Variable) then
      Parallelism_Count_For_Even_Loads := Integer'Value
        (Environment_Variables.Value (Even_Load_Work_Budget_Variable));
   end if;

   if Environment_Variables.Exists (Uneven_Load_Work_Budget_Variable) then
      Parallelism_Count_For_Uneven_Loads := Integer'Value
        (Environment_Variables.Value (Uneven_Load_Work_Budget_Variable));
   end if;

   if Environment_Variables.Exists (Fibonacci_Tests_Only_Variable) then
      Only_Test_Fibonacci := Boolean'Value
        (Environment_Variables.Value (Fibonacci_Tests_Only_Variable));
   end if;

   if Environment_Variables.Exists (Debug_Logging_Variable) then
      Debug_Logging_Enabled := Boolean'Value
        (Environment_Variables.Value (Debug_Logging_Variable));
   end if;

   if Environment_Variables.Exists (Fibonacci_Parallelism_Limit_Variable) then
         Fibonacci_Parallelism_Limit := Integer'Value
        (Environment_Variables.Value (Fibonacci_Parallelism_Limit_Variable));
   end if;

   if Environment_Variables.Exists
     (Fibonacci_Parallelism_Depth_Limit_Variable) then
         Fibonacci_Parallelism_Depth_Limit := Natural'Value
        (Environment_Variables.Value
           (Fibonacci_Parallelism_Depth_Limit_Variable));
   end if;

   if Environment_Variables.Exists (Non_Recursion_Binary_Tree_Variable) then
      Run_Non_Recursion_Binary_Tree := Boolean'Value
        (Environment_Variables.Value (Non_Recursion_Binary_Tree_Variable));
   end if;

   if Environment_Variables.Exists
     (Run_Double_Fibonacci_Variable) then
      Run_Double_Fibonacci := Boolean'Value
        (Environment_Variables.Value
           (Run_Double_Fibonacci_Variable));
   end if;

   if Environment_Variables.Exists
     (Run_Unbalanced_Iteration_Tests_Variable) then
      Run_Unbalanced_Iteration := Boolean'Value
        (Environment_Variables.Value
           (Run_Unbalanced_Iteration_Tests_Variable));
   end if;

   if Environment_Variables.Exists (Max_Stack_Limit_Variable) then
      Max_Stack_Limit := Natural'Value
        (Environment_Variables.Value (Max_Stack_Limit_Variable));
   end if;

   if Environment_Variables.Exists (Max_Stack_Depth_Variable) then
      Max_Stack_Depth := Parallel.Recursion.Stack_Percentage_Type'Value
        (Environment_Variables.Value (Max_Stack_Depth_Variable));
   end if;

   if Environment_Variables.Exists
     (Parallel_Tree_Parallelism_Limit_Variable) then
      Parallel_Tree_Parallelism_Depth := Natural'Value
        (Environment_Variables.Value
           (Parallel_Tree_Parallelism_Limit_Variable));
   end if;

   if Environment_Variables.Exists (Worker_Storage_Size_Variable) then
      Storage_Size_For_Worker_Tasks :=
        System.Storage_Elements.Storage_Count'Value
          (Environment_Variables.Value (Worker_Storage_Size_Variable));
   end if;

   if Environment_Variables.Exists (Min_Seek_Variable) then
      Min_Seek :=
        Natural'Value
          (Environment_Variables.Value (Min_Seek_Variable));
   end if;

   if Environment_Variables.Exists (Min_Steal_Variable) then
      Min_Steal :=
        Natural'Value
          (Environment_Variables.Value (Min_Steal_Variable));
   end if;

end Parallel_Test_Harness;
