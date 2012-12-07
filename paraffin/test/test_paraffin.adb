------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                       T E S T _ P A R A F F I N                          --
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

pragma Debug_Policy (Check);

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Exceptions; use Ada;
with Ada.Command_Line;

with Parallel_Test_Harness;
with Sequential_Addition;
with Parallel_Addition_Manual;
with Parallel_Addition;
with Parallel_Iteration;
with Work_Seeking_Parallel_Addition;
with Monoid_Function_Addition;
with Monoid_Procedure_Addition;
with Float_Addition;
with Container_Manipulations;
with Find_Prime;
with Work_Seeking_Case;
with Parallel_Tree;
with Test_Fibonacci;
with Parallel.Recursion;
with System.Storage_Elements;

procedure Test_Paraffin is
   procedure Display_Usage;
   pragma Priority (System.Default_Priority + 1);
   procedure Display_Usage is
   begin
      New_Line;
      Put_Line ("Usage:");
      New_Line;
      Put_Line ("test_paraffin iterations {fibonacci {containers}}");
      Put_Line ("   where:");
      Put_Line ("       iterations (integer" &
                Parallel.CPU_Count'Image (Parallel.Available_CPUs) &
                " .. 400_000_000) is the number of iterations to perform");
      Put_Line ("       fibonacci (integer 0 .. 40) is the fibonacci " &
                "number to calculate");
      Put_Line ("       containers (boolean) indicates whether parallel " &
                "container examples should run or not");
      New_Line;
      Put_Line ("Environment Variables:");
      New_Line;
      Put_Line ("  DEFAULT_WORKER_COUNT ; " &
                "Number of workers to use for each test");
      Put_Line ("  USE_AFFINITY : " &
                "Lock each worker to a specific physical processor");
      Put_Line ("  WORK_BUDGET_FOR_EVEN_LOADS : " &
                "No. of times work seeker may seek work, " &
                "for evenly balanced loads");
      Put_Line ("     -1 means unlimited ");
      Put_Line ("  WORK_BUDGET_FOR_UNEVEN_LOADS : " &
                "No. of times work seeker may seek work, " &
                "for unevenly balanced loads");
      Put_Line ("     -1 means unlimited ");
      Put_Line ("  MAX_STACK_LIMIT : " &
                "Percentage of stack before stack safe work deferrals occur");
      Put_Line ("  MAX_STACK_DEPTH : " &
                "No. of recursive calls before stack limited deferrals occur");
      Put_Line ("  WORKER_STORAGE_SIZE : " &
                "Stack size for workers");
      Put_Line ("  PARALLEL_TREE_PARALLELISM_DEPTH : " &
                "Max stack depth in parallel tree tests before " &
                "work seeking is disabled");
      Put_Line ("  FIBONACCI_PARALLELISM_LIMIT : " &
                "Nax Fibonacci number before work seeking is enabled " &
                "(work remaining)");
      Put_Line ("  FIBONACCI_PARALLELISM_DEPTH_LIMIT : " &
                "Max Fibonacci stack depth before work seeking is disabled " &
                "(work completed)");
      Put_Line ("  MIN_SEEK : " &
                "Min amount of work (iterations) remaining " &
                "to enable a work seek");
      Put_Line ("  MIN_STEAL : " &
                "Min amount of work (iterations) remaining " &
                "to enable a work steal");
      Put_Line ("  RUN_UNBALANCED_ITERATION : " &
                "Whether to run unbalanced parallel iteration tests");
      Put_Line ("  ONLY_TEST_FIBONACCI : " &
                "Skip all tests except fibonacci tests");
      Put_Line ("  RUN_DOUBLE_FIBONACCI : " &
                "Whether to run double fibonacci tests");
      Put_Line ("  PARALLEL_DEBUG_ENABLED : " &
                "If compiled with pragma debug enabled, " &
                "logs debug info to console");
      New_Line;
      Put_Line ("Suggested inputs: ");
      Put_Line ("     test_paraffin 200_000_000 37 false");
      Put_Line (" or");
      Put_Line ("     test_paraffin 20_000");
      New_Line;
      Put_Line ("NOTE: The second test will run some of the tests very fast");
      Put_Line ("         but values < 10_000_000 will enable some");
      Put_Line ("         additional tests that take longer to run.");
      New_Line;
   end Display_Usage;

begin

   if Command_Line.Argument_Count = 0 then
      Display_Usage;
      return;
   end if;

   Parallel_Test_Harness.Setup_Tests;

   New_Line;
   Put_Line ("************* Parallel Framework Test *************");
   Put_Line ("  Physical Processors=" &
             Parallel.CPU_Count'Image (Parallel.Available_CPUs));
   Put_Line ("  DEFAULT_WORKER_COUNT=" &
             Parallel.Positive_Worker_Count'Image
               (Parallel.Default_Worker_Count));
   Put_Line ("  USE_AFFINITY=" &
             Boolean'Image (Parallel_Test_Harness.Use_Affinity));
   Put_Line ("  WORK_BUDGET_FOR_EVEN_LOADS=" &
             Parallel.Work_Budget_Limit'Image
               (Parallel_Test_Harness.Work_Budget_For_Even_Loads));
   Put_Line ("  WORK_BUDGET_FOR_UNEVEN_LOADS=" &
             Parallel.Work_Budget_Limit'Image
               (Parallel_Test_Harness.Work_Budget_For_Uneven_Loads));
   Put_Line ("  ONLY_TEST_FIBONACCI=" &
             Boolean'Image
               (Parallel_Test_Harness.Only_Run_Fibonacci_Tests));
   Put_Line ("  MAX_STACK_LIMIT=" &
             Natural'Image
               (Parallel_Test_Harness.Maximum_Recursive_Stack_Limit));
   Put_Line ("  MAX_STACK_DEPTH=" &
             Parallel.Recursion.Stack_Percentage_Type'Image
               (Parallel_Test_Harness.Maximum_Recursive_Stack_Depth));
   Put_Line ("  WORKER_STORAGE_SIZE=" &
             System.Storage_Elements.Storage_Count'Image
               (Parallel_Test_Harness.Worker_Storage_Size));
   Put_Line ("  PARALLEL_TREE_PARALLELISM_DEPTH=" &
             Natural'Image
               (Parallel_Test_Harness.Parallel_Tree_Parallelism_Limit));
   Put_Line ("  FIBONACCI_PARALLELISM_LIMIT=" &
             Integer'Image
               (Parallel_Test_Harness.Fibonacci_Offer_Limit));
   Put_Line ("  FIBONACCI_PARALLELISM_DEPTH_LIMIT=" &
             Natural'Image
               (Parallel_Test_Harness.Fibonacci_Offer_Depth));
   Put_Line ("  RUN_UNBALANCED_ITERATION=" &
             Boolean'Image
               (Parallel_Test_Harness.Run_Unbalanced_Iteration_Tests));
   Put_Line ("  RUN_DOUBLE_FIBONACCI=" &
             Boolean'Image
               (Parallel_Test_Harness.Run_Double_Fibonacci_Tests));
   Put_Line ("  MIN_STEAL=" &
             Positive'Image
               (Parallel_Test_Harness.Minimum_Steal));
   Put_Line ("  MIN_SEEK=" &
             Positive'Image
               (Parallel_Test_Harness.Minimum_Seek));
   Put_Line ("  PARALLEL_DEBUG_ENABLED=" &
             Boolean'Image
               (Parallel_Test_Harness.Debug_Logging));
   Put_Line ("***************************************************");
   New_Line;
   Put_Line ("****** I T E R A T O R   T E S T S ******");
   New_Line;

   if not Parallel_Test_Harness.Only_Run_Fibonacci_Tests then
      Sequential_Addition;
      Parallel_Addition_Manual;
      Parallel_Addition;
      Work_Seeking_Parallel_Addition;

      if Command_Line.Argument_Count = 3 and then
        Boolean'Value (Command_Line.Argument (3)) then
         Monoid_Function_Addition;
      end if;

      Monoid_Procedure_Addition;
      Float_Addition;

      if Command_Line.Argument_Count = 3 and then
        Boolean'Value (Command_Line.Argument (3)) then
         Container_Manipulations;
      end if;

      Parallel_Iteration;
      Find_Prime;

      if Integer'Value (Command_Line.Argument (1)) < 10_000_000 then
         Work_Seeking_Case;
      end if;

      New_Line;
      Put_Line ("******* R E C U R S I O N   T E S T S *******");
      New_Line;

      if Integer'Value (Command_Line.Argument (1)) < 10_000_000 then
         Parallel_Tree;
      end if;
   end if;

   if Command_Line.Argument_Count = 2 then
      Test_Fibonacci;
   end if;

exception
   when E : others =>
      Put_Line
           ("Oh Oh! Main Task Died!" &
            Exceptions.Exception_Information (E));
      Display_Usage;

end Test_Paraffin;
