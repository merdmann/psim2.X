------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . R E C U R S I O N .                   --
--                          S T A C K _ S A F E _                           --
--                        W O R K _ S E E K I N G _                         --
--                 F U N C T I O N A L _ R E D U C T I O N            --
--                                                                          --
--                                S p e c                                   --
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

--  This procedure provides the capability to recurse in parallel
--  using a work seeking approach and produce a result for an elementary type.
--  In addition, the recursion can be limited such that when attempting to
--  recurse deeper than the specified limit, the recursion item is "saved"
--  for later processing which will then be processed with a fresh stack,
--  thus ensuring that overflow below the limit does not occur.
--  NOTE: Also, this stack safe feature may be applied to a single worker
--  so it may be beneficial to use this generic even in cases where parallel
--  execution is not needed.
--  This version of the code is considered "safer" than the stack_limited
--  version because this version lets you specify the limit in terms of
--  stack size or as a percentage of the current stack size, intead of just
--  the recursive call depth. It is difficult to know if a call depth limit
--  would be contained on the stack, but for instance specifying the limit as
--  80% of the stack size should ensure that the recursion does not overflow.

with System.Storage_Elements;
with Ada.Dynamic_Priorities; use Ada;

private with Parallel.Functional_Reducing_Linked_List;

generic
   type Work_Type is private;
   --  Data type to be processed recursively

   type Result_Type is private;
   --  Final Result type

   with function Reducer (Left, Right : Result_Type) return Result_Type;
   --  Reducing operation used to compute final result. The operation
   --  needs to take two values and reduce into a single value (the Left)
   --  parameter. The Right parameter is in out to allow for finalization
   --  if necessary.

   Identity_Value : Result_Type;
   --  A special value that when applied as the right operand of the
   --  Reducing function, does not change the value of the left operand.

package Parallel.Recursion.Stack_Safe_Work_Seeking_Functional_Reduction is

   pragma Preelaborate;

   type Recursion_Dispatcher is interface;

   function Recurse
     (Dispatcher : Recursion_Dispatcher;
      Item : Work_Type;
      Stack_Limit : System.Address) return Result_Type
   is abstract;

   type Recursion_Dispatcher_Access is access all Recursion_Dispatcher'Class;

   function Execute
    (Item : Work_Type;
     --  Top level item to process recursively

     Other_Workers : not null access Work_Seeking_State;
     --  Will be used to indicates if other workers are seeking work

     Dispatcher : not null access Recursion_Dispatcher_Access;
     --  Client provides global access variable which
     --  is assigned by this call, and should be called
     --  by client to recurse in the parallel version
     --  of the recursive procedure

     Process : not null access function
       (Item : Work_Type;
        Stack_Limit : System.Address) return Result_Type;
     --  Client supplied recursion routine that should checks
     --  Other_Workers to see if the parallel or the sequential
     --  form of the recursion should be invoked. The sequential
     --  version calls itself recursively, while the parralel
     --  version calls the 'Recursion' routine instead.

     Max_Depth : Stack_Percentage_Type := Default_Maximum_Stack_Depth;
     --  Maximum depth of the recursion expressed as a percentage of
     --  the current stack size.
     --  Attempts to recurse deeper than this limit result in
     --  "deferring" the work item to be processed later with a fresh
     --  stack

     Storage_Size : System.Storage_Elements.Storage_Count :=
       Default_Worker_Storage_Size;
     --  Sets the size of each workers stack

     Priority : System.Priority := Dynamic_Priorities.Get_Priority;
     --  Task priority assigned to workers

     Worker_Count : Positive_Worker_Count := Default_Worker_Count;
     --  Number of workers to use in parallel

     Work_Budget : Work_Budget_Limit := Unlimited_Work_Budget;
     --  The number of times a worker may seek work

     Use_Affinity : Boolean := False;
     --  Whether to lock workers to a specific processor, or to allow the
     --  worker to migrate to other processors.

     Stack_Deferrals : access Stack_Limit_Count
     --  Indicates the number of times workers had to defer their work
     --  in order to avoid stack overflow. Specifically, it indicates the
     --  number of times the Max_Depth threshold was crossed. This does not
     --  indicate a failure, only that you may want to increase the stack
     --  size next time, since hitting the stack limit does add extra
     --  processing which can significantly impact performance. We would
     --  like this to be an out parameter but we have to wait for Ada 2012
     --  for functions to have out parameters.

    ) return Result_Type;

   pragma Precondition (Dispatcher.all = null);
   pragma Postcondition (Dispatcher.all = null);

private
   package Reducing_List is new Functional_Reducing_Linked_List
     (Result_Type,
      Reducer,
      Identity_Value);
end Parallel.Recursion.Stack_Safe_Work_Seeking_Functional_Reduction;
