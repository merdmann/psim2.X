------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . R E C U R S I O N .                   --
--                        W O R K _ S E E K I N G _                         --
--                 P R O C E D U R A L _ R E D U C T I O N                  --
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
--  using a work seeking approach and produce a result for a composite type.

with System.Storage_Elements;
with Ada.Dynamic_Priorities; use Ada;

private with Parallel.Procedural_Reducing_Linked_List;

generic
   type Work_Type is private;
   --  Data type to be processed recursively

   type Result_Type is private;
   --  Final Result type

   with procedure Reducer (Left, Right : in out Result_Type);
   --  Reducing operation used to compute final result. The operation
   --  needs to take two values and reduce into a single value (the Left)
   --  parameter. The Right parameter is in out to allow for finalization
   --  if necessary.

   Identity_Value : Result_Type;
   --  A special value that when applied as the right operand of the
   --  Reducing function, does not change the value of the left operand.

package Parallel.Recursion.Work_Seeking_Procedural_Reduction is

   pragma Preelaborate;

   type Recursion_Dispatcher is interface;

   procedure Recurse
     (Dispatcher : Recursion_Dispatcher;
      Item : Work_Type;
      Result : out Result_Type) is abstract;

   type Recursion_Dispatcher_Access is access all Recursion_Dispatcher'Class;

   procedure Execute
     (Item : Work_Type;
      --  Top level item to process recursively

      Other_Workers : not null access Work_Seeking_State;
      --  Will be used to indicates if other workers are seeking work

      Dispatcher : not null access Recursion_Dispatcher_Access;
      --  Client provides global access variable which
      --  is assigned by this call, and should be called
      --  by client to recurse in the parallel version
      --  of the recursive procedure

      Process : not null access procedure
        (Item : Work_Type;
         Result : out Result_Type);
      --  Client supplied recursion routine that should checks
      --  Other_Workers to see if the parallel or the sequential
      --  form of the recursion should be invoked. The sequential
      --  version calls itself recursively, while the parralel
      --  version calls the 'Recursion' routine instead.

      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      --  Sets the size of each workers stack

      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      --  Ceiling priority to use for protected type that manages
      --  assigning work to workers.

      Worker_Count : Positive_Worker_Count := Default_Worker_Count;
      --  Number of workers to use in parallel

      Work_Budget : Work_Budget_Limit := Unlimited_Work_Budget;
      --  The number of times a worker may seek more work

      Use_Affinity : Boolean := False;
      --  Whether to lock workers to a specific processor, or to allow the
      --  worker to migrate to other processors.

      Result : out Result_Type
      --  Final Result
     );

   pragma Precondition (Dispatcher.all = null);
   pragma Postcondition (Dispatcher.all = null);

private

   package Reducing_List is new Procedural_Reducing_Linked_List
        (Result_Type,
         Reducer,
         Identity_Value);

end Parallel.Recursion.Work_Seeking_Procedural_Reduction;
