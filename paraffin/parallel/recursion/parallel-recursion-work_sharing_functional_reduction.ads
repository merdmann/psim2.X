------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . R E C U R S I O N .                   --
--                        W O R K _ S H A R I N G _                         --
--                F U N C T I O N A L _ R E D U C T I O N                   --
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

--  This package defines the dispatching object that can manage parallel
--  recursion using a work sharing approach and produce a result for
--  as a result for a function call.

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

package Parallel.Recursion.Work_Sharing_Functional_Reduction is

   pragma Preelaborate;

   type Recursion_Dispatcher is interface;

   function Recurse
     (Dispatcher : Recursion_Dispatcher;
      Item : Work_Type;
      Split : Positive_Worker_Count;
      Of_Splits : Positive_Worker_Count;
      Subcontractors : Worker_Count_Type) return Result_Type
    is abstract;

   type Recursion_Dispatcher_Access is access all Recursion_Dispatcher'Class;

   function Execute
     (Item : Work_Type;
      --  Top level item to process recursively

      Dispatcher : not null access Recursion_Dispatcher_Access;
      --  Client provides global access variable which
      --  is assigned by this call, and should be called
      --  by client to recurse in the parallel version
      --  of the recursive procedure

      Process : not null access
        function (Item : Work_Type) return Result_Type;
      --  Client supplied sequential version of recursion routine

      Parallel_Process : not null access
        function (Item : Work_Type;
                  Subcontractors : Worker_Count_Type) return Result_Type;
      --  Client supplied parallel version of recursion routine

      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      --  Sets the size of each workers stack

      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      --  Task priority assigned to workers.

      Worker_Count : Positive_Worker_Count := Default_Worker_Count;
      --  Number of workers to use in parallel

      Use_Affinity : Boolean := False
      --  Whether to lock workers to a specific processor, or to allow the
      --  worker to migrate to other processors.

     ) return Result_Type;

   pragma Precondition (Dispatcher.all = null);
   pragma Postcondition (Dispatcher.all = null);

private

   package Reducing_List is new Functional_Reducing_Linked_List
     (Result_Type,
      Reducer,
      Identity_Value);

end Parallel.Recursion.Work_Sharing_Functional_Reduction;
