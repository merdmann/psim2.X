------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . R E C U R S I O N .                   --
--         E L E M E N T A R Y _ S P L I T T I N G _ R E C U R S E          --
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
--  using a work sharing approach and produce a result for an elementary type.

with System.Storage_Elements;
with Ada.Dynamic_Priorities; use Ada;

generic
   type Work_Type is private;
   --  Data type to be processed recursively

   type Result_Type is private;
   --  Final Result type

--     type Recursion_Routine is access
--       function
--         (Work : Work_Type;
--          Subcontractors : access Worker_Count_Type;
--          Split : Positive_Worker_Count;
--          Of_Splits : Positive_Worker_Count) return Result_Type;
   --  Access type to Recursion Routine variable supplied by generic for
   --  client from the parallel version of the recursive code

   type Split_Routine is access
     procedure
       (Work : Work_Type;
        Subcontractors : Worker_Count_Type;
        Partial_Result : Result_Type);

function Parallel.Recursion.Elementary_Splitting_Recurse
  (Item : Work_Type;
   --  Top level item to process recursively

   Splitting : not null access Split_Routine;
   --  Client provides global access variable which
   --  is assigned by this call, and should be called
   --  by client in the pre splitting process

--   Recursion : not null access Recursion_Routine;
   --  Client provides global access variable which
   --  is assigned by this call, and should be called
   --  by client to recurse in the parallel version
   --  of the recursive procedure

--     Sequential_Process : not null access
--       function (Item : Work_Type) return Result_Type;
--     --  Client supplied sequential version of recursion routine
--
--     Parallel_Process : not null access
--       function (Item : Work_Type;
--                 Subcontractors : access Worker_Count_Type)
--           return Result_Type;
--     --  Client supplied sequential version of recursion routine
--
   Pre_Split_Process : not null access
     function (Item : Work_Type;
               Subcontractors : Worker_Count_Type) return Result_Type;
   --  Client supplied parallel version of recursion routine

   Post_Split_Process : not null access
     procedure (Item : Work_Type;
                Subcontractors : access Worker_Count_Type;
                Split_Result : Result_Type);
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

pragma Preelaborate (Parallel.Recursion.Elementary_Splitting_Recurse);
