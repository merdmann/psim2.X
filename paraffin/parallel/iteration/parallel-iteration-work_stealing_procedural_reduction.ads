------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                  P A R A L L E L . I T E R A T I O N .                   --
--                       W O R K _ S T E A L I N G _                        --
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

--  This procedure provides the capability to execute a loop in parallel
--  using a work stealing approach and produce a result for a composite type.

with System.Storage_Elements;
with Ada.Dynamic_Priorities; use Ada;

generic
   type Iteration_Index_Type is (<>);
   --  Loop Index type

   type Element_Type is private;
   --  Final Result type

   with procedure Reducer (Left, Right : in out Element_Type);
   --  Reducing operation used to compute final result. The operation
   --  needs to take two values and reduce into a single value (the Left)
   --  parameter. The Right parameter is in out to allow for finalization
   --  if necessary.

   Identity_Value : Element_Type;
   --  A special value that when applied as the right operand of the
   --  Reducing function, does not change the value of the left operand.

procedure Parallel.Iteration.Work_Stealing_Procedural_Reduction
  (From : Iteration_Index_Type := Iteration_Index_Type'First;
   To : Iteration_Index_Type := Iteration_Index_Type'Last;
   Worker_Count : Worker_Count_Type := Use_Optimal_Worker_Count;
   Minimum_Steal : Natural := Select_Minimum_Steal;
   --  Dont steal unless victim has at least this much iterations remaining

   Storage_Size : System.Storage_Elements.Storage_Count :=
     Default_Worker_Storage_Size;
   --  Sets the size of each workers stack

   Priority : Worker_Priority := Dynamic_Priorities.Get_Priority;

   Work_Budget : Work_Budget_Limit := Unlimited_Work_Budget;
   --  Number of times a worker may steal work

   Use_Affinity : Boolean := False;
   --  Whether or not affinity is applied to prevent workers from migrating
   --  to other processors.

   Process   : not null access procedure
     (Start, Finish : Iteration_Index_Type;
      Item : in out Element_Type);

   Item : in out Element_Type
   --  Final Result
  );

pragma Preelaborate
  (Parallel.Iteration.Work_Stealing_Procedural_Reduction);
