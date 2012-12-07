------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                   P A R A L L E L . I T E R A T I O N                    --
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

package Parallel.Iteration is
   pragma Preelaborate;

   function Optimal_Worker_Count
     (Iterations : Positive) return Worker_Count_Type;
   --  Returns the optimal number of workers needed to process
   --  a specific number of iterations for a parallel loop
   --  assuming that each iteration involves roughly the same amount
   --  of work.

private

   function Effective_Worker_Count
     (Workers : Worker_Count_Type;
      Iterations : Positive) return Positive_Worker_Count;

   function Select_Minimum_Steal_Value
     (Workers : Worker_Count_Type;
      Iterations : Positive;
      Minimum_Steal : Natural) return Positive;

   -----------------------------------------------
   --              W O R K   S T E A L I N G
   -----------------------------------------------

   --  Used for work stealing as loop iterators. It is atomic because
   --  the work stealing manager needs to be able to see the progress
   --  of the clients loop, and to modify the loop terminator if
   --  another worker wishes to steal work. In places where atomicity
   --  is not needed, the Work_Stealing_Loop_Index may be used instead.
   --  It is an unconstrained Integer subtype to faciliate integration
   --  with client code that may be using other integer types for loop
   --  indexes.
   --  Note: This type is a record rather than just an integer because
   --  by-reference semantics are needed when atomic variables as passed
   --  as parameters.
   --  RM C.6(12), C.6(18) do not specify whether Integer types have
   --  reference semantics, so the options are to wrap the Integer in a
   --  record, or use Integer, but pass using an access type to force
   --  reference semantics. The decision was made to use an enclosing
   --  record, so that parameter passing modes can be used, and makes it
   --  more clear that access values are not being squirreled away.

   subtype Work_Stealing_Loop_Index is Integer;

   type Atomic_Loop_Index_Type is
      record
         Value : Work_Stealing_Loop_Index;
      end record;
   pragma Atomic (Atomic_Loop_Index_Type);

   procedure Next (Item : in out Atomic_Loop_Index_Type);
   function "<=" (L, R : Atomic_Loop_Index_Type) return Boolean;

   pragma Inline (Next);
   pragma Inline ("<=");
   pragma Inline (Optimal_Worker_Count, Effective_Worker_Count);

end Parallel.Iteration;
