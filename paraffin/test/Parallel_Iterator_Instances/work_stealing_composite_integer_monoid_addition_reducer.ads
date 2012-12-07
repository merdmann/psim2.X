------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                      W O R K _ S T E A L I N G _                         --
--                   C O M P O S I T E _ I N T E G E R _                    --
--             M O N O I D _ A D D I T I O N _ R E D U C E R                --
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

with Parallel.Iteration.Work_Stealing_Procedural_Reduction;
with Composite_Integer_Monoids.Adder;
with Interfaces;

procedure Work_Stealing_Composite_Integer_Monoid_Addition_Reducer is new
Parallel.Iteration.Work_Stealing_Procedural_Reduction
  (Iteration_Index_Type => Interfaces.Unsigned_32,
   Element_Type => Composite_Integer_Monoids.Adder.Addition_Monoid,
   Reducer => Composite_Integer_Monoids.Adder.Reduce,
   Identity_Value => Composite_Integer_Monoids.Adder.Identity);
