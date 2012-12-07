------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L .                             --
--                  S Y N C H R O N O U S _ B A R R I E R S                 --
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

--  Barriers may be used to interleave sequential processing within
--  parallel code. Ada 2012 will be introducing a Synchronous_Barriers
--  package, but this provides a portable implementation in the meantime.
--  A barrier blocks until Number_Waiting tasks have called Wait_For_Release,
--  then all tasks get released. An optional Released_Last output flag may
--  be used to introduce some sequential code that is executed only by the
--  last worker task released by the barrier.

package Parallel.Synchronous_Barriers is
   pragma Preelaborate;
   pragma Remote_Types;

   type Synchronous_Barrier
     (Number_Waiting : Positive_Worker_Count) is limited private;

   procedure Wait_For_Release
     (Barrier : in out Synchronous_Barrier);

   procedure Wait_For_Release
     (Barrier : in out Synchronous_Barrier;
      Released_Last : out Boolean);

private

   protected type Synchronous_Barrier
     (Number_Waiting : Positive_Worker_Count) is

      entry Wait_For_All_Tasks (Released_Last : out Boolean);
   private
      Done : Boolean := False;
   end Synchronous_Barrier;

end Parallel.Synchronous_Barriers;
