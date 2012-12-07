------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L .                             --
--                  S Y N C H R O N O U S _ B A R R I E R S                 --
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

package body Parallel.Synchronous_Barriers is

   ----------------------
   -- Wait_For_Release --
   ----------------------

   procedure Wait_For_Release
     (Barrier : in out Synchronous_Barrier)
   is
      Dont_Care : Boolean;
   begin
      Barrier.Wait_For_All_Tasks (Dont_Care);
   end Wait_For_Release;

   procedure Wait_For_Release
     (Barrier : in out Synchronous_Barrier;
      Released_Last : out Boolean)
   is
   begin
      Barrier.Wait_For_All_Tasks (Released_Last);
   end Wait_For_Release;

   protected body Synchronous_Barrier is

      entry Wait_For_All_Tasks
        (Released_Last : out Boolean)
        when Done or else Wait_For_All_Tasks'Count = Number_Waiting is
      begin
         if Wait_For_All_Tasks'Count = 0 then
            Released_Last := True;
            Done := False;
         else
            Done := True;
            Released_Last := False;
         end if;
      end Wait_For_All_Tasks;
   end Synchronous_Barrier;

end Parallel.Synchronous_Barriers;
