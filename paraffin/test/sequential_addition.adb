------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                 S E Q U E N T I A L _ A D D I T I O N                    --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada;
with Ada.Real_Time;
with Ada.Calendar.Formatting;

procedure Sequential_Addition
is
   type Modular_Integer is mod 2 ** 32;
   Sum        : Modular_Integer                 := 0;
   Start_Time : constant Real_Time.Time := Real_Time.Clock;
   Loop_End : constant Integer := Integer'Value (Command_Line.Argument (1));
begin

   Put ("** Sequential Integer Sum = ");

   for I in 1 .. Loop_End loop
      Sum := Sum + Modular_Integer (I);
   end loop;

   Put_Line
     (Modular_Integer'Image (Sum) &
      ", Elapsed = " &
      Calendar.Formatting.Image
        (Elapsed_Time          =>
           Real_Time.To_Duration
             (Real_Time."-"
                  (Left  => Real_Time.Clock,
                   Right => Start_Time)),
         Include_Time_Fraction => True));

end Sequential_Addition;
