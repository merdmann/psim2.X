------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--          S E Q U E N T I A L _ D O U B L E _ F I B O N A C C I           --
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

--  Technically, I don't believe the term double fibonnaci exists.
--  I wanted an example where there could be more than two branches of
--  recursion, so here double fibonacci is a fibonacci-like calculation that
--  adds in another term to the recursion. I don't make any claims that this
--  formula calculates anything useful.
function Sequential_Double_Fibonacci (Value : Natural) return Natural is
begin
   if Value < 3 then
      if Value < 2 then
         return 0;
      else
         return 1;
      end if;

   else
      return
        Sequential_Double_Fibonacci (Value - 3) +
        Sequential_Double_Fibonacci (Value - 2) +
        Sequential_Double_Fibonacci (Value - 1);
   end if;
end Sequential_Double_Fibonacci;
