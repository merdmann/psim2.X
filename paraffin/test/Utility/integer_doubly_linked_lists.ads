------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--         I N T E G E R _ D O U B L Y _ L I N K E D _ L I S T S            --
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

with Ada.Containers.Doubly_Linked_Lists;
with Interfaces;
package Integer_Doubly_Linked_Lists is

   use type Interfaces.Unsigned_32;

   pragma Warnings (Off, "*in instantiation at a-cdlili.adb*");

   package Integer_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Interfaces.Unsigned_32);

   pragma Warnings (On, "*in instantiation at a-cdlili.adb*");

   type List is new Integer_Lists.List with private;
   procedure Reduce (Left, Right : in out List);

   Empty_List : constant List;

private

   type List is new Integer_Lists.List with null record;

   Empty_List : constant List :=
     List'(Integer_Lists.Empty_List with null record);
end Integer_Doubly_Linked_Lists;
