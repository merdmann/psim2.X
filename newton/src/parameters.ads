------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--               P A R A L L E L _ T E S T _ H A R N E S S                  --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Storage_Elements;

package Parameters is
   function Work_Budget_For_Even_Loads return Integer;
   function Work_Budget_For_Uneven_Loads return Integer;

   function Use_Affinity return Boolean;

   function Worker_Storage_Size
     return System.Storage_Elements.Storage_Count;

   function Minimum_Steal return Natural;
   function Minimum_Seek return Natural;
   function Debug_Logging return Boolean;

   --  Set to True to disable work seeking where the load is even
   --  and less likely to benefit from work seeking.
   --  This must be defined statically, as it causes code to be
   --  eliminated by the compiler.
   Eliminate_Work_Seeking_For_Even_Loads : constant Boolean := False;

   pragma Compile_Time_Warning
     (Eliminate_Work_Seeking_For_Even_Loads,
      "Work seeking is disabled for even loads");

   --  Generate Compile Time Warnings in places where changes should be
   --  made for Ada 2012
   Ada_2012_Warnings : constant Boolean := False;
end Parameters;
