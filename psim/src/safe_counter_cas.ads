--  ************************************************************************ --
--  *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
--  *****               FOR  CLASSICAL PARTICLES                       ***** --
--  ************************************************************************ --
--  $Id: types.ads 19 2010-10-05 19:43:13Z  $
--
--  Copyright (C) 2010 Michael Erdmann
--
--  PSim is free software;  you can redistribute it  and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Soft-
--  ware  Foundation;  either version 2,  or (at your option) any later
--  version.  Psim is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should have
--  received  a copy of the GNU General Public License distributed with GNAT;
--  see file COPYING.  If not, write to  the Free Software Foundation,
--  59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.                                                      --
--
--  As a special exception,  if other files  instantiate  generics from this
--  unit, or you link  this unit with other files  to produce an executable,
--  this  unit  does not  by itself cause  the resulting  executable  to  be
--  covered  by the  GNU  General  Public  License.  This exception does not
--  however invalidate  any other reasons why  the executable file  might be
--  covered by the  GNU Public License.
--
with Ada.Finalization;                    use Ada.Finalization;

package Safe_Counter is

   -- a thread safe counter
   type Safe_Counter_Type( Limit : Natural ) is new Controlled with private;

   procedure Increment( Safe : in out Safe_Counter_Type; Result : out Natural );
   procedure Decrement( Safe : in out Safe_Counter_Type; Result : out Natural );
   function Value( Safe : in Safe_Counter_Type ) return Natural ;
   procedure Reset( Safe : in out Safe_Counter_Type; Value : in Natural := 0 );

   function To_String( Safe : in Safe_Counter_Type ) return String;

   Collisions : Natural := 0;

private
   type Object_Data_Type;
   type Object_Data_Access is access all Object_Data_Type;

   type Safe_Counter_Type( Limit : Natural ) is new Controlled with record
      Data : Object_Data_Access := null;
   end record;

   procedure Initialize( This : in out Safe_Counter_Type );
   procedure Finalize( This : in out Safe_Counter_Type );

end Safe_Counter;
