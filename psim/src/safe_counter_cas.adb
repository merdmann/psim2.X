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
with Unchecked_Deallocation;

with Sync;                                   use Sync;
with Log;                                    use Log;

package body Safe_Counter is

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type is record
         Counter : aliased Natural := 0;
      end record;

   procedure Free is new Unchecked_Deallocation(Object_Data_Type,
                                                Object_Data_Access);
   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Safe_Counter_Type ) is
   begin
      pragma Debug( Log.Comment("Safe_Counter_Cas.Initialize" ) );

      This.Data := new Object_Data_Type;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Safe_Counter_Type ) is
   begin
      pragma Debug( Log.Comment("Safe_Counter_Cas.Finalize" ) );

      Free( This.Data );
   end Finalize;

   -----------
   -- Reset --
   -----------
   procedure Reset( Safe : in out Safe_Counter_Type; Value : in Natural := 0 ) is
      Data : Object_Data_Access renames Safe.Data;
      T    : Natural := Data.Counter;
   begin
      pragma debug ( Log.Comment("Safe_Counter_Cas.Reset --> " ) );

      while not CAS( Data.Counter'Address, T, Value) and T > 0 loop
         T := Data.Counter;
         Collisions := Collisions + 1;
      end loop;

      pragma debug ( Log.Comment("Safe_Counter_Cas.Reset <-- " ) );
   end Reset;

   -----------
   -- Value --
   -----------
   function Value( Safe : in Safe_Counter_Type ) return Natural is
      Data : Object_Data_Access renames Safe.Data;
   begin
      return Data.Counter;
   end Value;

   ---------------
   -- To_String --
   ---------------
   function To_String( Safe : in Safe_Counter_Type ) return String is
      Data : Object_Data_Access renames Safe.Data;
   begin
      return "[limit="  & Natural'Image(Safe.Limit) & ", " &
      "value=" & Natural'Image( Data.Counter ) & ", " &
      "Collisions=" & Natural'Image(Collisions) & " ]";
   end To_String;

   ---------------
   -- Increment --
   ---------------
   procedure Increment( Safe : in out Safe_Counter_Type; Result : out Natural ) is
      Data : Object_Data_Access renames Safe.Data;
   begin
      pragma debug ( Log.Comment("Safe_Counter_Cas.Increment --> " ) );

      loop
         declare
            Expected : constant Natural := Data.Counter;
         begin
            Result := Expected + 1;

            pragma Debug( Log.Comment( Natural'Image(Expected) & Natural'Image(Result) ) );

            exit when CAS( Data.Counter'Address, Expected, Result );

            Collisions := Collisions + 1;
         end;
      end loop;

      if Result > Safe.Limit then
         Result := 0;
      end if;

      pragma debug ( Log.Comment("Safe_Counter_Cas.Increment <-- "  & Natural'Image(Result) )  );
   end Increment;

   ---------------
   -- Decrement --
   ---------------
   procedure Decrement( Safe : in out Safe_Counter_Type; Result : out Natural ) is
      Data : Object_Data_Access renames Safe.Data;
      T    : Natural := Data.Counter;
   begin
      if T > 0 then
         while not CAS( Data.Counter'Address, T, T-1) and T > 0 loop
            T := Data.Counter;
            Collisions := Collisions + 1;
         end loop;

         Result := T;
      else
         Result := 0;
      end if;
   end Decrement;

end Safe_Counter;
