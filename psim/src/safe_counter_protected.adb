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

package body Safe_Counter is

   ----------------------
   -- Object_Data_Type --
   ----------------------
   protected type Object_Data_Type is
      procedure Increment( Result : out Natural );
      procedure Decrement(  Result : out Natural );
      procedure Reset( Value : in Natural );
      function Value return Natural;
   private
      Counter : Integer := 0;
   end Object_Data_Type;


   procedure Free is new Unchecked_Deallocation(Object_Data_Type,
                                                Object_Data_Access);


   protected body Object_Data_Type is
      procedure Increment( Result : out Natural ) is
      begin
         Counter := Counter + 1;
         Result := Counter;
      end Increment;

      procedure Decrement( Result : out Natural ) is
      begin
         Counter := Counter - 1;
         if Counter < 0 then
            Result := 0;
         else
            Result := Counter;
         end if;
      end Decrement;

      procedure Reset( Value : in Natural ) is
      begin
         Counter := Value;
      end Reset;

      function Value return Natural is
      begin
         return Counter;
      end Value;
   end Object_Data_Type;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Safe_Counter_Type ) is
   begin
      This.Data := new Object_Data_Type;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Safe_Counter_Type ) is
   begin
      Free( This.Data );
   end Finalize;


   ---------------
   -- To_String --
   ---------------
   function To_String( Safe : in Safe_Counter_Type ) return String is
   begin
      return "[limit="  & Natural'Image(Safe.Limit) & ", value=" & Natural'Image( Safe.Data.Value ) & " ]";
   end To_String;

   ---------------
   -- Increment --
   ---------------
   procedure Increment( Safe : in out Safe_Counter_Type; Result : out Natural ) is
   begin
      Safe.Data.Increment( Result );

       if Result > Safe.Limit then
         Result := 0;
      end if;

      --Put_Line( To_String(Safe) & ", Result=" & Natural'Image(Result) );
   end Increment;

   ---------------
   -- Decrement --
   ---------------
   procedure Decrement( Safe : in out Safe_Counter_Type; Result : out Natural ) is
   begin
      Safe.Data.Decrement( Result );
   end Decrement;

   -----------
   -- Reset --
   -----------
   procedure Reset( Safe : in out Safe_Counter_Type; Value : in Natural := 0 ) is
   begin
      Safe.Data.Reset(Value);
   end Reset;

   -----------
   -- Value --
   -----------
   function Value( Safe : in Safe_Counter_Type ) return Natural is
   begin
      return Safe.Data.Value;
   end Value;

end Safe_Counter;
