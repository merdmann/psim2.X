--  ************************************************************************ --
--  *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
--  *****               FOR  CLASSICAL PARTICLES                       ***** --
--  ************************************************************************ --
--  $Id: config.adb 24 2010-10-14 16:24:58Z  $
--
--  This File is Part of PSim.THis file provides the configuration data
--  for a given calculation run.
--
--  Copyright (C) 2012 Michael Erdmann
--
--  PSim is Free Software: You Can Redistribute It and/or Modify
--  It Under The Terms of The GNU General Public License As Published By
--  The Free Software Foundation, Either Version 3 of The License, or
--  (at Your Option) Any Later Version.
--
--  This Program is Distributed in The Hope That It Will Be Useful,
--  But WITHOUT ANY WARRANTY; Without Even The Implied Warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See The
--  GNU General Public License for More Details.
--
--  You Should Have Received A Copy of The GNU General Public License
--  Along with This Program.  If not, See <Http://Www.Gnu.Org/Licenses/>.
--
--  As a special exception,  if other files  instantiate  generics from this
--  unit, or you link  this unit with other files  to produce an executable,
--  this  unit  does not  by itself cause  the resulting  executable  to  be
--  covered  by the  GNU  General  Public  License.  This exception does not
--  however invalidate  any other reasons why  the executable file  might be
--  covered by the  GNU Public License.
--

with Ada.Exceptions;                   use Ada.Exceptions;
with Log;
with Config;                           use Config;
with Safe_Counter;                     use Safe_Counter;
with Vector_Space;                     use Vector_Space;

with Unchecked_Deallocation;

with Time_Measurement;                 use Time_Measurement;
with Timers;                           use Timers;

package body My_Vector is

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type(N : Natural ) is record
         Partition : Safe_Counter_Type( N );
         Sync      : Abstract_Sync.Handle := null;
         X1        : State_Vector_Access := null;
         X2        : State_Vector_Access := null;
      end record;

   procedure Free is new Unchecked_Deallocation(Object_Data_Type,
                                                Object_Data_Access);

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Object_Type ) is
      Data : constant Object_Data_Access := new Object_Data_Type(N);
   begin
      Vector_Processor.Initialize( Vector_Processor.Object_Type( This ));

      Reset( Data.Partition );
      Data.X2 := new State_Vector_Type( 1..N );
      Data.X1 := new State_Vector_Type( 1..N );

      This.Data := Data;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
   begin
      Vector_Processor.Finalize( Vector_Processor.Object_Type( This ));
      Free( This.Data );
   end Finalize;

   ---------------
   -- To_String --
   ---------------
   function To_String( State : in Particle_Type ) return String is
   begin
      return "[ X=" & To_String( State.X ) & ", " &
             "V=" & To_String( State.V ) & ", " &
             "M=" & Value_Type'Image( State.Mass ) & " ]" ;
   end To_String;


   -------------
   -- Minimim --
   -------------
   function Minimum( A : in Integer; B : in Integer ) return Integer is
      Result : Integer := B;
   begin
      if A < B then
         Result := A;
      end if;

      return Result;
   end Minimum;


   --------------------
   -- Process_Vector --
   --------------------
   function Process_Element( This : in Object_Type; Worker : in Integer := 0 ) return Boolean is
      Data    : Object_Data_Access renames This.Data ;

      X1      : State_Vector_Access renames Data.X1;
      X2      : State_Vector_Access renames Data.X2;

      procedure Calculate_Particle( I : in Natural ) is
         -----------
         -- Force --
         -----------
         function Force( I,J : in Integer ) return Vector_Type is
            DX : constant Vector_Type := X1(J).X - X1(I).X;
            R  : constant Value_Type := Norm(DX);
            G0 : constant Value_Type := 6.67428E-11;
            F  : constant Value_Type := G0 * X1(I).Mass * X1(J).Mass/(R+RS)**2;
         begin
            return F * DX;
         exception
            when E : others =>
               Log.Error("Exception while calculating force *** " & Exception_Name( E ) & " " &
                         Exception_Message( E ) & " : " &
                         "X1=" & To_String( X1(I) ) & ", " &
                         "X2=" & To_String( X1(J) ) & ", " &
                         "DX=" & To_String( DX ) & ", " &
                         "RS=" & Value_Type'Image( RS ) &
                         Integer'Image(I) & "/" & Integer'Image(J));
               raise;
         end Force;

         DV : Vector_Type;
         DX : Vector_Type;
         K  : Vector_Type := Null_Vector;

      begin
         Start_Lap(T_Forces);
         for J in 1..N loop
            if I /= J then
               K := K + Force(I,J);
            end if;
         end loop;
         Stop_Lap(T_Forces);

         --Log.Comment( Natural'Image(I) & To_String(K) );

         Start_Lap(T_Integration);
         DV := ( DT / X1(I).Mass) * K;
         X2(I).V := X1(I).V + DV;

         DX := DT * X2(I).V + DT**2 / ( 2.0 * X1(I).Mass)* K;
         X2(I).X := X1(I).X + DX;
         X2(I).Mass := X1(I).Mass;

         --X2(i).Error := Scalar_Product( X1(I).Mass * DV + K ,X1(I).V);
         Stop_Lap(T_Integration);

      exception
         when E : others =>
            Log.Error("Exception while processing vector *** " & Exception_Name( E ) & ":" &
                      Exception_Message( E ) & " for element" & Natural'Image(I) &
                      " " & To_String( X1(I) ) );
            Log.Error_Counter := Log.Error_Counter + 1;
            raise;
      end Calculate_Particle;

      Size : constant Natural := N / (Nbr_Of_Workers-1) ;

      Min  : constant Natural := 1 + (Worker-1) * Size;
      Max  : constant Natural := Minimum( N, Min + Size - 1 );
   begin
      pragma Debug( Log.Comment("My_Vector.Process_Vector: Partition" & Natural'Image(Worker)) );
      pragma Debug( Log.Comment( "Min=" & Natural'Image(Min) & ", " &
                     "Max=" & Natural'Image(Max) & Natural'Image(Size) & Natural'Image(Worker)) );

      for I in Min .. Max loop
         Start_Lap( T_Vector );
         Calculate_Particle( I );
         Stop_Lap( T_Vector );
      end loop;

      return True;
   exception
      when others =>
         return False;

   end Process_Element;

   --------------------
   -- Collect_Result --
   --------------------
   procedure Collect_Result( This : in Object_Type ) is
      Data    : Object_Data_Access renames This.Data ;
      Tmp     : constant State_Vector_Access := Data.X1;
   begin
      pragma Debug( Log.Comment("My_Vector.Collect_Result") );
      Data.X1 := Data.X2;
      Data.X2 := Tmp;

      Reset( Data.Partition );
   end Collect_Result;

   --------------
   -- Caculate --
   --------------
   function Calculate(
      This : in Object_Type; Times : in Natural ) return State_Vector_Access is
   begin
      Execute( This, Times);
      return This.Data.X1;
   end ;

   ----------------------
   -- Get_State_Vector --
   ----------------------
   function Get_State_Vector( This : in Object_Type ) return State_Vector_Access is
   begin
      return This.Data.X1 ;
   end Get_State_Vector;

end My_Vector;
