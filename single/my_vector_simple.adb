with Ada.Exceptions;                   use Ada.Exceptions;
with Log;
with Config;                           use Config;
with Vector_Space;                     use Vector_Space;

with Unchecked_Deallocation;

with Time_Measurement;                 use Time_Measurement;
with Timers;                           use Timers;

package body My_Vector is

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type(N : Natural ) is record
         X1       : State_Vector_Access := null;
         X2       : State_Vector_Access := null;
      end record;

   procedure Free is new Unchecked_Deallocation(Object_Data_Type,
                                                Object_Data_Access);

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Object_Type ) is
      Data : constant Object_Data_Access := new Object_Data_Type(N);
   begin
      Data.X2 := new State_Vector_Type( 1..N );
      Data.X1 := new State_Vector_Type( 1..N );

      This.Data := Data;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
   begin
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

   --------------------
   -- Process_Vector --
   --------------------
   procedure Iterate( This : in Object_Type ) is
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
         Start_Lap( T_Forces );
         for J in 1..N loop
            if I /= J then
               K := K + Force(I,J);
            end if;
         end loop;
         Stop_Lap( T_Forces );

         Start_Lap( T_Vector );

         DV := ( DT / X1(I).Mass) * K;
         X2(I).V := X1(I).V + DV;

         DX := DT * X2(I).V + DT**2 / ( 2.0 * X1(I).Mass)* K;
         X2(I).X := X1(I).X + DX;
         X2(I).Mass := X1(I).Mass;

         --X2(i).Error := Scalar_Product( X1(I).Mass * DV + K ,X1(I).V);
         Stop_Lap( T_Vector );

      exception
         when E : others =>
            Log.Error("Exception while processing vector *** " & Exception_Name( E ) & ":" &
                      Exception_Message( E ) & " for element" & Natural'Image(I) &
                      " " & To_String( X1(I) ) );
            raise;
      end ;

      Tmp : constant State_Vector_Access := Data.X1;

   begin
      for i in 1..N loop
         Calculate_Particle( I );
      end loop;

      Data.X1 := Data.X2;
      Data.X2 := Tmp;

   end Iterate;

   --------------
   -- Caculate --
   --------------
   function Calculate(
      This : in Object_Type; Times : in Natural ) return State_Vector_Access is
   begin
      Start_Lap(T_Iteration);
      for i in 1..Times loop
         Iteration_Count := Iteration_Count +1;
         Iterate( This );
      end loop;
      Stop_Lap(T_Iteration);

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
