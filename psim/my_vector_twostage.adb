with Ada.Exceptions;                   use Ada.Exceptions;
with Log;
with Config;                           use Config;
with Safe_Counter;                     use Safe_Counter;
with Vector_Space;                     use Vector_Space;

with Unchecked_Deallocation;

with Time_Measurement;                 use Time_Measurement;
with Timers;                           use Timers;

package body My_Vector is

   type Force_Array  is array( Natural range <> ) of Vector_Type;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type(N : Natural ) is record
         Particle : Safe_Counter_Type( N );
         Sync     : Abstract_Sync.Handle := null;
         X1       : State_Vector_Access := null;
         X2       : State_Vector_Access := null;
         K        : Force_Array( 1..N ) := (others=> Null_Vector);
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

      Reset( Data.Particle );
      Log.Comment("N=" & Natural'Image(N) & To_String( Data.Particle) );

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

   ---------------------
   -- Process_Element --
   ---------------------
   function Process_Element( This : in Object_Type; Worker : in Integer := 0 ) return Boolean is
      Data    : Object_Data_Access renames This.Data ;
      Current : Natural;

      X1      : State_Vector_Access renames Data.X1;
      X2      : State_Vector_Access renames Data.X2;

      function Force( I,J : in Integer ) return Vector_Type is
         DX : constant Vector_Type := X1(J).X - X1(I).X;
         R  : constant Value_Type := Norm(DX);
         G0 : constant Value_Type := 6.67428E-11;
         F  : constant Value_Type := G0 * X1(I).Mass * X1(J).Mass/(R+RS)**2;
      begin
         --Log.Comment(Natural'Image(I)& Natural'Image(J) & " R=" & Value_Type'Image(R));

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
   begin
      loop
         -- fetch an element to work on until not more particles
         Increment( Data.Particle, Current );
         if Current = 0 then
            pragma Debug( Log.Comment("My_Vector.Process_Vector: No more work, Particle" &
                To_String( Data.Particle ) ) );
            return false;
         end if;

         Data.K(Current) := Null_Vector;
         Start_Lap(T_Forces );
         for J in 1..N loop
            if J < Current then
               declare
                  F : constant Vector_Type := Force(Current,J);
               begin
                  Data.K(Current) := Data.K(Current) + F;
                  Data.K(J) := Data.K(J) - F;
               end;
            end if;
         end loop;
         Stop_Lap(T_Forces);
      end loop;

      return True;

   exception
      when E : others =>
         Log.Error("Exception while calculating force *** " & Exception_Name( E ) & " " &
                     Exception_Message( E ) & " : " );
         raise;

   end Process_Element;

   --------------------
   -- Collect_Result --
   --------------------
   procedure Collect_Result( This : in Object_Type ) is
      Data    : Object_Data_Access renames This.Data ;
      Tmp     : constant State_Vector_Access := Data.X1;

      X1      : State_Vector_Access renames Data.X1;
      X2      : State_Vector_Access renames Data.X2;
   begin
      pragma Debug( Log.Comment("My_Vector.Collect_Result") );

      for i in 1 .. N loop
         declare
            DV : Vector_Type;
            DX : Vector_Type;
            K  : constant Vector_Type := Data.K(i);
         begin
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
         end;
      end loop;

      Data.X1 := Data.X2;
      Data.X2 := Tmp;

      Data.K := (others => Null_Vector);
      Reset( Data.Particle );
   end ;

   --------------
   -- Caculate --
   --------------
   function Calculate(
      This : in Object_Type; Times : in Natural ) return State_Vector_Access is
      Data : Object_Data_Access renames This.Data ;
   begin
      Execute( This, Times);     -- calculate the matrix elements
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
