with Ada.Exceptions;                   use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;

with Log;
with Config;                           use Config;
with Safe_Counter;                     use Safe_Counter;
with Vector_Space;                     use Vector_Space;

with Unchecked_Deallocation;

with Time_Measurement;                 use Time_Measurement;
with Timers;                           use Timers;

package body My_Vector is

   type Counter_Array is array( Natural range <> ) of Natural;
   type Counter_Array_Access is access all Counter_Array;

   package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions (
     Value_Type);
   use Value_Functions;



   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type(N : Natural ) is record
         Particle : Safe_Counter_Type( N );
         Items    : Counter_Array_Access := null;
         Sync     : Abstract_Sync.Handle := null;
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
      Vector_Processor.Initialize( Vector_Processor.Object_Type( This ));

      Reset( Data.Particle );
      Data.X2 := new State_Vector_Type( 1..N );
      Data.X1 := new State_Vector_Type( 1..N );

      Data.Items := new Counter_Array( 1..Nbr_Of_Workers );
      Data.Items(1..Nbr_Of_Workers) := (others => 0);
      This.Data := Data;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
      Data : Object_Data_Access renames This.Data;
      Sum  : Natural := 0;
   begin
      for i in Data.Items'Range loop
         Log.Comment("Items("& Natural'Image(I) & ")=" & Natural'Image(Data.Items(i)));

         Sum := Sum + Data.Items(i);
      end loop;

      Log.Comment("Total Items:" & Natural'Image(Sum));

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

   --------------------
   -- Process_Vector --
   --------------------
   function Process_Element( This : in Object_Type; Worker : in Integer := 0 ) return Boolean is
      Data    : Object_Data_Access renames This.Data ;
      Current : Natural;
      Items   : Natural := 0;

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
            F  : constant Value_Type := G0 * X1(I).Mass * X1(J).Mass/R**2;
         begin
            if R < RS then
               return Value_Type(-1.0)* Exp( 1.0 / R ) * DX;
            else
               return F * DX;
         end if;

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

         --Log.Comment("K" & Natural'Image(I) & ";" & To_String(K));

         Start_Lap( T_Integration );
         DV := ( DT / X1(I).Mass) * K;
         X2(I).V := X1(I).V + DV;

         DX := DT * X2(I).V + DT**2 / ( 2.0 * X1(I).Mass)* K;
         X2(I).X := X1(I).X + DX;
         X2(I).Mass := X1(I).Mass;

         --X2(i).Error := Scalar_Product( X1(I).Mass * DV + K ,X1(I).V);
         Stop_Lap( T_Integration );

      exception
         when E : others =>
            Log.Error("Exception while processing vector *** " & Exception_Name( E ) & ":" &
                      Exception_Message( E ) & " for element" & Natural'Image(I) &
                      " " & To_String( X1(I) ) );
            raise;
      end ;

      Done : Boolean := False;
   begin

      while not Done loop
         -- fetch an element to work on
         Increment( Data.Particle, Current );
         if Current = 0 then
            pragma Debug( Log.Comment("My_Vector.Process_Vector: No more work, Particle" &
                     To_String( Data.Particle ) ) );
            Done := True;
         else
            pragma Debug( Log.Comment("My_Vector.Process_Vector: Particle" & Natural'Image( Current )) );

            Data.Items(Worker) := Data.Items(Worker) + 1;

            Start_Lap( T_Vector );
            Calculate_Particle( Current );
            Stop_Lap( T_Vector );
         end if;
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

      Reset( Data.Particle );
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
