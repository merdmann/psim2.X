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
         Particle : Safe_Counter_Type( N );

         X1 : State_Vector_Access := null;
         X2 : State_Vector_Access := null;
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
      return "[ null ]" ;
   end To_String;

   --------------------
   -- Process_Vector --
   --------------------
   function Process_Element( This : in Object_Type; Worker : in Integer := 0 ) return Boolean is
      Data    : Object_Data_Access renames This.Data ;
      Current : Natural;
   begin
      Increment( Data.Particle, Current );
      if Current = 0 then
         pragma Debug( Log.Comment("My_Vector.Process_Vector: No more work, Particle" &
             To_String( Data.Particle ) ) );
         return false;
      end if;

      return True;
   end Process_Element;

   --------------------
   -- Collect_Result --
   --------------------
   procedure Collect_Result( This : in Object_Type ) is
      Data    : Object_Data_Access renames This.Data ;
      Tmp     : constant State_Vector_Access := Data.X1;
   begin
      pragma Debug( Log.Comment("My_Vector.Collect_Result") );

      Reset( Data.Particle );

      Data.X1 := Data.X2;
      Data.X2 := Tmp;
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
