with Types;                               use Types;
with Ada.Finalization;                    use Ada.Finalization;

package My_Vector is

   type Object_Type is new Controlled with private;

   function Get_State_Vector( This : in Object_Type ) return State_Vector_Access ;

   function Calculate(
      This : in Object_Type; Times : in Natural ) return State_Vector_Access ;

   Iteration_Count : Natural := 0;

private
   type Object_Data_Type;
   type Object_Data_Access is access all Object_Data_Type;

   type Object_Type is new Controlled with record
      Data : Object_Data_Access;
   end record;

   procedure Initialize( This : in out Object_Type );
   procedure Finalize( This : in out Object_Type );

end My_Vector;
