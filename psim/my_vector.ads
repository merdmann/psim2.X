with Types;                            use Types;
with Vector_Processor;                 use Vector_Processor;
with Abstract_Sync;                    use Abstract_Sync;

package My_Vector is

   type Object_Type( Sync : Abstract_Sync.Handle ) is new Vector_Processor.Object_Type with private;

   function Get_State_Vector( This : in Object_Type ) return State_Vector_Access ;

   function Process_Element( This : in Object_Type; Worker : in Integer := 0 ) return Boolean;
   procedure Collect_Result( This : in  Object_Type);

   function Calculate(
      This : in Object_Type; Times : in Natural ) return State_Vector_Access ;

private
   type Object_Data_Type;
   type Object_Data_Access is access all Object_Data_Type;

   type Object_Type( Sync : Abstract_Sync.Handle ) is new Vector_Processor.Object_Type(Sync) with record
      Data : Object_Data_Access;
   end record;

   procedure Initialize( This : in out Object_Type );
   procedure Finalize( This : in out Object_Type );

end My_Vector;