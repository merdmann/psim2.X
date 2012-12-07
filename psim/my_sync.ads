with Abstract_Sync;                 use Abstract_Sync;

package My_Sync is

   type Object_Type( Workers : Natural ) is new Abstract_Sync.Object_Type with private;

   function To_String( This : in Object_Type ) return String;

   procedure Wakeup( This : in out Object_Type; Id : in Client_Id_Type );
   procedure Sleep( This : in out Object_Type; Id : in Client_Id_Type );

private
   type Object_Data_Type(Max_Worker : Client_Id_Type );
   type Object_Data_Access is access all Object_Data_Type;

   type Object_Type( Workers : Natural ) is new Abstract_Sync.Object_Type( Workers ) with record
      Data : Object_Data_Access;
   end record;

   procedure Initialize( This : in out Object_Type );
   procedure Finalize( This : in out Object_Type );

end My_Sync;
