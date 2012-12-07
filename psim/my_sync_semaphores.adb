
with Log;                                 use Log;
with Time_Measurement;		      use Time_Measurement;
with Timers;			      use Timers;


package body My_Sync is

   procedure  registerClient(Num : Integer);
   pragma Import (C, registerClient, "SEM_registerClient");

   procedure  unregisterClient(Num : Integer);
   pragma Import (C, unregisterClient, "SEM_unregisterClient");

   procedure  Wait(Num : Integer);
   pragma Import (C, Wait, "SEM_wait");

   procedure  Wakeup(Num : Integer);
   pragma Import (C, Wakeup, "SEM_wakeup");

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( Max_Worker : Client_Id_Type ) is null record;


   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Object_Type ) is
      Max_Client_Id : constant Client_Id_Type := Client_Id_Type(This.Workers);
   begin
      for i in 1..Max_Client_Id loop
         registerClient(Integer(i));
      end loop;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
     Max_Client_Id : constant Client_Id_Type := Client_Id_Type(This.Workers);
   begin
      pragma Debug( Log.Comment("My_Sync.Finalize") );

     for i in 1..Max_Client_Id loop
         unregisterClient(Integer(i));
      end loop;

   end Finalize;

   ---------------
   -- To_String --
   ---------------
   function To_String( This : in Object_Type ) return String is
      Data : Object_Data_Access renames This.Data;
      Max_Client_Id : constant Client_Id_Type := Client_Id_Type(This.Workers);
   begin
      return "[ no instance data available ]";
   end To_String;

   ------------
   -- Wakeup --
   ------------
   procedure Wakeup( This : in out Object_Type; Id : in Client_Id_Type ) is
      Data : Object_Data_Access renames This.Data;
   begin
      pragma Debug(Log.Comment( "My_Sync.Wakeup: " & To_String(This) & ", id=" & Client_Id_Type'Image(id)));

      Wakeup(Integer(Id));

   end Wakeup;

   -----------
   -- Sleep --
   -----------
   procedure Sleep( This : in out Object_Type; Id : in Client_Id_Type ) is
      Data : Object_Data_Access renames This.Data;
   begin
      pragma Debug( Log.Comment( "My_Sync.Sleep: " & To_String(This) & ", id=" & Client_Id_Type'Image(id)));

      Start_Lap( T_Sleep );
      Wait( Integer(Id) );
      Stop_Lap( T_Sleep );

   end Sleep;

end My_Sync;
