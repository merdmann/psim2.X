with Unchecked_Deallocation;

with Ada.Synchronous_Task_Control;        use Ada.Synchronous_Task_Control;
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Log;                                 use Log;
with Time_Measurement;			  use Time_Measurement;
with Timers;				  use Timers;


package body My_Sync is

   -----------------------------
   -- Suspension_Object_Array --
   -----------------------------
   type Suspension_Object_Array is array( Client_Id_Type range <> ) of Suspension_Object;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( Max_Worker : Client_Id_Type ) is record
      S : Suspension_Object_Array( 0..Max_Worker );
   end record;

   ----------
   -- Free --
   ----------
   procedure Free is new Unchecked_Deallocation(Object_Data_Type,
                                                Object_Data_Access);

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Object_Type ) is
      Data : Object_Data_Access renames This.Data;
      Max_Client_Id : constant Client_Id_Type := Client_Id_Type(This.Workers);
   begin
      Initialize( Abstract_Sync.Object_Type( This ) );
      Data :=  new Object_Data_Type( Max_Client_Id );

      for i in Client_Id_Type'First .. Max_Client_Id loop
         Set_False( Data.S(i) );
      end loop;

   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
   begin
      pragma Debug( Log.Comment("My_Sync.Finalize") );

      Free( This.Data );
      Finalize( Abstract_Sync.Object_Type( This ) );
   end Finalize;

   ---------------
   -- To_String --
   ---------------
   function To_String( This : in Object_Type ) return String is
      Data : Object_Data_Access renames This.Data;
      Max_Client_Id : constant Client_Id_Type := Client_Id_Type(This.Workers);

      S : Unbounded_String := To_Unbounded_String("[" & To_String( Abstract_Sync.Object_Type(This)));
   begin
      if Data /= null then
         for i in Client_Id_Type'First .. Max_Client_Id loop
            S := S & " " & Boolean'Image( Current_State(Data.S(I)) );
         end loop;
         return To_String(S) & "]" ;
      else
         return "[ no instance data available ]";
      end if;
   end To_String;

   ------------
   -- Wakeup --
   ------------
   procedure Wakeup( This : in out Object_Type; Id : in Client_Id_Type ) is
      Data : Object_Data_Access renames This.Data;
   begin
      pragma Debug(Log.Comment( "My_Sync.Wakeup: " & To_String(This) & ", id=" & Client_Id_Type'Image(id)));
      Set_True( Data.S(Id) );
   end Wakeup;

   -----------
   -- Sleep --
   -----------
   procedure Sleep( This : in out Object_Type; Id : in Client_Id_Type ) is
      Data : Object_Data_Access renames This.Data;
   begin
      pragma Debug( Log.Comment( "My_Sync.Sleep: " & To_String(This) & ", id=" & Client_Id_Type'Image(id)));
	
      Start_Lap( T_Sleep );
      if Data /= null then
         Suspend_Until_True( Data.S(Id) );
         Set_False( Data.S(Id) );
      end if;
      Stop_Lap( T_Sleep );

   end Sleep;

end My_Sync;
