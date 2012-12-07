with Unchecked_Deallocation;

with Sync;                                use Sync;
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Log;                                 use Log;
with Time_Measurement;                    use Time_Measurement;
with Timers;                              use Timers;

package body My_Sync is

   ----------------
   -- Sleep_Type --
   ----------------
   protected type Sleep_Type is
      entry Sleep;
      procedure Wakeup;
      function Image return String;
   private
      Ready : Boolean := False;
   end Sleep_Type;

   protected body Sleep_Type is
      entry Sleep when Ready is
      begin
         Ready := False;
      end Sleep;

      procedure Wakeup is
      begin
         Ready := True;
      end Wakeup;

      function Image return String is
      begin
         return Boolean'Image( Ready );
      end Image;

   end Sleep_Type;

   type Sleep_Array is array( Client_Id_Type range <> ) of Sleep_Type;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( Max_Worker : Client_Id_Type ) is record
      S : Sleep_Array( 0..Max_Worker ) ;
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
            S := S & " " & Data.S(I).Image;
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
      Data    : Object_Data_Access renames This.Data;
   begin
      pragma Debug(Log.Comment( "My_Sync.Wakeup: " & To_String(This) & ", id=" & Client_Id_Type'Image(id)));

      Data.S(Id).Wakeup;

      pragma Debug(Log.Comment( "My_Sync.Wakeup: done " & To_String(This) & ", id=" & Client_Id_Type'Image(id)));
   end Wakeup;

   -----------
   -- Sleep --
   -----------
   procedure Sleep( This : in out Object_Type; Id : in Client_Id_Type ) is
      Data    : Object_Data_Access renames This.Data;
   begin
      pragma Debug( Log.Comment( "My_Sync.Sleep: " & To_String(This) & ", id=" & Client_Id_Type'Image(id)));

      Start_Lap( T_Sleep );
      Data.S(Id).Sleep;
      Stop_Lap( T_Sleep);

      pragma Debug( Log.Comment( "My_Sync.Sleep: exiting " & To_String(This) & ", id=" & Client_Id_Type'Image(id)));
   end Sleep;

end My_Sync;
