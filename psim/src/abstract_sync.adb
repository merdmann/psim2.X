--  ************************************************************************ --
--  *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
--  *****               FOR  CLASSICAL PARTICLES                       ***** --
--  ************************************************************************ --
--  $Id: config.adb 24 2010-10-14 16:24:58Z  $
--
--  This File is Part of PSim.THis file provides the configuration data
--  for a given calculation run.
--
--  Copyright (C) 2012 Michael Erdmann
--
--  PSim is Free Software: You Can Redistribute It and/or Modify
--  It Under The Terms of The GNU General Public License As Published By
--  The Free Software Foundation, Either Version 3 of The License, or
--  (at Your Option) Any Later Version.
--
--  This Program is Distributed in The Hope That It Will Be Useful,
--  But WITHOUT ANY WARRANTY; Without Even The Implied Warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See The
--  GNU General Public License for More Details.
--
--  You Should Have Received A Copy of The GNU General Public License
--  Along with This Program.  If not, See <Http://Www.Gnu.Org/Licenses/>.
--
--  As a special exception,  if other files  instantiate  generics from this
--  unit, or you link  this unit with other files  to produce an executable,
--  this  unit  does not  by itself cause  the resulting  executable  to  be
--  covered  by the  GNU  General  Public  License.  This exception does not
--  however invalidate  any other reasons why  the executable file  might be
--  covered by the  GNU Public License.
--
--
-- This abstract package provide the communication infrastructure between
-- the dispatcher and the worker processes.
--
-- The dispatcher indicates to the worker group the that data is available
-- and is receives from the object an indication that all workers have
-- finished there work.
--
-- Each worker waits until it receives an indication that data should be
-- processed.
--
-- Dispatcher                           Client
-- ==========                          ========
--      |           Register               |
---     +<---------------------------------+
--      |                                  |
--     (1)                            Wait_For_Input
--      |                                  |
--  Indicate_Data_Available               (2)
--      |                                  :
--  Wake_All --------[wakeup]------------->:
--      :                                  |
--      :                              Processing
--      :                                  |
--      :                            Wait_For_Input
--      :                                  |
--      X<-----------[wakeup]--------------+ If last client
--      |                                  |
--    goto 1                             goto 2
--
with Ada.Task_Attributes;
with Ada.Task_Identification;       use Ada.Task_Identification;
with Unchecked_Deallocation;
use Ada;

with CPU;                           use CPU;
with Safe_Counter;                  use Safe_Counter;
with Log;                           use Log;

package body Abstract_sync is

   Nbr_Of_Clients : Safe_Counter_Type(999999);

   CPUs : constant Integer := Number_Of_Cpus;
   -----------------
   -- Client_Type --
   -----------------
   type Client_Type( Id : Client_Id_Type ) is record
      TID  : Task_Id;
   end record;

   type Client_Access is access all Client_Type;
   type Client_Array is array( Client_Id_Type range <> ) of Client_Access;

   package Per_Task is new Ada.Task_Attributes( Attribute => Client_Access,
                                               Initial_value => null);
   use Per_Task;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( Clients : Client_Id_Type ) is record
      CT      : Client_Array( 0..CLients) := (others => null);
      Waiting : Safe_Counter_Type(999999);
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
      Data : constant Object_Data_Access := new Object_Data_Type( CLient_Id_Type(This.Clients) );
   begin
      This.Data := Data;
      Data.CT(Dispatcher_ID) := new Client_Type(Dispatcher_ID);

      pragma Debug( Log.Comment( "Number of cpu's" & Natural'Image(CPUs)) );
   end ;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
   begin
      pragma Debug( Log.Comment("Abstract_Sync.Finalize"));
      Free( This.Data );
   end Finalize;

   ---------------
   -- To_String --
   ---------------
   function To_String( This : in Object_Type ) return String is
      Data : Object_Data_Access renames This.Data;
   begin
      return "[ clients:" & Client_Id_Type'Image(Data.Clients) & ", " &
                "waiting:" & Natural'Image(Value(Data.Waiting)) & ", " &
                "cpus:" & Integer'Image( CPUs ) &
             " ]";
   end To_String;

   --------------
   -- Register --
   --------------
   procedure Register( This : in out Object_Type'Class ) is
      Data   : Object_Data_Access renames This.Data;
      Id     : Client_Id_Type := 0;
      Result : Natural;
      My_Cpu : Natural;
   begin
      -- allocate the client identifier
      Increment( Nbr_Of_Clients, Result);
      pragma Debug( Log.Comment("Abstract_Sync.Register: Id" & Natural'Image(Result)) );
      Id := Client_Id_Type(Result);

      Data.CT(Id) := new Client_Type(Id);
      Per_Task.Set_Value( Data.CT(Id) );

      -- distribute the worker processes over the available CPUs. If possible
      -- we reserve one CPU (Cpu 0) for the operating processes. In case of dual core
      -- leave it to the operating system
      if CPUs > 2 then
         My_Cpu := 1 + Result mod (CPUs-1);

         Log.Comment("Worker started, My_Cpu :" & Natural'Image(My_Cpu));

         Bind_To_Cpu( My_Cpu );
      end if;

      pragma Debug( Log.Comment("Abstract_Sync.Register(This: " & To_String(This) &
                            ") bound to cpu" & Natural'Image(My_Cpu) ) );
   end Register;

   ----------------
   -- Unregister --
   ----------------
   procedure Unregister( This : in out Object_Type'Class ) is
      Data   : Object_Data_Access renames This.Data;
      Client : constant Client_Access := Per_Task.Value ;
   begin
      pragma Debug( Log.Comment("Abstract_Sync.Unregister id=" & Client_Id_Type'Image(Client.Id)) );

      Data.CT(Client.Id) := null;
   end Unregister;

   --------------------
   -- Wait_For_Input --
   --------------------
   procedure Wait_For_Input( This : in out Object_Type'Class ) is
      Data   : Object_Data_Access renames This.Data;
      Client : constant Client_Access := Per_Task.Value ;
      --Max_Clients : constant Client_Id_Type := Client_Id_Type( Value( Nbr_Of_Clients ) );

      function Is_Last_Client return Boolean is
         Result : Natural := 0;
      begin
         Increment( Data.Waiting, Result );
         pragma Debug( Log.comment("Abstract_Sync.Wait_For_Input: Waiting processes:" & Natural'Image(Result))) ;

         return Result = Value( Nbr_Of_CLients );
      end Is_Last_Client;

   begin
      pragma Debug( Log.comment("--> Abstract_Sync.Wait_For_Input") );

      if Is_Last_Client then
         Wakeup( This, Dispatcher_Id );
      end if;

      Sleep( This, Client.Id );
      pragma Debug( Log.Comment("<-- Anstract_Sync.Wait_For_Input") );
   end Wait_For_Input;

   ------------------------------
   -- Indicate_Input_Avaialble --
   ------------------------------
   procedure Indicate_Input_Available( This : in out Object_Type'Class ) is
      Data   : Object_Data_Access renames This.Data;
   begin
      pragma Debug( Log.Comment("-----> Abstract_Sync.Indicate_Input_Avaialbe") );

      Wakeup_All( This );

      Sleep(This, Dispatcher_Id);
      Reset( Data.Waiting );

      pragma Debug(Log.Comment("<------ Abstract_Sync.Indicate_Input_Avaialbe"));
   end;

   ------------------
   -- Wait_For_All --
   ------------------
   procedure Wait_For_All( This : in out Object_Type'Class ) is
      Data   : Object_Data_Access renames This.Data;
   begin
      pragma Debug( Log.Comment( "Abstract_Sync.Wait_For_All " & To_String(This)));

      Sleep(This, Dispatcher_Id);
      Reset( Data.Waiting );

      pragma Debug( Log.Comment( "Abstract_Sync.Wait_For_All return " & To_String(This)));
   end Wait_For_All;


   ------------
   -- Wakeup --
   ------------
   procedure Wakeup_All( This : in out Object_Type'Class ) is
      Data : Object_Data_Access renames This.Data;
   begin
      pragma Debug( Log.Comment("Abstract_Sync.Wakeup_All " & To_String(This)) );

      for i in Data.CT'Range loop
         if Data.CT(i) /= null and I /= Dispatcher_Id then
            Wakeup(This,Data.CT(i).Id );
         end if;
      end loop;
   end Wakeup_All;

end Abstract_Sync;
