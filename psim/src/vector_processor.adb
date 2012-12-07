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
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Synchronous_Task_Control;  use Ada.Synchronous_Task_Control;
with Unchecked_Deallocation;
use Ada;

with Log;
with Time_Measurement;              use Time_Measurement;
with Config;                        use Config;
with Safe_Counter;                  use Safe_Counter;
with Timers;                        use Timers;

package body Vector_Processor is

   ----------------------
   -- Worker_Task_Type --
   ----------------------
   task type Worker_Task_Type( id : Natural; This : Handle  );
   type Worker_Task_Access is access all Worker_Task_Type;

   --------------
   -- TCB_Type --
   --------------
   type TCB_Type is record
      Worker : Worker_Task_Access := null;
   end record;

   type TCB_Array is array( Natural range <> ) of TCB_Type;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( Nbr_Of_Workers : Natural ) is record
      Self              : Handle;
      TCB               : TCB_Array(1..Nbr_Of_Workers);
      Shutdown          : Boolean := False;

      Workers_Completed : Safe_Counter_Type(Nbr_Of_Workers);  -- the number of active workers
      All_Finalized     : Suspension_Object;
   end record;

   procedure Free is new Unchecked_Deallocation(Object_Data_Type,
                                                Object_Data_Access);

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Object_Type ) is
      Data : Object_Data_Access;
   begin
      -- create the data instance
      Data := new Object_Data_Type( Nbr_Of_Workers );
      Data.Self := This'Unchecked_Access;
      This.Data := Data;

      Log.Comment("Abstract_Sync.Initialize: Number of workers: " & Natural'Image(Nbr_Of_Workers) );

      -- initialize the execution environment
      Reset( Data.Workers_Completed );

      for I in 1..Nbr_Of_Workers loop
         Data.TCB(I).Worker := new Worker_Task_Type(I, This'Unchecked_Access);
      end loop;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
      Data : Object_Data_Access renames This.Data;
   begin
      pragma Debug( Log.comment( "Vector_Processor.Finalize" ) );

      if This.Data /= null then
         Set_False( Data.All_Finalized );
         Data.Shutdown := True;
         Wakeup_All( This.Sync.all );

         Suspend_Until_True( Data.All_Finalized );

         Free( This.Data );
      end if;
   end Finalize;

   ----------------------
   -- Worker_Task_Type --
   ----------------------
   task body Worker_Task_Type is
      Data : Object_Data_Access renames This.Data;
      Sync : Abstract_Sync.Object_Type'Class renames Data.Self.Sync.all;
      Result : Natural := 0;
   begin
      Log.Comment("Worker started" & Natural'Image(Id));

      Register( Sync );

      loop
         declare
            RC : Boolean := False;
         begin
            -- ......................................................
            Start_Lap( T_Wait_For_Ready );
            Wait_For_Input( Sync );
            if Data.Shutdown then
               Cancel_Lap( T_Wait_For_Ready );
               exit;
            else
               Stop_Lap(T_Wait_For_Ready);
            end if;
            -- ......................................................


            pragma Debug( Log.Comment("Vector_Processor.Worker_Task: Starting processing " & Natural'Image(id) ) );

            Start_Lap( T_Processing );
            -- process vectors as long as they are available
            RC := Process_Element( Data.Self.all, Id );
            Stop_Lap( T_Processing );

         exception
            when E : others =>
               Log.Error("Exception *** " & Exception_Name( E ) & ":" &
                      Exception_Message( E ) & " for worker " & Natural'Image(Id));
               exit;
         end;
      end loop;

      Unregister(Sync);

      Increment( Data.Workers_Completed, Result );
      if Result = Nbr_Of_Workers then
         pragma Debug( Log.Comment("Last worker " & Natural'Image(Id)));
         Set_True( Data.All_Finalized );
      end if;

      Log.Comment("Worker terminated" & Natural'Image(Id));
   end Worker_Task_Type;

   -------------
   -- Execute --
   -------------
   procedure Execute( This : in Object_Type'Class; times : in Natural ) is
      Sync : Abstract_Sync.Object_Type'Class renames This.Sync.all;
   begin
      Start_Lap( T_Iteration );

      for i in 1 .. times loop
         -- start all workers on the current vector
         pragma Debug( Log.Comment("Vector_Processor.Execute: Iteration ----------------" & Natural'Image(I)) );

         Start_Lap( T_Wait_For_Workers );
         Indicate_Input_Available( Sync );
         Stop_Lap( T_Wait_For_Workers );

         Collect_Result( This );
      end loop;

      Stop_Lap( T_Iteration );

      pragma Debug(Log.Comment("Vector_Processor.Execute **done**"));
   end ;

end Vector_Processor;
