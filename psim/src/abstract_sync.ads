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
with Ada.Finalization;                         use Ada.Finalization;

package Abstract_sync is

   type Object_Type( Clients : Natural ) is abstract new Controlled with private;
   type Handle is access all Object_Type'Class;

   Null_Handle : constant Handle := null;

   -- The client id is assigned to any task which calls the register method
   type Client_Id_Type is new Natural range 0..20;
   Dispatcher_Id : constant Client_Id_Type := 0;

   function To_String( This : in Object_Type ) return String;

   -- the dispatcher indicates that data is available. It returns after the
   -- all workers are done.
   procedure Indicate_Input_Available( This : in out Object_Type'Class );

   -- the worker waits for input
   procedure Wait_For_Input( This : in out Object_Type'Class );

   -- register the current task as a client. It will assign a client id and bind
   -- the thread to a cpu
   procedure Register( This : in out Object_Type'Class );
   procedure Unregister( This : in out Object_Type'Class );

   -- Wakeup all clients
   procedure Wakeup_All( This : in out Object_Type'Class );

   -- Wait for all clients to finish
   procedure Wait_For_All( This : in out Object_Type'Class );

   -- infrastructure to be provided by implementations

   -- The process sleeps and returns from this method only if it is woken up be wakeup
   procedure Sleep( This : in out Object_Type; Id : Client_Id_Type ) is abstract;

   -- Wakeup a process which has been put to sleep by the Sleep methods above
   procedure Wakeup( This : in out Object_Type; Id : Client_Id_Type ) is abstract;

private

   type Object_Data_Type;
   type Object_Data_Access is access all Object_Data_Type;

   type Object_Type( Clients : Natural ) is abstract new Controlled with record
         Data : Object_Data_Access;
   end record;

   procedure Initialize( This : in out Object_Type );
   procedure Finalize( This : in out Object_Type );

end Abstract_Sync;
