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
--  Functional Description
--  ======================
--
--  A vector processor performs on the vector elements identical tasks
--  in parallel. Each vector comonent is represented by a natural number.
--  The processor will call a procedure Process_Element for each of these
--  elements. This abstract method is expected to be provided by the
--  implementation.
--
with Ada.Finalization;                          use Ada.Finalization;
with Abstract_Sync;                             use Abstract_Sync;

package Vector_Processor is

   type Object_Type( Sync : Abstract_Sync.Handle ) is abstract new Controlled with private;
   type Handle is access all Object_Type'Class;

   -- process a vector element. returns false if there are no more elements
   function Process_Element( This : in Object_Type; Worker : in Integer := 0 ) return Boolean is abstract;

   -- collect computation results. Called after the complete vector has been processed.
   procedure Collect_Result( This : in Object_Type) is abstract;

   -- iterate n times over the complete vector
   procedure Execute( This : in  Object_Type'Class; times : in Natural );

private
   type Object_Data_Type( Nbr_Of_Workers : Natural );
   type Object_Data_Access is access all Object_Data_Type ;

   type Object_Type( Sync : Abstract_Sync.Handle ) is abstract new Controlled with record
         Data : Object_Data_Access := null;
      end record;

   procedure Initialize( This : in out Object_Type );
   procedure Finalize( This : in out Object_Type );

end Vector_Processor;
