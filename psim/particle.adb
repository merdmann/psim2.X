--  ************************************************************************ --
--  *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
--  *****               FOR  CLASSICAL PARTICLES                       ***** --
--  ************************************************************************ --
--  $Id: particle.adb 24 2010-10-14 16:24:58Z  $
--
--  Copyright (C) 2010 Michael Erdmann
--
--  PSim is free software;  you can redistribute it  and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Soft-
--  ware  Foundation;  either version 2,  or (at your option) any later
--  version.  Psim is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should have
--  received  a copy of the GNU General Public License distributed with GNAT;
--  see file COPYING.  If not, write to  the Free Software Foundation,
--  59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.                                                      --
--
--  As a special exception,  if other files  instantiate  generics from this
--  unit, or you link  this unit with other files  to produce an executable,
--  this  unit  does not  by itself cause  the resulting  executable  to  be
--  covered  by the  GNU  General  Public  License.  This exception does not
--  however invalidate  any other reasons why  the executable file  might be
--  covered by the  GNU Public License.
--
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Numerics;            use Ada.Numerics;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Numerics.Generic_Elementary_Functions;

with Vector_Space;            use Vector_Space;
with Types;                   use Types;
with Config;                  use Config;
with Log;                     use Log;
with Time_Measurement;        use Time_Measurement;
with Timers;                  use Timers;
with Scheduling;              use Scheduling;

with Abstract_Sync;           use Abstract_Sync;
with My_Sync;                 use My_Sync;
with My_Vector;               use My_Vector;

procedure Particle is
   package Numerics is new Generic_Elementary_Functions( Value_Type );
   use Numerics;

   -- State vectors which are to be used by the euler iteration
   Initial_State   : State_Vector_Access := null;
   State           : State_Vector_Access := null;
   Step            : Natural := 1;

   ----------------
   -- Dump_State --
   ----------------
   procedure Dump_State( T : in Value_Type; X : in State_Vector_Access ) is
   begin
      Start_Lap( T_Output );

      for I in 1..N loop
         Result(
                  "P" & Natural'Image(I)   & "; " &
                  Value_Type'Image(T)           & "; " &
                  Value_Type'Image(X(I).Mass)   & "; " &
                  Value_Type'Image(X(I).X(1))   & "; " &
                  Value_Type'Image(X(I).X(2))   & "; " &
                  Value_Type'Image(X(I).X(3))   & "; " &
                  Value_Type'Image(X(I).V(1))   & "; " &
                  Value_Type'Image(X(I).V(2))   & "; " &
                  Value_Type'Image(X(I).V(3))   & "; " &
                  Value_Type'Image(Norm(X(I).X)) & "; " &
                  Value_Type'Image(Norm(X(I).V)) & "; " &
                  Value_Type'Image(X(I).Error) );
      end loop;

      Stop_Lap( T_Output );
   end Dump_State;

   ----------------
   -- Save_State --
   ----------------
   procedure Save_State(
      Name      : in String;
      X         : in State_Vector_Access;
      T         : in Value_Type ) is
      File_Name : constant String := Name & ".init";
      Output    : File_Type;
   begin
      Create( File => Output, Name => File_Name, Mode => Out_File );

      for I in 1..N loop
         Put_Line( Output,
                  Value_Type'Image(T)      & "; " &
                  Value_Type'Image(X(I).Mass)   & "; " &
                  Value_Type'Image(X(I).X(1))   & "; " &
                  Value_Type'Image(X(I).X(2))   & "; " &
                  Value_Type'Image(X(I).X(3))   & "; " &
                  Value_Type'Image(X(I).V(1))   & "; " &
                  Value_Type'Image(X(I).V(2))   & "; " &
                  Value_Type'Image(X(I).V(3))   & "; "
                 );
      end loop;
      Close( Output );
      Comment("State Saved to " & File_Name );
   end Save_State;

   -------------
   -- Compute --
   -------------
   procedure Compute( S : in Abstract_Sync.Handle ) is
      P : My_Vector.Object_Type(S);
   begin
      Wait_For_All( s.all );

      Initial_State := Get_State_Vector(P);
      Config.Setup_Initial_Values( Argument(3), Initial_State );
      Dump_State( T, Initial_State );

      Log.Comment("Max_Iterations=" & Natural'Image(Max_Iterations));

      while Step < Max_Iterations loop
         State := Calculate( P, Report_Interval );
         Dump_State(T, State );

         T := T + Value_Type(Report_Interval) * DT;
         Step := Step + Report_Interval;
      end loop;
   end;

   RC : Integer := 0;

begin
   if Argument_Count < 2 then
      Put_Line("usage: particle config logfile state");
      return;
   end if;

   Log.Open( Argument(2) );

   RC := Set_Scheduling( $SCHED_POLICY );
   if RC /= 0 then
      Log.Comment("Scheduling policy not set");
   end if;

   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   -- ++++++++++++++++++     load the configuration       +++++++++++++++++ --
   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   Config.Initialize( Argument(1) );
   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   -- ++++++++++++++++++       Run Calculation            +++++++++++++++++ --
   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   Start_Lap( T_Total );
   declare
      S : constant Abstract_Sync.Handle := new My_Sync.Object_Type(Nbr_Of_Workers);
   begin
      Compute(S);

      Finalize(S.all);
   end ;
   Stop_Lap( T_Total );

   pragma Debug( Log.Comment("Particle.Main:Done"));
   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   -- ++++++++++++++++++           Terminate              +++++++++++++++++ --
   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   if Argument_Count > 2 then
      Comment( "Saving State for " & Value_Type'Image(T));
      Save_State( Argument(3), State, T );
   end if;

   -- dump the time measurements
   Update_Summary;
   Log.Comment( To_String( T_Total ));
   Log.Comment( To_String( T_Iteration ));
   Log.Comment( To_String( T_Wait_For_Workers ));
   Log.Comment( To_String( T_Wait_For_Ready ));
   Log.Comment( To_String( T_Processing ));
   Log.Comment( To_String( T_Vector ));
   Log.Comment( To_String( T_Forces ));
   Log.Comment( To_String( T_Integration ));

   Log.Comment( To_String( T_Output ));
   Log.Comment( To_String( T_Sleep ));

   Log.Close;

   Log.Comment("Particle.Main: done " & Natural'Image(Error_Counter));
exception
   when E : others =>
      Error("Exception *** " & Exception_Name( E ) & ":" & Exception_Message( E ));
      Log.Close;
end Particle;
