--  ************************************************************************ --
--  *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
--  *****               FOR  CLASSICAL PARTICLES                       ***** --
--  ************************************************************************ --
--  $Id: iterate.adb 24 2010-10-14 16:24:58Z  $
--
--  This File is Part of PSim.THis file provides the configuration data
--  for a given calculation run.
--
--  Copyright (C) 2010 Michael Erdmann
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
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Numerics;                  use Ada.Numerics;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;

with Parallel;                   use Parallel;
with Work_Seeking_Integer_Iterate;
with Parameters;

with Vector_Space;                  use Vector_Space;
with Time_Measurement;              use Time_Measurement;
with Timers;                        use Timers;
with Log;
with Config;                        use Config;

package body Iterate is



   package Numerics is new Ada.Numerics.Generic_Elementary_Functions( Value_Type );
   use Numerics;

   X1 : State_Vector_Access := null;
   X2 : State_Vector_Access := null;

   -------------
   -- Iterate --
   -------------
   procedure Iterate (Start, Finish : Integer) is
      -----------
      -- Force --
      -----------
      function Force( I,J : in Integer ) return Vector_Type is
         DX : constant Vector_Type := X1(J).X - X1(I).X;
         R  : constant Value_Type := Norm(DX);
         G0 : constant Value_Type := 6.67428E-11;
      begin
         return ((G0 * X1(I).Mass * X1(J).Mass/(R+RS)**2)/R) * DX;

      exception
         when E : others =>
            Put_Line("Exception *** " & Exception_Name( E ) & ":" &
                     Exception_Message( E ) &
                     To_String( X1(I).X ) & To_String( X1(J).X ) &
                     Integer'Image(I) & Integer'Image(J));
            raise;
      end Force;

      DV : Vector_Type;
      DX : Vector_Type;
      K  : Vector_Type := Null_Vector;

   begin
      Start_Lap( T_Processing );

      for I in Start .. Finish loop
         begin
            Start_Lap( T_Vector );

            for J in 1..N loop
               if  I /= J then
                  K := K + Force( I, J );
               end if;
            end loop;


            DV := ( DT / X1(I).Mass) * K;
            X2(I).V := X1(I).V + DV;

            DX := DT* X2(I).V + DT**2 / ( 2.0 * X1(I).Mass)* K;
            X2(I).X := X1(I).X + DX;
            X2(I).Mass := X1(I).Mass;

            -- X2(i).Error := Scalar_Product( X1(I).Mass * DV + K ,X1(I).V);

            Stop_Lap( T_Vector );

         exception
            when E : others =>
               Put_Line("Exception *** " & Exception_Name( E ) & ":" &
                        Exception_Message( E ) & " for " & Natural'Image(I));
         end;
      end loop;
      Stop_Lap( T_Processing );

   exception
      when E : others =>
         Put_Line("Exception *** " & Exception_Name( E ) & ":" &
                  Exception_Message( E ) );
   end Iterate;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( X0 : out State_Vector_Access ) is
   begin
      Log.Comment("Physical Processors=" &
                Parallel.CPU_Count'Image (Parallel.Available_CPUs));
      Log.Comment("DEFAULT_WORKER_COUNT=" &
                  Parallel.Positive_Worker_Count'Image (Parallel.Default_Worker_Count) );
      Log.Comment("Number of Particles=" & Natural'Image(N));
      X2 := new State_Vector_Type( 1..N );      -- result state
      X1 := new State_Vector_Type( 1..N );

      X0 := X1;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize is
   begin
      Log.Comment("Iterate.Finalize");
   end Finalize;

   ---------------
   -- Calculate --
   ---------------
   function Calculate( Steps : in Natural ) return State_Vector_Access is
   begin
      Log.Comment("Calculate steps=" & Natural'Image(Steps));

      Start_Lap( T_Iteration );

      for I in 1..Steps loop
         T := T + DT;

         Work_Seeking_Integer_Iterate
              (From          => 1,
               To            => N,
               Worker_Count  => Worker_Count_Type(Config.Nbr_Of_Workers),
               Process       => Iterate'Access,
               Minimum_Seek  => Parameters.Minimum_Seek,
               Work_Budget   => Parameters.Work_Budget_For_Uneven_Loads,
               Use_Affinity  => Parameters.Use_Affinity);

         X1.all := X2.all ;
      end loop;

      Stop_Lap(T_Iteration);

      return X1;
   end Calculate ;

end Iterate;
