------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                         F I B O N A C C I 7                              --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore                    --
--                                                                          --
--  Paraffin is free software;  you can  redistribute it  and/or modify it  --
--  under  terms of the  GNU General Public License  as  published  by the  --
--  Free Software  Foundation;  either version 2,  or (at your option) any  --
--  later  version.  Paraffin is  distributed in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU  --
--  General Public License for  more details.  You should have  received a  --
--  copy of the GNU General Public License distributed with Paraffin;  see  --
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,   --
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.         --
--                                                                          --
--  As a  special exception, if other files  instantiate generics from      --
--  this unit,  or you link this  unit with other files  to produce an      --
--  executable,  this unit  does  not by  itself  cause the  resulting      --
--  executable to be covered by  the GNU General Public License.  This      --
--  exception does  not however invalidate  any other reasons  why the      --
--  executable file might be covered by the GNU Public License.             --
------------------------------------------------------------------------------

with Parallel.Recursion.Work_Sharing_Procedural_Reduction;
use Parallel;

with Parallel_Test_Harness;

--  function Fibonacci (Number : Natural) return Natural is
--  begin
--     if Number < 2 then
--        return Number;
--     else
--        return Fibonacci (Number - 2) + Fibonacci (Number - 1);
--     end if;
--  end Fibonacci;
--
--  pragma Parallel_Recursion
--    (Fibonacci, Two_Way_Recursive_Integer_Addition.Parallel_Recursion);

function Fibonacci_Composite_Work_Sharing (Value : Natural) return Natural
is

   procedure Add (Left, Right : in out Natural) is
   begin
      Left := Left + Right;
   end Add;

   package Fibonacci_Recursion is new
     Parallel.Recursion.Work_Sharing_Procedural_Reduction
       (Work_Type => Natural,
        Result_Type => Natural,
        Reducer => Add,
        Identity_Value => 0);

   Dispatcher : aliased Fibonacci_Recursion.Recursion_Dispatcher_Access
     := null;

   procedure Parallel_Fibonacci
     (Number : Natural;
      Subcontractors : Worker_Count_Type;
      Result : out Natural) is
   begin

      if Number < 2 then
         Result := Number;
      else
         declare
            Temp : Natural;
         begin

            Dispatcher.Recurse
              (Item => Number - 1,
               Subcontractors => Subcontractors,
               Split => 1,
               Of_Splits => 2,
               Result => Result);

            Dispatcher.Recurse
              (Item => Number - 2,
               Subcontractors => Subcontractors,
               Split => 2,
               Of_Splits => 2,
               Result => Temp);

            Result := Result + Temp;
         end;
      end if;

   end Parallel_Fibonacci;

   procedure Sequential_Fibonacci
     (Number : Natural;
      Result : out Natural) is
   begin

      if Number < 2 then
         Result := Number;
      else
         declare
            Temp : Natural;
         begin
            Sequential_Fibonacci (Number - 2, Result);
            Sequential_Fibonacci (Number - 1, Temp);
            Result := Result + Temp;
         end;
      end if;

   end Sequential_Fibonacci;

      Result : Natural;
begin
   Fibonacci_Recursion.Execute
     (Item  => Value,
      Dispatcher => Dispatcher'Access,
      Parallel_Process => Parallel_Fibonacci'Access,
      Process => Sequential_Fibonacci'Access,
      --  Ceiling      => ,
      --  Worker_Count => ,
      Use_Affinity => Parallel_Test_Harness.Use_Affinity,
      Result => Result);

      return Result;
end Fibonacci_Composite_Work_Sharing;
