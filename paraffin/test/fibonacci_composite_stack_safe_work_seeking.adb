------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                         F I B O N A C C I 1 0                            --
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

with Parallel.Recursion.Stack_Safe_Work_Seeking_Procedural_Reduction;
use Parallel;

with System;
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

procedure Fibonacci_Composite_Stack_Safe_Work_Seeking
  (Value : Natural;
   Stack_Deferrals : out Parallel.Recursion.Stack_Limit_Count;
   Result : out Natural)
is
   Other_Workers : aliased Parallel.Work_Seeking_State;

   procedure Add (Left, Right : in out Natural) is
   begin
      Left := Left + Right;
   end Add;

   package Fibonacci_Recursion is new
     Parallel.Recursion.Stack_Safe_Work_Seeking_Procedural_Reduction
       (Work_Type => Natural,
        Result_Type => Natural,
        Reducer => Add,
        Identity_Value => 0);

   Dispatcher : aliased Fibonacci_Recursion.Recursion_Dispatcher_Access
     := null;

   procedure Parallel_Fibonacci
     (Number : Natural;
      Stack_Limit : System.Address;
      Result : out Natural)
   is
      use type System.Address;
   begin

      if Number < 2 then
         Result := Number;
      elsif Other_Workers.Seeking_Work and then
        Number > Parallel_Test_Harness.Fibonacci_Offer_Limit then

         declare
            Temp : Natural;
         begin

            Dispatcher.Recurse
              (Item => Number - 1,
               Stack_Limit => Stack_Limit,
               Result => Result);

            Parallel_Fibonacci (Number - 2, Stack_Limit, Temp);

            Result := Result + Temp;
         end;

      elsif Number'Address <= Stack_Limit then

         declare
            Temp : Natural;
         begin

            Dispatcher.Recurse
              (Item => Number - 1,
               Stack_Limit => Stack_Limit,
               Result => Result);

            Dispatcher.Recurse
              (Item => Number - 2,
               Stack_Limit => Stack_Limit,
               Result => Temp);

            Result := Result + Temp;
         end;

      else
         declare
            Temp : Natural;
         begin

            Parallel_Fibonacci (Number - 2, Stack_Limit, Result);
            Parallel_Fibonacci (Number - 1, Stack_Limit, Temp);
            Result := Result + Temp;
         end;
      end if;

   end Parallel_Fibonacci;

begin
   Fibonacci_Recursion.Execute
     (Item  => Value,
      Other_Workers => Other_Workers'Access,
      Dispatcher => Dispatcher'Access,
      Process => Parallel_Fibonacci'Access,
      Max_Depth => Parallel_Test_Harness.Maximum_Recursive_Stack_Depth,
      Storage_Size => Parallel_Test_Harness.Worker_Storage_Size,
      --  Ceiling      => ,
      --  Worker_Count => ,
      Work_Budget  => Parallel_Test_Harness.Work_Budget_For_Even_Loads,
      Use_Affinity => Parallel_Test_Harness.Use_Affinity,
      Stack_Deferrals => Stack_Deferrals,
      Result => Result);
end Fibonacci_Composite_Stack_Safe_Work_Seeking;
