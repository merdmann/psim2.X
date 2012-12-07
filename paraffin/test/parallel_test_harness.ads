------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--               P A R A L L E L _ T E S T _ H A R N E S S                  --
--                                                                          --
--                                S p e c                                   --
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

with Parallel.Recursion;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Storage_Elements;

package Parallel_Test_Harness is
   procedure Setup_Tests;

   function Work_Budget_For_Even_Loads return Integer;
   function Work_Budget_For_Uneven_Loads return Integer;

   function Use_Affinity return Boolean;
   function Only_Run_Fibonacci_Tests return Boolean;
   function Run_Non_Recursive_Parallel_Tree return Boolean;
   function Run_Unbalanced_Iteration_Tests return Boolean;
   function Run_Double_Fibonacci_Tests return Boolean;

   function Fibonacci_Offer_Limit return Integer;
   function Fibonacci_Offer_Depth return Natural;

   function Parallel_Tree_Parallelism_Limit return Natural;

   function Parallelism_Enabled (Loop_Name : String) return Boolean;

   function Maximum_Recursive_Stack_Limit return Natural;

   function Maximum_Recursive_Stack_Depth
     return Parallel.Recursion.Stack_Percentage_Type;

   function Worker_Storage_Size
     return System.Storage_Elements.Storage_Count;

   function Minimum_Steal return Natural;

   function Minimum_Seek return Natural;

   function Debug_Logging return Boolean;

   --  Set to True to disable work seeking where the load is even
   --  and less likely to benefit from work seeking.
   --  This must be defined statically, as it causes code to be
   --  eliminated by the compiler.
   Eliminate_Work_Seeking_For_Even_Loads : constant Boolean := False;
   pragma Compile_Time_Warning
     (Eliminate_Work_Seeking_For_Even_Loads,
      "Work seeking is disabled for even loads");

   type Tests is
     (Sequential_Integer_Sum,
      Manual_Parallel_Integer_Sum,
      Parallel_Integer_Sum,
      Work_Seeking_Parallel_Integer_Sum,
      Elementary_Monoid_Sum,
      Work_Seeking_Elementary_Monoid_Sum,
      Sequential_Composite_Monoid_Sum,
      Composite_Monoid_Sum,
      Work_Seeking_Composite_Monoid_Sum,
      Sequential_Elementary_Float_Sum,
      Parallel_Elementary_Float_Sum,
      Work_Seeking_Parallel_ELementary_Float_Sum);

   type Result_Time_Array is
     array (Positive range <>,
            Parallel.Positive_Worker_Count range <>) of Duration;

   type Test_Name_Array is array (Positive range <>) of Unbounded_String;

   type Result_Times
     (Number_Of_Tests : Positive;
      Number_Of_Workers : Parallel.Positive_Worker_Count) is
      record
         Test : Test_Name_Array (1 .. Number_Of_Tests);
         Times : Result_Time_Array (1 .. Number_Of_Tests,
                                    1 .. Number_Of_Workers);
      end record;

   --  Generate Compile Time Warnings in places where changes should be
   --  made for Ada 2012
   Ada_2012_Warnings : constant Boolean := False;
end Parallel_Test_Harness;
