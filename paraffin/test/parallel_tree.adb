------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--                       P A R A L L E L _ T R E E                          --
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

with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Command_Line;               use Ada;
with Ada.Real_Time;
with Ada.Calendar.Formatting;
with Interfaces;

with Parallel.Recursion; use Parallel;
with Parallel_Test_Harness; use Parallel_Test_Harness;

with Sequential_Red_Black_Trees;
with Work_Sharing_Red_Black_Trees;
with Work_Seeking_Red_Black_Trees;
with Stack_Safe_Work_Seeking_Red_Black_Trees;
with Integer_Doubly_Linked_Lists;

procedure Parallel_Tree is

      List       : Integer_Doubly_Linked_Lists.List;
      Start_Time : Real_Time.Time := Real_Time.Clock;
      Sum        : Interfaces.Unsigned_32        := 0;
      use type Interfaces.Unsigned_32;

      package Sequential_Integer_Red_Black_Tree is
        new Sequential_Red_Black_Trees
          (Element_Type => Interfaces.Unsigned_32);

      package Work_Sharing_Integer_Red_Black_Tree is
        new Work_Sharing_Red_Black_Trees
          (Element_Type => Interfaces.Unsigned_32);

      package Work_Seeking_Integer_Red_Black_Tree is
        new Work_Seeking_Red_Black_Trees
          (Element_Type => Interfaces.Unsigned_32);

      package Stack_Safe_Work_Seeking_Integer_Red_Black_Tree is
        new Stack_Safe_Work_Seeking_Red_Black_Trees
          (Element_Type => Interfaces.Unsigned_32);

      Sequential_Number_Tree : Sequential_Integer_Red_Black_Tree.Tree;
      Work_Sharing_Number_Tree : Work_Sharing_Integer_Red_Black_Tree.Tree;
      Work_Seeking_Number_Tree : Work_Seeking_Integer_Red_Black_Tree.Tree;
      Stack_Safe_Work_Seeking_Number_Tree :
         Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.Tree;

      function Work_Seeking_Elementary_Integer_Iterate_And_Reduce is new
        Work_Seeking_Integer_Red_Black_Tree.
          Elementary_Iterate_And_Reduce
            (Result_Type => Interfaces.Unsigned_32,
             Reducer => Interfaces."+",
             Identity_Value => 0);

      function Stack_Safe_Work_Seeking_Elementary_Integer_Iterate_And_Reduce
       is new
        Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.
          Elementary_Iterate_And_Reduce
            (Result_Type => Interfaces.Unsigned_32,
             Reducer => Interfaces."+",
             Identity_Value => 0);

      function Work_Sharing_Elementary_Integer_Iterate_And_Reduce is new
        Work_Sharing_Integer_Red_Black_Tree.
          Elementary_Iterate_And_Reduce
            (Result_Type => Interfaces.Unsigned_32,
             Reducer => Interfaces."+",
             Identity_Value => 0);

      function Sequential_Elementary_Integer_Iterate_And_Reduce is new
        Sequential_Integer_Red_Black_Tree.
          Elementary_Iterate_And_Reduce
            (Result_Type => Interfaces.Unsigned_32,
             Reducer => Interfaces."+",
             Identity_Value => 0);

      procedure Add (Total, Addend : in out Interfaces.Unsigned_32) is
      begin
         Total := Total + Addend;
      end Add;

      procedure Sequential_Composite_Integer_Iterate_And_Reduce is new
        Sequential_Integer_Red_Black_Tree.
          Composite_Iterate_And_Reduce
            (Result_Type => Interfaces.Unsigned_32,
             Reducer => Add,
             Identity_Value => 0);

   procedure Work_Sharing_Composite_Integer_Iterate_And_Reduce is new
        Work_Sharing_Integer_Red_Black_Tree.
          Composite_Iterate_And_Reduce
            (Result_Type => Interfaces.Unsigned_32,
             Reducer => Add,
             Identity_Value => 0);

      procedure Work_Seeking_Composite_Integer_Iterate_And_Reduce is new
        Work_Seeking_Integer_Red_Black_Tree.
          Composite_Iterate_And_Reduce
            (Result_Type => Interfaces.Unsigned_32,
             Reducer => Add,
             Identity_Value => 0);

   procedure Stack_Safe_Work_Seeking_Composite_Integer_Iterate_And_Reduce
      is new
        Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.
          Composite_Iterate_And_Reduce
            (Result_Type => Interfaces.Unsigned_32,
             Reducer => Add,
             Identity_Value => 0);

   procedure Work_Sharing_Recursive_Integer_Linked_List_Appending_Reducer is
     new Work_Sharing_Integer_Red_Black_Tree.
       Composite_Iterate_And_Reduce
         (Result_Type => Integer_Doubly_Linked_Lists.List,
          Reducer => Integer_Doubly_Linked_Lists.Reduce,
          Identity_Value => Integer_Doubly_Linked_Lists.Empty_List);

   procedure Work_Seeking_Recursive_Integer_Linked_List_Appending_Reducer is
     new Work_Seeking_Integer_Red_Black_Tree.
       Composite_Iterate_And_Reduce
         (Result_Type => Integer_Doubly_Linked_Lists.List,
            Reducer => Integer_Doubly_Linked_Lists.Reduce,
            Identity_Value => Integer_Doubly_Linked_Lists.Empty_List);

   ----------------------------------------------------------

      procedure Sequential_Print_Tree
        (Position : Sequential_Integer_Red_Black_Tree.Cursor) is
      begin
         Put (Interfaces.Unsigned_32'Image
              (Sequential_Integer_Red_Black_Tree.Element (Position)));
      end Sequential_Print_Tree;

      procedure Sequential_Recursive_Iteration
        (Position : Sequential_Integer_Red_Black_Tree.Cursor) is
         Temp : Integer := 10_000;
      begin
         pragma Debug
           (Debug_Logging,
            Put (Interfaces.Unsigned_32'Image
              (Sequential_Integer_Red_Black_Tree.Element (Position))));
--               delay 1.0;

         while Temp /= 0 loop
            Temp := Temp - 1;
         end loop;

      end Sequential_Recursive_Iteration;

      ----------------------------------------------------

      procedure Stack_Safe_Recursive_Iteration
        (Position : Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.Cursor) is
         Temp : Integer := 10_000;
      begin
         pragma Debug
           (Debug_Logging,
            Put (Interfaces.Unsigned_32'Image
              (Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.Element
                 (Position))));
--               delay 1.0;

         while Temp /= 0 loop
            Temp := Temp - 1;
         end loop;

      end Stack_Safe_Recursive_Iteration;

      ----------------------------------------------

      procedure Work_Seeking_Recursive_Iteration
        (Position : Work_Seeking_Integer_Red_Black_Tree.Cursor) is
         Temp : Integer := 10_000;
      begin
         pragma Debug
           (Debug_Logging,
            Put (Interfaces.Unsigned_32'Image
              (Work_Seeking_Integer_Red_Black_Tree.Element (Position))));
--               delay 1.0;

         while Temp /= 0 loop
            Temp := Temp - 1;
         end loop;

      end Work_Seeking_Recursive_Iteration;

      ---------------------------------------------

      procedure Work_Sharing_Recursive_Iteration
        (Position : Work_Sharing_Integer_Red_Black_Tree.Cursor) is
         Temp : Integer := 10_000;
      begin
         pragma Debug
           (Debug_Logging,
            Put (Interfaces.Unsigned_32'Image
              (Work_Sharing_Integer_Red_Black_Tree.Element (Position))));
--               delay 1.0;

         while Temp /= 0 loop
            Temp := Temp - 1;
         end loop;

      end Work_Sharing_Recursive_Iteration;

      ---------------------------------------------------

      Print_List_Contents : constant Boolean := False;

      Max_Iterations : constant Interfaces.Unsigned_32 :=
        Interfaces.Unsigned_32'Value (Ada.Command_Line.Argument (1));

      Deferral_Count : Recursion.Stack_Limit_Count;

begin
      for I in 1 .. 20 loop
            Sequential_Number_Tree.Insert (Interfaces.Unsigned_32 (I));
      end loop;

      Put_Line ("Test Tree should contain 1 .. 20, Tree Valid is" &
                  Boolean'Image (Sequential_Number_Tree.Is_Valid));
      Sequential_Number_Tree.Iterate
        (Process => Sequential_Print_Tree'Access);

      New_Line;
      Put_Line ("Delete 5");
      Sequential_Number_Tree.Delete (5);
      Put_Line ("Tree Valid is" &
                  Boolean'Image (Sequential_Number_Tree.Is_Valid));
      Sequential_Number_Tree.Iterate
        (Process => Sequential_Print_Tree'Access);

      New_Line;
      Put_Line ("Delete 15");
      Sequential_Number_Tree.Delete (15);
      Put_Line ("Deleted");
      Put_Line ("Tree Valid is" &
                  Boolean'Image (Sequential_Number_Tree.Is_Valid));
      Sequential_Number_Tree.Iterate
        (Process => Sequential_Print_Tree'Access);

      Sequential_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Sequential_Number_Tree.Insert (I);
      end loop;

      Put_Line ("Waiting 3 seconds");
      delay 3.0;

      New_Line;
      Put_Line ("(- Recursive Iteration of Red-Black Tree -)");
      New_Line;

      Put ("** Recursive Sequential Iteration through Binary Tree");

         --  Reset the clock
      Start_Time := Real_Time.Clock;

      Sequential_Number_Tree.Iterate
        (Process => Sequential_Recursive_Iteration'Access);

      Put_Line
        (", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Put ("** Recursive Parallel Work Sharing Iteration through Binary Tree");

      Sequential_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Work_Sharing_Number_Tree.Insert (I);
      end loop;

      --  Reset the clock
      Start_Time := Real_Time.Clock;

      Work_Sharing_Number_Tree.Iterate
        (Process => Work_Sharing_Recursive_Iteration'Access);

      Put_Line
     (", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Work_Sharing_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Work_Seeking_Number_Tree.Insert (I);
      end loop;

      Put ("** Recursive Parallel Work Seeking Iteration " &
           "through Binary Tree");

         --  Reset the clock
      Start_Time := Real_Time.Clock;

      Work_Seeking_Number_Tree.Iterate
        (Process => Work_Seeking_Recursive_Iteration'Access);

      Put_Line
        (", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Work_Seeking_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Stack_Safe_Work_Seeking_Number_Tree.Insert (I);
      end loop;

      Put ("** Stack Safe Recursive Parallel Work Seeking Iteration " &
           "through Binary Tree");

      --  Reset the clock
      Start_Time := Real_Time.Clock;

      Stack_Safe_Work_Seeking_Number_Tree.Iterate
        (Process => Stack_Safe_Recursive_Iteration'Access,
         Storage_Size => Parallel_Test_Harness.Worker_Storage_Size,
         Max_Depth => Parallel_Test_Harness.Maximum_Recursive_Stack_Depth,
         Deferral_Count => Deferral_Count);

      Put_Line
        (", Stack Limit Reached" &
         Recursion.Stack_Limit_Count'Image (Deferral_Count) &
         " times, Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Stack_Safe_Work_Seeking_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Sequential_Number_Tree.Insert (I);
      end loop;

      New_Line;
      Put_Line ("(- Functional Integer Reduction of Red-Black Tree -)");
      New_Line;

      Put ("** Recursive Sequential Elementary Integer Sum " &
           "of binary tree nodes = ");

      Sum := 0;

      --  Reset the clock
      Start_Time := Real_Time.Clock;

      --  for I in Number_Tree with
      --         Balanced_Recursive_Integer_Iterate_And_Reduce <Sum> loop
      --    Sum := Sum + Number_Tree (I);
      --  end loop
      declare
         function Iteration
           (Position : Sequential_Integer_Red_Black_Tree.Cursor)
            return     Interfaces.Unsigned_32
         is
            Temp : Interfaces.Unsigned_32 := 10_000;
         begin
            while Temp /= 0 loop
               Temp := Temp - 1;
            end loop;
            pragma Debug
              (Debug_Logging,
               Put (Interfaces.Unsigned_32'Image
                 (Sequential_Integer_Red_Black_Tree.Element (Position))));
            return Sequential_Integer_Red_Black_Tree.Element (Position);
         end Iteration;
      begin
         Sum :=
            Sequential_Elementary_Integer_Iterate_And_Reduce
              (Container     => Sequential_Number_Tree,
               Process       => Iteration'Access,
               Initial_Value => Sum);
      end;

      Put_Line
        (Interfaces.Unsigned_32'Image (Sum) &
         ", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Sequential_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Work_Sharing_Number_Tree.Insert (I);
      end loop;

      Put ("** Recursive Work Sharing Elementary Integer Sum " &
           "of binary tree nodes = ");

      Sum := 0;
      --  Reset the clock
      Start_Time := Real_Time.Clock;

      --  for I in Number_Tree with
      --         Balanced_Recursive_Integer_Iterate_And_Reduce <Sum> loop
      --    Sum := Sum + Number_Tree (I);
      --  end loop
      declare
         function Iteration
           (Position : Work_Sharing_Integer_Red_Black_Tree.Cursor)
            return     Interfaces.Unsigned_32
         is
            Temp : Interfaces.Unsigned_32 := 10_000;
         begin
            while Temp /= 0 loop
               Temp := Temp - 1;
            end loop;
            pragma Debug
              (Debug_Logging,
               Put (Interfaces.Unsigned_32'Image
                 (Work_Sharing_Integer_Red_Black_Tree.Element (Position))));
            return Work_Sharing_Integer_Red_Black_Tree.Element (Position);
         end Iteration;
      begin
         Sum :=
            Work_Sharing_Elementary_Integer_Iterate_And_Reduce
              (Container     => Work_Sharing_Number_Tree,
               Process       => Iteration'Access,
               Initial_Value => Sum);
      end;

      Put_Line
        (Interfaces.Unsigned_32'Image (Sum) &
         ", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Work_Sharing_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Work_Seeking_Number_Tree.Insert (I);
      end loop;

      Put ("** Recursive Work Seeking Elementary Integer Sum " &
           "of binary tree nodes = ");

      Sum := 0;

      --  Reset the clock
      Start_Time := Real_Time.Clock;

      --  for I in Number_Tree with
      --         Recursive_Integer_Iterate_And_Reduce <Sum> loop
      --    Sum := Sum + Number_Tree (I);
      --  end loop
      declare
         function Iteration
           (Position : Work_Seeking_Integer_Red_Black_Tree.Cursor)
            return     Interfaces.Unsigned_32
         is
            Temp : Interfaces.Unsigned_32 := 10_000;
         begin
            while Temp /= 0 loop
               Temp := Temp - 1;
            end loop;

            pragma Debug
              (Debug_Logging,
               Put (Interfaces.Unsigned_32'Image
                 (Work_Seeking_Integer_Red_Black_Tree.Element (Position))));

            return Work_Seeking_Integer_Red_Black_Tree.Element (Position);
         end Iteration;
      begin
         Sum :=
            Work_Seeking_Elementary_Integer_Iterate_And_Reduce
              (Container     => Work_Seeking_Number_Tree,
               Process       => Iteration'Access,
               Initial_Value => Sum);
      end;

      Put_Line
        (Interfaces.Unsigned_32'Image (Sum) &
         ", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Work_Seeking_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Stack_Safe_Work_Seeking_Number_Tree.Insert (I);
      end loop;

      Put ("** Recursive Stack Safe Work Seeking Elementary Integer Sum " &
           "of binary tree nodes = ");

      Sum := 0;

      --  Reset the clock
      Start_Time := Real_Time.Clock;

      --  for I in Number_Tree with
      --         Recursive_Integer_Iterate_And_Reduce <Sum> loop
      --    Sum := Sum + Number_Tree (I);
      --  end loop
      declare

         Deferral_Count : aliased Recursion.Stack_Limit_Count := 0;

         function Iteration
           (Position : Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.Cursor)
            return     Interfaces.Unsigned_32
         is
            Temp : Interfaces.Unsigned_32 := 10_000;
         begin
            while Temp /= 0 loop
               Temp := Temp - 1;
            end loop;

            pragma Debug
              (Debug_Logging,
               Put (Interfaces.Unsigned_32'Image
                 (Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.Element
                    (Position))));

            return Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.Element
              (Position);
         end Iteration;
      begin
         Sum :=
            Stack_Safe_Work_Seeking_Elementary_Integer_Iterate_And_Reduce
              (Container     => Stack_Safe_Work_Seeking_Number_Tree,
               Process       => Iteration'Access,
               Storage_Size  => Parallel_Test_Harness.Worker_Storage_Size,
               Initial_Value => Sum,
               Max_Depth     =>
                 Parallel_Test_Harness.Maximum_Recursive_Stack_Depth,
               Deferral_Count => Deferral_Count'Access);
      end;

      Put_Line
        (Interfaces.Unsigned_32'Image (Sum) &
         ", Stack Limit Reached" &
         Recursion.Stack_Limit_Count'Image (Deferral_Count) &
         " times, Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      New_Line;
      Put_Line ("(- Procedural Integer Reduction of Red-Black Tree -)");
      New_Line;

     Stack_Safe_Work_Seeking_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Sequential_Number_Tree.Insert (I);
      end loop;

      Put ("** Recursive Sequential Composite Integer Sum " &
           "of binary tree nodes = ");

      Sum := 0;

      --  Reset the clock
      Start_Time := Real_Time.Clock;

      --  for I in Number_Tree with
      --         Balanced_Composite_Integer_Iterate_And_Reduce <Sum> loop
      --    Sum := Sum + Number_Tree (I);
      --  end loop
      declare
         procedure Iteration
           (Position : Sequential_Integer_Red_Black_Tree.Cursor;
            Result : out Interfaces.Unsigned_32)
         is
            Temp : Integer := 10_000;
         begin
            while Temp /= 0 loop
               Temp := Temp - 1;
            end loop;
            pragma Debug
              (Debug_Logging,
               Put (Interfaces.Unsigned_32'Image
                 (Sequential_Integer_Red_Black_Tree.Element (Position))));
            Result := Sequential_Integer_Red_Black_Tree.Element (Position);
         end Iteration;
      begin
            Sequential_Composite_Integer_Iterate_And_Reduce
              (Container     => Sequential_Number_Tree,
               Process       => Iteration'Access,
               Result        => Sum);
      end;

      Put_Line
        (Interfaces.Unsigned_32'Image (Sum) &
         ", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Sequential_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Work_Sharing_Number_Tree.Insert (I);
      end loop;

      Put ("** Recursive Work Sharing Composite Integer Sum " &
           "of binary tree nodes = ");

      Sum := 0;

      --  Reset the clock
      Start_Time := Real_Time.Clock;

      --  for I in Number_Tree with
      --         Balanced_Composite_Integer_Iterate_And_Reduce <Sum> loop
      --    Sum := Sum + Number_Tree (I);
      --  end loop
      declare
         procedure Iteration
           (Position : Work_Sharing_Integer_Red_Black_Tree.Cursor;
            Result : out Interfaces.Unsigned_32)
         is
            Temp : Integer := 10_000;
         begin
            while Temp /= 0 loop
               Temp := Temp - 1;
            end loop;
            pragma Debug
              (Debug_Logging,
               Put (Interfaces.Unsigned_32'Image
                 (Work_Sharing_Integer_Red_Black_Tree.Element (Position))));
            Result := Work_Sharing_Integer_Red_Black_Tree.Element (Position);
         end Iteration;
      begin
            Work_Sharing_Composite_Integer_Iterate_And_Reduce
              (Container     => Work_Sharing_Number_Tree,
               Process       => Iteration'Access,
               Result        => Sum);
      end;

      Put_Line
        (Interfaces.Unsigned_32'Image (Sum) &
         ", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Work_Sharing_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Work_Seeking_Number_Tree.Insert (I);
      end loop;

      Put ("** Recursive Work Seeking Composite Integer Sum " &
           "of binary tree nodes = ");

      --  Reset the clock
      Start_Time := Real_Time.Clock;
      Sum        := 0;

      --  for I in Number_Tree with
      --         Recursive_Integer_Iterate_And_Reduce <Sum> loop
      --    Sum := Sum + Number_Tree (I);
      --  end loop
      declare
         procedure Iteration
           (Position : Work_Seeking_Integer_Red_Black_Tree.Cursor;
            Value    : out Interfaces.Unsigned_32)
         is
            Temp : Interfaces.Unsigned_32 := 10_000;
         begin
            while Temp > 0 loop
               Temp := Temp - 1;
            end loop;
            pragma Debug
              (Debug_Logging,
               Put (Interfaces.Unsigned_32'Image
                 (Work_Seeking_Integer_Red_Black_Tree.Element (Position))));
            Value := Work_Seeking_Integer_Red_Black_Tree.Element (Position);
         end Iteration;
      begin
         Work_Seeking_Composite_Integer_Iterate_And_Reduce
           (Container => Work_Seeking_Number_Tree,
            Process   => Iteration'Access,
            Result    => Sum);
      end;

      Put_Line
        (Interfaces.Unsigned_32'Image (Sum) &
         ", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      Work_Seeking_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Stack_Safe_Work_Seeking_Number_Tree.Insert (I);
      end loop;

      Put ("** Recursive Stack Safe Work Seeking Composite Integer Sum " &
           "of binary tree nodes = ");

         --  Reset the clock
      Start_Time := Real_Time.Clock;
      Sum        := 0;

      --  for I in Number_Tree with
      --         Recursive_Integer_Iterate_And_Reduce <Sum> loop
      --    Sum := Sum + Number_Tree (I);
      --  end loop
      declare
         Deferral_Count : Recursion.Stack_Limit_Count := 0;

         procedure Iteration
           (Position : Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.Cursor;
            Value    : out Interfaces.Unsigned_32)
         is
            Temp : Interfaces.Unsigned_32 := 10_000;
         begin
            while Temp > 0 loop
               Temp := Temp - 1;
            end loop;
            pragma Debug
              (Debug_Logging,
               Put (Interfaces.Unsigned_32'Image
                 (Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.Element
                    (Position))));
            Value := Stack_Safe_Work_Seeking_Integer_Red_Black_Tree.Element
              (Position);
         end Iteration;
      begin
         Stack_Safe_Work_Seeking_Composite_Integer_Iterate_And_Reduce
           (Container => Stack_Safe_Work_Seeking_Number_Tree,
            Process   => Iteration'Access,
            Max_Depth     =>
              Parallel_Test_Harness.Maximum_Recursive_Stack_Depth,
            Storage_Size => Parallel_Test_Harness.Worker_Storage_Size,
            Deferral_Count => Deferral_Count,
            Result    => Sum);

         Put_Line
           (Interfaces.Unsigned_32'Image (Sum) &
            ", Stack Limit Reached" &
            Recursion.Stack_Limit_Count'Image (Deferral_Count) &
            " times, Elapsed = " &
            Calendar.Formatting.Image
               (Elapsed_Time          =>
                   Real_Time.To_Duration
                     (Real_Time."-"
                         (Left  => Real_Time.Clock,
                          Right => Start_Time)),
                Include_Time_Fraction => True));
      end;

      New_Line;
      Put_Line ("(- Linked List Reductions of Red-Black Tree -)");
      New_Line;

      List.Clear;
      Stack_Safe_Work_Seeking_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Work_Sharing_Number_Tree.Insert (I);
      end loop;

      Put ("** Work Sharing Recursive Parallel Linked List append");

      --  Reset the clock
      Start_Time := Real_Time.Clock;

      --  for I in Recursive_Number_Tree with
      --      Recursive_Integer_Linked_List_Appending_Reducer <Number_List>
      --  loop
      --    Recursive_Number_List.Append (New_Item => I, Count => 1);
      --  end loop
      --
      declare
         procedure Iteration
           (Position    : Work_Sharing_Integer_Red_Black_Tree.Cursor;
            Number_List : out Integer_Doubly_Linked_Lists.List)
         is
         begin
            Number_List.Append
              (New_Item => Work_Sharing_Integer_Red_Black_Tree.Element
                 (Position),
               Count    => 1);
         end Iteration;
      begin
         Work_Sharing_Recursive_Integer_Linked_List_Appending_Reducer
           (Container => Work_Sharing_Number_Tree,
            Process   => Iteration'Access,
            Result    => List);
      end;

      Put_Line
        (", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      if Print_List_Contents then
         pragma Warnings (Off, "*code*has been deleted*");
         Put_Line ("Work Sharing Recursive Integer List contents");

         declare
            procedure Print_Elements
              (Position : Integer_Doubly_Linked_Lists.Integer_Lists.Cursor)
            is
            begin
               Put
                 (Interfaces.Unsigned_32'Image
                     (Integer_Doubly_Linked_Lists.Integer_Lists.Element
                         (Position => Position)));
            end Print_Elements;

         begin
            List.Iterate (Process => Print_Elements'Access);
         end;

         New_Line;
         pragma Warnings (On, "*code*has been deleted*");
         List.Clear;
      end if;

      Work_Sharing_Number_Tree.Clear;

      for I in 1 .. Max_Iterations loop
         Work_Seeking_Number_Tree.Insert (I);
      end loop;

      Put ("** Work Seeking Recursive Parallel Linked List append");

      --  Reset the clock
      Start_Time := Real_Time.Clock;

      --  for I in Recursive_Number_Tree with
      --      Recursive_Integer_Linked_List_Appending_Reducer <Number_List>
      --  loop
      --    Recursive_Number_List.Append (New_Item => I, Count => 1);
      --  end loop
      --
      declare
         procedure Iteration
           (Position    : Work_Seeking_Integer_Red_Black_Tree.Cursor;
            Number_List : out Integer_Doubly_Linked_Lists.List)
         is
         begin
            Number_List.Append
              (New_Item => Work_Seeking_Integer_Red_Black_Tree.Element
                 (Position),
               Count    => 1);
         end Iteration;
      begin
         Work_Seeking_Recursive_Integer_Linked_List_Appending_Reducer
           (Container => Work_Seeking_Number_Tree,
            Process   => Iteration'Access,
            Result    => List);
      end;

      Put_Line
        (", Elapsed = " &
         Calendar.Formatting.Image
            (Elapsed_Time          =>
                Real_Time.To_Duration
                  (Real_Time."-"
                      (Left  => Real_Time.Clock,
                       Right => Start_Time)),
             Include_Time_Fraction => True));

      if Print_List_Contents then
         pragma Warnings (Off, "*code*has been deleted*");
         Put_Line ("Work Seeking Recursive Integer List contents");

         declare
            procedure Print_Elements
              (Position : Integer_Doubly_Linked_Lists.Integer_Lists.Cursor)
            is
            begin
               Put
                 (Interfaces.Unsigned_32'Image
                     (Integer_Doubly_Linked_Lists.Integer_Lists.Element
                         (Position => Position)));
            end Print_Elements;

         begin
            List.Iterate (Process => Print_Elements'Access);
         end;

         New_Line;
         pragma Warnings (On, "*code*has been deleted*");
         List.Clear;
      end if;
end Parallel_Tree;
