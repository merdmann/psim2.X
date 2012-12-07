------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                      ( T E S T   D R I V E R S )                         --
--              C O N T A I N E R _ M A N I P U L A T I O N S               --
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

with Ada.Text_IO; use Ada.Text_IO;

with Integer_Doubly_Linked_Lists;
with Integer_Linked_List_Appending_Reducer;

with Work_Seeking_Integer_Linked_List_Appending_Reducer;
with Work_Stealing_Integer_Linked_List_Appending_Reducer;

with Integer_Vectors;
with Integer_Vector_Appending_Reducer;
with Integer_Addition_Reducer;
with Interfaces;
with Work_Seeking_Integer_Vector_Appending_Reducer;
with Work_Seeking_Integer_Addition_Reducer;
with Work_Stealing_Integer_Vector_Appending_Reducer;
with Work_Stealing_Integer_Addition_Reducer;
with Parallel;                                       use Parallel;
with Parallel_Test_Harness;                          use Parallel_Test_Harness;

procedure Container_Manipulations is
   Number_List : Integer_Doubly_Linked_Lists.List :=
     Integer_Doubly_Linked_Lists.Empty_List;
begin
   --  for I in 1 .. 100
   --         with Integer_Linked_List_Appending_Reducer <Number_List> loop
   --    Number_List.Append (New_Item => I, Count => 1);
   --  end loop
   --
   declare
      procedure Iteration
        (Start, Finish : Interfaces.Unsigned_32;
         Number_List   : in out Integer_Doubly_Linked_Lists.List)
      is
      begin
         for I in Start .. Finish loop
            Number_List.Append (New_Item => I, Count => 1);
         end loop;
      end Iteration;
   begin
      Integer_Linked_List_Appending_Reducer
        (From         => 1,
         To           => 100,
         Process      => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item         => Number_List,
         Use_Affinity => Parallel_Test_Harness.Use_Affinity);
   end;

   Put_Line ("** Parallel Composite Integer Linked List Creation");

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
      Number_List.Iterate (Process => Print_Elements'Access);
   end;

   Number_List := Integer_Doubly_Linked_Lists.Empty_List;
   --  for I in 1 .. 100
   --         with Integer_Linked_List_Appending_Reducer <Number_List> loop
   --    Number_List.Append (New_Item => I, Count => 1);
   --  end loop
   --
   declare

      procedure Iteration
        (Start, Finish  : Interfaces.Unsigned_32;
         Number_List : in out Integer_Doubly_Linked_Lists.List)
      is
      begin
         for I in Start .. Finish loop
            Number_List.Append (New_Item => I, Count => 1);
         end loop;
      end Iteration;
   begin
      Work_Seeking_Integer_Linked_List_Appending_Reducer
        (From          => 1,
         To            => 100,
         Process       => Iteration'Access,
         Item          => Number_List,
         Minimum_Seek  => Parallel_Test_Harness.Minimum_Seek,
         Worker_Count => Parallel.Default_Worker_Count,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Even_Loads,
         Use_Affinity  => Parallel_Test_Harness.Use_Affinity);
   end;

   New_Line;
   Put_Line
     ("** Work Seeking Parallel Composite" & " Integer Linked List Creation");
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
      Number_List.Iterate (Process => Print_Elements'Access);
   end;

   Number_List := Integer_Doubly_Linked_Lists.Empty_List;
   --  for I in 1 .. 100
   --         with Integer_Linked_List_Appending_Reducer <Number_List> loop
   --    Number_List.Append (New_Item => I, Count => 1);
   --  end loop
   --
   declare

      procedure Iteration
        (Start, Finish : Interfaces.Unsigned_32;
         Number_List : in out Integer_Doubly_Linked_Lists.List)
      is
      begin
         for I in Start .. Finish loop
            Number_List.Append
              (New_Item => I,
               Count => 1);
         end loop;
      end Iteration;
   begin
      Work_Stealing_Integer_Linked_List_Appending_Reducer
        (From          => 1,
         To            => 100,
         Process       => Iteration'Access,
         Worker_Count => Parallel.Default_Worker_Count,
         Item          => Number_List,
         Minimum_Steal => Parallel_Test_Harness.Minimum_Steal,
         Work_Budget   => Parallel_Test_Harness.Work_Budget_For_Even_Loads);
   end;

   New_Line;
   Put_Line
     ("** Work Stealing Parallel Composite" & " Integer Linked List Creation");
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
      Number_List.Iterate (Process => Print_Elements'Access);
   end;

   Test5 : declare
      Number_Array : Integer_Vectors.Vector := Integer_Vectors.Empty_Vector;
      Sum          : Interfaces.Unsigned_32                := 0;
   --      Start_Time : constant Real_Time.Time := Real_Time.Clock;
   begin
      --  for I in 1 .. 100
      --    with Integer_Vectors_Appending_Reducer <Number_Array> loop
      --    Number_Array.Append (New_Item => I, Count => 1);
      --  end loop
      --
      Parallel_For_Loop5 : declare
         procedure Iteration
           (Start, Finish : Natural;
            Number_Array  : in out Integer_Vectors.Vector)
         is
         begin
            for I in Start .. Finish loop
               Number_Array.Append
                 (New_Item => Interfaces.Unsigned_32 (I),
                  Count => 1);
            end loop;
         end Iteration;
      begin
         Integer_Vector_Appending_Reducer
           (From         => 1,
            To           => 100,
            Process      => Iteration'Access,
            Worker_Count => Parallel.Default_Worker_Count,
            Item         => Number_Array,
            Use_Affinity => Parallel_Test_Harness.Use_Affinity);
      end Parallel_For_Loop5;

      New_Line;
      Put_Line ("** Parallel Elementary Integer Vector Linked List Creation");
      declare
         procedure Print_Elements (Position : Integer_Vectors.Cursor) is
         begin
            Put
              (Interfaces.Unsigned_32'Image
                  (Integer_Vectors.Element (Position => Position)));
         end Print_Elements;

      begin
         Number_Array.Iterate (Process => Print_Elements'Access);
      end;

      --  BJM need to put proper syntax here....

      --  for Element in Number_Array with Integer_Addition_Reducer <Sum> loop
      --    Sum := Sum + Element (Position);
      --  end loop
      --
      Parallel_For_Loop5a : declare
         procedure Iteration
           (Start, Finish : Interfaces.Unsigned_32;
            Sum           : in out Interfaces.Unsigned_32)
         is
            Current_Position : Integer_Vectors.Cursor          :=
               Number_Array.To_Cursor (Index => Natural (Start));
            End_Position     : constant Integer_Vectors.Cursor :=
               Number_Array.To_Cursor (Index => Natural (Finish));
            use type Integer_Vectors.Cursor;
            use type Interfaces.Unsigned_32;
         begin

            loop
               Sum := Sum + Integer_Vectors.Element (Current_Position);
               exit when Current_Position = End_Position;
               Integer_Vectors.Next (Current_Position);
            end loop;
         end Iteration;
      begin
         Integer_Addition_Reducer
           (From         => Interfaces.Unsigned_32 (Number_Array.First_Index),
            To           => Interfaces.Unsigned_32 (Number_Array.Last_Index),
            Process      => Iteration'Access,
            Worker_Count => Parallel.Default_Worker_Count,
            Item         => Sum,
            Use_Affinity => Parallel_Test_Harness.Use_Affinity);
      end Parallel_For_Loop5a;

      New_Line;
      Put_Line
        ("** Parallel Elementary Integer Linked List Sum = " &
         Interfaces.Unsigned_32'Image (Sum));

   end Test5;

   Test5b : declare
      Number_Array : Integer_Vectors.Vector := Integer_Vectors.Empty_Vector;
      Sum          : Interfaces.Unsigned_32                := 0;
   --      Start_Time : constant Real_Time.Time := Real_Time.Clock;
   begin
      --  for I in 1 .. 100 with
      --    Work_Seeking_Integer_Vectors_Appending_Reducer <Number_Array> loop
      --    Number_Array.Append (New_Item => I, Count => 1);
      --  end loop
      --
      declare

         procedure Iteration
           (Start, Finish  : Natural;
            Number_Array : in out Integer_Vectors.Vector) is
         begin
            for I in Start .. Finish loop
               Number_Array.Append
                 (New_Item => Interfaces.Unsigned_32 (I),
                  Count => 1);
            end loop;
         end Iteration;
      begin
         Work_Seeking_Integer_Vector_Appending_Reducer
           (From          => 1,
            To            => 100,
            Process       => Iteration'Access,
            Item          => Number_Array,
            Worker_Count => Parallel.Default_Worker_Count,
            Work_Budget   =>
               Parallel_Test_Harness.Work_Budget_For_Even_Loads,
            Use_Affinity  => Parallel_Test_Harness.Use_Affinity);
      end;

      Put_Line
        ("** Work Seeking Parallel Elementary Integer" &
         " Vector Linked List Creation");
      declare
         procedure Print_Elements (Position : Integer_Vectors.Cursor) is
         begin
            Put
              (Interfaces.Unsigned_32'Image
                  (Integer_Vectors.Element (Position => Position)));
         end Print_Elements;

      begin
         Number_Array.Iterate (Process => Print_Elements'Access);
      end;

      --  BJM need to put proper syntax here....

      --  for Element in Number_Array with Integer_Addition_Reducer <Sum> loop
      --    Sum := Sum + Element (Position);
      --  end loop
      --
      declare

         procedure Iteration
           (Start, Finish  : Interfaces.Unsigned_32;
            Sum    : in out Interfaces.Unsigned_32)
         is
            Current_Position : Integer_Vectors.Cursor          :=
               Number_Array.To_Cursor (Index => Natural (Start));
            End_Position     : constant Integer_Vectors.Cursor :=
               Number_Array.To_Cursor (Index => Natural (Finish));
            use type Integer_Vectors.Cursor;
            use type Interfaces.Unsigned_32;
         begin

            loop
               Sum := Sum + Integer_Vectors.Element (Current_Position);
               exit when Current_Position = End_Position;
               Integer_Vectors.Next (Current_Position);
            end loop;
         end Iteration;
      begin
         Work_Seeking_Integer_Addition_Reducer
           (From          => Interfaces.Unsigned_32 (Number_Array.First_Index),
            To            => Interfaces.Unsigned_32 (Number_Array.Last_Index),
            Process       => Iteration'Access,
            Worker_Count => Parallel.Default_Worker_Count,
            Item          => Sum,
            Work_Budget   =>
               Parallel_Test_Harness.Work_Budget_For_Even_Loads,
            Use_Affinity  => Parallel_Test_Harness.Use_Affinity);
      end;

      New_Line;
      Put_Line
        ("** Work Seeking Parallel Elementary Integer Linked List Sum = " &
         Interfaces.Unsigned_32'Image (Sum));
   end Test5b;

   Test5c : declare
      Number_Array : Integer_Vectors.Vector := Integer_Vectors.Empty_Vector;
      Sum          : Interfaces.Unsigned_32                := 0;
   --      Start_Time : constant Real_Time.Time := Real_Time.Clock;
   begin
      --  for I in 1 .. 100 with
      --    Work_Seeking_Integer_Vectors_Appending_Reducer <Number_Array>
      --  loop
      --    Number_Array.Append (New_Item => I, Count => 1);
      --  end loop
      --
      declare

         procedure Iteration
           (Start, Finish : Natural;
            Number_Array : in out Integer_Vectors.Vector) is
         begin
            for I in Start .. Finish loop
               Number_Array.Append
                 (New_Item => Interfaces.Unsigned_32 (I),
                  Count => 1);
            end loop;
         end Iteration;
      begin
         Work_Stealing_Integer_Vector_Appending_Reducer
           (From          => 1,
            To            => 100,
            Process       => Iteration'Access,
            Worker_Count => Parallel.Default_Worker_Count,
            Item          => Number_Array,
            Minimum_Steal => Parallel_Test_Harness.Minimum_Steal,
            Work_Budget   =>
               Parallel_Test_Harness.Work_Budget_For_Even_Loads);
      end;

      Put_Line
        ("** Work Stealing Parallel Elementary Integer" &
         " Vector Linked List Creation");
      declare
         procedure Print_Elements (Position : Integer_Vectors.Cursor) is
         begin
            Put
              (Interfaces.Unsigned_32'Image
                  (Integer_Vectors.Element (Position => Position)));
         end Print_Elements;

      begin
         Number_Array.Iterate (Process => Print_Elements'Access);
      end;

      --  BJM need to put proper syntax here....

      --  for Element in Number_Array with Integer_Addition_Reducer <Sum>
      --  loop
      --    Sum := Sum + Element (Position);
      --  end loop
      --
      declare
         procedure Iteration
           (Start, Finish : Interfaces.Unsigned_32;
            Sum    : in out Interfaces.Unsigned_32)
         is
            Current_Position : Integer_Vectors.Cursor;
            use type Interfaces.Unsigned_32;
         begin
            for I in Start .. Finish loop
               Current_Position
                 := Number_Array.To_Cursor (Index => Natural (I));
               Sum := Sum + Integer_Vectors.Element (Current_Position);
            end loop;
         end Iteration;
      begin
         Work_Stealing_Integer_Addition_Reducer
           (From          => Interfaces.Unsigned_32 (Number_Array.First_Index),
            To            => Interfaces.Unsigned_32 (Number_Array.Last_Index),
            Process       => Iteration'Access,
            Worker_Count => Parallel.Default_Worker_Count,
            Item          => Sum,
            Minimum_Steal => Parallel_Test_Harness.Minimum_Steal,
            Work_Budget   =>
               Parallel_Test_Harness.Work_Budget_For_Even_Loads);
      end;

      New_Line;
      Put_Line
        ("** Work Stealing Parallel Elementary Integer Linked List Sum = " &
         Interfaces.Unsigned_32'Image (Sum));
   end Test5c;
end Container_Manipulations;
