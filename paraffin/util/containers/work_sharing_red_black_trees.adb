------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                    W O R K _ S H A R I N G                               --
--                 R E D _ B L A C K _ T R E E S                            --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                   Copyright (C) 2011, Bradley J. Moore                   --
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
--
--  The Node Insertion and Deletion implementation was adapted from a
--  public domain Java version
--  (http://eternallyconfuzzled.com/tuts/datastructures/jsw_tut_rbtree.aspx )

with Parallel_Test_Harness;
with Parallel.Recursion.Work_Sharing;
with Parallel.Recursion.Work_Sharing_Functional_Reduction;
with Parallel.Recursion.Work_Sharing_Procedural_Reduction;
use Parallel.Recursion;

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Work_Sharing_Red_Black_Trees is

   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Object => Node_Type, Name => Node_Access);

   function Is_Red (Node : Node_Access) return Boolean;

   type Direction_Kinds is (Left, Right);

   function Double_Rotation
     (Root : Node_Access;
      Direction : Direction_Kinds) return Node_Access;

   function Single_Rotation
     (Root : Node_Access;
      Direction : Direction_Kinds) return Node_Access;

   package Red_Black_Tree_Recursion is new
     Parallel.Recursion.Work_Sharing (Work_Type => Node_Access);

   ------------------------------------------

   function Black_Height (Root : Node_Access) return Natural
   is
      Left_Hand, Right_Hand : Natural;
      Left, Right : Node_Access;
   begin
      if Root = null then
         return 1;
      end if;

      Left := Root.Left;
      Right := Root.Right;

      --  Consecutive red links
      if  Is_Red (Root)  then
         if  Is_Red (Left) or else Is_Red (Right)  then
            pragma Debug
              (Parallel_Test_Harness.Debug_Logging,
               Put_Line ("Red violation"));
            return 0;
         end if;
      end if;

      Left_Hand := Black_Height (Left);
      Right_Hand := Black_Height (Right);

      --  Invalid binary search tree
      if   (Left /= null and then
              (Root.Element < Left.Element or else
                 Root.Element = Left.Element))  or else
        (Right /= null and then
           (Right.Element < Root.Element or else
              Right.Element = Root.Element)) then

         pragma Debug
           (Parallel_Test_Harness.Debug_Logging,
            Put_Line ("Binary tree violation"));
         return 0;
      end if;

      -- Black height mismatch
      if  Left_Hand /= 0 and then Right_Hand /= 0 and then
        Left_Hand /= Right_Hand  then
         pragma Debug
           (Parallel_Test_Harness.Debug_Logging,
            Put_Line ("Black violation"));
         return 0;
      end if;

      --  Only count black links
      if  Left_Hand /= 0 and then Right_Hand /= 0 then
         if Is_Red (Root) then
            return Left_Hand;
         else
            return Left_Hand + 1;
         end if;
      else
         return 0;
      end if;
   end Black_Height;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Tree) is

      Dispatcher : aliased
        Red_Black_Tree_Recursion.Recursion_Dispatcher_Access := null;

      procedure Span_Tree (Node : Node_Access) is
         --  Use the Rosen trick to gain write access to an "in" parameter.
         Writeable_Node : constant access Node_Access
           := Node'Unrestricted_Access;
      begin

         if Node = null then
            return;
         end if;

         Span_Tree (Node => Node.Left);
         Span_Tree (Node => Node.Right);

         Free_Node (Writeable_Node.all);
      end Span_Tree;

      procedure Span_Tree_In_Parallel
        (Node : Node_Access;
         Subcontractors : Parallel.Worker_Count_Type)
      is
         --  Use the Rosen trick to gain write access to an "in" parameter.
         Writeable_Node : constant access Node_Access
           := Node'Unrestricted_Access;
      begin

         if Node = null then
            return;
         end if;

         Dispatcher.Recurse
           (Item    => Node.Left,
            Split => 1,
            Of_Splits => Direction_Kinds'Range_Length,
            Subcontractors => Subcontractors);

         Dispatcher.Recurse
           (Item    => Node.Right,
            Split => 2,
            Of_Splits => Direction_Kinds'Range_Length,
            Subcontractors => Subcontractors);

         Free_Node (Writeable_Node.all);
      end Span_Tree_In_Parallel;

   begin

      --  Until Ada 2012, do in parallel, because Deepend can't
      --  deallocate controlled objects
      pragma Compile_Time_Warning
        (Parallel_Test_Harness.Ada_2012_Warnings,
         "Ada 2012 - Use Deepend to Clear Tree");

      Red_Black_Tree_Recursion.Execute
        (Item             => Container.Root,
         Dispatcher        => Dispatcher'Access,
         Parallel_Process => Span_Tree_In_Parallel'Access,
         Sequential_Process => Span_Tree'Access,
         --  Priority          => System.Priority'Last,
         Worker_Count     => Parallel.Default_Worker_Count,
         Use_Affinity     => Parallel_Test_Harness.Use_Affinity);

      --  Since we used aliases to delete the node, the node pointers were
      --  never modified, so we need to reflect that the nodes were actually
      --  deleted
      Container.Root := null;
   end Clear;

   ---------------------------------
   --  Composite_Iterate_And_Reduce
   ---------------------------------

   procedure Composite_Iterate_And_Reduce
     (Container : Tree;
      Process   : not null access procedure
        (Position : Cursor;
         Result : out Result_Type);
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size;
      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Worker_Count : Parallel.Positive_Worker_Count :=
        Parallel.Default_Worker_Count;
      Result : in out Result_Type)
   is

      package Red_Black_Tree_Recursion is new
        Parallel.Recursion.Work_Sharing_Procedural_Reduction
          (Work_Type => Node_Access,
           Result_Type => Result_Type,
           Reducer => Reducer,
           Identity_Value => Identity_Value);

      Dispatcher : aliased
        Red_Black_Tree_Recursion.Recursion_Dispatcher_Access := null;

      procedure Span_Tree
        (Node : Node_Access;
         Result : out Result_Type) is
      begin
         if Node = null then
            Result := Identity_Value;
         else
            declare
               Temp : Result_Type;
            begin
               Span_Tree (Node.Left, Result);

               Process
                 (Cursor'(Container'Unrestricted_Access, Node),
                  Temp);

               Reducer
                 (Left => Result,
                  Right => Temp);

               Span_Tree (Node.Right, Temp);

               Reducer
                 (Left => Result,
                  Right => Temp);
            end;
         end if;
      end Span_Tree;

      procedure Span_Tree_In_Parallel
        (Node : Node_Access;
         Subcontractors : Parallel.Worker_Count_Type;
         Result : out Result_Type) is
      begin

         if Node = null then
            Result := Identity_Value;
         else

            declare
               Temp : Result_Type;
            begin
               Dispatcher.Recurse
                 (Item => Node.Left,
                  Split => 1,
                  Of_Splits => 2,
                  Subcontractors => Subcontractors,
                  Result => Result);

               Process
                 (Cursor'(Container'Unrestricted_Access, Node),
                  Temp);

               Reducer
                 (Left => Result,
                  Right => Temp);

               Dispatcher.Recurse
                 (Item => Node.Right,
                  Split => 2,
                  Of_Splits => 2,
                  Subcontractors => Subcontractors,
                  Result => Temp);

               Reducer
                 (Left => Result,
                  Right => Temp);
            end;
         end if;
      end Span_Tree_In_Parallel;

      Temp : Result_Type;
   begin
      Red_Black_Tree_Recursion.Execute
        (Item         => Container.Root,
         Process      => Span_Tree'Access,
         Parallel_Process => Span_Tree_In_Parallel'Access,
         Dispatcher   => Dispatcher'Access,
         Storage_Size  => Storage_Size,
         Priority      => Priority,
         Worker_Count => Worker_Count,
         Use_Affinity => Parallel_Test_Harness.Use_Affinity,
         Result => Temp);

      pragma Warnings (Off, "*Temp*modified*value never referenced");
      Reducer (Left => Result, Right => Temp);
      pragma Warnings (On, "*Temp*modified*value never referenced");
   end Composite_Iterate_And_Reduce;

   --------------
   -- Contains --
   --------------

   function Contains (Container : Tree; Item : Element_Type) return Boolean
   is
      Current : Node_Access := Container.Root;
   begin

      while Current /= null loop
         if Item = Current.Element then
            return True;
         elsif Item < Current.Element then
            Current := Current.Left;
         else
            Current := Current.Right;
         end if;
      end loop;

      return False;
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Tree;
      Item      : Element_Type)
   is
      Head : aliased Node_Type :=   --  False tree root
        Node_Type'
          (Left => null,
           Right => null,
           Element => Item,
           Is_Red => True);

      Parent, Grandparent : Node_Access := null;  --  Helpers
      Iterator : Node_Access := Head'Unchecked_Access;

      Found_Item, Save : Node_Access := null;
      Direction, Direction2 : Direction_Kinds := Right;
      Last : Direction_Kinds;
   begin

      if Container.Root /= null then

         Iterator.Right := Container.Root;

         --  Search and push a red down
         Search_Loop :
         while  (Direction = Right and then Iterator.Right /= null) or else
           (Direction = Left and then Iterator.Left /= null) loop

            Last := Direction;

            --  Update helpers
            Grandparent  := Parent;
            Parent := Iterator;
            if Direction = Right then
               Iterator := Iterator.Right;
            else
               Iterator := Iterator.Left;
            end if;

            if  Iterator.Element < Item then
               Direction := Right;
            else
               Direction := Left;
            end if;

            --  Save found node
            if  Iterator.Element = Item then
               Found_Item := Iterator;
            end if;

            --  Push the red node down
            if  not Is_Red (Iterator) and then not
              ((Direction = Left and then Is_Red (Iterator.Left)) or else
                 (Direction = Right and then Is_Red (Iterator.Right))) then

               if (Direction = Left and then Is_Red (Iterator.Right)) or else
                 (Direction = Right and then Is_Red (Iterator.Left)) then

                  if Last = Left then
                     Parent.Left := Single_Rotation (Iterator, Direction);
                     Parent := Parent.Left;
                  else
                     Parent.Right := Single_Rotation (Iterator, Direction);
                     Parent := Parent.Right;
                  end if;
               elsif  not (Direction = Left and then
                              Is_Red (Iterator.Right)) or else
                          (Direction = Right and then
                              Is_Red (Iterator.Left)) then

                  if Last = Left then
                     Save := Parent.Right;
                  else
                     Save := Parent.Left;
                  end if;

                  if  Save /= null then

                     if not Is_Red (Save.Right) and then
                       not Is_Red (Save.Left) then

                        -- Color flip
                        Parent.Is_Red := False;
                        Save.Is_Red := True;
                        Iterator.Is_Red := True;
                     else

                        if Grandparent.Right = Parent then
                           Direction2 := Right;
                        else
                           Direction2 := Left;
                        end if;

                        if (Last = Left and then Is_Red (Save.Left)) or else
                          (Last = Right and then Is_Red (Save.Right)) then

                           if Direction2 = Left then
                              Grandparent.Left :=
                                Double_Rotation (Parent, Last);
                           else
                              Grandparent.Right :=
                                Double_Rotation (Parent, Last);
                           end if;

                        elsif  (Last = Left and then Is_Red (Save.Right))
                          or else
                            (Last = Right and then Is_Red (Save.Left)) then

                           if Direction2 = Left then
                              Grandparent.Left :=
                                Single_Rotation (Parent, Last);
                           else
                              Grandparent.Right :=
                                Single_Rotation (Parent, Last);
                           end if;
                        end if;

                        --  Ensure correct coloring

                        Iterator.Is_Red := True;
                        if Direction = Left then
                           Grandparent.Left.Is_Red := True;
                           Grandparent.Left.Left.Is_Red := False;
                           Grandparent.Left.Right.Is_Red := False;
                        else
                           Grandparent.Right.Is_Red := True;
                           Grandparent.Right.Left.Is_Red := False;
                           Grandparent.Right.Right.Is_Red := False;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end loop Search_Loop;

         --  Replace and remove if found
         if Found_Item /= null then

            Found_Item.Element := Iterator.Element;

            if Parent.Right = Iterator then
               if Iterator.Left = null then
                  Parent.Right := Iterator.Right;
               else
                  Parent.Right := Iterator.Left;
               end if;
            else
               if Iterator.Left = null then
                  Parent.Left := Iterator.Right;
               else
                  Parent.Left := Iterator.Left;
               end if;
            end if;

            Free_Node (Iterator);
         end if;

         --  Update root and make it black
         Container.Root := Head.Right;
         if Container.Root /= null then
            Container.Root.Is_Red := False;
         end if;
      end if;

   end Delete;

   -------------------------------------------------------

   function Double_Rotation
     (Root : Node_Access;
      Direction : Direction_Kinds) return Node_Access is
   begin

      case Direction is
         when Left =>
            Root.Right := Single_Rotation (Root.Right, Right);

         when Right =>
            Root.Left := Single_Rotation (Root.Left, Left);
      end case;

      return Single_Rotation (Root, Direction);

   end Double_Rotation;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Node.Element;
   end Element;

   -------------------------------------------------------------

   function Elementary_Iterate_And_Reduce
     (Container : Tree;
      Process   : not null access function
        (Position : Cursor) return Result_Type;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size;
      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Worker_Count : Parallel.Positive_Worker_Count :=
        Parallel.Default_Worker_Count;
      Initial_Value : Result_Type := Identity_Value) return Result_Type
   is

      package Red_Black_Tree_Recursion is new
        Parallel.Recursion.Work_Sharing_Functional_Reduction
          (Work_Type => Node_Access,
           Result_Type => Result_Type,
           Reducer => Reducer,
           Identity_Value => Identity_Value);

      Dispatcher : aliased
        Red_Black_Tree_Recursion.Recursion_Dispatcher_Access := null;

      function Span_Tree
        (Node : Node_Access) return Result_Type is
      begin
         if Node = null then
            return Identity_Value;
         else
            return
              Reducer
                (Left => Reducer
                     (Left => Span_Tree (Node.Left),
                      Right => Process
                        (Cursor'(Container'Unrestricted_Access, Node))),
                 Right => Span_Tree (Node.Right));
         end if;
      end Span_Tree;

      function Span_Tree_In_Parallel
        (Node : Node_Access;
         Subcontractors : Parallel.Worker_Count_Type) return Result_Type is
      begin
         if Node = null then
            return Identity_Value;
         else
            return
              Reducer
                (Left => Reducer
                     (Left => Dispatcher.Recurse
                          (Item => Node.Left,
                           Split => 1,
                           Of_Splits => 2,
                           Subcontractors => Subcontractors),
                      Right => Process
                        (Cursor'(Container'Unrestricted_Access, Node))),
                 Right => Dispatcher.Recurse
                   (Item => Node.Right,
                    Split => 2,
                    Of_Splits => 2,
                    Subcontractors => Subcontractors));

         end if;
      end Span_Tree_In_Parallel;

      Result : constant Result_Type := Red_Black_Tree_Recursion.Execute
        (Item          => Container.Root,
         Dispatcher => Dispatcher'Access,
         Process       => Span_Tree'Access,
         Parallel_Process => Span_Tree_In_Parallel'Access,
         Storage_Size => Storage_Size,
         Priority   => Priority,
         Worker_Count => Worker_Count,
         Use_Affinity  => Parallel_Test_Harness.Use_Affinity);

   begin
      return Reducer (Left => Initial_Value, Right => Result);
   end Elementary_Iterate_And_Reduce;

   --------------------------------------------------------------

   procedure Insert
     (Container : in out Tree;
      New_Item  : Element_Type)
   is
      function Make_Node return Node_Access is
      begin
         return new Node_Type'
           (Left => null,
            Right => null,
            Element => New_Item,
            Is_Red => True);
      end Make_Node;

      --  False tree root
      Head : aliased Node_Type := Node_Type'
        (Left => null,
         Right => null,
         Element => New_Item,
         Is_Red => True);

      Grandparent : Node_Access := null;
      Parent : Node_Access := Head'Unchecked_Access;
      Iterator, Iterator_Parent : Node_Access := null;
      Direction : Direction_Kinds := Left;
      Last : Direction_Kinds := Left;

   begin -- Insert

      if Container.Root = null then
         --  Empty tree case
         Container.Root := Make_Node;
      else

         --  Set up helpers
         Parent.Right := Container.Root;
         Iterator := Container.Root;

         --  Search down the tree
         Tree_Search_Loop : loop
            if Iterator = null then

               --  Insert new node at the bottom
               Iterator := Make_Node;

               case Direction is
                  when Left =>
                     Iterator_Parent.Left := Iterator;

                  when Right =>
                     Iterator_Parent.Right := Iterator;
               end case;

               --  Error checking? Is this necessary?
               --  if Iterator = null then
               --     return 0;
                -- end if;

            elsif Is_Red (Iterator.Left) and then
              Is_Red (Iterator.Right) then

               --  Color flip
               Iterator.Is_Red := True;
               Iterator.Left.Is_Red := False;
               Iterator.Right.Is_Red := False;
            end if;

            --  Fix red violation
            if Is_Red (Iterator) and then Is_Red (Iterator_Parent) then
               if Parent.Right = Grandparent then

                  case Last is
                     when Left =>
                        if Iterator = Iterator_Parent.Left then
                           Parent.Right :=
                             Single_Rotation (Grandparent, Right);
                        else
                           Parent.Right :=
                             Double_Rotation (Grandparent, Right);
                        end if;

                     when Right =>
                        if Iterator = Iterator_Parent.Right then
                           Parent.Right :=
                             Single_Rotation (Grandparent, Left);
                        else
                           Parent.Right :=
                             Double_Rotation (Grandparent, Left);
                        end if;
                  end case;
               else
                  case Last is

                     when Left =>
                        if Iterator = Iterator_Parent.Left then
                           Parent.Left :=
                             Single_Rotation (Grandparent, Right);
                        else
                           Parent.Left :=
                             Double_Rotation (Grandparent, Right);
                        end if;

                     when Right =>
                        if Iterator = Iterator_Parent.Right then
                           Parent.Left :=
                             Single_Rotation (Grandparent, Left);
                        else
                           Parent.Left :=
                             Double_Rotation (Grandparent, Left);
                        end if;
                  end case;
               end if;
            end if;

            --  Stop if found
            exit Tree_Search_Loop when Iterator.Element = New_Item;

            Last := Direction;
            if Iterator.Element < New_Item then
               Direction := Right;
            else
               Direction := Left;
            end if;

            --  Update helpers
            if Grandparent /= null then
               Parent := Grandparent;
            end if;

            Grandparent := Iterator_Parent;
            Iterator_Parent := Iterator;

            case Direction is
               when Right =>
                  Iterator := Iterator.Right;

               when Left =>
                  Iterator := Iterator.Left;
            end case;

         end loop Tree_Search_Loop;

         --  Update root
         Container.Root := Head.Right;

      end if;

      --  Make root black
      Container.Root.Is_Red := False;

   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Tree) return Boolean is
   begin
      return (Container.Root = null);
   end Is_Empty;

   ------------------------------------------

   function Is_Red (Node : Node_Access) return Boolean is
   begin
      return (Node /= null and then Node.Is_Red);
   end Is_Red;

   ------------------------------------------

   function Is_Valid (Container : Tree) return Boolean is
   begin
      return (Black_Height (Container.Root) > 0);
   end Is_Valid;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Tree;
      Process   : not null access procedure (Position : Cursor);
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size;
      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Worker_Count : Parallel.Positive_Worker_Count :=
        Parallel.Default_Worker_Count)
   is
      Dispatcher : aliased
        Red_Black_Tree_Recursion.Recursion_Dispatcher_Access := null;

      procedure Span_Tree (Node : Node_Access) is
      begin

         if Node = null then
            return;
         end if;

         Span_Tree (Node.Left);
         Process (Cursor'(Container'Unrestricted_Access, Node));
         Span_Tree (Node.Right);

      end Span_Tree;

      procedure Span_Tree_In_Parallel
        (Node : Node_Access;
         Subcontractors : Parallel.Worker_Count_Type) is
      begin

         if Node = null then
            return;
         end if;

         Dispatcher.Recurse
           (Item    => Node.Left,
            Split => 1,
            Of_Splits => Direction_Kinds'Range_Length,
            Subcontractors => Subcontractors);

         Process (Cursor'(Container'Unrestricted_Access, Node));

         Dispatcher.Recurse
           (Item    => Node.Right,
            Split => 2,
            Of_Splits => Direction_Kinds'Range_Length,
            Subcontractors => Subcontractors);
      end Span_Tree_In_Parallel;

   begin
      Red_Black_Tree_Recursion.Execute
        (Item             => Container.Root,
         Dispatcher        => Dispatcher'Access,
         Parallel_Process => Span_Tree_In_Parallel'Access,
         Sequential_Process => Span_Tree'Access,
         Storage_Size      => Storage_Size,
         Priority          => Priority,
         Worker_Count     => Worker_Count,
         Use_Affinity     => Parallel_Test_Harness.Use_Affinity);
   end Iterate;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : Tree;
      Process   : not null access procedure (Position : Cursor);
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size;
      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Worker_Count : Parallel.Positive_Worker_Count :=
        Parallel.Default_Worker_Count)
   is
      Dispatcher : aliased
        Red_Black_Tree_Recursion.Recursion_Dispatcher_Access := null;

      procedure Span_Tree (Node : Node_Access) is
      begin

         if Node = null then
            return;
         end if;

         Span_Tree (Node.Right);
         Process (Cursor'(Container'Unrestricted_Access, Node));
         Span_Tree (Node.Left);
      end Span_Tree;

      procedure Span_Tree_In_Parallel
        (Node : Node_Access;
         Subcontractors : Parallel.Worker_Count_Type) is
      begin

         if Node = null then
            return;
         end if;

         Dispatcher.Recurse
           (Item    => Node.Right,
            Split => 1,
            Of_Splits => Direction_Kinds'Range_Length,
            Subcontractors => Subcontractors);

         Process (Cursor'(Container'Unrestricted_Access, Node));

         Dispatcher.Recurse
           (Item    => Node.Left,
            Split => 2,
            Of_Splits => 2,
            Subcontractors => Subcontractors);
      end Span_Tree_In_Parallel;

   begin
      Red_Black_Tree_Recursion.Execute
        (Item             => Container.Root,
         Dispatcher       => Dispatcher'Access,
         Parallel_Process => Span_Tree_In_Parallel'Access,
         Sequential_Process          => Span_Tree'Access,
         Storage_Size     => Storage_Size,
         Priority         => Priority,
         Worker_Count     => Worker_Count,
         Use_Affinity     => Parallel_Test_Harness.Use_Affinity);
   end Reverse_Iterate;

   -------------------------------------------------------

   function Single_Rotation
     (Root : Node_Access;
      Direction : Direction_Kinds) return Node_Access
   is
      Save : Node_Access;
   begin

      case Direction is
         when Left =>
            Save := Root.Right;
            Root.Right := Save.Left;
            Save.Left := Root;

         when Right =>
            Save := Root.Left;
            Root.Left := Save.Right;
            Save.Right := Root;
      end case;

      Root.Is_Red := True;
      Save.Is_Red := False;

      return Save;

   end Single_Rotation;

end Work_Sharing_Red_Black_Trees;
