------------------------------------------------------------------------------
--
--                 Paraffin - Parallelism Generics for Ada
--                                                                          --
--                      S T A C K _ S A F E                             --
--                    W O R K _ S E E K I N G                          _
--                 R E D _ B L A C K _ T R E E S                       --
--                                                                          --
--                                S p e c                                   --
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

--  This package provides a generic parallel red-black tree container.
--  The package was initially created to test the parallelism generics,
--  though it may be useful as a general container.
--  NOTE: Some of the calls (Delete, Contains) have not yet been implemented.

private with Ada.Finalization;
with Parallel.Recursion;
with System.Storage_Elements;
with Ada.Dynamic_Priorities; use Ada;

generic

   type Element_Type is private;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Stack_Safe_Work_Seeking_Red_Black_Trees is

   --  pragma Preelaborate;  -- TODO, make Tree a streamable type
   --  pragma Remote_Types;

   type Tree is tagged limited private;
   pragma Preelaborable_Initialization (Tree);

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_Tree : constant Tree;

   No_Element : constant Cursor;

   function Is_Empty (Container : Tree) return Boolean;

   procedure Clear
     (Container : in out Tree);
   --  Frees all nodes in the tree using a stack safe work seeking parallel
   --  approach.

   function Element (Position : Cursor) return Element_Type;

   procedure Insert
     (Container : in out Tree;
      New_Item  : Element_Type);

   procedure Delete
     (Container : in out Tree;
      Item      : Element_Type);

   function Contains (Container : Tree; Item : Element_Type) return Boolean;

   procedure Iterate
     (Container : Tree;
      Process   : not null access procedure (Position : Cursor);
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size;
      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Worker_Count : Parallel.Positive_Worker_Count :=
        Parallel.Default_Worker_Count;
      Max_Depth : Parallel.Recursion.Stack_Percentage_Type
         := Parallel.Recursion.Default_Maximum_Stack_Depth;
      Deferral_Count : out Parallel.Recursion.Stack_Limit_Count);
   --  Iterates through the tree in parallel using a work seeking approach,
   --  and avoids stack overflow, while continuing to recurse successfully

   procedure Reverse_Iterate
     (Container : Tree;
      Process   : not null access procedure (Position : Cursor);
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size;
      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Worker_Count : Parallel.Positive_Worker_Count :=
        Parallel.Default_Worker_Count;
      Max_Depth : Parallel.Recursion.Stack_Percentage_Type
         := Parallel.Recursion.Default_Maximum_Stack_Depth;
      Deferral_Count : out Parallel.Recursion.Stack_Limit_Count);
   --  Iterates through the tree in parallel in reverse order using a stack
   --  safe, work seeking approach

   pragma Warnings (Off, "*formal parameter*is not referenced");
   generic
      type Result_Type is private;
      with function Reducer
        (Left, Right : Result_Type) return Result_Type;
      Identity_Value : Result_Type;
   function Elementary_Iterate_And_Reduce
     (Container : Tree;
      Process   : not null access function
        (Position : Cursor) return Result_Type;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Parallel.Default_Worker_Storage_Size;
      Priority : System.Priority := Dynamic_Priorities.Get_Priority;
      Worker_Count : Parallel.Positive_Worker_Count :=
        Parallel.Default_Worker_Count;
      Initial_Value : Result_Type := Identity_Value;
      Max_Depth : Parallel.Recursion.Stack_Percentage_Type
         := Parallel.Recursion.Default_Maximum_Stack_Depth;
      Deferral_Count : access Parallel.Recursion.Stack_Limit_Count)
      return Result_Type;
   --  Iterates through the tree in parallel using a work seeking approach
   --  to produce a stack safe result for an elementary type.

   pragma Warnings (On, "*formal parameter*is not referenced");

   generic
      type Result_Type is private;
      with procedure Reducer
        (Left, Right : in out Result_Type);
      Identity_Value : Result_Type;
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
      Max_Depth : Parallel.Recursion.Stack_Percentage_Type
         := Parallel.Recursion.Default_Maximum_Stack_Depth;
      Deferral_Count : out Parallel.Recursion.Stack_Limit_Count;
      Result : in out Result_Type);
   --  Iterates through the tree in parallel using a work seeking approach
   --  to produce a stack safe result for a composite type.

private

   use Ada.Finalization;

   type Node_Type;
   type Node_Access is access all Node_Type;

   type Node_Type is limited record
      Left    : Node_Access;
      Right   : Node_Access;
      Element : Element_Type;
      Is_Red  : Boolean;
   end record;

   type Tree is new Ada.Finalization.Limited_Controlled with record
      Root : Node_Access := null;
   end record;
   --  with Invariant => Is_Valid (Tree);

   function Is_Valid (Container : Tree) return Boolean;

   overriding
   procedure Finalize (Container : in out Tree) renames Clear;

   type Tree_Access is access all Tree;
   for Tree_Access'Storage_Size use 0;

   type Cursor is record
      Container : Tree_Access;
      Node      : Node_Access;
   end record;

   No_Element : constant Cursor := Cursor'(null, null);

   Empty_Tree : constant Tree :=
     (Limited_Controlled with Root => null);

end Stack_Safe_Work_Seeking_Red_Black_Trees;
