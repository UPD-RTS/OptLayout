-----------------------------------------------------------------------
--         OptLayout - A Cache-aware Memory Layout Optimizer         --
--                                                                   --
--               Copyright (c) 2011 Enrico Mezzetti                  --
--                     University of Padua                           --
--                  <emezzett@math.unipd.it>                         --
--                                                                   --
-- This file is part of OptLayout.                                   --
--                                                                   --
-- OptLayout is free software: you can redistribute it and/or modify --
-- it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, --
-- or (at your option) any later version.                            --
--                                                                   --
-- OptLayout is distributed in the hope that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public License --
-- along with OptLayout. If not, see <http://www.gnu.org/licenses/>. --
--                                                                   --
--                                                                   --
--                                                                   --
-----------------------------------------------------------------------
-- FILE NAME      : tree.ads
-- PACKAGE        : TREE spec (generic)
-- PURPOSE        : Generic package for tree-based data structure
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------


with Ada.Finalization;
with System.Storage_Pools;

generic

   type Tree_Node_Info is private;
   type Node_Type is private; --(RootNode, LoopNode, ProcCallNode, ExitNode, UndefNode);
   type Link_Type is private; --(Call, Taken, Nottaken);
   F : access function (Left, Right: Tree_Node_Info) return Boolean;
   M : access procedure (Left, Right: in out Tree_Node_Info);
   P : access procedure (N : Tree_Node_Info);
   PPretty : access procedure (N : Tree_Node_Info);
   Pnt : access function (nt : Node_Type) return String;
   Plt : access function (lt : Link_Type) return String;

package Tree is

   pragma Elaborate_Body;

   -- Type definition for a tree
   type Tree_Type is private;

   -- Type definition for a tree node
   type Tree_Node is private;

   -- Type definition for a pointer to a tree
   type Tree_Type_Ptr is access all Tree_Type;

   -- Type definition for a pointer to a tree node
   type Tree_Node_Ptr is access all Tree_Node;

   -- Type definition for a list of tree node pointers
   type Tree_Node_List is array (Positive range <>) of Tree_Node_Ptr;
   -- Type definition for a pointer to list of tree nodes
   type Tree_Node_List_Ptr is access all Tree_Node_List;

   -- **UNUSED**
   -- Type definition for a list of link types
   type Link_Type_List is array (Positive range <>) of Link_Type;

   --  Creates a new Tree with Root as root node
   function Create (Root : Tree_Node_Ptr; name : String) return Tree_Type_ptr;


   -- Creates a Tree node
   function Create_Node (nInfo : Tree_Node_Info; nType : Node_Type;
                         lType : Link_Type) return Tree_Node_Ptr;

   --  Equality test operator for the TREE_NODE generic type
   function "=" (Left, Right : Tree_Node_Info) return Boolean renames F.all;

   -- Merges node infos
   procedure Merge_Node_Info (Left, Right : in out Tree_Node_Info) renames M.all;

   -- Prints out the node Id
   procedure Print_Node (N : Tree_Node_Info) renames P.all;
   -- Renames PPretty.all;
   procedure Print_Node_Pretty (N : Tree_Node_Ptr);
   -- Prints out the node info
   procedure Print_Node (N : Tree_Node_Ptr);

   --  Returns a string representation of the node type
   function Print (nt : Node_Type) return String renames Pnt.all;

   --  Returns a string representation of the link type
   function Print (lt : Link_Type) return String renames Plt.all;

   --  Removes all tree nodes
   procedure Clear (T : in out Tree_Type);

   -- Remove a child node form the successors of its parent node
   procedure Remove_From_Succ (N, parent: in out Tree_Node_Ptr);

   --  Removes a node from the tree
   procedure Remove (N : in out Tree_Node_Ptr; preserve : in Boolean);

   -- Merges two nodes
   procedure Merge_Nodes (Left, Right : in out Tree_Node_Ptr);

   -- Return the child identified by the given index
   function Child (P: in Tree_Node_Ptr; Child : in Positive)
                   return Tree_Node_Ptr ;

   --  Returns teh node parent
   function Parent (N : in  Tree_Node_Ptr) return Tree_Node_Ptr;

   --  Unimplemented
   function Arity (T : Tree_Type) return Natural;

   --  Return True if and only if the tree has any non-null children.
   function Has_Children (T : in Tree_Type) return Boolean;

   --  Return True if and only if the tree has no items.
   function Is_Null (T : in Tree_Type) return Boolean;


   --  function Is_Root (T : in Tree_Type) return Boolean;
   --  Return True if and only if the tree is at the root of a tree.

   --  Return the item at the root of the tree.
   --  O(1)
   function Get_Root (T : in Tree_Type) return Tree_Node_Ptr;

   --  Return TRUE if node N is the root node of tree T
   function Is_Root (T : in Tree_Type; N : in Tree_Node_Ptr) return Boolean;

   --  Return the number of successor for a tree node
   function Get_Succ_Number (N : in Tree_Node_Ptr) return Integer;

   --  Return the list of successors for a tree node
   function Get_Succ (N : in Tree_Node_Ptr) return Tree_Node_List;

   --  Return a pointer to the list of successors for a tree node
   function Get_Succ (N : in Tree_Node_Ptr) return Tree_Node_List_Ptr;

   --  Print the list of successors for a tree node
   procedure Print_Succ (N : in Tree_Node_Ptr);

   --  Copies a node list into another
   procedure Copy_Node_List (Left, Right : Tree_Node_List_Ptr);

   --  Add a tree node to a node successor's list
   procedure Add_Succ (Parent, Child : in out Tree_Node_Ptr);

   --  Returns the number of children for a given node
   function Get_Succ_Size (N : Tree_Node_Ptr) return Positive;

   --  Mark the current node
   procedure Mark_Node (n : Tree_Node_Ptr);

   --  Unmark all the tree nodes
   procedure Unmark_Tree (t : Tree_Type_Ptr);

   --  Unmark all the tree nodes (given its root node)
   procedure Unmark_Tree (root : Tree_Node_Ptr);

   --  Return TRUE if the node is marked
   function Is_marked (n : Tree_Node_Ptr) return Boolean;

   --  Supplementary mark flag
   procedure Mark_Node_Curr (n : Tree_Node_Ptr);
   --  Supplementary mark flag
   procedure Unmark_Tree_Curr (root : Tree_Node_Ptr);
   --  Supplementary mark flag
   function Is_marked_Curr (n : Tree_Node_Ptr) return Boolean;

   --  Return the node weight
   function Get_Weight (n : Tree_Node_Ptr) return Integer;
   -- Set the node weight
   procedure Set_Weight (n : Tree_Node_Ptr; w : Integer);


   -- Returns the node info
   function Get_Info (n : Tree_Node_Ptr) return Tree_Node_Info;

   -- Sets the node info
   procedure Set_Info (n : in out Tree_Node_Ptr; info : Tree_Node_Info);

   -- Sets node "p" as parent of "c"
   procedure Set_Parent (c : in out Tree_Node_Ptr; p : Tree_Node_Ptr);

   -- Returns the node type
   function Get_NType (n: Tree_Node_Ptr) return Node_type;
   -- Set the node type
   procedure Set_NType (n: Tree_Node_Ptr; nT: Node_Type);

   -- Returns the link type
   function Get_LType (n: Tree_Node_Ptr) return Link_type;
   -- Return the node string identifier
   function Get_Id (t : Tree_Type_Ptr) return String;
   -- Check for null pointers
   function IsNull (n : Tree_Node_ptr) return Boolean;

private

   -- Default size for the successors list
   Succ_Default_Size : constant Positive := 30;
   -- Increment step for the dynamic successors list
   Increment_Step : constant Positive := 10;

   -- A tree is defined by an ID and a root node
   type Tree_Type is record --+new Ada.Finalization.Controlled with record
      -- Tree string identifier
      Id      : String (1..100);
      -- Pointer to the tree root node
      Head    : Tree_Node_Ptr;
   end record;

   -- A tree node
   type Tree_Node is record --new Ada.Finalization.Controlled with record
      -- Implementation specific info
      Info    	  	: Tree_Node_Info;
      -- Implementation specific node type
      NType   	  	: Node_Type;
      -- Implementation specific Link_type (from Parent -> this Node)
      LType	  	: Link_Type;
      -- Pointer to the parent node
      Parent  	  	: Tree_Node_Ptr := null;
      -- Dynamically growing list of successors
      Succ_List   	: Tree_Node_List_Ptr := new Tree_Node_List (1..Succ_Default_Size);
      -- Size of the list of successors
      Succ_list_Size 	: Positive := Succ_Default_Size;
      -- First free position in the successors list
      Next_Slot		: Positive := 1;
      -- Is this node marked?
      Marked    	: Boolean := False;
      -- Weight attribute (for future use)
      Weight    	: Integer := 0;
      -- Has this node been marked in the current traversal of the tree?
      MarkedCurr	: Boolean := False;
      --  LType : Link_Type_List (1..Succ_Default_Size);
   end record;

   -- Object finalization
   procedure Finalize (T : in out Tree_Type);

end Tree;
