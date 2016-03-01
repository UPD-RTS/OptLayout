-----------------------------------------------------------------------
--         OptLayout - A Cache-aware Memory Layout Optimizer          --
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
-- FILE NAME      : loop_tree.ads
-- PACKAGE        : LOOP_TREE spec
-- PURPOSE        : Representation of teh loop-call tree data structure
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with CFG_Graph, Tree;
with RAM;

-- Instantiate the TREE generic package
package Loop_Tree is
   use Ada.Strings.Unbounded;

   -- Definition of node types
   type Loop_Tree_Node_Type is (LoopNode, RootNode, IndependentProcNode, ProcNode);
   -- Definition of link types (dummy as we do not really need this)
   type Loop_Tree_Link_Type is (Call);

   --  Array of node indexes for the loop BB
   type Loop_BBs is array (Positive range <>) of Positive;
   -- Pointer to a list of Basic blocks for a LCT node
   type Loop_BBs_Ptr is access all Loop_BBs;
   -- Pointer to a list of pointers to Basic blocks for a LCT node
   type Loop_BBs_Array is array (Positive range <>) of Loop_BBs_ptr;
   -- Pointer to a list of pointers to Basic blocks for a LCT node
   type Loop_BBs_Arr_Ptr is access all Loop_BBs_Array;

   -- Unchecked deallocation
   procedure Free_BB_Array is new Ada.Unchecked_Deallocation (Loop_BBs_Array,
                                                              Loop_BBs_Arr_Ptr);
   -- Unchecked deallocation
   procedure Free_Loop_BBs is new Ada.Unchecked_Deallocation (Loop_BBs,
                                                              Loop_BBs_Ptr);

   -- Relevant info for a LCT node
   type Loop_Node_Info is record
      -- String identifier
      Id       : Unbounded_String;
      -- String identifier (label)
      Label    : Unbounded_String;
      -- Numerical identifier
      Index    : Positive;
      -- Size of the procedure represented by this node
      Size     : Integer := 0;
      -- Name of the procedure represented by this node
      ProcName : Unbounded_String;
      -- String identifier of the respective CFG
      CfgId    : String (1..10) := "***self***";
      -- Bound (if it is a loop node)
      Bound    : Positive;
      -- Pointer to a list of basic blocks
      BBs      : Loop_BBs_Ptr;
      -- Number of elements i the basic block list
      BBs_num  : Natural;
   end record;

   -- Equality test
   function "=" (Left, Right: Loop_Node_Info) return Boolean;

   -- Merge two LCT nodes (updates parent, successors and BB lists)
   procedure Merge (Left, Right: in out Loop_Node_Info);

   -- Print out node info
   procedure Print (N : Loop_Node_Info);

   -- Return the node type
   function Print_Nt (nt : Loop_Tree_Node_Type) return String;

   -- Return the link type
   function Print_Lt (lt : Loop_Tree_Link_Type) return String;

   -- Print the node info
   procedure PrettyPrint (N : Loop_Node_Info);

   -- Instantiation of the generic TREE package to LCT
   package LT is
     new Tree (Loop_Node_Info,
               Loop_Tree_Node_Type,
               Loop_Tree_Link_Type,
               "="'Access,
               Merge'Access,
               Print'Access,
               PrettyPrint'Access,
               Print_Nt'Access,
               Print_Lt'Access);
   use LT;

   -- Array of LCT
   type Loop_Tree_Array is array (Natural range <>) of Tree_Type_Ptr;

   -- Pointer to a LCT
   type Loop_Tree_Ptr is access all LT.Tree_Type_Ptr;
   -- Pointer to an array of LCT
   type Loop_Tree_Array_ptr is access all Loop_Tree_Array;

   -- Return the node info
   function Get_Node_Info (n : LT.Tree_Node_Ptr) return Loop_Node_Info;

   -- Return the index-th LCT in the static list of LCTs
   function Get_Tree (index : Positive) return LT.Tree_Type_Ptr;

   -- Print the list of BBs of a LCT node
   procedure Print_BBs (n : LT.Tree_Node_Ptr);

   -- Initialize the static list of trees of the given size
   procedure Init_trees (size : Positive);

   -- Update the list of BB in a LCT node
   procedure Set_BBs(N: in out Tree_Node_Ptr; newBBs : Loop_BBs_Ptr;
                     size : Positive);

   -- Set the bound of a loop node (max iterations)
   procedure Set_Bound(n : in out Tree_Node_Ptr; bound : Positive);

   --procedure Set_NType (n : in out Tree_Node_Ptr; nT : Loop_Tree_Node_Type);

   -- Check whether a basic block is already included in a node
   function In_BBs (BBs : Loop_BBs_Ptr; size : Positive; elem : Positive)
                    return Boolean;

   -- Return a string representation of the list of BB in a node
   function Get_BBs (n : LT.Tree_Node_Ptr) return String;

   -- Add a tree to the static list
   procedure Add_Tree (treePtr : LT.Tree_Type_Ptr; index : Positive);

   -- Return a pointer to the static list of trees
   function Trees return Loop_Tree_Array_Ptr;

   -- Return a pointer to the LCT with the given name (if any)
   function Get_Tree_By_Name (name : String) return Tree_Type_Ptr;

   --function Get_Tree_By_Id (id : Integer) return Tree_Type_Ptr;

   -- Recursively count the number of loop nodes (give a root node)
   function Get_Loops (treePtr : LT.Tree_Type_Ptr) return Natural;

   -- Get a loop node given its label
   function Get_Loop_By_Label (tree: Tree_Type_Ptr; name : String) return Tree_Node_Ptr;

   -- Export the super-LCT to .DOT format and
   -- generate a graphical representation (.PDF)
   procedure Export_To_Dot (Tree_list : Loop_Tree_Array_ptr;
                            trees : Positive;
                            size : Positive);

   -- Export a single LCT to .DOT format and
   -- generate a graphical representation (.PDF)
   procedure Export_to_Dot_Bounded;

   -- Export a single LCT to .DOT format and
   -- generate a graphical representation (.PDF)
   procedure Export_To_Dot_Single (Tree_list : Loop_Tree_Array_ptr;
                                   trees : Positive;
                                   size : Positive; cfgInd : Positive);

   -- Merge all LCT into a super-LCT
   procedure Merge_Trees;

   -- Return the number of Basic Blocks of a node
   function GetNumBB (n : LT.Tree_Node_Ptr) return Natural;

   -- Return the number of LCTS in the static list
   function Get_Tree_Num return Natural;

   -- Print out the LCT names (subprocedures)
   procedure Print_Tree_Names;

   -- Computes an optimized layout building on LCT
   procedure Compute_Layout;

   -- Computes an optimized layout building on WCG
   procedure Compute_Layout_WCG;

   -- Divide & conquer strategy
   procedure Process_Sub_Tree (n : Tree_Node_Ptr;
                               childNum : Natural;
                               pool : in out RAM.Proc_pool_ptr);

   -- Compute deepness of loop nests
   procedure Compute_Criticality;

private

   --  Static list of LCTs
   LoopTrees : Loop_Tree_Array_Ptr;
   -- Original number of LCTs in the list
   loopTreeNum : Natural := 0;

end Loop_Tree;

