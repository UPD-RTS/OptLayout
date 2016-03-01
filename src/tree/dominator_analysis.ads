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
-- FILE NAME      : dominator_analysis.ads
-- PACKAGE        : DOMINATOR_ANALYSIS spec
-- PURPOSE        : Representation of the dominator tree data structure
--                  used to detect loops and build the LCT
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with CFG_Graph, Tree;

-- Instantiate the TREE generic package
package Dominator_Analysis is
   use Ada.Strings.Unbounded;

   -- Definition of node types
   type Dom_Node_Type is (RootNode, ExitNode, BB, Stub, UndefNode);

   -- Definition of link types
   type Dom_Link_Type is (None, Taken, Nottaken, Call);

   --  List of booleans used to collect dominance relation
   type Dominator_List is array (Positive range <>) of Boolean;--CFG_Graph.CFG_Vertex_Ptr;
   --  Pointer to Dominator_List (dominance info for a node)
   type Dom_List_ptr is access all Dominator_List;
   -- Array of lists of booleans (dominance info for all nodes)
   type Dominator_Array is array (Positive range <>) of Dom_List_Ptr;
   -- Pointer to Dominator_Array
   type Dom_Arr_Ptr is access all Dominator_Array;
   -- Array of Dom_Arr_Ptr
   type Dom_Arr_List is array (Positive range <>) of Dom_Arr_Ptr;
   -- Pointer to Dom_Arr_List
   type Dom_List is access all Dom_Arr_List;
   -- Immediate dominance information
   type Imm_Dominators is array (Positive range <>) of Natural;

   -- Relevant info for a Dominator tree node
   type Dom_Node_Info is record
      -- String identifier
      Id : Unbounded_String;
      -- Name of the procedure the node belongs to
      ProcName : Unbounded_String;
      -- String identifier of the respective CFG
      CfgId : String (1..10) := "***self***";
      --  Immediate dominator
      --  ImmDominator : Tree_Node_Ptr := null;
      --  List of dominator nodes
      --  Dominators : Dominator_List (1..10):= (others => null);
   end record;

   -- Equality test function
   function "=" (Left, Right: Dom_Node_Info) return Boolean;

   -- Merge two dominator nodes (updates Id)
   procedure Merge (Left, Right: in out Dom_Node_Info);

   -- Print out node info
   procedure Print (N : Dom_Node_Info);
   -- Print out node info
   procedure PrettyPrint (N : Dom_Node_Info);

   -- Print out the node type
   function Print_Nt (nt : Dom_Node_Type) return String;
   -- Print out link type
   function Print_Lt (lt : Dom_Link_Type) return String;

   -- Instantiation of the generic TREE package to DOM Tree
   package Dominator_Tree is
     new Tree (Dom_Node_Info,
               Dom_Node_Type,
               Dom_Link_Type,
               "="'Access,
               Merge'Access,
               Print'Access,
               PrettyPrint'Access,
               Print_Nt'Access,
               Print_Lt'Access);
   use Dominator_Tree;

   type Dom_Tree_Array is array (Natural range <>) of Tree_Type_Ptr;
   type Dom_Tree_Array_ptr is access all Dom_Tree_Array;

   -- Performs a dominator analysis on all CFGs to detect loops
   -- and construct all LCTs
   procedure Loop_Analysis;

   -- Performs a dominator analysis on a single CFG to detect loops
   -- and build the respective LCT
   procedure Single_Loop_Analysis;

   -- Initialize the static list of dominator trees
   procedure Init (num : Positive);

   -- Return the number of Dominator trees in the static list
   function Get_Num return Natural;

private

   -- Static list of dominator trees
   DomTrees : Dom_Tree_Array_ptr;
   -- Number of dominator trees in the list
   domTreeNum : Natural := 0;

end Dominator_Analysis;

