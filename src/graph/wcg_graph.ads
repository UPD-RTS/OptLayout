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
-- FILE NAME      : wcg_graph.ads
-- PACKAGE        : WCG_GRAPH spec
-- PURPOSE        : Representation of Weighted Call Graphs (WCG),
--                  LCT to WCG transformation and simple layout computation
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

package WCG_Graph is
   use Ada.Strings.Unbounded;

   -- Declaration of WCG-related exceptions
   Vertex_Error, Edge_Error, Overflow: exception;

   -- Default WCG name
   cfgPlaceHolder : constant String := "##########";
   -- Upper bound to the number of nodes in a WCG
   Connectivity : constant Positive := 1000;

   -- Holds the names of the procedures in a WCG node
   type WCG_Proc_Names is array (Integer range <>) of Unbounded_String;
   -- Pointer to the list of procedures merged into a WCG node
   type WCG_Proc_Names_ptr is access WCG_Proc_Names;

   -- Type definition for the WCG weight (freq of invocation)
   subtype Frequency is Integer range 0 .. 10000;


   -- A Node in the WCG
   type WCG_Vertex is record
      originalProcedure : Unbounded_String;
      -- Ordered list of procedures that have been merged in the node
      procedures : WCG_Proc_Names_ptr;
      -- Number of represented procedures
      size : Integer := 1;
      Marked : Boolean := False;  -- Has the Vertex been visited?
   end record;

   -- Pointer to a WCG node
   type WCG_Vertex_Ptr is access all WCG_Vertex;
   -- Array of WCG nodes
   type WCG_Vertex_Array is array (Positive range <>) of WCG_Vertex;
   -- Pointer to an array of WCG nodes
   type WCG_Vertex_Array_Ptr is access WCG_Vertex_Array;

   -- A WCG edge
   type WCG_Edge is record
      Weight  : Frequency := 0;  -- Weight assigned to the edge
      Defined : Boolean := False;  -- Is this edge defined in the WCG?
   end record;
   -- Pointer to a WCG edge
   type WCG_Edge_Ptr is access all WCG_Edge;
   -- Adjacency matrix for a WCG
   type WCG_Edges  is array (Positive range <>,
                             Positive range <>) of WCG_Edge;
   -- Pointer to a WCG adjacency matrix
   type WCG_Edges_Ptr is access WCG_Edges;
   -- List od WCG nodes
   type WCG_Vertex_List is array (Positive range <>) of WCG_Vertex;
   -- Pointer to a list of WCG nodes
   type WCG_Vertex_List_Ptr is access all WCG_Vertex_List;
   -- List of WCG edges
   type WCG_Edge_List is array (Positive range <>) of WCG_Edge;

   -- Print a WCG node info
   procedure Print_Vertex (v : WCG_Vertex);
   -- **UNUSED**
   -- Just call Print_Vertex
   procedure Print_Vertex_Name (v : WCG_Vertex);

   -- Type definition for a WCG
   type WCG (Max_Vertices : Positive) is
      record
         -- WCG Name
         Name : String (1..100);
         -- Max number of nodes
         Cardinality : Positive := Max_Vertices;
         -- Effective number of nodes
         Number : Integer := 0;
         -- WCG Nodes
         Vertices : WCG_Vertex_Array (1..Max_Vertices);
         -- WCG Edges
         Edges    : WCG_Edges  (1..Max_Vertices, 1..Max_Vertices);
      end record;
   -- Pointer to a WCG
   type WCG_ptr is access all WCG;

   -- Wrapper for the unchecked deallocation of a WCG
   procedure Unset (G : in out WCG_Ptr);

   --  Initializes a "progName" WCG with "num" nodes
   procedure Init (progName : in String; num : in Natural);

   -- Merge the WCG according to frequency information
   procedure WCG_Merging_Process;

   --  Returns the number of vertices in the WCG
   function Get_Vertex_Num (cfg: in WCG_Ptr) return Natural;

   --  Returns the index-th nodes in the WCG
   function Get_Vertex_By_Index (cfg : in WCG_Ptr; index : Positive; dim : Positive)
                                 return WCG_Vertex;

   --  Returns the list of all WCG nodes
   function Get_Vertices (Graph : in WCG) return WCG_Vertex_Array;

   --  Returns the list of all WCG nodes
   function Get_Vertices (Graph : in WCG_Ptr) return WCG_Vertex_Array;

   --  Returns the list of WCG nodes (accounts for erased nodes)
   function Get_Vertices_Exact (Graph : in WCG_Ptr) return WCG_Vertex_Array;

   -- Return the index of a node if it belings to the WCG,
   -- otherwise return the next available index
   function Index_Of (Graph : WCG;
                      Vertex: WCG_Vertex) return Positive;

   -- Set the number of nodes in the WCG to 0 (zero)
   procedure Clear (Graph : in out WCG);

   -- Add a node to the WCG
   procedure Add_Vertex (Graph  : in out WCG;
                         Vertex : in     WCG_Vertex);

   -- Connect two nodes in the WCG
   procedure Add_Edge (Graph: in out WCG;
                       Head, Tail: in WCG_Vertex;
                       Weight: in Frequency);

   -- Return the call frequency between two nodes
   -- Raise EDEG_ERROR is such edge does not exist
   function Weight_Of (Graph: WCG;
                       Head, Tail: WCG_Vertex) return Frequency;

   -- Return the list of successors of a node
   function Successors (Graph  : WCG;
                        Vertex : WCG_Vertex) return WCG_Vertex_List;

   -- Return the first successors of a node
   function First_Predecessor (Graph  : WCG;
                               Vertex : WCG_Vertex) return WCG_Vertex;

   -- Return the list of predecessors of a node
   function Predecessors (Graph  : WCG;
                          Vertex : WCG_Vertex) return WCG_Vertex_List;

   -- Return the number of predecessors of a node
   function Predecessors_Num (Graph  : WCG;
                        Vertex : WCG_Vertex) return Natural;

   -- Return the number of successors of a node
   function Successors_Num (Graph  : WCG;
                            Vertex : WCG_Vertex) return Natural;

   -- Return the root node of the WCG
   function Head (Graph : WCG) return WCG_Vertex;

   -- Unmark all nodesof a WCG
   procedure Clear_All_Marks (Graph: in out WCG);

   -- Check whether a specific node in the WCG is marked
   function Marked (Graph  : WCG;
                    Vertex : WCG_Vertex) return Boolean;

   -- Mark a specific node in the WCG
   procedure Mark_Vertex (Graph : in out WCG;
                          Vertex: in     WCG_Vertex);

   -- Setter for the WCG name
   procedure Set_Name (Graph: in out WCG; n : in String);

   -- Getter for the WCG name
   function Get_Name (Graph: in WCG) return String ;

   -- -- Return a string description of the WCG
   function Get_Graph_Info (Graph : in WCG) return String;

   -- Print the information of a WCG node
   procedure Print_Vertex_Info (v: WCG_Vertex) renames Print_Vertex;

   -- Returns the number of Nodes in the WCG
   function Get_Graph_Number (Graph : in WCG) return Natural;


   -- Returns the number of Nodes in the WCG
   function Get_Graph_Number (Graph_Ptr : in WCG_Ptr) return Natural;


   --  Checks whether the Node is already defined in the WCG
   function Node_Defined (Graph : WCG; Vertex: WCG_Vertex) return Boolean;

   -- Return the singleton WCG
   function Get_WCG return WCG_ptr;

   -- **UNUSED**
   -- procedure Export_To_Dot;

   -- Export the WCG to .DOT format and
   -- generate a graphical representation (.PDF)
   procedure Export_To_Dot_Single (Graph : WCG_ptr);




private

   WCG_Singleton : WCG_ptr;


end WCG_Graph;

