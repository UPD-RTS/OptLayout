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
-- FILE NAME      : cfg_graph.ads
-- PACKAGE        : CFG_GRAPH spec
-- PURPOSE        : Representation of control flow graph as weighted digraph
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
--with Graph;

package CFG_Graph is
   use Ada.Strings.Unbounded;

   -- Declaration of CFG-related exceptions
   Vertex_Error, Edge_Error, Overflow: exception;

   -- Default CFG name
   cfgPlaceHolder : constant String := "##########";
   -- Upper bound to the number of nodes in a CFG
   Connectivity : constant Positive := 1000;

   -- Basic block types declaration
   type BB_Type is (Root, Stub, Simple, Leaf);
   -- Link types declaration
   type Link_Type is (Call, Taken, Nottaken, Undef);
   -- Edge weight (currently unused)
   subtype Edge_Weight is Integer range 0 .. 10;

   -- Type definition of a CFG node
   type CFG_Vertex is record
      -- Basic block type
      typ : BB_Type := Simple;
      -- Identifier for the BB
      BB : String(1..8);--String (1..10);
      -- Handles stub nodes duplication
      alias : Natural := 0;
      -- Identifier for the CFG to which the BB belongs
      cfgId : String (1..10) := "***self***";
      -- Has the Vertex been visited?
      Marked : Boolean := False;
   end record;
   -- Type definition of pointer to CFG nodes
   type CFG_Vertex_Ptr is access all CFG_Vertex;
   -- Type definition of list of nodes
   type CFG_Vertex_Array is array (Positive range <>) of CFG_Vertex;
   -- Type definition of pointer to list of nodes
   type CFG_Vertex_Array_Ptr is access CFG_Vertex_Array;

   -- Type definition of a CFG edge
   type CFG_Edge is record
      -- Edge type (e.g.: simple, call, taken, nottaken, etc)
      Edge_typ : Link_Type := Undef;
      -- Weight assigned to the edge
      Weight  : Edge_Weight := 0;
      -- Is this edge in the graph?
      Defined : Boolean := False;
   end record;
   -- Type definition of pointer to CFG edges
   type CFG_Edge_Ptr is access all CFG_Edge;
   -- Adjacency matrix for the CFG
   type Matrix_Type  is array (Positive range <>,
                               Positive range <>) of CFG_Edge;
   -- Pointer to adjacency matrix
   type Matrix_Type_Ptr is access Matrix_Type;

   -- Duplicate tyoe definition (?)
   type Vertex_List is array (Positive range <>) of CFG_Vertex;
   -- Duplicate tyoe definition (?)
   type Vertex_List_Ptr is access all Vertex_List;
   -- Duplicate tyoe definition (?)
   type Edge_List is array (Positive range <>) of CFG_Edge;

   -- Equality test for CFG edges
   function "=" (gA, gB : CFG_Vertex) return Boolean;

   -- Print out a CFG node
   procedure Print_Vertex (v : CFG_Vertex);
   -- Print out just the name of CFG node
   procedure Print_Vertex_Name (v : CFG_Vertex);

   -- CFG typoe definition
   type CFG_Type (Max_Vertices : Positive) is
      record
         -- Number of nodes (determining the dimension of the adjacency matrix)
         Cardinality : Positive := Max_Vertices;
         Number : Natural := 0;
         Id : String (1..10);
         Name : String (1..100);
         Is_Main : Boolean := False;
         Vertices : CFG_Vertex_Array (1..Max_Vertices);
         Edges    : Matrix_Type  (1..Max_Vertices, 1..Max_Vertices);
      end record;
   type CFG_ptr is access all CFG_Type;

   -- Wrapper for CFG unchecked deallocation
   procedure Unset (G : in out CFG_Ptr);

   -- Type definition of list of CFGs
   type CFG_Array is array (Natural range <>) of CFG_ptr;
   -- Type definition of pointer to CFG lists
   type CFG_Array_ptr is access all CFG_Array;

   -- Type definition for a dynamically growing list of CFGs
   type CFG_List is record
      Name : Unbounded_String;
      Items : CFG_Array_ptr := null;
      Size : Natural := 0;
   end record;

   -- Initializes the list of involved CFGs (one for each subprogram)
   procedure Init (progName : in String; cfg_num : in Natural);

   -- Add a CFG to the CFG list
   procedure Add (new_cfg : CFG_ptr);

   -- Get the INdex-th CFG form the list
   function Get_CFG (Index : Natural) return CFG_Ptr;

   -- Return the number of CFGs currently in memory
   -- (i.e. the CFG list size)
   function Get_CFG_Num return Natural;

   --  Return the number of nodes in a CFG
   function Get_Vertex_Num (cfg: in CFG_Ptr) return Natural;

   -- **UNIMPLEMENTED**
   -- Retrieve a node of the CFG by the node ID
   function Get_Vertex_By_Id (cfg : in CFG_Ptr; vertex_id : String)
                              return CFG_Vertex_Ptr;

   -- Retrieve a node of the CFG by its index
   function Get_Vertex_By_Index (cfg : in CFG_Ptr; index : Positive; dim : Positive)
                                 return CFG_Vertex;

   -- Return the set of CFG nodes
   function Get_Vertices (Graph : in CFG_Type) return CFG_Vertex_Array;
   -- Return the set of CFG nodes
   function Get_Vertices (Graph : in CFG_Ptr) return CFG_Vertex_Array;

   -- Return the actual set of CFG nodes (accounts for deleted nodes)
   function Get_Vertices_Exact (Graph : in CFG_Ptr) return CFG_Vertex_Array;

   -- Return the index of a node if it belings to the CFG,
   -- otherwise return the next available index
   function Index_Of (Graph : CFG_Type;
                      Vertex: CFG_Vertex) return Positive;

   -- Set the number of nodes in the CFG to 0 (zero)
   procedure Clear (Graph : in out CFG_Type);

   -- Add a node to the CFG
   procedure Add_Vertex (Graph  : in out CFG_Type;
                         Vertex : in     CFG_Vertex);

   -- Connect two nodes in the CFG
   procedure Add_Edge (Graph: in out CFG_Type;
                       Head, Tail: in CFG_Vertex;
                       Link : Link_Type;
                       Weight: in Edge_Weight);

   -- Return the weight of the edge between two nodes
   -- Raise EDEG_ERROR is such edge does not exist
   function Weight_Of (Graph: CFG_Type;
                       Head, Tail: CFG_Vertex) return Edge_Weight;

   -- Return the number of successors for a CFG node
   function Successors_Num (Graph  : CFG_Type;
                            Vertex : CFG_Vertex) return Natural;

   -- Return the list of successors for a CFG node
   function Successors (Graph  : CFG_Type;
                        Vertex : CFG_Vertex) return Vertex_List;

   -- Return the number of predecessor for a CFG node
   function Predecessors_Num (Graph  : CFG_Type;
                              Vertex : CFG_Vertex) return Natural;

   -- Return the first successor for a CFG node
   function First_Predecessor (Graph  : CFG_Type;
                               Vertex : CFG_Vertex) return CFG_Vertex;

   -- Return the list of predecessors for a CFG node
   function Predecessors (Graph  : CFG_Type;
                          Vertex : CFG_Vertex) return Vertex_List;

   -- Return the root node of a CFG
   function Head (Graph : CFG_Type) return CFG_Vertex;

   -- Unmark all nodes of a CFG
   procedure Clear_All_Marks (Graph: in out CFG_Type);

   -- Check whether a specific node in the CFG is marked
   function Marked (Graph  : CFG_Type;
                    Vertex : CFG_Vertex) return Boolean;

   -- Mark a specific node in the CFG
   procedure Mark_Vertex (Graph : in out CFG_Type;
                          Vertex: in     CFG_Vertex);

   -- Setter for the CFG name
   procedure Set_Name (Graph: in out CFG_Type; n : in String);

   -- Getter for the CFG name
   function Get_Name (Graph: in CFG_Type) return String ;

   -- Setter for the CFG ID
   procedure Set_Id (Graph: in out CFG_Type; n : in String);

   -- Getter for the CFG ID
   function Get_Id (Graph: in CFG_Type) return String;

   -- Set a CFG as the CFG of the root subprogram
   procedure Set_Is_Main (Graph: in out CFG_Type);

   -- Check whether the CFG is that of the root subprogram
   function Get_Is_Main (Graph: in CFG_Type) return Boolean;

   -- Return a string description of the graph
   function Get_Graph_Info (Graph : in CFG_Type) return String;

   -- Print the information of a CFG node
   procedure Print_Vertex_Info (v: CFG_Vertex) renames Print_Vertex;

   -- Get the actual number of nodes in the CFG
   function Get_Graph_Number (Graph : in CFG_Type) return Natural;
   -- Get the actual number of nodes in the CFG
   function Get_Graph_Number (Graph_Ptr : in CFG_Ptr) return Natural;

   -- Check whether the node is defined in the CFG
   -- (similar to Index_Of)
   function Node_Defined (Graph : CFG_Type;
                          Vertex: CFG_Vertex) return Boolean;

   -- Export CFG utilities

   -- Export the super-CFG to .DOT format and
   -- generate a graphical representation (.PDF)
   procedure Export_To_Dot;

   -- Export the CFG to .DOT format and
   -- generate a graphical representation (.PDF)
   procedure Export_To_Dot_Single (Graph : CFG_ptr);

private

   -- List of the CFG of all the involved subprograms
   CFGs : CFG_List;


end CFG_Graph;

