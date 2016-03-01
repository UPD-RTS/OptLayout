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
-- FILE NAME      : wcg_graph.adb
-- PACKAGE        : WCG_GRAPH body
-- PURPOSE        : Representation of Weighted Call Graphs (WCG),
--                  LCT to WCG transformation and simple layout computation
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Global, Utilities;
use Utilities;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded.Text_IO;
with Interactions; use Interactions;
package body WCG_Graph is

   -- Unchecked deallocation of nodes
   procedure Free_WCG is new  Ada.Unchecked_Deallocation (WCG, WCG_Ptr);
   ---------
   -- "=" --
   ---------

   function "=" (gA, gB : WCG_Vertex) return Boolean is
      use Utilities;
   begin
      return (UTrim(gA.originalProcedure) = UTrim(gB.originalProcedure));
   end "=";

   ------------------
   -- Print_Vertex --
   ------------------

   procedure Print_Vertex (v : WCG_Vertex) is
      procNames : WCG_Proc_Names (1..v.size);
      procName : Unbounded_String;
   begin
      procNames := v.procedures.all;
--        Put_Line ("++++++++++++++++++++++++++");
--        Put_Line ("WCG Node ");
      for I in 1..v.size loop
         procName := procNames (I);
         Put_Line ("- " & Utilities.UTrim(procName));
      end loop;
--        Put_Line ("++++++++++++++++++++++++++");
   end Print_Vertex;

   -----------------------
   -- Print_Vertex_Name --
   -----------------------

   procedure Print_Vertex_Name (v : WCG_Vertex) is
   begin
      Print_Vertex (v);
   end Print_Vertex_Name;

   -----------
   -- Unset --
   -----------

   procedure Unset (G : in out WCG_Ptr) is
      temp : WCG_ptr;
   begin
      temp := G;
      Free_WCG (temp);
   end Unset;

   ----------
   -- Init --
   ----------

   procedure Init (progName : in String; num : in Natural) is
   begin
      WCG_Singleton := new WCG (num);
      Set_name (WCG_Singleton.all, progName);
   end Init;

   --------------------
   -- Get_Vertex_Num --
   --------------------

   function Get_Vertex_Num (cfg: in WCG_Ptr) return Natural is
   begin
      return cfg.all.Number;
   end Get_Vertex_Num;

   -------------------------
   -- Get_Vertex_By_Index --
   -------------------------

   function Get_Vertex_By_Index
     (cfg : in WCG_Ptr;
      index : Positive;
      dim : Positive)
      return WCG_Vertex
   is
      nList : WCG_Vertex_Array (1..dim);
   begin
      nList := Get_vertices_Exact(cfg) ;
      return nList(index);
   end Get_Vertex_By_Index;

   -------------------------
   -- Get_Vertex_By_Index --
   -------------------------

--     function Get_Vertex_By_Index
--       (cfg : in WCG_Ptr;
--        index : Positive;
--        dim : Positive)
--        return WCG_Vertex_Ptr
--     is
--        nList : WCG_Vertex_Array (1..dim);
--     begin
--        nList := Get_vertices_Exact(cfg) ;
--        return nList(index);
--     end Get_Vertex_By_Index;

   ------------------
   -- Get_Vertices --
   ------------------

   function Get_Vertices (Graph : in WCG) return WCG_Vertex_Array is
   begin
      return Graph.Vertices;
   end Get_Vertices;

   ------------------
   -- Get_Vertices --
   ------------------

   function Get_Vertices (Graph : in WCG_Ptr) return WCG_Vertex_Array is
   begin
      return Graph.all.Vertices;
   end Get_Vertices;

   ------------------------
   -- Get_Vertices_Exact --
   ------------------------

   function Get_Vertices_Exact (Graph : in WCG_Ptr) return WCG_Vertex_Array is
   begin
      return Graph.Vertices (1..Graph.Number);
   end Get_Vertices_Exact;

   --------------
   -- Index_Of --
   --------------

   function Index_Of
     (Graph : WCG;
      Vertex: WCG_Vertex)
      return Positive
   is
   begin
      for I in Graph.Vertices'First..Graph.Number loop
         if Graph.Vertices(I)=Vertex then
            return I;
         end if;
      end loop;
      return Graph.Number+1;  --  if not there, return next index
   end Index_Of;

   -----------
   -- Clear --
   -----------

   procedure Clear (Graph : in out WCG) is
   begin
      Graph.Number := 0;
   end Clear;

   ----------------
   -- Add_Vertex --
   ----------------

   procedure Add_Vertex
     (Graph  : in out WCG;
      Vertex : in     WCG_Vertex)
   is
      -- checks whether the node has been already added
      Index: Positive := Index_Of (Graph, Vertex);
   begin
      if Index <= Graph.Number then
         --  already there
         null;
      elsif Index > Graph.Max_Vertices then
         Put_Line ("No more space");
         raise Overflow;       --  no space for any more
      else
         if Global.tbshootWCG then
            Put_line (" into vertex #" & Integer'Image(Index));
         end if;
         Graph.Vertices (Index) := Vertex;--(Vertex, False);
         Graph.Number := Index;
         --  no edges for this vertex (yet)
         for Destination in Graph.Vertices'Range loop
            Graph.Edges(Index,Destination).Defined := False;
         end loop;
      end if;
   end Add_Vertex;

   --------------
   -- Add_Edge --
   --------------

   procedure Add_Edge
     (Graph: in out WCG;
      Head, Tail: in WCG_Vertex;
      Weight: in Frequency)
   is
      use Global;
      oldWeight: Frequency;
      H: Positive := Index_Of (Graph, Head);
      T: Positive := Index_Of (Graph, Tail);
   begin
      if (Global.E_M = VERBOSE or tbshootWCG) then
         Put_Line ("Head: " & Integer'Image(H) & " Tail: " & Integer'Image(T)
                   & "(" & Integer'Image(Weight) & ")");
      end if;
      if H>Graph.Number or T>Graph.Number then
         Put_Line ("Graph.Number is " & INteger'Image(graph.Number));
         raise Edge_Error;
      else
         -- If already defined just add the frequency
         if Graph.Edges (H,T).Defined then
            oldWeight := Graph.Edges (H,T).Weight;
            Graph.Edges (H,T).Weight := oldWeight + Weight;
            if tbshootWCG then
               Put_Line ("Updating wieght: " & INteger'Image( oldWeight) &
                         " -> " & Integer'Image (Graph.Edges (H,T).Weight));
            end if;
         else
            Graph.Edges (H,T) := (Weight, True);
         end if;
      end if;
   end Add_Edge;

   ---------------
   -- Weight_Of --
   ---------------

   function Weight_Of
     (Graph: WCG;
      Head, Tail: WCG_Vertex)
      return Frequency
   is
      use Global;
      H: Positive := Index_Of (Graph, Head);
      T: Positive := Index_Of (Graph, Tail);
   begin
      if H>Graph.Number or T>Graph.Number then
         raise Edge_Error;
      else
         return Graph.Edges(H,T).Weight;
      end if;
   end Weight_Of;

   ----------------
   -- Successors --
   ----------------

   function Successors
     (Graph  : WCG;
      Vertex : WCG_Vertex)
      return WCG_Vertex_List
   is
      Index: Positive := Index_Of (Graph, Vertex);
      List: WCG_Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
   begin
      for Destination in Graph.Vertices'First..Graph.Number loop
         if Graph.Edges (Index, Destination).Defined then
            N := N+1;
            List(N) := Graph.Vertices(Destination);
         end if;
      end loop;
      return List(1..N);
   end Successors;

   -----------------------
   -- First_Predecessor --
   -----------------------

   function First_Predecessor
     (Graph  : WCG;
      Vertex : WCG_Vertex)
      return WCG_Vertex
   is
      Index: Positive := Index_Of (Graph, Vertex);
      N: Natural := 0;
      found : Boolean := False;
      firstPred : WCG_Vertex;
   begin
      --Put("Predecessors:");
      for Origin in Graph.Edges'Range loop
         if Graph.Edges (Origin, Index).Defined then
            firstPred := Graph.Vertices(Origin);
            if (not found) then
               found := True;
--              else
--                 Put("*** PROBLEMA ************");
            end if;
         end if;
      end loop;
      return firstPred;
   exception
      when Error:others =>
         Put_Line("No predecessors for this node!");
         raise;
   end First_Predecessor;

   ------------------
   -- Predecessors --
   ------------------

   function Predecessors
     (Graph  : WCG;
      Vertex : WCG_Vertex)
      return WCG_Vertex_List
   is
      Index: Positive := Index_Of (Graph, Vertex);
      List: WCG_Vertex_List (1..Graph.Number);  -- max possible indegree
      N: Natural := 0;
   begin
      --Put("Predecessors:");
      for Origin in Graph.Edges'Range loop
         if Graph.Edges (Origin, Index).Defined then
            --Put("* " & Integer'Image(N));
            N := N+1;
            --Print_Vertex_Info(Graph.Vertices(Origin));
            List(N) := Graph.Vertices(Origin);
         end if;
      end loop;
      return List(1..N);
   end Predecessors;

   ----------------------
   -- Predecessors_Num --
   ----------------------

   function Predecessors_Num
     (Graph  : WCG;
      Vertex : WCG_Vertex)
      return Natural
   is
      Index: Positive := Index_Of (Graph, Vertex);
      --List: Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
   begin
      --Put("Predecessors num:");
      for Destination in Graph.Edges'Range loop
         if Graph.Edges (Destination, Index).Defined then
            N := N+1;
            --Print_Vertex_Info(Graph.Vertices(Index).Info);
            --List(N) := Graph.Vertices(Index);
         end if;
      end loop;
      --Put_Line("*num " & Integer'Image(N) & " ");
      return N;
   end Predecessors_Num;

   --------------------
   -- Successors_Num --
   --------------------

   function Successors_Num
     (Graph  : WCG;
      Vertex : WCG_Vertex)
      return Natural
   is
      Index: Positive := Index_Of (Graph, Vertex);
      List: WCG_Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
   begin
      for Destination in Graph.Vertices'First..Graph.Number loop
         if Graph.Edges (Index, Destination).Defined then
            N := N+1;
            List(N) := Graph.Vertices(Destination);
         end if;
      end loop;
      --Put("% " & Integer'Image(N) & " ");
      return N;
   end Successors_Num;

   ----------
   -- Head --
   ----------

   function Head (Graph : WCG) return WCG_Vertex is
   begin
      return Graph.Vertices(1);
   end Head;

   ---------------------
   -- Clear_All_Marks --
   ---------------------

   procedure Clear_All_Marks (Graph: in out WCG) is
   begin
      for I in Graph.Vertices'First..Graph.Number loop
         Graph.Vertices(I).Marked := False;
      end loop;
   end Clear_All_Marks;

   ------------
   -- Marked --
   ------------

   function Marked
     (Graph  : WCG;
      Vertex : WCG_Vertex)
      return Boolean
   is
      Index: Positive := Index_Of (Graph, Vertex);
   begin
      if Index > Graph.Number then
         raise Vertex_Error;
      else
         return (Graph.Vertices(Index).Marked);
      end if;
   end Marked;

   -----------------
   -- Mark_Vertex --
   -----------------

   procedure Mark_Vertex
     (Graph : in out WCG;
      Vertex: in     WCG_Vertex)
   is
      Index: Positive := Index_Of (Graph, Vertex);
   begin
      if Index > Graph.Number then
         raise Vertex_Error;
      else
         Graph.Vertices(Index).Marked := True;
      end if;
   end Mark_Vertex;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Graph: in out WCG; n : in String) is
      use Ada.Strings.Fixed;
   begin
      Ada.Strings.Fixed.Move (n, Graph.Name, Ada.Strings.Right,
                              Ada.Strings.Left,Ada.Strings.Space);
   end Set_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Graph: in WCG) return String is
   begin
      return Graph.Name;
   end Get_Name;



   --------------------
   -- Get_Graph_Info --
   --------------------

   function Get_Graph_Info (Graph : in WCG) return String is
   begin
      return ("WCG Info: " & Graph.Name & " Child: " &Integer'Image(Graph.Number));
   end Get_Graph_Info;

   ----------------------
   -- Get_Graph_Number --
   ----------------------

   function Get_Graph_Number (Graph : in WCG) return Natural is
   begin
      return Graph.Number;
   end Get_Graph_Number;

   ----------------------
   -- Get_Graph_Number --
   ----------------------

   function Get_Graph_Number (Graph_Ptr : in WCG_Ptr) return Natural is
   begin
      return Get_Graph_Number(Graph_Ptr.all);
   end Get_Graph_Number;

   ------------------
   -- Node_Defined --
   ------------------

   function Node_Defined
     (Graph : WCG;
      Vertex: WCG_Vertex)
      return Boolean
   is
   begin
      for I in Graph.Vertices'First..Graph.Number loop
         if Graph.Vertices(I)=Vertex then
            return true;
         end if;
      end loop;
      return false;
   end Node_Defined;

   -------------
   -- Get_WCG --
   -------------

   function Get_WCG return WCG_ptr
   is
   begin
      return WCG_Singleton;
   end Get_WCG;


   --------------------------
   -- Select more frequent --
   --------------------------
   procedure Find_Frequent_Call (index1, index2 : in out Integer)
   is
      Graph : WCG := WCG_Singleton.all;
      maxWeight : Natural := 0;
   begin
      for I in 1..WCG_Singleton.Number loop
         for J in 1..WCG_Singleton.Number loop
            if Graph.Edges(I,J).Defined and then
              Graph.Edges(I,J).Weight>maxWeight then
               index1:=I;
               index2:=J;
               maxWeight := Graph.Edges(I,J).Weight;
            end if;
         end loop;
      end loop;

   end Find_Frequent_Call;

   procedure Add_Procedures (node : in out WCG_Vertex; node2 : in WCG_Vertex)
   is
      procN1_ptr : WCG_Proc_Names_ptr := node.procedures;
      procN2_ptr : WCG_Proc_Names_ptr := node2.procedures;
      newProcPtr : WCG_Proc_Names_ptr;
      sizeN1 : Integer := node.size;
      sizeN2 : Integer := node2.size;
   begin
      newProcPtr := new WCG_Proc_Names (1..sizeN1+sizeN2);
      for I in 1..sizeN1 loop
         newProcPtr(I):=procN1_ptr(I);
      end loop;
      for J in 1..sizeN2 loop
         newProcPtr(sizeN1+J):=procN2_ptr(J);
      end loop;
      node.size := node.size + node2.size;

   end Add_Procedures;


   procedure Merge_Nodes (index1, index2 : in Integer)
   is
      use Global;
      n1, n2 : WCG_Vertex;
      sizeN1, sizeN2 : Integer;
      mergedNode : WCG_Vertex;
      newProcPtr : WCG_Proc_Names_ptr;
      Graph : WCG_ptr := Get_WCG;
   begin

      n1 := Get_Vertex_By_Index(cfg   => WCG_Singleton,
                                index => index1,
                                dim   => WCG_Singleton.Number);
      n2 := Get_Vertex_By_Index(cfg   => WCG_Singleton,
                                index => index2,
                                dim   => WCG_Singleton.Number);
      if tbshootWCG then
         Put_Line ("Merging nodes [" & Utrim(n1.originalProcedure) & " "
                   & integer'Image(n1.size) & "-" & UTrim(n2.originalProcedure)
                   & " " & integer'Image(n2.size) & "]");
      end if;

      -- Add the procedures of "n2" to the subprogram list of "n1"
      sizeN1 := n1.size;
      sizeN2 := n2.size;
      newProcPtr := new WCG_Proc_Names (1..sizeN1+sizeN2);
      if tbshootWCG then
         Put_Line ("new size:" & Integer'Image(sizeN1+sizeN2));
      end if;
      newProcPtr (1..sizeN1) := n1.procedures (1..sizeN1);
      newProcPtr (sizeN1+1..sizeN1+sizeN2) := n2.procedures (1..sizeN2);
      if tbshootWCG then
         Put_Line ("NewProcPtr computed");
      end if;
      -- Remove the link
      Graph.Edges (index1, index2).Defined := False;
      -- Update all incoming and outgoing edges of n1
      for Destination in Graph.Vertices'First..Graph.Number loop
         if Graph.Edges (index2, Destination).Defined and then
           Destination/= index1 then
            if tbshootWCG then
               Put_line ("...updating a destination");
            end if;
            Graph.Edges (index1, Destination).Defined := True;
            Graph.Edges (index1, Destination).Weight :=
              Graph.Edges (index2, Destination).Weight;
            Graph.Edges (index2, Destination).Defined := False;
         end if;
      end loop;
      if tbshootWCG then
         Put_Line ("destinations updated:");
      end if;
      for Origin in Graph.Vertices'First..Graph.Number loop
         if Graph.Edges (Origin, index2).Defined and then
           Origin /= index1 then
            if tbshootWCG then
               Put_line ("...updating an origin");
            end if;

            Graph.Edges (Origin, index1).Defined := True;
            Graph.Edges (Origin, index1).Weight :=
              Graph.Edges (Origin, index2).Weight;
            Graph.Edges (Origin, index2).Defined := False;
         end if;
      end loop;
      if tbshootWCG then
         Put_Line ("Origins updated");
      end if;

      -- Update the node procedures and size
      Graph.Vertices (index1).procedures := newProcPtr;
      Graph.Vertices (index1).size := sizeN1+sizeN2;

      if tbshootWCG then
         Put_Line ("Everything updated");
      end if;
   end Merge_Nodes;



   -------------------------------------------------
   -- Merge WGC nodes according to call frequency --
   -------------------------------------------------
   procedure WCG_Merging_Process
   is
      use Global;
      iterations : Integer := WCG_Singleton.all.Number;
      ind1, ind2 : Integer := 0;
      node : WCG_Vertex;
   begin
      for I in WCG_Singleton.Vertices'Range loop
         node := WCG_Singleton.Vertices(I);
         node.procedures := new WCG_Proc_Names (1..1);
         node.procedures(1) := node.originalProcedure;
         WCG_Singleton.Vertices(I) := node;
      end loop;

      while iterations > 1 loop
         Find_Frequent_Call (ind1, ind2);
         Merge_Nodes (ind1, ind2);
         iterations := iterations - 1;
      end loop;
      -- At this point we have a single big node
   end WCG_Merging_Process;

   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- ++ Export WCG utilities
   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   -------------------
   -- Export_To_Dot --
   -------------------

   procedure Export_To_Dot
   is
   begin
      --  Generated stub: replace with real body!
      --pragma Compile_Time_Warning (True, "Export_To_Dot unimplemented");
      raise Program_Error with "Unimplemented procedure Export_To_Dot";
   end Export_To_Dot;

   --------------------------
   -- Export_To_Dot_Single --
   --------------------------

   procedure Export_To_Dot_Single (Graph : WCG_ptr) is
   use Global;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      currWCG : WCG_ptr;
      WCG_Node, From, To: WCG_Vertex;
      Entry_point : Boolean := True;
      Child_Number : Natural := 0;
   begin
      Create (Output, Out_File, Trim(Get_Name(Graph.all)) & "_WCG.dot");
      Put_line(Output, "/* DOT Graph description");
      Put_line(Output, " * Generated by OptLayout");
      Put (Output, " * Main program: ");
      Put_Line (Output, Global.Program);
      Put_line(Output, " */");
      Put_line(Output, "digraph WCG {");
      Put_Line(Output, "size = ""8,11""" );
      Put_Line(Output, "labelloc=t" );
      --Put_Line(Output, "ratio = ""fill""" );
      currWCG := Graph;
      --  Print Nodes
      --  WARNING: Call nodes should be handled separatedly
--        WCG_Node := Head (currWCG.all);
--        Put_Line (Output, "CIAO" & "[shape=box]" &
--                  "[label=""" & To_String(Get_Name(WCG_Node.originalProcedure))"];");
--        Mark_Vertex (Graph  => currWCG.all,
--                     Vertex => WCG_Node);
--        Export_To_Dot_Rec (Output, WCG_Node, currWCG.all);

      for I in Graph.Vertices'Range loop
         WCG_Node := Graph.Vertices(I);
         Put_Line (Output, UTrim(WCG_Node.originalProcedure) &
                   " [shape=circle]" & "[label=""" & Trim(I) & """];");
--                   " [shape=circle]" & "[label=""" & UTrim(WCG_Node.originalProcedure) & """];");
      end loop;
      for J in Graph.Edges'Range loop
         for K in Graph.Edges'Range loop
            if Graph.Edges(J,K).Defined then
               Put_Line (output, UTrim(Graph.Vertices(J).originalProcedure)
                         & " -> " & UTrim(Graph.Vertices(k).originalProcedure)
                         & "[label=""" & Integer'Image(Graph.Edges(J,K).Weight) & """];");
            end if;
         end loop;
      end loop;
      Put_line(Output, "label = ""Weighted Call Graph for:  " &
               UTrim(Graph.Vertices(1).originalProcedure) & --UTrim(Global.Program) &
               "\nGenerated by the OPTLayout tool"";");

      Put_line(Output, "subgraph Legend {");
      Put_line(Output, "rank = sink;");
      Put (Output, "A [shape=box, label=""");
      for I in Graph.Vertices'Range loop
         WCG_Node := Graph.Vertices(I);
        Put (Output, Trim(I) & " - " & UTrim(WCG_Node.originalProcedure) & "\l");
      end loop;
      Put_line (Output, """];");

      Put_line(Output, "}");
      Put_line(Output, "}");
      Close (Output);
      --Put_LIne("Now executing the dot2pdf invocation");
      Execute_Dot (Trim(Get_Name(Graph.all)) & "_WCG");
   end Export_To_Dot_Single;

end WCG_Graph;
