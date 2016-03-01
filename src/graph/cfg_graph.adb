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
-- FILE NAME      : cfg_graph.adb
-- PACKAGE        : CFG_GRAPH body
-- PURPOSE        : Representation of control flow graph as weighted digraph
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Global, Utilities;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Fixed;
with Interactions; use Interactions;


package body CFG_Graph is


   -- Unchecked deallocation of CFG
   procedure Free_CFG is new  Ada.Unchecked_Deallocation (
                                                      CFG_type,
                                                          CFG_Ptr);
   -- Unchecked deallocation of nodes
   procedure Free_Vertex is new  Ada.Unchecked_Deallocation (
                                                      CFG_Vertex,
                                                             CFG_Vertex_Ptr);

   -- Unchecked deallocation of edges
   procedure Free_Edge is new  Ada.Unchecked_Deallocation (
                                                        CFG_Edge,
                                                         CFG_Edge_Ptr);


   procedure Unset (G : in out CFG_Ptr) is
      temp : CFG_Ptr;
   begin
      temp := G;
      --Put_Line("Erasing CFG ");-- & Get_Name(G.all));
      Free_CFG (temp);
   end Unset;

--  private
--     CFGs : Graph_Array_Access;
--     CFG_Array : Graph_Array (1..30) := (others => null);

   overriding function "="(gA, gB : in CFG_Vertex) return Boolean is
   --function "="(gA, gB : in Vertex_Type) return Boolean is
   begin
      return (gA.BB = gB.BB and gA.alias = gB.alias);
   end "=";


   procedure Init (progName : in String; cfg_num : in Natural) is
      --cfgArray : aliased CFG_Array (cfg_num);
      use Global;
      U_String : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String(progName);
   begin
      CFGs.Name := U_String; --Ada.Strings.Unbounded.To_Unbounded_String(progName);
      if (not Global.PName_Spec) then
         Program := U_String;
      end if;

      CFGs.Items := new CFG_Array(1..cfg_num);
   end Init;

   procedure Add (new_cfg : CFG_ptr) is
   begin
      CFGs.Size := CFGs.Size + 1;
      CFGs.Items (CFGs.Size) := new_cfg;
   end Add;

   function Get_CFG (Index : Natural) return CFG_Ptr is
   begin
      if (Index <= CFGs.Size) then
         return CFGs.Items (Index);
      else
         return null;
      end if;
   end Get_CFG;

   --  Returns the number of CFGs currently in memory
   function Get_CFG_Num return Natural is
   begin
      return CFGs.Size;
   end Get_CFG_Num;

   function Get_Vertex_By_Id (cfg : in CFG_Ptr; vertex_id : String)
                              return CFG_Vertex_Ptr is
   begin
      --cfg.all
      return null;
   end Get_Vertex_By_Id;

   function Get_Vertex_By_Index (cfg : in CFG_Ptr; index : Positive; dim : Positive)
                                 return CFG_Vertex is
      --curCFG : CFG_Graph := cfg.all;
      --max : Positive := curCFG.Get_Vertex_Num;
      Vert : CFG_Vertex_Array (1..dim);
   begin
      --Put_Line("GetByIndex" & Integer'Image(index) & "-dim: " & Integer'Image(dim));
      --Vert := Get_vertices(cfg.all) ;
      Vert := Get_vertices_Exact(cfg) ;
      --Put_line("yes");
      return Vert(index);
   end Get_Vertex_By_Index;




   function Get_Vertex_Num (cfg: in CFG_Ptr) return Natural is
   begin
--      return cfg.Vertices'Length;
      return cfg.all.Number;
   end Get_Vertex_Num;


   procedure Clear (Graph : in out CFG_Type) is
   begin
      Graph.Number := 0;
   end Clear;

--     function Get_CFG_Number (index : in Positive) return Natural is
--        ptr : CFG_ptr;
--        --cfg : CFG_Type := cfg_ptr.all;
--     begin
--        ptr := Get_CFG (index);
--        return CFG.Get_Graph_Number(cfg_ptr);
--     end Get_CFG_Number;


   function V_Type_Pretty_Printer (t : BB_Type) return String is
   begin
      if (t = Simple) then
         return "Simple";
      elsif (t = Root) then
         return "Root";
      elsif (T = Stub) then
         return "Stub";
      else
         return "Leaf";
      end if;

   end V_Type_Pretty_Printer;

   function E_Type_Pretty_Printer (l : Link_Type) return String is
   begin
      if (l = Call) then
         return "Call";
      elsif (l = Taken) then
         return "Taken";
      else
         return "Nottaken";
      end if;

   end E_Type_Pretty_Printer;


   procedure Print_Vertex (v : CFG_Vertex) is
   begin
      Put_line ("Vertex: " & v.BB & " Type: " & V_Type_Pretty_Printer(v.typ));

   end Print_Vertex;

   procedure Print_Vertex_Name (v : CFG_Vertex) is
   begin
      Put ("Node: " & v.BB & " ");
   end Print_Vertex_Name;


   function To_String (str : String) return String renames Utilities.Trim;

--     is
--     begin
--        return Ada.Strings.Fixed.Trim(str,Ada.Strings.Right);
--     end To_String;


   function Index_Of (Graph : CFG_Type;
                      Vertex: CFG_Vertex) return Positive is
   begin
      for I in Graph.Vertices'First..Graph.Number loop
         if Graph.Vertices(I)=Vertex then
            return I;
         end if;
      end loop;
      return Graph.Number+1;  --  if not there, return next index
   end Index_Of;


   function Node_Defined (Graph : CFG_Type;
                          Vertex: CFG_Vertex) return Boolean is
   begin
      for I in Graph.Vertices'First..Graph.Number loop
         if Graph.Vertices(I)=Vertex then
            return true;
         end if;
      end loop;
      return false;
   end Node_Defined;


   procedure Add_Vertex (Graph  : in out CFG_Type;
                         Vertex : in     CFG_Vertex) is
      Index: Positive := Index_Of (Graph, Vertex);
   begin
      --Put_line(Integer'Image(Index) & "-" & Integer'Image(Graph.Number));
      if Index <= Graph.Number then
         --raise Vertex_Error;   --  already there
         --  Create an alias node
         --  Handled while creating the node
         null;

      elsif Index > Graph.Max_Vertices then
         Put_Line ("No more space");
         raise Overflow;       --  no space for any more
      else
         Graph.Vertices (Index) := Vertex;--(Vertex, False);
         Graph.Number := Index;
         --  no edges for this vertex (yet)
         for Destination in Graph.Vertices'Range loop
            Graph.Edges(Index,Destination).Defined := False;
         end loop;
      end if;
   end Add_Vertex;


   procedure Add_Edge (Graph: in out CFG_Type;
                       Head, Tail: CFG_Vertex;
                       Link : Link_Type; Weight: Edge_Weight) is
      use Global;
      H: Positive := Index_Of (Graph, Head);
      T: Positive := Index_Of (Graph, Tail);
   begin
        if (Global.E_M = VERBOSE) then
           Put_Line ("Head: " & Integer'Image(H) & "Tail: " & Integer'Image(T));
        end if;

      if H>Graph.Number or T>Graph.Number then
         raise Edge_Error;
      else
         Graph.Edges (H,T) := (Link , Weight, True);
      end if;
   end Add_Edge;

   function Weight_Of (Graph: CFG_Type;
                       Head, Tail: CFG_Vertex) return Edge_Weight is
      H: Positive := Index_Of (Graph, Head);
      T: Positive := Index_Of (Graph, Tail);
   begin
      if H>Graph.Number or T>Graph.Number then
         raise Edge_Error;
      else
         return Graph.Edges(H,T).Weight;
      end if;
   end Weight_Of;

   function Successors (Graph: CFG_Type;
                        Vertex: CFG_Vertex) return Vertex_List is
      Index: Positive := Index_Of (Graph, Vertex);
      List: Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
   begin
      for Destination in Graph.Vertices'First..Graph.Number loop
         if Graph.Edges (Index, Destination).Defined then
            --Put_Line("% " & Integer'Image(N));
            N := N+1;
            List(N) := Graph.Vertices(Destination);
         end if;
      end loop;
      return List(1..N);
   end Successors;

   function Predecessors (Graph: CFG_Type;
                        Vertex: CFG_Vertex) return Vertex_List is
      Index: Positive := Index_Of (Graph, Vertex);
      List: Vertex_List (1..Graph.Number);  -- max possible outdegree
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
      --New_line;
      return List(1..N);
   end Predecessors;

   function First_Predecessor (Graph  : CFG_Type;
                               Vertex : CFG_Vertex) return CFG_Vertex is
      Index: Positive := Index_Of (Graph, Vertex);
      --List: Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
      found : Boolean := False;
      firstPred : CFG_Vertex;
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

            --return Graph.Vertices(Origin);
         end if;
      end loop;
      return firstPred;
   exception
      when Error:others =>
         Put_Line("No predecessors for this node!");
         raise;
   end First_Predecessor;



   function Predecessors_Num (Graph: CFG_Type;
                              Vertex: CFG_Vertex) return Natural is
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

   function Successors_Num (Graph  : CFG_Type;
                            Vertex : CFG_Vertex) return Natural is
      Index: Positive := Index_Of (Graph, Vertex);
      List: Vertex_List (1..Graph.Number);  -- max possible outdegree
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


   function Head (Graph : CFG_Type) return CFG_Vertex is
--     V_Rec : Vertex_Rec := Graph.Vertices (1);
   begin
      return Graph.Vertices(1);
   end Head;


   procedure Clear_All_Marks (Graph: in out CFG_Type) is
   begin
      for I in Graph.Vertices'First..Graph.Number loop
         Graph.Vertices(I).Marked := False;
      end loop;
   end Clear_All_Marks;

   function Marked (Graph: CFG_Type;
                    Vertex: CFG_Vertex) return Boolean is
      Index: Positive := Index_Of (Graph, Vertex);
   begin
      if Index > Graph.Number then
         raise Vertex_Error;
      else
         return (Graph.Vertices(Index).Marked);
      end if;
   end Marked;

   procedure Mark_Vertex (Graph : in out CFG_Type;
                          Vertex: in     CFG_Vertex) is
      Index: Positive := Index_Of (Graph, Vertex);
   begin
      if Index > Graph.Number then
         raise Vertex_Error;
      else
         Graph.Vertices(Index).Marked := True;
      end if;
   end Mark_Vertex;


   procedure Set_Name (Graph: in out CFG_Type; n : in String) is
      use Ada.Strings.Fixed;
   begin
      Ada.Strings.Fixed.Move (n, Graph.Name, Ada.Strings.Right,
                              Ada.Strings.Left,Ada.Strings.Space);
   end Set_Name;

   function Get_Name (Graph: in CFG_Type) return String is
   begin
      return Graph.Name;
   end Get_Name;

   procedure Set_Id (Graph: in out CFG_Type; n : in String) is
      use Ada.Strings.Fixed;
   begin
      Ada.Strings.Fixed.Move (n, Graph.Id, Ada.Strings.Right,
                              Ada.Strings.Left,Ada.Strings.Space);
   end Set_Id;

   function Get_Id (Graph: in CFG_Type) return String is
   begin
      return Graph.Id;
   end Get_Id;

   procedure Set_Is_Main (Graph: in out CFG_Type) is
   begin
      Graph.Is_Main := True;
   end Set_Is_Main;

   function Get_Is_Main (Graph: in CFG_Type) return Boolean is
   begin
      return Graph.Is_Main;
   end Get_Is_Main;

   function Get_Graph_Info (Graph : in CFG_Type) return String is
   begin
      return ("Graph Info: " & Graph.Id & " Child: " &Integer'Image(Graph.Number));

   end Get_Graph_Info;

   function Get_Graph_Number (Graph : in CFG_Type) return Natural is
   begin
      return Graph.Number;
   end Get_Graph_Number;

   function Get_Graph_Number (Graph_Ptr : in CFG_Ptr) return Natural is
      G : CFG_Type := Graph_Ptr.all;
   begin
      return Get_Graph_Number(G);
   end Get_Graph_Number;

   function Get_Vertices (Graph : in CFG_Type) return CFG_Vertex_Array is
   begin
      return Graph.Vertices;
   end Get_Vertices;

   function Get_Vertices (Graph : in CFG_Ptr) return CFG_Vertex_Array is
   begin
      return Graph.all.Vertices;
   end Get_Vertices;

   function Get_Vertices_Exact (Graph : in CFG_Ptr) return CFG_Vertex_Array is
   begin
      return Graph.Vertices (1..Graph.Number);
   end Get_Vertices_Exact;


   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- Export CFG utilities
   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   procedure Export_To_Dot_Rec (output : in out File_type;
                                curr   : in CFG_Vertex;
                                cfg    : in out CFG_Type
                                --parent : String;
                                --rel    : String
                               ) is
      Child_Node : CFG_Vertex;
      --Is_Marked : Boolean;
   begin
      declare
         Child_Num  : Natural := Successors_Num (Graph  => cfg,
                                             Vertex => curr);
         Child_List : Vertex_List (1..Child_Num);
      begin
         --Print_Vertex (curr);
         Child_List := Successors (Graph  => cfg,
                                      Vertex => curr);
         if (Child_Num /= 0) then
            --Put_Line("Children -> " & Integer'Image(Child_Num));
            for Index in 1..Child_Num loop
               --Put_Line ("Child " & Integer'Image(Index));
               Child_Node := Child_List(Index);
               --  If not marked yet
               if ( not Marked(cfg, Child_Node)) then
                  --  Add this node
                  if (Child_Node.typ = Stub) then
                     Put_Line (output, Child_Node.BB & "[label=""" &
                               To_String(Child_Node.BB) &
                               " #" & Integer'Image(Index_Of(cfg, Child_Node)) &
                               "\n(" &
                               V_Type_Pretty_Printer(Child_Node.typ) &
                               ")"", style=dotted];");
                  else
                     Put_Line (output, Child_Node.BB & "[label=""" &
                               To_String(Child_Node.BB) &
                               " #" & Integer'Image(Index_Of(cfg, Child_Node)) &
                               --"-" &  Integer'Image(Index_Of(cfg, Child_Node)) &
                               "\n(" &
                               V_Type_Pretty_Printer(Child_Node.typ) &
                               ")""];");
                  end if;
                  --  Mark this node
                  Mark_Vertex (cfg, Child_Node);
                  --  Trigger recursion
                  Export_To_Dot_Rec (output, Child_Node, cfg);
               end if;
               --  Add an dege between the two nodes
               Put_Line (output, curr.BB & "-> " &
                         To_String(Child_Node.BB) & ";");
            end loop;
         end if;
      end;
   end Export_To_Dot_Rec;


   procedure Export_To_Dot is
      use Global;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      --Line : String (1 .. 10_000);
      --Last : Natural;
      currCFG : CFG_ptr;
      CFG_Node : CFG_Vertex;
      --CFG_Succ_Node : CFG_Vertex;
      --File_Name_String : String (1..8);
      Entry_point : Boolean := True;
      --ET : Link_Type;
      Child_Number : Natural := 0;
   begin
      Create (Output, Out_File, To_String(Global.Program)&".dot");
      Put_line(Output, "/* DOT Graph description");
      Put_line(Output, " * Generated by OptLayout");
      Put (Output, " * Main program: ");
      Put_Line (Output, Global.Program);
      Put_line(Output, " */");
      Put_line(Output, "digraph GFG {");
      -- Calculate a proper dimension
      for Index in 1..CFGs.Size loop
         currCFG := Get_CFG(Index);--Index);
         Child_Number := Child_Number + Get_Graph_Number(currCfg.all);
      end loop;
      --  Set graph dimensions
      Put_Line(Output, "size = ""9;9"";" );
               --& Integer'Image(Child_Number+20) & "," &
               --Integer'Image(Child_Number+40) & """;");
--        Put_Line(Output, "size = ""3,3"";");
      for Index in 1..CFGs.Size loop
         currCFG := Get_CFG(Index);
         --  Print Nodes
         --  WARNING: Call nodes should be handled separatedly
      CFG_Node := Head (currCFG.all);
      Put_Line (Output,CFG_Node.BB & "[shape=box]" &
                "[label=""" & To_String(Get_Name(currCFG.all)) & "\n" &
                To_String(CFG_Node.BB) &
                " #" & Integer'Image(Index_Of(currCFG.all, CFG_Node)) &
                "\n(" &
                V_Type_Pretty_Printer(CFG_Node.typ) & ")""];");
      Mark_Vertex (Graph  => currCFG.all,
                   Vertex => CFG_Node);

      Export_To_Dot_Rec (Output, CFG_Node, currCfg.all);

      end loop;
      Put_line(Output, "}");
      Close (Output);
      Execute_Dot (To_String(Global.Program));
      --Put_Line("dot -Tps " & To_String(Global.Program)&".dot");
   end Export_To_Dot;


   procedure Export_To_Dot_Single (Graph : CFG_ptr) is
      use Global;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      currCFG : CFG_ptr;
      CFG_Node : CFG_Vertex;
      Entry_point : Boolean := True;
      Child_Number : Natural := 0;
   begin
      Create (Output, Out_File, To_String(Get_Name(Graph.all))&".dot");
      Put_line(Output, "/* DOT Graph description");
      Put_line(Output, " * Generated by OptLayout");
      Put (Output, " * Main program: ");
      Put_Line (Output, Global.Program);
      Put_line(Output, " */");
      Put_line(Output, "digraph GFG {");
      Put_Line(Output, "size = ""9;9"";" );
               --& Integer'Image(Child_Number+20) & "," &
               --Integer'Image(Child_Number+40) & """;");
--        Put_Line(Output, "size = ""3,3"";");
      --for Index in 1..CFGs.Size loop
         currCFG := Graph;--Get_CFG(Index);
         --  Print Nodes
         --  WARNING: Call nodes should be handled separatedly
      CFG_Node := Head (currCFG.all);
      Put_Line (Output,CFG_Node.BB & "[shape=box]" &
                "[label=""" & To_String(Get_Name(currCFG.all)) & "\n" &
                To_String(CFG_Node.BB) &
                " #" & Integer'Image(Index_Of(currCFG.all, CFG_Node)) &
                "\n(" &
                V_Type_Pretty_Printer(CFG_Node.typ) & ")""];");
      Mark_Vertex (Graph  => currCFG.all,
                   Vertex => CFG_Node);
      Export_To_Dot_Rec (Output, CFG_Node, currCfg.all);
      Put_line(Output, "}");
      Close (Output);
      Execute_Dot (To_String(Get_Name(Graph.all)));
   end Export_To_Dot_Single;


--     digraph G {
--  2: size ="4,4";
--  3: main [shape=box]; /* this is a comment */
--  4: main -> parse [weight=8];
--  5: parse -> execute;
--  6: main -> init [style=dotted];
--  7: main -> cleanup;
--  8: execute -> { make_string; printf}
--  9: init -> make_string;
--  10: edge [color=red]; // so is this
--  11: main -> printf [style=bold,label="100 times"];
--  12: make_string [label="make a\nstring"];
--  13: node [shape=box,style=filled,color=".7 .3 1.0"];
--  14: execute -> compare;
--  15: }

end CFG_Graph;

