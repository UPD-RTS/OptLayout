--  graph.adb:  Generic package for weighted digraphs
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;
with Global;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;


package body Graph is

   -- Unchecked deallocation of nodes
   procedure Free_Graph is new  Ada.Unchecked_Deallocation (
                                                      Graph_Type,
                                                      Graph_Type_Ptr);

   procedure Unset (G : in out Graph_Type_Ptr) is
      temp : Graph_Type_Ptr := G;
   begin
      Put_line("unset");
      Free_Graph (temp);
   end Unset;


   procedure Clear (Graph : in out Graph_Type) is
   begin
      Graph.Number := 0;
   end Clear;

   function Index_Of (Graph : Graph_Type;
                      Vertex: Vertex_Type) return Positive is
   begin
      for I in Graph.Vertices'First..Graph.Number loop
         if Graph.Vertices(I).Info=Vertex then
            return I;
         end if;
      end loop;
      return Graph.Number+1;  --  if not there, return next index
   end Index_Of;

   function Node_Defined (Graph : Graph_Type;
                          Vertex: Vertex_Type) return Boolean is
   begin
      for I in Graph.Vertices'First..Graph.Number loop
         if Graph.Vertices(I).Info=Vertex then
            return true;
         end if;
      end loop;
      return false;  --  if not there, return next index
   end Node_Defined;


   procedure Add_Vertex (Graph  : in out Graph_Type;
                         Vertex : in     Vertex_Type) is
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
         Graph.Vertices (Index) := (Vertex, False);
         Graph.Number := Index;
         --  no edges for this vertex (yet)
         for Destination in Graph.Vertices'Range loop
            Graph.Edges(Index,Destination).Defined := False;
         end loop;
      end if;
   end Add_Vertex;

   procedure Add_Edge (Graph: in out Graph_Type;
                       Head, Tail: Vertex_Type;
                       Edge : Edge_Type; Weight: Edge_Weight) is
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
         Graph.Edges (H,T) := (Edge_Type(Edge), Weight, True);
      end if;
   end Add_Edge;

   function Weight_Of (Graph: Graph_Type;
                       Head, Tail: Vertex_Type) return Edge_Weight is
      H: Positive := Index_Of (Graph, Head);
      T: Positive := Index_Of (Graph, Tail);
   begin
      if H>Graph.Number or T>Graph.Number then
         raise Edge_Error;
      else
         return Graph.Edges(H,T).Weight;
      end if;
   end Weight_Of;

   function Successors (Graph: Graph_Type;
                        Vertex: Vertex_Type) return Vertex_List is
      Index: Positive := Index_Of (Graph, Vertex);
      List: Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
   begin
      for Destination in Graph.Vertices'First..Graph.Number loop
         if Graph.Edges (Index, Destination).Defined then
            --Put_Line("% " & Integer'Image(N));
            N := N+1;
            List(N) := Graph.Vertices(Destination).Info;
         end if;
      end loop;
      return List(1..N);
   end Successors;

   function Predecessors (Graph: Graph_Type;
                        Vertex: Vertex_Type) return Vertex_List is
      Index: Positive := Index_Of (Graph, Vertex);
      List: Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
   begin
      --Put("Predecessors:");
      for Origin in Graph.Edges'Range loop
         if Graph.Edges (Origin, Index).Defined then
            --Put("* " & Integer'Image(N));
            N := N+1;
            --Print_Vertex_Info(Graph.Vertices(Origin).Info);
            List(N) := Graph.Vertices(Origin).Info;
         end if;
      end loop;
      --New_line;
      return List(1..N);
   end Predecessors;

   function First_Predecessor (Graph  : Graph_Type;
                               Vertex : Vertex_Type) return Vertex_Type is
      Index: Positive := Index_Of (Graph, Vertex);
      --List: Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
   begin
      --Put("Predecessors:");
      for Origin in Graph.Edges'Range loop
         if Graph.Edges (Origin, Index).Defined then
            --Put("* " & Integer'Image(N));
            return Graph.Vertices(Origin).Info;
         end if;
      end loop;
   exception
      when Error:others =>
         Put_Line("No predecessors for this node!");
         raise;
   end First_Predecessor;



   function Predecessors_Num (Graph: Graph_Type;
                              Vertex: Vertex_Type) return Natural is
      Index: Positive := Index_Of (Graph, Vertex);
      List: Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
   begin
      --Put("Predecessors num:");
      for Destination in Graph.Edges'Range loop
         if Graph.Edges (Destination, Index).Defined then
            N := N+1;
            --Print_Vertex_Info(Graph.Vertices(Index).Info);
            List(N) := Graph.Vertices(Index).Info;
         end if;
      end loop;
      --Put("*num " & Integer'Image(N) & " ");
      return N;
   end Predecessors_Num;

   function Successors_Num (Graph  : Graph_Type;
                            Vertex : Vertex_Type) return Natural is
      Index: Positive := Index_Of (Graph, Vertex);
      List: Vertex_List (1..Graph.Number);  -- max possible outdegree
      N: Natural := 0;
   begin
      for Destination in Graph.Vertices'First..Graph.Number loop
         if Graph.Edges (Index, Destination).Defined then
            N := N+1;
            List(N) := Graph.Vertices(Destination).Info;
         end if;
      end loop;
      --Put("% " & Integer'Image(N) & " ");
      return N;
   end Successors_Num;


   function Head (Graph : Graph_Type) return Vertex_Type is
     V_Rec : Vertex_Rec := Graph.Vertices (1);
   begin
      return V_Rec.Info;
   end Head;


   procedure Clear_All_Marks (Graph: in out Graph_Type) is
   begin
      for I in Graph.Vertices'First..Graph.Number loop
         Graph.Vertices(I).Marked := False;
      end loop;
   end Clear_All_Marks;

   function Marked (Graph: Graph_Type;
                    Vertex: Vertex_Type) return Boolean is
      Index: Positive := Index_Of (Graph, Vertex);
   begin
      if Index > Graph.Number then
         raise Vertex_Error;
      else
         return (Graph.Vertices(Index).Marked);
      end if;
   end Marked;

   procedure Mark_Vertex (Graph : in out Graph_Type;
                          Vertex: in     Vertex_Type) is
      Index: Positive := Index_Of (Graph, Vertex);
   begin
      if Index > Graph.Number then
         raise Vertex_Error;
      else
         Graph.Vertices(Index).Marked := True;
      end if;
   end Mark_Vertex;


   procedure Set_Name (Graph: in out Graph_Type; n : in String) is
      use Ada.Strings.Fixed;
   begin
      Ada.Strings.Fixed.Move (n, Graph.Name, Ada.Strings.Right,
                              Ada.Strings.Left,Ada.Strings.Space);
   end Set_Name;

   function Get_Name (Graph: in Graph_Type) return String is
   begin
      return Graph.Name;
   end Get_Name;

   procedure Set_Id (Graph: in out Graph_Type; n : in String) is
      use Ada.Strings.Fixed;
   begin
      Ada.Strings.Fixed.Move (n, Graph.Id, Ada.Strings.Right,
                              Ada.Strings.Left,Ada.Strings.Space);
   end Set_Id;

   function Get_Id (Graph: in Graph_Type) return String is
   begin
      return Graph.Id;
   end Get_Id;

   procedure Set_Is_Main (Graph: in out Graph_Type) is
   begin
      Graph.Is_Main := True;
   end Set_Is_Main;

   function Get_Is_Main (Graph: in Graph_Type) return Boolean is
   begin
      return Graph.Is_Main;
   end Get_Is_Main;

   function Get_Graph_Info (Graph : in Graph_Type) return String is
   begin
      return ("Graph Info: " & Graph.Id & " Child: " &Integer'Image(Graph.Number));

   end Get_Graph_Info;

   function Get_Graph_Number (Graph : in Graph_Type) return Natural is
   begin
      return Graph.Number;
   end Get_Graph_Number;

   function Get_Graph_Number (Graph_Ptr : in Graph_Type_Ptr) return Natural is
      G : Graph_Type := Graph_Ptr.all;
   begin
      return Get_Graph_Number(G);
   end Get_Graph_Number;

   function Get_Vertices (Graph_ptr : in Graph_Type) return Vertex_Array is
      G : Graph_Type := Graph_Ptr;
   begin
      return G.Vertices;
   end Get_Vertices;





end Graph;

