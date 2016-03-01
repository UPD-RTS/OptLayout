
generic

   type Vertex_Type is private;
   type Edge_Type is private;
   type Edge_Weight is private;
   F : access function (gA, gB: Vertex_Type) return Boolean;
   P : access procedure (v : Vertex_Type);

package Graph is


   type Graph_Type (Max_Vertices : Positive) is limited private;

   type Graph_Type_Ptr is access all Graph_Type;

   type Vertex_List is array (Positive range <>) of Vertex_Type;

   type Vertex_List_Ptr is access all Vertex_List;

   type Edge_List is array (Positive range <>) of Edge_Type;

   Vertex_Error, Edge_Error, Overflow: exception;

   procedure Clear (Graph : in out Graph_Type);

   procedure Unset (G : in out Graph_Type_Ptr);

   procedure Add_Vertex (Graph  : in out Graph_Type;
                         Vertex : in     Vertex_Type);

   procedure Add_Edge (Graph: in out Graph_Type;
                       Head, Tail: in Vertex_Type;
                       Edge : Edge_Type;
                       Weight: in Edge_Weight);

   function Weight_Of (Graph: Graph_Type;
                       Head, Tail: Vertex_Type) return Edge_Weight;

   function Successors (Graph  : Graph_Type;
                        Vertex : Vertex_Type) return Vertex_List;

   function First_Predecessor (Graph  : Graph_Type;
                               Vertex : Vertex_Type) return Vertex_Type;

   function Predecessors (Graph  : Graph_Type;
                          Vertex : Vertex_Type) return Vertex_List;

   function Predecessors_Num (Graph  : Graph_Type;
                        Vertex : Vertex_Type) return Natural;

   function Successors_Num (Graph  : Graph_Type;
                            Vertex : Vertex_Type) return Natural;

   function Head (Graph : Graph_Type) return Vertex_type;

   procedure Clear_All_Marks (Graph: in out Graph_Type);

   function Marked (Graph  : Graph_Type;
                    Vertex : Vertex_Type) return Boolean;

   procedure Mark_Vertex (Graph : in out Graph_Type;
                          Vertex: in     Vertex_Type);

   procedure Set_Name (Graph: in out Graph_Type; n : in String);

   function Get_Name (Graph: in Graph_Type) return String ;

   procedure Set_Id (Graph: in out Graph_Type; n : in String);

   function Get_Id (Graph: in Graph_Type) return String;

   procedure Set_Is_Main (Graph: in out Graph_Type);

   function Get_Is_Main (Graph: in Graph_Type) return Boolean;

   function Get_Graph_Info (Graph : in Graph_Type) return String;

   function "="(gA, gB: Vertex_Type) return Boolean renames F.all;

   procedure Print_Vertex_Info (v: Vertex_type) renames P.all;

   function Get_Graph_Number (Graph : in Graph_Type) return Natural;

   function Get_Graph_Number (Graph_Ptr : in Graph_Type_Ptr) return Natural;


   function Node_Defined (Graph : Graph_Type;
                          Vertex: Vertex_Type) return Boolean;


   type Vertex_Rec is
      record
         Info   : Vertex_Type;       -- Information declared by user
         Marked : Boolean := False;  -- Has the Vertex been visited?
         --Is_Entry: Boolean := False;  -- Information declared by users
      end record;

   type Edge_Record is
      record
         EType   : Edge_Type;
         Weight  : Edge_Weight;       -- Weight assigned to the edge
         Defined : Boolean := False;  -- Is this edge in the graph?
      end record;

   type Vertex_Array is array (Positive range <>) of Vertex_Rec;
   type Vertex_Array_Ptr is access Vertex_Array;

   type Matrix_Type  is array (Positive range <>,
                               Positive range <>) of Edge_Record;
   type Matrix_Type_Ptr is access Matrix_Type;

   function Get_Vertices (Graph_ptr : in Graph_Type) return Vertex_Array;

   function Index_Of (Graph : Graph_Type;
                      Vertex: Vertex_Type) return Positive;

private

   type Graph_Type (Max_Vertices : Positive) is
      record
         Number : Natural := 0;
         Id : String (1..10);
         Name : String (1..100);
         Is_Main : Boolean := False;
         Vertices : Vertex_Array (1..Max_Vertices);
         Edges    : Matrix_Type  (1..Max_Vertices, 1..Max_Vertices);
      end record;

end Graph;
