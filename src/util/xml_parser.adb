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
-- FILE NAME      : xml_parser.adb
-- PACKAGE        : XML_PARSER body
-- PURPOSE        : Utilities to load/store information from/into XML
--                  including CFG reconstruction and constraint mgmt
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Core.Attrs;     use DOM.Core.Attrs;
with DOM.Core.Elements;     use DOM.Core.Elements;
with Ada.Text_IO;        use Ada.Text_IO;
--with Graph;
with CFG_Graph; use CFG_Graph;
with Global; use Global;
with Loop_Tree; use Loop_Tree;
with Dominator_Analysis;
with Cache, RAM, Ada.Integer_Text_IO, Utilities, Interactions;


with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Calendar; use Ada.Calendar;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package body Xml_Parser is


   package Loops renames Loop_Tree.LT;

   package T_IO renames Ada.Integer_Text_IO;

   FIRST_READ : Boolean := True;

   procedure Print_Node (N : in Node) is
      use Ada.Text_IO.Text_Streams;
   begin
      Write(Stream (Standard_Output), N);
   end Print_Node;

   function Get_Child (N: in Node; name : in String) return Node is
      NList : Node_List;
      Child : Node;
   begin
      NList := Child_Nodes(N);
      for Index in 1 .. Length (NList) loop
         Child := Item (NList, Index - 1);
         if (Node_Type(Child) = Element_Node and
               Node_Name(Child)=name) then
            --Print_Node(Child);
            return Child;
         end if;
      end loop;
      return null;
   end Get_Child;



   procedure Build_CFG (Filename : String) is
      --use ControlFlowGraph;
      Input    : File_Input;
      Reader   : Tree_Reader;
      Doc      : Document;
      List     : Node_List;
      BB_List  : Node_List;
      BBC_List : Node_List;
      N        : Node;
      C        : Node;
      E	       : Node;
      L	       : Node;
      curCFG   : CFG_ptr;
      CFG_Node : CFG_Vertex;
      CFG_Succ_Node : CFG_Vertex;
      --  CFG_Stub_Node : CFG_Vertex;
      Fix_String : String (1..8);
      Fix_Str_cfgId : String (1..10);
      Entry_point : Boolean := True;
      ET : Link_Type;
      --Node_Index : Positive;
      Cardinty : Positive;
   begin

      --if (Global.E_M = DEBUG) then
         Put_Line("ReadXML: " & Filename);
      --end if;

      Open (Filename, Input);
      Parse (Reader, Input);
      Close (Input);
      Doc := Get_Tree (Reader);
      List := Documents.Get_Elements_By_Tag_Name (Doc, "cfg");
      Cardinty := Length (List) + (Length (List)/4);

      --  Initialize the list of CFGs
      --  (one for the root procedure plus one for each called procedure)
      CFG_Graph.Init (progName =>
                        Get_Attribute (Get_Element (Doc), "name"),
                      cfg_num  => Length (List));

      if (Global.E_M in VERBOSE..DEBUG) then
         New_Line;
         Put_line ("Program name: " & Get_Attribute (Get_Element (Doc), "name"));
         Put_line ("CFGs: " & Integer'Image(Length (List)));
         New_Line;
      end if;

      --  Iterates through all cfgs
      --  Due to the CFG structure, we need to trverse the program structure
      --  twice: once for the nodes and once for the edges
      for Index in 1 .. Length (List) loop
         Entry_point := True;

         curCFG := new CFG_type (Cardinty);
         N := Item (List, Index - 1);
         --  Set CFG name
         Set_Name (curCFG.all, Value (Get_Named_Item (Attributes (N), "name")));
         Put_Line (Value (Get_Named_Item (Attributes (N), "name")));
         --  If the current cfg is that of the ROOT prodeure
         if (Get_Named_Item (Attributes (N), "main") /= null) then
            Put_Line("  ROOT  ");
            Set_Is_Main(curCFG.all);
         end if;
         --  Set CFG Id
         Set_Id (curCFG.all, Value (Get_Named_Item (Attributes (N), "id")));

--           if (Global.E_M in VERBOSE..DEBUG) then
--              New_Line;
--              Put_Line ("--" & Node_name(N) & ":: " &
--                        Value (Get_Named_Item (Attributes (N), "id")));
--
--           end if;

         --  Get children nodes (Basic Blocks)
         BB_List := Child_Nodes(N);
         --  Put_Line ("Figli: " & Integer'Image(Length (BB_List)));
         --  Iterates all child nodes a cfg
         for BB_Index in 1 .. Length (BB_List) loop
            C := Item (BB_List, BB_Index -1);
            --  Detect if we are parsing an ELEMENT node
            if (Node_Type(C) = Element_Node) then
               if (Global.E_M = VERBOSE) then
                  if (Entry_point) then
                     Put ("Entry->");
                  end if;
                  Put (Node_name(C) & " id:" &
                       Value (Get_Named_Item (Attributes (C), "id")));
                  --Print_Node(C);
               end if;

               --  Add this node to the CFG
               Ada.Strings.Fixed.Move (
                 Value (Get_Named_Item (Attributes (C),"id")),
                 Fix_String, Ada.Strings.Right,
                 Ada.Strings.Left,Ada.Strings.Space);

               -- BB_Type is (Root, Stub, Simple, Leaf);
               if (Entry_point) then
                  --  This Node is the procedure entry point
                  CFG_Node := (Root, Fix_String, 0, cfgPlaceHolder, False);
                  --   Value (Get_Named_Item (Attributes (C), "id")));
                  Entry_point := False;
               else
                  CFG_Node := (Simple,Fix_String, 0, cfgPlaceHolder, False);
                  --  Value (Get_Named_Item (Attributes (C), "id")));
               end if;
               Add_Vertex(curCFG.all, CFG_Node);

               --  Retrieve edge (link) data (<succ>)
--                 E := Get_Child(C, "succ");
--                 if (E /=null) then
--                    if (Global.E_M = VERBOSE) then
--                       Put("  " & Node_name(E));
--                    end if;
--
--                    --  Get all successors
--                    BBC_List := Child_Nodes(E);
--                    --  Iterates over <link>
--                    if (Global.E_M = VERBOSE) then
--                       Put(" ( ");
--                    end if;
--
--                    --  For each exit link
--                    for SUC_Index in 1 .. Length (BBC_List) loop
--                       L := Item (BBC_List, SUC_Index -1);
--
--                       if(Node_Type(L) = Element_Node) then
--                          if (Global.E_M = VERBOSE) then
--                             Put (Value (Get_Named_Item (Attributes (L), "bb")) &
--                               "->" & Value (Get_Named_Item (Attributes (L),
--                                 "type")) & " ");
--                          end if;
--                          --  Define an Edge between Nodes (with type)
--                          --  TODO
--
--
--                       end if;
--                    end loop;
--                    if (Global.E_M = VERBOSE) then
--                       Put(" ) ");
--                    end if;
--                 end if;
--                 if (Global.E_M = VERBOSE) then
--                    New_Line;
--                 end if;
            end if;
         end loop;
         --  Add the CFG to the CFG List
         Add (new_cfg => curCFG);
         --Put_line("CFG ::" & Get_Graph_Info(curCFG.all));
      end loop;

--        if (Global.E_M = VERBOSE) then
--           New_Line;
--           Put_Line("Second pass");
--           New_Line;
--        end if;


      --  Second pass through the cfgs to build the cfg edges
      for Index in 1 .. Length (List) loop
         Entry_point := True;

         curCFG := Get_CFG(Index);
         --Put_Line("Current Graph: " & Get_Graph_Info(curCFG.all));
         N := Item (List, Index - 1);
         --  CFG name, root attribute and Id are already set
--           if (Global.E_M in VERBOSE..DEBUG) then
--              --New_Line;
--              Put_Line ("--" & Node_name(N) & ":: " &
--                        Value (Get_Named_Item (Attributes (N), "id")));
--
--           end if;

         --  Get children nodes (Basic Blocks)
         BB_List := Child_Nodes(N);
         --  Put_Line ("Figli: " & Integer'Image(Length (BB_List)));
         --  Iterates all child nodes a cfg
         for BB_Index in 1 .. Length (BB_List) loop
            C := Item (BB_List, BB_Index -1);
            --  Detect if we are parsing an ELEMENT node
            if (Node_Type(C) = Element_Node) then
               if (Global.E_M = VERBOSE) then
                  if (Entry_point) then
                     Put ("Entry->");
                  end if;
                  --Put (Node_name(C) & " id:" &
                   --    Value (Get_Named_Item (Attributes (C), "id")));
                  --Print_Node(C);
               end if;
               --  Node C has been already added to the CFG
               --  A new CFG node for C
               Ada.Strings.Fixed.Move (
                 Value (Get_Named_Item (Attributes (C),"id")),
                 Fix_String, Ada.Strings.Right,
                                       Ada.Strings.Left,Ada.Strings.Space);

               CFG_Node := (Simple, Fix_String, 0, cfgPlaceHolder, False);

               if (Global.E_M = VERBOSE) then
                  Put_Line ("Node: " & Fix_String);
               end if;


               --  Retrieve edge (link) data (<succ>)
               E := Get_Child(C, "succ");
               if (E /=null) then
                  if (Global.E_M = VERBOSE) then
                     Put("  " & Node_name(E));
                  end if;

                  --  Get all successors
                  BBC_List := Child_Nodes(E);
                  --  Iterates over <link>
                  if (Global.E_M = VERBOSE) then
                     Put(" ( ");
                  end if;

                  --  For each exit link
                  for SUC_Index in 1 .. Length (BBC_List) loop
                     L := Item (BBC_List, SUC_Index -1);

                     if(Node_Type(L) = Element_Node) then
                        if (Global.E_M = VERBOSE) then
                           Put (Value (Get_Named_Item (Attributes (L), "bb")) &
                             "->" & Value (Get_Named_Item (Attributes (L),
                               "type")) & " ");
                        end if;


                        --  Detect link type
                        if ( Value (Get_Named_Item (Attributes (L),"type")) =
                              "call") then
                           ET := Call;
                        elsif ( Value (Get_Named_Item (Attributes (L),"type")) =
                                 "taken") then
                           ET := Taken;
                        else
                           ET:= Nottaken;
                        end if;

                        --  Define an Edge between Nodes (with type)
                        Ada.Strings.Fixed.Move (
                           Value (Get_Named_Item (Attributes (L),"bb")),
                           Fix_String, Ada.Strings.Right,
                                                Ada.Strings.Left,Ada.Strings.Space);
                        --Put_Line ("Succ: " & Fix_String);

                        if (ET = Call) then
                           if (Global.E_M in VERBOSE..DEBUG) then
                              Put_Line("***************  CALL ****************");
                              Put ("To: " & Fix_String);
                           end if;

                           Ada.Strings.Fixed.Move (
                               Value (Get_Named_Item (Attributes (L),"cfg")),
                               Fix_Str_cfgId, Ada.Strings.Right,
                               Ada.Strings.Left,Ada.Strings.Space);
                           --  Provide a stub node for call links
                           CFG_Succ_Node := (Stub, Fix_String, 0, Fix_Str_cfgId, False);
                           --  Check if a stub node has been already defined
                           --  for the current procedure
                           if (Node_Defined(curCFG.all, CFG_Succ_Node)) then
                              CFG_Succ_Node := (Stub, Fix_String, 1, Fix_Str_cfgId, False);
                           end if;

                           --Print_Vertex(CFG_Succ_Node);

                           Add_Vertex (curCFG.all, CFG_Succ_Node);
                           --  Put_Line ("CALL Vertex Added");
                        else
                           CFG_Succ_Node := (Simple, Fix_String, 0, cfgPlaceHolder, False);
                        end if;


                        --Put_Line ("***** BEFORE ADD********");
                        --Put_Line (Boolean'Image(=(CFG_Node, CFG_Node)));
                        --Put_Line(Get_Graph_Info(curCFG.all));
                        if (Global.E_M = VERBOSE) then
                           Print_Vertex (CFG_Node);
                           Print_vertex (CFG_Succ_Node);
                        end if;

                        --if (ET /= Call) then
                           Add_Edge (Graph  => curCFG.all,
                                     Head   => CFG_Node,
                                     Tail   => CFG_Succ_Node,
                                     Link   => ET,
                                     Weight => 1);
                           if (Global.E_M = VERBOSE) then
                              Put_Line ("Edge Added");
                           end if;

                        --end if;




                     end if;
                  end loop;
--                    if (Global.E_M = VERBOSE) then
--                       Put(" ) ");
--                    end if;
               end if;
--                 if (Global.E_M = VERBOSE) then
--                    New_Line;
--                 end if;
            end if;
         end loop;
         --  Add the CFG to the CFG List
         --  Add (new_cfg => curCFG);
      end loop;
      --  END Iterates through all cfgs

      if (Global.drawGraphs) then
         CFG_Graph.Export_To_Dot;
      end if;

      Free (Reader);
   end Build_CFG;

   XML_POINTER : Positive := 1;

   procedure Build_One_CFG (Filename : String) is
      --use ControlFlowGraph;
      Input    : File_Input;
      Reader   : Tree_Reader;
      Doc      : Document;
      List     : Node_List;
      BB_List  : Node_List;
      BBC_List : Node_List;
      N        : Node;
      C        : Node;
      E	       : Node;
      L	       : Node;
      curCFG   : CFG_ptr;
      CFG_Node : CFG_Vertex;
      CFG_Succ_Node : CFG_Vertex;
      --  CFG_Stub_Node : CFG_Vertex;
      Fix_String : String (1..8);
      Fix_Str_cfgId : String (1..10);
      Entry_point : Boolean := True;
      ET : Link_Type;
      Index : Positive := XML_POINTER;
      --Node_Index : Positive;
      Cardinty : Positive;
      aliasLabel : Natural := 1;
   begin

      --if (Global.E_M = DEBUG) then
        -- Put_Line("ReadXML: " & Filename);
        --end if;

      Open (Filename, Input);
      Parse (Reader, Input);
      Close (Input);
      Doc := Get_Tree (Reader);
      List := Documents.Get_Elements_By_Tag_Name (Doc, "cfg");
      --Cardinty := Length (List) + (Length (List)/4);


      if (Global.E_M in VERBOSE..DEBUG) then
         New_Line;
         Put_line ("Program name: " & Get_Attribute (Get_Element (Doc), "name"));
         Put_line ("CFGs: " & Integer'Image(Length (List)));
         New_Line;
      end if;

      --  Build a single cfg at a time to avoid unmanageable memory consumption
      --  Due to the CFG structure, we need to trverse the program structure
      --  twice: once for the nodes and once for the edges
      --for Index in 1 .. Length (List) loop
      --  Index : Positive := XML_POINTER;

      Entry_point := True;

      -- # curCFG := new CFG_type (Cardinty);
      N := Item (List, Index - 1);
      BB_List := Child_Nodes(N);
      --Put_Line (" Figli: " & Integer'Image(Length (BB_List)));
      Cardinty := Length (BB_List);-- + (Length (BB_List)/4);
      curCFG := new CFG_type (Cardinty);
      --  Set CFG name
      Set_Name (curCFG.all, Value (Get_Named_Item (Attributes (N), "name")));

      if Global.tbshootLCT then
        Put (Value (Get_Named_Item (Attributes (N), "name")));
        Put_Line (" - " & Integer'Image(Cardinty) & " nodes");
      end if;

      --  If the current cfg is that of the ROOT prodeure
         if (Get_Named_Item (Attributes (N), "main") /= null) then
            --Put_Line("  ROOT  ");
            Set_Is_Main(curCFG.all);
         end if;
         --  Set CFG Id
         Set_Id (curCFG.all, Value (Get_Named_Item (Attributes (N), "id")));

--           if (Global.E_M in VERBOSE..DEBUG) then
--              New_Line;
--              Put_Line ("--" & Node_name(N) & ":: " &
--                        Value (Get_Named_Item (Attributes (N), "id")));
--
--           end if;

         --Put ("*");
         --  Get children nodes (Basic Blocks)
         -- # BB_List := Child_Nodes(N);
         --Put_Line ("Figli: " & Integer'Image(Length (BB_List)));
         --  Iterates all child nodes a cfg
         for BB_Index in 1 .. Length (BB_List) loop
            C := Item (BB_List, BB_Index -1);
            --Put_Line(Integer'Image(BB_Index) & " di " & Integer'Image(Length (BB_List)));
            --  Detect if we are parsing an ELEMENT node
            if (Node_Type(C) = Element_Node) then
               if (Global.E_M = VERBOSE) then
               if (Entry_point) then
                  --Put ("Entry->");
                  null;
               end if;
               --Put (Node_name(C) & " id:" &
                  --     Value (Get_Named_Item (Attributes (C), "id")));
                  --Print_Node(C);
               end if;

               --  Add this node to the CFG
               Ada.Strings.Fixed.Move (
                 Value (Get_Named_Item (Attributes (C),"id")),
                 Fix_String, Ada.Strings.Right,
                 Ada.Strings.Left,Ada.Strings.Space);

               -- BB_Type is (Root, Stub, Simple, Leaf);
               if (Entry_point) then
                  --  This Node is the procedure entry point
                  CFG_Node := (Root, Fix_String, 0, cfgPlaceHolder, False);
                  --   Value (Get_Named_Item (Attributes (C), "id")));
                  Entry_point := False;
               else
                  CFG_Node := (Simple,Fix_String, 0, cfgPlaceHolder, False);
                  --  Value (Get_Named_Item (Attributes (C), "id")));
               end if;
               --Put("Add v");
               Add_Vertex(curCFG.all, CFG_Node);
               --Put_Line("Post Add v");
               --  Retrieve edge (link) data (<succ>)
--                 E := Get_Child(C, "succ");
--                 if (E /=null) then
--                    if (Global.E_M = VERBOSE) then
--                       Put("  " & Node_name(E));
--                    end if;
--
--                    --  Get all successors
--                    BBC_List := Child_Nodes(E);
--                    --  Iterates over <link>
--                    if (Global.E_M = VERBOSE) then
--                       Put(" ( ");
--                    end if;
--
--                    --  For each exit link
--                    for SUC_Index in 1 .. Length (BBC_List) loop
--                       L := Item (BBC_List, SUC_Index -1);
--
--                       if(Node_Type(L) = Element_Node) then
--                          if (Global.E_M = VERBOSE) then
--                             Put (Value (Get_Named_Item (Attributes (L), "bb")) &
--                               "->" & Value (Get_Named_Item (Attributes (L),
--                                 "type")) & " ");
--                          end if;
--                          --  Define an Edge between Nodes (with type)
--                          --  TODO
--
--
--                       end if;
--                    end loop;
--                    if (Global.E_M = VERBOSE) then
--                       Put(" ) ");
--                    end if;
--                 end if;
--                 if (Global.E_M = VERBOSE) then
--                    New_Line;
--                 end if;
            end if;
         end loop;

         --  Add the CFG to the CFG List
         Add (new_cfg => curCFG);
         --Put_line("CFG ::" & Get_Graph_Info(curCFG.all));
      --end loop;

--        if (Global.E_M = VERBOSE) then
--           New_Line;
--           Put_Line("Second pass");
--           New_Line;
--        end if;
--      Put (">");

      --  Second pass through the cfgs to build the cfg edges
      --for Index in 1 .. Length (List) loop
      --Index : Positive := XML_POINTER;
         Entry_point := True;

         curCFG := Get_CFG(Index);
         --Put_Line("Current Graph: " & Get_Graph_Info(curCFG.all));
         N := Item (List, Index - 1);
         --  CFG name, root attribute and Id are already set
--           if (Global.E_M in VERBOSE..DEBUG) then
--              New_Line;
--              Put_Line ("--" & Node_name(N) & ":: " &
--                        Value (Get_Named_Item (Attributes (N), "id")));
--
--           end if;

         --  Get children nodes (Basic Blocks)
         BB_List := Child_Nodes(N);
         --  Put_Line ("Figli: " & Integer'Image(Length (BB_List)));
         --  Iterates all child nodes a cfg
         for BB_Index in 1 .. Length (BB_List) loop
            C := Item (BB_List, BB_Index -1);
            --  Detect if we are parsing an ELEMENT node
            if (Node_Type(C) = Element_Node) then
--                 if (Global.E_M = VERBOSE) then
--  --                    if (Entry_point) then
--  --                       Put ("Entry->");
--  --                    end if;
--                    --Put (Node_name(C) & " id:" &
--                    --     Value (Get_Named_Item (Attributes (C), "id")));
--                    --Print_Node(C);
--                 end if;
               --  Node C has been already added to the CFG
               --  A new CFG node for C
               Ada.Strings.Fixed.Move (
                 Value (Get_Named_Item (Attributes (C),"id")),
                 Fix_String, Ada.Strings.Right,
                                       Ada.Strings.Left,Ada.Strings.Space);

               CFG_Node := (Simple, Fix_String, 0, cfgPlaceHolder, False);

--                 if (Global.E_M = VERBOSE) then
--                    Put_Line ("Node: " & Fix_String);
--                 end if;


               --  Retrieve edge (link) data (<succ>)
               E := Get_Child(C, "succ");
               if (E /=null) then
--                    if (Global.E_M = VERBOSE) then
--                       Put("  " & Node_name(E));
--                    end if;

                  --  Get all successors
                  BBC_List := Child_Nodes(E);
                  --  Iterates over <link>
--                    if (Global.E_M = VERBOSE) then
--                       Put(" ( ");
--                    end if;

                  --  For each exit link
                  for SUC_Index in 1 .. Length (BBC_List) loop
                     L := Item (BBC_List, SUC_Index -1);

                     if(Node_Type(L) = Element_Node) then
--                          if (Global.E_M = VERBOSE) then
--                             Put (Value (Get_Named_Item (Attributes (L), "bb")) &
--                               "->" & Value (Get_Named_Item (Attributes (L),
--                                 "type")) & " ");
--                          end if;


                        --  Detect link type
                        if ( Value (Get_Named_Item (Attributes (L),"type")) =
                              "call") then
                           ET := Call;
                        elsif ( Value (Get_Named_Item (Attributes (L),"type")) =
                                 "taken") then
                           ET := Taken;
                        else
                           ET:= Nottaken;
                        end if;

                        --  Define an Edge between Nodes (with type)
                        Ada.Strings.Fixed.Move (
                           Value (Get_Named_Item (Attributes (L),"bb")),
                           Fix_String, Ada.Strings.Right,
                                                Ada.Strings.Left,Ada.Strings.Space);
                        --Put_Line ("Succ: " & Fix_String);

                        if (ET = Call) then
--                             if (Global.E_M in VERBOSE..DEBUG) then
--                                Put_Line("***************  CALL ****************");
--                                Put ("To: " & Fix_String);
--                             end if;

                           Ada.Strings.Fixed.Move (
                               Value (Get_Named_Item (Attributes (L),"cfg")),
                               Fix_Str_cfgId, Ada.Strings.Right,
                               Ada.Strings.Left,Ada.Strings.Space);
                           --  Provide a stub node for call links
                           CFG_Succ_Node := (Stub, Fix_String, 0, Fix_Str_cfgId, False);
                           --  Check if a stub node has been already defined
                           --  for the current procedure
                           --  Put ("CALL Vertex Added -> " & CFG_Succ_Node.cfgId);

                        if (Node_Defined(curCFG.all, CFG_Succ_Node)) then
                           --CFG_Succ_Node := (Stub, Fix_String, 1, Fix_Str_cfgId, False);
                           CFG_Succ_Node := (Stub, Fix_String, aliasLabel, Fix_Str_cfgId, False);
                           aliasLabel := aliasLabel +1;
                           -- Put(" [alias]");
                        end if;
                        --New_Line;
                        --Print_Vertex(CFG_Succ_Node);

                        Add_Vertex (curCFG.all, CFG_Succ_Node);
                        --  Put_Line ("CALL Vertex Added");
                     else
                        CFG_Succ_Node := (Simple, Fix_String, 0, cfgPlaceHolder, False);
                     end if;


                        --Put_Line ("***** BEFORE ADD********");
                        --Put_Line (Boolean'Image(=(CFG_Node, CFG_Node)));
                        --Put_Line(Get_Graph_Info(curCFG.all));
                        if (Global.E_M = VERBOSE) then
                           Print_Vertex (CFG_Node);
                           Print_vertex (CFG_Succ_Node);
                        end if;

                        --if (ET /= Call) then
                           Add_Edge (Graph  => curCFG.all,
                                     Head   => CFG_Node,
                                     Tail   => CFG_Succ_Node,
                                     Link   => Call,
                                     Weight => 1);
                           if (Global.E_M = VERBOSE) then
                              Put_Line ("Edge Added");
                           end if;

                        --end if;




                     end if;
                  end loop;
--                    if (Global.E_M = VERBOSE) then
--                       Put(" ) ");
--                    end if;
               end if;
--                 if (Global.E_M = VERBOSE) then
--                    New_Line;
--                 end if;
            end if;
         end loop;
         --  Add the CFG to the CFG List
         --  Add (new_cfg => curCFG);
      --end loop;
      --  END Iterates through all cfgs

      if (Global.drawGraphs) then
         CFG_Graph.Export_To_Dot_Single (Graph => curCFG);
      end if;
      --Put ("*");
      XML_POINTER := XML_POINTER + 1;
      Free (Reader);
   end Build_ONE_CFG;

   procedure CFG_Reconstruction (Filename : String) is
   --use ControlFlowGraph;
      Input    : File_Input;
      Reader   : Tree_Reader;
      Doc      : Document;
      List     : Node_List;
      BB_List  : Node_List;
      BBC_List : Node_List;
      --  CFG_Stub_Node : CFG_Vertex;
      Entry_point : Boolean := True;
      Index : Positive := XML_POINTER;
      --Node_Index : Positive;
      Size : Natural := 0;
      -- Time report variables
      Start_Time, End_Time : Ada.Calendar.Time;
      timeInLCT : Duration;
      --use Ada.Integer_Text_IO;
   begin

      --if (Global.E_M = DEBUG) then
        -- Put_Line("ReadXML: " & Filename);
        --end if;
      Put_line("FileName=" & Filename);
      Open (Filename, Input);
      Put_Line ("Open");
      Parse (Reader, Input);
      Put_Line ("Read");
      Close (Input);
      Doc := Get_Tree (Reader);
      List := Documents.Get_Elements_By_Tag_Name (Doc, "cfg");
      Size := Length (List);
      Put_Line ("Here");
      if (Global.E_M in VERBOSE..DEBUG) then
         New_Line;
         Put_line ("   Program name: " & Get_Attribute (Get_Element (Doc), "name"));
         Put_line ("   CFGs: " & Integer'Image(Size));
         New_Line;
      end if;
      -- if it is the first time we access the XML representation of the
      -- problem we need to initialize a few things
      if FIRST_READ then
         --  Initialize the list of CFGs
         --  (one for the root procedure plus one for each called procedure)
         CFG_Graph.Init (progName => Get_Attribute (Get_Element (Doc), "name"),
                         cfg_num  => Size);
         Dominator_Analysis.Init (Size);
         Loop_Tree.Init_trees (Size);
      end if;

      -- Time reporting
      Start_Time := Clock;

      while XML_POINTER <= Size loop
         if Global.tbshootLCT then
            Put (Integer'Image(XML_POINTER) & " - " );
         end if;
         Build_One_CFG (Filename);
         Dominator_Analysis.Single_Loop_Analysis;
--           declare
--              myCFG : CFG_ptr;
--           begin
--              myCFG := Get_CFG (XML_POINTER);
--  --            Put_Line("£");
--              Unset (myCFG);
--
--           end;

         if (XML_POINTER mod 50 = 0) then
            Put ("+");
         end if;

      end loop;
--        if (Global.E_M in VERBOSE..MINIMAL) then
--           Put_line ("................." & );
--        end if;
      -- Time reporting
      End_Time := Clock;
      timeInLCT := (End_Time - Start_Time);
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put_Line ("   LCTs constructed: " &
                   Integer'Image (XML_POINTER));
         Put ("  ..................................... OK ");
         Put_Line ("(" & Duration'Image ((End_Time - Start_time)) & " seconds)");
      else
         Put_Line (" OK (" & Duration'Image ((End_Time - Start_time)) & " seconds)");
      end if;
      --Put ("                     Elapsed time: ");


      -- Apply loop bopunds here
      --  If loop bounds are provided then
      if (Loop_File /= null) then
         --if (Global.E_M in VERBOSE..MINIMAL) then
            Put ("Applying loop bounds...................");
         --end if;

         Apply_Bounds (Loop_File.all);

         --if (Global.E_M = MINIMAL) then
            Put_line (" OK");
         --end if;
      end if;
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put ("Merging trees................");
      else
         Put ("Merging trees..........................");
      end if;

      Start_time := Clock;
      Merge_Trees;
      End_Time := Clock;
      --if (Global.E_M in VERBOSE..MINIMAL) then
         Put (" OK");
         --+Put ("                     Elapsed time: ");
         Put_Line (" (" & Duration'Image ((End_Time - Start_time)) &
                   " seconds)");
         Put ("Computing a Layout ");
      --end if;
      Start_time := Clock;
      --  Adopt the WCG approach if required
      if wcgOpti then
         Put (" [WCG] .............");
         Compute_Layout_WCG;
      else
         Put (" ...................");
         Compute_Layout;
      end if;
      End_Time := Clock;
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put ("....................................... OK");
      else
         Put (" OK");
      end if;

      --+Put ("                     Elapsed time: ");
      Put_Line (" (" & Duration'Image ((End_Time - Start_time)) &
                " seconds)");

      --+Loop_tree.Export_to_Dot_Bounded;

      if tbshootLOP then
         Put ("Exporting .LCT file..");
         Start_Time := Clock;
         -- Analyze LOOP TREE
         Compute_Criticality;
         End_Time := Clock;
         Put ("...............OK (");
         Put_Line (Duration'Image ((End_Time - Start_time)) & " seconds)");
      end if;
      --+Put_line ("OK");
      --Put_Line ("Time for buidling LCTs: " & Duration'Image (timeInLCT) & " seconds.");

   end CFG_Reconstruction;






   procedure Apply_Bounds (fileName : String) is
      use Loop_Tree;
      Input    : File_Input;
      Reader   : Tree_Reader;
      Doc        : Document;
      List       : Node_List;
      Proc_List  : Node_List;
      Loop_List : Node_List;
      C        : Node;
      --E	       : Node;
      L	       : Node;
      curTree  : LT.Tree_Type_Ptr;
      loopNode : LT.Tree_Node_Ptr;
      curProc  : Unbounded_String;
      counter : Positive := 1;
   begin
      if (Global.E_M = DEBUG) then
         Put_Line("ReadXML: " & Filename);
      end if;

      Open (Filename, Input);
      Parse (Reader, Input);
      Close (Input);
      Doc := Get_Tree (Reader);
      --E := Get_Child(Doc, "procedures");
      Proc_List := Documents.Get_Elements_By_Tag_Name (Doc, "procedure");
      --Put_line("List length: " & Integer'Image(Length(Proc_List)));
      for index in 1 .. Length (Proc_List) loop
         C := Item (Proc_List, index -1);
         if (C /=null) then
            if(Global.E_M = DEBUG) then
               Put_line("Procedure: " & Get_Attribute(C, "id"));
               Put_Line("Trees: " & Integer'Image(Loop_Tree.Get_Tree_Num));
               Print_Tree_Names;
            end if;
            curTree := Get_Tree_By_Name(Get_Attribute(C, "id"));
            LT.Unmark_Tree(curTree);
            --Put_line("Procedure: " & LT.Get_Id(curTree));
            Loop_List := Child_Nodes(C);
            --Put_line("Loop list length: " & Integer'Image(Length(Loop_List)));
            --Put_Line("Loops: " & Integer'Image(Get_Loops(curTree)));
            for loopIndex in 1..Length(Loop_List) loop
               L := Item (Loop_List, loopindex -1);
               if (Node_Type(L) = Element_Node) then
                  --Put ("Loop " & Get_Attribute(L, "id"));
                  loopNode := Get_Loop_By_Label(curTree, Get_Attribute(L, "id"));
                  Set_Bound(loopNode, Integer'Value(Node_Value (First_Child (L))));
                  --Put_Line(" " & "bounded by " & Node_Value (First_Child (L)));
                  --LT.Print_Node(loopNode);
               end if;
            end loop;
            --New_Line;
         end if;
      end loop;
      if (Global.drawGraphs) then
         Loop_Tree.Export_To_Dot_Bounded;
      end if;

   end Apply_Bounds;

   procedure Load_Cache (fileName : String) is
      use Cache;
      Input    : File_Input;
      Reader   : Tree_Reader;
      Doc        : Document;
      List       : Node_List;
      Elem_List  : Node_List;
      C        : Node;
      lineS    : Line_Dim;
      setS     : Set_Dim;
      assoc    : Positive;
   begin
      if (Global.E_M = DEBUG) then
         Put_Line("ReadXML: " & Filename);
      end if;

      Open (Filename, Input);
      Parse (Reader, Input);
      Close (Input);
      Doc := Get_Tree (Reader);
      Elem_List := Documents.Get_Elements_By_Tag_Name (Doc, "line");
      C := Item (Elem_List, 0);
      --Put_line("Line size: " & Get_Attribute(C, "size"));
      lineS := Integer'Value(Get_Attribute(C, "size"));
      Elem_List := Documents.Get_Elements_By_Tag_Name (Doc, "set");
      C := Item (Elem_List, 0);
      --Put_line("Set size: " & Get_Attribute(C, "lines"));
      setS := Integer'Value(Get_Attribute(C, "lines"));
      Elem_List := Documents.Get_Elements_By_Tag_Name (Doc, "assoc");
      C := Item (Elem_List, 0);
      --Put_line("Associativity: " & Get_Attribute(C, "ways"));
      assoc := Integer'Value(Get_Attribute(C, "ways"));
      Init_Cache(lineS, setS, assoc);
   end Load_Cache;

   procedure Load_Constraints (fileName : String) is
      use RAM, Ada.Integer_Text_IO;
      Input     : File_Input;
      Reader    : Tree_Reader;
      Doc       : Document;
      List      : Node_List;
      Pool_List : Node_List;
      Proc_List : Node_List;
      C         : Node;
      P		: Node;
      size      : Integer;
      sizeStr   : String (1..8);
   begin
      Open (Filename, Input);
      Parse (Reader, Input);
      Close (Input);
      Doc := Get_Tree (Reader);
      Pool_List := Documents.Get_Elements_By_Tag_Name (Doc, "pool");
      for index in 1 .. Length (Pool_List) loop
         C := Item (Pool_List, index -1);
         if (C /=null and then Get_Attribute(C, "care")="true") then
            -- Care -> Get_Attribute(C, "care");
            Proc_List := Child_Nodes (C);
            for pIndex in 1..Length(Proc_List) loop
               P := Item (Proc_List, pIndex -1);
               if (Node_Type(P) = Element_Node) then
                  -- Procedure -> Get_Attribute(C, "name");
                  --Put ("Proc: " & Get_Attribute(P, "name"));
                  Ada.Strings.Fixed.Move (Get_Attribute (P,"size"),
                                          sizeStr, Ada.Strings.Right,
                                          Ada.Strings.Right,'0');
                  --Put_Line (" - size: " & Get_Attribute (P,"size"));
                  --Put_Line (" Integer'Value(""16#""" & sizeStr);

                  size := Integer'Value("16#" & sizeStr & "#");
                  RAM.Map_Procedure (procName => Get_Attribute(P, "name"),
                                     size     => size);
               end if;
            end loop;
         end if;
      end loop;
      --Set_New_Start_Bin (Get_Ram_Sets'Length);
   end Load_Constraints;


   procedure Save_Constraints (fileName : String) is
      use RAM, Ada.Integer_Text_IO, Utilities,Interactions;
      --Input     : File_Input;
      Output    : File_Type;
      --Reader    : Tree_Reader;
      --Doc       : Document;
      List      : Node_List;
      Pool_List : Node_List;
      Proc_List : Node_List;
      --C         : Node;
      --P         : Node;
      flag : Boolean := True;
      curBin : Bin_Ptr;
      procNum : Positive;
      curDesc : Descriptor;
      prevDesc : Descriptor := Null_Descriptor;
      I : Positive := 1;
      lastProc : String (1..100) := "****************************************************************************************************";
      Fix_String : String (1..8);
      addr : String (1..12);
      sharpIndex : Natural;
      size : Integer := 0;
   begin
--        if (Global.Constrained) then
--           Open (Filename, Input);
--           Parse (Reader, Input);
--           Close (Input);
--           Doc := Get_Tree (Reader);
--           Create (Output, Out_File, "tmp_" & fileName);
--           Put_Line (Output, "<constraints>");
--           Pool_List := Documents.Get_Elements_By_Tag_Name (Doc, "pool");
--           for index in 1 .. Length (Pool_List) loop
--              C := Item (Pool_List, index -1);
--              if (C /=null) then
--                 Put_Line (Output, "  <pool care=""" & Get_Attribute(C, "care") & """>");
--                 Proc_List := Child_Nodes (C);
--                 for pIndex in 1..Length(Proc_List) loop
--                    P := Item (Proc_List, pIndex -1);
--                    if (Node_Type(P) = Element_Node) then
--                       Put (Output, "    <proc name=""" & Get_Attribute(P, "name") & """");
--                       Put_Line (Output, " size=""" & Get_Attribute(P, "size") & """/>");
--                    end if;
--                 end loop;
--              end if;
--              Put_Line (Output, "  </pool>");
--           end loop;
--        else
         Create (Output, Out_File, "tmp_" & fileName);
         Put_Line (Output, "<constraints>");
--      end if;

         --C := Create_Element (Doc, "pool");
         --Set_Attribute(Elem  => C,
         --              Name  => "care",
         --              Value => "true");
         --P := Append_Child (N => Get_Element (Doc),
         --                   New_Child => C);
      Put_Line (Output, "  <pool care=""true"">");
      while flag loop
         curBin := Get_Ram_Sets (I);
         procNum := curBin.Proc'Length;

         for pr in 1..procNum loop
            curDesc := curBin.Proc(pr);
            --  Analyising a new procedure --> wirte the old one
            if (curDesc.name /= lastProc and
                  lastProc /= "****************************************************************************************************") then
               --Put_LIne("$$$$$$$$$$$$$");
               --                 Set_Attribute (Elem  => P,
               --                                Name  => "proc",
               --                                Value => Trim(curDesc.name));
               --                 Set_Attribute (Elem  => P,
               --                                Name  => "size",
               --                                Value => Integer'Image(curDesc.size));
               Put (Output, "    <proc name=""" & Trim(prevDesc.name) & """");
               Put (Output, " size=""");

               T_IO.Put (To   => addr,
                         Item => size,--prevDesc.size,
                         Base => 16);
               Ada.Strings.Fixed.Move (addr,
                                       Fix_String, Ada.Strings.Left,
                                       Ada.Strings.Left,'0');
               sharpIndex := First_Index (Src => Fix_String,
                                          Ch  => '#')+1;
               --Put_Line(" " & Integer'Image(sharpIndex));
               Put_line (Output, Fix_String(sharpIndex..7) &  """/>");
               lastProc := curDesc.name;
               prevDesc := curDesc;
               size := curDesc.size;
            else
               size := size + curDesc.size;
               lastProc := curDesc.name;
               prevDesc := curDesc;
            end if;
         end loop;
         if curBin.State /= FULL then
            flag := false;
            Put (Output, "    <proc name=""" & Trim(curDesc.name) & """");
               Put (Output, " size=""");
               T_IO.Put (To   => addr,
                         Item => size,--prevDesc.size,
                         Base => 16);
               Ada.Strings.Fixed.Move (addr,
                                       Fix_String, Ada.Strings.Left,
                                       Ada.Strings.Left,'0');
               sharpIndex := First_Index (Src => Fix_String,
                                          Ch  => '#')+1;
               --Put_Line(" " & Integer'Image(sharpIndex));
               Put_line (Output, Fix_String(sharpIndex..7) &  """/>");
         end if;
         I := I + 1;
      end loop;

      Put_Line (Output, "  </pool>");
      Put_line (Output, "</constraints>");

      Close (Output);
      --Interactions.Normalize ("tmp_" & fileName);
      if (Global.Constrained) then
         Copy (fileName, Get_Bk_name(fileName));
      end if;
      Replace (fileName, "tmp_" & fileName);
--      Set_name
  --    Set_Attribute (Elem  => OP,
    --                 Name  => ,
      --               Value => )
   end Save_Constraints;

   -- **FIXME**
   function Get_Bk_name (fileName : String) return String is
      use Utilities;
      ind : Natural := Last_Index (fileName, '.');
   begin
      Back_Up := Back_Up +1;
--        Put_Line("-> " & fileName (fileName'First..ind-1) & "_" &
--                 Ada.Strings.Fixed.Trim (Integer'Image(Back_Up), Ada.Strings.Left) & ".xml");
      return fileName (fileName'First..ind-1)
        & "_" & Ada.Strings.Fixed.Trim (Integer'Image(Back_Up),
                                        Ada.Strings.Left) & ".xml";

   end Get_Bk_name;



end Xml_Parser;
