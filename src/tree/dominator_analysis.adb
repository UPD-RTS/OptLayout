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
-- FILE NAME      : dominator_analysis.adb
-- PACKAGE        : DOMINATOR_ANALYSIS body
-- PURPOSE        : Representation of the dominator tree data structure
--                  used to detect loops and build the LCT
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with CFG_Graph; use CFG_Graph;
with Global; use Global;
--with Graph;
with Ada.Strings.Unbounded.Text_IO;
with Tree;
with Interactions; use Interactions;
with Ada.Exceptions; use Ada.Exceptions;
with Loop_Tree, Utilities;
use Utilities;
use Loop_tree;

package body Dominator_Analysis is
   use Ada.Strings.Unbounded;

   package CFG renames CFG_Graph;

   package DT renames Dominator_Tree;

   package Loops renames Loop_Tree.LT;

   Special : array (1..11) of Unbounded_String := (To_Unbounded_String(".umul"),
                                                  To_Unbounded_String("ostrales_begin_service"),
                                                  To_Unbounded_String("ostrales_end_service"),
                                                  To_Unbounded_String("ostrales_give_svc"),
                                                  To_Unbounded_String(".urem"),
                                                  To_Unbounded_String(".udiv"),
                                                  To_Unbounded_String("COM_enter_critical_section"),
                                                  To_Unbounded_String("COPRES_set_bit_register"),
                                                  To_Unbounded_String("COPRES_clear_bit_register"),
                                                  To_unbounded_String("COPRES_set_register"),
                                                  To_Unbounded_String("COM_leave_critical_section"));


   Special_Size : array (1..11) of String (1 .. 8) := ("00000078", "00000064", "00000064", "00000018",
                                                       "0000000c", "0000000c", "00000030", "00000024",
                                                       "00000024", "00000018", "00000030");

   function isSpecial (us : Unbounded_String) return Integer is
   begin
      for I in Special'Range loop
         if Utilities.UTrim(Special(I)) = Utilities.UTrim(us) then
            return I;
         end if;
      end loop;
      return 0;
   end isSpecial;


   CFG_Index : Positive := 1;


   --  Implementation of Generic procedures and functions
   function "=" (Left, Right: Dom_Node_Info) return Boolean is
   begin
      return Left.Id = Right.Id;
   end "=";

   --  Merge the Info.Id of two nodes
   procedure Merge (Left, Right: in out Dom_Node_Info) is
   begin
      Left.Id := (Left.Id & Right.Id);
   end Merge;

   procedure Print (N : Dom_Node_Info) is
   begin
      Put_Line ("Node: " & Ada.Strings.Unbounded.To_String(N.procName) &
           "::" & Ada.Strings.Unbounded.To_String(N.Id));
   end Print;

   procedure PrettyPrint (N : Dom_Node_Info) is
      use Utilities;
   begin
      Put ("Node: " & UTrim(N.Id));
   end PrettyPrint;

   --  RootNode, ExitNode, BB, Stub, UndefNode
   function Print_Nt (nt : Dom_Node_Type) return String is
   begin
      if (nt = RootNode) then
         return "Root";
      elsif (nt = ExitNode) then
         return "Exit";
      elsif (nt = BB) then
         return "BB";
      elsif (nt = Stub) then
         return "Stub";
      else
         return "Undef";
      end if;
   end Print_Nt;

   --  Return node info
   function Get_Node_Info (n : Tree_Node_Ptr) return Dom_Node_Info is
   begin
      return Get_Info(n);
   end Get_Node_Info;


   --  Call, Taken, Nottaken
   function Print_Lt (lt : DOm_Link_Type) return String is
   begin
      if (lt = Call) then
         return "Call";
      elsif (lt = Taken) then
         return "Taken";
      else
         return "Nottaken";
      end if;
   end Print_Lt;

   --  Prints a list of dominator
   procedure Print_Dom_List (list : Dominator_list; size: positive) is
   begin
      --New_Line;
      Put("DomList: ");
      for index in 1..size loop
         -- Short format
         Put (Integer'Image(index));
         if (list(index)) then
            Put ("T");
         else
            Put ("F");
         end if;
         --Put(Boolean'Image(list(index)));
         if(index/=size) then
            Put(",");
         end if;
      end loop;
      New_Line;
   end Print_Dom_List;

   --  Tests whether two dominator list have any element in common
   function eq (Left, Right : Dominator_List; size : Positive ) return Boolean is
      --equals : Boolean;
   begin
      for Index in 1..size loop
         if (Left(index)/=Right(Index)) then
            if (Global.E_M in VERBOSE..DEBUG) then
               Put_Line("*** EQ ***");
               Print_Dom_List(Left, size);
               Put_line("----------------------------------");
               Print_Dom_List(Right, size);
               Put_Line("***      ***");
               Put_line(" " & Boolean'Image(Left(index)) & "!=" &
                        Boolean'Image(Right(index)));
               Put_Line("*** end EQ ***");
            end if;
            return False;
         end if;
      end loop;
      return True;
   end eq;


   --  Checks whether a node is dominated by another node
   --  (index-th entry in its dominator list is TRUE)
   function IsDominated(list : Dominator_List;
                        --size : Positive;
                        index : Positive) return Boolean is
   begin
      return list(index);
   end IsDominated;


   procedure Dominator_And (Left : in out Dominator_List;
                            Right : Dominator_List;
                            size : Positive;
                            index : Positive;
                            ch : in out Boolean) is
      newDom : Dominator_List (1..Left'Length);
   begin
      --Put("-");
      --  Assuming Left'Size = Right'Size
      --Put_Line("Size= " & Integer'Image(size));
      for Index in 1..size loop
         newDom (Index) := (Left(Index) and Right(Index));
      end loop;
      --if (ch = False) then
         newDom (index) := True;
         ch := not (eq(newDom, Left, size));
      --end if;
      --Put("Left- ");Print_Dom_List(Left, size);
      --Put("Right-");Print_Dom_List(Right, size);
      --Put("Res-  ");Print_Dom_List(newDom, size);
      Left := newDom;
      -- Preserve self-domination
      if(ch) and (Global.E_M in VERBOSE..DEBUG) then
         Put_line("Res-OLD ");Print_Dom_List(Left, size);
         Put_line("Res-PRED ");Print_Dom_List(Right, size);
         Put_line("Res-AND  ");Print_Dom_List(newDom, size);
         Put_Line("**********************************");
      end if;


      --New_line;
      --Put_line("--> " & Boolean'Image(ch));
   end Dominator_And;


   --  Prints information on the successors of the given node
   procedure Print_Succ (curr : CFG_Vertex; cofg : in out CFG_Type) is
      Child_Num  : Natural := CFG.Successors_Num (Graph  => cofg,
                                             Vertex => curr);
      Child_List : CFG.Vertex_List (1..Child_Num);
      Anode : CFG_Vertex;
   begin
      --Put_Line("In Print_Succ");
      if (Child_Num /= 0) then
         Child_List := CFG.Successors (cofg, curr);
         Anode := Child_List(1);
         declare
            New_Child_Num : Natural := CFG.Predecessors_Num (cofg, Anode);
            New_Child_List : CFG.Vertex_List (1..New_Child_Num) := CFG.Predecessors (cofg, Anode);
         begin
            Print_Vertex (New_Child_List(1));
         end;

      end if;
   end Print_Succ;


   --  Recursively traverse a CFG in a bottom-up way
   --  following a single path towards the first dominator
   --  In the best case the recursion depth is
   --  the shortest path from the analysed node to
   --  its immediate dominator. In the worst case its
   --  bounded by the longest path.
   function Find_Immediate_Dominator_Rec (curCFG : CFG_ptr;
                                          dom : Dominator_List;
                                          size : Positive;
                                          index : Positive) return Natural is
      CFG_Node : CFG_Vertex := Get_Vertex_By_Index(curCFG, index,
                                         curCFG.Number);--CFG_Graph.Connectivity);
   begin
      declare
         predNum : Natural := CFG.Predecessors_Num(curCFG.all,
                                                   CFG_Node);
         P_Node : CFG_Vertex;
      begin
         -- for dom in Index..size loop
         --              null;
         --           end loop;
         if (predNum > 0) then
            declare
               predList : CFG.Vertex_List (1..predNum) :=
                 CFG.Predecessors(curCFG.all,CFG_Node);
               predIndex : Natural;
            begin
               --  Check if one of the predecessor is
               --  in Dominator (of the original node)
               for predec in 1..predNum loop
                  P_node := predList(predec);
                  --if (P_Node.typ /= Root) then
                  predIndex := CFG.Index_Of(curCFG.all, P_node);
                  if (dom(predIndex)=True) then
                     return predIndex;
                  end if;
                  --end if;
               end loop;

               --  Otherwise if none of the predecessors is a dominator
               --  then we need to look recursively at the predecessors
               --  of each predecessor.
               --  As per the [Find_Immediate_Dominator] procedure,
               --  it is sufficient to pick a predecessor and follow
               --  its path.
               --  Sooner or later we will step into a dominator
               --  and it will be the immediate one

               --for predec in 1..predNum loop
               P_node := predList(1);--predec);
               --if (P_Node.typ /= Root) then
               predIndex := CFG.Index_Of(curCFG.all, P_node);
               predIndex := Find_Immediate_Dominator_Rec (curCFG, dom,
                                                          size, predIndex);
               if (predIndex /= 0) then
                  return predIndex;
               end if;
               --end if;
               --end loop;
            end;
         end if;
         return 0;
      end;
      exception
      when Storage_Error =>
         Put_Line("2 -STORAGE ERROR!!!!!!!!!!");
         raise;
      return 0;
   end Find_Immediate_Dominator_Rec;

   --  Returns the immediate dominator for a given node
   function Find_Immediate_Dominator (curCFG : CFG_ptr;
                                      dom : Dominator_List;
                                      size : Positive;
                                      index : Positive) return Natural is
      CFG_Node : CFG_Vertex := Get_Vertex_By_Index(curCFG, index,
                                         curCFG.Number);--CFG_Graph.Connectivity);
   begin
      declare
         predNum : Natural := CFG.Predecessors_Num(curCFG.all,
                                                   CFG_Node);
         P_Node : CFG_Vertex;
      begin
         -- for dom in Index..size loop
         --              null;
         --           end loop;
         if (predNum > 0) then
            declare
               predList : CFG.Vertex_List (1..predNum) :=
                 CFG.Predecessors(curCFG.all,CFG_Node);
               predIndex : Natural;
            begin
               --  Look at predecessors
               --  if CFG_Node has exactly 1 predecessor P_Node
               --  then P_Node is a inherently a dominator and
               --  moreover it is the Immediate dominator of CFG_Node
               if (predNum = 1) then
                  P_Node := predList(1);
                  --  Return the index of P_Node
                  return CFG.Index_Of(curCFG.all, P_node);
               end if;

               --  This part is needed since, even if CFG_Node has
               --  more than 1 predecessor, exactly one of them can
               --  still be a dominator (and the immediate one)
               for predec in 1..predNum loop
                  P_node := predList(predec);
                     predIndex := CFG.Index_Of(curCFG.all, P_node);
                     if (dom(predIndex)=True) then
                        return predIndex;
                     end if;
               end loop;

               --  If none of the predecessors was a dominator
               --  then we need to look at the predecessors
               --  of each predecessor
               --  [RECURSIVE sub-procedure}
               --  Actually it is sufficient to pick a
               --  a predecessor and follow its path.
               --  Sooner or later we will step into a dominator
               --  and it will be the immediate one

               --for predec in 1..predNum loop
               P_node := predList(1);
               predIndex := CFG.Index_Of(curCFG.all, P_node);
               predIndex := Find_Immediate_Dominator_Rec (curCFG, dom,
                                                          size, predIndex);
               if (predIndex /= 0) then
                  return predIndex;
               end if;

               --end loop;

            end;
         end if;
      end;
      Put_LINE("ATTENZIONE!!!!!!!!!!!!");
      return 0;
      exception
      when Storage_Error =>
         Put_Line("1 - STORAGE ERROR!!!!!!!!!!");
         raise;
         return 0;

   end Find_Immediate_Dominator;

   function To_Dom_Node_Type (t : BB_Type) return Dom_Node_Type is
   begin
      --  From : Root, Stub, Simple, Leaf
      --  To   : RootNode, ExitNode, BB, Stub, UndefNode)
      if (t=Simple) then
         return BB;
      elsif (t=Stub) then
         return Stub;
      elsif (t=Leaf) then
         return ExitNode;
      else
         return UndefNode;
      end if;
   end To_Dom_Node_Type;

   function To_Dom_Link_Type (t : Link_Type) return Dom_Link_Type is
   begin
      --  From : Call, Taken, Nottaken
      --  To   : None, Taken, Nottaken, Call
      if (t=Call) then
         return Call;
      elsif (t=Taken) then
         return Taken;
      elsif (t=Nottaken) then
         return Nottaken;
      else
         return None;
      end if;
   end To_Dom_Link_Type;


   --  Populates a dominator tree starting with the root node
   procedure Populate_Dom_Tree(currDomTre    : in out DT.Tree_Type_Ptr;
                               curNode       : in out DT.Tree_Node_Ptr;
                               curNode_index : Positive;
                               curCFG_Ptr    : CFG_ptr;
                               curCFG_Num    : Positive;
                               domArray      : Imm_Dominators) is
      --Child_num : Natural :=
      Child_Node : CFG_Vertex;
      childNodeInfo  : Dom_Node_Info;
      childNode : DT.Tree_Node_ptr;
      nType : Dom_Node_Type;
      --lType : Dom_Link_Type;
   begin
      --Put(". ");
      --  Iterates over all nodes
      for index in 1..curCFG_Num loop
         --  The a node is immediately dominated by curNode
         if (domArray(index)=curNode_index) then
            -- Create a new node and add it to curNode children
            Child_Node := Get_Vertex_By_Index(cfg   => curCFG_ptr,
                                              index => index,
                                              dim   => curCFG_Ptr.Number);--Connectivity);
            childNodeInfo := (Id => To_Unbounded_String(Child_Node.BB),
                            CfgId => CFG.Get_Id(curCFG_ptr.all),
                            ProcName => To_Unbounded_String(
                                CFG.Get_Name(curCFG_ptr.all)));
            --  Compute node and link type
            nType := To_Dom_Node_Type(Child_Node.typ);
            --lType := To_Dom_Link_Type(Child_node);
            childNode := Create_Node(childNodeInfo, nType, None);
            Add_Succ(Parent => curNode,
                     Child  => childNode);

            --Put_LIne("Here");Print_Node(Get_Info(curNode));Print_Node(Get_Info(childNode));

            if (Global.E_M = VERBOSE) then
               Put_Line ("Child node added");
            end if;

            -- Procceed recursively
            Populate_Dom_Tree(currDomTre, childNode, index,
                              curCFG_Ptr, curCFG_Num, domArray);

         end if;
      end loop;
      exception
      when Error : Storage_Error =>
         Put_Line("Populate STORAGE ERROR!!!!!!!!!!");
         raise;
   end Populate_Dom_Tree;

   procedure Export_To_Dot_Rec (output : in out File_type;
                                domTreeList: Dom_Tree_Array_ptr;
                                domNode : Tree_Node_Ptr;
                                size : Positive) is
      use Global;
      use DT;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      node_ptr    : DT.Tree_Node_Ptr;
      nodeInfo    : Dom_Node_Info;
      childNum    : Natural := Get_Succ_Number (domNode);
      succSize    : Positive := Get_Succ_Size(domNode);
   begin
      --Put_Line("In ExportDotRec");
      if(childNum>0) then
         declare
            children : Tree_Node_List (1..succSize) := Get_Succ(domNode);
            parentNodeInfo : Dom_Node_Info := Get_Info (domNode);
         begin
            for index in 1..childNum loop
               node_ptr := children(index);
               if ( not Is_Marked(node_ptr)) then
                  --Put_Line("node is not marked");
                  nodeInfo := Get_Info (node_ptr);
                  if (Get_LType(node_ptr) = Call) then

                     Put_Line (output, To_String(nodeInfo.Id) & "[shape=box]" &
                               "[label=""" & To_String(nodeInfo.Id) & "\n"
                               & To_String(nodeInfo.ProcName) & "\n(" &
                               Print_Nt(Get_NType(node_ptr)) &
                               ")"", style=dotted];");
                  else
                     --Put_Line("Adding to dot");
                     Put_Line (output, To_String(nodeInfo.Id) & --"[shape=box]" &
                               "[label=""" & To_String(nodeInfo.Id) & "\n" &
                               To_String(nodeInfo.ProcName) & "\n(" &
                               Print_Nt(Get_NType(node_ptr)) & ")""];");
                  end if;

                  Mark_Node (node_ptr);
               end if;
               Export_To_Dot_Rec (output, domTreeList, node_ptr, size);

               --  Add an dege between the two nodes
               Put_Line (output, To_String(parentNodeInfo.Id) & "-> " &
                         To_String(nodeInfo.Id) & ";");
            end loop;
         end;
      end if;
   exception
      when Error: Storage_Error =>
         Put_Line("Export Dot REC --> STORAGE ERROR!!!!!!!!!!");
         raise;
      when Error: others =>
         Put_line(Exception_Name(Error));
         raise;
   end Export_To_Dot_Rec;



   procedure Export_To_Dot (domTreeList: Dom_Tree_Array_ptr; trees : Positive;
                            size : Positive) is
      use Global;
      use DT;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      curTree_ptr : DT.Tree_Type_Ptr;
      curTree 	  : DT.Tree_Type;
      node_ptr    : DT.Tree_Node_Ptr;
      --node    	  : DT.Tree_Node;
      nodeInfo    : Dom_Node_Info;
   begin
      Create (Output, Out_File, To_String(Global.Program)&"_DOM.dot");
      Put_line(Output, "/* DOT Graph description");
      Put_line(Output, " * Dominator tree");
      Put_line(Output, " * Generated by OptLayout");
      Put (Output, " * Main program: ");
      Put_Line (Output, Global.Program);
      Put_line(Output, " */");
      Put_line(Output, "digraph GFG {");
      Put_Line(Output, "size = """ & Integer'Image(size) &
               "," & Integer'Image(size) & """;");
      for tree in 1..trees loop
         if (Global.E_M = VERBOSE) then
            Put("- " & Integer'Image(tree));
         end if;

         curTree_ptr := domTreeList (tree);
         curTree := curTree_ptr.all;
         node_ptr := Get_Root(curTree);
         --node := node_ptr.all;
         --  If the node has not been printed yet
         if (not Is_marked(node_ptr) or tree = 1) then
            if (Global.E_M = VERBOSE) then
               Put_line ("Adding a node to DOT");
            end if;

            nodeInfo := Get_Info (node_ptr);
            Put_Line (Output, To_String(nodeInfo.Id)
                & "[shape=box]" &
                  "[label=""" & To_String(nodeInfo.Id) & "\n" & To_String(nodeInfo.ProcName) &
                      "\n(" & Print_Nt(Get_NType(node_ptr)) & ")""];");
            Mark_Node (node_ptr);
         end if;
         Export_To_Dot_Rec(output,
                           domTreeList,
                           node_ptr,
                           size);

            --Put("*" & Integer'Image(tree));
      end loop;

      Put_line(Output, "}");
      Put_line(Output, "/* End of file */");
      Close (Output);
      Execute_Dot (To_String(Global.Program) & "_DOM");
   exception
      when Error : Storage_Error =>
         Put_Line("Export STORAGE ERROR!!!!!!!!!!");
         raise;
      when Error:others => Put_Line(Exception_Message(Error));
         raise;

   end Export_To_Dot;

   procedure Export_To_Dot_Single (domTreeList: Dom_Tree_Array_ptr; trees : Positive;
                            size : Positive; cfgInd : Positive) is
      use Global;
      use DT;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      curTree_ptr : DT.Tree_Type_Ptr;
      curTree 	  : DT.Tree_Type;
      node_ptr    : DT.Tree_Node_Ptr;
      --node    	  : DT.Tree_Node;
      nodeInfo    : Dom_Node_Info;
   begin
      Create (Output, Out_File, To_String(Global.Program)&"_DOM.dot");
      Put_line(Output, "/* DOT Graph description");
      Put_line(Output, " * Dominator tree");
      Put_line(Output, " * Generated by OptLayout");
      Put (Output, " * Main program: ");
      Put_Line (Output, Global.Program);
      Put_line(Output, " */");
      Put_line(Output, "digraph GFG {");
      Put_Line(Output, "size = """ & Integer'Image(4*size) &
               "," & Integer'Image(4*size) & """;");
      for tree in 1..trees loop
         if(tree=cfgInd) then
         if (Global.E_M = VERBOSE) then
            Put("- " & Integer'Image(tree));
         end if;

         curTree_ptr := domTreeList (tree);
         curTree := curTree_ptr.all;
         node_ptr := Get_Root(curTree);
         --node := node_ptr.all;
         --  If the node has not been printed yet
         if (not Is_marked(node_ptr) or tree = 1) then
            if (Global.E_M = VERBOSE) then
               Put_line ("Adding a node to DOT");
            end if;

            nodeInfo := Get_Info (node_ptr);
            Put_Line (Output, To_String(nodeInfo.Id)
                & "[shape=box]" &
                  "[label=""" & To_String(nodeInfo.Id) & "\n" & To_String(nodeInfo.ProcName) &
                      "\n(" & Print_Nt(Get_NType(node_ptr)) & ")""];");
            Mark_Node (node_ptr);
         end if;
         Export_To_Dot_Rec(output,
                           domTreeList,
                           node_ptr,
                           size);

            --Put("*" & Integer'Image(tree));
         end if;

      end loop;

      Put_line(Output, "}");
      Put_line(Output, "/* End of file */");
      Close (Output);
      Execute_Dot (To_String(Global.Program) & "_DOM");
   exception
      when Error : Storage_Error =>
         Put_Line("Export STORAGE ERROR!!!!!!!!!!");
         raise;
      when Error:others => Put_Line(Exception_Message(Error));
         raise;


   end Export_To_Dot_Single;


   procedure Export_To_Dot_Part (domTreeList: Dom_Tree_Array_ptr; trees : Positive;
                            size : Positive; cfgInd : Positive) is
      use Global;
      use DT, Utilities;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      Input, Output : File_Type;
      curTree_ptr : DT.Tree_Type_Ptr;
      curTree 	  : DT.Tree_Type;
      node_ptr    : DT.Tree_Node_Ptr;
      --node    	  : DT.Tree_Node;
      nodeInfo    : Dom_Node_Info;
   begin
      Create (Output, Out_File, "cfg" & Trim(cfgInd)
              & "_" & To_String(Global.Program)&"_DOM.dot");
      Put_line(Output, "/* DOT Graph description");
      Put_line(Output, " * Dominator tree");
      Put_line(Output, " * Generated by OptLayout");
      Put (Output, " * [CFG " & Integer'IMage(cfgInd) & "] " );
      Put_Line (Output, Global.Program);
      Put_line(Output, " */");
      Put_line(Output, "digraph GFG {");
--        Put_Line(Output, "size = """ & Integer'Image(4*size) &
--                 "," & Integer'Image(4*size) & """;");
      Put_Line(Output, "size = "" 10, 7.5""");
      Put_Line(Output, "ratio =""auto""");
      Put_Line(Output, "orientation = ""landscape""");

      for tree in 1..trees loop
         if(tree=cfgInd) then
         if (Global.E_M = VERBOSE) then
            Put("- " & Integer'Image(tree));
         end if;

         curTree_ptr := domTreeList (tree);
         curTree := curTree_ptr.all;
         node_ptr := Get_Root(curTree);
         --node := node_ptr.all;
         --  If the node has not been printed yet
         if (not Is_marked(node_ptr) or tree = 1) then
            if (Global.E_M = VERBOSE) then
               Put_line ("Adding a node to DOT");
            end if;

            nodeInfo := Get_Info (node_ptr);
            Put_Line (Output, To_String(nodeInfo.Id)
                & "[shape=box]" &
                  "[label=""" & To_String(nodeInfo.Id) & "\n" & To_String(nodeInfo.ProcName) &
                      "\n(" & Print_Nt(Get_NType(node_ptr)) & ")""];");
            Mark_Node (node_ptr);
         end if;
         Export_To_Dot_Rec(output,
                           domTreeList,
                           node_ptr,
                           size);

            --Put("*" & Integer'Image(tree));
         end if;

      end loop;

      Put_line(Output, "}");
      Put_line(Output, "/* End of file */");
      Close (Output);
      Execute_Dot ("cfg" & Trim(cfgInd)
              & "_" & To_String(Global.Program)&"_DOM");
   exception
      when Error : Storage_Error =>
         Put_Line("Export STORAGE ERROR!!!!!!!!!!");
         raise;
      when Error:others => Put_Line(Exception_Message(Error));
         raise;


   end Export_To_Dot_Part;



   --  Return TRUE if the node indexed by nodeIndex is
   --  a dominator of the current node
   function Is_Back_Edge (dom : Dominator_List;
                          nodeIndex : Positive
                         ) return Boolean is
   begin
      return dom(nodeIndex);
   end Is_Back_Edge;

   -- Add unique bb to the loop BB list
   procedure Add_Unique_BB (bbs : in out Loop_Tree.Loop_BBs_Ptr;
                            count : in out Positive;
                            bb : Positive) is
      newBBs : Loop_Tree.Loop_BBs_Ptr;
      eraseBBs : Loop_Tree.Loop_BBs_Ptr := bbs;
      IsNew  :Boolean := True;
   begin
      for oldBB in 1..count loop
         if (Global.E_M = DEBUG) then
            Put ("[" & Integer'Image(bbs(oldBB)) & "=" &  Integer'Image(bb) & "?]");
         end if;

         if bbs(oldBB) = bb then
            if (Global.E_M = DEBUG) then
               Put_Line ("Already in Loop BBs: Node# " & Integer'Image(bbs(oldBB)));
            end if;

            IsNew := False;
         end if;
      end loop;
      if (IsNew) then
         newBBs := new Loop_Tree.Loop_BBs (1..count+1);
         for index in 1..count loop
            newBBs(index) := bbs (index);
         end loop;
         if (Global.E_M = DEBUG) then
            Put_Line("No! Growing: + " & Integer'Image(bb));
         end if;
         newBBs (count+1) := bb;
         Loop_Tree.Free_Loop_BBs (eraseBBs);
         bbs := newBBs;
         count := count +1;

      end if;
   exception
      when Error: others =>
         Put_Line("Add_Unique_BB -> " &Exception_Name(Error));
         raise;
   end Add_Unique_BB;



   procedure Compute_BBs_Rec (bbs : in out Loop_Tree.Loop_BBs_Ptr;
                              curCFG : CFG_ptr;
                              count : in out Positive;
                              from, to : in CFG_Vertex) is
   begin
      declare
         predNum : Natural := CFG.Predecessors_Num(curCFG.all, to);
         predList : CFG.Vertex_List (1..predNum) :=
           CFG.Predecessors(curCFG.all,to);
         P_Node : CFG_Vertex;
      begin
         if (Global.E_M = DEBUG) then
            Put("Rec ");
         end if;

         for pred in 1..predNum loop
            P_Node := predList (pred);

            --CFG_Graph.Print_Vertex_Name (P_Node);

            if(P_Node /= From and not CFG.Marked(curCFG.all,P_Node)) then
               Add_Unique_BB (bbs, count, CFG.Index_Of(curCFG.all, P_Node));
               CFG.Mark_Vertex(curCFG.all,P_Node);
               --if (P_Node /= From) then
                  Compute_BBs_Rec (bbs, curCFG, count, from, P_Node);
               --nd if;
            end if;
         end loop;
         --Put_Line ("End rec");
      end;
   end Compute_BBs_Rec;


   --  Compute_BBs (curLoopBB, curCFG_ptr, count,
   --               S_Node, CFG_Node);
   --  From: successor (loop header)
   --  To: node in the loop with a back edge
   procedure Compute_BBs (bbs : in out Loop_Tree.Loop_BBs_Ptr;
                          curCFG : CFG_ptr;
                          count : in out Positive;
                          from, to : in CFG_Vertex) is
   begin
      declare
         predNum : Natural := CFG.Predecessors_Num(curCFG.all, to);
         predList : CFG.Vertex_List (1..predNum) :=
           CFG.Predecessors(curCFG.all,to);
         P_Node : CFG_Vertex;
      begin
         if (Global.E_M = DEBUG) then
            Put ("From: "); Print_Vertex_Info(From);
            Put (" to: "); Print_Vertex_Info(to);
         end if;

         -- Add the "to" and "from" nodes themselves to the loop bbs list
         bbs := new Loop_Tree.Loop_BBs (1..2);
         bbs (1) := CFG.Index_Of(curCFG.all, From);
         bbs (2) := CFG.Index_Of(curCFG.all, to);
         --  Mark the header node as visited
         --  to avoid going beyond the loop header
         CFG.Mark_Vertex(curCFG.all,From);
         count := 2;
         for pred in 1..predNum loop
            P_Node := predList (pred);
            --+ if(P_Node /= From and not CFG.Marked(curCFG.all,P_Node)) then
            if (not CFG.Marked(curCFG.all,P_Node)) then
               if (Global.E_M = DEBUG) then
                  Put_Line("Is not marked");
               end if;
               --Put ("-Adding "); Print_Vertex_Name(P_Node);
               Add_Unique_BB (bbs, count, CFG.Index_Of(curCFG.all,P_Node));
               CFG.Mark_Vertex(curCFG.all,P_Node);
	       --Put_line("Entering in Compute_BBs_Rec");
               Compute_BBs_Rec (bbs, curCFG, count, from, P_Node);
            end if;
            if (Global.E_M = DEBUG) then
               Put_Line("Is marked!");
            end if;

         end loop;
      end;

   end Compute_BBs;


   -- Updates a loop node basic blocks, avoiding duplicates
   procedure Update_BBs (parent, child :  in out Loops.Tree_Node_Ptr) is
      use Utilities;
      pInfo : Loop_Tree.Loop_Node_Info := Loop_Tree.Get_Node_Info(parent);
      cInfo : Loop_Tree.Loop_Node_Info := Loop_Tree.Get_Node_Info(child);
      pBBnum : Natural := pInfo.BBs_num;
      cBBnum : Natural := cInfo.BBs_num;
      common : Boolean := False;
      index : Natural := 1;
      domIndex : Positive := cInfo.Index;
      pBB, cBB : Positive;
      pBBlist, cBBlist, newBBs : Loop_Tree.Loop_BBs_Ptr;
   begin
--        if (pBBnum = 0) then
--           Put_Line("pBBnum");
--        end if;

      --Put("--------" & UTrim(pInfo.Id));
      --Print_BBs(parent);
      --Print_BBs(child);
      --Put("--------" & UTrim(cInfo.Id));
      --newBBs := new Loop_Tree.Loop_BBs (1..(pBBnum-cBBnum));
      --New_Line;
      --Put_Line("trace 1");
      newBBs := new Loop_Tree.Loop_BBs (1..pBBnum);
      pBBlist := pInfo.BBs;
      cBBlist := cInfo.BBs;
      for pIndex in 1..pBBnum loop
         common := False;
         pBB := pBBlist(pIndex);
         for cIndex in 1..cBBnum loop
            cBB := cBBlist(cIndex);
            if (pBB = cBB) then
               common:=True;
            end if;
         end loop;
--         if (not common) then
         if (not common and pBB/=domIndex) then

            newBBs (index) := pBB;
            index := index +1;
         end if;
      end loop;
      if Global.tbshootLCT then
         Put_Line("trace. 'index' =" & INteger'Image(index));
      end if;

      Loop_tree.Set_BBs(parent, newBBs, index-1);

   end Update_BBs;



   --  Performs dominator analysis to detect loops
   procedure Loop_Analysis is
      use Loops;
      --  Number of CFGs in memory
      cfgNum : Natural := Get_CFG_Num;
      --  Pointer to the current CFG
      curCFG_Ptr : CFG_ptr;
      --  Number of VERTICES in the current CFG
      curCFG_Num : Natural;
      --  INFO of the current Tree Node
      curNodeInfo  : Dom_Node_Info;
      --  INFO of the child Tree Node
      childNodeInfo  : Dom_Node_Info;
      --  INFO of the current LoopTree node
      rootLoopInfo : Loop_Tree.Loop_Node_Info;
      --  INFO of the current LoopTree node
      childLoopInfo : Loop_Tree.Loop_Node_Info;
      --  Pointer to the current LoopTree Node
      rootLoopNode : Loops.Tree_Node_Ptr;

      tempLoopNode : Loops.Tree_Node_Ptr;
      tempLoopNodeInfo : Loop_Tree.Loop_Node_Info;
      --  Ppointer to the current Loop Tree
      curLoopTree : Loops.Tree_Type_Ptr;
      --  Numebr of successors
      succNum : Natural;
      --  Dimension of the list of successors
      succlistSize : Natural;
      --  Index (in CFG) of the Immediate dominator of the current node
      curIDom : Natural;
      -- Pointer to a Vertex List
      --vList_ptr : CFG.Vertex_List_Ptr;
      --  Pointer to the current LoopTree Node
      childLoopNode : Loops.Tree_Node_Ptr;
      --  Pointer to the current Tree Node
      curNode : DT.Tree_Node_ptr;
      --  Pointer to the current Dominator tree
      currDomTree : DT.Tree_Type_Ptr;
      --  Current node of the CFG
      CFG_Node : CFG_Vertex;
      --  Predecessor node of the current node of the CFG
      CFG_Pred : CFG_Vertex;

      --  Index of the current CFG Node (see Get_Vertex_By _Index)
      ID_index : Natural;
      Proc_size : Integer;
   begin
      --  Put_Line("Analysing loops and procedure calls");
      --  Construct DOM tree form CFG_Graph
      --  A list of dom trees for each CFG?
      --  Put_Line ("CFGs :::::: " & Integer'Image(cfgNum));
      --  Initialize the list of dominator trees



      DomTrees := new Dom_Tree_Array (1..cfgNum);
      Loop_Tree.Init_trees (cfgNum);

      if (Global.E_M in VERBOSE..MINIMAL) then
         Put ("Constructing Loop-Call Trees...........");
      end if;

      --  Construct the dominator tree for each CFG
      for CFG_Index in 1..cfgNum loop
         curCFG_ptr := Get_CFG (CFG_Index);
         curCFG_Num := Get_Vertex_Num(curCFG_ptr);
         if Global.tbshootLCT then
            Put_Line ("CFG" & Integer'Image(CFG_Index) & "has " & Integer'Image(curCFG_Num) &
                      " nodes.");
         end if;
         declare
            --subtype D_list is Dominator_List (1..curCFG_Num);
            type D_M is array (1..curCFG_Num) of Dominator_List (1..curCFG_Num);--D_List;
            D_Matrix : D_M;
            Imm_D_Matrix : D_M;
            IDom : Imm_Dominators (1..curCFG_Num);
            Domtors, predDom : Dominator_List (1..curCFG_Num);-- D_List;
            Changed : Boolean := True;
            Loc_changed : Boolean := False;
            incremLabel : Positive := 1;
         begin
            -- Dom_Matrix (1) := (True, others => False);
            -- Dom_Matrix (2..curCFG_Num) := (others => (others=>True));

            --  Initialize dominators
            for Index in 1..curCFG_Num loop
               --  Root node is dominated by itself
               if (Index = 1) then
                  Domtors := (True, others=>False);
                  D_Matrix(index) := Domtors;
               else
                  Domtors := (others=>True);
                  D_Matrix(index) := Domtors;
               end if;
            end loop;
            -- Put_Line ("DomList per nodo 1: ");
            -- Print_Dom_List(D_Matrix(1), curCFG_Num);

            --  Dominator algorithms
            while Changed loop

               Changed := False;

               for NodeIndex in 2..curCFG_Num loop
                  Loc_changed := False;
                  --New_Line;
                  --Put ("node index: " & integer'Image(NodeIndex));
                  CFG_Node := Get_Vertex_By_Index(curCFG_Ptr, NodeIndex,
                                                  curCFG_Ptr.Number);--CFG_Graph.Connectivity);
                  Domtors := D_Matrix (NodeIndex);
                  --Print_Vertex (CFG_Node);
                  declare
                     predNum : Natural := CFG.Predecessors_Num(curCFG_ptr.all,
                                                             CFG_Node);
                     P_Node : CFG_Vertex;
                  begin

                     --Put_Line (" (Predec: " & Integer'Image(predNum) & ")");
                     if (predNum > 0) then
                        declare
                           predList : CFG.Vertex_List (1..predNum) :=
                             CFG.Predecessors(curCFG_ptr.all,CFG_Node);
                           predIndex : Positive;
                        begin
                           for predec in 1..predNum loop
                              P_node := predList(predec);
                              predIndex := CFG.Index_Of(curCFG_ptr.all, P_node);
                              predDom := D_Matrix (predIndex);
                              if (Global.E_M = VERBOSE) then
                                 Put("Comparing: " & Integer'Image(NodeIndex)
                                     & " : " & Integer'Image(predIndex));
                                 Print_Vertex (P_node);
                              end if;

                              Dominator_And (Domtors, predDom, curCFG_Num, NodeIndex, Loc_changed);
                              if(Loc_changed) then
                                 --Put_line("+++");
                                 --Put_Line("node : " & Integer'Image(NodeIndex) &
                                 --         " changed: " & Boolean'Image(Loc_changed));
                                 --Print_Dom_List(D_Matrix (NodeIndex), curCFG_Num);
                                 --Put_line("---");
                                 --Print_Dom_List(Domtors, curCFG_Num);
                                 --Put_line("+++");
                                 D_Matrix (NodeIndex) := Domtors;
                                 Changed := True;
                                 loc_changed := false;
                              end if;

                           end loop;
                        end;
                     else
                        Put(" no predecessors");
                     end if;
                  end;
                  Loc_changed:= False;
               end loop;
            end loop;
            if (Global.E_M = VERBOSE) then
               Put_line ("End dominator analysis");
            end if;

            --Put_line ("End dominator analysis");
            --  Compute immediate dominators
            --  Imm_D_Matrix
            --Imm_D_Matrix := D_Matrix;

            for dIndex in 1..curCFG_Num loop
               Imm_D_Matrix (dIndex) := (others=>False);
            end loop;


            if (Global.E_M = VERBOSE) then
               Put_Line("Computing immediate dominators");
            end if;

            --  Root node has no immediate dominators
            for Index in 2..curCFG_Num loop

               CFG_Node := Get_Vertex_By_Index(curCFG_ptr, Index, curCFG_Ptr.Number);--Connectivity);
               if (Global.E_M = VERBOSE) then
                  Put ("Index: " & Integer'Image(Index));
                  Put (" NodeInfo:" & CFG_Node.BB);
                  Put_Line("[" & Integer'Image(curCFG_Num) & "]");
               end if;


               ID_index := Find_Immediate_Dominator (curCFG_Ptr,
                                                     D_Matrix (Index),
                                                     curCFG_Num,
                                                     Index);
               --Put_Line("ID Index: " & Integer'Image(ID_Index));
               Imm_D_Matrix (Index)(ID_Index) := True;

--                 Debug_Dominators(index => Index,
--                                  node  => CFG_Node,
--                                  list  => Imm_D_Matrix(Index),
--                                  size  => curCFG_Num);
            end loop;

            if (Global.E_M in VERBOSE..DEBUG) then
               Put_Line ("Immmediate dominators computed");
               if (Global.E_M = DEBUG) then
                  for dList in 1..curCFG_Num loop
                     CFG_Node := Get_Vertex_By_Index(curCFG_ptr, dList, curCFG_Ptr.Number);--Connectivity);
                     Print_Vertex (CFG_Node);
                     Print_Dom_List(Imm_D_Matrix(dList), curCFG_Num);
                  end loop;
               end if;
               New_Line;
               Put_Line ("Constructing Dominator tree");
            end if;

            if (Global.E_M = VERBOSE) then
               Put_Line ("Constructin Dominator tree");
            end if;

            --  Trasform the immediate dominator info into a visible type
            --  (i.e., Imm_Dominators is array (Positive range <>) of Natural;
            IDom(1) := 0;
            if(curCFG_Num>1) then
               for listIndex in 2..curCFG_Num loop
                  Domtors := Imm_D_Matrix(listIndex);
                  for elemIndex in 1..curCFG_Num loop
                     if (Domtors(elemIndex) = True) then
                        IDom(listIndex):=elemIndex;
                        --exit;
                     end if;
                  end loop;
               end loop;
            end if;


            --  Construct a dominator tree
            --  Get the CFG root node
            CFG_Node := CFG.Head(curCFG_ptr.all);
            --Print_Succ (CFG_Node, curCFG_Ptr.all);
            --  Construct the respective Dom-Tree root node
            curNodeInfo := (Id => To_Unbounded_String(CFG_Node.BB),
                            CfgId => CFG.Get_Id(curCFG_ptr.all),
                            ProcName => To_Unbounded_String(
                              CFG.Get_Name(curCFG_ptr.all)));
            curNode := Create_Node(curNodeInfo, RootNode, None);
            --  Initialize a DOM tree with curNode as ROOT
            --  The DomTrees dimension has been already initialized

            currDomTree := Create (curNode, CFG.Get_Name(curCFG_Ptr.all));
            --DomTrees (CFG_Index) := currDomTree;
            --  Add other nodes to the dom tree
            --  Add_Succ (Parent, Child : in out Tree_Node_Ptr);
            --  Recursive procedure
            Populate_Dom_Tree(currDomTree, curNode, 1, curCFG_ptr, curCFG_Num, IDom);
            DomTrees (CFG_Index) := currDomTree;
            --Put_Line("--ok---");

            if (Global.E_M = VERBOSE) then
               Put_Line ("Dominator has" &
                    Integer'Image(Get_Succ_Number(curNode)) & " root children.");
            end if;
            domTreeNum := domTreeNum +1;


            --  Loop analysis here
            --  Because DOMINANCE INFO IS LOCAL
            --  Due to type constraints

            -- Define a new loop tree
            -- Starting from a root Node

            Put_line("Performing loop analysis");
            --  Get the CFG root node
            CFG_Node := CFG.Head(curCFG_ptr.all);

            --  Compute the size of the root procedure
            Proc_size := Get_Procedure_Size(Utilities.UTrim(Global.Program),--Global.File_Name.all,
                                            Utilities.Trim(CFG.Get_Name(curCFG_ptr.all)));
                                            --"_ada_main");
            Put_Line("Procedure '"& CFG.Get_Name(curCFG_ptr.all) &"'  size: " & integer'Image(Proc_Size));
            --  Set Loop node info
            rootLoopInfo := (Id => To_Unbounded_String(CFG_Node.BB),
                             Label => To_Unbounded_String("P"),
--                                 To_Unbounded_String("L" &
--                                 Integer'Image(incremLabel));
                             Index => 1,
                             Size => Proc_Size,
                            CfgId => CFG.Get_Id(curCFG_ptr.all),
                            ProcName => To_Unbounded_String(
                              CFG.Get_Name(curCFG_ptr.all)),
                            Bound => 1,
                            BBs => null,
                            BBs_num => 0
                            );
            --  Create a loop node
            rootLoopNode := Loops.Create_Node(nInfo => rootLoopInfo,
                                             nType => Loop_Tree.RootNode,
                                             lType => Loop_Tree.Call);
            --  Create a Loop tree with such node as root
            Loop_Tree.Add_Tree(
                               Loops.Create(rootLoopNode, CFG.Get_Name(curCFG_Ptr.all)),
                               CFG_Index);
            curLoopTree := Loop_Tree.Get_Tree(CFG_Index);

            if (global.E_M = DEBUG) then
               Put_line("Check root: ");
               Loops.Print_Node(Loops.Get_Root(curLoopTree.all));
               Loops.Print_Node(rootLoopNode);
            end if;


            --  Unmark the CFG nodes
            CFG.Clear_All_Marks(curCFG_ptr.all);

            --  Iterates over all CFG nodes
            for NodeIndex in 2..curCFG_Num loop
               --Put_Line ("NodeIndex: " & Integer'Image(NodeIndex));
               --  The current node
               CFG_Node := Get_Vertex_By_Index(curCFG_ptr, NodeIndex,
                                               curCFG_Ptr.Number);--CFG_Graph.Connectivity);
               --Print_Vertex(CFG_Node);
               --  Dominators of the current node
               Domtors := D_Matrix (nodeIndex);
               --  For each node N (except for STUB nodes)
              -- declare
                  --  Number of successors of N
               succNum := CFG.Successors_Num(curCFG_ptr.all,
                                                          CFG_Node);
                  --  Successor node in succ(N)
                  --S_Node : CFG_Vertex;
              -- begin
                  --  If N has succassors
               if (succNum > 0) then
                  declare
                     --  Successor node in succ(N)
                     S_Node : CFG_Vertex;
                     --  List of successors of N i.e., succ(N)
                     succList : CFG.Vertex_List (1..succNum) :=
                       CFG.Successors(curCFG_ptr.all,CFG_Node);
                     --  Index of S the successor node (to compute dominators)
                     succIndex : Positive;
                     curLoopBB : Loop_Tree.Loop_BBs_Ptr;
                     count : Positive := 1;

                  begin
                     --  Iterates over all nodes in succ(N)
                     for succ in 1..succNum loop
                        --  Get S the next successor
                        S_node := succList(succ);
                        --  and computes its index
                        succIndex := CFG.Index_Of(curCFG_ptr.all, S_node);
                        --  If S dominates N then N->S is a back edge
                        if (Is_Back_Edge(Domtors, succIndex)) then
                           if (Global.E_M = VERBOSE) then
                              Put("Node #" & Integer'Image(succIndex)
                                  & " is a dominator of node # " & Integer'Image(nodeIndex));
                              Print_Vertex (S_node);
                           end if;
                           --  succ(N) is a loop header
                           --  Create a Loop node in the Loop Tree
                           --  where Loop BB are those paths from S to N
                           Compute_BBs (curLoopBB, curCFG_ptr, count,
                                           S_Node, CFG_Node);
                              --  Construct loop node info
                              --  The loop header is the dominator node
                           childLoopInfo := (Id => To_Unbounded_String(S_Node.BB),
                                             Label => To_Unbounded_String("L" &
                                               Integer'Image(incremLabel)),
                                             Index => succIndex,
                                             Size => 0,
                                             CfgId => CFG.Get_Id(curCFG_ptr.all),
                                             ProcName => To_Unbounded_String(
                                               CFG.Get_Name(curCFG_ptr.all)),
                                             Bound => 9,
                                             BBs => curLoopBB,
                                             BBs_num => count
                                            );
                           if (Global.E_M = DEBUG) then
                              Put_line("L"& Integer'Image(incremLabel));
                           end if;

                           incremLabel := incremLabel +1;
                           --  Create a loop node
                           childLoopNode := Loops.Create_Node(
                                                              nInfo => childLoopInfo,
                                                              nType => Loop_Tree.LoopNode,
                                                              lType => Loop_Tree.Call);
                           --Put("BBs of the new loop node: " &
                           --   S_Node.BB);Loop_tree.Print_BBs(childLoopNode);
                           --Put_Line ("++++++");
                           --  add it to the loop tree as a successor of the ROOT node
                           Loops.Add_Succ(Parent => rootLoopNode,
                                          Child  => childLoopNode);
                           --Put_Line("NODE ADDED : " & Utilities.UTrim(childLoopInfo.Id));
                           --Loops.Print_Node(childLoopNode);
                           --Put_Line(Integer'Image(childLoopInfo.Bound));
                        end if;
                     end loop;
                  end;
               else
                  --  The current node is an exit or call node
                  --  Nothing to be done in this case
                  --Put_Line("No successors for this node");
                  null;
               end if;
                  --		N->succ(N) is a back edge
                  --		and succ(N) is the loop header
                  --		the loop BB are included in the paths from N to succ(N)
                  --	DO: Add a LOOP node to the CLS graph
                  --		if such path include a STUB node
                  -- 	DO: Add an outgoing PROC node to the CLS graph
                  --	else find recursively
               --end;
            end loop;

            --Loop_Tree.Export_To_Dot_Single (Loop_Tree.Trees, cfgNum, cfgNum, 1);
            --Loop_tree.Export_To_Dot (Loop_Tree.Trees, cfgNum, cfgNum);
            --Put_line("Exported to dot");


            --  Flat LOOP tree constructed

            if Global.tbshootLCT then
               Put_line("Flat loop tree constructed");
            end if;

            --+Export_To_Dot_Part(DomTrees, cfgNum, cfgNum, CFG_Index);

            --+LT.Print_Node(rootLoopNode);
            --  Check dominance between loops
            --  Retrieve the current loop tree
            curLoopTree := Loop_Tree.Get_Tree (CFG_Index);
            succListSize := Loops.Get_Succ_Size(rootLoopNode);
            succNum := Loops.Get_Succ_Number(rootLoopNode);
            --+Put_Line("Succs: " & Integer'IMage(succNum));
            --+New_Line;

            --  The root node
            if (succNum > 1) then
               declare
                  succList : Loops.Tree_Node_List (1..succListSize);
                  -- := Loops.Get_Succ(rootLoopNode);

                  childIndex : Positive;
                  tempIndex : Positive;
--                    function IsDominated(list : Dominator_List;
--                                         --size : Positive;
--                                         index : Positive)
--                                         return Boolean is
--                    begin
--                       return list(index);
--                    end IsDominated;

               begin
                  --Loops.Print_Node(rootLoopNode);
                  --Put_Line("Before: " & Integer'Image(succNum));
                  succList := Loops.Get_Succ(rootLoopNode);
                  --Put_Line("Before: " & Integer'Image(succNum));
                  for lNode in 1..succNum loop
                     childLoopNode := succList(lNode);
                     childLoopInfo := Loops.Get_info(childLoopNode);
                     childIndex := childLoopInfo.Index;
                     --Put ("Child" & Integer'Image(childIndex));
                     Domtors := D_Matrix (childIndex);
                     --Put ("SUccessors: ");Loops.Print_Node(childLoopNode);
                     -- If the parent node is the root node
                     -- then the root node is its dominator
--                       if( Loops.Parent(childLoopNode)=rootLoopNode)then
--                          Loops.Add_Succ(rootLoopNode,childLoopNode);
--                       else

                     for temp in 1..succNum loop
                        if (temp /= lNode) then
                           tempLoopNode := succList(temp);
                           --Put(Integer'Image(temp) & "th successor: " );
                           --Loops.Print_Node(tempLoopNode);
                           tempLoopNodeInfo := Loops.Get_Info(tempLoopNode);
                           tempIndex := tempLoopNodeInfo.Index;
                           if (IsDominated(Domtors,tempIndex)) then
                              if (Global.tbshootLCT) then
                                 Put_Line ("Is Dominated! by" & Integer'Image(tempIndex));
                              end if;
                              --  Need to update BBs of the new parent node
                              --  (tempLoopNode) so that it does not
                              --  includes BB belonging to the child node
                              if (not Loop_Tree.In_BBs(tempLoopNodeInfo.BBs, tempLoopNodeInfo.BBs_num, childIndex)) then
                                 Put_Line ("..but not in BBs");
                                 --Loops.Add_Succ(tempLoopNode,childLoopNode);
                              else

                              Update_BBs(tempLoopNode,childLoopNode);
                              Loops.Add_Succ(tempLoopNode,childLoopNode);
                              --+New_Line;
                              --+Put_Line("Now the root:");
                              Loops.Print_Node(rootLoopNode);
                              --+New_Line;
                              Loops.Remove_From_Succ(childLoopNode, rootLoopNode);
                              --Put_Line("The cjilfNode:");
                              --Loops.Print_Node(childLoopNode);
                              --Loop_Tree.Print_BBs(childLoopNode);
                              --New_Line;
                              succNum := succNum -1;
                              end if;
                           end if;
                        end if;
                        if (Global.E_M = VERBOSE) then
                           Put_line("-------------Nodes-----" & Integer'Image(temp)
                                    &  "_" & Integer'Image(succNum));
                           Loops.Print_Node(childLoopNode);
                           Loop_Tree.Print_BBs(childLoopNode);
                           Put_line("--------------------");
                           New_Line;
                        end if;
                     end loop;
                     --                          end if;
                  end loop;
--                    Put_Line("After: " & Integer'Image(
--                    Loops.Get_Succ_Size(rootLoopNode)));
--                    Loops.Print_Node(tempLoopNode);
--                    Loop_Tree.Print_BBs(tempLoopNode);
               end;
            end if;

            --+Export_To_Dot_Part(DomTrees, cfgNum, cfgNum, CFG_Index);
            --+Loop_Tree.Export_To_Dot_Single (Loop_Tree.Trees, cfgNum, cfgNum, 1);

            if (Global.drawGraphs or Global.tbshootLCT) then
               Loop_Tree.Export_To_Dot_Single (Loop_Tree.Trees, cfgNum, cfgNum, 1);
            end if;

            Put_line ("Setting procedure nodes");

            --  Setup procedure nodes
            succListSize := Loops.Get_Succ_Size(rootLoopNode);
            succNum := Loops.Get_Succ_Number(rootLoopNode);

            declare  --  Setup procedure nodes
               use Loop_Tree;
               function Get_Parent_Node (root : Loops.Tree_Node_Ptr;
                                         dom: Positive) return Loops.Tree_Node_Ptr is
                  use Loops;
                  succList : Loops.Tree_Node_List_ptr := Loops.Get_Succ(root);
                  node_ptr     : Loops.Tree_Node_Ptr;
                  node_info    : Loop_Tree.Loop_Node_Info;
                  root_info    : Loop_Tree.Loop_Node_Info := Loops.Get_Info(root);
                  res : Loops.Tree_Node_Ptr;
               begin
                  succListSize := Loops.Get_Succ_Size(root);
                  succNum := Loops.Get_Succ_Number(root);

                  if (Global.E_M = DEBUG) then
                     Put_line("Looking children of: " & Integer'Image(root_info.Index));
                     Put_Line("succNum: " & Integer'Image(succNum));
                  end if;

                  if (succNum >0) then
                     for lN in 1..succNum loop
                        node_ptr := succList(lN);
                        node_info := Loops.Get_Info(node_ptr);
                        --Put_line("$: " & Utilities.UTrim(node_info.Id));
--                           & " " & Loop_tree.Print_Nt (Get_NType(node_ptr)));
                        --  The loop-node is the immediate dominator
                        --  or includes the dominator in its BB
--  node_info.CfgId
-- (Get_NType(node_ptr) /= Loop_tree.ProcNode) and
                        if (( node_info.Index = dom)
                        or ( Get_NType(node_ptr) /= Loop_tree.ProcNode and then Loop_Tree.In_BBs(node_info.BBs,
                        node_info.BBs_num, dom)))
                        then
                           Print_BBs(node_ptr);
                           if (Global.E_M = DEBUG) then
                              Put("Found! -> " &Integer'Image(dom));
                              --Loops.Print_Node(node_ptr);
                           end if;
                           return node_ptr;
                        end if;
                     end loop;
                     if (Global.E_M = DEBUG) then
                        Put_Line("second part");
                     end if;

                     for lN in 1..succNum loop
                        node_ptr := succList(lN);
                        --Loop_tree.Print_BBs(node_ptr);
                        res := Get_Parent_Node(node_ptr, dom);
                     end loop;
                  else
                     null;
                     Put_Line("No succ-->returns null");
                  end if;
                  return res;
               end Get_Parent_Node;

            begin
               --if (succNum > 0) then
		Put_Line("stubs");
               --  Exclude Root node
               for Index in 2..curCFG_Num loop
                  CFG_Node := Get_Vertex_By_Index(curCFG_ptr, Index, curCFG_Ptr.Number);--Connectivity);
                  --Put("Here:");Print_Vertex(CFG_Node);

                  --  If the current node is a stub (calls a procedure)
                  if (CFG_Node.typ = Stub ) then
                     --Put_line ("Intercettato Stub");
                     --Put_Line("Is STUB");Put_Line("n: " & Integer'Image(curCFG_Num));
                     Print_Vertex_Info(CFG_Node);
                     --Put_Line("CFG: " & CFG_node.cfgId);
                     --  Find its predecessor (has just 1 by definition)
                     CFG_pred := CFG.First_Predecessor(curCFG_ptr.all, CFG_Node);
                     --P_node := vList_ptr(1);
                     --  Find its immediate dominator
                     --Put_line ("Pred:"); CFG.Print_Vertex_Info(CFG_pred);
                     --if(CFG.Head(Graph => curCFG_ptr.all) = CFG_pred) then
                     --   Put_Line("WELL");
                     --end if;
                     curIDom := IDom(CFG.Index_Of(curCFG_ptr.all,CFG_pred));
                     --Put_Line("DOm:" & Integer'Image(curIDom));
                     -- If no immediate dominators have been found then
                     --  the node is simply dominated by the root
                     if (curIDom = 0) then
                        --Put_Line("dom by root");
                        curIDom := 1;
                        tempLoopNode := rootLoopNode;
                     else

                     --  Is there a node in the Loop Tree with
                     --  ID = curIDom or includes IDom in its BBs?
                     --  tempLoopNode := such node

                     	--Put_line ("Intercetta8b - curIDOM:" & Integer'IMage(curIdom));
                        tempLoopNode := Get_Parent_Node(rootLoopNode,curIDom);
                        if(LT.IsNull(tempLoopNode)) then
                           tempLoopNode := rootLoopNode;
                           --Put_Line("dom by root");
                        end if;


                     end if;

                     --  Compute the size of the referenced procedure
                     --Proc_size := Get_Procedure_Size(Global.File_Name.all,
                                            --CFG.Get_Name(curCFG_ptr.all);
                                            --"_ada_main");
                                            --Loops.Print_node(tempLoopNode);

                     --Put_Line("  **  " & Utilities.UTrim(tempLoopNodeInfo.Id));

                     -- then add a ProcNode to such node
                     childLoopInfo := (Id => To_Unbounded_String(CFG_Node.BB),
                                       Label => To_Unbounded_String("P"),
                                       Index => Index,
                                       Size => 0,
                                       --  Get its CFG!
                                       CfgId => CFG_Node.cfgId,
                                       ProcName => To_Unbounded_String(
                                         CFG.Get_Name(curCFG_ptr.all)),
                                       Bound => 1,
                                       BBs => null,
                                       BBs_num => 0);


                     childLoopNode :=
                       Loops.Create_Node(nInfo => childLoopInfo,
                                         nType => Loop_Tree.ProcNode,
                                         lType => Loop_Tree.Call);

                     Put_line("AHEM");
                     Loops.Add_Succ(Parent => tempLoopNode,
                                    Child  => childLoopNode);
                     Put_line("Added");

                  end if;
                  end loop;
               --end if;
            end; --------------------  End Setup procedure nodes
         end;

         Put_Line("OK CFG " & Integer'Image(CFG_Index));
         --New_Line; New_Line;
         -- Clear the current CFG from memory
         --Unset(curCFG_ptr);

      end loop; -------------------  End iteration throught CFGs (procedures)

      --Print(Get_Info(Get_root(Domtrees(1).all)));

      if (Global.E_M in VERBOSE..MINIMAL) then
         Put_Line (" OK");
      end if;

      if (Global.drawGraphs) then
         --Export_To_Dot_Single (DomTrees, cfgNum, cfgNum, 1);
         --Export_To_Dot (DomTrees, cfgNum, cfgNum);
         Loop_tree.Export_To_Dot_Single (Loop_Tree.Trees, cfgNum, cfgNum, 1);
      end if;


   exception
      when Error : Storage_Error =>
         Put_Line(" [3] - STORAGE ERROR!!!!!!!!!!");
         Put_Line(CFG.Get_Graph_Info(curCFG_Ptr.all));
      when Error: END_ERROR =>
         Put_Line ("Was not able to find the program executable");
         Put_Line ("(unable to compute the procedure size)");
         --Abort_Program;
         raise;
      when Error : others => Put_Line("Unknown_Loop_Analysis:" & Exception_Name(Error) &
                                      Exception_Message(Error));
         raise;
   end Loop_Analysis;

   procedure Init (num : Positive) is
   begin
      DomTrees := new Dom_Tree_Array (1..num);
   end Init;



      --  Performs dominator analysis to detect loops
   procedure Single_Loop_Analysis  is
      use Loops;
      --  Number of CFGs in memory
      cfgNum : Natural := Get_CFG_Num;
      --  Pointer to the current CFG
      curCFG_Ptr : CFG_ptr;-- := cfg;
      --  Number of VERTICES in the current CFG
      curCFG_Num : Natural;
      --  INFO of the current Tree Node
      curNodeInfo  : Dom_Node_Info;
      --  INFO of the child Tree Node
      childNodeInfo  : Dom_Node_Info;
      --  INFO of the current LoopTree node
      rootLoopInfo : Loop_Tree.Loop_Node_Info;
      --  INFO of the current LoopTree node
      childLoopInfo : Loop_Tree.Loop_Node_Info;
      --  Pointer to the current LoopTree Node
      rootLoopNode : Loops.Tree_Node_Ptr;

      tempLoopNode : Loops.Tree_Node_Ptr;
      tempLoopNodeInfo : Loop_Tree.Loop_Node_Info;
      --  Pointer to the current Loop Tree
      curLoopTree : Loops.Tree_Type_Ptr;
      --  Numebr of successors
      succNum : Natural;
      --  Dimension of the list of successors
      succlistSize : Natural;
      --  Index (in CFG) of the Immediate dominator of the current node
      curIDom : Natural;
      -- Pointer to a Vertex List
      --vList_ptr : CFG.Vertex_List_Ptr;
      --  Pointer to the current LoopTree Node
      childLoopNode : Loops.Tree_Node_Ptr;
      --  Pointer to the current Tree Node
      --+curNode : DT.Tree_Node_ptr;
      --  Pointer to the current Dominator tree
      --+currDomTree : DT.Tree_Type_Ptr;
      --  Current node of the CFG
      CFG_Node : CFG_Vertex;
      --  Predecessor node of the current node of the CFG
      CFG_Pred : CFG_Vertex;

      --  Index of the current CFG Node (see Get_Vertex_By _Index)
      ID_index : Natural;
      Proc_size : Integer;
      isSpecial_Id : Integer := 0;
      specialStr : String (1..8);
   begin
      --  Put_Line("Analysing loops and procedure calls");
      --  Construct DOM tree form CFG_Graph
      --  A list of dom trees for each CFG?
      --  Put_Line ("CFGs :::::: " & Integer'Image(cfgNum));
      --  Initialize the list of dominator trees



      -- INITIALIZATION HANDLED IN XML_PARSER
      --DomTrees := new Dom_Tree_Array (1..cfgNum);
      --Loop_Tree.Init_trees (cfgNum);

      if (Global.E_M in VERBOSE..MINIMAL) then
         Put ("   Constructing a LCT...........");
      end if;

      -- Construct the dominator tree for **ONE** CFG (in input)
      -- That is the CFG identified by CFG_Index
      --+for CFG_Index in 1..cfgNum loop
      curCFG_ptr := Get_CFG (CFG_Index);
      curCFG_Num := Get_Vertex_Num(curCFG_ptr);
      --if (Global.E_M = VERBOSE) then
      --New_line;
      if Global.tbshootLCT then
         Put_Line ("CFG #" & Integer'Image(CFG_Index) & " has "
                   & Integer'Image(curCFG_Num)
                   & " nodes.");
      end if;

      declare
         --+subtype D_list is Dominator_List (1..curCFG_Num);
         type D_M is array (1..curCFG_Num) of Dominator_List (1..curCFG_Num);--+D_List;
         D_Matrix : D_M;
         Imm_D_Matrix : D_M;
         IDom : Imm_Dominators (1..curCFG_Num);
         Domtors, predDom : Dominator_List (1..curCFG_Num);-- D_List;
         Changed : Boolean := True;
         Loc_changed : Boolean := False;
         -- Used for loop node labeling
         incremLabel : Positive := 1;
      begin
         --+ Dom_Matrix (1) := (True, others => False);
         --+ Dom_Matrix (2..curCFG_Num) := (others => (others=>True));
         --+Put_Line("--°°°");

         --  Initialize dominators
         for Index in 1..curCFG_Num loop
            -- Root node is dominated by itself
            if (Index = 1) then
               Domtors := (True, others=>False);
               D_Matrix(index) := Domtors;
            else
               Domtors := (others=>True);
               D_Matrix(index) := Domtors;
            end if;
         end loop;
         --+ Put_Line ("DomList per nodo 1: ");
         --+ Print_Dom_List(D_Matrix(1), curCFG_Num);

         --  Dominator algorithms
         while Changed loop
            Changed := False;

            for NodeIndex in 2..curCFG_Num loop
               Loc_changed := False;
               --+New_Line;
               --+Put ("node index: " & integer'Image(NodeIndex));
               --+Put_line("Size of curCFG_Ptr.Vertices-> " & Integer'Image(curCFG_Ptr.Vertices'Length));
               CFG_Node := Get_Vertex_By_Index(curCFG_Ptr, NodeIndex,
                                               curCFG_Ptr.Number); --+CFG_Graph.Connectivity);
               --+Put_Line("@");
               Domtors := D_Matrix (NodeIndex);
               --+Print_Vertex (CFG_Node);
               declare

                  predNum : Natural := CFG.Predecessors_Num(curCFG_ptr.all,
                                                             CFG_Node);
                  P_Node : CFG_Vertex;
               begin

                  --+Put_Line (" (Predec: " & Integer'Image(predNum) & ")");
                  if (predNum > 0) then
                     declare
                        predList : CFG.Vertex_List (1..predNum) :=
                          CFG.Predecessors(curCFG_ptr.all,CFG_Node);
                        predIndex : Positive;
                     begin
                        for predec in 1..predNum loop
                           P_node := predList(predec);
                           predIndex := CFG.Index_Of(curCFG_ptr.all, P_node);
                           predDom := D_Matrix (predIndex);
                           if (Global.E_M = VERBOSE) then
                              Put("Comparing: " & Integer'Image(NodeIndex)
                                  & " : " & Integer'Image(predIndex));
                              Print_Vertex (P_node);
                           end if;

                           Dominator_And (Domtors, predDom, curCFG_Num, NodeIndex, Loc_changed);
                           if(Loc_changed) then
                              --+Put_line("+++");
                              --+Put_Line("node : " & Integer'Image(NodeIndex) &
                              --+        " changed: " & Boolean'Image(Loc_changed));
                              --+Print_Dom_List(D_Matrix (NodeIndex), curCFG_Num);
                              --+Put_line("---");
                              --+Print_Dom_List(Domtors, curCFG_Num);
                              --+Put_line("+++");
                              D_Matrix (NodeIndex) := Domtors;
                              Changed := True;
                              loc_changed := false;
                           end if;
                        end loop;
                     end;
                  else
                     Put(" no predecessors");
                  end if;
               end;
               Loc_changed:= False;
            end loop;
         end loop;
         if (Global.E_M = VERBOSE or Global.tbshootLCT) then
            Put_line ("End dominator analysis");
         end if;

         --Put_line ("End dominator analysis");
         --  Compute immediate dominators
         --  Imm_D_Matrix
         --Imm_D_Matrix := D_Matrix;

         for dIndex in 1..curCFG_Num loop
            Imm_D_Matrix (dIndex) := (others=>False);
         end loop;


         if (Global.E_M = VERBOSE or Global.tbshootLCT) then
             Put_Line("Computing immediate dominators");
         end if;

         --  Root node has no immediate dominators
         for Index in 2..curCFG_Num loop
            CFG_Node := Get_Vertex_By_Index(curCFG_ptr, Index, curCFG_Ptr.Number);--Connectivity);
            if (Global.E_M = VERBOSE) then
               Put ("Index: " & Integer'Image(Index));
               Put (" NodeInfo:" & CFG_Node.BB);
               Put_Line("[" & Integer'Image(curCFG_Num) & "]");
            end if;


            ID_index := Find_Immediate_Dominator (curCFG_Ptr,
                                                  D_Matrix (Index),
                                                  curCFG_Num,
                                                  Index);
            --+Put_Line("ID Index: " & Integer'Image(ID_Index));
            Imm_D_Matrix (Index)(ID_Index) := True;

            --+                 Debug_Dominators(index => Index,
            --+                                  node  => CFG_Node,
            --+                                  list  => Imm_D_Matrix(Index),
            --+                                  size  => curCFG_Num);
         end loop;

         if (Global.E_M in VERBOSE..DEBUG) then
            Put_Line ("Immmediate dominators computed");
            if (Global.E_M = DEBUG) then
               for dList in 1..curCFG_Num loop
                  CFG_Node := Get_Vertex_By_Index(curCFG_ptr, dList, curCFG_Ptr.Number);
                     Print_Vertex (CFG_Node);
                     Print_Dom_List(Imm_D_Matrix(dList), curCFG_Num);
               end loop;
            end if;
            --+raise Edge_Error;
            New_Line;
            Put_Line ("Constructing Dominator tree");
         end if;

         if (Global.E_M = VERBOSE) then
            Put_Line ("Constructin Dominator tree");
         end if;

         -- Trasform the immediate dominator info into a visible type
         -- (i.e., Imm_Dominators is array (Positive range <>) of Natural;
         IDom(1) := 0;
         if(curCFG_Num>1) then
            for listIndex in 2..curCFG_Num loop
               Domtors := Imm_D_Matrix(listIndex);
               for elemIndex in 1..curCFG_Num loop
                  if (Domtors(elemIndex) = True) then
                     IDom(listIndex):=elemIndex;
                     --+exit;
                  end if;
               end loop;
            end loop;
         end if;
--
--
--           -- Construct a dominator tree
--           -- Get the CFG root node
--           CFG_Node := CFG.Head(curCFG_ptr.all);
--           --+Print_Succ (CFG_Node, curCFG_Ptr.all);
--           --  Construct the respective Dom-Tree root node
--           curNodeInfo := (Id => To_Unbounded_String(CFG_Node.BB),
--                           CfgId => CFG.Get_Id(curCFG_ptr.all),
--                           ProcName => To_Unbounded_String(
--                             CFG.Get_Name(curCFG_ptr.all)));
--           curNode := Create_Node(curNodeInfo, RootNode, None);
--
--           -- Initialize a DOM tree with curNode as ROOT
--           -- The DomTrees dimension has been already initialized
--           currDomTree := Create (curNode, CFG.Get_Name(curCFG_Ptr.all));
--           --+DomTrees (CFG_Index) := currDomTree;
--
--           -- Add other nodes to the dom tree
--           --+ Add_Succ (Parent, Child : in out Tree_Node_Ptr);
--           -- Recursive procedure
--           Populate_Dom_Tree(currDomTree, curNode, 1, curCFG_ptr, curCFG_Num, IDom);
--
--           -- Add the DomTree to the list f DOMINATOR trees
--           DomTrees (CFG_Index) := currDomTree;
--           --+Put_Line("--ok---");
--
--           if (Global.E_M = VERBOSE) then
--              Put_Line ("Dominator has" &
--                        Integer'Image(Get_Succ_Number(curNode)) & " root children.");
--           end if;
--           domTreeNum := domTreeNum +1;

         --  Loop analysis is performed here
         --  Because DOMINANCE INFO IS LOCAL
         --  Due to type constraints

         -- Define a new loop tree
         -- Starting from a root Node
         if (Global.E_M = VERBOSE or Global.tbshootLCT) then
            Put_line("Performing loop analysis");
         end if;


         -- Loop Tree are constructed mergin information from CFG and IDom tree
         -- Get the CFG root node
         CFG_Node := CFG.Head(curCFG_ptr.all);


         -- Compute the size of the root procedure (for later positioning)
         --+Put_Line("Procedure '"& CFG.Get_Name(curCFG_ptr.all) &"'  program: " & Utilities.UTrim(Global.Program));
         -- **TODO** This is just a workaround for procedure whise size cannot
         -- **TODO** be retrieved by "nm"
         isSpecial_Id := isSpecial(To_Unbounded_String(CFG.Get_Name(curCFG_ptr.all)));
         if isSpecial_Id = 0 then
            if (Global.E_M = VERBOSE or Global.tbshootLCT) then
               Put_Line("Trying to retrieve size of " & Utilities.Trim(CFG.Get_Name(curCFG_ptr.all)));
            end if;
            Proc_size := Get_Procedure_Size(Utilities.UTrim(Global.Program),--+Global.File_Name.all,
                                            Utilities.Trim(CFG.Get_Name(curCFG_ptr.all)));
            --+Put_Line("Problem with Size");
         else
            specialStr := Special_Size (isSpecial_Id);
            Proc_Size := Integer'Value("16#" & specialStr &"#");
            --+Put_Line("Problem with Size");
         end if;

         --+Put_Line("Procedure '"& CFG.Get_Name(curCFG_ptr.all) &"'  size: " & integer'Image(Proc_Size));

         --  Set Loop node info
         --+Put_line("Tag Set loop node info");
         rootLoopInfo := (Id => To_Unbounded_String(CFG_Node.BB),
                          Label => To_Unbounded_String("P"),
                          --+  To_Unbounded_String("L" &
                          --+  Integer'Image(incremLabel));
                          Index => 1,
                          Size => Proc_Size,
                          CfgId => CFG.Get_Id(curCFG_ptr.all),
                          ProcName => To_Unbounded_String(
                          CFG.Get_Name(curCFG_ptr.all)),
                          Bound => 1,
                          BBs => null,
                          BBs_num => 0
                         );

         -- Create a loop node
         rootLoopNode := Loops.Create_Node(nInfo => rootLoopInfo,
                                           nType => Loop_Tree.RootNode,
                                           lType => Loop_Tree.Call);

         --  Create a Loop-Call tree with such node as root
         Loop_Tree.Add_Tree( Loops.Create(rootLoopNode,
           CFG.Get_Name(curCFG_Ptr.all)),
           CFG_Index);
         --  Retrieve the just created loop-tree
         curLoopTree := Loop_Tree.Get_Tree(CFG_Index);


         if (global.E_M = DEBUG or Global.tbshootLCT) then
            Put_line("Check root: ");
            --+Loops.Print_Node(Loops.Get_Root(curLoopTree.all));
            Loops.Print_Node(rootLoopNode);
         end if;


         -- Unmark the CFG nodes
         CFG.Clear_All_Marks(curCFG_ptr.all);
         --+Put_line("Marks cleared");

         -- Iterates over all CFG nodes
         for NodeIndex in 2..curCFG_Num loop

            --  The current node
            --CFG.Clear_All_Marks(curCFG_ptr.all);
            CFG_Node := Get_Vertex_By_Index(curCFG_ptr, NodeIndex,
                                            curCFG_Ptr.Number);--+CFG_Graph.Connectivity);
            if Global.tbshootLCT then
               Put ("Node"); Print_Vertex_Name(CFG_Node);
               Put_Line (" ["& Integer'Image(NodeIndex)&"]");

            end if;

            --  Dominators of the current node
            Domtors := D_Matrix (NodeIndex);

            --  For each node N (except for STUB nodes) and
            --  nodes with no successors
            --  If  number of successors <= 0 --> No successors
            succNum := CFG.Successors_Num(curCFG_ptr.all, CFG_Node);

            --  If N has at least one successor
            if (succNum > 0) then
               declare
                  --  Successor node S in succ(N)
                  S_Node : CFG_Vertex;
                  --  List of successors of N i.e., succ(N)
                  succList : CFG.Vertex_List (1..succNum) :=
                    CFG.Successors(curCFG_ptr.all,CFG_Node);
                  --  Index of S the successor node (to compute dominators)
                  succIndex : Positive;
                  --  List of basic blocks of the current node
                  curLoopBB : Loop_Tree.Loop_BBs_Ptr;
                  --  Used for node labels
                  count : Positive := 1;

               begin
                  --  Iterates over all nodes in succ(N)
                  for succ in 1..succNum loop
                     --  Get S the next successor
                     S_node := succList(succ);
                     --+Put ("   Successor:" ); Print_Vertex_Name (S_node);
                     -- and computes its index
                     succIndex := CFG.Index_Of(curCFG_ptr.all, S_node);

                     --  If S dominates N then N->S is a back edge
                     --  and we have a loop
                     if (Is_Back_Edge(Domtors, succIndex)) then
                        if (Global.E_M = VERBOSE or GLobal.tbshootLCT) then
                           Put("   Node #" & Integer'Image(succIndex)
                               & " is a dominator of node # "
                               & Integer'Image(nodeIndex) & " [ ");
                           Print_Vertex (S_node);
                        end if;

                        --  S_Node is a loop header
                        --  Create a Loop node in the Loop Tree
                        --  where Loop BB are those basic blocks in
                        --  the reverse paths from N to S_node
                        --+Put_Line("Entering in Compute_bbs");
                        Compute_BBs (curLoopBB, curCFG_ptr, count,
                                     S_Node, CFG_Node);
                        --+Put_line("h@s");

                        --  Construct loop node info
                        --  The loop header is the dominator node
                        childLoopInfo := (Id => To_Unbounded_String(S_Node.BB),
                                          Label => To_Unbounded_String("L" &
                                            Integer'Image(incremLabel)),
                                          Index => succIndex,
                                          Size => 0,
                                          CfgId => CFG.Get_Id(curCFG_ptr.all),
                                          ProcName => To_Unbounded_String(
                                            CFG.Get_Name(curCFG_ptr.all)),
                                          Bound => 9,
                                          BBs => curLoopBB,
                                          BBs_num => count
                                         );
                        --+Put_line("Child");
                        if (Global.E_M = DEBUG) then
                           Put_line("L"& Integer'Image(incremLabel));
                        end if;

                        incremLabel := incremLabel +1;
                        --  Create a loop node
                        childLoopNode := Loops.Create_Node(
                                                           nInfo => childLoopInfo,
                                                           nType => Loop_Tree.LoopNode,
                                                           lType => Loop_Tree.Call);
                        --+Put("BBs of the new loop node: " &
                        --+   S_Node.BB);Loop_tree.Print_BBs(childLoopNode);
                        --+Put_Line ("++++++");
                        --  Add it to the loop tree as a successor of the ROOT node
                        Loops.Add_Succ(Parent => rootLoopNode,
                                       Child  => childLoopNode);
                        --Put_Line("NODE ADDED : " & Utilities.UTrim(childLoopInfo.Id));
                        --Loops.Print_Node(childLoopNode);
                        --Put_Line(Integer'Image(childLoopInfo.Bound));
                     end if;
                  end loop;
               end;
            else
               --  The current node is an exit or call node
               --  Nothing to be done in this case
               --Put_Line("No successors for this node");
               null;
            end if;
            --		N->succ(N) is a back edge
            --		and succ(N) is the loop header
            --		the loop BB are included in the paths from N to succ(N)
            --	DO: Add a LOOP node to the CLS graph
            --		if such path include a STUB node
            -- 	DO: Add an outgoing PROC node to the CLS graph
            --	else find recursively
               --end;
            end loop;

            --+ Export the flat LCT tree
            --Loop_Tree.Export_To_Dot_Single (Loop_Tree.Trees, cfgNum, cfgNum, 1);
            --Loop_tree.Export_To_Dot (Loop_Tree.Trees, cfgNum, cfgNum);
            --Put_line("Exported to dot");


            --+ Flat LOOP tree constructed
            --+ Check dominance between loops
            --Put_line("Flat loop tree " & Integer'Image(CFG_index) & " constructed");
         if Global.tbshootLCT then
            Put_line("Flat loop tree constructed");
         end if;


            --Export_To_Dot_Part(DomTrees, cfgNum, cfgNum, CFG_Index);

            --raise Edge_Error;

         --  Retrieve the current loop tree
         curLoopTree := Loop_Tree.Get_Tree (CFG_Index);
         --  and its successors (all loop nodes)
         succListSize := Loops.Get_Succ_Size(rootLoopNode);
         succNum := Loops.Get_Succ_Number(rootLoopNode);
         --+Put_Line("Succs: " & Integer'IMage(succNum));
         --+New_Line;

         --  If the procedure do not include loops do nothing
         if (succNum > 1) then
            declare
               --  Set of successors (i.e. loop nodes)
               succList : Loops.Tree_Node_List (1..succListSize);
               --  Index of the loop node
               childIndex : Positive;
               --  Index of the temporary node
               tempIndex : Positive;
               -- Index of the "nearest" dominator
               domIndex : Integer := 0;
            begin
               --+Loops.Print_Node(rootLoopNode);
               --+Put_Line("Before: " & Integer'Image(succNum));
               --  Retrieve the set of successors
               succList := Loops.Get_Succ(rootLoopNode);
               -- Print the loop nodes
--                 Put_Line ("Successors in succlist:");
--                 for I in 1 .. succNum loop
--                    Put (  "                       ");  Print_Node(succList(I));
--                 end loop;
               --+Print_Succ (rootLoopNode);
               --  Iterates over those (successor) loop nodes
               for lNode in 1..succNum loop
                  -- Get the current node
                  childLoopNode := succList(lNode);
                  -- Get its information
                  childLoopInfo := Loops.Get_info(childLoopNode);
                  -- Get its index
                  childIndex := childLoopInfo.Index;
                  if Global.tbshootLCT then
                     Put_Line ("Checkig node: " & Integer'Image(childIndex));
                  end if;

                  -- Reset "domIndex"
                  domIndex := 0;

                  --+ if (lNode = 1) then
                  --+       Loop_Tree.Print(childLoopInfo);
                  --+    end if;

                  --  Dominators of the loop node
                  Domtors := D_Matrix (childIndex);
                  --+Put ("SUccessors: ");Loops.Print_Node(childLoopNode);
                  --  ** NEW METHOD ***
                  if Global.tbshootLCT then
                     Put_Line("  loopnode-" & Integer'Image(childIndex));
                  end if;

                  for temp in 1..succNum loop
                     if (temp /= lNode) then
                        if Global.tbshootLCT then
                           Put_Line ("ln"& INteger'IMage(temp));
                        end if;
                        tempLoopNode := succList(temp);
                        tempLoopNodeInfo := Loops.Get_Info(tempLoopNode);
                        tempIndex := tempLoopNodeInfo.Index;
                        --  If the child node is dominated by the current node
                        if (IsDominated(Domtors,tempIndex)) then
                           if Global.tbshootLCT then
                              Put (" Node " & Integer'Image(childIndex)&
                                   " is dominated by node " &
                                   Integer'Image(tempIndex));
                           end if;

                           --  If the child node is dominated by other nodes
                           --  in the tree we need look for the enclosing
                           --  dominator: check if this is the
                           --  "nearest" dominator.
                           --  If we already found a dominator "DOMiNDEX" for
                           --  the child node and THIS current dominator also
                           --  dominates "DOMiNDEX"
                           if domIndex /= 0 and then
                             IsDominated(D_Matrix(domIndex), tempIndex) then
                              -- Do nothing: "DOMiNDEX" is the nearest
                              -- dominator up now
                              null;
                              if Global.tbshootLCT then
                                 Put_line ("[NOT candidate]");
                              end if;
                           else
                              -- THIS dominator is nearer -> it becomes the
                              -- new "DOMiNDEX"
                              domIndex := temp;
                              if Global.tbshootLCT then
                                 Put_line ("[candidate]");
                              end if;
                           end if;
                        else
                           -- The current node is not dominated by any node
                           if Global.tbshootLCT then
                              Put_Line (" not dominated ");
                           end if;
                        end if;
                     end if;
                  end loop;

                  --  If we found at least one candidate dominator "DOMiNDEX"
                  if (domIndex /= 0) then
                     -- get the respective node in the LOOP TREE
                     tempLoopNode := succList(domIndex);
                     tempLoopNodeInfo := Loops.Get_Info(tempLoopNode);
                     tempIndex := tempLoopNodeInfo.Index;
                     --  Update the loop BB so that no dupolicate BB
                     --  belongs to more than one loop node
                     Update_BBs(tempLoopNode,childLoopNode);
                     if Global.tbshootLCT then
                        Put_line ("Adding node " & Integer'Image(childIndex) &
                                  " to node" & Integer'Image(tempIndex));
                     end if;

                     Loops.Add_Succ(tempLoopNode,childLoopNode);
                     Loops.Remove_From_Succ(childLoopNode, rootLoopNode);
                     --removeList (childIndex) := True;
                  else
                     -- Leave it as root children
                     if Global.tbshootLCT then
                        Put_line ("Loop node " & Utrim(childLoopInfo.Label)
                                  & " is not dominated by any node");
                     end if;
                     null;
                  end if;
                  --+Print_Node (rootLoopNode);
               end loop;
            end;
         end if;

         --+Export_To_Dot_Part(DomTrees, cfgNum, cfgNum, CFG_Index);
         --+Loop_Tree.Export_To_Dot_Single (Loop_Tree.Trees, cfgNum, cfgNum, 1);

         if (Global.drawGraphs or Global.tbshootLCT) then
            Loop_Tree.Export_To_Dot_Single (Loop_Tree.Trees, cfgNum, cfgNum, 1);
         end if;

         if Global.E_M in VERBOSE .. DEBUG or Global.tbshootLCT then
            Put_line ("Setting procedure nodes");
         end if;

         --  Setup procedure nodes
         succListSize := Loops.Get_Succ_Size(rootLoopNode);
         succNum := Loops.Get_Succ_Number(rootLoopNode);
         if Global.E_M in VERBOSE .. DEBUG or Global.tbshootLCT then
            Put_Line ("Root children: " & Integer'Image(succNum));
         end if;

         declare  --  Setup procedure nodes
            use Loop_Tree;
            --  Find the loop header to which the procedure call belongs
            function Get_Imm_Parent (root : Loops.Tree_Node_Ptr;
                                     dom: Positive)
                                     return Loops.Tree_Node_Ptr is
               use Loops;
               succList : Loops.Tree_Node_List_ptr := Loops.Get_Succ(root);

               node_ptr     : Loops.Tree_Node_Ptr;
               node_info    : Loop_Tree.Loop_Node_Info;
               root_info    : Loop_Tree.Loop_Node_Info := Loops.Get_Info(root);
               res : Loops.Tree_Node_Ptr := null;
            begin
               succListSize := Loops.Get_Succ_Size(root);
               succNum := Loops.Get_Succ_Number(root);
               if (succNum >0) then
                  for lN in 1..succNum loop
                     node_ptr := succList(lN);
                     node_info := Loops.Get_Info(node_ptr);
                     --+Put_line("$: " & Utilities.UTrim(node_info.Id));
                     if (node_info.Index = dom) then
                        if Global.tbshootLCT then
                           Put("Found! -> " &Integer'Image(node_info.Index));
                        end if;
                        --+Loops.Print_Node(node_ptr);
                        return node_ptr;
                     end if;
                  end loop;
                  if (Global.tbshootLCT) then
                     Put("second part <");
                  end if;

                  Recursion:
                  for lN in 1..succNum loop
                     node_ptr := succList(lN);
                     --+Loop_tree.Print_BBs(node_ptr);
                     res := Get_Imm_Parent(node_ptr, dom);
                     if (not LT.IsNull(res)) then
                        exit Recursion;
                     end if;

                  end loop Recursion;
               else
                  null;
                  --+Put_Line("No succ-->returns null");
               end if;
               if Global.tbshootLCT then
                  Put (" >");
               end if;
               return res;
            end Get_Imm_Parent;

            function Get_Parent_Node (root : Loops.Tree_Node_Ptr;
                                      dom: Positive) return Loops.Tree_Node_Ptr is
               use Loops;
               succList : Loops.Tree_Node_List_ptr := Loops.Get_Succ(root);

               node_ptr     : Loops.Tree_Node_Ptr;
               node_info    : Loop_Tree.Loop_Node_Info;
               root_info    : Loop_Tree.Loop_Node_Info := Loops.Get_Info(root);
               res : Loops.Tree_Node_Ptr;
            begin
               succListSize := Loops.Get_Succ_Size(root);
               succNum := Loops.Get_Succ_Number(root);

               if (Global.E_M = DEBUG) then
                  Put_line("Looking children of: " & Integer'Image(root_info.Index));
                  Put_Line("succNum: " & Integer'Image(succNum));
               end if;

               if (succNum >0) then
                  for lN in 1..succNum loop
                     node_ptr := succList(lN);
                     node_info := Loops.Get_Info(node_ptr);
                     if (( node_info.Index = dom)
                         or ( Get_NType(node_ptr) /= Loop_tree.ProcNode
                             and then Loop_Tree.In_BBs(node_info.BBs,
                                                       node_info.BBs_num, dom)))
                     then
                        --+Print_BBs(node_ptr);
                        if Global.tbshootLCT then
                           Put("@Found! -> " &Integer'Image(dom));
                           --+Loops.Print_Node(node_ptr);
                        end if;
                        return node_ptr;
                     end if;
                  end loop;
                  if Global.tbshootLCT then
                     Put ("second part< ");
                  end if;

                  Recursion:
                  for lN in 1..succNum loop
                     node_ptr := succList(lN);
                     --+Loop_tree.Print_BBs(node_ptr);
                     res := Get_Parent_Node(node_ptr, dom);
                     if (not LT.IsNull(res)) then
                        exit Recursion;
                     end if;

                  end loop Recursion;
               else
                  null;
                  --+Put_Line("No succ-->returns null");
               end if;
               if Global.tbshootLCT then
                  Put (" >");
               end if;
               return res;
            end Get_Parent_Node;

         begin
            --+if (succNum > 0) then
            --  Exclude Root node (it cannot be a stub node)
            for Index in 2..curCFG_Num loop
               CFG_Node := Get_Vertex_By_Index(curCFG_ptr,
                                               Index,
                                               curCFG_Ptr.Number);
               --+Put("Here:");Print_Vertex(CFG_Node);

               --  If the current node is a stub (calls a procedure)
               if (CFG_Node.typ = Stub ) then
                  if Global.tbshootLCT then
                     Put ("Intercettato Stub -> ");
                     Put_Line("n: " & CFG_node.BB);
                     CFG.Print_Vertex_Info(CFG_Node);
                     --+Put_Line("CFG: " & CFG_node.cfgId);
                  end if;
                  --  Find its predecessor (has just 1 by definition)
                  --  DOES ALWAYS HOLD TRUE
                  CFG_pred := CFG.First_Predecessor(curCFG_ptr.all, CFG_Node);

                  -- Try with the predecessor itself (special case)
                  -- in which it is a loop header
                  tempLoopNode := Get_Imm_Parent(rootLoopNode,
                                                 CFG.Index_Of(curCFG_ptr.all,CFG_pred));
                  --  if it could not be found
                  if(LT.IsNull(tempLoopNode)) then
                     if Global.tbshootLCT then
                        Put_line ("first predecessor is not a loop header");
                     end if;

                     -- Look for its IMMEDIATE dominator node
                     curIDom := IDom(CFG.Index_Of(curCFG_ptr.all,CFG_pred));
                     --  If it does not have any immediate dominator then
                     --  it is dominated by the root node
                     if (curIDom = 0) then
                        if Global.tbshootLCT then
                           Put_Line("dom by root");
                        end if;
                        curIDom := 1;
                        tempLoopNode := rootLoopNode;
                     else
                        --  Is there a node in the Loop Tree with
                        --  ID = curIDom or includes IDom in its BBs?
                        --  tempLoopNode := such node (if exists)
                        --+Put_line ("Intercetta8b - curIDOM:" &
                        --+          Integer'IMage(curIdom));
                        tempLoopNode := Get_Parent_Node(rootLoopNode,curIDom);
                        -- If it dors not exist then the node is dominated
                        -- by the root
                        if(LT.IsNull(tempLoopNode)) then
                           tempLoopNode := rootLoopNode;
                           if Global.tbshootLCT then
                              Put_Line("dom by root");
                           end if;
                        end if;
                     end if;
                  end if;

                  --+Compute the size of the referenced procedure
                  --+Proc_size := Get_Procedure_Size(Global.File_Name.all,
                  --+CFG.Get_Name(curCFG_ptr.all);
                  --+Loops.Print_node(tempLoopNode);
                  --+Put_Line("  **  " & Utilities.UTrim(tempLoopNodeInfo.Id));

                  -- Then add a ProcNode to such node
                  childLoopInfo := (Id => To_Unbounded_String(CFG_Node.BB),
                                    Label => To_Unbounded_String("P"),
                                    Index => Index,
                                    Size => 0,
                                    --  Get its CFG!
                                    CfgId => CFG_Node.cfgId,
                                    ProcName => To_Unbounded_String(
                                      CFG.Get_Name(curCFG_ptr.all)),
                                    Bound => 1,
                                    BBs => null,
                                    BBs_num => 0);


                  childLoopNode :=
                    Loops.Create_Node(nInfo => childLoopInfo,
                                      nType => Loop_Tree.ProcNode,
                                      lType => Loop_Tree.Call);

                  --+ Put_line("Check root: ");
               	  --+ Loops.Print_Node(Loops.Get_Root(curLoopTree.all));
                  --+ Loops.Print_Node(rootLoopNode);
                  Loops.Add_Succ(Parent => tempLoopNode,
                                 Child  => childLoopNode);
                  --+Put_line("Added");
               end if;
            end loop;
         end; --------------------  End Setup procedure nodes
      end;
      if (Global.E_M = VERBOSE or Global.tbshootLCT) then
         Put_Line("OK CFG " & Integer'Image(CFG_Index));
         --New_Line; New_Line;
      end if;

      -- Clear the current CFG from memory
      -- Keep both the LoopTree and the Dom Tree
      Unset(curCFG_ptr);

      -- LOOP in not needed anymore since dominator analysis is performed on
      -- each tree separatedly
      --+ end loop; -------------------  End iteration throught CFGs (procedures)

      --+Print(Get_Info(Get_root(Domtrees(1).all)));
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put_Line (" OK");
      end if;

      if (Global.drawGraphs) or Global.drawFinal then
         --Export_To_Dot_Single (DomTrees, cfgNum, cfgNum, 1);
         --Export_To_Dot (DomTrees, cfgNum, cfgNum);
         Loop_tree.Export_To_Dot_Single (Loop_Tree.Trees, cfgNum, cfgNum, 1);
      end if;

      CFG_Index := CFG_Index + 1;

   exception
      when Error : Storage_Error =>
         Put_Line(" [3] - STORAGE ERROR!!!!!!!!!!");
         Put_Line(CFG.Get_Graph_Info(curCFG_Ptr.all));
         raise;
      when Error: END_ERROR =>
         Put_Line (" Was not able to find the program executable :-(");
         Put_Line (" (unable to compute the procedure size) -> " &
                   Utilities.Trim(CFG.Get_Name(curCFG_ptr.all)));
         --Abort_Program;
         raise;
      when Error : others => Put_Line("Unknown:" & Exception_Name(Error) &
                                      Exception_Message(Error));
         raise;
   end Single_Loop_Analysis;


   --  Returns the number of Dominator trees currently in memory
   function Get_Num return Natural is
   begin
      return domTreeNum;
   end Get_Num;

end Dominator_Analysis;

