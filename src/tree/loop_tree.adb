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
-- FILE NAME      : loop_tree.adb
-- PACKAGE        : LOOP_TREE body
-- PURPOSE        : Representation of teh loop-call tree data structure
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
use Ada.Exceptions;
with CFG_Graph, Tree, WCG_Graph;
with Global; use Global;
with Interactions;
with Utilities; use Utilities;
with RAM, Tree, Xml_Parser, Ada.Integer_Text_IO;
-- Temporary 
with Ada.Calendar; use Ada.Calendar;

package body Loop_Tree is
   use Ada.Strings.Unbounded;

   markedNum : Integer := 0;

   function "=" (Left, Right: Loop_Node_Info) return Boolean is
   begin
      return Left.Id = Right.Id;
   end "=";

   function CFG_Index_By_Id (id : String) return Positive is
      temp : access String;
   begin
      temp := new String'(Trim(id(id'First + 1..id'Last)));
      --      Put_Line("STR: " & temp.all);
      --Strings. Strings.Search.Index(id, "c");
      --      for I in 1..LoopTreeNum loop
      --         curtree_ptr := LoopTrees(I);
      --         curtreeName := Get_Id(curtree_ptr);
      --        Put("IN: " & curtreeName & "=" & id);
      --        if (Trim(curtreeName) = id) then
      --            Put (" -> OK");
      --            return I;
      --         end if;
      --         New_Line;
      --      end loop;
      return Integer'Value(temp.all);
   end CFG_Index_By_Id;

   function CFG_Index_By_Id2 (id : String) return Integer is
      --temp : access String;
      curtree_ptr : Tree_Type_Ptr;
      rootNode : Tree_Node_Ptr;
      curInfo : Loop_Node_Info;
      index : Integer;
      curtreeId : access String;
   begin
      --+temp := new String'(Trim(id(id'First + 1..id'Last)));
      --+Put_Line("STR: " & temp.all);
      --+Strings.Search.Index(id, "c");
      for I in 1..LoopTreeNum loop
         curtree_ptr := LoopTrees(I);
         rootNode := Get_Root(curtree_ptr.all);
         curInfo := Get_Info(rootNode);
         index := First_Index(UTrim(curinfo.Id), 'b');
         curtreeId := new String'(UTrim(curInfo.Id)(1..index-1));
         --Put("IN: " & curtreeId.all & "=" & id);
         if (Trim(curtreeId.all) = Trim(id)) then
            --Put (" -> OK");
            return I;
         end if;
         --New_Line;
      end loop;
      return 0;
      --return Integer'Value(temp.all);
   end CFG_Index_By_Id2;

   function CFG_Index_By_Name (name : String) return Integer is
      curtreeName : String (1..60);
   begin
      for I in 1..loopTreeNum loop
         curtreeName := Get_Id(LoopTrees(I));
         --Put("IN: " & curtreeName & "=" & name);
         if(Trim(curtreeName) = name) then
            --Put(" -> OK");
            return I;
         end if;
         --New_Line;
      end loop;
      return 0;
   end CFG_Index_By_Name;



   function Includes (Left : Loop_BBs_Ptr; Right, size : Positive)
                      return Boolean is
   begin
      for index in 1..size loop
         if (Left(index)=Right) then
            return True;
         end if;
      end loop;
      return False;
   end Includes;

   procedure Replace_Node (Left, Right: in out Tree_Node_Ptr) is
      --info : Loop_Node_Info;
      --parent : Tree_Node_Ptr;
   begin
      --info := Get_Info(Right);
      --Put_Line("Replace: " & Integer'Image(info.Bound));
      --parent := Parent(Left);
      Set_Parent(Right, LT.Parent(Left));
      Left := Right;
   end Replace_Node;


   procedure Merge (Left, Right: in out Loop_Node_Info) is
      NewBBs : Loop_BBs_Ptr;
      ResBBs : Loop_BBs_Ptr;
      fromLeft : Natural := 0;
      inserted : Natural;
   begin
      NewBBs := new Loop_BBs (1..(Left.BBs_Num+Right.BBs_Num));
      for index in 1..Left.BBs_Num loop
         NewBBs (index) := Left.BBs(index);
         fromLeft := fromLeft + 1;
      end loop;
      inserted := fromLeft;
      for index in 1..Right.BBs_Num loop
         if (not Includes(NewBBs, Right.BBs(index), index + fromLeft-1)) then
            NewBBs (index + fromLeft) := Right.BBs(index);
            inserted := inserted + 1;
         end if;
      end loop;
      -- Shrink
      ResBBs  := new Loop_BBs (1..inserted);
      for index in 1..inserted loop
         ResBBs (index) := NewBBs(index);
--         fromLeft := fromLeft + 1;
      end loop;
   end Merge;


   procedure Print (N : Loop_Node_Info) is
      use Ada.Strings.Unbounded;

   begin
      Put (To_String(N.Id));
      Put ("-ProcName:  " & To_String(N.ProcName));
      Put_line ("-ind: " & Integer'Image(N.Index));
   end Print;

   procedure PrettyPrint (N : Loop_Node_Info) is
      use Utilities;
   begin
      Put ("Node: " & UTrim(N.Id) &"(#" & Integer'Image(N.Index) & ")");
   end PrettyPrint;


   function Print_Nt (nt : Loop_Tree_Node_Type) return String is
   begin
      -- LoopNode, ProcNode, RootNode
      if (nt=LoopNode) then
         return "LoopNode";
      elsif (nt=ProcNode) then
         return "ProcNode";
      elsif (nt=IndependentProcNode) then
         return "NL-ProcNode";
      else

         return "RootNode";
      end if;

   end Print_Nt;


   function Print_Lt (lt : Loop_Tree_Link_Type) return String is
   begin
      return "Call";
   end Print_Lt;


   function Get_Node_Info (n : Tree_Node_Ptr) return Loop_Node_Info is
   begin
      return Get_Info(n);
   end Get_Node_Info;

   function Get_Tree (index : Positive) return LT.Tree_Type_Ptr is
   begin
      return LoopTrees(index);
   end Get_Tree;

   procedure Init_trees (size : Positive) is
   begin
      LoopTrees := new Loop_Tree_Array (1..size);
      loopTreeNum := size;
   end Init_Trees;

   procedure Set_BBs(N: in out Tree_Node_Ptr; newBBs : Loop_BBs_Ptr;
                    size : Positive) is
      nodeInfo : Loop_Node_Info := Get_Info(N);
      newNodeInfo : Loop_Node_Info;
   begin
      newNodeInfo := (Id => nodeInfo.Id,
                      Label => nodeInfo.Label,
                      Index => nodeInfo.Index,
                      Size => nodeInfo.Size,
                      CfgId => nodeInfo.CfgId,
                      ProcName => nodeInfo.ProcName,
                      Bound => nodeInfo.Bound,
                      BBs => newBBs,
                      BBs_num => size);
      Set_Info(N, newNodeInfo);
   end Set_BBs;

   procedure Set_Bound(n : in out Tree_Node_Ptr; bound : Positive) is
      nodeInfo : Loop_Node_Info := Get_Info(N);
      newNodeInfo : Loop_Node_Info;
   begin
      newNodeInfo := (Id => nodeInfo.Id,
                      Label => nodeInfo.Label,
                      Index => nodeInfo.Index,
                      Size => nodeInfo.Size,
                      CfgId => nodeInfo.CfgId,
                      ProcName => nodeInfo.ProcName,
                      Bound => bound,
                      BBs => nodeInfo.BBs,
                      BBs_num => nodeInfo.BBs_num);
      Set_Info(n, newNodeInfo);

   end Set_Bound;

   procedure Add_Tree (treePtr : LT.Tree_Type_Ptr; index : Positive) is
   begin
      LoopTrees (index) := treePtr;
   end Add_Tree;

   procedure Export_To_Dot_Rec (output : in out File_type;
                                loopTreeList: Loop_Tree_Array_ptr;
                                loopNode : LT.Tree_Node_Ptr;
                                size : Positive) is
      use Global;
      use LT;
      use Ada.Strings.Unbounded.Text_IO;
      use Ada.Strings.Fixed;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      node_ptr    : LT.Tree_Node_Ptr;
      nodeInfo    : Loop_Node_Info;
      childNum    : Natural := Get_Succ_Number (loopNode);
      succSize    : Positive := Get_Succ_Size(loopNode);
   begin
      --Put_Line("In ExportDotRec");
      --Put_Line("Export " & Integer'Image(childNUm));
      if(childNum>0) then
         declare
            children : Tree_Node_List (1..succSize) := Get_Succ(loopNode);
            parentNodeInfo : Loop_Node_Info := Get_Info (loopNode);
         begin
            for index in 1..childNum loop
               --Put_Line(Integer'Image(index));
               node_ptr := children(index);
               --if ( not Is_Marked(node_ptr)
                --  or Get_NType(node_ptr) = ProcNode) then
                  --Put_Line("node is not marked");
                  nodeInfo := Get_Info (node_ptr);
                  --if (Get_LType(node_ptr) = Call) then
               if (Get_NType(node_ptr) = RootNode or
                     Get_NType(node_ptr) = IndependentProcNode) then
                     --Put_Line("In E2DotRec: " & UTrim(nodeInfo.Id)
                     --         & UTrim(nodeInfo.ProcName));
                     Put_Line (output, To_String(nodeInfo.Id) & "[shape=box]" &
                               "[label=""" --& To_String(nodeInfo.Id) & "\n"
                               & Trim(To_String(nodeInfo.ProcName))
                                 & "\n" & Integer'Image(nodeInfo.Size) & "Bytes"
                               & "\n(" & Print_Nt(Get_NType(node_ptr)) &
                               --")"", style=dotted];");
                               ")""];");
                  else

                     if (Global.E_M = GRAPHDB) then
                        Put_Line (output, To_String(nodeInfo.Id) &
                       "[label=""" & Trim(To_String(nodeInfo.Id)) &
                               "\n" & To_String(nodeInfo.ProcName) &
                       "\n(" & Print_Nt(Get_NType(node_ptr)) &
                       "\n" & Get_BBs(node_ptr) &
                          ")""];");
                     else

                     --Put_Line("Adding to dot");
                     Put_Line (output, To_String(nodeInfo.Id) & --"[shape=box]" &
                       "[label=""" & Trim(To_String(
                         nodeInfo.Label)) & """];");
                         --nodeInfo.Id)) &
                           --    "\n" & To_String(nodeInfo.ProcName) &
                       --"\n(" & Print_Nt(Get_NType(node_ptr)) &
                       --"\n" & Get_BBs(node_ptr) &
                        --  ")""];");
                     end if;

                  end if;

                  Mark_Node (node_ptr);
               --end if;
               Export_To_Dot_Rec (output, loopTreeList, node_ptr, size);

               --  Add an dege between the two nodes
--                 Put_Line (output, To_String(parentNodeInfo.Id) & "-> " &
--                           To_String(nodeInfo.Id) & " [label="" " &
--                           Integer'Image(parentNodeInfo.Bound)
--                           & " ""];");

               Put (output, To_String(parentNodeInfo.Id) & "-> " &
                    To_String(nodeInfo.Id));

               --Put_Line(" *** " & Integer'Image(parentNodeInfo.Bound));
               if (parentNodeInfo.Bound>1) then
                  Put (output, " [label="" " & Integer'Image(parentNodeInfo.Bound)
                       & " ""]");
               end if;
               Put_Line(output, ";");

            end loop;
         end;
      end if;
      --Put_line("&&&&");
      --Put_line("££");
   exception
      when Error: Storage_Error =>
         Put_Line("Export Dot REC --> STORAGE ERROR!!!!!!!!!!");
         raise;
      when Error: others =>
         Put_line(Exception_Name(Error));
         raise;
   end Export_To_Dot_Rec;



   procedure Export_To_Dot (Tree_list : Loop_Tree_Array_ptr; trees : Positive;
                            size : Positive) is
      use Global;
      use LT;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      use Interactions;
      Input, Output : File_Type;
      curTree_ptr : LT.Tree_Type_Ptr;
      curTree 	  : LT.Tree_Type;
      node_ptr    : LT.Tree_Node_Ptr;
      --node    	  : DT.Tree_Node;
      nodeInfo    : Loop_Node_Info;
   begin
      --Put_Line("Trees in export: " & integer'Image(trees));
      if (Global.E_M = VERBOSE) then
         Put ("Exporting the Loop Graph...");
      end if;

      Create (Output, Out_File, To_String(Global.Program)&"_LOOP.dot");
      Put_line(Output, "/* DOT Graph description");
      Put_line(Output, " * Dominator tree");
      Put_line(Output, " * Generated by OptLayout");
      Put (Output, " * Main program: ");
      Put_Line (Output, Global.Program);
      Put_line(Output, " */");
      Put_line(Output, "digraph GFG {");
--        if(Global.E_M = GRAPHDB) then
--           Put_Line(Output, "size = """ & Integer'Image(size) &
--                    "," & Integer'Image(size) & """;");
--        else
--           Put_Line(Output, "size = """ & Integer'Image(4*size) &
--                    "," & Integer'Image(4*size) & """;");
--        end if;
      Put_Line(Output, "size = "" 10, 7.5""");
      Put_Line(Output, "ratio =""auto""");
      Put_Line(Output, "orientation = ""landscape""");

      for tree in 1..trees loop
         --Put("CFG- " & Integer'Image(tree));
         curTree_ptr := Tree_List (tree);
         Unmark_Tree(curTree_ptr);
         curTree := curTree_ptr.all;
         node_ptr := Get_Root(curTree);
         Unmark_Tree(node_Ptr);
         --node := node_ptr.all;
         --  If the node has not been printed yet
         --if (not Is_marked(node_ptr) or tree = 1) then
            --Put_line ("Adding a node to DOT");
         nodeInfo := Get_Info (node_ptr);
         if (tree = 1) then
            Ada.Text_IO.Put_Line (Output, To_String(nodeInfo.Id)
                      & "[shape=box]"
                      & "[label=""" --& To_String(nodeInfo.Id) & "\n"
                      & Trim(To_String(nodeInfo.ProcName)) &
                      "\n" & Integer'Image(nodeInfo.Size) & "Bytes" &
                      "\n(" & Print_Nt(Get_NType(node_ptr)) & ")""];");
         else
            --  For a better consistent graph visualization
            Put_Line (Output, To_String(nodeInfo.Id)
                      & "[shape=box]"
                      & "[label=""" --& To_String(nodeInfo.Id) & "\n"
                      & Trim(To_String(nodeInfo.ProcName)) &
                      "\n" & Integer'Image(nodeInfo.Size) & "Bytes" &
                      "\n(ProcNode)""];");
         end if;

         Mark_Node (node_ptr);
         --end if;
         Export_To_Dot_Rec(output,
                           Tree_List,
                           node_ptr,
                           size);

          --Put("*" & Integer'Image(tree));
      end loop;
      --Put_Line("qui");

      Put_line(Output, "}");
      Put_line(Output, "/* End of file */");
      Close (Output);
      Normalize_Dot (To_String(Global.Program) & "_LOOP");
      Execute_Dot (To_String(Global.Program) & "_LOOP");
      if (Global.E_M = VERBOSE) then
         Put_Line("Loop Graph successfully created");
      end if;
   exception
      when Error : Storage_Error =>
         Put_Line("Export STORAGE ERROR!!!!!!!!!!");
      when Error:others =>
         Put_Line(Exception_Message(Error));

   end Export_To_Dot;

   procedure Export_To_Dot_Single (Tree_list : Loop_Tree_Array_ptr; trees : Positive;
                            size : Positive; cfgInd : Positive) is
      use Global;
      use LT;
      use Ada.Strings.Unbounded.Text_IO;
      package U_IO renames Ada.Strings.Unbounded.Text_IO;
      use Interactions;
      Input, Output : File_Type;
      curTree_ptr : LT.Tree_Type_Ptr;
      curTree 	  : LT.Tree_Type;
      node_ptr    : LT.Tree_Node_Ptr;
      --node    	  : DT.Tree_Node;
      nodeInfo    : Loop_Node_Info;
   begin
      if (Global.E_M = VERBOSE) then
         Put ("Exporting the Loop Graph...");
      end if;

      Create (Output, Out_File, To_String(Global.Program)&"_part_LOOP.dot");
      Put_line(Output, "/* DOT Graph description");
      Put_line(Output, " * Dominator tree");
      Put_line(Output, " * Generated by OptLayout");
      Put (Output, " * Main program: ");
      Put_Line (Output, Global.Program);
      Put_line(Output, " */");
      Put_line(Output, "digraph GFG {");
--        Put_Line(Output, "size = """ & Integer'Image(4*size) &
--                 "," & Integer'Image(4*size) & """;");
      Put_Line(Output, "size = "" 10, 7.5""");
      Put_Line(Output, "ratio =""auto""");
      Put_Line(Output, "orientation = ""landscape""");
      --for tree in 1..trees loop
         --Put("CFG- " & Integer'Image(tree));
         curTree_ptr := Tree_List (cfgInd);
         Unmark_Tree(curTree_ptr);
         curTree := curTree_ptr.all;
         node_ptr := Get_Root(curTree);
         Unmark_Tree(node_Ptr);
         --node := node_ptr.all;
         --  If the node has not been printed yet
         --if (not Is_marked(node_ptr) or tree = 1) then
            --Put_line ("Adding a node to DOT");
         nodeInfo := Get_Info (node_ptr);
         if (cfgInd = 1) then
            Put_Line (Output, To_String(nodeInfo.Id)
                      & "[shape=box]"
                      & "[label=""" --& To_String(nodeInfo.Id) & "\n"
                      & Trim(To_String(nodeInfo.ProcName)) &
                      "\n(" & Print_Nt(Get_NType(node_ptr)) & ")""];");
         else
            --  For a better consistent graph visualization
            Put_Line (Output, To_String(nodeInfo.Id)
                      & "[shape=box]"
                      & "[label=""" & To_String(nodeInfo.Id) & "\n"
                      & Trim(To_String(nodeInfo.ProcName)) &
                      "\n(ProcNode)""];");
         end if;

         Mark_Node (node_ptr);
      --end if;

         Export_To_Dot_Rec(output,
                           Tree_List,
                           node_ptr,
                           size);

      --end loop;

      Put_line(Output, "}");
      Put_line(Output, "/* End of file */");
      Close (Output);
      Execute_Dot (To_String(Global.Program) & "_part_LOOP");
      if (Global.E_M = VERBOSE) then
         Put_Line("Loop Graph successfully created");
      end if;
      --Put_Line("Loop Graph "&Integer'IMage(cfgInd)&" successfully created");
   exception
      when Error : Storage_Error =>
         Put_Line("Export STORAGE ERROR!!!!!!!!!!");
         raise;
      when Error:others =>
         Put_Line(Exception_Message(Error));
         raise;
   end Export_To_Dot_Single;


   function Trees return Loop_Tree_Array_Ptr is
   begin

      return LoopTrees;
   end Trees;

   function Get_BBs (n : LT.Tree_Node_Ptr) return String is
      res : access String;
      nInfo : Loop_Node_Info := Get_Node_Info(N);
      bbs : Loop_BBs_Ptr := nInfo.BBs;
      bbNum : Natural := nInfo.BBs_num;
   begin
      if (bbNum/=0) then
         res := new STring'("Loop BBs: ");
         for index in 1..bbNum loop
            res := new String'(res.all & (Integer'Image(bbs(index)) & " - "));
         end loop;
         return res.all;
      end if;
      return "No BBs";
   end Get_BBs;

   function GetNumBB (n : LT.Tree_Node_Ptr) return Natural is
      nInfo : Loop_Node_Info := Get_Node_Info(N);
   begin
      return nInfo.BBs_num;
   end GetNumBB;


   procedure Print_BBs (n : LT.Tree_Node_Ptr) is
      nInfo : Loop_Node_Info := Get_Node_Info(N);
      bbs : Loop_BBs_Ptr := nInfo.BBs;
      bbNum : Natural := nInfo.BBs_num;
   begin

      if (bbNum/=0) then
         Put ("Loop BBs: ");
         for index in 1..bbNum loop
            Put_Line(Integer'Image(bbs(index)) & " - ");
         end loop;
      end if;

   end Print_BBs;

   function In_BBs (BBs : Loop_BBs_Ptr; size : Positive; elem : Positive)
                    return Boolean is
   begin
      for bb in 1..size loop
         if(BBs(bb) = elem) then
            return True;
         end if;
      end loop;
      return False;
   end In_BBs;


   function Get_Tree_By_Name(name : String) return Tree_Type_Ptr is
      treeList : Loop_Tree_Array_ptr := Trees;
      curTree : Tree_Type_Ptr;
   begin
      --Put_Line("loopTrees #" & Integer'Image(loopTreeNum));
      for index in 1..loopTreeNUm loop
         curTree := treeList(index);
         if (Global.E_M = DEBUG) then
            if (curTree = null) then
               Put_Line("Lista loop tree vuota!!");
            else
               Put_Line("name:" & Trim(Get_Id(curTree)) & "***");
            end if;
         end if;

         if Trim(Get_Id(curTree))=name then
            return curTree;
         end if;

      end loop;
      return null;
   end Get_Tree_By_Name;

   function Get_Loops (root : LT.Tree_Node_Ptr)
                       return Natural is
      succNum : Natural := LT.Get_Succ_Number(root);
      succList : LT.Tree_Node_List := LT.Get_Succ(root);
      succNode : LT.Tree_Node_Ptr;
      count : Natural := 0;
   begin
      if succNum > 0 then
         for n in 1..succNum loop
            succNode := succlist(n);
            if Get_NType(succNode)=LoopNode then
               count := Get_Loops(succNode) + 1;
            end if;
         end loop;
      end if;

      return count;
   end Get_Loops;


   function Get_Loops (treePtr : LT.Tree_Type_Ptr) return Natural is
      root : LT.Tree_Node_Ptr := Get_Root(treePtr.all);
      succNum : Natural := LT.Get_Succ_Number(root);
      succList : LT.Tree_Node_List := LT.Get_Succ(root);
      succNode : LT.Tree_Node_Ptr;
      count : Natural := 0;
   begin
      if succNum > 0 then
         for n in 1..succNum loop
            succNode := succlist(n);
            if Get_NType(succNode)=LoopNode then
               count := Get_Loops(succNode) + 1;
            end if;
         end loop;
      end if;

      return count;
   end Get_Loops;


   function Get_Loop_By_Label (node:  Tree_Node_Ptr; name : String)
                               return Tree_Node_Ptr is
      succNum : Natural := LT.Get_Succ_Number(node);
      succList : LT.Tree_Node_List := LT.Get_Succ(node);
      succNode : LT.Tree_Node_Ptr;
      resNode : LT.Tree_Node_Ptr;
      info : Loop_Tree.Loop_Node_Info;
   begin
      if succNum > 0 then
         for n in 1..succNum loop
            succNode := succlist(n);
            if (Get_NType(succNode)=LoopNode) then
               info := Get_Info(succNode);
               if (info.Label=name) then
                  return succNode;
               else
                  resNode := Get_loop_By_Label(succNode, name);
                  if (resNode /= null) then
                     return resNode;
                  end if;
               end if;
            end if;
         end loop;
      end if;
      return null;
   end Get_Loop_By_Label;



   function Get_Loop_By_Label (tree: Tree_Type_Ptr; name : String)
                               return Tree_Node_Ptr is
      root : LT.Tree_Node_Ptr := Get_Root(tree.all);
      succNum : Natural := LT.Get_Succ_Number(root);
      succList : LT.Tree_Node_List := LT.Get_Succ(root);
      succNode : LT.Tree_Node_Ptr := null;
      resNode : LT.Tree_Node_Ptr := null;
      info : Loop_Tree.Loop_Node_Info;
   begin
      if succNum > 0 then
         for n in 1..succNum loop
            succNode := succlist(n);
            if (Get_NType(succNode)=LoopNode) then

               if (Global.E_M = DEBUG) then
                  Put(Print_Nt(Get_NType(succNode)));
                  Put_line(To_String(info.Label));
               end if;

               info := Get_Info(succNode);
               if UTrim(info.Label) = name then
                  return succNode;
               else
                  resNode := Get_loop_By_Label(root, name);
                  if (succNode /= null) then
                     return resNode;
                  end if;
               end if;
            end if;
         end loop;
      end if;
      return resNode;
   end Get_Loop_By_Label;

   procedure Export_to_Dot_Bounded is
   begin
      Export_To_Dot(LoopTrees, 1, 3);
   end Export_to_Dot_Bounded;

   procedure Merge_Tree_Aux (root : in out LT.Tree_Node_Ptr) is
      succNum : Natural := LT.Get_Succ_Number(root);
      succList : LT.Tree_Node_List_ptr := LT.Get_Succ(root);
      succNode : LT.Tree_Node_Ptr := null;
      childTree : LT.Tree_Type_Ptr;
      childTreeRoot : LT.Tree_Node_Ptr;
      rootinfo : Loop_Tree.Loop_Node_Info := Get_Info(root);
      info : Loop_Tree.Loop_Node_Info;
      calledCfg : Positive;
   begin
      if succNum > 0 then
         if Global.tbshootLCT then
            Put_Line("Merging Tree " & Utrim(rootinfo.Id) &
                     ":'" & UTrim(rootinfo.ProcName) & "'");
            --+Put_line("Lbl: " & Utrim(rootinfo.Label) & " '"
            --+& UTrim(rootinfo.ProcName) & "'");
         end if;

         for n in 1..succNum loop
            succNode := succlist.all(n);
            if (Get_NType(succNode)=ProcNode) then
               info := Get_Info(succNode);
               if Global.tbshootLCT then
                  Put("   " & Utrim(rootinfo.Id) & " calls " &
                      info.CfgId & "'" & UTrim(info.ProcName) & "'");
               end if;
               --+calledCfg := CFG_Index_By_Id(info.CfgId);
               calledCfg := CFG_Index_By_Id2(info.CfgId);

               if Global.tbshootLCT then
                  Put_line ("->" & Integer'Image(calledCFG));
                  --Put_line(Integer'Image(LoopTreeNum));
                  if(calledCFG=0) then
                     Put_line("????");
                  end if;
               end if;

               childTree := Trees(calledCfg);
               --childTree := Get_Tree_By_Id (
               childTreeRoot := Get_Root(childTree.all);
               --Print_Node(succNode);
               --Print_Node(childTreeRoot);

               Replace_Node(succNode, childTreeRoot);
               succList.all(n) := succNode;
               --Put_Line("$$$$$$$" & Integer'Image(Get_Succ_Number(root)));
               --Print_Node(succList.all(n));
               --Put_Line("$$$$$$$");
               --Put_Line("Info: " & Utilities.UTrim(info.ProcName) & "-" & info.CfgId);
               --Put ("  Going deeper: "& Integer'Image(calledCFG));
               --Merge_Tree_Aux(childTreeRoot);
            else
               Merge_Tree_Aux(succNode);
               --null;
            end if;
         end loop;
      else
         if Global.tbshootLCT then
            Put_line (" no succ!");
         end if;
      end if;
   exception
         when Error : others => Put_Line("Merge_Aux:" & Exception_Name(Error) &
                                         Exception_Message(Error));
         raise;
   end Merge_Tree_Aux;


   procedure Merge_Trees is
      tempLoopTree : LT.Tree_Type_Ptr;
      rootNode : LT.Tree_Node_Ptr;
   begin
      --Put_Line ("Constructing the final loop-call tree");
      --  If we are analysing a leaf procedure
      --  we already have a single tree
      if (loopTreeNum > 1) then
         if(Global.E_M in VERBOSE..MINIMAL) then
            Put ("..........");
         end if;
         for t in reverse 1..loopTreeNUm loop
            tempLoopTree := Get_Tree(t);
            rootNode := Get_Root(tempLoopTree.all);
            if Global.tbshootLCT then
               Put_Line("********RootChild " & Utrim(Get_Info(rootnode).Id) &
                        " "& Integer'Image(t) & "******");
               --Put_line(CFG.Get_Name(tempLoopTree.all));
            end if;

            Merge_Tree_Aux(rootNode);
         end loop;
      else
         -- Already a single LCT
         if(Global.E_M in VERBOSE..MINIMAL) then
            Put ("..(none)..");
         end if;
      end if;
      if Global.drawGraphs or drawFinal then
         --Put_line("Exporting to Dot (bounded)");
         Export_to_Dot_Bounded;
      end if;

      exception
      when Error : Storage_Error =>
         Put_Line("4 - STORAGE ERROR!!!!!!!!!!");
         raise;
         --Put_Line(CFG.Get_Graph_Info(curCFG_Ptr.all));
      --when Error: END_ERROR =>
         --Put_Line ("Was not able to find the program executable");
         --Put_Line ("(unable to compute the procedure size)");
         --Abort_Program;
         --raise;
      when Error : others => Put_Line("Unknown:" & Exception_Name(Error) &
                                      Exception_Message(Error));
         raise;



   end Merge_Trees;

   function Get_Tree_Num return Natural is
   begin
      return loopTreeNum;
   end Get_Tree_Num;

   procedure Print_Tree_Names is
      t : Tree_Type_Ptr;
   begin
      for I in 1..loopTreeNUm loop
         t := Get_Tree(I);
         Put_Line("Tree: " & Integer'Image(I) &
                  " -> " & Trim(Get_Id(t)));
      end loop;
   end Print_Tree_Names;

   --  Recursively computes the Height of a sub-tree
   procedure Max_Height (n : Tree_Node_Ptr; h : in out Natural) is
      childNum : Natural := Get_Succ_Number(n);
      succList : Tree_Node_List_Ptr := Get_Succ(n);
      child : Tree_Node_Ptr;
      nodeInfo : Loop_Node_Info;
      temp : Positive;
   begin
      if (childNum = 0) then
         --Put_Line("Nochildren");
         return;
      elsif (Get_Weight(n) > 0) then
         h := Get_Weight(n);
         return;
      else
         h := h+1;
         temp := h;
         --Put("H: " & Integer'Image(H));
         for I in 1..childNUm loop
            child := succList(I);
            nodeInfo := Get_Info(child);
            if (Get_NType(child) = LoopNode) then
               temp := temp + 2;
            end if;

            --Put_Line(" Node: " & UTrim(nodeInfo.Id));
            Max_Height(child, temp);
            if(temp>h) then
               h:=temp;
            end if;
         end loop;
      end if;
      Set_Weight (n, h);
      --Put_Line ("Max height " & INteger'Image(h));
   end Max_Height;

   procedure Order_Tree (n: Tree_Node_ptr) is
      childNum : Natural := Get_Succ_Number(n);
      Heights : array (1..childNUm) of Natural := (others => 0);
      succList : Tree_Node_List_Ptr := Get_Succ(n);
      child, cS: Tree_Node_Ptr;
      swap : Boolean := True;
      nodeInfo : Loop_Node_Info;
      a,b,s : Integer;
   begin
      if (not Is_marked(N)) then
         -- For each successor in succ(n)
         for I in 1..childNUm loop
            -- Increment its Height (because of its deepness)
            Heights(I) := Heights (I) + 1;
            child := succList(I);
            --  Compute the max height of its sub-tree
            Max_Height (child, Heights(I));
            if(Global.E_M = VERBOSE) then
               nodeInfo:= Get_Info(n);
               Put_Line("Node: " & Utrim(nodeINfo.ProcName)
                        & "Max height '" & Integer'Image(Heights(I)) & "'");
            end if;
            --  Order such subtree recursively
            Order_Tree (child);
         end loop;
         if (Global.E_M = DEBUG) then
            Put_line ("Bubblesorting");
         end if;
           -- Bubble sorting
         while (swap) loop
            swap := False;
            for I in 1..childNum-1 loop
               a := Heights(I);
               --Put ("a " & Integer'Image (a));
               b := Heights(I+1);
               --Put ("b " & Integer'Image (b));
               cS := succList(I);
               if (a<b) then
                  succList(I) := succList(I+1);
                  succList(I+1) := cS;
                  s := Heights(I);
                  Heights(I):= b;
                  Heights(I+1) := s;
                  --Put_Line("[*]");
                  swap := True;
               end if;
               if (Global.E_M = DEBUG) then
                  nodeInfo := Get_Info(cS);
                  Put_Line ("Rec call to " & UTrim(nodeInfo.Id));
               end if;
               --+if not Is_marked (cs) then
                --Order_Tree (cS);
                  --Mark_Node (cs);
               --+end if;
            end loop;
         end loop;

         markedNum := markedNum + 1;
         Mark_Node (n);
--           if (markedNum mod 50 = 0) then
--              Put_line ("Marked: " & Integer'Image(markedNum));
--           end if;
      end if;
   exception
      when Error: others =>
         Put_line("Order: " & Exception_Name(Error));
         raise;
   end Order_Tree;


   procedure Find_Independent_Procedures (n : Tree_Node_Ptr;
                                                   childProc : in out Natural) is
      use Global;
      child : Tree_Node_Ptr;
      childNum : Natural := Get_Succ_Number(n);
      cList : Tree_Node_List_Ptr := Get_Succ(n);
      nInfo : Loop_Node_Info := Get_Info(n);
   begin
      if (Get_NType(n)=ProcNode or
            Get_NType(n)=RootNode) then
          childProc := childProc + 1;
      end if;
      if (childNum > 0) then
         for c in 1..childNum loop
            child := cList(c);
            --cArity := Get_Succ_Number(child);
            Find_Independent_Procedures(child, childProc);
            --if (Get_NType(child)=ProcNode) then
            --   childProc := childProc + 1;
            --end if;
         end loop;
      --elsif (Get_NType(n)=ProcNode) then
         --  Has no children and is a ProcNode
      --   Set_NType (n, IndependentProcNode);
      end if;
   end Find_Independent_Procedures;

   procedure Mark_Independent (n : Tree_Node_Ptr; flag : in out Boolean) is
      childNum : Natural := Get_Succ_Number(n);
      cList : Tree_Node_List_Ptr := Get_Succ(n);
      child : Tree_Node_Ptr;
   begin
      --Put ("m ");
      if (not flag) then
         --Put_Line("Type" & Print(Get_NType(n)));
         if (Get_NType (n) = RootNode) then
            Set_NType (n, IndependentProcNode);
            --Put_Line("->" & Print(Get_NType(n)));
            flag := True;
         elsif (childNUm > 0) then
            searchLoop:
            for I in 1..childNum loop
               child := cList(I);
               Mark_Independent(child, flag);
               if (flag = True) then
                  exit searchLoop;
               end if;
            end loop searchLoop;
         end if;
      end if;
   end Mark_Independent;



   procedure Update_Node_Types (n : Tree_Node_Ptr) is
      childProc : Natural := 0;
      childNum : Natural := Get_Succ_Number(n);
      cList : Tree_Node_List_Ptr := Get_Succ(n);
      child : Tree_Node_Ptr;
      flag : Boolean := False;
   begin
      for I in 1..childNum loop
         child := cList(I);
         Find_Independent_Procedures(child, childProc);
         --Put_line ("childProc number: " & Integer'Image(childProc));
         if (childProc <= 1) then
            -- this sub tree is independent
            -- Mark the (one and only) procedure
            -- as INDEPENDENT
            Mark_Independent (child, flag);
         end if;
         childProc := 0;
         flag := False;

--           Find_NonLooped_Procedures(n, childProc);
--           Put_line("childProc: " & Integer'Image(childProc));
--           if (childProc <= 1) then
--              Put_line ("Set_NType (n, " & Print(NonLoopedProcNode));
--              Set_NType (n, NonLoopedProcNode);
--              -- Does not change
--              Put_Line("->" & Print(Get_NType(n)));
--           end if;
--           childProc := 0;
      end loop;
   end Update_Node_Types;


   -- Used for Prepocessing of shared procedures
   alreadyMapped : Ram.Contents;
   alreadyNum    : Natural;
   tobeMapped    : Integer;


   --  Depth-first bottom exploration of a sub-tree
   --  collecting information on shared procedures
   procedure Preprocess_SubTree (n : Tree_Node_Ptr; childNum : Natural;
                                 sharing : in out Boolean) is
      use Ram;
      --subTreesNum : Positive := Get_Succ_Number (n);
      subTreesList : Tree_Node_List_Ptr := Get_Succ (n);
      child : Tree_Node_Ptr;
      childArity : Natural;
      nInfo : Loop_Node_Info;
      curBin  : Bin_Ptr;
      procNum : Positive;
      curDesc : Descriptor;
      flag, placeHolder : Boolean := False;
   begin
      Put (":");
      if (childNum > 0) then
         for c in 1..childNum loop
            child := subTreesList (c);
            childArity := Get_Succ_Number (child);
            Preprocess_SubTree (child, childArity, sharing);
         end loop;
      end if;

      -- If node_Type = LoopNode
      if (Get_NType(n) /= LoopNode) then
         nInfo := Get_Info(n);
         mainLoop : for I in 1..ramSize loop
            curBin := Get_Ram_Sets(I);
            if (curBin.State /= FREE) then
               procNum := curBin.Proc'Length;
               for d in 1..procNum loop
                  curDesc := curBin.Proc(d);
                  if (Trim(curDesc.name)=Utrim(nInfo.ProcName)) then
                     if (Global.E_M in VERBOSE..DEBUG) then
                        Put_Line (" Procedure '" & Utrim(nInfo.ProcName)
                                  & "' already mapped");
                     end if;

                     declare
                        tempProc : Contents;
                        newProc : Contents := new Procedures (1..alreadyNum+1);
                     begin
                        --  Copy the shared procedures collected so far
                        if (alreadyNum > 0) then
                           for I in 1..alreadyNum loop
                              --Put_line ("Copying...");
                              newProc(I) := alreadyMapped(I);
                           end loop;
                        end if;
                        alreadyNum := alreadyNum +1;
                        newProc (alreadyNum) := curDesc;
                        tempProc := alreadyMapped;
                        alreadyMapped := newProc;
                        Free_Proc (tempProc);
                     end;

                     flag := True;
                     sharing := True;
                     exit mainLoop when flag = True;
                  end if;
               end loop;
            else
               --Put_Line("EXIT");
               exit mainLoop;
            end if;
            if(curBin.State /= FULL) then
               exit mainLoop;
            end if;
         end loop mainLoop;
         if (not flag) then
            tobeMapped := tobeMapped + nInfo.Size;
         end if;
      end if;
   exception
      when Error: others =>
         Put_line("Preprocess_SubTree -> " & Exception_Name(Error));
         raise;
   end Preprocess_SubTree;

   --PST_MappedProcedure : RAM.Contents;

   procedure Process_Sub_Tree (n : Tree_Node_Ptr;
                               childNum : Natural;
                               pool : in out RAM.Proc_pool_ptr) is
   use Global, RAM;
      child : Tree_Node_Ptr;
      cArity: Natural;
      curDesc : Descriptor;
      cList : Tree_Node_List_Ptr := Get_Succ(n);
      nInfo : Loop_Node_Info := Get_Info(n);
      childInfo : Loop_Node_Info;
      found : Boolean;
      --+temp_pool : RAM.Proc_pool_ptr;
   begin
      --+Put_line ("Preprocess");
      --  Depth first recursion
      if (childNum > 0) then
         for c in 1..childNum loop
            --+Put(">");
            child := cList(c);
            childInfo := Get_Info (child);
            --+Put ("£");
-- ***************************************************************************
            if Get_NType (child) /= LoopNode then
               -- This is a PROC NODE
               if Is_marked_Curr (n => child) then
                  -- It has been already analysed in this subtree
                  -- Just stop recursion
                  --+Put_Line ("Already visited in the current subtree");
                  null;
               elsif Is_marked(n => child) then
                  -- It has been already analysed in a previous subtree
                  -- However, if already mapped we may need to account
                  -- for the sub-tree rooted at this procedure for
                  -- conflict avoidance. Therefore:
                  -- Check if it has been already mapped ?
                  Already_Mapped(procName => childInfo.ProcName,
                                 found    => found,
                                 d        => curDesc);
                  -- Probably it is not needed: if already veisited in
                  -- a previous subtree the it has been mapped as well
                  -- Mark this node with Mark_Node_Curr to avoid reanalysing
                  -- it as part of the same current sub tree
                  Mark_Node_Curr (n => child);
                  if (found) then
                     -- Add this procedure to "alreadyMApped"
                     declare
                        tempProc : Contents;
                        newProc : Contents := new Procedures (1..alreadyNum+1);
                     begin
                        --  Copy the shared procedures collected so far
                        if (alreadyNum > 0) then
                           for I in 1..alreadyNum loop
                              --Put_line ("Copying...");
                              newProc(I) := alreadyMapped(I);
                           end loop;
                        end if;
                        alreadyNum := alreadyNum +1;
                        newProc (alreadyNum) := (curDesc.name,
                                                 curDesc.size,
                                                 curDesc.startAdd,
                                                 curDesc.offSet);
                        --curDesc;
                        tempProc := alreadyMapped;
                        alreadyMapped := newProc;
                        Free_Proc (tempProc);
                     end;
                     --  Update the sharing flag
                     pool.sharing := True;
                     -- We do not stop recursion: collect all the procedure in
                     -- the sub tree for conflivt reduction
                     -- All successors will be in the same categorization as
                     -- the current node
                     -- ** CHANGE **
                     -- Since the procedure has been already mapped even the
                     -- respective subtree has been mapped
                     -- Therefore we can avoid any further investigation on
                     -- the subtree
                     -- **Moreover**: conflict avoidance should focus on the
                     -- already mapped procedures and the procedures collected
                     -- so far in the pool!
                     -- **cArity := Get_Succ_Number (child);
                     -- **Process_Sub_Tree (child, cArity, pool);
                     -- Flush the pool NOW
                     if (localOpti) then
                        Flush_Pool (pool);
                     end if;
                  end if;
               else -- It has never been visited
                  --+Put ("**Neither mapped nor visited ;-)");
                  -- If it has been **neither** MAPPED **nor** VISITED
                  -- Keep recursion first
                  cArity := Get_Succ_Number (child);
                  Process_Sub_Tree (child, cArity, pool);
                  -- Mark this node
                  Mark_Node (n => child);
                  Mark_Node_Curr (n => child);
                  --   Add this proc to the the current pool (out parameter)
                  --+Put_Line ("-> Add-Procedure " & Utrim (childInfo.ProcName) &
                  --+          Integer'Image(childInfo.size));
                  Add_Procedure (p => pool, --temp_pool,
                                 procName => childInfo.ProcName,
                                 size => childInfo.Size );
--                    -- keep recursion
--                    cArity := Get_Succ_Number (child);
--                    Process_Sub_Tree (child, cArity, pool); --temp_pool);
               end if;
            else
               -- This is a LOOP NODE
               -- keep recursion
               --+Put_Line("Just a LOOP node...");
               cArity := Get_Succ_Number (child);
               Process_Sub_Tree (child, cArity, pool); --temp_pool);
            end if;

            -- FLUSH the POOL (local GREEDY optimization) --
            -- ****************************************** --
            -- *  ++++  +     +    +   +++++  +    +  * --
            -- *  +     +     +    +  +       +    +  * --
            -- *  +++   +     +    +   ++++   ++++++  * --
            -- *  +     +     +    +       +  +    +  * --
            -- *  +	++++   ++++   +++++   +    +  * --
            -- ****************************************** --


-- ***************************************************************************
--              -- The node is a Procedure Node
--              if (Get_NType(n) /= LoopNode) then
--                 -- check if it's in alreadyMapped
--                 Already_Mapped (childInfo.ProcName,found,curDesc);
--
--                 -- Already VISITED
--                 if Is_marked(n => child) then
--                    if not found then
--                       Put_Line("**Already visited but not mapped ;-)");
--                       -- if it has been already VISITED **but** NOT MAPPED
--                       -- then the procedure belongs to the same subtree
--                       -- just stop recursion
--                    else
--                       Put_Line("**Already visited and mapped ;-)");
--                       -- if it has been already VISITED **and** MAPPED
--                       -- then the procedure belongs to a different subtree
--                       -- ***TRY a good placement here***
--                       --  ==> Flush the "subTreeProc"
--                       --  and stop recursion
--
--                       -- ***********************************************
--                       -- Add this procedure to "alreadyMApped"
--                       declare
--                          tempProc : Contents;
--                          newProc : Contents := new Procedures (1..alreadyNum+1);
--                       begin
--                          --  Copy the shared procedures collected so far
--                          if (alreadyNum > 0) then
--                             for I in 1..alreadyNum loop
--                                --Put_line ("Copying...");
--                                newProc(I) := alreadyMapped(I);
--                             end loop;
--                          end if;
--                          alreadyNum := alreadyNum +1;
--                          newProc (alreadyNum) := (curDesc.name,
--                                                   curDesc.size,
--                                                   curDesc.startAdd,
--                                                   curDesc.offSet);
--                          --curDesc;
--                          tempProc := alreadyMapped;
--                          alreadyMapped := newProc;
--                          Free_Proc (tempProc);
--                       end;
--                       pool.sharing := True;
--                       cArity := Get_Succ_Number (child);
--                       Process_Sub_Tree (child, cArity, pool); --temp_pool);
--                       -- ***********************************************
--
--                    end if;
--                    -- Not VISITED yet
--                 else -- Has not been VISITED
--                    if found then
--                       -- if it has been already MAPPED **but** NOT VISITED
--                       Put_Line("**Already mapped but not visited ;-)");
--                       -- Add this procedure to "alreadyMApped"
--                       declare
--                          tempProc : Contents;
--                          newProc : Contents := new Procedures (1..alreadyNum+1);
--                       begin
--                          --  Copy the shared procedures collected so far
--                          if (alreadyNum > 0) then
--                             for I in 1..alreadyNum loop
--                                --Put_line ("Copying...");
--                                newProc(I) := alreadyMapped(I);
--                             end loop;
--                          end if;
--                          alreadyNum := alreadyNum +1;
--                          newProc (alreadyNum) := (curDesc.name,
--                                                   curDesc.size,
--                                                   curDesc.startAdd,
--                                                   curDesc.offSet);
--                          --curDesc;
--                          tempProc := alreadyMapped;
--                          alreadyMapped := newProc;
--                          Free_Proc (tempProc);
--                       end;
--                       pool.sharing := True;
--                       -- Mark this node
--                       Mark_Node (n => child);
--                       -- keep recursion
--                       cArity := Get_Succ_Number (child);
--                       Process_Sub_Tree (child, cArity, pool); --temp_pool);
--                    else
--                       Put_Line("**Neither mapped nor visited ;-)");
--                       -- if it has been **neither** MAPPED **nor** VISITED
--                       -- Mark this node
--                       Mark_Node (n => child);
--                       --   Add this proc to the the current pool (out parameter)
--                       Put_Line ("-> Add-Procedure " & Utrim (childInfo.ProcName));
--                       Add_Procedure (p => pool, --temp_pool,
--                                      procName => childInfo.ProcName,
--                                      size => childInfo.Size );
--                       -- keep recursion
--                       cArity := Get_Succ_Number (child);
--                       Process_Sub_Tree (child, cArity, pool); --temp_pool);
--                    end if;
--                 end if;
--
--              elsif not Is_marked(n => child) then  -- This is a LOOP node
--                 Put_Line("Just a LOOP node...");
--                 -- keep recursion
--                 cArity := Get_Succ_Number (child);
--                 Process_Sub_Tree (child, cArity, pool); --temp_pool);
--              end if;




--              if (Get_NType(n) /= LoopNode and Is_marked(n => child) ) then
--                 --+Put ("NULL");
--                 Put_line ("*********Already visited  ;-)");
--                 null;
--              else
--                 if (Get_NType(n) /= LoopNode) then
--                    --+Put ("---");
--                    Already_Mapped (childInfo.ProcName,found,curDesc);
--                    --+Put ("% ");
--                    if found then
--                       Put_Line("Found=True");
--                       declare
--                          tempProc : Contents;
--                          newProc : Contents := new Procedures (1..alreadyNum+1);
--                       begin
--                          --  Copy the shared procedures collected so far
--                          if (alreadyNum > 0) then
--                             for I in 1..alreadyNum loop
--                                --Put_line ("Copying...");
--                                newProc(I) := alreadyMapped(I);
--                             end loop;
--                          end if;
--                          alreadyNum := alreadyNum +1;
--                          newProc (alreadyNum) := (curDesc.name,
--                                                   curDesc.size,
--                                                   curDesc.startAdd,
--                                                   curDesc.offSet);
--                          --curDesc;
--                          tempProc := alreadyMapped;
--                          alreadyMapped := newProc;
--                          Free_Proc (tempProc);
--                       end;
--                       pool.sharing := True;
--                       Put_line ("*********Already mapped  ;-)");
--                       --Print_Descriptor (alreadyMapped(alreadyNum));
--                       --Put_Line ("****************************");
--                    else
--                       --+Put ("After");
--                       -- Otherwise undergo a set of actions:
--                       --   Mark this node
--                       Mark_Node (n => child);
--                       --   Add this proc to the the current pool (out parameter)
--                       Put_Line ("-> Add-Procedure " & Utrim (childInfo.ProcName));
--                       Add_Procedure (p => pool, --temp_pool,
--                                      procName => childInfo.ProcName,
--                                      size => childInfo.Size );
--                       --+Put ("-Add-Procedure");
--                 -- (It will be added to the mapped proc only later)
--                 --+Put (" " &UTrim(Get_Node_Info(child).ProcName) & " ");
--                    end if;
--
--                    -- Then proceed recursively
--                    cArity := Get_Succ_Number (child);
--                    Process_Sub_Tree (child, cArity, pool); --temp_pool);
--                 end if;
--              end if;
            found := False;
         end loop;
--        else
--           Put_Line("No child in preprocess");
      end if;
   exception
      when Error: others =>
         Put_line("Proc_Sub_tree -> " & Exception_Name(Error));
         raise;
   end Process_Sub_Tree;



   procedure Get_Proc (n : Tree_Node_Ptr; childNum : Natural) is
      use Global, RAM;
      child : Tree_Node_Ptr;
      cArity: Natural;
      cList : Tree_Node_List_Ptr := Get_Succ(n);
      nInfo : Loop_Node_Info := Get_Info(n);
   begin
      --Put (">[" & Integer'Image(childNum) & "-" & Integer'IMage(Get_Succ_Number(n)) &"]");
      --  If has children
      if (childNum > 0) then
         for c in 1..childNum loop
            child := cList(c);
            Put (" " &UTrim(Get_Node_Info(child).ProcName) & " ");
            cArity := Get_Succ_Number (child);
            Get_Proc(child, cArity);
         end loop;
      end if;
      -- If this is the Root or a procedure node
      if (Get_NType(n) /= LoopNode) then
         if (Get_NType(N) = IndependentProcNode and then
               Map_In_Place_Holder(UTrim(nInfo.ProcName), nInfo.Size)) then
            null;
         else
            Map_Procedure(UTrim(nInfo.ProcName), nInfo.Size);
         end if;
      end if;
   exception
         when Error: others =>
         Put_line("Get_Proc -> " & Exception_Name(Error));
         raise;
   end Get_proc;


   procedure Compute_Layout is
      use Global, RAM, Xml_Parser;
      t : Tree_Type_Ptr := Trees(1);
      n : Tree_Node_Ptr := Get_Root(t.all);
      child : Tree_Node_Ptr;
      cList : Tree_Node_List_Ptr;
      childInfo : Loop_Node_Info;
      children : Natural;
      sharing : Boolean := False;
      displacement : Natural := 0;
      rootInfo : Loop_Node_Info;
      subTreeProc : Proc_Pool_ptr;
      curProc : Descriptor;
      curProcCopy : Descriptor;
      tempContents : Contents;
      thisChildNum : Integer;
   begin
      Init_Ram;
      Update_Node_Types (n);
      --+Put_Line("Unmarking");
      Unmark_Tree (t);
      --+Put_Line("LCT Unmarked");
      --+Put_Line("Before order");
      Order_Tree (n);
      if Global.E_M in VERBOSE .. MINIMAL then
         New_Line;
         Put_Line("  LCT Ordered");
      end if;

      if (Global.drawGraphs and Global.E_M in VERBOSE..DEBUG) then
      --  Export the ordered tree
         Put ("Exporting to Dot.......");
         Export_to_Dot_Bounded;
         Put_Line ("OK");
      end if;

      if(Constrained) then
         --Load_Mapping;
         Load_Constraints (Global.Constr_File.all);
         --Save_Mapping;
      end if;



      --+Put_Line("Unmarking");
      Unmark_Tree (t);
      --+Put_Line("LCT Unmarked");

      children := Get_Succ_Number(n);
      cList := Get_Succ (n);

      if(children>0) then
         --+Put("childNUm:" & integer'IMage(children));
         for I in 1..children loop
            --+Put_Line ("Child " & Integer'Image(I) & " of " & Integer'Image(children) );

            tempContents := alreadyMapped;
            Free_Proc (alreadyMapped);
            --alreadyMapped :=
            alreadyNum := 0;
            --Put_Line ("alreadyNum : " & Integer'Image(alreadyNum));
            displacement := 0;
            tobeMapped := 16#0#;
            sharing := False;
            child := cList (I);
            childInfo := Get_Info (n => child);
            thisChildNum := Get_Succ_Number(child);

            subTreeProc := new Proc_Pool'(new Procedures (1..1400), --thisChildNum),
                                          0,
                                          0,
                                          False);

            --Preprocess_SubTree (child, Get_Succ_Number(child), sharing);
            Process_Sub_Tree (n        => child,
                              childNum => Get_Succ_Number(child),
                              pool     => subTreeProc);

            -- mark and add this

            If (not Is_marked(n => child) and
                  Get_NType(n => child) /=LoopNode) then
               Mark_Node (n => child);
--                 Put_Line ("-> Add-Procedure " & Utrim (childInfo.ProcName) &
--                              Integer'Image(childInfo.size));
                  Add_Procedure (p => subTreeProc, --temp_pool,
                                 procName => childInfo.ProcName,
                                 size => childInfo.Size );
            end if;


            sharing := subTreeProc.sharing;
            --Put ("@");
            if (sharing) then
               --Put_Line("AlreadyNUM: " & INteger'IMage(alreadyNum) & "[" &
               --         Integer'Image(alreadyMapped'Length) & "]");
               --Print_Descriptor (alreadyMapped (1));
               --Check_Mapping (alreadyMapped, alreadyNum, tobeMapped, displacement);
               Check_Mapping (alreadyMapped, alreadyNum,
                              subTreeProc.size, displacement);
               --Put ("§");
               if (displacement /= 0) then
                  -- add a placeholder in the memory map
                  --+Put("Optimal displacement: ");
                  --+Ada.Integer_Text_IO.Put(Base=>16, Item=>displacement);
                  --+New_Line;
                  Map_Procedure(PLACE_HOLDER, displacement);-- 16#00000004#);
                  --Put ("+");
               end if;
            end if;
            -- Loop over the subTreeProc Proc_Pool_ptr to map procedures
            --+Put (" -- subT num:" & Integer'Image(subTreeProc.num));
            for J in 1..subTreeProc.num loop
               --+Put ("Map ");
               curProc := RAM.Get_Procedure (p => subTreeProc,
                                             n => J);

               Map_This (procName => curProc.name,
                         size     => curProc.size);
               -- At the same time update the MAPPED_PROCEDURES pool
               -- ********** DOUBLE insertion (see MAP_THIS)
               curProcCopy := (curProc.name, curProc.size,
                               curProc.startAdd, curProc.offSet);
               Add_Procedure_unique (p    => MAPPED_PROCEDURES,
                                     proc => curProcCopy,
                                     size => 0);
               -- **********
               --Print_Descriptor (curProcCopy);
            end loop;
            --Put_Line (" ");
            --+deallocate subTreeProc
            Erase_Pool (subTreeProc);
            --+Put ("::");
            Unmark_Tree_Curr (child);
         end loop;
      end if;

      --+Put_Line("NO children:" & integer'IMage(children));

      -- Only the root procedures remains to be mapped
      rootInfo := Get_Info(n);
      Map_Procedure(UTrim(rootInfo.ProcName), rootInfo.Size);

      if Global.E_M in VERBOSE..MINIMAL then
         Put_line ("  Mapped" & Integer'Image(RAM.Mapped) & " procedures");
      end if;
      --+Get_Proc (n, children);
      --+Print_Bins(false);
--        if(Global.E_M in VERBOSE..MINIMAL) then
--              Put_Line (" OK");
--        end if;

      --Save_Mapping;
      if (Global.printWays) then
         Print_Bins (true);
      end if;
      --+Put_Line("+++++++++++++++++++++++++");

      --Export_Linker_Script;
      Export_Linker_Script_Tsim;
      --+Export_Linker_Script_TAS;

      if (Global.Constr_File = null) then
         Global.Constr_File := new String'("constraints.xml");
      end if;

      Save_Constraints (Global.Constr_File.all);

   exception
         when Error: others =>
         Put_line("Compute_Layout " & Exception_Name(Error));
         raise;
   end Compute_Layout;


   --
   procedure LCT2WCG_Rec (wcgraph : WCG_Graph.WCG_ptr;
                          ltc_node : Tree_Node_Ptr;
                          wcg_node_parent : WCG_Graph.WCG_Vertex;
                          wcg_node : WCG_Graph.WCG_Vertex;
                          freq : Natural)
   is
      use WCG_Graph;
      wcg_node_ch : WCG_Vertex;
      child : Tree_Node_Ptr;
      cList : Tree_Node_List_Ptr;
      childInfo : Loop_Node_Info;
      children : Natural;
      localFreq : Natural;
   begin
      -- Browse the children
      children := Get_Succ_Number (ltc_node);
      cList := Get_Succ (ltc_node);
      if children > 0 then
         for I in 1..children loop
            child := cList (I);
            childInfo := Get_Info (n => child);
            if tbshootWCG then
               Put_line ("Analysing node " & Utrim(childInfo.Label));
            end if;
            -- If the node has been visited already just skip it
            -- if not Is_marked (child) then
               -- Check the lct node type
               if Get_NType(child) /=LoopNode then
                  -- Add a new vertex
               wcg_node_ch := (childInfo.ProcName,null,1,False);
               if tbshootWCG then
                  Put ("Adding node " & Utrim(childInfo.ProcName));
               end if;

               Add_Vertex(wcgraph.all, wcg_node_ch);
               Mark_Node (child);
               if freq /= 0 then
                  -- the parent wcg_node is a loop node
                  -- each procedure is actually called by the parent node
                  -- with frequency = freq
                  -- add a new edge with weight = freq from parent to this child
                  if tbshootWCG then
                     Put_line ("--Previous node (" &
                               Utrim(Get_info(ltc_node).Label) &
                               ") was a LOOP node");
                     Put_line ("Adding an edge between nodes " &
                               Utrim(wcg_node_parent.originalProcedure) &
                               " - " & Utrim(wcg_node_ch.originalProcedure));
                  end if;

                  Add_Edge (Graph  => wcgraph.all,
                            Head   => wcg_node_parent,
                            Tail   => wcg_node_ch,
                            Weight => freq);
                  LCT2WCG_Rec (wcgraph, child, wcg_node, wcg_node_ch, 0);
               else
                  -- the wcg_node is a procedure node
                  if tbshootWCG then
                     Put_line ("Adding an edge between nodes " &
                               Utrim(wcg_node.originalProcedure) &
                               " - " & Utrim(wcg_node_ch.originalProcedure));
                  end if;

                  Add_Edge (Graph  => wcgraph.all,
                            Head   => wcg_node,
                            Tail   => wcg_node_ch,
                            Weight => 1);
                  LCT2WCG_Rec (wcgraph, child, wcg_node, wcg_node_ch, 0);
               end if;
            else
               if tbshootWCG then
                  Put_line ("This is a Loop node and its parent is node #"
                            & Utrim(wcg_node.originalProcedure) & " -> "
                            & Integer'Image(freq) &
                            "*" & Integer'Image(childInfo.Bound) &
                            "=" & Integer'Image(freq*childInfo.Bound));
               end if;
               -- the child node is a loop node
               -- propagate the frequency=freq*childInfo.Bound
               if freq = 0 then
                  -- either this is the first loop node encountered
                  localFreq := 1;
                  LCT2WCG_Rec (wcgraph, child, wcg_node, wcg_node_ch, localFreq*childInfo.Bound);
               else
                  -- or it is a chain of loop nodes
                  localFreq := freq;
                  LCT2WCG_Rec (wcgraph, child, wcg_node_parent, wcg_node_ch, localFreq*childInfo.Bound);
               end if;
--               LCT2WCG_Rec (wcgraph, child, wcg_node_parent, wcg_node_ch, localFreq*childInfo.Bound);
            end if;
            --end if;
         end loop;
      else
         if tbshootWCG then
            Put_line ("Node with no children");
         end if;
      end if;

   end LCT2WCG_Rec;


   procedure LCT2WCG is
      use WCG_Graph;
      lct : Tree_Type_Ptr := Trees(1);
      lct_root : Tree_Node_Ptr := Get_Root(lct.all);
      lct_root_info : Loop_Node_Info;
      wcgraph : WCG_ptr;
      wcg_node: WCG_Vertex;
      wcg_node_ch : WCG_Vertex;
      child : Tree_Node_Ptr;
      cList : Tree_Node_List_Ptr;
      childInfo : Loop_Node_Info;
      children : Natural;

   begin
      --Put_Line ("Transforming LCT into a WCG ...........");
      -- Initialize the WCG_Singleton
      -- The number of procedures is determined by "loopTreeNum" var
      Init (progName => Get_Id(lct) & "_WCG",
            num      => loopTreeNum);
      wcgraph := Get_WCG;
      if tbshootWCG then
         Put_line("Initialized a WCG with size->" &
                  Integer'Image(loopTreeNum));
      end if;

      -- Transform the LCT into a WCG
      -- Unmark the LCT
      Unmark_Tree (lct);

      -- Add the LCT root to the WCG
      lct_root_info := Get_Info (lct_root);
      wcg_node := (lct_root_info.ProcName,null,1,False);
      if tbshootWCG then
         Put ("Adding node " & UTrim(lct_root_info.ProcName));
      end if;

      Add_Vertex(wcgraph.all, wcg_node);
      Mark_Node (lct_root);
      -- Browse its children
      children := Get_Succ_Number (lct_root);
      cList := Get_Succ (lct_root);
      if children > 0 then
         for I in 1..children loop
            child := cList (I);
            childInfo := Get_Info (n => child);
            -- If the node has been visited already just skip it
            if not Is_marked (child) then
               -- Check the lct node type
               if Get_NType(child) /=LoopNode then
                  -- Add a new vertex
                  wcg_node_ch := (childInfo.ProcName,null,1,False);
                  if tbshootWCG then
                     Put ("Adding node " & UTrim(childInfo.ProcName));
                  end if;

                  Add_Vertex(wcgraph.all, wcg_node_ch);
                  Mark_node (child);
                  -- add a new edge with weight = 1
                  if tbshootWCG then
                     Put_line ("Adding an edge between nodes " &
                               Utrim(wcg_node.originalProcedure) &
                               " - " & Utrim(wcg_node_ch.originalProcedure));
                  end if;

                  Add_Edge (Graph  => wcgraph.all,
                            Head   => wcg_node,
                            Tail   => wcg_node_ch,
                            Weight => 1);
                  LCT2WCG_Rec (wcgraph, child, wcg_node, wcg_node_ch, 0);
               else
                  LCT2WCG_Rec (wcgraph, child, wcg_node, wcg_node_ch, childInfo.Bound);
               end if;
            end if;
         end loop;
      end if;
      --if (Global.drawGraphs) then
         Export_To_Dot_Single (wcgraph);
      --end if;

      --Put_Line(" OK");
   end LCT2WCG;



   procedure Compute_Layout_WCG is
      use Global, RAM, Xml_Parser, WCG_Graph;
Start_Time, End_Time : Ada.Calendar.Time;
   begin
      Init_Ram;
Start_Time := Clock; 
      LCT2WCG;
End_Time := Clock;
Put_line("");
Put_Line ("LCT2WCG-> (" & Duration'Image ((End_Time - Start_time)) & " seconds)");
      if (Global.E_M in VERBOSE..MINIMAL) then
         New_Line;
         Put_Line ("  LCT2WCG trasformation performed");
      end if;
      --Put_line ("  WCG constructed");
Start_Time := Clock; 
      WCG_Merging_Process;
End_Time := Clock;
Put_Line ("WCGMERGING -> (" & Duration'Image ((End_Time - Start_time)) & " seconds)");
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put_line ("  WCG Merged");
      end if;
      --Export_Linker_Script;
      --+Export_Linker_Script_Tsim;
      --+Export_Linker_Script_TAS;
      if wcgOpti then
         Export_Linker_Script_Tsim_WCG;
      else
         Export_Linker_Script_Tsim;
      end if;

   exception
         when Error: others =>
         Put_line("Compute_Layout WCG -> " & Exception_Name(Error));
         raise;
   end Compute_Layout_WCG;




   serial : Natural := 1;

   procedure Node_Criticality (n : Tree_Node_Ptr;
                               lNodes : in out Natural;
                               pNodes : in out Natural;
                               align : Unbounded_String;
                               output : File_Type) is
      child : Tree_Node_Ptr;
      nInfo : Loop_Node_Info := Get_Info (n);
      childInfo : Loop_Node_Info;
      childType : Loop_Tree_Node_Type;
      cList : Tree_Node_List_Ptr := Get_Succ(n);
      children : Natural := Get_Succ_Number (n);
      succ_lNodes : Natural := 0;
      succ_pNodes : Natural := 0;
      countL : Natural := 0;
      countP : Natural := 0;
      newAlign : Unbounded_String := align & "-";
   begin
      if children > 0 then
         for I in 1 .. children loop
            succ_lNodes := 0;
            succ_pNodes := 0;
            child := cList (I);
            childInfo := Get_Info (child);
            childType := Get_NType (child);
            --Put ("  ");
            Node_Criticality (child, succ_lNodes, succ_pNodes, newAlign, output);
            if childType = LoopNode then
               --Put_Line ("Aloop node!");
               countL := countL + 1;
               succ_lNodes := succ_lNodes + 1;
            elsif childType = RootNode or childType = IndependentProcNode then
               succ_pNodes := succ_pNodes + 1;
               countP := countP + 1;
            else
               Put_Line (output, "no type!?!: " & Print_Nt (Get_NType(child)));
            end if;
            --              Node_Criticality (child, succ_lNodes, succ_pNodes);
            lNodes := lNodes + succ_lNodes;
            pNodes := pNodes + succ_pNodes;
         end loop;
      end if;
      if not Is_marked(n) and Get_NType(n) /= LoopNode then
         Put (output, UTrim(align) & "(" & Integer'Image(serial) & ") Node: " & Utrim(nInfo.ProcName));
         Put (output, "(" & Print_Nt (Get_NType(n)) & ")");
         Put (output, " Branching factor: " & Integer'Image(children) & " (" & Trim(countP)& "," & Trim(countL));
         Put_line (output, ") [" & Integer'Image(succ_pNodes) & "," & Integer'Image(succ_lNodes) & " ]");
         -- Update sequence counter
         serial := serial + 1;
         -- Mark this node
         Mark_Node (n);
      elsif Is_marked(n) and Get_NType(n) /= LoopNode then
         Put_Line (output, UTrim(align) & "Link to -> " & Utrim(nInfo.ProcName));
      end if;
      --lNodes := lNodes + succ_lNodes;
      --pNodes := pNodes + succ_pNodes;

   end Node_Criticality;

   procedure Node_Criticality_Inverse (n : Tree_Node_Ptr;
                                       lNodes : in out Natural;
                                       pNodes : in out Natural) is
      child : Tree_Node_Ptr;
      nInfo : Loop_Node_Info := Get_Info (n);
      childInfo : Loop_Node_Info;
      childType : Loop_Tree_Node_Type;
      cList : Tree_Node_List_Ptr := Get_Succ(n);
      children : Natural := Get_Succ_Number (n);
      succ_lNodes : Natural := 0;
      succ_pNodes : Natural := 0;
   begin
      if not Is_marked(n) and Get_NType(n) /= LoopNode then
         Put ("(" & Integer'Image(serial) & ")Node: " & Utrim(nInfo.ProcName));
         Put ("(" & Print_Nt (Get_NType(n)) & ")");
         Put_Line (" Branching factor: " & Integer'Image(children));
         --Put_line ("[" & Integer'Image(succ_pNodes) & "," & Integer'Image(succ_lNodes) & "]");
         -- Update sequence counter
         serial := serial + 1;
      elsif not Is_marked(n) and Get_NType(n) = LoopNode then
         Put_Line (" Branching factor: " & Integer'Image(children));
      end if;

      if children > 0 then
         for I in 1 .. children loop
            --Put (" ");
            child := cList (I);
            childInfo := Get_Info (child);
            childType := Get_NType (child);
            if childType = LoopNode then
               --Put_Line ("Aloop node!");
               --Put (" L ");
               succ_lNodes := succ_lNodes + 1;
            elsif childType = RootNode or childType = IndependentProcNode then
               succ_pNodes := succ_pNodes + 1;
               --Put (" ->");
            else
               Put_Line ("no type!?!: " & Print_Nt (Get_NType(child)));
            end if;
            Node_Criticality_Inverse (child, lNodes, pNodes);
         end loop;
         --New_Line;
      end if;
      if not Is_marked(n) then
         Mark_Node (n);
      end if;
      lNodes := lNodes + succ_lNodes;
      pNodes := pNodes + succ_pNodes;

   end Node_Criticality_Inverse;

   procedure Compute_Criticality is
      use Global, RAM, Xml_Parser, Ada.Integer_Text_IO;
      Output    : File_Type;
      t : Tree_Type_Ptr := Trees(1);
      n : Tree_Node_Ptr := Get_Root(t.all);
      nInfo : Loop_Node_Info := Get_Info (n);
      --child : Tree_Node_Ptr;
      l,p  : Natural := 0;
      align : Unbounded_String := To_Unbounded_String("-");
   begin
      -- Unmark all nodes in the LCT
      Unmark_Tree (n);
      --
      Create (Output, Out_File, UTrim(nInfo.ProcName) & ".txt");
      --Put_Line (Output, "<constraints>");

      --child := Get_Root (t.all);
      Put_Line (Output, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ");
      Node_Criticality (n, l, p, align, Output);
      Put_Line (Output, " ");
      Put_Line (Output, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ");
      Put_Line (Output, "+ Procedure: " & Utrim(nInfo.ProcName));
      Put_Line (Output, "+++++++++++  LCT Statistics  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ");
      Close (Output);
      Interactions.Reverse_File (name => UTrim(nInfo.ProcName), ext => ".txt");
      Interactions.Erase_File(UTrim(nInfo.ProcName));
   end Compute_Criticality;


end Loop_Tree;

