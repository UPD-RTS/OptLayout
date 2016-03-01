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
-- FILE NAME      : tree.adb
-- PACKAGE        : TREE body (generic)
-- PURPOSE        : Generic package for tree-based data structure
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
--use Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Global; use Global;

package body Tree is

   --  Unchecked deallocation of nodes
   procedure Free is new  Ada.Unchecked_Deallocation (Tree_Node,
                                                      Tree_Node_Ptr);

   -- Unchecked deallocation of successor list
   procedure Free_Node_List is new
     Ada.Unchecked_Deallocation ( Tree_Node_List,
                                 Tree_Node_List_Ptr);


   procedure Print_Node (N : Tree_Node_Ptr) is
      use Ada.Text_IO;
      successors : Tree_Node_List := N.Succ_List.all;
      succ : Tree_Node_Ptr;
      succSize : Natural;
   begin
      Print_Node (N.Info);
      Put ("Parent: ");
      if(N.Parent /= null) then
         Print_node (N.Parent.Info);
      else
         Put_Line ("none (Root)");
      end if;
      Put ("Node_Type: " & Print(N.NType));
      Put_Line (" - Link Type: " & Print(N.LType));
      succSize := N.Next_Slot-1;
      Put ("Succ (" & Integer'Image(succSize) & "): ");
      for Index in 1 .. succSize loop
         succ := successors(Index);
         Put ("- "); Print_Node (succ.Info);
      end loop;
      New_Line;
   end Print_Node;

   function Get_Info (n : Tree_Node_Ptr) return Tree_Node_Info is
   begin
      return n.Info;
   end Get_Info;

   procedure Set_Info (n : in out Tree_Node_Ptr; info : Tree_Node_Info) is
   begin
      n.Info := info;
   end Set_Info;

   procedure Set_Parent (c : in out Tree_Node_Ptr; p : Tree_Node_Ptr) is
   begin
      c.Parent := p;
   end Set_Parent;


   function Get_NType (n: Tree_Node_Ptr) return Node_type is
   begin
      return n.NType;
   end Get_NType;

   procedure Set_NType (n: Tree_Node_Ptr; nT: Node_Type) is
   begin
      --Put_Line("++ " & Print(nT) & "++ ");
      n.NType := nT;
   end Set_NType;


   function Get_LType (n: Tree_Node_Ptr) return Link_type is
   begin
      return n.LType;
   end Get_LType;

   function Create (Root : Tree_Node_Ptr; name : String) return Tree_Type_ptr is
      tree : Tree_Type_Ptr;
   begin
      --Put (name); Put_Line("***");
      tree := new Tree_Type;
      tree.Head := Root;
      tree.Id := name;
      return tree;
   end Create;

   function Create_Node (nInfo : Tree_Node_Info; nType : Node_Type;
                         lType : Link_Type) return Tree_Node_Ptr is
      newNode : Tree_Node_Ptr;
   begin
      newNode := new Tree_Node'(nInfo,nType,lType, null,
                                new Tree_Node_List (1..Succ_Default_Size),
                                Succ_Default_Size, 1, False, 0, False);
      return newNode;
   end Create_Node;


   function IsNull (n : Tree_Node_ptr) return Boolean is
   begin
      return n = null;
   end IsNull;


   function "=" (Left, Right : Tree_Node_Ptr) return Boolean is
   begin
      return Left.Info = Right.Info;
   end "=";


   procedure Clear (N : in out Tree_Node_Ptr) is
      Succs : Tree_Node_List := N.Succ_List.all;
   begin
      for Index in 1..Succs'Size loop
         Free(Succs(Index));
      end loop;
   end Clear;

   procedure Clear (T : in out Tree_Type) is
   begin
      Clear(T.Head);
   end Clear;


   function Get_Succ_Number (N : in Tree_Node_Ptr) return Integer is
      node : Tree_Node := N.all;
   begin
      --Put_line(Integer'Image(node.Next_Slot));
      return (node.Next_Slot - 1);
   end Get_Succ_Number;

   function Get_Succ (N : in Tree_Node_Ptr) return Tree_Node_List is
      List : Tree_Node_List_Ptr;
   begin
      List := N.Succ_List;
      return List.all;
   end Get_Succ;

   function Get_Succ (N : in Tree_Node_Ptr) return Tree_Node_List_Ptr is
   begin
      return N.Succ_List;
   end Get_Succ;

   procedure Print_Succ (N : in Tree_Node_Ptr) is
      num : Integer := Get_Succ_Number (N);
      list : Tree_Node_List_Ptr := Get_Succ (N);
   begin
      --Put_line ("Successors: ");
      for I in 1..num loop
         Print_Node (list(I));
      end loop;
      --Put_Line ("+++++++++++++ ");
   end Print_Succ;




   --  Right size = Left size + 10
   --  Copy_Node_List(Parent.Succ_List, List); --Right size = Left size + 10
   procedure Copy_Node_List (Left, Right : Tree_Node_List_Ptr) is
      temp : Tree_Node_List_Ptr;
   begin
      for Index in 1..Left'Length loop
         Right (Index) := Left (Index);
      end loop;
      temp := Left;
      Free_Node_List (temp);
   end Copy_Node_List;

   procedure Add_Succ_Deep (Parent, Child : in out Tree_Node_Ptr) is
      --use Global;
      --nT : Node_Type;-- := Parent.NType;
      --Pos : Natural; --:= Parent.Next_Slot;
      --List : Tree_Node_List_Ptr;
   begin
      null;
   end Add_Succ_Deep;


   procedure Add_Succ (Parent, Child : in out Tree_Node_Ptr) is
      use Global;
      nT : Node_Type;-- := Parent.NType;
      Pos : Natural; --:= Parent.Next_Slot;
      List : Tree_Node_List_Ptr;
      succNum : Natural;
      alreadyChild : Boolean:= False;
   begin
      if Global.E_M in VERBOSE..DEBUG or GLobal.tbshootLCT then
         Put ("In Add_Succ (parent => "); Print_Node_Pretty (Parent); New_Line;
         Put ("             child  => "); Print_Node_Pretty (Child);  New_Line;
      end if;
      nT := Parent.NType;
      Pos := Parent.Next_Slot;
      -- If child is already in succ(parent)
      succNUm := parent.Next_Slot-1;
      for I in 1..succNum loop
         if Parent.Succ_List.all (I) = Child then
            alreadyChild := True;
         end if;
      end loop;
      if not alreadyChild then
         if Global.tbshootLCT then
            Put_line(Integer'Image(Parent.Succ_List.all'Length) &
                     "(" & Integer'Image(Parent.Succ_list_Size) &
                     ") " & "here:" & Integer'Image(Pos));
         end if;

         --  Succ list is not full
         if (Pos < Parent.Succ_List.all'Length) then
            --qui

            Parent.Succ_List.all (Pos) := Child;
            --+Parent.Succ_list_Size := Pos+1;
            if (Global.E_M = VERBOSE or Global.tbshootLCT) then
               Put_line("Expansion not required!!!");
            end if;
         else
            --  Succ list is full: grow the list capacity
            if (Global.E_M = VERBOSE or Global.tbshootLCT) then
               Put_line("Expanding succList");
            end if;
            List := new Tree_Node_List(1..Pos+Increment_Step);
            --+Put_Line("Going to copy");
            Copy_Node_List(Parent.Succ_List, List); --Right size = Left size + 10
            Parent.Succ_List := List;
            Parent.Succ_List.all (Pos) := Child;
            Parent.Succ_list_Size := Pos+Increment_Step;
         end if;
         Child.Parent := Parent;
         --  Update next available position
         Parent.Next_Slot := Pos +1;
         --  Put ("Now node # "); Print_Node(Parent.Info);
         --  Put(" has " & Integer'Image(Parent.Next_Slot-1) & " children:");
         if (Global.E_M = VERBOSE) then
            Put ("Now node # "); Print_Node(Parent.Info);
            Put(" has " & Integer'Image(Parent.Next_Slot-1) & " children:");
            New_Line;
            Print_Node(Parent);
            Print_Node(Child);
         --Put_Line("end_Add");
         end if;
      end if;
   end Add_Succ;

   function Get_Succ_Size (N : Tree_Node_Ptr) return Positive is
   begin
      return N.Succ_list_Size;
   end Get_Succ_Size;

   procedure Print_Node_Pretty (N : Tree_Node_Ptr) is
   begin
      PPretty.all (N.info);
   end Print_Node_Pretty;




   procedure Remove_From_Succ (N, parent: in out Tree_Node_Ptr) is
      -- parentInfo : Loop_Tree.Loop_Node_Info := parent.Info;
      --succN : Tree_Node_List := N.Succ_List.all;
      test : Tree_Node_Ptr;
      succParent : Tree_Node_List_Ptr := parent.Succ_List;--.all;
      --succList : Tree_Node_List := Get_Succ(parent);
      lastElement : Natural := parent.Next_Slot-1;
   begin
      --  Remove N from its Parent's children
      --  if the children is the last element in succList
      --  simply remove it
      if Global.E_M in VERBOSE..DEBUG or GLobal.tbshootLCT then
         Put ("In Remove_From_Succ (parent => "); Print_Node_Pretty (parent);
         Put ("                     child  => "); Print_Node_Pretty (N); New_Line;
      end if;
      --Put_Line (Integer'Image(lastElement));
      Remove :
      for Index in 1..lastElement loop
         --Put (Integer'Image(Index) & " ");
         test := succParent(Index);
         --Print_Node (test);
         if(succParent(Index)=N) then
            if Global.E_M in VERBOSE..DEBUG or GLobal.tbshootLCT then
               Put ("FOund");
            end if;
            if(Index /= lastElement) then
               succParent(Index):=succParent(lastElement);
               --Index = Index-1;
            end if;
            succParent(lastElement):=null;
            parent.Next_Slot := lastElement;
            exit Remove;
         end if;
      end loop Remove;
      if Global.E_M in VERBOSE..DEBUG or GLobal.tbshootLCT then
         New_Line;
      end if;
   end Remove_From_Succ;

   --  Removes a node from the tree

   procedure Remove (N : in out Tree_Node_Ptr; preserve : in Boolean) is
      parent : Tree_Node_Ptr := N.Parent;
      succN : Tree_Node_List := N.Succ_List.all;
      succParent : Tree_Node_List := parent.Succ_List.all;
      lastElement : Natural := parent.Next_Slot-1;
   begin
      --  Remove N from its Parent's children
      for Index in 1..succParent'Size loop
         if(succParent(Index)=N) then
            succParent(Index):=succParent(lastElement);
            succParent(lastElement):=null;
            parent.Next_Slot := lastElement;
         end if;
      end loop;

      --  If PRESERVE is True then N's children become N.Parent children
      if (preserve) then
         for Index in 1..succN'Size loop
            Add_Succ (parent, succN(Index));
         end loop;
      end if;

      --  Free N
      --  Clear (N);
   end Remove;

   --  Merges two sibling nodes (Same Parent and Link_Type)
   procedure Merge_Nodes (Left, Right : in out Tree_Node_Ptr)  is
      pos : Positive := Left.Next_Slot;
      succLeft_Num : Natural := Get_Succ_Number (Right);
      --succLeft : Tree_Node_List := Left.Succ_List.all;
      succRight: Tree_Node_List := Right.Succ_List.all;
   begin
      --  Capacity problems are handled by the Add_Succ procedure
      --  Next_Slot is updated by the Add_Succ procedure
      for Index in 1..succRight'Size loop
         Add_Succ (Left, succRight(Index));
      end loop;
      --  Update Node_Info
      Merge_Node_Info (Left.Info, Right.Info);
      --  Free the Right node
      Clear (Right);
   end Merge_Nodes;

   function Child (P: in Tree_Node_Ptr; Child : in Positive) return
     Tree_Node_Ptr is
   begin
      return P.Succ_List.all (Child);
   end Child;

   function Parent (N : in Tree_Node_Ptr) return Tree_Node_Ptr is
   begin
      return N.Parent;
   end Parent;

   function Arity (T : Tree_Type) return Natural is
   begin
      return 0;
   end Arity;

   function Has_Children (T : in Tree_Type) return Boolean is
   begin
      return False;
   end Has_Children;

   function Is_Null (T : in Tree_Type) return Boolean is
   begin
      return T.Head = null;
   end Is_Null;

   function Is_Root (T : in Tree_Type; N : in Tree_Node_Ptr) return Boolean is
   begin
      return T.Head = N;
   end Is_Root;


   function Get_Root (T : in Tree_Type) return Tree_Node_Ptr is
   begin
      return T.Head;
   end Get_Root;

   procedure Mark_Node (n : Tree_Node_Ptr) is
   begin
      n.Marked := True;
   end Mark_Node;

   procedure Unmark_Tree (root : Tree_Node_Ptr) is
      succNum : Natural := Get_Succ_Number(root);
      succList : Tree_Node_List := Get_Succ(root);
      succNode : Tree_Node_Ptr;
   begin
      --+Put_line("Unmarking");
      if succNum > 0 then
         for n in 1..succNum loop
            succNode := succlist(n);
            succNode.Marked := False;
            Unmark_Tree(succNode);
         end loop;
      end if;
      root.Marked := False;
   end Unmark_Tree;

   procedure Unmark_Tree (t : Tree_Type_Ptr) is
   begin
      Unmark_Tree(Get_Root(t.all));
   end Unmark_Tree;

   function Get_Weight (n : Tree_Node_Ptr) return Integer is
   begin
      return n.Weight;
   end Get_Weight;


   procedure Set_Weight (n : Tree_Node_Ptr; w : Integer) is
   begin
      n.Weight := w;
   end Set_Weight;



   function Is_marked (n : Tree_Node_Ptr) return Boolean is
   begin
      return n.Marked;
   end Is_marked;

   function Is_marked_Curr (n : Tree_Node_Ptr) return Boolean is
   begin
      return n.MarkedCurr;
   end Is_marked_Curr;

   procedure Mark_Node_Curr (n : Tree_Node_Ptr) is
   begin
      n.MarkedCurr := True;
   end Mark_Node_Curr;

   procedure Unmark_Tree_Curr (root : Tree_Node_Ptr) is
      succNum : Natural := Get_Succ_Number(root);
      succList : Tree_Node_List := Get_Succ(root);
      succNode : Tree_Node_Ptr;
   begin
      if succNum > 0 then
         for n in 1..succNum loop
            succNode := succlist(n);
            succNode.MarkedCurr := False;
            Unmark_Tree_Curr(succNode);
         end loop;
      end if;
      root.MarkedCurr := False;
   end Unmark_Tree_Curr;

   function Get_Id (t : Tree_Type_Ptr) return String is
   begin
      --Put("같같�");
      --Put(t.Id);Put_line("같같�");
      return t.Id;
   end Get_Id;



   procedure Finalize (N : in out Tree_Node_ptr) is
      List : Tree_Node_List := N.Succ_List.all;
   begin
      for Index in 1..List'Size loop
         Free(List(Index));
      end loop;
      Free(N);
   end Finalize;


   procedure Finalize (T : in out Tree_Type) is
   begin
      Free (T.Head);
   end Finalize;



end Tree;

