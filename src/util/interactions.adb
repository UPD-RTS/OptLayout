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
-- FILE NAME      : interactions.adb
-- PACKAGE        : INTERACTIONS body
-- PURPOSE        : File system and command line utilities
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
----------------------------------------------------------------------

with Interfaces.C; use Interfaces.C;
with Ada.Text_IO;
with Ada.Command_Line.Environment;

package body Interactions is

   procedure Execute_System is
      function Sys (Arg : Char_Array) return Integer;
      pragma Import(C, Sys, "system");
      Ret_Val : Integer;
   begin
      Ret_Val := Sys(To_C("ls"));
   end Execute_System;


   procedure Execute_Dot (name : String) is
      use Ada.Text_IO;
      function Sys (Arg : Char_Array) return Integer;
      pragma Import(C, Sys, "system");
      Ret_Val : Integer;
   begin
      Ret_Val := Sys(To_C("dot -Tps " & name & ".dot -o " & name & ".ps"));
      --Put_Line("Ret_val= " & Integer'Image(Ret_Val));
      if(Ret_val = 0) then
         Ret_Val := Sys(To_C("ps2pdf " & name & ".ps " & name & ".pdf"));
      else
         Put_Line ("Ecnountered some error while producing the GRAPH.");
      end if;
   end Execute_Dot;

   procedure Erase_File (fileName : String) is
      use Ada.Text_IO;
      function Sys (Arg : Char_Array) return Integer;
      pragma Import(C, Sys, "system");
      Ret_Val : Integer;
   begin
      Ret_Val := Sys(To_C("rm "& fileName &".txt"));

   end Erase_File;

   procedure Normalize_Dot (fileName : String) is
      use Ada.Text_IO;
      function Sys (Arg : Char_Array) return Integer;
      pragma Import(C, Sys, "system");
      Ret_Val : Integer;
   begin
      -- sort -u -k 1,32 main_LOOP.dot > main_LOOP.dot
      --Put_Line("££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££");
      --Put_Line("uniq "& fileName &".dot > "& fileName &"_u.dot");
      -- awk '!x[$0]++' file > file.new
      --Ret_Val := Sys(To_C("uniq "& fileName &".dot > "& fileName &"_u.dot"));
      Ret_Val := Sys(To_C("awk '!x[$0]++' "& fileName &".dot > "& fileName &"_u.dot"));
      Ret_Val := Sys(To_C("mv "& fileName &"_u.dot "& fileName &".dot"));
   end Normalize_Dot;

   procedure Normalize (fileName : String) is
      use Ada.Text_IO;
      function Sys (Arg : Char_Array) return Integer;
      pragma Import(C, Sys, "system");
      Ret_Val : Integer;
   begin
      -- sort -u -k 1,32 main_LOOP.dot > main_LOOP.dot
      --Put_Line("££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££££");
      --Put_Line("uniq "& fileName &".dot > "& fileName &"_u.dot");
      -- awk '!x[$0]++' file > file.new
      --Ret_Val := Sys(To_C("uniq "& fileName &".dot > "& fileName &"_u.dot"));
      Ret_Val := Sys(To_C("awk '!x[$0]++' "& fileName & "> temp.xml"));
      --Ret_Val := Sys(To_C("mv temp.xml" & fileName(5..fileName'Last)));
   end Normalize;

   procedure Reverse_File (name : String; ext : String) is
   use Ada.Text_IO;
      function Sys (Arg : Char_Array) return Integer;
      pragma Import(C, Sys, "system");
      Ret_Val : Integer;
   begin
      Ret_Val := Sys(To_C("tac "& name & ext & "> " & name & ".ltc"));
   end Reverse_File;

   procedure Procedure_Size (prog : String; name : String) is
      use Ada.Command_Line.Environment;
      use Ada.Text_IO;
      function Sys (Arg : Char_Array) return Integer;
      pragma Import(C, Sys, "system");
      Ret_Val : Integer;
      --b : String (1..8);
   begin
      Ret_Val := Sys(To_C("nm -S " & prog & " | grep -w " & name & " | cut -d ' ' -f 2 > " & name &".txt"));
   end Procedure_Size;

   function Get_Procedure_Size (prog : String; name : String) return Integer is
      size : String (1..8);
      use Ada.Text_IO;
      My_File   : File_Type;
      last : Integer := 7;
      File_Name : String := name & ".txt";

   begin
      Procedure_Size(prog, name);
      Open(File => My_File,
           Mode => In_File,
           Name => File_Name);

      Get_Line(File => My_File,
               Item => size,
               Last => last);

      Close(My_File);
      Erase_File(name);
      --Put_line("WW: " & size);
      return Integer'Value("16#" & size & "#");
   end Get_Procedure_Size;


   procedure Replace (oldF, newF : String) is
      use Ada.Command_Line.Environment;
      use Ada.Text_IO;
      function Sys (Arg : Char_Array) return Integer;
      pragma Import(C, Sys, "system");
      Ret_Val : Integer;
   begin
      Ret_Val := Sys(To_C("mv " & newF & " " & oldF));

   end Replace;

   procedure Copy (oldF, newF : String) is
      use Ada.Command_Line.Environment;
      use Ada.Text_IO;
      function Sys (Arg : Char_Array) return Integer;
      pragma Import(C, Sys, "system");
      Ret_Val : Integer;
   begin
      Ret_Val := Sys(To_C("cp " & oldF & " " & newF));
   end Copy;


   procedure Abort_Program is
   begin
      Ada.Command_Line.Set_Exit_Status(1);
      return;
   end Abort_Program;



end Interactions;
