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
-- FILE NAME      : cache.adb
-- PACKAGE        : CACHE body
-- PURPOSE        : Representation of the current cache configuration
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Global;

package body Cache is

   procedure Init_Cache (l : Line_Dim; s : Set_Dim; w : Positive ) is
      use Global;
   begin
      -- Initialize the singleton cache instance
      cacheSingleton := new Cache_type'(ways       => w,
                                        line       => l,
                                        lines_X_set => s);
      if (Global.E_M = VERBOSE) then
         if((l*s*w)/1024 > 0) then
            Put_Line("A " & Integer'Image((l*s*w)/1024) &
                     " KBytes Cache has been correctly initialized: ");
         else
            Put_Line("A " & Integer'Image((l*s*w)) &
                     " Bytes Cache has been correctly initialized: ");
         end if;

         Put_line("    " & Integer'Image(l) & " Byte lines");
         Put_line("    " & Integer'Image(s) & " sets ");
         Put_line("    " & Integer'Image(w) & "-ways set-associative");
      end if;
   end Init_Cache;

   function Get_Ways return Positive is
   begin
      return cacheSingleton.ways;
   end Get_Ways;

   function Get_Line_Size return Positive is
   begin
      return cacheSingleton.line;
   end Get_Line_Size;

   function Get_Lines_X_Set return Positive is
   begin
      return cacheSingleton.lines_X_set;
   end Get_Lines_X_Set;

   function End_Of_Set (start : Address) return Integer is
      use Ada.Integer_Text_IO;
   begin
      --Put (Base => 16,
       --    Item => start+(cacheSingleton.line*cacheSingleton.lines_X_set)-1);
      return start+(cacheSingleton.line*cacheSingleton.lines_X_set)-1;
   end End_Of_Set;

   function Get_Set_Size return Integer is
   begin
      return cacheSingleton.line * cacheSingleton.lines_X_set;
   end Get_Set_Size;

end Cache;
