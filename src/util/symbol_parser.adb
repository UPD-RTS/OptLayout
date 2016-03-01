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
-- FILE NAME      : symbol_parser.adb
-- PACKAGE        : SYMBOL_PARSER body
-- PURPOSE        : Custom file parser
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Input_Sources.File; use Input_Sources.File;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Utilities;use Utilities;

package body Symbol_Parser is

--    function Get_Line return Unbounded_String is
--       BufferSize : constant := 2000;
--       Retval     : Unbounded_String := Null_Unbounded_String;
--       Item       : String (1 .. BufferSize);
--       Last       : Natural;
--    begin
--       Get_Whole_Line :
--          loop
--             Get_Line (Item => Item, Last => Last);
--
--             Append (Source => Retval, New_Item => Item (1 .. Last));
--
--             exit Get_Whole_Line when Last < Item'Last;
--          end loop Get_Whole_Line;
--          return Retval;
--    end Get_Line;


   procedure Load_Symbols (fileName : String) is
      Input    : File_Type;
      Output   : FIle_Type;
      Line     : Unbounded_String;
   begin
      Put_Line("reading program structure");
      Open(Input, In_File, fileName);
      Create(Output, Out_File, fileName & ".clean");
      while not End_Of_File(Input) loop
         Line := To_Unbounded_String(Get_Line(Input));
         --if(
         Put_Line(Output, UTrim(Line));
      end loop;


   exception
      when Name_Error =>
         Put(File => Standard_Error, Item => "File name not found.");
      when Status_Error =>
         Put(File => Standard_Error, Item => "File already open.");
      when Use_Error =>
         Put(File => Standard_Error,
             Item => "You do not have permission to open the file");
   end Load_Symbols;



end Symbol_Parser;
