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
-- FILE NAME      : main.adb
-- PACKAGE        :
-- PURPOSE        : Program main
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Argument_Parser; use Argument_Parser;
with Xml_Parser; use Xml_Parser;
with Global; use Global;
with Ada.Text_IO; use Ada.Text_IO;
with Dominator_Analysis; use Dominator_Analysis;
with Loop_Tree; use Loop_Tree;
with Ada.Exceptions; use Ada.Exceptions;
with Cache; use Cache;
with Symbol_Parser; use Symbol_Parser;
with Utilities;


procedure main is
begin
   -- Retrieve command line arguments
   Get_Arguments;

   -- **UNIMPLEMENTED**
   -- Hook for future bespoke disassembler
   if (Program_File /= null) then
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put ("Loading file symbols...................");
      end if;
      Load_Symbols(Program_File.all);
      if (Global.E_M in VERBOSE..MINIMAL) then
         Put_line (" OK");
      end if;
   end if;

   -- Printing the executable name
   if (Global.PName_Spec) then --Global.E_M in VERBOSE..MINIMAL and
      Put_Line ("  Executable name: " &
                Utilities.UTrim(Program));
      New_Line;
   end if;

   --  Cache configuration
   if (Cache_File /= null) then
      -- Load the cache settings
      Put ( "Loading cache config...................");
      Load_Cache(Cache_File.all);
      Put_line (" OK");
   else
      --  Provide a default configuration
      Put ( "Using a default cache config...........");
      Init_Cache(32, 256, 4);
      Put_line (" OK");
   end if;

   if (Loop_File /= null) then
      -- If loop bounds are provided then
      Global.Bounded := True;
   else
      -- No loop bound information is available
      Global.Bounded := False;
   end if;

   if (Global.E_M in VERBOSE..MINIMAL) then
      Put_Line ("Reconstructing CFGs and building LCTs..");
   else
      Put ("Reconstructing CFGs and building LCTs..");
   end if;

   -- Trigger the optimization process either LCT- or WCG-based
   -- Reconstruct the program CFGs from the XML structure given in input
   -- and build the corresponding LCT or WCG
   -- finally computes an optimized layout and outputs a linker script
   CFG_Reconstruction (File_Name.all);

exception
   when Error : Storage_Error =>
      Put_Line("Main -> STORAGE ERROR!!!");
   when Error: others => null;--Put_Line(Exception_Name(Error));
end main;
