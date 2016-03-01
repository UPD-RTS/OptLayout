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
-- FILE NAME      : xml_parser.ads
-- PACKAGE        : XML_PARSER spec
-- PURPOSE        : Utilities to load/store information from/into XML
--                  including CFG reconstruction and constraint mgmt
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

package Xml_Parser is

   Back_Up : Natural := 0;

   -- Construct a CFG in memory from the XML representation of
   -- the program structure
   procedure CFG_Reconstruction (Filename : String);

   -- Loads the program structure
   procedure Build_CFG (Filename : String);

   -- Incrementally loads the program structure
   procedure Build_One_CFG (Filename : String);

   -- Includes loop bounds
   procedure Apply_Bounds (fileName : String);

   -- Loads cache configuration
   procedure Load_Cache (fileName : String);

   -- Loads layout constraints
   procedure Load_Constraints (fileName : String);

   -- Loads current layout as a set of constraints
   procedure Save_Constraints (fileName : String);

   -- **DUMMY**
   -- Handle renaming of the constraint file
   function Get_Bk_name (fileName : String) return String;

private

   --+procedure Usage;

end Xml_Parser;
