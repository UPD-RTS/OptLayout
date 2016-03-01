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
-- FILE NAME      : interactions.ads
-- PACKAGE        : INTERACTIONS spec
-- PURPOSE        : File system and command line utilities
-- CREATION       : 2011-09-15
-- LAST REVISION  : 2011-09-15
-- REVISED BY     : Enrico
-----------------------------------------------------------------------

with Interfaces.C; use Interfaces.C;


package Interactions is

   -- **DUMMY**
   procedure Execute_System;

   -- **UNUSED**
   procedure Normalize (fileName : String);

   -- Remove dupplicate keys form a dot file
   procedure Normalize_Dot (fileName : String);

   -- Replace a file with a new one
   procedure Replace (oldF, newF : String);

   -- Reverse the order of lines in a text file
   procedure Reverse_File (name : String; ext : String);

   -- Copy a file
   procedure Copy (oldF, newF : String);

   -- Execute dot -> ps and ps2pdf
   procedure Execute_Dot (name : String);

   -- Compute a precedure size by using "nm" from binutils
   procedure Procedure_Size (prog : String; name : String);

   -- Retrieve a precedure size from a text file
   function Get_Procedure_Size (prog : String; name : String) return Integer;

   -- Erase a file
   procedure Erase_File (fileName : String);

   -- Abruptely exit from the program
   procedure Abort_Program;

end Interactions;
